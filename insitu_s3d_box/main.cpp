#include <iostream>
#include "stdio.h"

#include <Box.H>
#include <BoxArray.H>
#include <MultiFab.H>

#include <ParallelDescriptor.H>
#include <DistributionMapping.H>

#include "S3D_Box.H"

#include "insitu_tools.h"
#include "insitu_data_description.h"
#ifdef HAVE_DSPACES
#include "dataspaces_api.h"
#endif



//#include "f_testing_tools.h"

/*
 * main
 * Setup boxlib
 * Initialize s3d libray
 * Query s3d modules for data sizes
 * Allocate FABs
 * Call s3d field init routines
 * Loop over timesteps, call s3d integration routines
 * 
 */
int main(int argc, char ** argv) {

    // ------------------------------------------------------------------------
    // Setup BoxLib and s3d
/*
    BoxLib::Initialize(argc,argv);
    ParallelDescriptor::Barrier();

    if( ParallelDescriptor::MyProc() == 0 ){
	std::cout << "Welcome to S3D-Box..." << std::endl;
    }

    // Get communicator from BoxLib and convert to fortran communictor
    MPI_Comm c_comm = ParallelDescriptor::Communicator();

    int myid;  MPI_Comm_rank( c_comm, &myid );
    int npes;  MPI_Comm_size( c_comm, &npes );
*/
    int myid, npes;

    MPI_Comm c_comm;
    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &npes);
    MPI_Comm_rank(MPI_COMM_WORLD, &myid);


    /* Create sub-communicator for all in-situ processes DS */
    int color = 0;
    MPI_Comm_split(MPI_COMM_WORLD, color, myid, &c_comm);
    MPI_Comm_size(c_comm, &npes);
    MPI_Comm_rank(c_comm, &myid);
    /***/

#ifdef HAVE_DSPACES
    double init_time = -1 * MPI_Wtime();
    ds_init(npes, IN_SITU);
    init_time += MPI_Wtime();
    if (!myid)
        fprintf(stderr, "EVAL: insitu ds_init() time is %0.5f\n", init_time);
#endif

    BoxLib::Initialize(argc, argv, true, c_comm);

    MPI_Fint f_comm = MPI_Comm_c2f( c_comm );

    // Push communictor to s3d and call initialization routines
    // to parse s3d.in, setup decomposition, grid, modules
    int io;
    s3d_initialize_( &f_comm, &io );

    // End of boxlib and s3d 'library' init routines
    // ------------------------------------------------------------------------

    // ------------------------------------------------------------------------
    // This section sets up boxes corresponding to s3d domain decomposition

    // Get min/max extents of domain (local and global) from s3d
    int local_size[3], global_min[3], global_max[3], xyzpes[3], xyzid[3];
    get_s3d_extents_( local_size, global_min, global_max, xyzpes, xyzid);
    int qnx = local_size[0];
    int qny = local_size[1];
    int qnz = local_size[2];
   std::cout << "qnx = " << qnx << " qny = " << qny << " qnz = " << qnz << std::endl;
   std::cout << "xyzpes[0] = " << xyzpes[0] << " xyzpes[1] = " << xyzpes[1] << " xyzpes[2] = " << xyzpes[2] << std::endl;
   int global_dim[3];
   for(int i=0; i < 3; i++) global_dim[i] = global_max[i]-global_min[i];
   std::cout << "global_dim[0] = " << global_dim[0] << " global_dim[1] = " << global_dim[1] << " global_dim[2] = " << global_dim[2] << std::endl;

    // Build the list of extents for all ranks
    int * los = new int[3*npes];
    int * his = new int[3*npes];

    MPI_Allgather( (global_min), 3, MPI_INTEGER, los, 3, MPI_INTEGER, c_comm  );
    MPI_Allgather( (global_max), 3, MPI_INTEGER, his, 3, MPI_INTEGER, c_comm  );

    // Make the boxes on all ranks and put them into a boxlist
    // This list needs to be in order of ranks,
    // which we'll use with a linear distribution mapping to get the right box
    // on the right rank

    BoxList theboxlist;

    for(int ibox=0; ibox < npes; ibox++ ){
       IntVect my_min( &(los[ibox*3]) );
       IntVect my_max( &(his[ibox*3]) );
       Box my_box(my_min, my_max);
       theboxlist.push_back(my_box);
    }

#ifdef DEBUG
    std::cout << "Rank " << myid 
       << " made a BoxList with " << theboxlist.size() << " boxes (" 
       << npes << " ranks)"<< std::endl;
#endif

    // Then a boxarray from the BoxList
    BoxArray boxes( theboxlist );

#ifdef DEBUG
    std::cout << "Rank " << myid 
       << " made a BoxArray with " << boxes.size() << " boxes (" 
       << npes << " ranks)"<< std::endl;
#endif

    // Make distribution mapping that matches S3D - e.g., box i goes on rank i
    Array<int> pmap(npes+1);
    for(int ibox=0; ibox < npes; ibox++ ){
       pmap[ibox] = ibox;
    }
    pmap[npes] = myid;

    DistributionMapping distMap( pmap );

    // End of section to set up boxes corresponding to s3d domain decomposition
    // ------------------------------------------------------------------------


    // ------------------------------------------------------------------------
    // Setup memory allocation
    
    // Now we can allocate a multifab, with ngrow = 0 and mem_mode = Fab_allocate
    // ncomp = nvar_tot*nreg (rk registers for the variables from S3D)
    // This will be for s3d 'q' vector
    int ncomp, nvar_tot, nreg;
    get_s3d_nvar_tot_( &nvar_tot, &nreg );

    ncomp = nvar_tot*nreg;
    int n_q_grow = 0;

    // Seem to need to do define separately to spec a custom distribution mapping
    MultiFab qmfab;
    qmfab.define( boxes,
                  ncomp,
                  n_q_grow,
                  distMap,
                  Fab_allocate);


    // These are work arrays for s3d RHS - those that will be differentiated
    // with 8th-order method, need 4 halo points
    int n_primary_grow = 4;
    MultiFab yspcmfab, tempmfab, uvelmfab, workmfab;
    MultiFab insumfab_ghost; //HK. MFab for insitu variables with ghost data
    MultiFab insumfab_noghost; //HK. MFab for insitu variables without ghost data

    // Species mass fraction
    yspcmfab.define( boxes,
                     nvar_tot,
                     n_primary_grow,
                     distMap,
                     Fab_allocate);

    // Temperature
    tempmfab.define( boxes,
                     1,
                     n_primary_grow,
                     distMap,
                     Fab_allocate);

    // Velocity
    uvelmfab.define( boxes,
                     3,
                     n_primary_grow,
                     distMap,
                     Fab_allocate);

    // Operand for differentiation
    workmfab.define( boxes,
                     1,
                     n_primary_grow,
                     distMap,
                     Fab_allocate);

    //HK. MFab for Copies of 8 primitive/derived vars for in_situ analyses (viz/stats/topology) 
    //HK. Ideally this number should be stored in something like n_insitu_vars whose value
    //HK. can be set at initialisation. these have ghost cells
    int insitu_ghost_size=1;
    insumfab_ghost.define( boxes,
                     gNumInsituVars_ghost,
                     insitu_ghost_size,
                     distMap,
                     Fab_allocate);
    //JB. MFab for Copies of 3 primitive/derived vars for in_situ analyses (viz/stats/topology) 
    //JB. Ideally this number should be stored in something like n_insitu_vars whose value
    //JB. can be set at initialisation.  these have no ghost cells
    insumfab_noghost.define( boxes,
                     gNumInsituVars_noghost,
                     n_q_grow,
                     distMap,
                     Fab_allocate);

    // Get pointer to FABs; for q check that there is one and only one,
    // others just make sure we have a valid pointer
    int nvalidFabs = 0;
    double * q = NULL;
    for(MFIter mfi(qmfab); mfi.isValid(); ++mfi){
        BL_ASSERT( q == NULL );
        q = (qmfab[mfi]).dataPtr();
        nvalidFabs++;
    }
    BL_ASSERT( nvalidFabs == 1 );
    BL_ASSERT( q != NULL );

    double * yspc = NULL;
    for( MFIter mfi(yspcmfab); mfi.isValid(); ++mfi){
        BL_ASSERT( yspc == NULL );
        yspc = (yspcmfab[mfi]).dataPtr();
    }
    BL_ASSERT( yspc != NULL );

    double * op_work = NULL;
    for( MFIter mfi(workmfab); mfi.isValid(); ++mfi){
        BL_ASSERT( op_work == NULL );
        op_work = (workmfab[mfi]).dataPtr();
    }
    BL_ASSERT( op_work != NULL );

    double * temp = NULL;
    for( MFIter mfi(tempmfab); mfi.isValid(); ++mfi){
        BL_ASSERT( temp == NULL );
        temp = (tempmfab[mfi]).dataPtr();
    }
    BL_ASSERT( temp != NULL );

    double * uvel = NULL;
    for( MFIter mfi(uvelmfab); mfi.isValid(); ++mfi){
        BL_ASSERT( uvel == NULL );
        uvel = (uvelmfab[mfi]).dataPtr();
    }
    BL_ASSERT( uvel != NULL );

    double * insitu_vars_ghost = data_ptr(&insumfab_ghost);
    double * insitu_vars_noghost = data_ptr(&insumfab_noghost);

    // End of setting up memory allocation
    // ------------------------------------------------------------------------

    // ------------------------------------------------------------------------
    // Set initial conditions - field init is set by run_title in s3d.in
    
    initialize_field_(&io,q,&qnx,&qny,&qnz,&nvar_tot,&nreg);

    // Copy temperature from s3d module 'temp' variable to temp data in tempmfab
    // (this is only required because the newton solve starts from the previous
    // temperature, and 0 is outside limits)
    get_s3d_module_temp_( temp );

    // Generate active file (must be done after all s3d init)
    generate_active_file_(&io);


    int num_peers = xyzpes[0]*xyzpes[1]*xyzpes[2];
    int totalDimensions[3];
    totalDimensions[0] = xyzpes[0]*qnx;
    totalDimensions[1] = xyzpes[1]*qny;
    totalDimensions[2] = xyzpes[2]*qnz;

    initialize_insitu(&myid, &c_comm, xyzpes, xyzid, totalDimensions, &insumfab_ghost, &insumfab_noghost, insitu_ghost_size); 

    // End of initial conditions setup
    // ------------------------------------------------------------------------


    // ------------------------------------------------------------------------
    // Time advance loop

    int itime_end, itime_save;

    get_s3d_itime_end_save_( &itime_end, &itime_save );

    
    int iorder;
    get_s3d_iorder_( &iorder );

    int itime = 0;
    double time = 0.0;
    get_s3d_time_ (&time);
    double dt;

    MultiFab * yspcmfab_ptr = &yspcmfab;
    MultiFab * tempmfab_ptr = &tempmfab;
    MultiFab * uvelmfab_ptr = &uvelmfab;
    MultiFab * workmfab_ptr = &workmfab;
    MultiFab * insumfab_ptr_ghost = &insumfab_ghost;

    int do_insitu;         //HK. Integer flag to perform update of insitu_vars. Could it be boolean??

    for( itime=0; itime<=itime_end; itime++){
        set_s3d_itime_( &itime );
        set_timestep_(&io, q, uvel,  &qnx, &qny, &qnz, &nvar_tot, &nreg);
        get_s3d_dt_( &dt );

	//HK. set do_insitu flag
	if( insitu_timestep( itime) ){
	  do_insitu = 1;
	}
	else {
	  do_insitu = 0;
	}
	
    //std::cout << io<< qnx<< qny<< qnz<< nvar_tot<< nreg << std::endl;
    //std::cout << "beginning themf addresses: " << &yspcmfab << ", " << &tempmfab << ", " << &uvelmfab << " , " << &workmfab << std::endl;
    //std::cout << "beginning themf_ptrs: " << yspcmfab_ptr << ", " << tempmfab_ptr << ", " << uvelmfab_ptr << " , " << workmfab_ptr << std::endl;

        integrate_(&io, q, &qnx, &qny, &qnz, &nvar_tot, &nreg, 
        	   yspc, &yspcmfab_ptr,
        	   temp, &tempmfab_ptr,
        	   uvel, &uvelmfab_ptr,
        	   op_work, &workmfab_ptr,
		   insitu_vars_ghost, &insumfab_ptr_ghost, insitu_vars_noghost, &do_insitu, &insitu_ghost_size); //HK // JB

        time += dt;
        set_s3d_time_( &time );

        // write savefile
	if ((itime>0) && ((itime%itime_save)==0)){
            dump_s3d_( &io );
        }
       
        // perform insitu_analysis
        if( do_insitu ){
	// DS Do we need a barrier here as in sample code?
	    //std::cout << "itime=" << itime << " to perform insitu operation"<< std::endl;

	    //MPI_Barrier(c_comm);
            perform_insitu_analysis( &myid, &c_comm, itime );
	    // binary file of first few fields of some mfab
	    int ncomp_noghost = gNumInsituVars_noghost;
            int ncomp_ghost = gNumInsituVars_ghost;
            //dump_klm_files_mfab_( insitu_vars_noghost, &ncomp_noghost, insitu_vars_ghost, &ncomp_ghost, &insitu_ghost_size);
        }


        // Monitor solution
        double q_err;
//	double temp_max, pres_max, q_err;
    //    monitor_(&io,&temp_max,&pres_max, q,
    //           	 &qnx, &qny, &qnz, &nvar_tot, &nreg);
        get_s3d_err_( &q_err );

        if( myid == 0 ){
            printf(" %d \t %g \t %g \t %g \n", 
        	    itime, time, dt, q_err); 
            /*printf(" %d \t %g \t %g \t %g \t %f \t %f \n", 
        	    itime, time, dt, 
        	    q_err, temp_max, pres_max); */
        }

    }
    
    MPI_Barrier(c_comm);

    // End of time advance loop
    // ------------------------------------------------------------------------

    // ------------------------------------------------------------------------
    // Cleanup

    finalize_insitu(&myid, &c_comm);

    s3d_finalize_( &io );

    BoxLib::Finalize(false);
    std::cout << "Goodbye from S3D-Box..." << std::endl;

#ifdef HAVE_DSPACES
    ds_finalize();
#endif

    MPI_Barrier(MPI_COMM_WORLD);
    MPI_Finalize();

    return 0;
}
