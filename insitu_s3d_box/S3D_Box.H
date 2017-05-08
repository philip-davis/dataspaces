#ifndef S3D_BOX_H
#define S3D_BOX_H


#include <stdio.h>
extern "C" {

    // Trigger s3d actions in s3d_box files

    // From s3d_initialize.f90
    void s3d_initialize_( MPI_Fint *, int *  );
    void s3d_finalize_(  int *  );

    // From integrate_erk.f90
    void set_timestep_( int * , double *, double *, 
	                int *, int *, int *, int *, int *  );
    void integrate_(int *, double *, int *, int *, int *, int *, int *, 
	            double *, MultiFab **, 
	            double *, MultiFab **, 
	            double *, MultiFab **, 
		    double *, MultiFab **,
		    double *, MultiFab **,
		    double *, int *, int *); //HK. Last line of args for insitu_analytics 

    // From s3d_box_tools.f90
    void dump_s3d_(int * );
    
    // From dump_klm_file.f90
    void dump_klm_files_mfab_( double *, int *, double *, int *, int * );

    // Accessor functions for s3d module variables
    void get_s3d_err_( double * );
    void get_s3d_dt_( double * );
    void get_s3d_extents_( int *, int *, int *, int *, int * );
    void get_s3d_nvar_tot_( int *, int * );
    void get_s3d_module_temp_( double * );
    void get_s3d_iorder_( int * );
    void get_s3d_itime_end_save_( int *, int * );

    void set_s3d_itime_( int * );
    void set_s3d_time_( double * );
    void get_s3d_time_( double * );


    
    // Part of s3d_lib
    void monitor_(int *, double *, double *, double *, 
	          int *, int *, int *, int *, int *  );
    void generate_active_file_(int *);

    void initialize_field_(int *, double *, int *, int *,
	                   int *, int *, int * );
    void dump_klm_files_(int *);


    // Trigger BoxLib actions
    void do_boxlib_fill_( MultiFab ** );
    void get_mfab_components_( MultiFab ** , int * );

}

void do_boxlib_fill_( MultiFab ** theMF ){
//   std::cout << "theMF: " << *theMF << std::endl;
    (*theMF)->FillBoundary();
}

void get_mfab_components_( MultiFab ** theMF, int * nphi ){
    (*theMF)->FillBoundary();
}

#endif
