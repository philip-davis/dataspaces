#include <winstd.H>
#include <cstdio>

#include "LevelBld.H"

#include "ADR.H"
#include "ADR_F.H"
#include "Derive_F.H"

using std::string;

static Box the_same_box (const Box& b) { return b; }
static Box grow_box_by_one (const Box& b) { return BoxLib::grow(b,1); }

typedef StateDescriptor::BndryFunc BndryFunc;

//
// Components are:
//  Interior, Inflow, Outflow,  Symmetry,     SlipWall,     NoSlipWall
//
static int scalar_bc[] =
{
    INT_DIR, EXT_DIR, FOEXTRAP, REFLECT_EVEN, REFLECT_EVEN, REFLECT_EVEN
};

static int norm_vel_bc[] =
{
    INT_DIR, EXT_DIR, FOEXTRAP, REFLECT_ODD,  REFLECT_ODD,  REFLECT_ODD
};

static int tang_vel_bc[] =
{
    INT_DIR, EXT_DIR, FOEXTRAP, REFLECT_EVEN, REFLECT_EVEN, REFLECT_EVEN
};

static
void
set_scalar_bc (BCRec& bc, const BCRec& phys_bc)
{
    const int* lo_bc = phys_bc.lo();
    const int* hi_bc = phys_bc.hi();
    for (int i = 0; i < BL_SPACEDIM; i++)
    {
        bc.setLo(i,scalar_bc[lo_bc[i]]);
        bc.setHi(i,scalar_bc[hi_bc[i]]);
    }
}

static
void
set_x_vel_bc(BCRec& bc, const BCRec& phys_bc)
{
    const int* lo_bc = phys_bc.lo();
    const int* hi_bc = phys_bc.hi();
    bc.setLo(0,norm_vel_bc[lo_bc[0]]);
    bc.setHi(0,norm_vel_bc[hi_bc[0]]);
#if (BL_SPACEDIM >= 2)
    bc.setLo(1,tang_vel_bc[lo_bc[1]]);
    bc.setHi(1,tang_vel_bc[hi_bc[1]]);
#endif
#if (BL_SPACEDIM == 3)
    bc.setLo(2,tang_vel_bc[lo_bc[2]]);
    bc.setHi(2,tang_vel_bc[hi_bc[2]]);
#endif
}

#if (BL_SPACEDIM >= 2)
static
void
set_y_vel_bc(BCRec& bc, const BCRec& phys_bc)
{
    const int* lo_bc = phys_bc.lo();
    const int* hi_bc = phys_bc.hi();
    bc.setLo(0,tang_vel_bc[lo_bc[0]]);
    bc.setHi(0,tang_vel_bc[hi_bc[0]]);
    bc.setLo(1,norm_vel_bc[lo_bc[1]]);
    bc.setHi(1,norm_vel_bc[hi_bc[1]]);
#if (BL_SPACEDIM == 3)
    bc.setLo(2,tang_vel_bc[lo_bc[2]]);
    bc.setHi(2,tang_vel_bc[hi_bc[2]]);
#endif
}
#endif

#if (BL_SPACEDIM == 3)
static
void
set_z_vel_bc(BCRec& bc, const BCRec& phys_bc)
{
    const int* lo_bc = phys_bc.lo();
    const int* hi_bc = phys_bc.hi();
    bc.setLo(0,tang_vel_bc[lo_bc[0]]);
    bc.setHi(0,tang_vel_bc[hi_bc[0]]);
    bc.setLo(1,tang_vel_bc[lo_bc[1]]);
    bc.setHi(1,tang_vel_bc[hi_bc[1]]);
    bc.setLo(2,norm_vel_bc[lo_bc[2]]);
    bc.setHi(2,norm_vel_bc[hi_bc[2]]);
}
#endif

void
ADR::variableSetUp ()
{
    BL_ASSERT(desc_lst.size() == 0);

    // Initialize the network
    adr_network_init();

    // Get options, set phys_bc
    read_params();
    //
    // Set number of state variables and pointers to components
    //
    int cnt = 0;
    Density = cnt++;
    Xvel = cnt++;
    Yvel = cnt++;
#if (BL_SPACEDIM == 3)
    Zvel = cnt++;
#endif
    NumAdv = 1;
    if (NumAdv > 0)
    {
        FirstAdv = cnt++;
        cnt += NumAdv - 2;
        LastAdv = cnt++;
    }

    int dm = BL_SPACEDIM;

    // Get the number of species from the network model.
    BL_FORT_PROC_CALL(GET_NUM_SPEC, get_num_spec)(&NumSpec);

    if (NumSpec > 0)
    {
        FirstSpec = cnt++;
        cnt += NumSpec - 2;
        LastSpec = cnt++;
    }

    NUM_STATE = cnt;

    // Define NUM_GROW from the f90 module.
    BL_FORT_PROC_CALL(GET_METHOD_PARAMS, get_method_params)(&NUM_GROW);

    const Real run_strt = ParallelDescriptor::second() ; 

    BL_FORT_PROC_CALL(SET_METHOD_PARAMS, set_method_params)
        (dm, Density, Xvel, FirstAdv, FirstSpec, 
         NumAdv, normalize_species);

    Real run_stop = ParallelDescriptor::second() - run_strt;
 
    ParallelDescriptor::ReduceRealMax(run_stop,ParallelDescriptor::IOProcessorNumber());
 
    if (ParallelDescriptor::IOProcessor())
        std::cout << "\nTime in set_method_params: " << run_stop << '\n' ;

    int coord_type = Geometry::Coord();
    BL_FORT_PROC_CALL(SET_PROBLEM_PARAMS, set_problem_params)
         (dm,phys_bc.lo(),phys_bc.hi(),Outflow,Symmetry,coord_type);

    Interpolater* interp = &cell_cons_interp;

    // Note that the default is state_data_extrap = false, store_in_checkpoint = true
    // We only need to put these explicitly if we want to do something different,
    // like not store the state data in a checkpoint directory
    bool state_data_extrap = false;
    bool store_in_checkpoint;

    store_in_checkpoint = true;
    desc_lst.addDescriptor(State_Type,IndexType::TheCellType(),
                           StateDescriptor::Point,0,NUM_STATE,
                           interp,state_data_extrap,store_in_checkpoint);

    Array<BCRec>       bcs(NUM_STATE);
    Array<std::string> name(NUM_STATE);
    
    BCRec bc;
    cnt = 0;
    set_scalar_bc(bc,phys_bc); bcs[cnt] = bc; name[cnt] = "density";
    cnt++; set_x_vel_bc(bc,phys_bc);  bcs[cnt] = bc; name[cnt] = "xvel";
#if (BL_SPACEDIM >= 2)
    cnt++; set_y_vel_bc(bc,phys_bc);  bcs[cnt] = bc; name[cnt] = "yvel";
#endif
#if (BL_SPACEDIM == 3)
    cnt++; set_z_vel_bc(bc,phys_bc);  bcs[cnt] = bc; name[cnt] = "zvel";
#endif

    for (int i=0; i<NumAdv; ++i)
    {
        char buf[64];
        sprintf(buf, "adv_%d", i);
        cnt++; set_scalar_bc(bc,phys_bc); bcs[cnt] = bc; name[cnt] = string(buf);
    }

    // Get the species names from the network model.
    char* spec_names[NumSpec];
    for (int i = 0; i < NumSpec; i++) {
          int len = 20;
          Array<int> int_spec_names(len);
          // This call return the actual length of each string in "len" 
          BL_FORT_PROC_CALL(GET_SPEC_NAMES, get_spec_names)(int_spec_names.dataPtr(),&i,&len);
          spec_names[i] = new char[len+1];
          for (int j = 0; j < len; j++) 
             spec_names[i][j] = int_spec_names[j];
          spec_names[i][len] = '\0';
    }

    if ( ParallelDescriptor::IOProcessor())
    {
        std::cout << NumSpec << " Species: " << std::endl;
        for (int i = 0; i < NumSpec; i++)  
           std::cout << spec_names[i] << ' ' << ' ';
        std::cout << std::endl;
    } 

    for (int i=0; i<NumSpec; ++i)
    {
        cnt++; 
        set_scalar_bc(bc,phys_bc); 
        bcs[cnt] = bc; 
        string spec_string(spec_names[i]);
        name[cnt] = "rho_" + spec_string;
    }

    desc_lst.setComponent(State_Type,
                          Density,
                          "density",
                          bcs[Density],
                          BndryFunc(BL_FORT_PROC_CALL(DENFILL,denfill)));
    desc_lst.setComponent(State_Type,
                          Xvel,
                          "xvel",
                          bcs[Xvel],
                          BndryFunc(BL_FORT_PROC_CALL(XVELFILL,xvelfill)));
    desc_lst.setComponent(State_Type,
                          Yvel,
                          "yvel",
                          bcs[Yvel],
                          BndryFunc(BL_FORT_PROC_CALL(YVELFILL,yvelfill)));
#if (BL_SPACEDIM == 3)
    desc_lst.setComponent(State_Type,
                          Zvel,
                          "zvel",
                          bcs[Zvel],
                          BndryFunc(BL_FORT_PROC_CALL(ZVELFILL,zvelfill)));
#endif
    for (int i=0; i<NumSpec; ++i)
    {
    desc_lst.setComponent(State_Type,
                          FirstSpec+i,
                          name[FirstSpec+i],
                          bcs[FirstSpec+i],
                          BndryFunc(BL_FORT_PROC_CALL(SPECFILL,specfill)));
    }

    for (int i=0; i<NumAdv; ++i)
    {
    desc_lst.setComponent(State_Type,
                          FirstAdv+i,
                          name[FirstAdv+i],
                          bcs[FirstAdv+i],
                          BndryFunc(BL_FORT_PROC_CALL(SPECFILL,specfill)));
    }

    //
    // DEFINE DERIVED QUANTITIES
    //

    derive_lst.add("StateErr",IndexType::TheCellType(),NumSpec,
                   BL_FORT_PROC_CALL(DERSTATE,derstate),grow_box_by_one);
    derive_lst.addComponent("StateErr",desc_lst,State_Type,Density,1);
    derive_lst.addComponent("StateErr",desc_lst,State_Type,FirstSpec,NumSpec);

    //
    // Vorticity
    //
    derive_lst.add("magvort",IndexType::TheCellType(),1,
                   BL_FORT_PROC_CALL(DERMAGVORT,dermagvort),grow_box_by_one);
    derive_lst.addComponent("magvort",desc_lst,State_Type,Xvel,BL_SPACEDIM);

    //
    // X from rhoX
    //
    for (int i = 0; i < NumSpec; i++){
      string spec_string(spec_names[i]);
      spec_string = "X("+spec_string+")";

      derive_lst.add(spec_string,IndexType::TheCellType(),1,
                   BL_FORT_PROC_CALL(DERSPEC,derspec),the_same_box);
      derive_lst.addComponent(spec_string,desc_lst,State_Type,Density,1);
      derive_lst.addComponent(spec_string,desc_lst,State_Type,FirstSpec+i,1);
    }

    derive_lst.add("magvel",IndexType::TheCellType(),1,
          BL_FORT_PROC_CALL(DERMAGVEL,dermagvel),the_same_box);
    derive_lst.addComponent("magvel",desc_lst,State_Type,Density,1);
    derive_lst.addComponent("magvel",desc_lst,State_Type,Xvel,BL_SPACEDIM);

    for (int i = 0; i < NumSpec; i++)  {
      string spec_string(spec_names[i]);
      derive_lst.add(spec_string,IndexType::TheCellType(),1,
          BL_FORT_PROC_CALL(DERSPEC,derspec),the_same_box);
      derive_lst.addComponent(spec_string,desc_lst,State_Type,Density,1);
      derive_lst.addComponent(spec_string,desc_lst,State_Type,FirstSpec+i,1);
    }

#if 0
    //
    // A derived quantity equal to all the state variables.
    //
    derive_lst.add("FULLSTATE",IndexType::TheCellType(),NUM_STATE,FORT_DERCOPY,the_same_box);
    derive_lst.addComponent("FULLSTATE",desc_lst,State_Type,Density,NUM_STATE);

#endif

    //
    // DEFINE ERROR ESTIMATION QUANTITIES
    //
    ErrorSetUp();

    for (int i = 0; i < NumSpec; i++) {
      delete[] spec_names[i];
    }
}
