#include <iomanip>

#include <Utility.H>
#include <Geometry.H>
#include <PArray.H>
#include <MultiFab.H>

#include <COMP_NORM_F.H>

void compute_norm(const PArray<MultiFab>& soln, const PArray<MultiFab>& exac, 
		  const std::vector<Geometry>& geom, const std::vector<BoxArray>& grids,
		  int nsoln, int iCpp, int iF90, int iHyp)
{
  Array<Real> twonorm(nsoln, 0.0);
  Array<Real> maxnorm(nsoln, 0.0);
  Real volume=0.0;
  
  int nlevel = soln.size();
  int ref_ratio = 2.;

  for (int ilev=0; ilev<nlevel; ilev++) {

    BoxArray baf;

    if (ilev < nlevel-1) {
      baf = grids[ilev+1];
      baf.coarsen(ref_ratio);
    }

    for (MFIter mfi(soln[ilev]); mfi.isValid(); ++mfi) {
      int i = mfi.index();
      
      const Box& bx = grids[ilev][i];

      FArrayBox mask(bx, 1);
      mask.setVal(1.0);
      if (ilev < nlevel-1) {
      	std::vector< std::pair<int,Box> > isects = baf.intersections(bx);

        for (int ii = 0; ii < isects.size(); ii++) {
          mask.setVal(0.0, isects[ii].second, 0);
        }
      }

      FArrayBox volbox(bx, 1);
      geom[ilev].GetVolume(volbox, grids[ilev], i, 0);

      BL_FORT_PROC_CALL(LST_COMP_NORM, lst_comp_norm)
	(bx.loVect(), bx.hiVect(),
	 BL_TO_FORTRAN(soln[ilev][mfi]),
	 BL_TO_FORTRAN(exac[ilev][mfi]),
	 BL_TO_FORTRAN(mask),
	 BL_TO_FORTRAN(volbox),
	 twonorm.dataPtr(),
	 maxnorm.dataPtr(),
	 &volume, &nsoln);
    }
  }

  ParallelDescriptor::ReduceRealSum(twonorm.dataPtr(), nsoln);
  ParallelDescriptor::ReduceRealSum(volume);
  ParallelDescriptor::ReduceRealMax(maxnorm.dataPtr(), nsoln);

  for (int i=0; i<nsoln; i++) {
    twonorm[i] = sqrt(twonorm[i] / volume); 
  }
 
  std::cout << std::setprecision(15);
  if (ParallelDescriptor::IOProcessor()) {
    if (iCpp >= 0) {
      std::cout << "----------------------------------------" << std::endl;
      std::cout << "BoxLib_C: not implemented yet. " << std::endl;
      //      std::cout << "BoxLib_C: max-norm error = "<< maxnorm[iCpp] << std::endl;
      //      std::cout << "BoxLib_C:   2-norm error = "<< twonorm[iCpp] << std::endl;
    }
    if (iF90 >= 0) {
      std::cout << "----------------------------------------" << std::endl;
      std::cout << "BoxLib_F: max-norm error = "<< maxnorm[iF90] << std::endl;
      std::cout << "BoxLib_F:   2-norm error = "<< twonorm[iF90] << std::endl;
    }
    if (iHyp >= 0) {
      std::cout << "----------------------------------------" << std::endl;
      std::cout << "Hypre: max-norm error = "<< maxnorm[iHyp] << std::endl;
      std::cout << "Hypre:   2-norm error = "<< twonorm[iHyp] << std::endl;
    }
    std::cout << "----------------------------------------" << std::endl;
  }

  return;
}

