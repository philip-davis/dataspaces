#include "multifab_tools.h"
 
double * data_ptr(MultiFab *mFab) {
  double * data = NULL;
  for( MFIter mfi(*(mFab)); mfi.isValid(); ++mfi){
    BL_ASSERT( data == NULL );
    data = ((*mFab)[mfi]).dataPtr();
  }
  BL_ASSERT( data != NULL );
  return data;
}

