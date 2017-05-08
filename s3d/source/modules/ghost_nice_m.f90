#include "globalDefines.h"
!$Id: ghost_nice_m.f90,v 1.1.2.1 2006/07/13 19:34:37 rsankar Exp $
!----------------------------------------------------------------------
! ******* Written by Ramanan Sankaran. ********
!----------------------------------------------------------------------
! New module to fill ghostzones. 
!----------------------------------------------------------------------
! This module does not use derived data types.
! Uses generic interface so that ghost zones can be filled for 
! arrays of any data type. 
! The actual subroutines are present in another file and are #inclooded
!----------------------------------------------------------------------
module ghost_nice_m
implicit none

public ghostzone_real, ghostzone_integer, ghostzone_logical

private

contains

#define GHOSTSUBNAME ghostzone_real
#define GHOSTFORTRANTYPE real
#define GHOSTMPITYPE MPI_REAL8
#include "ghost_nice_typefree.f90"

#undef GHOSTSUBNAME
#define GHOSTSUBNAME ghostzone_integer
#undef GHOSTFORTRANTYPE
#define GHOSTFORTRANTYPE integer
#undef GHOSTMPITYPE
#define GHOSTMPITYPE MPI_INTEGER
#include "ghost_nice_typefree.f90"

#undef GHOSTSUBNAME
#define GHOSTSUBNAME ghostzone_logical
#undef GHOSTFORTRANTYPE
#define GHOSTFORTRANTYPE logical
#undef GHOSTMPITYPE
#define GHOSTMPITYPE MPI_LOGICAL
#include "ghost_nice_typefree.f90"

end module ghost_nice_m
