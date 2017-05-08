!#define PAOSPEC 0
!=========================================================================================
      module espectrum_m

      contains

      subroutine  e_spectrum(mag_k, e_spec, ke, kd)

      implicit none

      real mag_k
      real e_spec
      real ke(*)
      real kd
!  local variables
      integer iopt
      parameter (iopt = 2)
      real pi
      real rat1_k
      real rat2_k
      real vk
      real pao
!
      pi = 4. * atan (1.)
!
!  Transform ke and kd into  wavenumbers
!
!     ke = 2.0 * pi / ke
!     kd = 2.0 * pi / kd

!
!
! hardwire in kx and kd
!
      if (iopt.eq.1) then
        if (mag_k .ge. ke(1)-ke(2)/2.0                                               &
      .and. mag_k .le. ke(1)+ke(2)/2.0) then
          e_spec = 1.0
        else
          e_spec = 0.0
        endif
      else
!#if PAOSPEC
!        rat1_k = mag_k / ke(1)
!        rat2_k = mag_k / kd
!        vk = rat1_k**4 / ( 1.0 + rat1_k**2 )**(17./6.)
!        pao = exp ( -2.25 * rat2_k**(4./3.) )
!        e_spec = vk * pao
!#else
        rat1_k = mag_k / ke(1)
        e_spec = rat1_k**4*exp(-2.0*rat1_k*rat1_k)
!#endif
      endif

!     write(10,*) mag_k, e_spec
!     write(*,*)'vk,pao,rat1_k ...',vk,pao,rat1_k
      return
      end subroutine e_spectrum

      end module espectrum_m
