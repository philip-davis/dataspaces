#include "globalDefines.h"

  SUBROUTINE FINISH_STEP(io,finish )

  use variables_m, only : q, u, temp, pressure, yspecies, volum                 
!  USE HDF5
  use topology_m
  use grid_m
  use param_m, only: nx, ny, nz,n_spec, npx, npy,npz, nx_g, ny_g, nz_g, iselect
!  use s3d_hdf_interface_m  !no longer used
  use reference_m
  use chemkin_m, only: reaction_rate
!  use zclookup_m
  use transport_m
  use thermchem_m
!  use premix_drvd_var_m
  use timescales_m
  IMPLICIT NONE
  integer, intent(in) :: io
  logical, intent(in) :: finish

  real Const1, Const2, Const3
  real y_centre, Y_edge, u_centre, u_edge, rho_centre, rho_edge, maxgradYM, maxgradUM
  real rhodiss_ref

  real, dimension(nx,ny,nz) :: debug


if(myid.eq.0)write(io,*)'***** starting finish step *****'
        
allocate(KE(nxf,nyf,nzf,2,2))
allocate(EPS(nxf,nyf,nzf,2,2))
allocate(CONV(nxf,nyf,nzf,2,2,1))
allocate(DIFF(nxf,nyf,nzf,2,2,1))
!allocate(DISS(nxf,nyf,nzf,3,3,4))
allocate(DENS(nxf,nyf,nzf))

!allocate(T1a(nxf,nyf,nzf,3,3,4))
allocate(T1b(nxf,nyf,nzf,3,3,1))
!allocate(T2(nxf,nyf,nzf,3,3,4))
!allocate(T3a(nxf,nyf,nzf,3,3,4))
!allocate(T3b(nxf,nyf,nzf,3,3,4))
allocate(T3c(nxf,nyf,nzf,2,2,1))
!allocate(T4(nxf,nyf,nzf,3,3,4))
!allocate(T5(nxf,nyf,nzf,3,3,4))

allocate(sumgradYM(nxf,nyf,nzf,2))
allocate(YflucError(nxf,nyf,nzf,2))
sumgradYM = 0.0
YflucError = 0.0

allocate(sqCONV(nxf,nyf,nzf,2,2,1))
allocate(sqDIFF(nxf,nyf,nzf,2,2,1))
allocate(sqT1b(nxf,nyf,nzf,3,3,1))
allocate(sqT3c(nxf,nyf,nzf,2,2,1))
allocate(sqDENS(nxf,nyf,nzf))
sqCONV=0.0
sqDIFF=0.0
sqT1b=0.0
sqT3c=0.0
sqDENS = 0.0

allocate(PDFCsum(nxf,nyf,nzf))
allocate(PDFDsum(nxf,nyf,nzf))
allocate(PDFCcond(nxf,nyf,nzf,ncond+1))
allocate(PDFDcond(nxf,nyf,nzf,ncond+1))


! ok up to here

!call mpi_barrier(gcomm,ierr)
!if(myid.eq.0)write(io,*)'***** allocations made *****'

! Find the Reynolds averaged progress variable PDF as a function of y and zeta. 
! Note that Cpdfcond and Dpdfcond use a different windowy from the other
! quantities which are cross stream averaged.
  PDFCsum(:,:,:)=0.0
  PDFDsum(:,:,:)=0.0
  do ic=2,ncond+1  ! NB. the ic=1 is the unconditional average
  PDFCsum(:,:,:)=PDFCsum+Cpdfcond(:,:,:,ic)
  PDFDsum(:,:,:)=PDFDsum+Dpdfcond(:,:,:,ic)
  enddo

  do ic=1,ncond+1
  where(PDFCsum(:,:,:).gt.1.0D-10)
  PDFCcond(:,:,:,ic)=Cpdfcond(:,:,:,ic)/PDFCsum(:,:,:)/REAL(NCOND)
  elsewhere
  PDFCcond(:,:,:,ic)=0.0
  endwhere

  where(PDFDsum(:,:,:).gt.1.0D-10)
  PDFDcond(:,:,:,ic)=Dpdfcond(:,:,:,ic)/PDFDsum(:,:,:)/REAL(NCOND)
  elsewhere
  PDFDcond(:,:,:,ic)=0.0
  endwhere
  enddo



 where(CFcond(:,:,:,:).ne.0.0)
 do m=1,2
 do n=1,2
 gradCheck(:,:,:,m,n,:)=gradCheck(:,:,:,m,n,:)/CFcond
 enddo
 enddo
 do m=1,2
 CosAlpha(:,:,:,m,:)=CosAlpha(:,:,:,m,:)/CFcond
 enddo
 do m=1,9
 FlamNormGrad(:,:,:,m,:)=FlamNormGrad(:,:,:,m,:)/CFcond
 enddo
 elsewhere
 do m=1,2
 do n=1,2
 gradCheck(:,:,:,m,n,:)=0.0
 enddo
 CosAlpha(:,:,:,m,:)=0.0
 enddo
 do m=1,9
 FlamNormGrad(:,:,:,m,:)=0.0
 enddo
 endwhere

 where(DFILcond(:,:,:,:).ne.0.0)
 do n=1,2
 do m=1,2
 YPYPcond(:,:,:,m,n,:)=YPYPcond(:,:,:,m,n,:)/DFILcond(:,:,:,:)
 enddo
 enddo
 elsewhere
 do m=1,2
 do n=1,2
 YPYPcond(:,:,:,m,n,:)=0.0
 enddo
 enddo
 endwhere

 where(DFILcond(:,:,:,:).ne.0.0)
 do m=1,2
 Ycond(:,:,:,m,:)=Ycond(:,:,:,m,:)/DFILcond(:,:,:,:)
 Wcond(:,:,:,m,:)=Wcond(:,:,:,m,:)/DFILcond(:,:,:,:)
 enddo
 elsewhere
 do m=1,2
 Ycond(:,:,:,m,:)=0.0
 Wcond(:,:,:,m,:)=0.0
 enddo
 endwhere

 do m=1,2
 do n=1,2
 YPYPcond(:,:,:,m,n,:)=YPYPcond(:,:,:,m,n,:)-Ycond(:,:,:,m,:)*Ycond(:,:,:,n,:)
 enddo
 enddo
 

 AF(:,:,:)=AF(:,:,:)/CF(:,:,:)  !since we require a Reynolds average of </rho /alpha>
 VF(:,:,:)=VF(:,:,:)/DFIL(:,:,:)
 RHOSL(:,:,:)=RHOSL(:,:,:)/DFIL(:,:,:)
 DENS(:,:,:)=DFIL(:,:,:)/CF(:,:,:)

if(myid.eq.0)write(io,*)'COUNTER IS: ',COUNTER, sqrt(COUNTER)

!  sqAF = sqrt(abs(sqAF/CF-(AF*AF)))                           !sigma thermal conduct.
 fieldz=counter/CF/CF
 sqAF = sqrt(sqAF*fieldz-(AF*AF))
 
 sqAF = 1.96*sqAF / sqrt(COUNTER)

! sqVF = sqrt(abs(sqVF-(VF*DFIL)**2))/DFIL +  &               ! uncertainty in viscosity
!        sqrt(abs(sqDFIL-DFIL**2))*VF/DFIL                    ! uncertainty in density

! sqVF = VF* (  (sqrt(sqVF/CF - ((VF*DFIL)/CF)**2)/(VF*DFIL/CF))  &     ! (where) should it be CF or COUNTER ????
!              +(sqrt(sqDFIL/CF - (DFIL/CF)**2)/(DFIL/CF)) )

 sqVF = VF* ( sqrt(sqVF*fieldz   - (VF*DFIL/CF)**2)/(VF*DFIL/CF) &
             +sqrt(sqDFIL*fieldz - (DFIL/CF)**2)/(DFIL/CF) )
 
 sqVF = 1.96*sqVF / sqrt(COUNTER)                        ! total width of 95% confidence interval

! sqRHOSL = sqrt(abs(sqRHOSL-(RHOSL*DFIL)**2))/DFIL +  &      ! 
!           sqrt(abs(sqDFIL-DFIL**2))*RHOSL/DFIL              ! uncertainty in density

 sqRHOSL = RHOSL* (  (sqrt(sqRHOSL*fieldz - ((RHOSL*DFIL)/CF)**2)/(RHOSL*DFIL/CF))  &     ! (where) should it be CF or COUNTER ????
               +(sqrt(sqDFIL*fieldz - (DFIL/CF)**2)/(DFIL/CF)) )
            
 sqRHOSL = 1.96*sqRHOSL / sqrt(COUNTER)                  ! total width of 95% confidence interval
 
 sqDENS = sqrt(abs(sqDFIL*fieldz-(DENS*DENS)))          
 sqDENS = 1.96*sqDENS / sqrt(COUNTER)
 
!error assessment
 do i=1,3
   do m=1,2
     sumrhogradYI(:,:,:,m,i)=sumrhogradYI(:,:,:,m,i)/DFIL(:,:,:)
     sumgradYP2(:,:,:,m,i)=sumgradYP2(:,:,:,m,i)/CF(:,:,:)
   enddo
 enddo
 do m=1,2
 sumrhogradYI(:,:,:,m,1) = sumrhogradYI(:,:,:,m,1) &
                         + sumrhogradYI(:,:,:,m,2) &
                         + sumrhogradYI(:,:,:,m,3)
 sumgradYP2(:,:,:,m,1) = sumgradYP2(:,:,:,m,1) &
                       + sumgradYP2(:,:,:,m,2) &
                       + sumgradYP2(:,:,:,m,3)
 sumgradYM(:,:,:,m) = gradYM(:,:,:,m,1) &
                    + gradYM(:,:,:,m,2) &
                    + gradYM(:,:,:,m,3)

 YflucError(:,:,:,m) = ((sumrhogradYI(:,:,:,m,1)-sumgradYM(:,:,:,m))**2.0/  &
                            sumgradYP2(:,:,:,m,1))**0.5
 enddo
 
 do i=1,3
 UFIL(:,:,:,i)=UFIL(:,:,:,i)/DFIL(:,:,:)
 enddo
 do i=1,2!3
 YFIL(:,:,:,i)=YFIL(:,:,:,i)/DFIL(:,:,:)
 YcFIL(:,:,:,i)=YcFIL(:,:,:,i)/DFIL(:,:,:)
 enddo
 
 do j=1,2!3
 do i=1,2!3
 KE(:,:,:,i,j)=YPYPF(:,:,:,i,j)/DFIL(:,:,:)
 YYcFIL(:,:,:,i,j)=YYcFIL(:,:,:,i,j)/DFIL(:,:,:)-YcFIL(:,:,:,i)*YcFIL(:,:,:,j)
 enddo
 enddo
! KE(:,:,:,2,3)=KE(:,:,:,3,2)
 KE(:,:,:,1,2)=KE(:,:,:,2,1)
! KE(:,:,:,1,3)=KE(:,:,:,3,1)

 KEMECH(:,:,:)=KEMECH(:,:,:)/DFIL(:,:,:)
 EPSMECH(:,:,:)=EPSMECH(:,:,:)/DFIL(:,:,:)
 do n=1,3
 do m=1,3
 KEMECH_TENS(:,:,:,m,n)=KEMECH_TENS(:,:,:,m,n)/DFIL(:,:,:)
 EPSMECH_TENS(:,:,:,m,n)=EPSMECH_TENS(:,:,:,m,n)/DFIL(:,:,:)
 enddo
 enddo


 do i=1,3
!  sqUFIL(:,:,:,i) = &
!     sqrt(abs(sqUFIL(:,:,:,i)-(UFIL(:,:,:,i)*DFIL(:,:,:))**2))/DFIL(:,:,:) +  &               
!     sqrt(abs(sqDFIL(:,:,:)-DFIL(:,:,:)**2))*UFIL(:,:,:,i)/DFIL(:,:,:)                    

 sqUFIL(:,:,:,i) = UFIL(:,:,:,i)* &
   (  (sqrt(sqUFIL(:,:,:,i)*fieldz(:,:,:) - ((UFIL(:,:,:,i)*DFIL(:,:,:))/CF(:,:,:))**2)/(UFIL(:,:,:,i)*DFIL(:,:,:)/CF(:,:,:)))  &   
     +(sqrt(sqDFIL(:,:,:)*fieldz(:,:,:) - (DFIL(:,:,:)/CF(:,:,:))**2)/(DFIL(:,:,:)/CF(:,:,:))) )    
  sqUFIL(:,:,:,i) = 1.96*sqUFIL(:,:,:,i) / sqrt(COUNTER)
 enddo
 do i=1,2
!  sqYFIL(:,:,:,i) = &
!     sqrt(abs(sqYFIL(:,:,:,i)-(YFIL(:,:,:,i)*DFIL(:,:,:))**2))/DFIL(:,:,:) +  &
!     sqrt(abs(sqDFIL(:,:,:)-DFIL(:,:,:)**2))*YFIL(:,:,:,i)/DFIL(:,:,:)   
  sqYFIL(:,:,:,i) = YFIL(:,:,:,i)* &
   (  (sqrt(sqYFIL(:,:,:,i)*fieldz(:,:,:) - ((YFIL(:,:,:,i)*DFIL(:,:,:))/CF(:,:,:))**2)/(YFIL(:,:,:,i)*DFIL(:,:,:)/CF(:,:,:)))  &     
     +(sqrt(sqDFIL(:,:,:)*fieldz(:,:,:) - (DFIL(:,:,:)/CF(:,:,:))**2)/(DFIL(:,:,:)/CF(:,:,:))) )
  sqYFIL(:,:,:,i) = 1.96*sqYFIL(:,:,:,i) / sqrt(COUNTER)
  sqYcFIL(:,:,:,i) = YcFIL(:,:,:,i)* &
   (  (sqrt(sqYcFIL(:,:,:,i)*fieldz(:,:,:) - ((YcFIL(:,:,:,i)*DFIL(:,:,:))/CF(:,:,:))**2)/(YcFIL(:,:,:,i)*DFIL(:,:,:)/CF(:,:,:)))  &
     +(sqrt(sqDFIL(:,:,:)*fieldz(:,:,:) - (DFIL(:,:,:)/CF(:,:,:))**2)/(DFIL(:,:,:)/CF(:,:,:))) )
  sqYcFIL(:,:,:,i) = 1.96*sqYcFIL(:,:,:,i) / sqrt(COUNTER)
 enddo

 do j=1,2
 do i=1,2
!  sqYPYPF(:,:,:,i,j) = &
!     sqrt(abs(sqYPYPF(:,:,:,i,j)-(KE(:,:,:,i,j)*DFIL(:,:,:))**2))/DFIL(:,:,:) +  &
!     sqrt(abs(sqDFIL(:,:,:)-DFIL(:,:,:)**2))*KE(:,:,:,i,j)/DFIL(:,:,:)       
     
 sqYPYPF(:,:,:,i,j) = KE(:,:,:,i,j)* &
 (  (sqrt(sqYPYPF(:,:,:,i,j)*fieldz(:,:,:) - ((KE(:,:,:,i,j)*DFIL(:,:,:))/CF(:,:,:))**2)/(KE(:,:,:,i,j)*DFIL(:,:,:)/CF(:,:,:)))  &     
   +(sqrt(sqDFIL(:,:,:)*fieldz(:,:,:) - (DFIL(:,:,:)/CF(:,:,:))**2)/(DFIL(:,:,:)/CF(:,:,:))) )

  sqYPYPF(:,:,:,i,j) = 1.96*sqYPYPF(:,:,:,i,j) / sqrt(COUNTER)

! sqYYcFIL(:,:,:,i,j) = YYcFIL(:,:,:,i,j)* &
! (  (sqrt(sqYYcFIL(:,:,:,i,j)*fieldz(:,:,:) - ((YYcFIL(:,:,:,i,j)*DFIL(:,:,:))/CF(:,:,:))**2)/(YYcFIL(:,:,:,i,j)*DFIL(:,:,:)/CF(:,:,:)))  &
!   +(sqrt(sqDFIL(:,:,:)*fieldz(:,:,:) - (DFIL(:,:,:)/CF(:,:,:))**2)/(DFIL(:,:,:)/CF(:,:,:))) )
!
!  sqYYcFIL(:,:,:,i,j) = 1.96*sqYYcFIL(:,:,:,i,j) / sqrt(COUNTER)

 enddo
 enddo
 sqYPYPF(:,:,:,1,2) = sqYPYPF(:,:,:,2,1)                       
 sqYYcFIL(:,:,:,1,2)= sqYYcFIL(:,:,:,2,1)
          
! sqKEMECH = &
!    sqrt(abs(sqKEMECH-(KEMECH*DFIL)**2))/DFIL +  & 
!    sqrt(abs(sqDFIL-DFIL(:,:,:)**2))*KEMECH/DFIL               

  sqKEMECH = KEMECH* &
    (  (sqrt(sqKEMECH*fieldz(:,:,:) - ((KEMECH*DFIL)/CF)**2)/(KEMECH*DFIL/CF))  &    
      +(sqrt(sqDFIL*fieldz(:,:,:) - (DFIL/CF)**2)/(DFIL/CF)) )
    
 sqKEMECH = 1.96*sqKEMECH/ sqrt(COUNTER)

! sqEPSMECH = &
!    sqrt(abs(sqEPSMECH-(EPSMECH*DFIL)**2))/DFIL +  &                               
!    sqrt(abs(sqDFIL-DFIL(:,:,:)**2))*EPSMECH/DFIL  


  sqEPSMECH(:,:,:) = EPSMECH(:,:,:)* &
    (  (sqrt(sqEPSMECH(:,:,:)*fieldz(:,:,:) - ((EPSMECH(:,:,:)*DFIL)/CF)**2)/(EPSMECH(:,:,:)*DFIL/CF))  &
      +(sqrt(sqDFIL*fieldz(:,:,:) - (DFIL/CF)**2)/(DFIL/CF)) )

 sqEPSMECH(:,:,:) = 1.96*sqEPSMECH(:,:,:)/ sqrt(COUNTER)

  do n=1,3
  do m=1,3
  sqKEMECH_tens(:,:,:,m,n) = KEMECH_tens(:,:,:,m,n)* &
    (  (sqrt(sqKEMECH_tens(:,:,:,m,n)*fieldz(:,:,:) - ((KEMECH_tens(:,:,:,m,n)*DFIL)/CF)**2)/(KEMECH_tens(:,:,:,m,n)*DFIL/CF))  &
      +(sqrt(sqDFIL*fieldz(:,:,:) - (DFIL/CF)**2)/(DFIL/CF)) )

 sqKEMECH_tens(:,:,:,m,n) = 1.96*sqKEMECH_tens(:,:,:,m,n)/ sqrt(COUNTER)


  sqEPSMECH_tens(:,:,:,m,n) = EPSMECH_tens(:,:,:,m,n)* &
    (  (sqrt(sqEPSMECH_tens(:,:,:,m,n)*fieldz(:,:,:) - ((EPSMECH_tens(:,:,:,m,n)*DFIL)/CF)**2)/(EPSMECH_tens(:,:,:,m,n)*DFIL/CF))  & 
      +(sqrt(sqDFIL*fieldz(:,:,:) - (DFIL/CF)**2)/(DFIL/CF)) )
    
 sqEPSMECH_tens(:,:,:,m,n) = 1.96*sqEPSMECH_tens(:,:,:,m,n)/ sqrt(COUNTER)
  enddo
  enddo

 !NB, it is possible to check that Y" is found ok, by alternatively evaluating KE using YY-Y.Y
 
 do j=1,2!3
 do i=1,2!3
 EPS(:,:,:,i,j)=AdYPdYPF(:,:,:,i,j)/DFIL(:,:,:)

! sqAdYPdYPF(:,:,:,i,j)=  &
!   sqrt(abs(sqAdYPdYPF(:,:,:,i,j)-(AdYPdYPF(:,:,:,i,j)*DFIL(:,:,:))**2))/DFIL(:,:,:) +  &            
!   sqrt(abs(sqDFIL(:,:,:)-DFIL(:,:,:)**2))*AdYPdYPF(:,:,:,i,j)/DFIL(:,:,:)

 sqAdYPdYPF(:,:,:,i,j) = EPS(:,:,:,i,j)* &
  (  (sqrt(sqAdYPdYPF(:,:,:,i,j)*fieldz(:,:,:) - ((EPS(:,:,:,i,j)*DFIL(:,:,:))/CF(:,:,:))**2)/(EPS(:,:,:,i,j)*DFIL(:,:,:)/CF(:,:,:)))  &
     +(sqrt(sqDFIL(:,:,:)*fieldz(:,:,:) - (DFIL(:,:,:)/CF(:,:,:))**2)/(DFIL(:,:,:)/CF(:,:,:))) )
   
 EPS(:,:,:,i,j)=invLe(i,j)*AdYPdYPF(:,:,:,i,j)/DFIL(:,:,:)
 sqAdYPdYPF(:,:,:,i,j)=invLe(i,j)*1.96*sqAdYPdYPF(:,:,:,i,j)/sqrt(COUNTER)
 enddo
 enddo

 do i=1,2
 AdYPdYPF_MOD(:,:,:,i)=invLe(i,2)*AdYPdYPF_MOD(:,:,:,i)/DFIL(:,:,:)
 enddo

 EPS(:,:,:,1,2)=EPS(:,:,:,2,1) 
 sqAdYPdYPF(:,:,:,1,2)=sqAdYPdYPF(:,:,:,2,1)

if(myid.eq.0)write(io,*)'***** eps done *****'
 
do n=1,2
do m=1,2
KEDIFF(:,:,:,m,n)=invLe(m,n)*KEDIFF(:,:,:,m,n)/CF(:,:,:)
KETRIP(:,:,:,m,n)=KETRIP(:,:,:,m,n)/CF(:,:,:)
KET1(:,:,:,m,n)=KET1(:,:,:,m,n)/CF(:,:,:)
do i=1,3
  KET1b(:,:,:,m,n,i)=KET1b(:,:,:,m,n,i)/CF(:,:,:)
enddo
KET3(:,:,:,m,n)=KET3(:,:,:,m,n)/CF(:,:,:)
KET4(:,:,:,m,n)=KET4(:,:,:,m,n)/CF(:,:,:)
KET5(:,:,:,m,n)=KET5(:,:,:,m,n)/CF(:,:,:)

sqKEDIFF(:,:,:,m,n) = invLe(m,n)*sqrt(abs(sqKEDIFF(:,:,:,m,n)*fieldz(:,:,:)-KEDIFF(:,:,:,m,n)**2)) 
sqKEDIFF(:,:,:,m,n) = 1.96*sqKEDIFF(:,:,:,m,n) / sqrt(COUNTER)
sqKET1(:,:,:,m,n) = sqrt(abs(sqKET1(:,:,:,m,n)*fieldz(:,:,:)-KET1(:,:,:,m,n)**2))  
sqKET1(:,:,:,m,n) = 1.96*sqKET1(:,:,:,m,n) / sqrt(COUNTER)
sqKET1b=0.0
sqKET3(:,:,:,m,n) = sqrt(abs(sqKET3(:,:,:,m,n)*fieldz(:,:,:)-KET3(:,:,:,m,n)**2))  
sqKET3(:,:,:,m,n) = 1.96*sqKET3(:,:,:,m,n) / sqrt(COUNTER)
sqKET4(:,:,:,m,n) = sqrt(abs(sqKET4(:,:,:,m,n)*fieldz(:,:,:)-KET4(:,:,:,m,n)**2))  
sqKET4(:,:,:,m,n) = 1.96*sqKET4(:,:,:,m,n) / sqrt(COUNTER)
sqKET5(:,:,:,m,n) = sqrt(abs(sqKET5(:,:,:,m,n)*fieldz(:,:,:)-KET5(:,:,:,m,n)**2))  
sqKET5(:,:,:,m,n) = 1.96*sqKET5(:,:,:,m,n) / sqrt(COUNTER)

enddo
enddo

do i=1,3
do m=1,2
KET2(:,:,:,m,i)=KET2(:,:,:,m,i)/CF(:,:,:)
sqKET2(:,:,:,m,i) = sqrt(abs(sqKET2(:,:,:,m,i)*fieldz(:,:,:)-KET2(:,:,:,m,i)**2))
sqKET2(:,:,:,m,i) = 1.96*sqKET2(:,:,:,m,i) / sqrt(COUNTER)
enddo
enddo
    
 ! Convection term (This is put on the RHS so that it has a '-' sign)
  do j=1,2!3
  do i=1,2!3
!    field(:,:,:)= - DFIL(:,:,:)/CF(:,:,:) * UFIL(:,:,:,1) * EPS(:,:,:,i,j)  !NB UFIL has already been divided by DFIL.
!    call derivative_x(nx,ny,nz, field(:,:,:), CONV(:,:,:,i,j,1),scale_1x,1)
!    field(:,:,:)= - DFIL(:,:,:)/CF(:,:,:) * UFIL(:,:,:,2) * EPS(:,:,:,i,j) 
!    call derivative_y(nx,ny,nz, field(:,:,:), CONV(:,:,:,i,j,2),scale_1y,1)
!    field(:,:,:)= - DFIL(:,:,:)/CF(:,:,:) * UFIL(:,:,:,3) * EPS(:,:,:,i,j) 
!    call derivative_z(nx,ny,nz, field(:,:,:), CONV(:,:,:,i,j,3),scale_1z,1)

    field(:,:,:)= - DENS(:,:,:) * UFIL(:,:,:,1) * EPS(:,:,:,i,j)  !NB UFIL has already been divided by DFIL.
    call derivative_x(nx,ny,nz, field(:,:,:), fieldx(:,:,:),scale_1x,1)     
    field(:,:,:)= - DENS(:,:,:) * UFIL(:,:,:,2) * EPS(:,:,:,i,j) 
    call derivative_y(nx,ny,nz, field(:,:,:), fieldy(:,:,:),scale_1y,1)
    field(:,:,:)= - DENS(:,:,:) * UFIL(:,:,:,3) * EPS(:,:,:,i,j) 
    call derivative_z(nx,ny,nz, field(:,:,:), fieldz(:,:,:),scale_1z,1)
    CONV(:,:,:,i,j,1)=fieldx(:,:,:)+fieldy(:,:,:)+fieldz(:,:,:)
    sqCONV(:,:,:,i,j,1)=0.0
  enddo
  enddo
 
 ! Diffusion term (This is moved to the RHS so that it has a '+' sign)
 
  do j=1,2!3
  do i=1,2!3
    call derivative_x(nx,ny,nz, EPS(:,:,:,i,j), field(:,:,:),scale_1x,1)
    field(:,:,:)=field(:,:,:) * AF(:,:,:) * invLe(i,j)  !NB AF is already normalised
    call derivative_x(nx,ny,nz, field(:,:,:), fieldx(:,:,:),scale_1x,1)
    call derivative_y(nx,ny,nz, EPS(:,:,:,i,j), field(:,:,:),scale_1y,1)
    field(:,:,:)=field(:,:,:) * AF(:,:,:) * invLe(i,j) 
    call derivative_y(nx,ny,nz, field(:,:,:), fieldy(:,:,:),scale_1y,1)
    call derivative_z(nx,ny,nz, EPS(:,:,:,i,j), field(:,:,:),scale_1z,1)
    field(:,:,:)=field(:,:,:) * AF(:,:,:) * invLe(i,j)   !NB AF is already normalised
    call derivative_z(nx,ny,nz, field(:,:,:), fieldz(:,:,:),scale_1z,1)
    DIFF(:,:,:,i,j,1)=fieldx(:,:,:)+fieldy(:,:,:)+fieldz(:,:,:)
    sqDIFF(:,:,:,i,j,1) = 0.0
  enddo
  enddo
 
 ! Dissipation term (This is moved to the RHS so that it has a '-' sign)
!  DISS(:,:,:,:,:,4)=0.0
  do n=1,2!3
  do m=1,2!3
    DISSvar(:,:,:,m,n,1)=-DISSvar(:,:,:,m,n,1)/CF(:,:,:) * invLe(m,n) * invLe(m,n) 

    fieldz=counter/CF/CF
    sqDissvar(:,:,:,m,n,1)=sqrt(abs(sqDissVar(:,:,:,m,n,1)*fieldz(:,:,:)-(DissVAR(:,:,:,m,n,1)**2)))
    sqDissvar(:,:,:,m,n,1)=1.96*sqDissVar(:,:,:,m,n,1) / sqrt(COUNTER)
  enddo
  enddo

 ! T1a
  do n=1,2!3
  do m=1,2!3
    field(:,:,:)=T1aVar(:,:,:,m,n,1)/CF(:,:,:)*invLe(m,n)
    call derivative_x(nx,ny,nz, field(:,:,:), fieldx(:,:,:),scale_1x,1)
    field(:,:,:)=T1aVar(:,:,:,m,n,2)/CF(:,:,:)*invLe(m,n) 
    call derivative_y(nx,ny,nz, field(:,:,:), fieldy(:,:,:),scale_1y,1)
    field(:,:,:)=T1aVar(:,:,:,m,n,3)/CF(:,:,:)*invLe(m,n)
    call derivative_z(nx,ny,nz, field(:,:,:), fieldz(:,:,:),scale_1z,1)
    T1aVar(:,:,:,m,n,1)=fieldx(:,:,:)+fieldy(:,:,:)+fieldz(:,:,:)

    sqT1aVar(:,:,:,m,n,1)=0.0
    
  enddo
  enddo
  T1aVar=-T1aVar
 
 ! T1b


! the following line is a slight approximation to T1bvar but it might be smoother. this term
! is expected to be small anyway (swami and bray). see how it goes .....
do j=1,3
do i=1,3
do m=1,2
T1bVar(:,:,:,m,i,j)=AF(:,:,:)*(UdYVar(:,:,:,m,i,j)/DFIL(:,:,:)+UFIL(:,:,:,i)*gradYM(:,:,:,m,j))
enddo
enddo
enddo



  T1b(:,:,:,:,:,:)=0.0
  do n=1,2!3  !species loop
  do m=1,2!3  !species loop
    call derivative_x(nx,ny,nz, YFIL(:,:,:,n), fieldx(:,:,:),scale_1x,1)
    call derivative_y(nx,ny,nz, YFIL(:,:,:,n), fieldy(:,:,:),scale_1y,1)
    call derivative_z(nx,ny,nz, YFIL(:,:,:,n), fieldz(:,:,:),scale_1z,1)
 
    call derivative_x(nx,ny,nz, fieldx(:,:,:), field(:,:,:),scale_1x,1)
    T1b(:,:,:,m,n,1)=T1b(:,:,:,m,n,1)+T1bVar(:,:,:,m,1,1)*field(:,:,:)
    call derivative_x(nx,ny,nz, fieldy(:,:,:), field(:,:,:),scale_1x,1)
    T1b(:,:,:,m,n,1)=T1b(:,:,:,m,n,1)+T1bVar(:,:,:,m,2,1)*field(:,:,:)
    call derivative_x(nx,ny,nz, fieldz(:,:,:), field(:,:,:),scale_1x,1)
    T1b(:,:,:,m,n,1)=T1b(:,:,:,m,n,1)+T1bVar(:,:,:,m,3,1)*field(:,:,:)
 
    call derivative_y(nx,ny,nz, fieldx(:,:,:), field(:,:,:),scale_1y,1)
    T1b(:,:,:,m,n,1)=T1b(:,:,:,m,n,1)+T1bVar(:,:,:,m,1,2)*field(:,:,:)
    call derivative_y(nx,ny,nz, fieldy(:,:,:), field(:,:,:),scale_1y,1)
    T1b(:,:,:,m,n,1)=T1b(:,:,:,m,n,1)+T1bVar(:,:,:,m,2,2)*field(:,:,:)
    call derivative_y(nx,ny,nz, fieldz(:,:,:), field(:,:,:),scale_1y,1)
    T1b(:,:,:,m,n,1)=T1b(:,:,:,m,n,1)+T1bVar(:,:,:,m,3,2)*field(:,:,:)
 
    call derivative_z(nx,ny,nz, fieldx(:,:,:), field(:,:,:),scale_1z,1)
    T1b(:,:,:,m,n,1)=T1b(:,:,:,m,n,1)+T1bVar(:,:,:,m,1,3)*field(:,:,:)
    call derivative_z(nx,ny,nz, fieldy(:,:,:), field(:,:,:),scale_1z,1)
    T1b(:,:,:,m,n,1)=T1b(:,:,:,m,n,1)+T1bVar(:,:,:,m,2,3)*field(:,:,:)
    call derivative_z(nx,ny,nz, fieldz(:,:,:), field(:,:,:),scale_1z,1)
    T1b(:,:,:,m,n,1)=T1b(:,:,:,m,n,1)+T1bVar(:,:,:,m,3,3)*field(:,:,:)

    T1b(:,:,:,m,n,1)=T1b(:,:,:,m,n,1)*invLe(m,n)

    sqT1b(:,:,:,m,n,1)=0.0
  enddo
  enddo
  T1b=-T1b
 
 ! T2
  do n=1,2!3
  do m=1,2!3
    T2var(:,:,:,m,n,1)=T2var(:,:,:,m,n,1)/CF(:,:,:)
    fieldz=counter/CF/CF     
    sqT2var(:,:,:,m,n,1)=sqrt(abs(sqT2var(:,:,:,m,n,1)*fieldz(:,:,:)-T2Var(:,:,:,m,n,1)**2))
    sqT2var(:,:,:,m,n,1)=1.96*sqT2var(:,:,:,m,n,1)/ sqrt(COUNTER)
  enddo
  enddo
  T2Var=-T2Var
 
! T3a
  do n=1,2!3
  do m=1,2!3
!find the uncertainty in T3avar first then combine it with the uncertainty in AF and DFIL.
! {AF*sumT3a/sumDfil} * [sqAF/AF + sqrt{sumsqT3a-sumT3a**2}/sumT3a + sqrt{sumsqDFIL-sumDFIL**2}/sumDFIL]

!    sqT3aVar(:,:,:,m,n,1)=(AF(:,:,:)*T3avar(:,:,:,m,n,1)/DFIL(:,:,:)) * &
!        (sqAF(:,:,:)/AF(:,:,:) &
!      + sqrt(abs(sqT3aVar(:,:,:,m,n,1)-T3aVar(:,:,:,m,n,1)**2))/T3aVar(:,:,:,m,n,1)  &
!      + sqrt(abs(sqDFIL(:,:,:)-DFIL(:,:,:)**2))/DFIL(:,:,:) )

field(:,:,:)=AF(:,:,:)*T3avar(:,:,:,m,n,1)/DFIL(:,:,:)*invLe(m,n)
fieldx(:,:,:)=sqT3aVar(:,:,:,m,n,1)

 fieldz=counter/CF/CF

sqT3aVar(:,:,:,m,n,1)=   field(:,:,:)* &
   sqrt(abs(fieldx*fieldz - (T3aVar(:,:,:,m,n,1)/CF)**2))/(T3aVar(:,:,:,m,n,1)/CF)  

sqT3aVar(:,:,:,m,n,1)= 1.96*sqT3aVar(:,:,:,m,n,1)/sqrt(COUNTER)  &
                         + field*(  sqDENS/DENS   &
                                  + sqAF/AF   )

    T3aVar(:,:,:,m,n,1)=field(:,:,:)
  enddo
  enddo
  T3aVar=-T3aVar 
 
 ! T3b
  do n=1,2!3
  do m=1,2!3
!    sqT3bVar(:,:,:,m,n,1)=(AF(:,:,:)*T3bvar(:,:,:,m,n,1)/DFIL(:,:,:)) * &
!        (sqAF(:,:,:)/AF(:,:,:) &
!      + sqrt(abs(sqT3bVar(:,:,:,m,n,1)-T3bVar(:,:,:,m,n,1)**2))/T3bVar(:,:,:,m,n,1)  &
!      + sqrt(abs(sqDFIL(:,:,:)-DFIL(:,:,:)**2))/DFIL(:,:,:) )        
      
field(:,:,:)=AF(:,:,:)*T3bvar(:,:,:,m,n,1)/DFIL(:,:,:)*invLe(m,n)
fieldx(:,:,:)=sqT3bVar(:,:,:,m,n,1)

!sqT3bVar(:,:,:,m,n,1)=(AF(:,:,:)*T3bvar(:,:,:,m,n,1)/DFIL(:,:,:)) * &
!      (sqAF(:,:,:)/AF(:,:,:) &
! + field(:,:,)* (  (sqrt(fieldx/CF - ((field*DFIL)/CF)**2)/(field*DFIL/CF))  &     
!                  +(sqrt(sqDFIL/CF - (DFIL/CF)**2)/(DFIL/CF)) )) 

!sqT3bVar(:,:,:,m,n,1)=   field(:,:,:)* &
!  (  (sqrt(fieldx*fieldz - ((field*DFIL)/CF)**2)/(field*DFIL/CF))      &
!    +(sqrt(sqDFIL*fieldz - (DFIL/CF)**2)/(DFIL/CF))    &
!    + sqAF/AF )
      
sqT3bVar(:,:,:,m,n,1)=   field(:,:,:)* &
    sqrt(abs(fieldx*fieldz - (T3bVar(:,:,:,m,n,1)/CF)**2))/(T3bVar(:,:,:,m,n,1)/CF)

!   &
!    + sqDENS/DENS   &
!    + sqAF/AF )

sqT3bVar(:,:,:,m,n,1)= 1.96*sqT3bVar(:,:,:,m,n,1)/sqrt(COUNTER)  &
                         + field*(  sqDENS/DENS   &
                                  + sqAF/AF   )

    T3bvar(:,:,:,m,n,1)=field(:,:,:)
  enddo
  enddo
  T3bvar=-T3bvar
 
 ! T3c
  T3c=0.0
  do n=1,2!3
  do m=1,2!3
!    call derivative_x(nx,ny,nz, YFIL(:,:,:,n), field(:,:,:),scale_1x,1)
!    T3c(:,:,:,m,n,1)=AF(:,:,:)*field(:,:,:)*T3cVar(:,:,:,m,1)/DFIL(:,:,:)
!    call derivative_y(nx,ny,nz, YFIL(:,:,:,n), field(:,:,:),scale_1y,1)
!    T3c(:,:,:,m,n,1)=T3c(:,:,:,m,n,1)+AF(:,:,:)*field(:,:,:)*T3cVar(:,:,:,m,2)/DFIL(:,:,:)
!    call derivative_z(nx,ny,nz, YFIL(:,:,:,n), field(:,:,:),scale_1z,1)
!    T3c(:,:,:,m,n,1)=T3c(:,:,:,m,n,1)+AF(:,:,:)*field(:,:,:)*T3cVar(:,:,:,m,3)/DFIL(:,:,:)

    do i=1,3
    T3c(:,:,:,m,n,1)=T3c(:,:,:,m,n,1)+AF(:,:,:)*gradYM(:,:,:,n,i)*T3cVar(:,:,:,m,i)/DFIL(:,:,:)*invLe(m,n)
    enddo

  sqT3c(:,:,:,m,n,1)= 0.0

  enddo
  enddo
  T3c=-T3c
 
 
 ! T4
  do n=1,2!3
  do m=1,2!3
    T4var(:,:,:,m,n,1)=T4var(:,:,:,m,n,1)/CF(:,:,:)*invLe(m,n)
    sqT4var(:,:,:,m,n,1) = sqrt(abs(sqT4var(:,:,:,m,n,1)*fieldz(:,:,:)-(T4var(:,:,:,m,n,1)**2)))
    sqT4var(:,:,:,m,n,1) = 1.96*sqT4var(:,:,:,m,n,1) / sqrt(COUNTER)
  enddo
  enddo
 
 ! T5
  do n=1,2!3
  do m=1,2!3
    T5var(:,:,:,m,n,1)=T5var(:,:,:,m,n,1)/CF(:,:,:)*invLe(m,n)  
    sqT5var(:,:,:,m,n,1) = sqrt(abs(sqT5var(:,:,:,m,n,1)*fieldz(:,:,:)-(T5var(:,:,:,m,n,1)**2)))
    sqT5var(:,:,:,m,n,1) = 1.96*sqT5var(:,:,:,m,n,1) / sqrt(COUNTER)
  enddo
  enddo
 



if(myid.eq.0)write(io,*)'***** evaluating final TKE terms *****'

!   do k=1,nz
!   do j=1,ny
!   field(:,j,k)=x(:)
!   enddo
!   enddo
!   call derivative_x(nx,ny,nz, field(:,:,:), fieldx(:,:,:),scale_1x,1)
!   DEBUG(:,:,:)=fieldx(:,:,:)
  

                                
!TKCONV                                
   field(:,:,:)= - DENS(:,:,:) * UFIL(:,:,:,1) * KEMECH(:,:,:)
   call derivative_x(nx,ny,nz, field(:,:,:), fieldx(:,:,:),scale_1x,1)     
   DEBUG(:,:,:)=fieldx(:,:,:)
   field(:,:,:)= - DENS(:,:,:) * UFIL(:,:,:,2) * KEMECH(:,:,:)
   call derivative_y(nx,ny,nz, field(:,:,:), fieldy(:,:,:),scale_1y,1)
   field(:,:,:)= - DENS(:,:,:) * UFIL(:,:,:,3) * KEMECH(:,:,:)
   call derivative_z(nx,ny,nz, field(:,:,:), fieldz(:,:,:),scale_1z,1)
   TKCONV(:,:,:)= fieldx+fieldy+fieldz
  sqTKCONV(:,:,:)=0.0
!TK1 =rhoUPUP 
  do m=1,3
  do n=1,3
    TK1(:,:,:,m,n)=TK1(:,:,:,m,n)/CF(:,:,:)
  enddo
  enddo

! the following two blocks of code should give the same values:
! option 1
!  do m=1,3
!  fieldz(:,:,:) = UFIL(:,:,:,m)
!  call derivative_x(nx,ny,nz, fieldz(:,:,:), fieldx(:,:,:),scale_1x,1)
!  call derivative_y(nx,ny,nz, fieldz(:,:,:), fieldy(:,:,:),scale_1y,1)
!  call derivative_z(nx,ny,nz, fieldz(:,:,:), fieldz(:,:,:),scale_1z,1)
!  field(:,:,:)=field(:,:,:)-fieldx*TK1(:,:,:,m,1)-fieldy*TK1(:,:,:,m,2)-fieldz*TK1(:,:,:,m,3) 
!  enddo

  
! option 2
  field(:,:,:)=0.0
  do m=1,3
  do n=1,3
  field(:,:,:) = field(:,:,:) - GRADUM(:,:,:,m,n)*TK1(:,:,:,m,n) 
  enddo
  enddo
  TK1(:,:,:,1,1) = field(:,:,:) 
  sqTK1=0.0

  field(:,:,:)=UFIL(:,:,:,1)
  call derivative_x(nx,ny,nz, field(:,:,:), fieldx(:,:,:),scale_1x,1)
  if (gradum(nx/2,ny/2,nz/2,1,1).ne.fieldx(nx/2,ny/2,nz/2).or.gradum(nx/2,ny/2,nz/2,1,1).eq.0.0)write(io,*)myid,'gradum',gradum(nx/2,ny/2,nz/2,1,1),fieldx(nx/2,ny/2,nz/2)

!  if(gradum(nx/2,ny/2,nz/2,1,1).ne.0.0)write(io,*)myid,'gradum.ne.0',gradum(nx/2,ny/2,nz/2,1,1)
  
!TK2
  do n=1,3
    TK2(:,:,:,n)=TK2(:,:,:,n)/CF(:,:,:)
  enddo
  field(:,:,:)= TK2(:,:,:,1) 
  call derivative_x(nx,ny,nz, field(:,:,:), fieldx(:,:,:),scale_1x,1)
  field(:,:,:)= TK2(:,:,:,2) 
  call derivative_y(nx,ny,nz, field(:,:,:), fieldy(:,:,:),scale_1y,1)
  field(:,:,:)= TK2(:,:,:,3) 
  call derivative_z(nx,ny,nz, field(:,:,:), fieldz(:,:,:),scale_1z,1)
  TK2(:,:,:,1) =  - 0.5 * (fieldx + fieldy + fieldz)
  sqTK2=0.0
!TK3
  do n=1,3
    TK3(:,:,:,n)=TK3(:,:,:,n)/CF(:,:,:)
  enddo
  field(:,:,:)= PM(:,:,:) 
  call derivative_x(nx,ny,nz, field(:,:,:), fieldx(:,:,:),scale_1x,1)     
  call derivative_y(nx,ny,nz, field(:,:,:), fieldy(:,:,:),scale_1y,1)
  call derivative_z(nx,ny,nz, field(:,:,:), fieldz(:,:,:),scale_1z,1)
  TK3(:,:,:,1) = - (fieldx*TK3(:,:,:,1) + fieldy*TK3(:,:,:,2) + fieldz*TK3(:,:,:,3) )
  sqTK3=0.0
!TK4
  do n=1,3
    TK4(:,:,:,n)=TK4(:,:,:,n)/CF(:,:,:)
  enddo
  field(:,:,:)= TK4(:,:,:,1) 
  call derivative_x(nx,ny,nz, field(:,:,:), fieldx(:,:,:),scale_1x,1)     
  field(:,:,:)= TK4(:,:,:,2)
  call derivative_y(nx,ny,nz, field(:,:,:), fieldy(:,:,:),scale_1y,1)
  field(:,:,:)= TK4(:,:,:,3)
  call derivative_z(nx,ny,nz, field(:,:,:), fieldz(:,:,:),scale_1z,1)
  TK4(:,:,:,1) =  - (fieldx + fieldy + fieldz)
  sqTK4=0.0
!TK5
  TK5(:,:,:)=TK5(:,:,:)/CF(:,:,:)
  sqTK5=0.0


! Darbyshire stuff:
! Y1_Y2M
  Y1_Y2M(:,:,:)=Y1_Y2M(:,:,:)/DFIL(:,:,:)
! Y1_Y2sq
  Y1_Y2sq(:,:,:)=Y1_Y2sq(:,:,:)/DFIL(:,:,:)
! YWM
  YWM(:,:,:)=YWM(:,:,:)/DFIL(:,:,:)
! EPS_C
! note that chi=2eps
  EPS_C(:,:,:)=invLe(1,1)*EPS_C(:,:,:)/DFIL(:,:,:)
  
! UYflux
  do n=1,2
  do m=1,3
  UYflux(:,:,:,m,n)=UYflux(:,:,:,m,n)/DFIL(:,:,:)-UFIL(:,:,:,m)*YFIL(:,:,:,n)
  enddo
  enddo
! UCflux
  do m=1,3
  UCflux(:,:,:,m)=UCflux(:,:,:,m)/DFIL(:,:,:)-UFIL(:,:,:,m)*(1.0-Y1_Y2M(:,:,:))
  enddo

! UCflux=<rho.u_i".c">/<rho>




!    call mpi_barrier(gcomm,ierr)
!    if(myid.eq.0)write(io,*)'***** Finishing step complete, writing HDF files *****'
!    call mpi_barrier(gcomm,ierr)

 ! plot ke, eps, etc....
 
!    call mpi_barrier(gcomm,ierr)   !probably this is not necessary.
! gather equation terms data along the y direction


! Rewrite the gathering part of the code so that it does things quantity by quantity using a 3d array for the gather

!  xplot(1)=0.25
!  xplot(2)=0.50
!  xplot(3)=0.75 ! don't use 1.00
!  
!  zplot(1)=0.00
!  zplot(2)=0.25
!  zplot(3)=0.50

do j=2,2  ! zplot
do i=2,2  ! xplot

! x direction condition:
!if(real(xid*nxf).le.xplot(i)*real(nxf*npx).and.real((xid+1)*nxf).gt.xplot(i)*real(nxf*npx))then
! the ix value at which we want to plot is then int(0.25*real(nxf*npx))-xid*nxf
ixplot=max(1,1+int(xplot(i)*real(nxf*npx-1))-xid*nxf)
ixplot=max(min(ixplot,nxf),1)
! z direction condition:
!if(real(zid*nzf).le.zplot(j)*real(nzf*npz).and.real((zid+1)*nzf).gt.zplot(j)*real(nzf*npz))then
izplot=max(1,1+int(zplot(j)*real(nzf*npz-1))-zid*nzf)
izplot=max(min(izplot,nzf),1)
if(myid.eq.0)write(io,*)'in side the loop at yid=',yid,'i,j',i,j


!if(yid.eq.0)then
!if(real(xid*nxf).le.xplot(i)*real(nxf*npx).and.real((xid+1)*nxf).gt.xplot(i)*real(nxf*npx))then    
!if(real(zid*nzf).le.zplot(j)*real(nzf*npz).and.real((zid+1)*nzf).gt.zplot(j)*real(nzf*npz))then


if(yid.eq.0 .and. &
   real(xid*nxf).le.xplot(i)*real(nxf*npx).and.real((xid+1)*nxf).gt.xplot(i)*real(nxf*npx)  .and.  &
   real(zid*nzf).le.zplot(j)*real(nzf*npz).and.real((zid+1)*nzf).gt.zplot(j)*real(nzf*npz))then


! write the stuff to a tecplot file

  write(y1id_ext,'(I1.1)') i
  write(y2id_ext,'(I1.1)') j
  write(y1rl_ext,'(e8.3)') x(ixplot)*l_ref 
  write(y2rl_ext,'(e8.3)') z(izplot)*l_ref
  if(iselect.eq.1)then
  nomspec(1)='O2'
  nomspec(2)='Xi'
  if(n_spec.eq.13.and.imf.eq.2)then
    nomspec(2)='H'  ! premixed case does not have Xi
  elseif(n_spec.eq.13.and.imf.eq.1)then
    nomspec(2)='H2'  
  elseif(n_spec.eq.13.and.imf.eq.5)then
    nomspec(2)='OH'
  elseif(n_spec.eq.13)then
    nomspec(2)='CO'
  endif
  nomspec(3)='H2'
  elseif(iselect.eq.2)then
  nomspec(1)='O2'
  nomspec(2)='H2'
  nomspec(3)='Xi'
  elseif(iselect.eq.3)then
  nomspec(1)='Xi'
  nomspec(2)='H2'
  nomspec(3)='O2'
  endif
  do ic=1,3
  write(direction(ic),'(I1.1)') ic
  enddo
  write(io,*)'y1rl_ext',y1rl_ext, y2rl_ext
!  filename = '../post/yplots/everythingX'//trim(Y1id_ext)//'Z'//trim(Y2id_ext)//'.tec'
   filename = '../post/yplots/everythingX'//trim(Y1rl_ext)//'Z'//trim(Y2rl_ext)//'.tec'

 
!call mpi_barrier(gcomm,ierr)   !probably this is not necessary.
if(myid.eq.0)write(io,*)'about to open the tecplot filed'
 
  open(unit=78,file=trim(filename),status='unknown')
!----------------------------------------------------------------------
!   write title of file

    write(78,*) 'title = "',trim(filename),'"'
!----------------------------------------------------------------------
!   write variables header
    write(78,*) 'variables = '
! write coordinates labels
    write(78,*) '"y (mm)"'
    write(78,*) '"y/H"'

!   write variables labels
    write(78,*) '"u (m/s)"'
    write(78,*) '"v (m/s)"'
    write(78,*) '"w (m/s)"'
    write(78,*) '"y1"'
    write(78,*) '"y2"'
!    write(78,*) '"y3"'

    write(78,*) '"yc1"'
    write(78,*) '"yc2"'

!write the 2d variable labels
!ke
do ic=1,2!3
do id=1,2!3
  write(78,*) '"'//trim(nomspec(ic))//trim(nomspec(id))//'"'
enddo
enddo
!ke_c
do ic=1,2!3
do id=1,2!3
  write(78,*) '"ke_c'//trim(nomspec(ic))//trim(nomspec(id))//'"'
enddo
enddo


!eps
do ic=1,2!3
do id=1,2!3
  write(78,*) '"eps_'//trim(nomspec(ic))//'_'//trim(nomspec(id))//'"'
enddo
enddo
!invTau_ij
do ic=1,2!3
do id=1,2!3
  write(78,*) '"invtau_'//trim(nomspec(ic))//'_'//trim(nomspec(id))//'"'
enddo
enddo
!Cphi_ij
do ic=1,2!3
do id=1,2!3
  write(78,*) '"Cphi_'//trim(nomspec(ic))//'_'//trim(nomspec(id))//'"'
enddo
enddo

  write(78,*) '"ke_mech (m^2/s^2)"'

do n=1,3
do m=1,3
  write(78,*) '"',m,n,'ke_mech (m^2/s^3)"'
enddo
enddo

  write(78,*) '"eps_mech (m^2/s^3)"'

do n=1,3
do m=1,3
  write(78,*) '"',m,n,'eps_mech (m^2/s^3)"'
enddo
enddo

 write(78,*) '"invtau_mech (1/s)"'

do n=1,3
do m=1,3
  write(78,*) '"',m,n,'invtau_mech (1/s)"'
enddo
enddo

  write(78,*) '"invtau_model1 (1/s)"'     !constant c_phi
  write(78,*) '"invtau_model2 (1/s)"'     !rr effect only
do ic=1,2!3
do id=1,2!3
  write(78,*) '"invtau_model3'//trim(nomspec(ic))//'_'//trim(nomspec(id))//' (1/s)"'  !diff-diff effect
enddo
enddo

  write(78,*) '"Cphi_model1"'
  write(78,*) '"Cphi_model2"'
do ic=1,2!3
do id=1,2!3
  write(78,*) '"Cphi_model3'//trim(nomspec(ic))//'_'//trim(nomspec(id))//'"'
enddo
enddo

  
!write the 3d variable labels...
! write the total first and then loop over the three directions
do ic=1,2!3
  do id=1,2!3
    write(78,*) '"VARCONV'//trim(nomspec(ic))//trim(nomspec(id))//'"'
  enddo
enddo
do ic=1,2!3
  do id=1,2!3
    write(78,*) '"VARDIFF'//trim(nomspec(ic))//trim(nomspec(id))//'"'
  enddo
enddo
!do ic=1,2!3
!  do id=1,2!3
!    write(78,*) '"VARTRIP'//trim(nomspec(ic))//trim(nomspec(id))//'"'
!  enddo
!enddo
do ic=1,2!3
  do id=1,2!3
    write(78,*) '"VARDISS'//trim(nomspec(ic))//trim(nomspec(id))//'"'
  enddo
enddo
do ic=1,2!3
  do id=1,2!3
    write(78,*) '"VART1'//trim(nomspec(ic))//trim(nomspec(id))//'"'
  enddo
enddo
do ic=1,2!3
  do id=1,2!3
    write(78,*) '"VART2'//trim(nomspec(ic))//trim(nomspec(id))//'"'
  enddo
enddo
do ic=1,2!3
  do id=1,2!3
    write(78,*) '"VART3'//trim(nomspec(ic))//trim(nomspec(id))//'"'
  enddo
enddo
do ic=1,2!3
  do id=1,2!3
    write(78,*) '"VART4'//trim(nomspec(ic))//trim(nomspec(id))//'"'
  enddo
enddo
do ic=1,2!3
  do id=1,2!3
    write(78,*) '"VART5'//trim(nomspec(ic))//trim(nomspec(id))//'"'
  enddo
enddo

do ic=1,2!3
  do id=1,2!3
    write(78,*) '"CONV'//trim(nomspec(ic))//trim(nomspec(id))//'"'
  enddo
enddo
do ic=1,2!3
  do id=1,2!3
    write(78,*) '"DIFF'//trim(nomspec(ic))//trim(nomspec(id))//'"'
  enddo
enddo
do ic=1,2!3
  do id=1,2!3
    write(78,*) '"DISS'//trim(nomspec(ic))//trim(nomspec(id))//'"'
  enddo
enddo
do ic=1,2!3
  do id=1,2!3
    write(78,*) '"T1a'//trim(nomspec(ic))//trim(nomspec(id))//'"'
  enddo
enddo
do ic=1,2!3
  do id=1,2!3
    write(78,*) '"T1b'//trim(nomspec(ic))//trim(nomspec(id))//'"'
  enddo
enddo
do ic=1,2!3
  do id=1,2!3
    write(78,*) '"T2'//trim(nomspec(ic))//trim(nomspec(id))//'"'
  enddo
enddo
do ic=1,2!3
  do id=1,2!3
    write(78,*) '"T3a'//trim(nomspec(ic))//trim(nomspec(id))//'"'
  enddo
enddo
do ic=1,2!3
  do id=1,2!3
    write(78,*) '"T3b'//trim(nomspec(ic))//trim(nomspec(id))//'"'
  enddo
enddo
do ic=1,2!3
  do id=1,2!3
    write(78,*) '"T3c'//trim(nomspec(ic))//trim(nomspec(id))//'"'
  enddo
enddo
do ic=1,2!3
  do id=1,2!3
    write(78,*) '"T4'//trim(nomspec(ic))//trim(nomspec(id))//'"'
  enddo
enddo
do ic=1,2!3
  do id=1,2!3
    write(78,*) '"T5'//trim(nomspec(ic))//trim(nomspec(id))//'"'
  enddo
enddo

!uncertainties ! uncertainties
!uncertainties 
!uncertainties !   write variables labels
!uncertainties     write(78,*) '"u_err (m/s)"'
!uncertainties     write(78,*) '"v_err (m/s)"'
!uncertainties     write(78,*) '"w_err (m/s)"'
!uncertainties     write(78,*) '"y1_err"'
!uncertainties     write(78,*) '"y2_err"'
!uncertainties !    write(78,*) '"y3_err"'
!uncertainties 
!uncertainties !write the 2d variable labels
!uncertainties !ke
!uncertainties do ic=1,2!3
!uncertainties do id=1,2!3
!uncertainties   write(78,*) '"'//trim(nomspec(ic))//trim(nomspec(id))//'_err"'
!uncertainties enddo
!uncertainties enddo
!uncertainties !!ke_c
!uncertainties !do ic=1,2!3
!uncertainties !do id=1,2!3
!uncertainties !  write(78,*) '"'//trim(nomspec(ic))//trim(nomspec(id))//'_err_c"'
!uncertainties !enddo
!uncertainties !enddo
!uncertainties !eps
!uncertainties do ic=1,2!3
!uncertainties do id=1,2!3
!uncertainties   write(78,*) '"eps_'//trim(nomspec(ic))//'_'//trim(nomspec(id))//'_err"'
!uncertainties enddo
!uncertainties enddo
!uncertainties 
!uncertainties   write(78,*) '"ke_mech_err (m^2/s^2)"'
!uncertainties   write(78,*) '"eps_mech_err (m^2/s^3)"'
!uncertainties 
!uncertainties !write the 3d variable labels...
!uncertainties ! write the total first and then loop over the three directions
!uncertainties do ic=1,2!3
!uncertainties   do id=1,2!3
!uncertainties     write(78,*) '"VARCONV'//trim(nomspec(ic))//trim(nomspec(id))//'_err"'
!uncertainties   enddo
!uncertainties enddo
!uncertainties do ic=1,2!3
!uncertainties   do id=1,2!3
!uncertainties     write(78,*) '"VARDIFF'//trim(nomspec(ic))//trim(nomspec(id))//'_err"'
!uncertainties   enddo
!uncertainties enddo
!uncertainties do ic=1,2!3
!uncertainties   do id=1,2!3
!uncertainties     write(78,*) '"VARDISS'//trim(nomspec(ic))//trim(nomspec(id))//'_err"'
!uncertainties   enddo
!uncertainties enddo
!uncertainties do ic=1,2!3
!uncertainties   do id=1,2!3
!uncertainties     write(78,*) '"VART1'//trim(nomspec(ic))//trim(nomspec(id))//'_err"'
!uncertainties   enddo
!uncertainties enddo
!uncertainties do ic=1,2!3
!uncertainties   do id=1,2!3
!uncertainties     write(78,*) '"VART2'//trim(nomspec(ic))//trim(nomspec(id))//'_err"'
!uncertainties   enddo
!uncertainties enddo
!uncertainties do ic=1,2!3
!uncertainties   do id=1,2!3
!uncertainties     write(78,*) '"VART3'//trim(nomspec(ic))//trim(nomspec(id))//'_err"'
!uncertainties   enddo
!uncertainties enddo
!uncertainties do ic=1,2!3
!uncertainties   do id=1,2!3
!uncertainties     write(78,*) '"VART4'//trim(nomspec(ic))//trim(nomspec(id))//'_err"'
!uncertainties   enddo
!uncertainties enddo
!uncertainties do ic=1,2!3
!uncertainties   do id=1,2!3
!uncertainties     write(78,*) '"VART5'//trim(nomspec(ic))//trim(nomspec(id))//'_err"'
!uncertainties   enddo
!uncertainties enddo
!uncertainties 
!uncertainties do ic=1,2!3
!uncertainties   do id=1,2!3
!uncertainties     write(78,*) '"CONV'//trim(nomspec(ic))//trim(nomspec(id))//'_err"'
!uncertainties   enddo
!uncertainties enddo
!uncertainties do ic=1,2!3
!uncertainties   do id=1,2!3
!uncertainties     write(78,*) '"DIFF'//trim(nomspec(ic))//trim(nomspec(id))//'_err"'
!uncertainties   enddo
!uncertainties enddo
!uncertainties do ic=1,2!3
!uncertainties   do id=1,2!3
!uncertainties     write(78,*) '"DISS'//trim(nomspec(ic))//trim(nomspec(id))//'_err"'
!uncertainties   enddo
!uncertainties enddo
!uncertainties do ic=1,2!3
!uncertainties   do id=1,2!3
!uncertainties     write(78,*) '"T1a'//trim(nomspec(ic))//trim(nomspec(id))//'_err"'
!uncertainties   enddo
!uncertainties enddo
!uncertainties do ic=1,2!3
!uncertainties   do id=1,2!3
!uncertainties     write(78,*) '"T1b'//trim(nomspec(ic))//trim(nomspec(id))//'_err"'
!uncertainties   enddo
!uncertainties enddo
!uncertainties do ic=1,2!3
!uncertainties   do id=1,2!3
!uncertainties     write(78,*) '"T2'//trim(nomspec(ic))//trim(nomspec(id))//'_err"'
!uncertainties   enddo
!uncertainties enddo
!uncertainties do ic=1,2!3
!uncertainties   do id=1,2!3
!uncertainties     write(78,*) '"T3a'//trim(nomspec(ic))//trim(nomspec(id))//'_err"'
!uncertainties   enddo
!uncertainties enddo
!uncertainties do ic=1,2!3
!uncertainties   do id=1,2!3
!uncertainties     write(78,*) '"T3b'//trim(nomspec(ic))//trim(nomspec(id))//'_err"'
!uncertainties   enddo
!uncertainties enddo
!uncertainties do ic=1,2!3
!uncertainties   do id=1,2!3
!uncertainties     write(78,*) '"T3c'//trim(nomspec(ic))//trim(nomspec(id))//'_err"'
!uncertainties   enddo
!uncertainties enddo
!uncertainties do ic=1,2!3
!uncertainties   do id=1,2!3
!uncertainties     write(78,*) '"T4'//trim(nomspec(ic))//trim(nomspec(id))//'_err"'
!uncertainties   enddo
!uncertainties enddo
!uncertainties do ic=1,2!3
!uncertainties   do id=1,2!3
!uncertainties     write(78,*) '"T5'//trim(nomspec(ic))//trim(nomspec(id))//'_err"'
!uncertainties   enddo
!uncertainties enddo
!uncertainties 
!uncertainties 
!uncertainties do ic=1,2
!uncertainties   do id=1,2
!uncertainties   write(78,*) '"YflucErr'//trim(nomspec(ic))//trim(nomspec(id))//'"'
!uncertainties   enddo
!uncertainties enddo
!uncertainties  
! NORMALISED QUANTITIES
!    write(78,*) '"y/H"'
!    write(78,*) '"norm_u"'
!    write(78,*) '"norm_v"'
!    write(78,*) '"norm_w"'
!    write(78,*) '"norm_y1"'
!    write(78,*) '"norm_y2"'
!!    write(78,*) '"norm_y3"'
!   
!normbasics  !write the 2d variable labels
!normbasics  !ke
!normbasics  do ic=1,2!3
!normbasics  do id=1,2!3
!normbasics    write(78,*) '"norm_'//trim(nomspec(ic))//trim(nomspec(id))//'"'
!normbasics  enddo
!normbasics  enddo
!normbasics  !eps
!normbasics  do ic=1,2!3
!normbasics  do id=1,2!3
!normbasics    write(78,*) '"norm_eps_'//trim(nomspec(ic))//'_'//trim(nomspec(id))//'"'
!normbasics  enddo
!normbasics  enddo
!normbasics  !   !invTau_ij
!normbasics  !   do ic=1,2!3
!normbasics  !   do id=1,2!3
!normbasics  !     write(78,*) '"norm_invtau_'//trim(nomspec(ic))//'_'//trim(nomspec(id))//'"'
!normbasics  !   enddo
!normbasics  !   enddo
!normbasics  !   !Cphi_ij
!normbasics  !   do ic=1,2!3
!normbasics  !   do id=1,2!3
!normbasics  !     write(78,*) '"norm_Cphi_'//trim(nomspec(ic))//'_'//trim(nomspec(id))//'"'
!normbasics  !   enddo
!normbasics  !   enddo
!normbasics  !   
!normbasics  !     write(78,*) '"norm_ke_mech (m^2/s^2)"'
!normbasics  !     write(78,*) '"norm_eps_mech (m^2/s^3)"'
!normbasics  !   
!normbasics  !     write(78,*) '"norm_invtau_mech (1/s)"'
!normbasics       write(78,*) '"norm_invtau_model1 (1/s)"'     !constant c_phi
!normbasics       write(78,*) '"norm_invtau_model2 (1/s)"'     !rr effect only
!normbasics     do ic=1,2!3
!normbasics     do id=1,2!3
!normbasics       write(78,*) '"norm_invtau_model3'//trim(nomspec(ic))//'_'//trim(nomspec(id))//' (1/s)"'  !diff-diff effect
!normbasics     enddo
!normbasics     enddo
!normbasics     
!normbasics       write(78,*) '"norm_Cphi_model1"'
!normbasics       write(78,*) '"norm_Cphi_model2"'
!normbasics     do ic=1,2!3
!normbasics     do id=1,2!3
!normbasics       write(78,*) '"norm_Cphi_model3'//trim(nomspec(ic))//'_'//trim(nomspec(id))//'"'
!normbasics     enddo
!normbasics     enddo
!normbasics  !   
!normbasics  !     
!normbasics  !write the 3d variable labels...
!normbasics  ! write the total first and then loop over the three directions
!normbasics  do ic=1,2!3
!normbasics    do id=1,2!3
!normbasics      write(78,*) '"norm_VARCONV'//trim(nomspec(ic))//trim(nomspec(id))//'"'
!normbasics    enddo
!normbasics  enddo
!normbasics  do ic=1,2!3
!normbasics    do id=1,2!3
!normbasics      write(78,*) '"norm_VARDIFF'//trim(nomspec(ic))//trim(nomspec(id))//'"'
!normbasics    enddo
!normbasics  enddo
!normbasics  do ic=1,2!3
!normbasics    do id=1,2!3
!normbasics      write(78,*) '"norm_VARDISS'//trim(nomspec(ic))//trim(nomspec(id))//'"'
!normbasics    enddo
!normbasics  enddo
!normbasics  do ic=1,2!3
!normbasics    do id=1,2!3
!normbasics      write(78,*) '"norm_VART1'//trim(nomspec(ic))//trim(nomspec(id))//'"'
!normbasics    enddo
!normbasics  enddo
!normbasics  do ic=1,2!3
!normbasics    do id=1,2!3
!normbasics      write(78,*) '"norm_VART2'//trim(nomspec(ic))//trim(nomspec(id))//'"'
!normbasics    enddo
!normbasics  enddo
!normbasics  do ic=1,2!3
!normbasics    do id=1,2!3
!normbasics      write(78,*) '"norm_VART3'//trim(nomspec(ic))//trim(nomspec(id))//'"'
!normbasics    enddo
!normbasics  enddo
!normbasics  do ic=1,2!3
!normbasics    do id=1,2!3
!normbasics      write(78,*) '"norm_VART4'//trim(nomspec(ic))//trim(nomspec(id))//'"'
!normbasics    enddo
!normbasics  enddo
!normbasics  do ic=1,2!3
!normbasics    do id=1,2!3
!normbasics      write(78,*) '"norm_VART5'//trim(nomspec(ic))//trim(nomspec(id))//'"'
!normbasics    enddo
!normbasics  enddo
!normbasics  
!normbasics  do ic=1,2!3
!normbasics    do id=1,2!3
!normbasics      write(78,*) '"norm_CONV'//trim(nomspec(ic))//trim(nomspec(id))//'"'
!normbasics    enddo
!normbasics  enddo
!normbasics  do ic=1,2!3
!normbasics    do id=1,2!3
!normbasics      write(78,*) '"norm_DIFF'//trim(nomspec(ic))//trim(nomspec(id))//'"'
!normbasics    enddo
!normbasics  enddo
!normbasics  do ic=1,2!3
!normbasics    do id=1,2!3
!normbasics      write(78,*) '"norm_DISS'//trim(nomspec(ic))//trim(nomspec(id))//'"'
!normbasics    enddo
!normbasics  enddo
!normbasics  do ic=1,2!3
!normbasics    do id=1,2!3
!normbasics      write(78,*) '"norm_T1a'//trim(nomspec(ic))//trim(nomspec(id))//'"'
!normbasics    enddo
!normbasics  enddo
!normbasics  do ic=1,2!3
!normbasics    do id=1,2!3
!normbasics      write(78,*) '"norm_T1b'//trim(nomspec(ic))//trim(nomspec(id))//'"'
!normbasics    enddo
!normbasics  enddo
!normbasics  do ic=1,2!3
!normbasics    do id=1,2!3
!normbasics      write(78,*) '"norm_T2'//trim(nomspec(ic))//trim(nomspec(id))//'"'
!normbasics    enddo
!normbasics  enddo
!normbasics  do ic=1,2!3
!normbasics    do id=1,2!3
!normbasics      write(78,*) '"norm_T3a'//trim(nomspec(ic))//trim(nomspec(id))//'"'
!normbasics    enddo
!normbasics  enddo
!normbasics  do ic=1,2!3
!normbasics    do id=1,2!3
!normbasics      write(78,*) '"norm_T3b'//trim(nomspec(ic))//trim(nomspec(id))//'"'
!normbasics    enddo
!normbasics  enddo
!normbasics  do ic=1,2!3
!normbasics    do id=1,2!3
!normbasics      write(78,*) '"norm_T3c'//trim(nomspec(ic))//trim(nomspec(id))//'"'
!normbasics    enddo
!normbasics  enddo
!normbasics  do ic=1,2!3
!normbasics    do id=1,2!3
!normbasics      write(78,*) '"norm_T4'//trim(nomspec(ic))//trim(nomspec(id))//'"'
!normbasics    enddo
!normbasics  enddo
!normbasics  do ic=1,2!3
!normbasics    do id=1,2!3
!normbasics      write(78,*) '"norm_T5'//trim(nomspec(ic))//trim(nomspec(id))//'"'
!normbasics    enddo
!normbasics  enddo
!normbasics  

do ic=1,2!3
  do id=1,2!3
    write(78,*) '"netVar'//trim(nomspec(ic))//trim(nomspec(id))//'"'
  enddo
enddo
do ic=1,2!3
  do id=1,2!3
    write(78,*) '"netDiss'//trim(nomspec(ic))//trim(nomspec(id))//'"'
  enddo
enddo

if (icgd.eq.1)then
  write(78,*) '"FlxLam (m/s)"'
  write(78,*) '"FlxK-E (m/s)"'
  write(78,*) '"FlxUV(m/s)"'
  write(78,*) '"FlxDNS (m/s)"'
  write(78,*) '"FlxCGD (m/s)"'
  write(78,*) '"L_c (m)"'
  write(78,*) '"L_u (m)"'
  write(78,*) '"L_t (m)"'
  write(78,*) '"Tau "'
  write(78,*) '"Sl (m/s)"'
  write(78,*) '"gradYM (1/m)"'
  write(78,*) '"alpha (m^2/s)"'
endif

!condstuff do ic=1,ncond+1
!condstuff  do m=1,2!3
!condstuff  do n=1,2!3
!condstuff    write(78,*) '"gradcheck'//trim(nomspec(m))//trim(nomspec(n)),ic-1,'"'
!condstuff  enddo
!condstuff  enddo
!condstuff enddo
!condstuff
!condstuff do ic=1,ncond+1
!condstuff  do m=1,2
!condstuff     write(78,*) '"Ycond'//trim(nomspec(m)),ic-1,'"'
!condstuff  enddo
!condstuff enddo
!condstuff
!condstuff do ic=1,ncond+1
!condstuff  do m=1,2
!condstuff     write(78,*) '"Wcond'//trim(nomspec(m)),ic-1,'"'
!condstuff  enddo
!condstuff enddo
!condstuff
!condstuff
!condstuff do ic=1,ncond+1
!condstuff  do m=1,2!3
!condstuff  do n=1,2!3
!condstuff    write(78,*) '"KEcond'//trim(nomspec(m))//trim(nomspec(n)),ic-1,'"'
!condstuff  enddo
!condstuff  enddo
!condstuff enddo
!condstuff
!condstuff do ic=1,ncond+1
!condstuff    write(78,*) '"PDFcond',ic-1,'"'
!condstuff enddo
!condstuff
!condstuff do ic=1,ncond+1
!condstuff    write(78,*) '"FavPDFcond',ic-1,'"'
!condstuff enddo
!condstuff
!condstuff do ic=1,ncond+1
!condstuff    write(78,*) '"Denscond',ic-1,'"'
!condstuff enddo
!condstuff
!condstuff    write(78,*) '"EPS_model_spec2"'
!condstuff    write(78,*) '"EPS_model_cross12"'
!condstuff    write(78,*) '"InvTau_model_spec2"'
!condstuff    write(78,*) '"InvTau_model_cross12"'
!condstuff    write(78,*) '"InvTau2_InvTau1"'
!condstuff    write(78,*) '"InvTau12_InvTau1"'

    write(78,*) '"TKE_conv (kg/m/s^3)"'
    write(78,*) '"TKE_T1 (kg/m/s^3)"'
    write(78,*) '"TKE_T2 (kg/m/s^3)"'
    write(78,*) '"TKE_T3 (kg/m/s^3)"'
    write(78,*) '"TKE_T4 (kg/m/s^3)"'
    write(78,*) '"TKE_T5 (kg/m/s^3)"'
    write(78,*) '"TKE_T6 (kg/m/s^3)"'

    write(78,*) '"Dens (kg/m^3)"'
    write(78,*) '"Prog_f"'
    write(78,*) '"Prog_f_var"'
    write(78,*) '"Prog-Z_var"'
    write(78,*) '"Prog_f_eps"'
    write(78,*) '"Wdot_f (Kg/m^3/s)"'
    write(78,*) '"YPWP_f (Kg/m^3/s)"'
    write(78,*) '"GradYM.GradZM (1/m^2)"'
    write(78,*) '"GradZM.GradZM (1/m^2)"'

    write(78,*) '"<UC>.dPdx"'
    write(78,*) '"Y1flux (m/s)"'
    write(78,*) '"|gradY1| (1/m)"'
    write(78,*) '"Y2flux (m/s)"'       
    write(78,*) '"|gradY2| (1/m)"'
    write(78,*) '"Cflux (m/s)"'
    write(78,*) '"|gradC| (1/m)"'
! rhou'c' dpdx
! y1 flux
! grad y1
! y2 flux
! grad y2
! c flux
! grad c


!    write(78,*) '"TKE_conv1 (kg/m/s^3)"'
!    write(78,*) '"TKE_conv2 (kg/m/s^3)"'
!    write(78,*) '"TKE_conv3 (kg/m/s^3)"'
    write(78,*) '"DEBUG1"'
    write(78,*) '"DEBUG2"'
    write(78,*) '"DEBUG3"'

!write the axis
    write(78,1) int(nyf*npy*writemax)-int(nyf*npy*writemin)+1
    write(78,9) (yg(iy)*l_ref*1e3, iy=int(nyf*npy*writemin),int(nyf*npy*writemax))
    write(78,9) (yg(iy)*l_ref/1.8e-3, iy=int(nyf*npy*writemin),int(nyf*npy*writemax))

endif
!endif
!endif

!loop over all the variables to be written, gather them, ifstatement, write them, cycle.

!velocity and species first

allocate(basics(3,3,nyf))
allocate(basics_g(3,3,nyf*npy))
allocate(normbasics(3,3,nyf))
basics=0.0
basics_g=0.0
normbasics=1.0

allocate(onedim(nyf));        onedim=0.0
allocate(onedim_g(nyf*npy));  onedim_g=0.0
allocate(normonedim(nyf));    normonedim=1.0

do ic=1,3
  basics(1,ic,:)=ufil(ixplot,:,izplot,ic)   !ic= icomponent
enddo
do ic=1,2
  basics(2,ic,:)=yfil(ixplot,:,izplot,ic)
enddo
  if(myid.eq.0)write(io,*)'eps_eq set'
  call mpi_barrier(gcomm,ierr)   !probably this is not necessary.
root_id=0 
call MPI_Gather(basics,nyf*3*3,MPI_REAL8,basics_g,nyf*3*3,MPI_REAL8,root_id,ycomm,ierr)

if(yid.eq.0 .and. &
   real(xid*nxf).le.xplot(i)*real(nxf*npx).and.real((xid+1)*nxf).gt.xplot(i)*real(nxf*npx)  .and.  &
   real(zid*nzf).le.zplot(j)*real(nzf*npz).and.real((zid+1)*nzf).gt.zplot(j)*real(nzf*npz))then

 write(78,9) (basics_g(1,1,iy)*a_ref, iy=int(nyf*npy*writemin),int(nyf*npy*writemax))
 write(78,9) (basics_g(1,2,iy)*a_ref, iy=int(nyf*npy*writemin),int(nyf*npy*writemax))
 write(78,9) (basics_g(1,3,iy)*a_ref, iy=int(nyf*npy*writemin),int(nyf*npy*writemax))
 write(78,9) (basics_g(2,1,iy), iy=int(nyf*npy*writemin),int(nyf*npy*writemax))
 write(78,9) (basics_g(2,2,iy), iy=int(nyf*npy*writemin),int(nyf*npy*writemax))
! write(78,9) (basics_g(2,3,iy), iy=int(nyf*npy*writemin),int(nyf*npy*writemax))

endif
!  deallocate(basics_g) ! don't deallocate here since it's used to write the confidence intervals.

!YcFIL
do m=1,2
onedim(:)=YcFIL(ixplot,:,izplot,m)
call write_onedim(78, onedim, xplot(i), zplot(j), writemin, writemax)
enddo

! ke
do ic=1,2!3
  do id=1,2!3
    basics(ic,id,:)=ke(ixplot,:,izplot,ic,id)  
  enddo
enddo
call write_basics(78, basics, xplot(i), zplot(j), 3, 3 , writemin, writemax)

! ke_c
do ic=1,2!3
  do id=1,2!3
    basics(ic,id,:)=YYcFIL(ixplot,:,izplot,ic,id)  
  enddo
enddo
call write_basics(78, basics, xplot(i), zplot(j), 3, 3 , writemin, writemax)

! eps
do ic=1,2!3
  do id=1,2!3
    basics(ic,id,:)=eps(ixplot,:,izplot,ic,id) 
  enddo
enddo
basics=basics/time_ref
call write_basics(78, basics, xplot(i), zplot(j), 3, 3 , writemin, writemax)
      
! invTau
do ic=1,2!3
  do id=1,2!3
    basics(ic,id,:)=eps(ixplot,:,izplot,ic,id)/(ke(ixplot,:,izplot,ic,id)+1.0e-10)
  enddo
enddo
basics=basics/time_ref
call write_basics(78, basics, xplot(i), zplot(j), 3, 3 , writemin, writemax)
                
! C_phi_ij
do ic=1,2!3
  do id=1,2!3
    basics(ic,id,:)=2.0 * eps(ixplot,:,izplot,ic,id)/(ke(ixplot,:,izplot,ic,id) + 1.0e-10) &
                        / (epsmech(ixplot,:,izplot)/(kemech(ixplot,:,izplot) + 1.0e-10) + 1.0e-10)
  enddo
enddo
call write_basics(78, basics, xplot(i), zplot(j), 3, 3 , writemin, writemax)
                
!KEMECH
onedim(:)=kemech(ixplot,:,izplot)*a_ref*a_ref
call write_onedim(78, onedim, xplot(i), zplot(j), writemin, writemax)

do n=1,3
do m=1,3
onedim(:)=kemech_tens(ixplot,:,izplot,m,n)*a_ref*a_ref
call write_onedim(78, onedim, xplot(i), zplot(j), writemin, writemax)
enddo
enddo


!EPSMECH

onedim(:)=epsmech(ixplot,:,izplot)*a_ref*a_ref/time_ref
call write_onedim(78, onedim, xplot(i), zplot(j), writemin, writemax)

do n=1,3
do m=1,3
onedim(:)=epsmech_tens(ixplot,:,izplot,m,n)*a_ref*a_ref/time_ref
call write_onedim(78, onedim, xplot(i), zplot(j), writemin, writemax)
enddo
enddo

!invTAUMECH  (it was given dimensions previously)

onedim(:)=epsmech(ixplot,:,izplot)/(kemech(ixplot,:,izplot)+1.0e-10)
onedim(:)=onedim(:)/time_ref
call write_onedim(78, onedim, xplot(i), zplot(j), writemin, writemax)


! find the inverse of the dissipation tensor and find mixing frequency tensor as suggested by S. Pope.

! find the determinant of the dissipation tensor

   field(:,:,:) = epsmech_tens(:,:,:,1,1)*(epsmech_tens(:,:,:,3,3)*epsmech_tens(:,:,:,2,2)-epsmech_tens(:,:,:,3,2)*epsmech_tens(:,:,:,2,3)) &
                - epsmech_tens(:,:,:,2,1)*(epsmech_tens(:,:,:,3,3)*epsmech_tens(:,:,:,1,2)-epsmech_tens(:,:,:,3,2)*epsmech_tens(:,:,:,1,3)) &
                + epsmech_tens(:,:,:,3,1)*(epsmech_tens(:,:,:,2,3)*epsmech_tens(:,:,:,1,2)-epsmech_tens(:,:,:,2,2)*epsmech_tens(:,:,:,1,3)) 
   
   ! find the inverse of the dissipation tensor
   
   inveps(:,:,:,1,1)       = +(epsmech_tens(:,:,:,3,3)*epsmech_tens(:,:,:,2,2)-epsmech_tens(:,:,:,3,2)*epsmech_tens(:,:,:,2,3))
   inveps(:,:,:,1,2)       = -(epsmech_tens(:,:,:,3,3)*epsmech_tens(:,:,:,1,2)-epsmech_tens(:,:,:,3,2)*epsmech_tens(:,:,:,1,3))
   inveps(:,:,:,1,3)       = +(epsmech_tens(:,:,:,2,3)*epsmech_tens(:,:,:,1,2)-epsmech_tens(:,:,:,2,2)*epsmech_tens(:,:,:,1,3))
   inveps(:,:,:,2,1)       = -(epsmech_tens(:,:,:,3,3)*epsmech_tens(:,:,:,2,1)-epsmech_tens(:,:,:,3,1)*epsmech_tens(:,:,:,2,3))
   inveps(:,:,:,2,2)       = +(epsmech_tens(:,:,:,3,3)*epsmech_tens(:,:,:,1,1)-epsmech_tens(:,:,:,3,1)*epsmech_tens(:,:,:,1,3))
   inveps(:,:,:,2,3)       = -(epsmech_tens(:,:,:,2,3)*epsmech_tens(:,:,:,1,1)-epsmech_tens(:,:,:,2,1)*epsmech_tens(:,:,:,1,3))
   inveps(:,:,:,3,1)       = +(epsmech_tens(:,:,:,3,2)*epsmech_tens(:,:,:,2,1)-epsmech_tens(:,:,:,3,1)*epsmech_tens(:,:,:,2,2))
   inveps(:,:,:,3,2)       = -(epsmech_tens(:,:,:,3,2)*epsmech_tens(:,:,:,1,1)-epsmech_tens(:,:,:,3,1)*epsmech_tens(:,:,:,1,2))
   inveps(:,:,:,3,3)       = +(epsmech_tens(:,:,:,2,2)*epsmech_tens(:,:,:,1,1)-epsmech_tens(:,:,:,2,1)*epsmech_tens(:,:,:,1,2))
   
!   !divide by the determinant.

!     do n=1,3
!      do m=1,3
!       invepsln(:,:,:,m,n)=invepsln(:,:,:,m,n)/field(:,:,:)
!      enddo
!     enddo

!     variable(:,:,:,m,n)=0.0
!     do i=1,3
!     variable(:,:,:,m,n)=variable()+invepsln(:,:,:,m,i)*kemech_tens(:,:,:,i,n)    ! or something like htat
!     enddo
!

     do n=1,3
     do m=1,3

       fieldx(:,:,:)=0.0
       do k=1,3
         fieldx(:,:,:)=fieldx(:,:,:)+inveps(:,:,:,m,k)*kemech_tens(:,:,:,k,n)
       enddo

!       where(field(:,:,:).gt.1.0e-10)
        fieldx(:,:,:)=fieldx(:,:,:)/field(:,:,:)   ! divide by determinant
!       elsewhere
!        fieldx(:,:,:)=0.0
!       endwhere
       onedim(:)=1.0/fieldx(ixplot,:,izplot)/time_ref
       call write_onedim(78, onedim, xplot(i), zplot(j), writemin, writemax)

     enddo
     enddo

!do n=1,3
!do m=1,3
!onedim(:)=epsmech_tens(ixplot,:,izplot,m,n)/(kemech_tens(ixplot,:,izplot,m,n)+1.0e-10)
!onedim(:)=onedim(:)/time_ref
!call write_onedim(78, onedim, xplot(i), zplot(j), writemin, writemax)
!enddo
!enddo

Const1=2.0
Const2=1.2
Const3=0.6
if(myid.eq.0)write(io,*)'C_phi_1=',Const1
if(myid.eq.0)write(io,*)'C_phi_2=',Const2
if(myid.eq.0)write(io,*)'C_phi_3=',Const3


field(:,:,:) =(Const1/2.0) 

!invTAUMODEL 1
onedim(:) = field(ixplot,:,izplot)*epsmech(ixplot,:,izplot)/(kemech(ixplot,:,izplot)+1.0e-10)
onedim(:)=onedim(:)/time_ref
call write_onedim(78, onedim, xplot(i), zplot(j), writemin, writemax)

field(:,:,:) =(Const1/2.0) * &                                                     !  C/2.0
              (1.0 + Const2 * RHOSL(:,:,:)/(DENS(:,:,:)+1.0e-10) /  &    ! (1+C*(Rho_u*Sl/dens/V_kolm)),  V_kolm = (visc*eps)^0.25
              ((abs(VF(:,:,:)*epsmech(:,:,:)))**0.25+1.0e-10))


!invTAUMODEL 2
onedim(:) = field(ixplot,:,izplot)*epsmech(ixplot,:,izplot)/(kemech(ixplot,:,izplot)+1.0e-10)
onedim(:)=onedim(:)/time_ref
call write_onedim(78, onedim, xplot(i), zplot(j), writemin, writemax)

do n=1,2
do m=1,2

field(:,:,:) =(Const1/2.0) * &                                                     
              (1.0 + (Const2+Const3*(invLe(m,n)-1.0)) * RHOSL(:,:,:)/(DENS(:,:,:)+1.0e-10) /  & 
              ((abs(VF(:,:,:)*epsmech(:,:,:)))**0.25+1.0e-10))

!invTAUMODEL 3
basics(m,n,:) = field(ixplot,:,izplot)*epsmech(ixplot,:,izplot)/(kemech(ixplot,:,izplot)+1.0e-10)
basics(m,n,:)=basics(m,n,:)/time_ref

enddo
enddo

call write_basics(78, basics, xplot(i), zplot(j), 3, 3 , writemin, writemax)
              
!C_phi_MODEL1
field=Const1/2.0
onedim(:) = field(ixplot,:,izplot)
call write_onedim(78, onedim, xplot(i), zplot(j), writemin, writemax)

!C_phi_MODEL2
field(:,:,:) =(Const1/2.0) * &                                                     !  C/2.0
              (1.0 + Const2 * RHOSL(:,:,:)/(DENS(:,:,:)+1.0e-10) /  &    ! (1+C*(Rho_u*Sl/dens/V_kolm)),  V_kolm = (visc*eps)^0.25
              ((abs(VF(:,:,:)*epsmech(:,:,:)))**0.25+1.0e-10))

onedim(:) = field(ixplot,:,izplot)
call write_onedim(78, onedim, xplot(i), zplot(j), writemin, writemax)

!C_phi_MODEL3
do n=1,2
do m=1,2
field(:,:,:) =(Const1/2.0) * &
              (1.0 + (Const2+Const3*(invLe(m,n)-1.0)) * RHOSL(:,:,:)/(DENS(:,:,:)+1.0e-10) /  &
              ((abs(VF(:,:,:)*epsmech(:,:,:)))**0.25+1.0e-10))

basics(m,n,:) = field(ixplot,:,izplot)
enddo
enddo

call write_basics(78, basics, xplot(i), zplot(j), 3, 3 , writemin, writemax)


if(myid.eq.0)write(io,*)'***** mech done, starting var eq *****'
!The Var-eqn

!CONV   !signs checked ok
  do n=1,2
    do m=1,2
      basics(m,n,:)=0.0
   !xdir
    field(:,:,:)=-DENS(:,:,:)*UFIL(:,:,:,1)*KE(:,:,:,m,n)
    call derivative_x(nx,ny,nz, field(:,:,:), fieldx(:,:,:),scale_1x,1)
    basics(m,n,:)=fieldx(ixplot,:,izplot)  
   !ydir
    field(:,:,:)=-DENS(:,:,:)*UFIL(:,:,:,2)*KE(:,:,:,m,n)
    call derivative_y(nx,ny,nz, field(:,:,:), fieldx(:,:,:),scale_1y,1)
    basics(m,n,:)=basics(m,n,:)+fieldx(ixplot,:,izplot)
   !zdir
    field(:,:,:)=-DENS(:,:,:)*UFIL(:,:,:,3)*KE(:,:,:,m,n)
    call derivative_z(nx,ny,nz, field(:,:,:), fieldx(:,:,:),scale_1z,1)
    basics(m,n,:)=basics(m,n,:)+fieldx(ixplot,:,izplot)
    enddo
  enddo
  
    basics=basics*rho_ref/time_ref
    call write_basics(78, basics, xplot(i), zplot(j), 3, 3 , writemin, writemax)
                                
!DIFF  ! signs ok
 do n=1,2
 do m=1,2
   basics(m,n,:)=KEDIFF(ixplot,:,izplot,m,n)
 enddo
 enddo
 basics=basics*rho_ref/time_ref
 call write_basics(78, basics, xplot(i), zplot(j), 3, 3 , writemin, writemax)



!DISS  ! signs checked ok
 do n=1,2
 do m=1,2
   basics(m,n,:)=-2.0*DENS(ixplot,:,izplot)*EPS(ixplot,:,izplot,m,n)
!   if(m.eq.n)basics(m,n,:)=basics(m,n,:)*invLe(m,m)
 enddo
 enddo
 basics=basics*rho_ref/time_ref
 call write_basics(78, basics, xplot(i), zplot(j), 3, 3 , writemin, writemax)

!TRIP  ! signs ok
 do n=1,2
 do m=1,2
   basics(m,n,:)=-KETRIP(ixplot,:,izplot,m,n)
 enddo
 enddo
 basics=basics*rho_ref/time_ref
 call write_basics(78, basics, xplot(i), zplot(j), 3, 3 , writemin, writemax)
!!T1  !sign checked ok
!! do n=1,2
!! do m=1,2
!!   basics(m,n,:)= KET1(ixplot,:,izplot,m,n) &
!!                + KET1(ixplot,:,izplot,n,m) 
!! enddo
!! enddo
!! basics=basics*rho_ref/time_ref
!! call write_basics(78, basics, xplot(i), zplot(j), 3, 3 , writemin, writemax)
!!
!  do n=1,2
!    do m=1,2
!      basics(m,n,:)=0.0
!   !xdir
!    field(:,:,:)=KET1b(:,:,:,m,n,1)
!    call derivative_x(nx,ny,nz, field(:,:,:), fieldx(:,:,:),scale_1x,1)
!    basics(m,n,:)=fieldx(ixplot,:,izplot)  
!   !ydir
!    field(:,:,:)=KET1b(:,:,:,m,n,2)
!    call derivative_y(nx,ny,nz, field(:,:,:), fieldx(:,:,:),scale_1y,1)
!    basics(m,n,:)=basics(m,n,:)+fieldx(ixplot,:,izplot)
!   !zdir
!    field(:,:,:)=KET1b(:,:,:,m,n,3)
!    call derivative_z(nx,ny,nz, field(:,:,:), fieldx(:,:,:),scale_1z,1)
!    basics(m,n,:)=basics(m,n,:)+fieldx(ixplot,:,izplot)
!    enddo
!  enddo
!  
!    basics=basics*rho_ref/time_ref
!    call write_basics(78, basics, xplot(i), zplot(j), 3, 3 , writemin, writemax)


!T2   !sign checked ok
 do n=1,2
 do m=1,2
   basics(m,n,:)=0.0
   do ic=1,3
     basics(m,n,:)=basics(m,n,:)+KET2(ixplot,:,izplot,m,ic)*gradYM(ixplot,:,izplot,n,ic) &
                                +KET2(ixplot,:,izplot,n,ic)*gradYM(ixplot,:,izplot,m,ic)
   enddo
 enddo
 enddo
 basics=-basics*rho_ref/time_ref
 call write_basics(78, basics, xplot(i), zplot(j), 3, 3 , writemin, writemax)
                  
!T3
 do n=1,2
 do m=1,2
   basics(m,n,:)=KET3(ixplot,:,izplot,m,n) &
                +KET3(ixplot,:,izplot,n,m)
 enddo
 enddo
 basics=basics*rho_ref/time_ref
 call write_basics(78, basics, xplot(i), zplot(j), 3, 3 , writemin, writemax)

!T4
 do n=1,2
 do m=1,2
   
   basics(m,n,:)=KET4(ixplot,:,izplot,m,n) &
                +KET4(ixplot,:,izplot,n,m)
   if(m.eq.n)basics=0.0
 enddo
 enddo
 basics=basics*rho_ref/time_ref
 call write_basics(78, basics, xplot(i), zplot(j), 3, 3 , writemin, writemax)                  

!T5
 do n=1,2
 do m=1,2
     
   basics(m,n,:)=KET5(ixplot,:,izplot,m,n) &
                +KET5(ixplot,:,izplot,n,m)
   if(m.eq.n)basics=0.0
 enddo
 enddo
 basics=basics*rho_ref/time_ref
 call write_basics(78, basics, xplot(i), zplot(j), 3, 3 , writemin, writemax) 

!The Eps-eqn
! CONV
  do ic=1,2!3
    do id=1,2!3
      basics(ic,id,:)=CONV(ixplot,:,izplot,ic,id,1) 
    enddo
  enddo
  basics=basics*rho_ref/time_ref/time_ref
  call write_basics(78, basics, xplot(i), zplot(j), 3, 3 , writemin, writemax)

! DIFF
  do ic=1,2!3
    do id=1,2!3
      basics(ic,id,:)=DIFF(ixplot,:,izplot,ic,id,1) 
    enddo
  enddo
  basics=basics*rho_ref/time_ref/time_ref
  call write_basics(78, basics, xplot(i), zplot(j), 3, 3 , writemin, writemax)

! DISS
  do ic=1,2!3
    do id=1,2!3
      basics(ic,id,:)=DISSvar(ixplot,:,izplot,ic,id,1) 
    enddo
  enddo
  basics=basics*rho_ref/time_ref/time_ref
  call write_basics(78, basics, xplot(i), zplot(j), 3, 3 , writemin, writemax)

! T1a
  do ic=1,2!3
    do id=1,2!3
      basics(ic,id,:)=T1aVar(ixplot,:,izplot,ic,id,1) 
    enddo
  enddo
  basics=basics*rho_ref/time_ref/time_ref
  call write_basics(78, basics, xplot(i), zplot(j), 3, 3 , writemin, writemax)

! T1b
  do ic=1,2!3
    do id=1,2!3
      basics(ic,id,:)=T1b(ixplot,:,izplot,ic,id,1) &
                     +T1b(ixplot,:,izplot,id,ic,1)
    enddo
  enddo
  basics=basics*rho_ref/time_ref/time_ref
  call write_basics(78, basics, xplot(i), zplot(j), 3, 3 , writemin, writemax)

! T2
  do ic=1,2!3
    do id=1,2!3
      basics(ic,id,:)=T2Var(ixplot,:,izplot,ic,id,1) &
                     +T2Var(ixplot,:,izplot,id,ic,1)
    enddo
  enddo
  basics=basics*rho_ref/time_ref/time_ref
  call write_basics(78, basics, xplot(i), zplot(j), 3, 3 , writemin, writemax)

! T3a
  do ic=1,2!3
    do id=1,2!3
      basics(ic,id,:)=T3aVar(ixplot,:,izplot,ic,id,1) &
                     +T3aVar(ixplot,:,izplot,id,ic,1)
    enddo
  enddo
  basics=basics*rho_ref/time_ref/time_ref  
  call write_basics(78, basics, xplot(i), zplot(j), 3, 3 , writemin, writemax)

! T3b
  do ic=1,2!3
    do id=1,2!3
      basics(ic,id,:)=T3bVar(ixplot,:,izplot,ic,id,1) &
                     +T3bVar(ixplot,:,izplot,id,ic,1)
    enddo
  enddo
  basics=basics*rho_ref/time_ref/time_ref
  call write_basics(78, basics, xplot(i), zplot(j), 3, 3 , writemin, writemax)

! T3c
  do ic=1,2!3
    do id=1,2!3
      basics(ic,id,:)=T3c(ixplot,:,izplot,ic,id,1) &
                     +T3c(ixplot,:,izplot,id,ic,1)
    enddo
  enddo
  basics=basics*rho_ref/time_ref/time_ref
  call write_basics(78, basics, xplot(i), zplot(j), 3, 3 , writemin, writemax)

! T4
  do ic=1,2!3
    do id=1,2!3
      basics(ic,id,:)=T4var(ixplot,:,izplot,ic,id,1) &
                     +T4var(ixplot,:,izplot,id,ic,1)
    enddo
  enddo
  basics=basics*rho_ref/time_ref/time_ref
  call write_basics(78, basics, xplot(i), zplot(j), 3, 3 , writemin, writemax)

! T5
  do ic=1,2!3
    do id=1,2!3
      basics(ic,id,:)=T5var(ixplot,:,izplot,ic,id,1) &
                     +T5var(ixplot,:,izplot,id,ic,1)
    enddo
  enddo
  basics=basics*rho_ref/time_ref/time_ref
  call write_basics(78, basics, xplot(i), zplot(j), 3, 3 , writemin, writemax)

!uncertainties   ! confidence intervals:
!uncertainties   
!uncertainties   do ic=1,3
!uncertainties     basics(1,ic,:)=squfil(ixplot,:,izplot,ic)   !ic= icomponent
!uncertainties   enddo
!uncertainties   do ic=1,2
!uncertainties     basics(2,ic,:)=sqyfil(ixplot,:,izplot,ic)
!uncertainties   enddo
!uncertainties     call mpi_barrier(gcomm,ierr)   
!uncertainties   root_id=0 
!uncertainties   call MPI_Gather(basics,nyf*3*3,MPI_REAL8,basics_g,nyf*3*3,MPI_REAL8,root_id,ycomm,ierr)
!uncertainties   
!uncertainties   if(yid.eq.0 .and. &
!uncertainties      real(xid*nxf).le.xplot(i)*real(nxf*npx).and.real((xid+1)*nxf).gt.xplot(i)*real(nxf*npx)  .and.  &
!uncertainties      real(zid*nzf).le.zplot(j)*real(nzf*npz).and.real((zid+1)*nzf).gt.zplot(j)*real(nzf*npz))then
!uncertainties   
!uncertainties    write(78,9) (basics_g(1,1,iy)*a_ref, iy=int(nyf*npy*writemin),int(nyf*npy*writemax))
!uncertainties    write(78,9) (basics_g(1,2,iy)*a_ref, iy=int(nyf*npy*writemin),int(nyf*npy*writemax))
!uncertainties    write(78,9) (basics_g(1,3,iy)*a_ref, iy=int(nyf*npy*writemin),int(nyf*npy*writemax))
!uncertainties    write(78,9) (basics_g(2,1,iy), iy=int(nyf*npy*writemin),int(nyf*npy*writemax))
!uncertainties    write(78,9) (basics_g(2,2,iy), iy=int(nyf*npy*writemin),int(nyf*npy*writemax))
!uncertainties   ! write(78,9) (basics_g(2,3,iy), iy=int(nyf*npy*writemin),int(nyf*npy*writemax))
!uncertainties   
!uncertainties   endif
!uncertainties   deallocate(basics_g)
!uncertainties   
!uncertainties   ! ke
!uncertainties   do ic=1,2!3
!uncertainties     do id=1,2!3
!uncertainties       basics(ic,id,:)=sqYPYPF(ixplot,:,izplot,ic,id)
!uncertainties     enddo
!uncertainties   enddo
!uncertainties   call write_basics(78, basics, xplot(i), zplot(j), 3, 3 , writemin, writemax)
!uncertainties   
!uncertainties   !! ke
!uncertainties   !do ic=1,2!3
!uncertainties   !  do id=1,2!3
!uncertainties   !    basics(ic,id,:)=sqYYcFIL(ixplot,:,izplot,ic,id)
!uncertainties   !  enddo
!uncertainties   !enddo
!uncertainties   !call write_basics(78, basics, xplot(i), zplot(j), 3, 3 , writemin, writemax)
!uncertainties   
!uncertainties   
!uncertainties   ! eps
!uncertainties   do ic=1,2!3
!uncertainties     do id=1,2!3
!uncertainties       basics(ic,id,:)=sqAdYPdYPF(ixplot,:,izplot,ic,id)
!uncertainties     enddo
!uncertainties   enddo
!uncertainties   basics=basics/time_ref
!uncertainties   call write_basics(78, basics, xplot(i), zplot(j), 3, 3 , writemin, writemax)
!uncertainties    
!uncertainties   allocate(onedim(nyf));        onedim=0.0
!uncertainties   allocate(onedim_g(nyf*npy));  onedim_g=0.0
!uncertainties   
!uncertainties   
!uncertainties   !KEMECH
!uncertainties   onedim(:)=sqkemech(ixplot,:,izplot)
!uncertainties   onedim(:)=onedim(:)*a_ref*a_ref
!uncertainties   call write_onedim(78, onedim, xplot(i), zplot(j), writemin, writemax)
!uncertainties   
!uncertainties   !EPSMECH
!uncertainties   onedim(:)=sqepsmech(ixplot,:,izplot)
!uncertainties   onedim(:)=onedim(:)*a_ref*a_ref/time_ref
!uncertainties   call write_onedim(78, onedim, xplot(i), zplot(j), writemin, writemax)
!uncertainties   
!uncertainties   !The Var-eqn
!uncertainties   !CONV   !signs checked ok
!uncertainties     do n=1,2
!uncertainties       do m=1,2
!uncertainties         basics(m,n,:)=0.0
!uncertainties       enddo
!uncertainties     enddo
!uncertainties    
!uncertainties       basics=basics*rho_ref/time_ref
!uncertainties       call write_basics(78, basics, xplot(i), zplot(j), 3, 3 , writemin, writemax)
!uncertainties   
!uncertainties   
!uncertainties   !DIFF  ! signs ok
!uncertainties    do n=1,2
!uncertainties    do m=1,2
!uncertainties      basics(m,n,:)=sqKEDIFF(ixplot,:,izplot,m,n)
!uncertainties    enddo
!uncertainties    enddo
!uncertainties    basics=basics*rho_ref/time_ref
!uncertainties    call write_basics(78, basics, xplot(i), zplot(j), 3, 3 , writemin, writemax)
!uncertainties   !TRIP  ! signs ok
!uncertainties    do n=1,2
!uncertainties    do m=1,2
!uncertainties      basics(m,n,:)=sqKEDIFF(ixplot,:,izplot,m,n)
!uncertainties    enddo
!uncertainties    enddo
!uncertainties    basics=basics*rho_ref/time_ref
!uncertainties    call write_basics(78, basics, xplot(i), zplot(j), 3, 3 , writemin, writemax)
!uncertainties    
!uncertainties   !DISS ! signs checked ok
!uncertainties    do n=1,2
!uncertainties    do m=1,2
!uncertainties      basics(m,n,:)=sqDENS(ixplot,:,izplot)*EPS(ixplot,:,izplot,m,n)+  &
!uncertainties                    sqAdYPdYPF(ixplot,:,izplot,m,n)*DENS(ixplot,:,izplot)
!uncertainties    enddo
!uncertainties    enddo
!uncertainties    basics=basics*rho_ref/time_ref
!uncertainties    call write_basics(78, basics, xplot(i), zplot(j), 3, 3 , writemin, writemax)
!uncertainties                    
!uncertainties   !T1  !sign checked ok
!uncertainties    do n=1,2
!uncertainties    do m=1,2
!uncertainties      basics(m,n,:)= sqKET1(ixplot,:,izplot,m,n) &
!uncertainties                   + sqKET1(ixplot,:,izplot,n,m) 
!uncertainties    enddo
!uncertainties    enddo
!uncertainties    basics=basics*rho_ref/time_ref
!uncertainties    call write_basics(78, basics, xplot(i), zplot(j), 3, 3 , writemin, writemax)
!uncertainties   
!uncertainties   !T2   !sign checked ok
!uncertainties    do n=1,2
!uncertainties    do m=1,2
!uncertainties      basics(m,n,:)=0.0
!uncertainties   !uncrt    do ic=1,3
!uncertainties   !uncrt      basics(m,n,:)=basics(m,n,:)+KET2(ixplot,:,izplot,m,ic)*gradYM(ixplot,:,izplot,n,ic) &
!uncertainties   !uncrt                                 +KET2(ixplot,:,izplot,n,ic)*gradYM(ixplot,:,izplot,m,ic)
!uncertainties   !uncrt    enddo
!uncertainties    enddo
!uncertainties    enddo
!uncertainties    basics=-basics*rho_ref/time_ref
!uncertainties    call write_basics(78, basics, xplot(i), zplot(j), 3, 3 , writemin, writemax)
!uncertainties                      
!uncertainties   !T3
!uncertainties    do n=1,2
!uncertainties    do m=1,2
!uncertainties      basics(m,n,:)=sqKET3(ixplot,:,izplot,m,n) &
!uncertainties                   +sqKET3(ixplot,:,izplot,n,m)
!uncertainties    enddo
!uncertainties    enddo
!uncertainties    basics=basics*rho_ref/time_ref
!uncertainties    call write_basics(78, basics, xplot(i), zplot(j), 3, 3 , writemin, writemax)
!uncertainties   
!uncertainties   !T4
!uncertainties    do n=1,2
!uncertainties    do m=1,2
!uncertainties      basics(m,n,:)=sqKET4(ixplot,:,izplot,m,n) &
!uncertainties                   +sqKET4(ixplot,:,izplot,n,m)
!uncertainties    enddo
!uncertainties    enddo
!uncertainties    basics=basics*rho_ref/time_ref
!uncertainties    call write_basics(78, basics, xplot(i), zplot(j), 3, 3 , writemin, writemax)                  
!uncertainties   
!uncertainties   !T5
!uncertainties    do n=1,2
!uncertainties    do m=1,2
!uncertainties        
!uncertainties      basics(m,n,:)=sqKET5(ixplot,:,izplot,m,n) &
!uncertainties                   +sqKET5(ixplot,:,izplot,n,m)
!uncertainties    enddo
!uncertainties    enddo
!uncertainties    basics=basics*rho_ref/time_ref
!uncertainties    call write_basics(78, basics, xplot(i), zplot(j), 3, 3 , writemin, writemax) 
!uncertainties   
!uncertainties   !The Eps-eqn
!uncertainties   ! CONV
!uncertainties     do ic=1,2!3
!uncertainties       do id=1,2!3
!uncertainties         basics(ic,id,:)=sqCONV(ixplot,:,izplot,ic,id,1) 
!uncertainties       enddo
!uncertainties     enddo
!uncertainties     basics=basics*rho_ref/time_ref/time_ref
!uncertainties     call write_basics(78, basics, xplot(i), zplot(j), 3, 3 , writemin, writemax)
!uncertainties   
!uncertainties   ! DIFF
!uncertainties     do ic=1,2!3
!uncertainties       do id=1,2!3
!uncertainties         basics(ic,id,:)=sqDIFF(ixplot,:,izplot,ic,id,1) 
!uncertainties       enddo
!uncertainties     enddo
!uncertainties     basics=basics*rho_ref/time_ref/time_ref
!uncertainties     call write_basics(78, basics, xplot(i), zplot(j), 3, 3 , writemin, writemax)
!uncertainties   
!uncertainties   ! DISS
!uncertainties     do ic=1,2!3
!uncertainties       do id=1,2!3
!uncertainties         basics(ic,id,:)=sqDISSvar(ixplot,:,izplot,ic,id,1) 
!uncertainties       enddo
!uncertainties     enddo
!uncertainties     basics=basics*rho_ref/time_ref/time_ref
!uncertainties     call write_basics(78, basics, xplot(i), zplot(j), 3, 3 , writemin, writemax)
!uncertainties   
!uncertainties   ! T1a
!uncertainties     do ic=1,2!3
!uncertainties       do id=1,2!3
!uncertainties         basics(ic,id,:)=sqT1aVar(ixplot,:,izplot,ic,id,1) 
!uncertainties       enddo
!uncertainties     enddo
!uncertainties     basics=basics*rho_ref/time_ref/time_ref
!uncertainties     call write_basics(78, basics, xplot(i), zplot(j), 3, 3 , writemin, writemax)
!uncertainties   
!uncertainties   ! T1b
!uncertainties     do ic=1,2!3
!uncertainties       do id=1,2!3
!uncertainties         basics(ic,id,:)=sqT1b(ixplot,:,izplot,ic,id,1) &
!uncertainties                        +sqT1b(ixplot,:,izplot,id,ic,1)
!uncertainties       enddo
!uncertainties     enddo
!uncertainties     basics=basics*rho_ref/time_ref/time_ref
!uncertainties     call write_basics(78, basics, xplot(i), zplot(j), 3, 3 , writemin, writemax)
!uncertainties   
!uncertainties   ! T2
!uncertainties     do ic=1,2!3
!uncertainties       do id=1,2!3
!uncertainties         basics(ic,id,:)=sqT2Var(ixplot,:,izplot,ic,id,1) &
!uncertainties                        +sqT2Var(ixplot,:,izplot,id,ic,1)
!uncertainties       enddo
!uncertainties     enddo
!uncertainties     basics=basics*rho_ref/time_ref/time_ref
!uncertainties     call write_basics(78, basics, xplot(i), zplot(j), 3, 3 , writemin, writemax)
!uncertainties   
!uncertainties   ! T3a
!uncertainties     do ic=1,2!3
!uncertainties       do id=1,2!3
!uncertainties         basics(ic,id,:)=sqT3aVar(ixplot,:,izplot,ic,id,1) &
!uncertainties                        +sqT3aVar(ixplot,:,izplot,id,ic,1)
!uncertainties       enddo
!uncertainties     enddo
!uncertainties     basics=basics*rho_ref/time_ref/time_ref  
!uncertainties     call write_basics(78, basics, xplot(i), zplot(j), 3, 3 , writemin, writemax)
!uncertainties   
!uncertainties   ! T3b
!uncertainties     do ic=1,2!3
!uncertainties       do id=1,2!3
!uncertainties         basics(ic,id,:)=sqT3bVar(ixplot,:,izplot,ic,id,1) &
!uncertainties                        +sqT3bVar(ixplot,:,izplot,id,ic,1)
!uncertainties       enddo
!uncertainties     enddo
!uncertainties     basics=basics*rho_ref/time_ref/time_ref
!uncertainties     call write_basics(78, basics, xplot(i), zplot(j), 3, 3 , writemin, writemax)
!uncertainties   
!uncertainties   ! T3c
!uncertainties     do ic=1,2!3
!uncertainties       do id=1,2!3
!uncertainties         basics(ic,id,:)=sqT3c(ixplot,:,izplot,ic,id,1) &
!uncertainties                        +sqT3c(ixplot,:,izplot,id,ic,1)
!uncertainties       enddo
!uncertainties     enddo
!uncertainties     basics=basics*rho_ref/time_ref/time_ref
!uncertainties     call write_basics(78, basics, xplot(i), zplot(j), 3, 3 , writemin, writemax)
!uncertainties   
!uncertainties   ! T4
!uncertainties     do ic=1,2!3
!uncertainties       do id=1,2!3
!uncertainties         basics(ic,id,:)=sqT4var(ixplot,:,izplot,ic,id,1) &
!uncertainties                        +sqT4var(ixplot,:,izplot,id,ic,1)
!uncertainties       enddo
!uncertainties     enddo
!uncertainties     basics=basics*rho_ref/time_ref/time_ref
!uncertainties     call write_basics(78, basics, xplot(i), zplot(j), 3, 3 , writemin, writemax)
!uncertainties   
!uncertainties   ! T5
!uncertainties     do ic=1,2!3
!uncertainties       do id=1,2!3
!uncertainties         basics(ic,id,:)=sqT5var(ixplot,:,izplot,ic,id,1) &
!uncertainties                        +sqT5var(ixplot,:,izplot,id,ic,1)
!uncertainties       enddo
!uncertainties     enddo
!uncertainties     basics=basics*rho_ref/time_ref/time_ref
!uncertainties     call write_basics(78, basics, xplot(i), zplot(j), 3, 3 , writemin, writemax)
!uncertainties   
!uncertainties   
!uncertainties   
!uncertainties   !Error assessment
!uncertainties   do ic=1,2
!uncertainties     do id=1,2
!uncertainties       basics(ic,id,:)=YflucError(ixplot,:,izplot,ic)
!uncertainties     enddo
!uncertainties   enddo
!uncertainties   ! this measure is dimensionless
!uncertainties   call write_basics(78, basics, xplot(i), zplot(j), 3, 3 , writemin, writemax)
!uncertainties   
!uncertainties   
!uncertainties   
! Normalised quantities
!onedim(:)=l_ref*y(:)/1.8e-3
!call write_onedim(78, onedim, xplot(i), zplot(j), writemin, writemax)
!  call mpi_barrier(gcomm,ierr)   !probably this is not necessary.
!  if(myid.eq.0)write(io,*)'after barrier of y/H'

!
!!basics(1,1,:)=(ufil(ixplot,:,izplot,ic)-25.0/a_ref)/(75.0/a_ref)   !ic= icomponent
!do ic=1,3
!  basics(1,ic,:)=ufil(ixplot,:,izplot,ic)   !   /(75.0/a_ref)   !ic= icomponent
!enddo
!do ic=1,2
!  basics(2,ic,:)=yfil(ixplot,:,izplot,ic)
!enddo
!  if(myid.eq.0)write(io,*)'eps_eq set'
!  call mpi_barrier(gcomm,ierr)   !probably this is not necessary.
!  if(myid.eq.0)write(io,*)'after barrier of eps_eq set'
!!      write(io,*)'myid=',myid
!!    
!!    root_id=0 
!call MPI_Gather(basics,nyf*3*3,MPI_REAL8,basics_g,nyf*3*3,MPI_REAL8,root_id,ycomm,ierr)
!  if(myid.eq.0)write(io,*)'after gather of u and y'
!
!
!if(yid.eq.0 .and. &
!   real(xid*nxf).le.xplot(i)*real(nxf*npx).and.real((xid+1)*nxf).gt.xplot(i)*real(nxf*npx)  .and.  &
!   real(zid*nzf).le.zplot(j)*real(nzf*npz).and.real((zid+1)*nzf).gt.zplot(j)*real(nzf*npz))then
!
! write(78,9) (basics_g(1,1,iy)*a_ref, iy=int(nyf*npy*writemin),int(nyf*npy*writemax))
! write(78,9) (basics_g(1,2,iy)*a_ref, iy=int(nyf*npy*writemin),int(nyf*npy*writemax))
! write(78,9) (basics_g(1,3,iy)*a_ref, iy=int(nyf*npy*writemin),int(nyf*npy*writemax))
! write(78,9) (basics_g(2,1,iy), iy=int(nyf*npy*writemin),int(nyf*npy*writemax))
! write(78,9) (basics_g(2,2,iy), iy=int(nyf*npy*writemin),int(nyf*npy*writemax))
!! write(78,9) (basics_g(2,3,iy), iy=int(nyf*npy*writemin),int(nyf*npy*writemax))
!
!endif



!normbasics      if(myid.eq.0)write(io,*)'writing ke'
!normbasics    ! ke
!normbasics    do ic=1,2!3
!normbasics      do id=1,2!3
!normbasics    normbasics(ic,id,:)= &
!normbasics       sqrt(max(1.0e-10,yfil(ixplot,:,izplot,ic)*(1.0-yfil(ixplot,:,izplot,ic))* &
!normbasics                        yfil(ixplot,:,izplot,id)*(1.0-yfil(ixplot,:,izplot,id))))
!normbasics    
!normbasics        basics(ic,id,:)=ke(ixplot,:,izplot,ic,id)   / &
!normbasics            normbasics(ic,id,:)
!normbasics      enddo
!normbasics    enddo
!normbasics    call write_basics(78, basics, xplot(i), zplot(j), 3, 3 , writemin, writemax)
!normbasics    
!normbasics    ! eps
!normbasics    do ic=1,2!3
!normbasics      do id=1,2!3
!normbasics        basics(ic,id,:)=eps(ixplot,:,izplot,ic,id) / &
!normbasics            normbasics(ic,id,:)
!normbasics      enddo
!normbasics    enddo
!normbasics    basics=basics/time_ref
!normbasics    call write_basics(78, basics, xplot(i), zplot(j), 3, 3 , writemin, writemax)
!normbasics!    
!normbasics!    ! invTau
!normbasics!    do ic=1,2!3
!normbasics!      do id=1,2!3
!normbasics!        basics(ic,id,:)=eps(ixplot,:,izplot,ic,id)/(ke(ixplot,:,izplot,ic,id)+1.0e-10)
!normbasics!      enddo
!normbasics!    enddo
!normbasics!    basics=basics/time_ref
!normbasics!    call write_basics(78, basics, xplot(i), zplot(j), 3, 3 , writemin, writemax)
!normbasics!    
!normbasics!    ! C_phi_ij
!normbasics!    do ic=1,2!3
!normbasics!      do id=1,2!3
!normbasics!        basics(ic,id,:)=2.0 * eps(ixplot,:,izplot,ic,id)/(ke(ixplot,:,izplot,ic,id) + 1.0e-10) &
!normbasics!                            / (epsmech(ixplot,:,izplot)/(kemech(ixplot,:,izplot) + 1.0e-10) + 1.0e-10)
!normbasics!      enddo
!normbasics!    enddo
!normbasics!    call write_basics(78, basics, xplot(i), zplot(j), 3, 3 , writemin, writemax)
!normbasics!                    
!normbasics!    !KEMECH
!normbasics!    onedim(:)=kemech(ixplot,:,izplot)*a_ref*a_ref
!normbasics!    call write_onedim(78, onedim, xplot(i), zplot(j), writemin, writemax)
!normbasics!    
!normbasics!    !EPSMECH
!normbasics!    onedim(:)=epsmech(ixplot,:,izplot)*a_ref*a_ref/time_ref
!normbasics!    call write_onedim(78, onedim, xplot(i), zplot(j), writemin, writemax)
!normbasics!    
!normbasics!    !invTAUMECH  (it was given dimensions previously)
!normbasics!    onedim(:)=epsmech(ixplot,:,izplot)/(kemech(ixplot,:,izplot)+1.0e-10)
!normbasics!    onedim(:)=onedim(:)/time_ref
!normbasics!    call write_onedim(78, onedim, xplot(i), zplot(j), writemin, writemax)
!normbasics!    
!normbasics   Const1=1.0
!normbasics   Const2=1.0
!normbasics   Const3=1.0
!normbasics!    if(myid.eq.0)write(io,*)'C_phi_1=',Const1
!normbasics!    if(myid.eq.0)write(io,*)'C_phi_2=',Const2
!normbasics!    if(myid.eq.0)write(io,*)'C_phi_3=',Const3
!normbasics!    
!normbasics!    
!normbasics    field(:,:,:) =(Const1/2.0) 
!normbasics                
!normbasics  !invTAUMODEL 1
!normbasics  onedim(:) = field(ixplot,:,izplot)*epsmech(ixplot,:,izplot)/(kemech(ixplot,:,izplot)+1.0e-10)
!normbasics  onedim(:)=onedim(:)/time_ref
!normbasics  call write_onedim(78, onedim, xplot(i), zplot(j), writemin, writemax)
!normbasics  
!normbasics  field(:,:,:) = 0.5* &
!normbasics                ( Const2 * RHOSL(:,:,:)/(DENS(:,:,:)+1.0e-10) /  &    ! (1+C*(Rho_u*Sl/dens/V_kolm)),  V_kolm = (visc*eps)^0.25
!normbasics                ((abs(VF(:,:,:)*epsmech(:,:,:)))**0.25+1.0e-10))
!normbasics  
!normbasics  
!normbasics  !invTAUMODEL 2
!normbasics  onedim(:) = field(ixplot,:,izplot)*epsmech(ixplot,:,izplot)/(kemech(ixplot,:,izplot)+1.0e-10)
!normbasics  onedim(:)=onedim(:)/time_ref
!normbasics  call write_onedim(78, onedim, xplot(i), zplot(j), writemin, writemax)
!normbasics  
!normbasics  do n=1,2
!normbasics  do m=1,2
!normbasics  
!normbasics  field(:,:,:) =0.5* &                                                     
!normbasics                ( (Const3*(invLe(m,n)-1.0)) * RHOSL(:,:,:)/(DENS(:,:,:)+1.0e-10) /  & 
!normbasics                ((abs(VF(:,:,:)*epsmech(:,:,:)))**0.25+1.0e-10))
!normbasics  
!normbasics  !invTAUMODEL 3
!normbasics  onedim(:) = field(ixplot,:,izplot)*epsmech(ixplot,:,izplot)/(kemech(ixplot,:,izplot)+1.0e-10)
!normbasics  basics(m,n,:)=onedim(:)/time_ref
!normbasics  
!normbasics  enddo
!normbasics  enddo
!normbasics  
!normbasics  call write_basics(78, basics, xplot(i), zplot(j), 3, 3 , writemin, writemax)
!normbasics                
!normbasics  !C_phi_MODEL1
!normbasics  field=Const1/2.0
!normbasics  onedim(:) = field(ixplot,:,izplot)
!normbasics  call write_onedim(78, onedim, xplot(i), zplot(j), writemin, writemax)
!normbasics  
!normbasics  !C_phi_MODEL2
!normbasics  field(:,:,:) = 0.5* &                                                     !  C/2.0
!normbasics                ( RHOSL(:,:,:)/(DENS(:,:,:)+1.0e-10) /  &    ! (1+C*(Rho_u*Sl/dens/V_kolm)),  V_kolm = (visc*eps)^0.25
!normbasics                ((abs(VF(:,:,:)*epsmech(:,:,:)))**0.25+1.0e-10))
!normbasics  
!normbasics  onedim(:) = field(ixplot,:,izplot)
!normbasics  call write_onedim(78, onedim, xplot(i), zplot(j), writemin, writemax)
!normbasics  
!normbasics  !C_phi_MODEL3
!normbasics  do n=1,2
!normbasics  do m=1,2
!normbasics  field(:,:,:) =(1.0/2.0) * &
!normbasics                ((invLe(m,n)-1.0) * RHOSL(:,:,:)/(DENS(:,:,:)+1.0e-10) /  &
!normbasics                ((abs(VF(:,:,:)*epsmech(:,:,:)))**0.25+1.0e-10))
!normbasics  
!normbasics  onedim(:) = field(ixplot,:,izplot)
!normbasics  basics(m,n,:)=onedim(:)
!normbasics  enddo
!normbasics  enddo
!normbasics  
!normbasics  call write_basics(78, basics, xplot(i), zplot(j), 3, 3 , writemin, writemax)
!normbasics    if(myid.eq.0)write(io,*)'finding normbasics for var eq'
!normbasics  
!normbasics  do id=1,2
!normbasics  do ic=1,2
!normbasics      normbasics(ic,id,:)= &
!normbasics          eps(ixplot,:,izplot,ic,id) / &
!normbasics          max(1.0e-10,ke(ixplot,:,izplot,ic,id)**2)
!normbasics  enddo
!normbasics  enddo
!normbasics  
!normbasics!    
!normbasics  if(myid.eq.0)write(io,*)'***** mech done, starting var eq *****'
!normbasics  !The Var-eqn
!normbasics    if(myid.eq.0)write(io,*)'writing var eq'
!normbasics  !CONV   !signs checked ok
!normbasics    do n=1,2
!normbasics      do m=1,2
!normbasics        basics(m,n,:)=0.0
!normbasics     !xdir
!normbasics      field(:,:,:)=-DENS(:,:,:)*UFIL(:,:,:,1)*KE(:,:,:,m,n)
!normbasics      call derivative_x(nx,ny,nz, field(:,:,:), fieldx(:,:,:),scale_1x,1)
!normbasics      basics(m,n,:)=fieldx(ixplot,:,izplot)  
!normbasics     !ydir
!normbasics      field(:,:,:)=-DENS(:,:,:)*UFIL(:,:,:,2)*KE(:,:,:,m,n)
!normbasics      call derivative_y(nx,ny,nz, field(:,:,:), fieldx(:,:,:),scale_1y,1)
!normbasics      basics(m,n,:)=basics(m,n,:)+fieldx(ixplot,:,izplot)
!normbasics     !zdir
!normbasics      field(:,:,:)=-DENS(:,:,:)*UFIL(:,:,:,3)*KE(:,:,:,m,n)
!normbasics      call derivative_z(nx,ny,nz, field(:,:,:), fieldx(:,:,:),scale_1z,1)
!normbasics      basics(m,n,:)=basics(m,n,:)+fieldx(ixplot,:,izplot)
!normbasics      enddo
!normbasics    enddo
!normbasics    
!normbasics      basics=basics*rho_ref/time_ref
!normbasics      basics=basics*normbasics   
!normbasics  
!normbasics      call write_basics(78, basics, xplot(i), zplot(j), 3, 3 , writemin, writemax)
!normbasics                                  
!normbasics                                  
!normbasics  !DIFF  ! signs ok
!normbasics   do n=1,2
!normbasics   do m=1,2
!normbasics     basics(m,n,:)=KEDIFF(ixplot,:,izplot,m,n)
!normbasics   enddo
!normbasics   enddo
!normbasics   basics=basics*rho_ref/time_ref
!normbasics   basics=basics*normbasics
!normbasics   call write_basics(78, basics, xplot(i), zplot(j), 3, 3 , writemin, writemax)
!normbasics  
!normbasics  !TRIP  ! signs ok
!normbasics   do n=1,2
!normbasics   do m=1,2
!normbasics     basics(m,n,:)=KETRIP(ixplot,:,izplot,m,n)
!normbasics   enddo
!normbasics   enddo
!normbasics   basics=basics*rho_ref/time_ref
!normbasics   basics=basics*normbasics
!normbasics   call write_basics(78, basics, xplot(i), zplot(j), 3, 3 , writemin, writemax)
!normbasics  
!normbasics  !DISS ! signs checked ok
!normbasics   do n=1,2
!normbasics   do m=1,2
!normbasics     basics(m,n,:)=-2.0*DENS(ixplot,:,izplot)*EPS(ixplot,:,izplot,m,n)
!normbasics   enddo
!normbasics   enddo
!normbasics   basics=basics*rho_ref/time_ref
!normbasics   basics=basics*normbasics
!normbasics   call write_basics(78, basics, xplot(i), zplot(j), 3, 3 , writemin, writemax)
!normbasics                     
!normbasics  !T1  !sign checked ok
!normbasics   do n=1,2
!normbasics   do m=1,2
!normbasics     basics(m,n,:)= KET1(ixplot,:,izplot,m,n) &
!normbasics                  + KET1(ixplot,:,izplot,n,m) 
!normbasics   enddo
!normbasics   enddo
!normbasics   basics=basics*normbasics
!normbasics   basics=basics*rho_ref/time_ref
!normbasics   call write_basics(78, basics, xplot(i), zplot(j), 3, 3 , writemin, writemax)
!normbasics  
!normbasics  !T2   !sign checked ok
!normbasics   do n=1,2
!normbasics   do m=1,2
!normbasics     basics(m,n,:)=0.0
!normbasics     do ic=1,3
!normbasics       basics(m,n,:)=basics(m,n,:)+KET2(ixplot,:,izplot,m,ic)*gradYM(ixplot,:,izplot,n,ic) &
!normbasics                                  +KET2(ixplot,:,izplot,n,ic)*gradYM(ixplot,:,izplot,m,ic)
!normbasics     enddo
!normbasics   enddo
!normbasics   enddo
!normbasics   basics=-basics*rho_ref/time_ref
!normbasics   basics=basics*normbasics
!normbasics  
!normbasics   call write_basics(78, basics, xplot(i), zplot(j), 3, 3 , writemin, writemax)
!normbasics  
!normbasics  
!normbasics  !T3
!normbasics   do n=1,2
!normbasics   do m=1,2
!normbasics     basics(m,n,:)=KET3(ixplot,:,izplot,m,n) &
!normbasics                  +KET3(ixplot,:,izplot,n,m)
!normbasics   enddo
!normbasics   enddo
!normbasics   basics=basics*rho_ref/time_ref
!normbasics   basics=basics*normbasics
!normbasics   call write_basics(78, basics, xplot(i), zplot(j), 3, 3 , writemin, writemax)
!normbasics  
!normbasics  !T4
!normbasics   do n=1,2
!normbasics   do m=1,2
!normbasics     basics(m,n,:)=KET4(ixplot,:,izplot,m,n) &
!normbasics                  +KET4(ixplot,:,izplot,n,m)
!normbasics   enddo
!normbasics   enddo
!normbasics   basics=basics*rho_ref/time_ref
!normbasics   basics=basics*normbasics
!normbasics   call write_basics(78, basics, xplot(i), zplot(j), 3, 3 , writemin, writemax)                  
!normbasics  
!normbasics  !T5
!normbasics   do n=1,2
!normbasics   do m=1,2
!normbasics       
!normbasics     basics(m,n,:)=KET5(ixplot,:,izplot,m,n) &
!normbasics                  +KET5(ixplot,:,izplot,n,m)
!normbasics   enddo
!normbasics   enddo
!normbasics   basics=basics*rho_ref/time_ref
!normbasics   basics=basics*normbasics
!normbasics   call write_basics(78, basics, xplot(i), zplot(j), 3, 3 , writemin, writemax) 
!normbasics  
!normbasics    if(myid.eq.0)write(io,*)'finding normbasics for eps eq'
!normbasics  
!normbasics  
!normbasics  do id=1,2
!normbasics  do ic=1,2
!normbasics      normbasics(ic,id,:)= 1.0 / &
!normbasics          max(1.0e-10,abs(ke(ixplot,:,izplot,ic,id)))
!normbasics  enddo
!normbasics  enddo
!normbasics  
!normbasics    if(myid.eq.0)write(io,*)'writing eps eq'
!normbasics  
!normbasics  
!normbasics  !***********************************************************************
!normbasics  !***********************************************************************
!normbasics  !***********************************************************************
!normbasics  !The Eps-eqn
!normbasics  ! CONV
!normbasics    do ic=1,2!3
!normbasics      do id=1,2!3
!normbasics        basics(ic,id,:)=CONV(ixplot,:,izplot,ic,id,1) 
!normbasics      enddo
!normbasics    enddo
!normbasics    basics=basics*rho_ref/time_ref/time_ref
!normbasics    basics=basics*normbasics
!normbasics    call write_basics(78, basics, xplot(i), zplot(j), 3, 3 , writemin, writemax)
!normbasics  
!normbasics  ! DIFF
!normbasics    do ic=1,2!3
!normbasics      do id=1,2!3
!normbasics        basics(ic,id,:)=DIFF(ixplot,:,izplot,ic,id,1) 
!normbasics      enddo
!normbasics    enddo
!normbasics    basics=basics*rho_ref/time_ref/time_ref
!normbasics    basics=basics*normbasics
!normbasics    call write_basics(78, basics, xplot(i), zplot(j), 3, 3 , writemin, writemax)
!normbasics  
!normbasics  ! DISS
!normbasics    do ic=1,2!3
!normbasics      do id=1,2!3
!normbasics        basics(ic,id,:)=DISSvar(ixplot,:,izplot,ic,id,1) 
!normbasics      enddo
!normbasics    enddo
!normbasics    basics=basics*rho_ref/time_ref/time_ref
!normbasics    basics=basics*normbasics
!normbasics    call write_basics(78, basics, xplot(i), zplot(j), 3, 3 , writemin, writemax)
!normbasics  
!normbasics  ! T1a
!normbasics    do ic=1,2!3
!normbasics      do id=1,2!3
!normbasics        basics(ic,id,:)=T1aVar(ixplot,:,izplot,ic,id,1) 
!normbasics      enddo
!normbasics    enddo
!normbasics    basics=basics*rho_ref/time_ref/time_ref
!normbasics    basics=basics*normbasics
!normbasics    call write_basics(78, basics, xplot(i), zplot(j), 3, 3 , writemin, writemax)
!normbasics  
!normbasics  ! T1b
!normbasics    do ic=1,2!3
!normbasics      do id=1,2!3
!normbasics        basics(ic,id,:)=T1b(ixplot,:,izplot,ic,id,1) &
!normbasics                       +T1b(ixplot,:,izplot,id,ic,1)
!normbasics      enddo
!normbasics    enddo
!normbasics    basics=basics*rho_ref/time_ref/time_ref
!normbasics    basics=basics*normbasics
!normbasics    call write_basics(78, basics, xplot(i), zplot(j), 3, 3 , writemin, writemax)
!normbasics  
!normbasics  ! T2
!normbasics    do ic=1,2!3
!normbasics      do id=1,2!3
!normbasics        basics(ic,id,:)=T2Var(ixplot,:,izplot,ic,id,1) &
!normbasics                       +T2Var(ixplot,:,izplot,id,ic,1)
!normbasics      enddo
!normbasics    enddo
!normbasics    basics=basics*rho_ref/time_ref/time_ref
!normbasics    basics=basics*normbasics
!normbasics    call write_basics(78, basics, xplot(i), zplot(j), 3, 3 , writemin, writemax)
!normbasics  
!normbasics  ! T3a
!normbasics    do ic=1,2!3
!normbasics      do id=1,2!3
!normbasics        basics(ic,id,:)=T3aVar(ixplot,:,izplot,ic,id,1) &
!normbasics                       +T3aVar(ixplot,:,izplot,id,ic,1)
!normbasics      enddo
!normbasics    enddo
!normbasics    basics=basics*rho_ref/time_ref/time_ref  
!normbasics    basics=basics*normbasics
!normbasics    call write_basics(78, basics, xplot(i), zplot(j), 3, 3 , writemin, writemax)
!normbasics  
!normbasics  ! T3b
!normbasics    do ic=1,2!3
!normbasics      do id=1,2!3
!normbasics        basics(ic,id,:)=T3bVar(ixplot,:,izplot,ic,id,1) &
!normbasics                       +T3bVar(ixplot,:,izplot,id,ic,1)
!normbasics      enddo
!normbasics    enddo
!normbasics    basics=basics*rho_ref/time_ref/time_ref
!normbasics    basics=basics*normbasics
!normbasics    call write_basics(78, basics, xplot(i), zplot(j), 3, 3 , writemin, writemax)
!normbasics  
!normbasics  ! T3c
!normbasics    do ic=1,2!3
!normbasics      do id=1,2!3
!normbasics        basics(ic,id,:)=T3c(ixplot,:,izplot,ic,id,1) &
!normbasics                       +T3c(ixplot,:,izplot,id,ic,1)
!normbasics      enddo
!normbasics    enddo
!normbasics    basics=basics*rho_ref/time_ref/time_ref
!normbasics  !  basics=basics*normbasics
!normbasics    call write_basics(78, basics, xplot(i), zplot(j), 3, 3 , writemin, writemax)
!normbasics  
!normbasics  ! T4
!normbasics    do ic=1,2!3
!normbasics      do id=1,2!3
!normbasics        basics(ic,id,:)=T4var(ixplot,:,izplot,ic,id,1) &
!normbasics                       +T4var(ixplot,:,izplot,id,ic,1)
!normbasics      enddo
!normbasics    enddo
!normbasics    basics=basics*rho_ref/time_ref/time_ref
!normbasics    basics=basics*normbasics
!normbasics    call write_basics(78, basics, xplot(i), zplot(j), 3, 3 , writemin, writemax)
!normbasics  
!normbasics  ! T5
!normbasics    do ic=1,2!3
!normbasics      do id=1,2!3
!normbasics        basics(ic,id,:)=T5var(ixplot,:,izplot,ic,id,1) &
!normbasics                       +T5var(ixplot,:,izplot,id,ic,1)
!normbasics      enddo
!normbasics    enddo
!normbasics    basics=basics*rho_ref/time_ref/time_ref
!normbasics    basics=basics*normbasics
!normbasics    call write_basics(78, basics, xplot(i), zplot(j), 3, 3 , writemin, writemax)


! NetVar   
  do m=1,2!3
    do n=1,2!3
      basics(ic,id,:)= &
                +KET1(ixplot,:,izplot,m,n) &
                +KET1(ixplot,:,izplot,n,m) &
                +KET3(ixplot,:,izplot,m,n) &
                +KET3(ixplot,:,izplot,n,m) &
!                +KET4(ixplot,:,izplot,m,n) &
!                +KET4(ixplot,:,izplot,n,m) &
                +KET5(ixplot,:,izplot,m,n) &
                +KET5(ixplot,:,izplot,n,m)
   do ic=1,3
     basics(m,n,:)=basics(m,n,:)+KET2(ixplot,:,izplot,m,ic)*gradYM(ixplot,:,izplot,n,ic) &
                                +KET2(ixplot,:,izplot,n,ic)*gradYM(ixplot,:,izplot,m,ic)
   enddo
!CONV
   !xdir
    field(:,:,:)=-DENS(:,:,:)*UFIL(:,:,:,1)*KE(:,:,:,m,n)
    call derivative_x(nx,ny,nz, field(:,:,:), fieldx(:,:,:),scale_1x,1)
    basics(m,n,:)=basics(m,n,:)+fieldx(ixplot,:,izplot)
   !ydir
    field(:,:,:)=-DENS(:,:,:)*UFIL(:,:,:,2)*KE(:,:,:,m,n)
    call derivative_y(nx,ny,nz, field(:,:,:), fieldx(:,:,:),scale_1y,1)
    basics(m,n,:)=basics(m,n,:)+fieldx(ixplot,:,izplot)
   !zdir
    field(:,:,:)=-DENS(:,:,:)*UFIL(:,:,:,3)*KE(:,:,:,m,n)
    call derivative_z(nx,ny,nz, field(:,:,:), fieldx(:,:,:),scale_1z,1)
    basics(m,n,:)=basics(m,n,:)+fieldx(ixplot,:,izplot)
!DIFF
    basics(m,n,:)=basics(m,n,:)+KEDIFF(ixplot,:,izplot,m,n)
!TRIP
    basics(m,n,:)=basics(m,n,:)+KETRIP(ixplot,:,izplot,m,n)
!DISS
    basics(m,n,:)=basics(m,n,:)+-2.0*DENS(ixplot,:,izplot)*EPS(ixplot,:,izplot,m,n)

    enddo 
  enddo

  basics=basics*rho_ref/time_ref
  call write_basics(78, basics, xplot(i), zplot(j), 3, 3 , writemin, writemax)


! NetDiss   
  do ic=1,2!3
    do id=1,2!3
      basics(ic,id,:)=  &
                     +CONV(ixplot,:,izplot,ic,id,1) &
                     +DIFF(ixplot,:,izplot,ic,id,1) &
                     +DISSVar(ixplot,:,izplot,ic,id,1) &
                     +T3c(ixplot,:,izplot,id,ic,1) &
                     +T1aVar(ixplot,:,izplot,ic,id,1) &
                     +T1b(ixplot,:,izplot,ic,id,1) &
                     +T1b(ixplot,:,izplot,id,ic,1) &
                     +T2var(ixplot,:,izplot,ic,id,1) &
                     +T2Var(ixplot,:,izplot,id,ic,1) &
                     +T3aVar(ixplot,:,izplot,ic,id,1) &
                     +T3aVar(ixplot,:,izplot,id,ic,1) &
                     +T3bVar(ixplot,:,izplot,ic,id,1) &
                     +T3bVar(ixplot,:,izplot,id,ic,1) &
                     +T3c(ixplot,:,izplot,ic,id,1) &
                     +T3c(ixplot,:,izplot,id,ic,1) &
                     +T4var(ixplot,:,izplot,ic,id,1) &
                     +T4var(ixplot,:,izplot,id,ic,1) &
                     +T5var(ixplot,:,izplot,ic,id,1) &
                     +T5var(ixplot,:,izplot,id,ic,1)
    enddo 
  enddo
  basics=basics*rho_ref/time_ref/time_ref
  call write_basics(78, basics, xplot(i), zplot(j), 3, 3 , writemin, writemax)



if (icgd.eq.1)then

!declare y_cent, Y_edge, u_cent, u_edge, rho_cent, rho_edge, upup?

!find Y_centre, Y_edge, U_centre, U_edge, Rho_centre, Rho_edge 
! reduce the data
onedim=YFIL(ixplot,:,izplot,1)
call MPI_Allgather(onedim,ny,MPI_REAL8,onedim_g,ny,MPI_REAL8,ycomm,ierr)
y_centre=onedim_g(ny_g/2)
y_edge=onedim_g(ny_g/5)

onedim=UFIL(ixplot,:,izplot,1)
call MPI_Allgather(onedim,ny,MPI_REAL8,onedim_g,ny,MPI_REAL8,ycomm,ierr)
u_centre=onedim_g(ny_g/2)
u_edge=onedim_g(ny_g/5)

onedim=DENS(ixplot,:,izplot)
call MPI_Allgather(onedim,ny,MPI_REAL8,onedim_g,ny,MPI_REAL8,ycomm,ierr)
rho_centre=onedim_g(ny_g/2)
rho_edge=onedim_g(ny_g/5)

onedim=gradYM(ixplot,:,izplot,1,2)
call MPI_Allgather(onedim,ny,MPI_REAL8,onedim_g,ny,MPI_REAL8,ycomm,ierr)
maxgradYM=maxval(onedim_g)

onedim=gradUM(ixplot,:,izplot,1,2)
call MPI_Allgather(onedim,ny,MPI_REAL8,onedim_g,ny,MPI_REAL8,ycomm,ierr)
maxgradUM=maxval(onedim_g)


!FlxLam (m/s)

    onedim(:)=-gradYM(ixplot,:,izplot,1,2)*AF(ixplot,:,izplot)/DENS(ixplot,:,izplot)
    onedim(:)=onedim(:)*a_ref
    call write_onedim(78, onedim, xplot(i), zplot(j), writemin, writemax)

!FlxK-E (m/s)

    onedim(:)=-gradYM(ixplot,:,izplot,1,2)*KEMECH(ixplot,:,izplot)**2/(EPSMECH(ixplot,:,izplot)+1.0e-10)
    onedim(:)=onedim(:)*a_ref
    call write_onedim(78, onedim, xplot(i), zplot(j), writemin, writemax)

!FlxU'V'(m/s)
    
    onedim(:)=-gradYM(ixplot,:,izplot,1,2) * &
      (-abs(UPVP(ixplot,:,izplot))/DFIL(ixplot,:,izplot)) / &
        max(abs(gradUM(ixplot,:,izplot,1,2)),1.0e-10)
    onedim(:)=onedim(:)*a_ref
    call write_onedim(78, onedim, xplot(i), zplot(j), writemin, writemax)

!FlxDNS (m/s)

    onedim(:)=KET2(ixplot,:,izplot,1,2)/DENS(ixplot,:,izplot)
    onedim(:)=onedim(:)*a_ref
    call write_onedim(78, onedim, xplot(i), zplot(j), writemin, writemax)

!FlxCGD (m/s)

    onedim(:)=-(y(:)/(abs(y(:))+1.0e-10))*RHOSL(ixplot,:,izplot)/(0.435/rho_ref) * & ! sl
              max(0.0,(Rho_centre/Rho_edge - 1.0))   *  &  ! tau
              (YFIL(ixplot,:,izplot,1)-Y_centre)/(Y_edge-Y_centre) * &
         (1.0-(YFIL(ixplot,:,izplot,1)-Y_centre)/(Y_edge-Y_centre))
    onedim(:)=onedim(:)*a_ref
    call write_onedim(78, onedim, xplot(i), zplot(j), writemin, writemax)

!L_c (m)

    onedim(:)=abs((Y_centre-Y_edge)/maxgradYM)   !maxgradYM  maxval(gradYM(ixplot,:,izplot,1,2))
    onedim(:)=onedim(:)*l_ref
    call write_onedim(78, onedim, xplot(i), zplot(j), writemin, writemax)

!L_u (m)

    onedim(:)=abs((U_centre-U_edge)/maxgradUM)  !maxval(gradUM(ixplot,:,izplot,1,2))
    onedim(:)=onedim(:)*l_ref
    call write_onedim(78, onedim, xplot(i), zplot(j), writemin, writemax)

!L_t (m)

    onedim(:)=KEMECH(ixplot,:,izplot)**1.5/max(1.0e-10,EPSMECH(ixplot,:,izplot))
    onedim(:)=onedim(:)*l_ref
    call write_onedim(78, onedim, xplot(i), zplot(j), writemin, writemax)

!Tau

    onedim(:)=Rho_centre/Rho_edge - 1.0
    call write_onedim(78, onedim, xplot(i), zplot(j), writemin, writemax)

!Sl (m/s)

    onedim(:)=RHOSL(ixplot,:,izplot)/(0.435/rho_ref)
    onedim(:)=onedim(:)*a_ref
    call write_onedim(78, onedim, xplot(i), zplot(j), writemin, writemax)

!gradYM (1/m)

    onedim(:)=gradYM(ixplot,:,izplot,1,2)
    onedim(:)=onedim(:)/l_ref
    call write_onedim(78, onedim, xplot(i), zplot(j), writemin, writemax)

!alpha (m^2/s)

    onedim(:)=AF(ixplot,:,izplot)/DENS(ixplot,:,izplot)
    onedim(:)=onedim(:)*a_ref
    call write_onedim(78, onedim, xplot(i), zplot(j), writemin, writemax)

endif

!condstuff !gradCheck 
!condstuff    do ic=1,ncond+1
!condstuff    do m=1,2
!condstuff    do n=1,2
!condstuff     basics(m,n,:)=gradCheck(ixplot,:,izplot,m,n,ic)
!condstuff    enddo
!condstuff    enddo
!condstuff     call write_basics(78, basics, xplot(i), zplot(j), 3, 3, writemin, writemax)
!condstuff    enddo
!condstuff 
!condstuff !Ycond
!condstuff    do ic=1,ncond+1
!condstuff    do m=1,2
!condstuff     onedim(:)=Ycond(ixplot,:,izplot,m,ic)
!condstuff     call write_onedim(78, onedim, xplot(i), zplot(j), writemin, writemax)
!condstuff    enddo
!condstuff    enddo
!condstuff 
!condstuff !Wcond
!condstuff    do ic=1,ncond+1
!condstuff    do m=1,2
!condstuff     onedim(:)=Wcond(ixplot,:,izplot,m,ic)*rho_ref/time_ref
!condstuff     call write_onedim(78, onedim, xplot(i), zplot(j), writemin, writemax)
!condstuff    enddo
!condstuff    enddo
!condstuff 
!condstuff !YPYPcond
!condstuff    do ic=1,ncond+1
!condstuff    do m=1,2
!condstuff    do n=1,2
!condstuff     basics(m,n,:)=YPYPcond(ixplot,:,izplot,m,n,ic)
!condstuff    enddo
!condstuff    enddo
!condstuff     call write_basics(78, basics, xplot(i), zplot(j), 3, 3, writemin, writemax)
!condstuff    enddo
!condstuff 
!condstuff ! Write the conditional PDFs and density for use in the lamianr dissipation rate model
!condstuff    do ic=1,ncond+1
!condstuff     onedim(:)=PDFCcond(ixplot,:,izplot,ic)
!condstuff     call write_onedim(78, onedim, xplot(i), zplot(j), writemin, writemax)
!condstuff    enddo
!condstuff 
!condstuff    do ic=1,ncond+1
!condstuff     onedim(:)=PDFDcond(ixplot,:,izplot,ic)
!condstuff     call write_onedim(78, onedim, xplot(i), zplot(j), writemin, writemax)
!condstuff    enddo
!condstuff 
!condstuff    do ic=1,ncond+1
!condstuff     where(Cpdfcond(ixplot,:,izplot,ic).gt.1.0e-20)
!condstuff     onedim(:)=Dpdfcond(ixplot,:,izplot,ic)/Cpdfcond(ixplot,:,izplot,ic)
!condstuff     elsewhere
!condstuff     onedim(:)=0.0
!condstuff     endwhere
!condstuff     onedim(:)=onedim(:)*rho_ref
!condstuff     call write_onedim(78, onedim, xplot(i), zplot(j), writemin, writemax)
!condstuff    enddo
!condstuff 
!condstuff 
!condstuff ! Modelled dissipation rate using the look uptable.
!condstuff     onedim(:)=AdYPdYPF_MOD(ixplot,:,izplot,1)/time_ref                                     !spec2 dissipation rate
!condstuff     call write_onedim(78, onedim, xplot(i), zplot(j), writemin, writemax)
!condstuff     onedim(:)=AdYPdYPF_MOD(ixplot,:,izplot,2)/time_ref                                     !spec12 cross dissipation rate
!condstuff     call write_onedim(78, onedim, xplot(i), zplot(j), writemin, writemax)
!condstuff     onedim(:)=AdYPdYPF_MOD(ixplot,:,izplot,1)/(ke(ixplot,:,izplot,2,2)+1.0e-10)/time_ref   !invtau mod_2
!condstuff     call write_onedim(78, onedim, xplot(i), zplot(j), writemin, writemax)
!condstuff     onedim(:)=AdYPdYPF_MOD(ixplot,:,izplot,2)/(ke(ixplot,:,izplot,1,2)+1.0e-10)/time_ref   !invtau mod_12
!condstuff     call write_onedim(78, onedim, xplot(i), zplot(j), writemin, writemax)
!condstuff     onedim(:)=AdYPdYPF_MOD(ixplot,:,izplot,1)/(ke(ixplot,:,izplot,2,2)+1.0e-10)  &
!condstuff                     / (eps(ixplot,:,izplot,1,1)/(ke(ixplot,:,izplot,1,1) + 1.0e-10) + 1.0e-10)  
!condstuff                                                                                            !invtaumod_2/invtau_1 
!condstuff     call write_onedim(78, onedim, xplot(i), zplot(j), writemin, writemax)
!condstuff     onedim(:)=AdYPdYPF_MOD(ixplot,:,izplot,2)/(ke(ixplot,:,izplot,1,2)+1.0e-10)  &
!condstuff                     / (eps(ixplot,:,izplot,1,1)/(ke(ixplot,:,izplot,1,1) + 1.0e-10) + 1.0e-10)  
!condstuff                                                                                            !invtaumod_12/invtau_1 
!condstuff     call write_onedim(78, onedim, xplot(i), zplot(j), writemin, writemax)
!condstuff     onedim(:)=AdYPdYPF_MOD(ixplot,:,izplot,2)/time_ref   

! Writing the TKE equation
if(myid.eq.0)write(io,*)'***** writing the TKE equation *****'
    
    rhodiss_ref= rho_ref * (a_ref**2) / time_ref
    onedim(:)=TKCONV(ixplot,:,izplot)*rhodiss_ref
    call write_onedim(78, onedim, xplot(i), zplot(j), writemin, writemax)
    onedim(:)=TK1(ixplot,:,izplot,1,1)*rhodiss_ref
    call write_onedim(78, onedim, xplot(i), zplot(j), writemin, writemax)
    onedim(:)=TK2(ixplot,:,izplot,1)*rhodiss_ref
    call write_onedim(78, onedim, xplot(i), zplot(j), writemin, writemax)
    onedim(:)=TK3(ixplot,:,izplot,1)*rhodiss_ref
    call write_onedim(78, onedim, xplot(i), zplot(j), writemin, writemax)
    onedim(:)=TK4(ixplot,:,izplot,1)*rhodiss_ref
    call write_onedim(78, onedim, xplot(i), zplot(j), writemin, writemax)
    onedim(:)=TK5(ixplot,:,izplot)*rhodiss_ref
    call write_onedim(78, onedim, xplot(i), zplot(j), writemin, writemax)
    onedim(:)=-EPSMECH(ixplot,:,izplot)*DENS(ixplot,:,izplot)*rhodiss_ref
    call write_onedim(78, onedim, xplot(i), zplot(j), writemin, writemax)

! Writing the values needed for Oliver Darbyshire analysis
!  Dens (kg/m^3)
    onedim(:)=DENS(ixplot,:,izplot)*rho_ref
    call write_onedim(78, onedim, xplot(i), zplot(j), writemin, writemax)
!  Prog_f
    onedim(:)=1.0-Y1_Y2M(ixplot,:,izplot)
    call write_onedim(78, onedim, xplot(i), zplot(j), writemin, writemax)
!  Prog_f_var
    onedim(:)=Y1_Y2sq(ixplot,:,izplot)-Y1_Y2M(ixplot,:,izplot)**2
    call write_onedim(78, onedim, xplot(i), zplot(j), writemin, writemax)
!  Prog-Z_var
    onedim(:)=Yfil(ixplot,:,izplot,2)*Y1_Y2M(ixplot,:,izplot)-Yfil(ixplot,:,izplot,1)
    call write_onedim(78, onedim, xplot(i), zplot(j), writemin, writemax)
!  Prog_f_eps
    onedim(:)=EPS_C(ixplot,:,izplot)*1.0/time_ref
    call write_onedim(78, onedim, xplot(i), zplot(j), writemin, writemax)
!  Wdot_f (Kg/m^3/s)
    onedim(:)=WM(ixplot,:,izplot,1)*rho_ref/time_ref
    call write_onedim(78, onedim, xplot(i), zplot(j), writemin, writemax)
!  YPWP_f (Kg/m^3/s)
    onedim(:)=(YWM(ixplot,:,izplot)-WM(ixplot,:,izplot,1)*YM(ixplot,:,izplot,1)) &
                       *rho_ref/time_ref
    call write_onedim(78, onedim, xplot(i), zplot(j), writemin, writemax)
!  GradYM.GradZM (1/m^2)
    onedim(:)=gradYM(ixplot,:,izplot,1,1)*gradYM(ixplot,:,izplot,2,1) &
             +gradYM(ixplot,:,izplot,1,2)*gradYM(ixplot,:,izplot,2,2) &
             +gradYM(ixplot,:,izplot,1,3)*gradYM(ixplot,:,izplot,2,3)
    onedim(:)=onedim(:)/l_ref/l_ref
    call write_onedim(78, onedim, xplot(i), zplot(j), writemin, writemax)
!  GradZM.GradZM (1/m^2)
    onedim(:)=gradYM(ixplot,:,izplot,2,1)*gradYM(ixplot,:,izplot,2,1) &
             +gradYM(ixplot,:,izplot,2,2)*gradYM(ixplot,:,izplot,2,2) &
             +gradYM(ixplot,:,izplot,2,3)*gradYM(ixplot,:,izplot,2,3)
    onedim(:)=onedim(:)/l_ref/l_ref
    call write_onedim(78, onedim, xplot(i), zplot(j), writemin, writemax)


!   Stuff for TKE pressure work term: <Ui"c">.d<p>/dxi
    field(:,:,:)= PM(:,:,:) 
    call derivative_x(nx,ny,nz, field(:,:,:), fieldx(:,:,:),scale_1x,1)
    call derivative_y(nx,ny,nz, field(:,:,:), fieldy(:,:,:),scale_1y,1)
    call derivative_z(nx,ny,nz, field(:,:,:), fieldz(:,:,:),scale_1z,1)
    onedim(:)=UCflux(ixplot,:,izplot,1)*fieldx(ixplot,:,izplot) + &
              UCflux(ixplot,:,izplot,2)*fieldy(ixplot,:,izplot) + &
              UCflux(ixplot,:,izplot,3)*fieldz(ixplot,:,izplot)
    onedim(:)=onedim(:)*rhodiss_ref
    call write_onedim(78, onedim, xplot(i), zplot(j), writemin, writemax)

!   Turbulent flux stuff
    do n=1,2

    onedim(:)=UYflux(ixplot,:,izplot,1,n)*gradYM(ixplot,:,izplot,n,1) &
             +UYflux(ixplot,:,izplot,2,n)*gradYM(ixplot,:,izplot,n,2) &
             +UYflux(ixplot,:,izplot,3,n)*gradYM(ixplot,:,izplot,n,3)
    onedim(:)=onedim(:)/ (max(1.0D-20,gradYM(ixplot,:,izplot,n,1)**2.0 + gradYM(ixplot,:,izplot,n,2)**2.0 + gradYM(ixplot,:,izplot,n,3)**2.0))**0.5
    onedim(:)=onedim(:)*a_ref
    call write_onedim(78, onedim, xplot(i), zplot(j), writemin, writemax)

!   Write the magnitude of the gradient:
    onedim(:)=(max(1.0D-20,gradYM(ixplot,:,izplot,n,1)**2.0 + gradYM(ixplot,:,izplot,n,2)**2.0 + gradYM(ixplot,:,izplot,n,3)**2.0))**0.5
    onedim(:)=onedim(:)/l_ref
    call write_onedim(78, onedim, xplot(i), zplot(j), writemin, writemax)
    
    enddo

!!   species two flux
!    onedim(:)=UYflux(ixplot,:,izplot,1,2)*gradYM(ixplot,:,izplot,2,1) &
!             +UYflux(ixplot,:,izplot,2,2)*gradYM(ixplot,:,izplot,2,2) &
!             +UYflux(ixplot,:,izplot,3,2)*gradYM(ixplot,:,izplot,2,3)
!    onedim(:)=onedim(:)/ (max(1.0D-20,gradYM(ixplot,:,izplot,2,1)**2.0 + gradYM(ixplot,:,izplot,2,2)**2.0 + gradYM(ixplot,:,izplot,2,3)**2.0))**0.5
!    onedim(:)=onedim(:)*a_ref
!    call write_onedim(78, onedim, xplot(i), zplot(j), writemin, writemax)
!    
!!   Write the magnitude of the progress variable gradient:
!    onedim(:)=(max(1.0D-20,gradYM(ixplot,:,izplot,2,1)**2.0 + gradYM(ixplot,:,izplot,2,2)**2.0 + gradYM(ixplot,:,izplot,2,3)**2.0))**0.5
!    onedim(:)=onedim(:)/l_ref
!    call write_onedim(78, onedim, xplot(i), zplot(j), writemin, writemax)

!   progress variable flux
!   UCflux, write the component normal to flame:  <rho.u"c">/<rho> . grad<c>/|grad<c>| 
    field(:,:,:)= 1.0-Y1_Y2M
    call derivative_x(nx,ny,nz, field(:,:,:), fieldx(:,:,:),scale_1x,1)     
    call derivative_y(nx,ny,nz, field(:,:,:), fieldy(:,:,:),scale_1y,1)
    call derivative_z(nx,ny,nz, field(:,:,:), fieldz(:,:,:),scale_1z,1)
    onedim(:)=UCflux(ixplot,:,izplot,1)*fieldx(ixplot,:,izplot) &
             +UCflux(ixplot,:,izplot,2)*fieldy(ixplot,:,izplot) &
             +UCflux(ixplot,:,izplot,3)*fieldz(ixplot,:,izplot)
    onedim(:)=onedim(:)/ (max(1.0D-20,fieldx(ixplot,:,izplot)**2.0 + fieldy(ixplot,:,izplot)**2.0 + fieldz(ixplot,:,izplot)**2.0))**0.5
    onedim(:)=onedim(:)*a_ref
    call write_onedim(78, onedim, xplot(i), zplot(j), writemin, writemax)
    
!   Write the magnitude of the progress variable gradient:
    onedim(:)=(max(1.0D-20,fieldx(ixplot,:,izplot)**2.0 + fieldy(ixplot,:,izplot)**2.0 + fieldz(ixplot,:,izplot)**2.0))**0.5
    onedim(:)=onedim(:)/l_ref
    call write_onedim(78, onedim, xplot(i), zplot(j), writemin, writemax)

! variables for investigation of convection term:
!    onedim(:)=TKCONVcomp(ixplot,:,izplot,1)*rhodiss_ref
!    call write_onedim(78, onedim, xplot(i), zplot(j), writemin, writemax)
!    onedim(:)=TKCONVcomp(ixplot,:,izplot,2)*rhodiss_ref
!    call write_onedim(78, onedim, xplot(i), zplot(j), writemin, writemax)
!    onedim(:)=TKCONVcomp(ixplot,:,izplot,3)*rhodiss_ref
!    call write_onedim(78, onedim, xplot(i), zplot(j), writemin, writemax)


!    onedim(:)=DEBUG(ixplot,:,izplot)*rhodiss_ref
!   call write_onedim(78, onedim, xplot(i), zplot(j), writemin, writemax)

   !DEBUG1 
!    onedim(:)=(max(1.0D-20,gradYM(ixplot,:,izplot,2,3)**2.0))**0.5
    onedim(:)=gradYM(ixplot,:,izplot,2,1)
    onedim(:)=onedim(:)/l_ref
    call write_onedim(78, onedim, xplot(i), zplot(j), writemin, writemax)
   !DEBUG2
    onedim(:)=gradYM(ixplot,:,izplot,2,2)
    onedim(:)=onedim(:)/l_ref
!    onedim(:)=YM(ixplot,:,izplot,2)
    call write_onedim(78, onedim, xplot(i), zplot(j), writemin, writemax)
   !DEBUG3
    onedim(:)=gradYM(ixplot,:,izplot,2,3)
    onedim(:)=onedim(:)/l_ref
!    field(:,:,:)= YM(:,:,:,2)
!    call derivative_y(nx,ny,nz, field(:,:,:), fieldy(:,:,:),scale_1y,1)     
!    onedim(:)=fieldy(ixplot,:,izplot)/l_ref
    call write_onedim(78, onedim, xplot(i), zplot(j), writemin, writemax)


deallocate(basics)
deallocate(onedim)
deallocate(onedim_g)
deallocate(normonedim)
deallocate(normbasics)

  close(78)

if(myid.eq.0)write(io,*)'***** everything written *****',xplot(i),zplot(j),ixplot,izplot

enddo  !xplot(i)
enddo  !yplot(j)


  1 format(' zone t="stationary", i=',i5,', f=block')
  9 format(10(1pe12.5,1x))
  10 format(15(1pe12.5,1x))


 ! write UFIL,YFIL,KE,EPS as 3D HDF fields.
 
!      ! WRITE OUTPUT FIELD TO HDF FILE
!      do i=1,3
!      write(sid_ext,'(I2.2)') i
!      filteredfield = UFIL(:,:,:,i) * a_ref
!      filename = '../post/UFIL'//trim(sid_ext)//'.h5'
!      call write_field(io, filteredfield, nxf, nyf, nzf, FX, FY, FZ, filename)
!      enddo
!      do i=1,3
!      write(sid_ext,'(I2.2)') i
!      filteredfield = YFIL(:,:,:,i)
!      filename = '../post/YFIL'//trim(sid_ext)//'.h5'
!      call write_field(io, filteredfield, nxf, nyf, nzf, FX, FY, FZ, filename)
!      enddo
!  !    do i=1,3
!  !     write(sid_ext,'(I2.2)') i
!  !    filteredfield = WFIL(:,:,:,i) * rr_ref
!  !    filename = '../post/WFIL'//trim(sid_ext)//'.h5'
!  !    call write_field(io, filteredfield, nxf, nyf, nzf, FX, FY, FZ, filename)
!  !    enddo
!  
!      do i=1,3
!      do j=1,3
!       write(y1id_ext,'(I1.1)') i
!       write(y2id_ext,'(I1.1)') j
!      filteredfield = KE(:,:,:,i,j)   !no units here
!      filename = '../post/KE-'//trim(Y1id_ext)//'-'//trim(Y2id_ext)//'.h5'
!      call write_field(io, filteredfield, nxf, nyf, nzf, FX, FY, FZ, filename)
!      enddo
!      enddo
!  
!      do i=1,3
!      do j=1,3
!       write(y1id_ext,'(I1.1)') i
!       write(y2id_ext,'(I1.1)') j
!      filteredfield = EPS(:,:,:,i,j)/time_ref
!      filename = '../post/EPS-'//trim(Y1id_ext)//'-'//trim(Y2id_ext)//'.h5'
!      call write_field(io, filteredfield, nxf, nyf, nzf, FX, FY, FZ, filename)
!      enddo
!      enddo
!
!       if(myid.eq.0)write(io,*)'***** HDF files written *****'


! write some output versus progress variable:

allocate(condonedim(ncond+1,nyf))
allocate(condonedim_g(ncond+1,nyf*npy))
allocate(condbasics(14,ncond+1))

i=2
j=2

ixplot=max(1,1+int(xplot(i)*real(nxf*npx-1))-xid*nxf)
ixplot=max(min(ixplot,nxf),1)
! z direction condition:
izplot=max(1,1+int(zplot(j)*real(nzf*npz-1))-zid*nzf)
izplot=max(min(izplot,nzf),1)


! loop over y positions

if(myid.eq.0)write(io,*)'writing cond gradCheck 1'

! gather data

! FlamNormGrad contains the following conditional averages
!  1)      <gradY1.gradY2/|gradY1|>          
!  2)      <gradY1.gradY2/|gradY1|>)^2       
!  3)      <gradY1.gradY1/|gradY1|>          
!  4)      <gradY1.gradY1/|gradY1|>^2        
!  5)      <gradY1.gradY2/gradY1.gradY1>     
!  6)      <gradY1.gradY2/gradY1.gradY1>^2   
!  7)      <D1gradY1.gradY1>                 
!  8)      <D2gradY2.gradY2>                 
!  9)      <D2(gradY1.gradY2/|gradY1|)^2>    

! write gradY2.gradY1/gradY1.gradY1
! <gradY1.gradY2/gradY1.gradY1|zeta>
do ic=1,ncond+1
 condonedim(ic,:)=FlamNormGrad(ixplot,:,izplot,5,ic)
enddo
call mpi_barrier(gcomm,ierr)   !probably this is not necessary.
root_id=0 
call MPI_Gather(condonedim,nyf*(ncond+1),MPI_REAL8,condonedim_g,nyf*(ncond+1),MPI_REAL8,root_id,ycomm,ierr)

condbasics(1,:)=condonedim_g(:,ny_g/2+45)


!write the flame normal gradient of species 2. <gradY2.(gradY1/|gradY1|)>
if(myid.eq.0)write(io,*)'writing cond gradCheck 2'

do ic=1,ncond+1
 condonedim(ic,:)=FlamNormGrad(ixplot,:,izplot,1,ic)
enddo
call mpi_barrier(gcomm,ierr)   !probably this is not necessary.
root_id=0
call MPI_Gather(condonedim,nyf*(ncond+1),MPI_REAL8,condonedim_g,nyf*(ncond+1),MPI_REAL8,root_id,ycomm,ierr)

condbasics(2,:)=condonedim_g(:,ny_g/2+45)/l_ref/1.0e3   !units of (1/mm)

!write |gradY1|
if(myid.eq.0)write(io,*)'writing cond gradCheck 3'

do ic=1,ncond+1
 condonedim(ic,:)=FlamNormGrad(ixplot,:,izplot,3,ic)
enddo
call mpi_barrier(gcomm,ierr)   !probably this is not necessary.
root_id=0
call MPI_Gather(condonedim,nyf*(ncond+1),MPI_REAL8,condonedim_g,nyf*(ncond+1),MPI_REAL8,root_id,ycomm,ierr)

condbasics(3,:)=condonedim_g(:,ny_g/2+45)/l_ref/1.0e3   !units of (1/mm)


if(myid.eq.0)write(io,*)'writing cond Ycond'

do m=1,2
do ic=1,ncond+1
condonedim(ic,:)=Ycond(ixplot,:,izplot,m,ic)
enddo

call mpi_barrier(gcomm,ierr)   !probably this is not necessary.
root_id=0
call MPI_Gather(condonedim,nyf*(ncond+1),MPI_REAL8,condonedim_g,nyf*(ncond+1),MPI_REAL8,root_id,ycomm,ierr)

condbasics(m+3,:)=condonedim_g(:,ny_g/2+45)

enddo

if(myid.eq.0)write(io,*)'writing cond Wcond'

do m=1,2
do ic=1,ncond+1
condonedim(ic,:)=Wcond(ixplot,:,izplot,m,ic)
enddo

call mpi_barrier(gcomm,ierr)   !probably this is not necessary.
root_id=0
call MPI_Gather(condonedim,nyf*(ncond+1),MPI_REAL8,condonedim_g,nyf*(ncond+1),MPI_REAL8,root_id,ycomm,ierr)

condbasics(m+5,:)=condonedim_g(:,ny_g/2+45)   ! note this needs to be dimensionalised

enddo

if(myid.eq.0)write(io,*)'writing cond YPYP'


do m=1,2
do n=1,2
do ic=1,ncond+1
condonedim(ic,:)=YPYPcond(ixplot,:,izplot,m,n,ic)
enddo

call mpi_barrier(gcomm,ierr)   !probably this is not necessary.
root_id=0
call MPI_Gather(condonedim,nyf*(ncond+1),MPI_REAL8,condonedim_g,nyf*(ncond+1),MPI_REAL8,root_id,ycomm,ierr)

condbasics(m+n+6,:)=condonedim_g(:,ny_g/2+45)

enddo
enddo

! write the conditional average of cosAlpha and cosAlpha_squared. (alpha is the angle between)
! the two scalar gradients.
do m=1,2

do ic=1,ncond+1
condonedim(ic,:)=CosAlpha(ixplot,:,izplot,m,ic)
enddo

call mpi_barrier(gcomm,ierr)
root_id=0
call MPI_Gather(condonedim,nyf*(ncond+1),MPI_REAL8,condonedim_g,nyf*(ncond+1),MPI_REAL8,root_id,ycomm,ierr)

condbasics(m+10,:)=condonedim_g(:,ny_g/2+45)

enddo

! write the dissipation rate ratio of species 1 and 2:

do ic=1,ncond+1
!condonedim(ic,:)=invLe(1,1)/invLe(2,2)*gradCheck(ixplot,:,izplot,2,2,ic)/gradCheck(ixplot,:,izplot,1,1,ic)
condonedim(ic,:)=FlamNormGrad(ixplot,:,izplot,8,ic)/FlamNormGrad(ixplot,:,izplot,7,ic)
enddo

call mpi_barrier(gcomm,ierr)
root_id=0
call MPI_Gather(condonedim,nyf*(ncond+1),MPI_REAL8,condonedim_g,nyf*(ncond+1),MPI_REAL8,root_id,ycomm,ierr)

condbasics(13,:)=condonedim_g(:,ny_g/2+45)

! write the approximated dissipation rate ratio of species 1 and 2 based only on the component of 
! gradY2 in the direction of gradY1.

do ic=1,ncond+1
!condonedim(ic,:)=invLe(1,1)/invLe(2,2)*FlamNormGrad(ixplot,:,izplot,2,ic)/gradCheck(ixplot,:,izplot,1,1,ic)
condonedim(ic,:) = FlamNormGrad(ixplot,:,izplot,9,ic) / FlamNormGrad(ixplot,:,izplot,7,ic)
enddo

call mpi_barrier(gcomm,ierr)
root_id=0
call MPI_Gather(condonedim,nyf*(ncond+1),MPI_REAL8,condonedim_g,nyf*(ncond+1),MPI_REAL8,root_id,ycomm,ierr)

condbasics(14,:)=condonedim_g(:,ny_g/2+45)



write(io,*)'writing cond stuff', myid

call mpi_barrier(gcomm,ierr)   !probably this is not necessary.

!write(io,*)myid,yid,'xid*nxf,xplot(i)*real(nxf*npx)',xid*nxf,xplot(i)*real(nxf*npx)
!write(io,*)myid,yid,'zid*nzf,zplot(j)*real(nzf,npz)',zid*nzf,zplot(j)*real(nzf,npz)

if(yid.eq.0 .and. &
 real(xid*nxf).le.xplot(i)*real(nxf*npx).and.real((xid+1)*nxf).gt.xplot(i)*real(nxf*npx) .and.  &
 real(zid*nzf).le.zplot(j)*real(nzf*npz).and.real((zid+1)*nzf).gt.zplot(j)*real(nzf*npz))then

write(io,*)'inside writing clause'

  filename = '../post/yplots/conditionalsX'//trim(Y1rl_ext)//'Z'//trim(Y2rl_ext)//'.tec'
  open(unit=78,file=trim(filename),status='unknown')

 do ic=2,ncond+1
 write(78,10) (real(ic)-1.5)/real(ncond),(condbasics(m,ic),m=1,14)
 enddo

 close(78)
endif

! end loop over y positions

deallocate(condbasics)
deallocate(condonedim)
deallocate(condonedim_g)

!deallocate the variavles used only in the finishing section
deallocate(KE)
deallocate(EPS)
deallocate(CONV)
deallocate(DIFF)
!deallocate(DISS)
deallocate(DENS)

deallocate(PDFCsum)
deallocate(PDFDsum)
deallocate(PDFCcond)
deallocate(PDFDcond)

!deallocate(T1a)
deallocate(T1b)
!deallocate(T2)
!deallocate(T3a)
!deallocate(T3b)
deallocate(T3c)
!deallocate(T4)
!deallocate(T5)

deallocate(sumgradYM)
deallocate(YflucError)

!deallocate the saved variables
deallocate(CF)
deallocate(DFIL)
deallocate(AF)
deallocate(VF)
deallocate(RHOSL)
deallocate(UFIL)
deallocate(YFIL)
deallocate(YcFIL)
!deallocate(YYF)
deallocate(YPYPF)
deallocate(YYcFIL)
deallocate(AdYPdYPF)
deallocate(AdYPdYPF_MOD)
deallocate(DISSvar)
deallocate(T1aVar)
deallocate(T1bVar)
deallocate(UdYVar)
deallocate(T2Var)
deallocate(T3aVar)
deallocate(T3bVar)
deallocate(T3cVar)
deallocate(T4Var)

deallocate(KEDIFF)
deallocate(KETRIP)
deallocate(KET1)
deallocate(KET2)
deallocate(KET3)
deallocate(KET4)
deallocate(KET5)

deallocate(TKCONV)
deallocate(TK1)
deallocate(TK2)
deallocate(TK3)
deallocate(TK4)
deallocate(TK5)
deallocate(sqTKCONV)
deallocate(sqTK1)
deallocate(sqTK2)
deallocate(sqTK3)
deallocate(sqTK4)
deallocate(sqTK5)

deallocate(Y1_Y2M)
deallocate(Y1_Y2sq)
deallocate(YWM)
deallocate(EPS_C)
deallocate(sqY1_Y2M)
deallocate(sqY1_Y2sq)
deallocate(sqYWM)
deallocate(sqEPS_C)

deallocate(UYflux)
deallocate(UCflux)

deallocate(CM)
deallocate(PM)

deallocate(gradCheck)
deallocate(YPYPcond)
deallocate(Ycond)
deallocate(Wcond)
deallocate(CFcond)
deallocate(Dpdfcond)
deallocate(Cpdfcond)
deallocate(DFILcond)
deallocate(CosAlpha)
deallocate(FlamNormGrad)

deallocate(KEMECH)
deallocate(KEMECH_tens)
deallocate(EPSMECH)
deallocate(EPSMECH_tens)

!deallocate the saved uncertainty variables
deallocate(sqCF)
deallocate(sqDFIL)
deallocate(sqAF)
deallocate(sqVF)
deallocate(sqRHOSL)
deallocate(sqUFIL)
deallocate(sqYFIL)
deallocate(sqYcFIL)
!deallocate(sqYYF)
deallocate(sqYPYPF)
deallocate(sqYYcFIL)
deallocate(sqAdYPdYPF)
deallocate(sqDISSvar)
deallocate(sqT1aVar)
deallocate(sqT1bVar)
deallocate(sqUdYvar)
deallocate(sqT2Var)
deallocate(sqT3aVar)
deallocate(sqT3bVar)
deallocate(sqT3cVar)
deallocate(sqT4Var)

deallocate(sqKEDIFF)
deallocate(sqKET1)
deallocate(sqKET2)
deallocate(sqKET3)
deallocate(sqKET4)
deallocate(sqKET5)

deallocate(sqKEMECH)
deallocate(sqKEMECH_tens)
deallocate(sqEPSMECH)
deallocate(sqEPSMECH_tens)

!deallocate the non-saved uncertainty variables
deallocate(sqCONV)
deallocate(sqDIFF)
deallocate(sqDENS)
deallocate(sqT1b)
deallocate(sqT3c)

!!!!!!
deallocate(YM)
deallocate(UM)
deallocate(WM)
deallocate(gradYM)
deallocate(gradUM)

deallocate(sumrhogradYI)
deallocate(sumgradYP2)


initialized=.false.

    deallocate(fx)
    deallocate(fy)
    deallocate(fz)
    deallocate(field)
    deallocate(fieldx)
    deallocate(fieldy)
    deallocate(fieldz)
    deallocate(inveps)
    deallocate(filteredfield)
                    
    if(myid.eq.0)write(io,*)'***** reached end of routine ... phew! *****'
    
    END SUBROUTINE FINISH_STEP
