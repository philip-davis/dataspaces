!     !$========================================================================
!===============
      
      
      subroutine computeheatflux_looptool(grad_t, enthalpy, diffflux, la
     *mbda, heatflux, nx, ny, nz, n_spec)
!       Rewritten by Ramanan Sankaran - 01/05/05
!       To save memory this function was rewritten.
!       heatFlux and grad_T share the same storage. Represent them as
!       separate arrays rather than using a pointer to overlay one on
!       top of the other because pointers hurt compiler optimization.
!       ----------------------------------------------------------------------
        
        
!       !-----------------------------------------------------------------------
!---------
!       ! Compute the heat flux vector given by:
!       !
!       !    q_i = -lambda * dT/dx_i + SUM[ h_j*J_i,j ]
!       !
!       ! With
!       !   mu      - viscosity
!       !   lambda  - mixture thermal conductivity
!       !   dT/dx_i - temperature gradient in direction i
!       !   h_j     - enthalpy of species j
!       !   J_i,j   - diffusion flux of species j in direction i
!       !-----------------------------------------------------------------------
!---------
        
        
        implicit none
        
        integer, intent(in) :: nx, ny, nz, n_spec
        real, intent(inout), dimension(nx, ny, nz, 3) :: grad_t
        real, intent(inout), dimension(nx, ny, nz, n_spec) :: enthalpy
        real, intent(inout), dimension(nx, ny, nz, n_spec, 3) :: diffflu
     *x
        real, intent(inout), dimension(nx, ny, nz, 3) :: heatflux
        real, intent(in), dimension(nx, ny, nz) :: lambda
        
        integer m, n
!       ------------------------------------
!       Temporaries for naive vectorizations
!       ------------------------------------
        integer lt__2, lt__1, lt__0
!       -------------------------------------
!       declarations for unroll-jam variables
!       -------------------------------------
        integer ujUpper11, ujUpper10, ujUpper00
!       
        ujUpper00 = (3 - 1 + 1) / 3 * 3 + 1 - 1
!       -----------------------------------
!       Temporaries for smart vectorization
!       -----------------------------------
!       
!       
        
!       heatFlux => grad_T
        
        
!       dir$ uj 3
        do m = 1, ujUpper00, 3
          do lt__2 = 1, nz
            do lt__1 = 1, ny
              do lt__0 = 1, nx
                heatflux(lt__0, lt__1, lt__2, m) = -lambda(lt__0, lt__1,
     * lt__2) * grad_t(lt__0, lt__1, lt__2, m)
                heatflux(lt__0, lt__1, lt__2, m + 1) = -lambda(lt__0, lt
     *__1, lt__2) * grad_t(lt__0, lt__1, lt__2, m + 1)
                heatflux(lt__0, lt__1, lt__2, m + 2) = -lambda(lt__0, lt
     *__1, lt__2) * grad_t(lt__0, lt__1, lt__2, m + 2)
              enddo
            enddo
          enddo
        enddo
        ujUpper10 = (3 - 1 + 1) / 3 * 3 + 1 - 1
        ujUpper11 = (n_spec - 1 + 1) / 2 * 2 + 1 - 1
        
!       dir$ uj 3
        do m = 1, ujUpper10, 3
          
          
          
!         ! add in the mass diffusion component.
!         ! add in the mass diffusion component.
!         ! add in the mass diffusion component.
!         dir$ uj 2
!         dir$ uj 2
!         dir$ uj 2
          do n = 1, ujUpper11, 2
            do lt__2 = 1, nz
              do lt__1 = 1, ny
                do lt__0 = 1, nx
                  heatflux(lt__0, lt__1, lt__2, m) = heatflux(lt__0, lt_
     *_1, lt__2, m) + enthalpy(lt__0, lt__1, lt__2, n) * diffflux(lt__0,
     * lt__1, lt__2, n, m)
                  heatflux(lt__0, lt__1, lt__2, m + 1) = heatflux(lt__0,
     * lt__1, lt__2, m + 1) + enthalpy(lt__0, lt__1, lt__2, n) * diffflu
     *x(lt__0, lt__1, lt__2, n, m + 1)
                  heatflux(lt__0, lt__1, lt__2, m + 2) = heatflux(lt__0,
     * lt__1, lt__2, m + 2) + enthalpy(lt__0, lt__1, lt__2, n) * diffflu
     *x(lt__0, lt__1, lt__2, n, m + 2)
                  heatflux(lt__0, lt__1, lt__2, m) = heatflux(lt__0, lt_
     *_1, lt__2, m) + enthalpy(lt__0, lt__1, lt__2, n + 1) * diffflux(lt
     *__0, lt__1, lt__2, n + 1, m)
                  heatflux(lt__0, lt__1, lt__2, m + 1) = heatflux(lt__0,
     * lt__1, lt__2, m + 1) + enthalpy(lt__0, lt__1, lt__2, n + 1) * dif
     *fflux(lt__0, lt__1, lt__2, n + 1, m + 1)
                  heatflux(lt__0, lt__1, lt__2, m + 2) = heatflux(lt__0,
     * lt__1, lt__2, m + 2) + enthalpy(lt__0, lt__1, lt__2, n + 1) * dif
     *fflux(lt__0, lt__1, lt__2, n + 1, m + 2)
                enddo
              enddo
            enddo
          enddo
          do n = ujUpper11 + 1, n_spec, 1
            do lt__2 = 1, nz
              do lt__1 = 1, ny
                do lt__0 = 1, nx
                  heatflux(lt__0, lt__1, lt__2, m) = heatflux(lt__0, lt_
     *_1, lt__2, m) + enthalpy(lt__0, lt__1, lt__2, n) * diffflux(lt__0,
     * lt__1, lt__2, n, m)
                  heatflux(lt__0, lt__1, lt__2, m + 1) = heatflux(lt__0,
     * lt__1, lt__2, m + 1) + enthalpy(lt__0, lt__1, lt__2, n) * diffflu
     *x(lt__0, lt__1, lt__2, n, m + 1)
                  heatflux(lt__0, lt__1, lt__2, m + 2) = heatflux(lt__0,
     * lt__1, lt__2, m + 2) + enthalpy(lt__0, lt__1, lt__2, n) * diffflu
     *x(lt__0, lt__1, lt__2, n, m + 2)
                enddo
              enddo
            enddo
          enddo
          
          
          
        enddo
        
        return
      end
