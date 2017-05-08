      subroutine diffflux_proc_looptool(nx, ny, nz, 
     *n_spec, baro_switch, thermdif
     *f_switch, diffflux, grad_ys, grad_mixmw, ys, grad_p, press, ds_mix
     *avg, avmolwt, molwt, rs_therm_diff, grad_t, temp)
        
        implicit none
        
!       ! ------------- arguments
        
        integer, intent(in) :: nx, ny, nz, n_spec
        logical, intent(in) :: baro_switch
        logical, intent(in) :: thermdiff_switch
        
        real, intent(inout), dimension(nx, ny, nz, n_spec, 3) :: diffflu
     *x
        
        real, intent(in), dimension(nx, ny, nz, n_spec, 3) :: grad_ys
        real, intent(in), dimension(nx, ny, nz, 3) :: grad_mixmw
        real, intent(in), dimension(nx, ny, nz, n_spec) :: ys
        real, intent(in), dimension(nx, ny, nz, n_spec) :: grad_p, ds_mi
     *xavg
        real, intent(in), dimension(nx, ny, nz) :: press
        real, intent(in) :: avmolwt
        real, intent(in), dimension(n_spec) :: molwt
        
        real, intent(in), dimension(nx, ny, nz, n_spec) :: rs_therm_diff
        real, intent(in), dimension(nx, ny, nz, 3) :: grad_t
        real, intent(in), dimension(nx, ny, nz) :: temp
        
!       ! ------------- local variables
        integer n, m
!       ------------------------------------
!       Temporaries for naive vectorizations
!       ------------------------------------
        integer lt__2, lt__1, lt__0
!       -----------------------------------------------
!       declarations for compiler-generated temporaries
!       -----------------------------------------------
!       -------------------------------------
!       declarations for unroll-jam variables
!       -------------------------------------
        integer ujUpper30, ujUpper20, ujUpper10, ujUpper00
!       
!       
!       -----------------------------------
!       Temporaries for smart vectorization
!       -----------------------------------
!       
        
        
        if (baro_switch) then
          if (thermdiff_switch) then
            if (ny .ge. 1 .and. nz .ge. 1 .and. nx .ge. 1 .and. n_spec .
     *ge. 2) then
              ujUpper00 = (3 - 1 + 1) / 3 * 3 + 1 - 1
              do m = 1, ujUpper00, 3
                do n = 1, n_spec - 1
                  do lt__2 = 1, nz
                    do lt__1 = 1, ny
                      do lt__0 = 1, nx
                        diffflux(lt__0, lt__1, lt__2, n, m) = -ds_mixavg
     *(lt__0, lt__1, lt__2, n) * (grad_ys(lt__0, lt__1, lt__2, n, m) + y
     *s(lt__0, lt__1, lt__2, n) * (grad_mixmw(lt__0, lt__1, lt__2, m) + 
     *(1 - molwt(n) * avmolwt) * grad_p(lt__0, lt__1, lt__2, m) / press(
     *lt__0, lt__1, lt__2)))
                        diffflux(lt__0, lt__1, lt__2, n, m) = diffflux(l
     *t__0, lt__1, lt__2, n, m) - ds_mixavg(lt__0, lt__1, lt__2, n) * rs
     *_therm_diff(lt__0, lt__1, lt__2, n) * molwt(n) * avmolwt * grad_t(
     *lt__0, lt__1, lt__2, m) / temp(lt__0, lt__1, lt__2)
                        diffflux(lt__0, lt__1, lt__2, n_spec, m) = difff
     *lux(lt__0, lt__1, lt__2, n_spec, m) - diffflux(lt__0, lt__1, lt__2
     *, n, m)
                        diffflux(lt__0, lt__1, lt__2, n, m + 1) = -ds_mi
     *xavg(lt__0, lt__1, lt__2, n) * (grad_ys(lt__0, lt__1, lt__2, n, m 
     *+ 1) + ys(lt__0, lt__1, lt__2, n) * (grad_mixmw(lt__0, lt__1, lt__
     *2, m + 1) + (1 - molwt(n) * avmolwt) * grad_p(lt__0, lt__1, lt__2,
     * m + 1) / press(lt__0, lt__1, lt__2)))
                        diffflux(lt__0, lt__1, lt__2, n, m + 1) = difffl
     *ux(lt__0, lt__1, lt__2, n, m + 1) - ds_mixavg(lt__0, lt__1, lt__2,
     * n) * rs_therm_diff(lt__0, lt__1, lt__2, n) * molwt(n) * avmolwt *
     * grad_t(lt__0, lt__1, lt__2, m + 1) / temp(lt__0, lt__1, lt__2)
                        diffflux(lt__0, lt__1, lt__2, n_spec, m + 1) = d
     *iffflux(lt__0, lt__1, lt__2, n_spec, m + 1) - diffflux(lt__0, lt__
     *1, lt__2, n, m + 1)
                        diffflux(lt__0, lt__1, lt__2, n, m + 2) = -ds_mi
     *xavg(lt__0, lt__1, lt__2, n) * (grad_ys(lt__0, lt__1, lt__2, n, m 
     *+ 2) + ys(lt__0, lt__1, lt__2, n) * (grad_mixmw(lt__0, lt__1, lt__
     *2, m + 2) + (1 - molwt(n) * avmolwt) * grad_p(lt__0, lt__1, lt__2,
     * m + 2) / press(lt__0, lt__1, lt__2)))
                        diffflux(lt__0, lt__1, lt__2, n, m + 2) = difffl
     *ux(lt__0, lt__1, lt__2, n, m + 2) - ds_mixavg(lt__0, lt__1, lt__2,
     * n) * rs_therm_diff(lt__0, lt__1, lt__2, n) * molwt(n) * avmolwt *
     * grad_t(lt__0, lt__1, lt__2, m + 2) / temp(lt__0, lt__1, lt__2)
                        diffflux(lt__0, lt__1, lt__2, n_spec, m + 2) = d
     *iffflux(lt__0, lt__1, lt__2, n_spec, m + 2) - diffflux(lt__0, lt__
     *1, lt__2, n, m + 2)
                      enddo
                    enddo
                  enddo
                enddo
              enddo
            endif
          else
            if (ny .ge. 1 .and. nz .ge. 1 .and. nx .ge. 1 .and. n_spec .
     *ge. 2) then
              ujUpper10 = (3 - 1 + 1) / 3 * 3 + 1 - 1
              do m = 1, ujUpper10, 3
                do n = 1, n_spec - 1
                  do lt__2 = 1, nz
                    do lt__1 = 1, ny
                      do lt__0 = 1, nx
                        diffflux(lt__0, lt__1, lt__2, n, m) = -ds_mixavg
     *(lt__0, lt__1, lt__2, n) * (grad_ys(lt__0, lt__1, lt__2, n, m) + y
     *s(lt__0, lt__1, lt__2, n) * (grad_mixmw(lt__0, lt__1, lt__2, m) + 
     *(1 - molwt(n) * avmolwt) * grad_p(lt__0, lt__1, lt__2, m) / press(
     *lt__0, lt__1, lt__2)))
                        diffflux(lt__0, lt__1, lt__2, n_spec, m) = difff
     *lux(lt__0, lt__1, lt__2, n_spec, m) - diffflux(lt__0, lt__1, lt__2
     *, n, m)
                        diffflux(lt__0, lt__1, lt__2, n, m + 1) = -ds_mi
     *xavg(lt__0, lt__1, lt__2, n) * (grad_ys(lt__0, lt__1, lt__2, n, m 
     *+ 1) + ys(lt__0, lt__1, lt__2, n) * (grad_mixmw(lt__0, lt__1, lt__
     *2, m + 1) + (1 - molwt(n) * avmolwt) * grad_p(lt__0, lt__1, lt__2,
     * m + 1) / press(lt__0, lt__1, lt__2)))
                        diffflux(lt__0, lt__1, lt__2, n_spec, m + 1) = d
     *iffflux(lt__0, lt__1, lt__2, n_spec, m + 1) - diffflux(lt__0, lt__
     *1, lt__2, n, m + 1)
                        diffflux(lt__0, lt__1, lt__2, n, m + 2) = -ds_mi
     *xavg(lt__0, lt__1, lt__2, n) * (grad_ys(lt__0, lt__1, lt__2, n, m 
     *+ 2) + ys(lt__0, lt__1, lt__2, n) * (grad_mixmw(lt__0, lt__1, lt__
     *2, m + 2) + (1 - molwt(n) * avmolwt) * grad_p(lt__0, lt__1, lt__2,
     * m + 2) / press(lt__0, lt__1, lt__2)))
                        diffflux(lt__0, lt__1, lt__2, n_spec, m + 2) = d
     *iffflux(lt__0, lt__1, lt__2, n_spec, m + 2) - diffflux(lt__0, lt__
     *1, lt__2, n, m + 2)
                      enddo
                    enddo
                  enddo
                enddo
              enddo
            endif
          endif
        else
          if (thermdiff_switch) then
            if (ny .ge. 1 .and. nz .ge. 1 .and. nx .ge. 1 .and. n_spec .
     *ge. 2) then
              ujUpper20 = (3 - 1 + 1) / 3 * 3 + 1 - 1
              do m = 1, ujUpper20, 3
                do n = 1, n_spec - 1
                  do lt__2 = 1, nz
                    do lt__1 = 1, ny
                      do lt__0 = 1, nx
                        diffflux(lt__0, lt__1, lt__2, n, m) = -ds_mixavg
     *(lt__0, lt__1, lt__2, n) * (grad_ys(lt__0, lt__1, lt__2, n, m) + y
     *s(lt__0, lt__1, lt__2, n) * grad_mixmw(lt__0, lt__1, lt__2, m))
                        diffflux(lt__0, lt__1, lt__2, n, m) = diffflux(l
     *t__0, lt__1, lt__2, n, m) - ds_mixavg(lt__0, lt__1, lt__2, n) * rs
     *_therm_diff(lt__0, lt__1, lt__2, n) * molwt(n) * avmolwt * grad_t(
     *lt__0, lt__1, lt__2, m) / temp(lt__0, lt__1, lt__2)
                        diffflux(lt__0, lt__1, lt__2, n_spec, m) = difff
     *lux(lt__0, lt__1, lt__2, n_spec, m) - diffflux(lt__0, lt__1, lt__2
     *, n, m)
                        diffflux(lt__0, lt__1, lt__2, n, m + 1) = -ds_mi
     *xavg(lt__0, lt__1, lt__2, n) * (grad_ys(lt__0, lt__1, lt__2, n, m 
     *+ 1) + ys(lt__0, lt__1, lt__2, n) * grad_mixmw(lt__0, lt__1, lt__2
     *, m + 1))
                        diffflux(lt__0, lt__1, lt__2, n, m + 1) = difffl
     *ux(lt__0, lt__1, lt__2, n, m + 1) - ds_mixavg(lt__0, lt__1, lt__2,
     * n) * rs_therm_diff(lt__0, lt__1, lt__2, n) * molwt(n) * avmolwt *
     * grad_t(lt__0, lt__1, lt__2, m + 1) / temp(lt__0, lt__1, lt__2)
                        diffflux(lt__0, lt__1, lt__2, n_spec, m + 1) = d
     *iffflux(lt__0, lt__1, lt__2, n_spec, m + 1) - diffflux(lt__0, lt__
     *1, lt__2, n, m + 1)
                        diffflux(lt__0, lt__1, lt__2, n, m + 2) = -ds_mi
     *xavg(lt__0, lt__1, lt__2, n) * (grad_ys(lt__0, lt__1, lt__2, n, m 
     *+ 2) + ys(lt__0, lt__1, lt__2, n) * grad_mixmw(lt__0, lt__1, lt__2
     *, m + 2))
                        diffflux(lt__0, lt__1, lt__2, n, m + 2) = difffl
     *ux(lt__0, lt__1, lt__2, n, m + 2) - ds_mixavg(lt__0, lt__1, lt__2,
     * n) * rs_therm_diff(lt__0, lt__1, lt__2, n) * molwt(n) * avmolwt *
     * grad_t(lt__0, lt__1, lt__2, m + 2) / temp(lt__0, lt__1, lt__2)
                        diffflux(lt__0, lt__1, lt__2, n_spec, m + 2) = d
     *iffflux(lt__0, lt__1, lt__2, n_spec, m + 2) - diffflux(lt__0, lt__
     *1, lt__2, n, m + 2)
                      enddo
                    enddo
                  enddo
                enddo
              enddo
            endif
          else
            if (ny .ge. 1 .and. nz .ge. 1 .and. nx .ge. 1 .and. n_spec .
     *ge. 2) then
              ujUpper30 = (3 - 1 + 1) / 3 * 3 + 1 - 1
              do m = 1, ujUpper30, 3
                do n = 1, n_spec - 1
                  do lt__2 = 1, nz
                    do lt__1 = 1, ny
                      do lt__0 = 1, nx
                        diffflux(lt__0, lt__1, lt__2, n, m) = -ds_mixavg
     *(lt__0, lt__1, lt__2, n) * (grad_ys(lt__0, lt__1, lt__2, n, m) + y
     *s(lt__0, lt__1, lt__2, n) * grad_mixmw(lt__0, lt__1, lt__2, m))
                        diffflux(lt__0, lt__1, lt__2, n_spec, m) = difff
     *lux(lt__0, lt__1, lt__2, n_spec, m) - diffflux(lt__0, lt__1, lt__2
     *, n, m)
                        diffflux(lt__0, lt__1, lt__2, n, m + 1) = -ds_mi
     *xavg(lt__0, lt__1, lt__2, n) * (grad_ys(lt__0, lt__1, lt__2, n, m 
     *+ 1) + ys(lt__0, lt__1, lt__2, n) * grad_mixmw(lt__0, lt__1, lt__2
     *, m + 1))
                        diffflux(lt__0, lt__1, lt__2, n_spec, m + 1) = d
     *iffflux(lt__0, lt__1, lt__2, n_spec, m + 1) - diffflux(lt__0, lt__
     *1, lt__2, n, m + 1)
                        diffflux(lt__0, lt__1, lt__2, n, m + 2) = -ds_mi
     *xavg(lt__0, lt__1, lt__2, n) * (grad_ys(lt__0, lt__1, lt__2, n, m 
     *+ 2) + ys(lt__0, lt__1, lt__2, n) * grad_mixmw(lt__0, lt__1, lt__2
     *, m + 2))
                        diffflux(lt__0, lt__1, lt__2, n_spec, m + 2) = d
     *iffflux(lt__0, lt__1, lt__2, n_spec, m + 2) - diffflux(lt__0, lt__
     *1, lt__2, n, m + 2)
                      enddo
                    enddo
                  enddo
                enddo
              enddo
            endif
          endif
        endif
        return
      end
