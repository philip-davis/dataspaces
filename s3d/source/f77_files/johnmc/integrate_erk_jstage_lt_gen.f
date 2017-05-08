      subroutine integrate_erk_jstage_lt(q, q_err, rk_err, rk_alpha, rk_
     *beta, rk_time, yspecies, avmolwt, nx, ny, nz, nvar_tot, n_reg, nst
     *age, n4, tstep, time, time_accum)
        
        implicit none
        
!       ! arguments
        real, dimension(nx, ny, nz, nvar_tot, n_reg), intent(inout) :: q
     *, q_err
        real, dimension(nstage), intent(in) :: rk_alpha, rk_beta, rk_err
     *, rk_time
        real, intent(in) :: yspecies(nx, ny, nz, n4)
        integer, intent(in) :: nx, ny, nz, nvar_tot, n_reg, nstage, n4
        
        real, intent(in) :: tstep, time
        real, intent(inout) :: time_accum
        real, intent(in) :: avmolwt
        
!       ! local variables
        
        integer jstage
!       ------------------------------------
!       Temporaries for naive vectorizations
!       ------------------------------------
        integer lt__3, lt__2, lt__1, lt__0
!       -----------------------------------------------
!       declarations for compiler-generated temporaries
!       -----------------------------------------------
!       -------------------------------------
!       declarations for unroll-jam variables
!       -------------------------------------
        integer ujUpper01
!       
!       
!       -----------------------------------
!       Temporaries for smart vectorization
!       -----------------------------------
!       
!       
        
!       ------------------------------------------------------------------------
!----------------
!       set initial time_accum
        
        time_accum = time
        ujUpper01 = (nvar_tot - 1 + 1) / 2 * 2 + 1 - 1
        do jstage = 1, nstage
          call rhsf(q(:, :, :, :, 1), q(:, :, :, :, 2))
          time_accum = time + rk_time(jstage) * tstep
          if (nz .ge. 1 .and. nx .ge. 1 .and. ny .ge. 1) then
            do lt__3 = 1, (ujUpper01), 2
              do lt__2 = 1, nz
                do lt__1 = 1, ny
                  do lt__0 = 1, nx
                    q_err(lt__0, lt__1, lt__2, lt__3, 1) = q_err(lt__0, 
     *lt__1, lt__2, lt__3, 1) + rk_err(jstage) * tstep * q(lt__0, lt__1,
     * lt__2, lt__3, 2)
                    q(lt__0, lt__1, lt__2, lt__3, 1) = q(lt__0, lt__1, l
     *t__2, lt__3, 3) + rk_alpha(jstage) * tstep * q(lt__0, lt__1, lt__2
     *, lt__3, 2)
                    q(lt__0, lt__1, lt__2, lt__3, 3) = q(lt__0, lt__1, l
     *t__2, lt__3, 1) + rk_beta(jstage) * tstep * q(lt__0, lt__1, lt__2,
     * lt__3, 2)
                    q_err(lt__0, lt__1, lt__2, lt__3 + 1, 1) = q_err(lt_
     *_0, lt__1, lt__2, lt__3 + 1, 1) + rk_err(jstage) * tstep * q(lt__0
     *, lt__1, lt__2, lt__3 + 1, 2)
                    q(lt__0, lt__1, lt__2, lt__3 + 1, 1) = q(lt__0, lt__
     *1, lt__2, lt__3 + 1, 3) + rk_alpha(jstage) * tstep * q(lt__0, lt__
     *1, lt__2, lt__3 + 1, 2)
                    q(lt__0, lt__1, lt__2, lt__3 + 1, 3) = q(lt__0, lt__
     *1, lt__2, lt__3 + 1, 1) + rk_beta(jstage) * tstep * q(lt__0, lt__1
     *, lt__2, lt__3 + 1, 2)
                  enddo
                enddo
              enddo
            enddo
            do lt__3 = ujUpper01 + 1, nvar_tot, 1
              do lt__2 = 1, nz
                do lt__1 = 1, ny
                  do lt__0 = 1, nx
                    q_err(lt__0, lt__1, lt__2, lt__3, 1) = q_err(lt__0, 
     *lt__1, lt__2, lt__3, 1) + rk_err(jstage) * tstep * q(lt__0, lt__1,
     * lt__2, lt__3, 2)
                    q(lt__0, lt__1, lt__2, lt__3, 1) = q(lt__0, lt__1, l
     *t__2, lt__3, 3) + rk_alpha(jstage) * tstep * q(lt__0, lt__1, lt__2
     *, lt__3, 2)
                    q(lt__0, lt__1, lt__2, lt__3, 3) = q(lt__0, lt__1, l
     *t__2, lt__3, 1) + rk_beta(jstage) * tstep * q(lt__0, lt__1, lt__2,
     * lt__3, 2)
                  enddo
                enddo
              enddo
            enddo
          endif
          call impose_hard_bc(q, yspecies, avmolwt)
        enddo
      end
