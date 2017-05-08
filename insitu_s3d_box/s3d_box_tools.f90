! These routines (get_s3d_*) are accessor functions to fetch values stored in s3d modules
subroutine get_s3d_err( q_err )
    use rk_m, only : q_err_max

    implicit none

    real, intent(out) :: q_err
    
    q_err = q_err_max
    
    return
end subroutine

subroutine get_s3d_dt( dt )
    use runtime_m, only : tstep
    use reference_m, only : time_ref

    implicit none

    real, intent(out) :: dt
    
    dt = tstep*time_ref
    
    return
end subroutine


subroutine get_s3d_extents( lsize, gmin, gmax, xyzpes, xyzid)

    use param_m, only : nx, ny, nz
    use topology_m,  only : npes, mypx, mypy, mypz, myid, xpes, ypes, zpes, xid, yid, zid
    implicit none

    integer, intent(out), dimension(3) :: lsize, gmin, gmax, xyzpes, xyzid 
    
    lsize(1) = nx
    lsize(2) = ny
    lsize(3) = nz

    gmin(1) = mypx*nx
    gmin(2) = mypy*ny
    gmin(3) = mypz*nz

    gmax = gmin+lsize-1

    xyzpes(1) = xpes
    xyzpes(2) = ypes
    xyzpes(3) = zpes

    xyzid(1) = xid
    xyzid(2) = yid
    xyzid(3) = zid

    !write(6,'(1a,1i5,1a,3i4,1a,3i4,1a)') 'Rank ', myid, ' range (', gmin, ';',gmax, ')'
    !flush(6)
    
    return 
end subroutine

subroutine get_s3d_nvar_tot( nvartot, nreg )

    use param_m, only : nvar_tot, n_reg
    implicit none

    integer, intent(out) :: nvartot, nreg

    nvartot = nvar_tot
    nreg = n_reg
    
    return
end subroutine

subroutine get_s3d_module_temp( temp_mfab );
    use param_m, only : nx, ny, nz, iorder
    use variables_m, only : temp

    real, intent(out) :: temp_mfab(1-iorder/2:nx+iorder/2, &
                                   1-iorder/2:ny+iorder/2, &
                                   1-iorder/2:nz+iorder/2)

    temp_mfab(1:nx,1:ny,1:nz) = temp

    return

end subroutine

subroutine get_s3d_iorder( i_order )

    use param_m, only : nx, ny, nz, iorder

    integer, intent(out) :: i_order

    i_order = iorder

    return

end subroutine

! Trigger s3d restart file write
subroutine dump_s3d( io )

    implicit none

    integer, intent(in) :: io

    call write_savefile(io, 'd' )

    return
end subroutine


subroutine set_s3d_itime( itime )


    use runtime_m, only : i_time

    implicit none

    integer, intent(in) :: itime

    i_time = itime

    return

end subroutine

subroutine set_s3d_time( in_time )


    use runtime_m, only : time

    implicit none

    real, intent(in) :: in_time

    time = in_time

    return

end subroutine

subroutine get_s3d_itime_end_save( it_end, it_save )


    use runtime_m, only : i_time_end, i_time_save

    implicit none

    integer, intent(out) :: it_end, it_save

    it_end = i_time_end
    it_save = i_time_save

    return

end subroutine

subroutine get_s3d_time( out_time )

  use runtime_m, only:time
  implicit none
  real,intent(out)::out_time
  out_time = time
  return

end subroutine
