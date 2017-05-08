
subroutine q_set( q, qnx, qny, qnz, nvar_tot, nreg)

    use param_m, only : nx, ny, nz
    use topology_m,  only : npes, mypx, mypy, mypz, myid

      implicit none

      integer, intent(in) :: qnx, qny, qnz, nvar_tot, nreg

      real :: q(qnx, qny, qnz, nvar_tot, nreg)

      integer i

      do i=1,qnx
        q(i,:,:,:,:) = mypx*nx + i
      enddo
end subroutine

subroutine q_copy_yspc( q, qnx, qny, qnz, nvar_tot, nreg, yspc, nhalo)

      implicit none

      integer, intent(in) :: qnx, qny, qnz, nvar_tot, nreg, nhalo

      real :: q(qnx, qny, qnz, nvar_tot, nreg)
      real :: yspc(1-nhalo:qnx+nhalo,1-nhalo:qny+nhalo,1-nhalo:qnz+nhalo,1:nvar_tot)

      yspc(1:qnx,1:qny,1:qnz,1:nvar_tot) = q(:,:,:,:,1)

end subroutine

subroutine print_yspc( qnx, qny, qnz, nvar_tot, nreg, yspc, &
                       nhalo, myid, yspcmfab)

      
      implicit none

      integer, intent(in) :: qnx, qny, qnz, nvar_tot, nreg, nhalo
      integer, intent(in) :: myid

      ! Pointer to multifab object - need this to trigger boxlib fill of halo
      integer*8, intent(in) :: yspcmfab

      real :: yspc(1-nhalo:qnx+nhalo,1-nhalo:qny+nhalo,1-nhalo:qnz+nhalo,1:nvar_tot)

      call do_boxlib_fill( yspcmfab )
      write(6, *) 'nhalo=', nhalo
      write(6,'(1i4,4x,20(1f5.2,2x))') myid, yspc(:,qny/2,qnz/2,1)
      flush(6)


end subroutine
