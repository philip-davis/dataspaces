program test
  implicit none
  integer :: err

  call check_dc_alloc(err)
  call check_dc_process(err)
  call check_dc_free(err)

  call check_ds_alloc(err)
  call check_ds_process(err)
  call check_ds_free(err)
end program
