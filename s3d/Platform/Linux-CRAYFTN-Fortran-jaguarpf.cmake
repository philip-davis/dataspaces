
  MESSAGE("jaguarpf is host; using custom compiler comand and flags for cray fortran compiler")
  SET( CMAKE_Fortran_COMPILER "ftn" )
  SET( DEF_CPQ ON CACHE BOOL "Define CPQ")
  SET( CMAKE_Fortran_FLAGS "-rm -s real64  -F -em")
  SET( CMAKE_EXE_LINKER_FLAGS "")
  set(CMAKE_SHARED_LIBRARY_Fortran_FLAGS " ")
