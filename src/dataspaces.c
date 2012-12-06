#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>

#include "debug.h"
#include "dataspaces.h"

/* Name mangling for C functions to adapt Fortran compiler */
#define FC_FUNC(name,NAME) name ## _


/* 
   C interface.
*/

int check_dc_alloc() {
  return wrap_dc_alloc();
}

int check_dc_free() {
  return wrap_dc_free();
}

int check_dc_process() {
  return wrap_dc_process();
}

int check_ds_alloc() {
  return wrap_ds_alloc();
}

int check_ds_free() {
  return wrap_ds_free();
}

int check_ds_process() {
  return wrap_ds_process();
}

/* 
   Fortran interface.
*/

void FC_FUNC(check_dc_alloc, CHECK_DC_ALLOC)(int *err)
{
  *err = check_dc_alloc();
}

void FC_FUNC(check_dc_free, CHECK_DC_FREE)(int *err)
{
  *err = check_dc_free();
}

void FC_FUNC(check_dc_process, CHECK_DC_PROCESS)(int *err)
{
  *err = check_dc_process();
}

void FC_FUNC(check_ds_alloc, CHECK_DS_ALLOC)(int *err)
{
  *err = check_ds_alloc();
}

void FC_FUNC(check_ds_free, CHECK_DS_FREE)(int *err)
{
  *err = check_ds_free();
}

void FC_FUNC(check_ds_process, CHECK_DS_PROCESS)(int *err)
{
  *err = check_ds_process();
}
