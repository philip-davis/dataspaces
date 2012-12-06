#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>

#include "debug.h"
#include "dart.h"

static struct dart_client *dc = NULL;
static struct dart_server *ds = NULL;

/* 
   C interface.
*/

int wrap_dc_alloc() {

  dc = dc_alloc(1, 1, NULL);
  return 0;
}

int wrap_dc_free() {
  dc_free(dc);
  return 0;
}

int wrap_dc_process() {
  return dc_process(dc);
}

int wrap_ds_alloc() {
  ds = ds_alloc(1, 2, NULL);
  return 0;
}

int wrap_ds_free() {
  ds_free(ds);
  return 0;
}

int wrap_ds_process() {
  return ds_process(ds);
}

