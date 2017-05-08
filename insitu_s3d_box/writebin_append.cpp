#include <stdio.h>

extern "C" {
void writebin_append_(void* ptr, int *n, char* filename);
}

void writebin_append_(void* ptr, int *n, char* filename){
  FILE * op;
/*  float a[*n];*/

/*  op = fopen(filename, "r");

    fread(a, 4, *n, op);
    printf("%e\n",a[*n-1]);*/

  op = fopen(filename, "a");
  fwrite(ptr, 4, *n, op);
  fclose(op);

  }
