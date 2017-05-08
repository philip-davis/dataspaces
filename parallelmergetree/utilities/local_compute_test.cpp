/*
 * local_compute_test.cpp
 *
 *  Created on: Feb 8, 2014
 *      Author: landge1, bremer5
 */

#include <cstdio>
#include <cfloat>
#include <unistd.h>
#include <cstdlib>
#include "SortedUnionFindAlgorithm.h"
#include "MergeTree.h"
#include "AugmentedMergeTree.h"


int main(int argc, char *argv[])
{

  if (argc < 5) {
    fprintf(stderr,"Usage: %s <input_data> <Xdim> <Ydim> <Zdim> \n", argv[0]);
    return 0;
  }

  uint32_t valence = atoi(argv[8]);

  // Read the input from the file
  char file_name[256];
  sprintf(file_name,"%s", argv[1]);
  FILE* input = fopen(file_name, "rb");
  if (input == NULL) {
    fprintf(stderr, "File not found!\n");
    return 1;
  }

  // obtain file size
  long file_size = 0;
  fseek(input,0, SEEK_END);
  file_size = ftell(input);
  rewind(input);

  // read the contents
  char* buffer = (char*)malloc(file_size);
  fread(buffer, sizeof(char), file_size, input);
  fclose(input);

  GlobalIndexType data_dim[3];
  data_dim[0] = atoi(argv[2]);
  data_dim[1] = atoi(argv[3]);
  data_dim[2] = atoi(argv[4]);

  GlobalIndexType low[3], high[3];
  low[0] = 0;
  low[1] = 0;
  low[2] = 0;

  high[0] = data_dim[0]-1;
  high[1] = data_dim[1]-1;
  high[2] = data_dim[2]-1;

  FunctionType threshold;
  if (argc == 6)
    threshold = (FunctionType)atof(argv[5]);
  else 
    threshold = (FunctionType)(-1)*FLT_MAX; 
  
  MergeTree::setDimension(data_dim);
  fprintf(stderr, "Data Dimension: %llu %llu %llu\n", data_dim[0], data_dim[1], 
                                                   data_dim[2]);

  std::vector<DataBlock> inputs, outputs(2);
  DataBlock data_block;
  data_block = make_local_block((FunctionType*)buffer, low, high, threshold); 
  inputs.push_back(data_block);
  fprintf(stderr, "Data read : %ld vertices\n", file_size/(sizeof(FunctionType)));
  
  sorted_union_find_algorithm(inputs, outputs, 0);

  AugmentedMergeTree result;

  result.decode(outputs[0]);
#if USE_TOPO_FILE_PARSER
  result.outputFeatureHierarchy("test.family");
#endif

  fprintf(stderr, "Done\n");
  return 0;
}


