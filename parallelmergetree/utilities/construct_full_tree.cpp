#include <cstdio>
#include <cfloat>
#include <unistd.h>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include "TypeDefinitions.h"
#include <map>
#include <cassert>

int main(int argc, char* argv[]) {

  if (argc < 3) {
    std::cerr << "Usage: construct_full_tree <input_file_name_prefix> <number of files> \n";
    return 0;
  }
  char input_file_prefix[64];
  int num_files;
  
  strcpy(input_file_prefix, (const char*)argv[1]);
  num_files = atoi(argv[2]);

  std::cerr << "Reading input files starting with : " << input_file_prefix << "\n";
  std::cerr << "Number of files: " << num_files << "\n";


  std::map<GlobalIndexType, GlobalIndexType> arcs_map;
  std::map<GlobalIndexType, GlobalIndexType>::iterator it;

  for (int i=0; i<num_files; i++) {
    
    char input_filename[64];
    sprintf(input_filename, "%s%d.dat", input_file_prefix, i);

    FILE *fp = fopen(input_filename, "rb");
    if (fp == NULL) {
      std::cerr << "Error in opening file : " << input_filename << "\n";
      return 1;
    }

    int file_size;
    fseek(fp , 0 , SEEK_END);
    file_size = ftell(fp);
    rewind (fp);

    char* buffer;

    if (file_size > 0) {
      buffer = (char*)malloc(sizeof(char)*file_size);
      fread(buffer, 1, file_size, fp);
    }
    else {
      std::cerr << "Skipping empty file: " << input_filename << "\n";
      fclose(fp);
      continue;
    }

    std::cerr << "Reading file : " << input_filename 
              << " : bytes : " << file_size << "\n";

    int num_arcs = file_size/(sizeof(GlobalIndexType)*2);
    std::cerr << "Number of arcs: " << num_arcs << "\n";

    GlobalIndexType* arcs_buffer = (GlobalIndexType*)buffer;

    for (int k=0; k<num_arcs; k++) {
      GlobalIndexType arc[2];
      
      arc[0] = *arcs_buffer;
      arcs_buffer++;
      arc[1] = *arcs_buffer;
      arcs_buffer++;

      it = arcs_map.find(arc[0]);

      if (it!=arcs_map.end()) {
        if (it->second != arc[1]) {
          std::cerr << "ERROR: Found different node to : " << it->first
                    << " existing node : " << it->second
                    << " new node : " << arc[1]
                    << " Task : " << i << "\n";
          assert(it->second == arc[1]);
        }
      }
      else {
        arcs_map[arc[0]] = arc[1];
      }
    }
    free(buffer);
    fclose(fp);
  }

  std::cerr << "Number of edges in final tree: " << arcs_map.size() << "\n";


  FILE *op;
  op = fopen("full_tree.dot", "w");

  fprintf(op, "digraph G {\n");
  for (it=arcs_map.begin(); it!=arcs_map.end(); it++) {
    fprintf(op, "%llu->%llu\n", it->first, it->second);
  }
  fprintf(op, "}\n");

  fclose(op);

  return 0;
}
