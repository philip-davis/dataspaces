/*
 * TaskGraph.cpp
 *
 *  Created on: Dec 15, 2014
 *      Author: bremer5
 */

#include "TaskGraph.h"

int TaskGraph::output_graph(ControllerId count, const TaskMap* task_map, FILE* output)
{
  fprintf(output,"digraph G {\n");

  std::vector<Task> tasks;
  std::vector<Task>::iterator tIt;
  std::vector<TaskId>::iterator it;

  for (uint32_t i=0;i<count;i++) {
    tasks = localGraph(i,task_map);

    for (tIt=tasks.begin();tIt!=tasks.end();tIt++) {
      fprintf(output,"%d [label=\"%d,%d\"]\n",tIt->id(),tIt->id(),tIt->callback());
      for (it=tIt->incoming().begin();it!=tIt->incoming().end();it++) {
        if (*it != TNULL)
          fprintf(output,"%d -> %d\n",*it,tIt->id());
      }
    }
  }

  fprintf(output,"}\n");
  return 1;
}


