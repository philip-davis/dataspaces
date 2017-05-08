/*
 * KWayMerge.cpp
 *
 *  Created on: Dec 15, 2014
 *      Author: bremer5
 */

#include <cassert>

#include "KWayMerge.h"

KWayMerge::KWayMerge(uint32_t block_dim[3], uint32_t factor) : mFactor(factor)
{
  uint32_t f;
  uint32_t dim[3];
  dim[0] = block_dim[0];
  dim[1] = block_dim[1];
  dim[2] = block_dim[2];

  mLvlOffset.push_back(0);
  mLvlDim.push_back(std::vector<uint32_t>(3));
  mLvlDim[0][0] = dim[0];
  mLvlDim[0][1] = dim[1];
  mLvlDim[0][2] = dim[2];

  assert ((factor == 2) || (factor == 4) || (factor == 8) || 
          (factor == 16) || (factor == 32));

  // Now we compute how many rounds
  mRounds = 1;

  while (dim[0]*dim[1]*dim[2] > 1) {
    mLvlOffset.push_back(mLvlOffset.back()+dim[0]*dim[1]*dim[2]);
    mRounds++;

    mFactors.push_back(std::vector<uint8_t>(3));
    mFactors.back()[0] = mFactors.back()[1] = mFactors.back()[2] = 1;

    f = 1;

    while ((f < factor) && (dim[0]*dim[1]*dim[2] > 1)) {
      if ((dim[0] >= dim[1]) && (dim[0] >= dim[2])) {
        dim[0] = (dim[0]+1) / 2;
        mFactors.back()[0] *= 2;
      }
      else if (dim[1] >= dim[2]) {
        dim[1] = (dim[1] + 1) / 2;
        mFactors.back()[1] *= 2;
      }
      else {
        dim[2] = (dim[2]+1) / 2;
        mFactors.back()[2] *= 2;
      }

      f *= 2;
    }

    mLvlDim.push_back(std::vector<uint32_t>(3));
    mLvlDim.back()[0] = dim[0];
    mLvlDim.back()[1] = dim[1];
    mLvlDim.back()[2] = dim[2];
  }
  mLvlOffset.push_back(mLvlOffset.back()+1);

  mFactors.push_back(std::vector<uint8_t>(3));
  mFactors.back()[0] = mFactors.back()[1] = mFactors.back()[2] = 1;

  //for (uint8_t i=0;i<mRounds;i++)
    //fprintf(stderr,"Lvl %d: dim [%d,%d,%d], offset %d, factors [%d,%d,%d]\n",
    //        i,mLvlDim[i][0],mLvlDim[i][1],mLvlDim[i][2],mLvlOffset[i],
    //        mFactors[i][0],mFactors[i][1],mFactors[i][2]);

}

TaskId KWayMerge::size() const
{
  assert (false); // Not meaningfull;
}

std::vector<Task> KWayMerge::localGraph(ControllerId id, 
                                        const TaskMap* task_map) const
{
  TaskId i;

  // First get all the ids we need
  std::vector<TaskId> ids = task_map->tasks(id);

  // The create the required number of tasks
  std::vector<Task> tasks(ids.size());
  std::vector<Task>::iterator it;

  //! Now assign all the task ids
  for (i=0;i<ids.size();i++)
    tasks[i].id(ids[i]);


  std::vector<TaskId> incoming;
  std::vector<std::vector<TaskId> > outgoing;

  //! Now assign the callback functions as well as the incoming and outgoing
  for (it=tasks.begin();it!=tasks.end();it++) {

    if (gatherTask(it->id())) { // If this is part of the reduction

      if (it->id() < mLvlOffset[1]) { // If this is a leaf node
        it->callback(1); // Local compute

        incoming.resize(1); // One dummy input
        incoming[0] = TNULL;
        it->incoming(incoming);

        // Two output: (1) the boundary tree (2) the local information
        outgoing.resize(2);

        outgoing[0].resize(1);
        outgoing[0][0] = reduce(it->id()); // parent

        outgoing[1].resize(1);
        outgoing[1][0] = roundId(it->id(),1); // Myself in round 1

        it->outputs(outgoing);

        //fprintf(stderr,"Leaf %d: outputs %d (%d,%d)\n",it->id(),
        //        outgoing[0][0],baseId(outgoing[1][0]),round(outgoing[1][0]));

      } // end-if leaf node
      else {
        //fprintf(stderr,"ID = %d\n",it->id());
        uint8_t lvl = level(it->id());

        // Join computation
        it->callback(2);

        // Directly compute all the incoming
        incoming = expand(it->id());
        it->incoming(incoming);

        // Two outputs: (1) Boundary tree (down) (2) augmented tree (up)
        outgoing.resize(2);

        outgoing[0].resize(1);
        if (it->id() == mLvlOffset.back()-1)  // if this is the root
          outgoing[0][0] = TNULL; // parent
        else
          outgoing[0][0] = reduce(it->id()); // parent

        // The up neighbors are the same as the incoming but at different lvl
        outgoing[1] = incoming;
        for (uint32_t k=0;k<outgoing[1].size();k++) {
          outgoing[1][k] = roundId(outgoing[1][k],lvl);
        }

        it->outputs(outgoing);

        //fprintf(stderr,"Merge %d: incoming %d %d outputs %d (%d,%d)\n",
        //        it->id(),incoming[0],incoming[1],outgoing[0][0],
        //        baseId(outgoing[1][0]),round(outgoing[1][0]));

      }
    }// end-if gatherTask()
    else { // This is a scatter task

      TaskId local = baseId(it->id()); // get the corresponding reduction id
      uint8_t lvl = level(local);
      uint8_t rnd = round(it->id());

      incoming.resize(1);
      incoming[0] = reduce(local);
      if (rnd > lvl+1) // If this incoming is not the root of the scatter
        incoming[0] = roundId(incoming[0],rnd); // create the correct id

      it->incoming(incoming);

      if (local < mLvlOffset[1]) { // If this is a leaf node
        //printf("ROUNDS: %d , rnd : %d\n", rounds(), rnd);
        if (rnd < rounds()) {
          it->callback(3); // Local correction

          // We need the input from the previous round
          incoming.push_back(roundId(local,rnd-1));
          it->incoming(incoming);


          // One output to the next round of corrections
          outgoing.resize(1);
          outgoing[0].resize(1);
          outgoing[0][0] = roundId(local,rnd+1);

          it->outputs(outgoing);

          //printf("Leaf correction task : %d Incoming[0]: %d [1]: %d Outgoing: %d\n",
          //        it->id(), incoming[0], incoming[1], outgoing[0][0]);
        }
        else { // this is final task that does segmentation and writes output
          it->callback(4); // write the results to disk
          
          incoming[0] = roundId(local,rnd-1);
          it->incoming(incoming);

          outgoing.resize(0);
          it->outputs(outgoing);
        }
      }
      else { // Not a leaf node
        it->callback(0); // Relay task

        outgoing.resize(1);
        outgoing[0] = expand(local);
        for (uint32_t k=0;k<outgoing[0].size();k++) {
          outgoing[0][k] = roundId(outgoing[0][k],rnd);
        }

        it->outputs(outgoing);
      }
    }// end-if scatter task
  } // end-for all tasks

  return tasks;
}

int KWayMerge::output_graph(ControllerId count, 
                            const TaskMap* task_map, FILE* output)
{
  fprintf(output,"digraph G {\n");
  fprintf(output,"\trankdir=TB;ranksep=0.8;\n");

  for (uint8_t i=0;i<mRounds;i++)
    fprintf(output,"f%d [label=\"level %d\"]",i,i);


  fprintf(output,"f0 ");
  for (uint8_t i=1;i<mRounds;i++) {
    fprintf(output," -> f%d",i);
  }
  fprintf(output,"\n\n");

  std::vector<Task> tasks;
  std::vector<Task>::iterator tIt;
  std::vector<TaskId>::iterator it;

  for (uint32_t i=0;i<count;i++) {
    tasks = localGraph(i,task_map);


    for (tIt=tasks.begin();tIt!=tasks.end();tIt++) {
      if (round(tIt->id()) == 0)
        fprintf(output,"%d [label=\"(%d, %d) ,%d)\",color=red]\n",
                tIt->id(),baseId(tIt->id()),round(tIt->id()),tIt->callback());
      else
        fprintf(output,"%d [label=\"(%d, %d) ,%d)\",color=black]\n",
                tIt->id(),baseId(tIt->id()),round(tIt->id()),tIt->callback());

      for (it=tIt->incoming().begin();it!=tIt->incoming().end();it++) {
        if (*it != TNULL)
          fprintf(output,"%d -> %d\n",*it,tIt->id());
      }
    }

    for (tIt=tasks.begin();tIt!=tasks.end();tIt++)
      fprintf(output,"{rank = same; f%d; %d}\n",
              level(baseId(tIt->id())),tIt->id());

  }

  fprintf(output,"}\n");
  return 1;
}



uint8_t KWayMerge::level(TaskId id) const
{
  uint8_t l = 0;

  assert (gatherTask(id));

  // Figure out what level we are on
  while (id >= mLvlOffset[l+1])
    l++;

  return l;
}

TaskId KWayMerge::roundId(TaskId id, uint8_t round) const
{
  return (id | (round << sPostfixSize));
}


TaskId KWayMerge::reduce(TaskId source) const
{
  uint32_t l = level(source);

  // Map the global id to an in-level grid row-major id
  if (l > 0)
    source -= mLvlOffset[l];

  assert (l < mLvlOffset.size()-1);

  // Map this to an in-level row major id on the next level
  source = gridReduce(source,l);

  // Finally map it back to a global id
  source += mLvlOffset[l+1];

  return source;
}

std::vector<TaskId> KWayMerge::expand(TaskId source) const
{
  uint32_t l = level(source);

  assert (l > 0);

  // Map the global id to an in-level grid row-major id
  if (l > 0)
    source -= mLvlOffset[l];


  // Get the row-major indices for the lower level
  std::vector<TaskId> up = gridExpand(source,l);

  // Convert them to global indices
  for (uint32_t i=0;i<up.size();i++)
    up[i] += mLvlOffset[l-1];

  return up;
}


TaskId KWayMerge::gridReduce(TaskId source, uint8_t lvl) const
{
  TaskId p[3];

  assert (lvl < mLvlDim.size()-1);

  // Compute the current index
  p[0] = source % mLvlDim[lvl][0];
  p[1] = ((source - p[0]) / mLvlDim[lvl][0]) % mLvlDim[lvl][1];
  p[2] = source / (mLvlDim[lvl][0]*mLvlDim[lvl][1]);

  // Adapt the indices
  p[0] = p[0] / mFactors[lvl][0];
  p[1] = p[1] / mFactors[lvl][1];
  p[2] = p[2] / mFactors[lvl][2];


  // Compute the new index
  source = (p[2]*mLvlDim[lvl+1][1]  + p[1])*mLvlDim[lvl+1][0] + p[0];

  return source;
}

std::vector<TaskId> KWayMerge::gridExpand(TaskId source, uint8_t lvl) const
{
  TaskId p[3];
  std::vector<TaskId> up;

  assert (lvl > 0);

  // Compute the current index
  p[0] = source % mLvlDim[lvl][0];
  p[1] = ((source - p[0]) / mLvlDim[lvl][0]) % mLvlDim[lvl][1];
  p[2] = source / (mLvlDim[lvl][0]*mLvlDim[lvl][1]);

  // Adapt the indices
  p[0] = p[0] * mFactors[lvl-1][0];
  p[1] = p[1] * mFactors[lvl-1][1];
  p[2] = p[2] * mFactors[lvl-1][2];

  // Compute the new indices
  for (TaskId i=0;i<mFactors[lvl-1][2];i++) {
    for (TaskId j=0;j<mFactors[lvl-1][1];j++) {
      for (TaskId k=0;k<mFactors[lvl-1][0];k++) {

        if ((p[0]+k < mLvlDim[lvl-1][0]) && (p[1]+j < mLvlDim[lvl-1][1]) && (p[2]+i < mLvlDim[lvl-1][2]))
            up.push_back(((p[2]+i) * mLvlDim[lvl-1][1] + (p[1]+j))*mLvlDim[lvl-1][0] + (p[0]+k));
      }
    }
  }

  return up;
}



