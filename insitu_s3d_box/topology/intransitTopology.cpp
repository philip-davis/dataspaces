#include "intransitTopology.h"
#include "insitu_data_description.h"

#include "ParallelTopology/PointIndex.h"
#include "ParallelTopology/TBox.h"
#include "ParallelTopology/Patch.h"
#include "ParallelTopology/SerialControl.h"
#include "ParallelTopology/ModuloGather.h"
#include "ParallelTopology/ModuloScatter.h"
#include "ParallelTopology/DataSpaceOutputStream.h"
#include "ParallelTopology/DataSpaceInputStream.h"
#include "ParallelTopology/LocalComputeAlgorithm.h"
#include "StreamingTopology/EnhancedSegMergeTree.h"
#include "StreamingTopology/MultiResGraph.h"
#include "StreamingTopology/MappedSegmentation.h"
#include "StreamingTopology/GraphIO.h"
#include "StreamingTopology/ArcMetrics.h"
#include "StreamingTopology/FeatureFamily.h"
#include "FlexArray/BlockedArray.h"


#include <iostream>
#include <fstream>
#include <cmath>
using namespace std;

//dummy value
PointIndex globalDimensions(0, 0, 0);

int perform_intransit_topology(struct list_head *data_list)
{
  struct data_item *item;
  int count=0, tstep;

  MultiResGraph<> global_graph;
  MappedSegmentation::FunctionArrayType function_values;
  std::map<GlobalIndexType,LocalIndexType> index_map;
  std::map<GlobalIndexType,LocalIndexType>::iterator mIt,mIt2;
  MappedMTSegmentation segmentation(function_values);
  GlobalMergeTree global_tree(&global_graph,&segmentation);
  FlexArray::BlockedArray<GlobalIndexType> global_indices(22);
  FlexArray::BlockedArray<GlobalIndexType> seg_indices(22);

  Token::Token* t = NULL;
  list_for_each_entry(item, data_list, item_entry) {

    tstep = item->desc.tstep;

    DataSpaceInputStream input(item);

    input >> t;
    while (t->type() > 0 && t->type() < Token::EMPTY) {
      switch (t->type()) {
        case Token::VERTEX: {
          const Token::VertexToken& v = *t;
          if (index_map.find(v.index()) == index_map.end()) {
#if 0
            index_map[v.index()] = count;
            global_tree.addVertex(SharedVertex(count++,v.value(),v.multiplicity()));
#else
            index_map[v.index()] = v.index();
            global_tree.addVertex(SharedVertex(v.index(),v.value(),v.multiplicity()));
#endif
            function_values.insert(v.index(),FlexArray::MappedFunctionElement<GlobalIndexType,LocalIndexType>(v.value(),v.index()));
            //fprintf(stdout,"Vertex %d %d %d\n",t->type(),index_map.find(v.index())->second,v.multiplicity());
         }
         break;
       }  
       case Token::EDGE: {
         const Token::EdgeToken& e = *t;
         mIt = index_map.find(e[0]);
         mIt2 = index_map.find(e[1]);

         if ((mIt != index_map.end()) && (mIt2 != index_map.end())) {
           //fprintf(stdout,"Edge token %d %d %d\n",t->type(),e[0],e[1]);
           global_tree.addEdge(mIt->second,mIt2->second);
         }
         break;
      }
      case Token::FINAL: {
        const Token::FinalToken& f = *t;
        SharedVertex* v;

        //fprintf(stderr,"Processing token %d %d\n",t->type(),f.index());
        mIt = index_map.find(f.index());
        if (mIt != index_map.end()) {
          v = global_tree.findVertex(mIt->second);
          // We now force a multiplicity of 0 on local minima. This may result in
          // a vertex that is part of the index map but not of the tree.
          if ((v != NULL) && (v->multiplicity() > 0)) {
            sterror(v->multiplicity() == 0,"Found too many finalizations for vertex");
            v->integrate();
            assert(v->id() == mIt->second);
            if (v->multiplicity() == 0)
              global_tree.finalizeVertex(v->id());
          }
        }
        break;
      }
        case Token::SEG: {
          const Token::SegToken& s = *t;

          global_indices.push_back(s.id());
          seg_indices.push_back(s.seg());
          break;
        }
        case Token::EMPTY:
          break;

        case Token::UNDEFINED:
          sterror(true,"Undefined token.");
          break;
      }

      input >> t;
    }
  } 
  global_tree.cleanup();

  segmentation.complete(global_graph);

  HighestSaddleFirst<> metric(&global_graph);
  global_graph.constructHierarchy(metric,MAXIMA_HIERARCHY);

  char filename[256];
  sprintf(filename, "../post/topology/output-%d.family", tstep);
  TopologyFileFormat::ClanHandle clan(filename);

  clan.dataset("Test");

  TopologyFileFormat::FamilyHandle family;
  family.timeIndex(0);
  family.time(1.0);
  family.variableName(gInsituVarNames_ghost[0]);
  clan.add(family);

  write_feature_family<DefaultNodeData>(clan,global_graph);

  sprintf(filename, "../post/topology/output-%d.dot", tstep);
  FILE* output = fopen(filename,"w");

  write_dot<DefaultNodeData>(output,global_graph,30,global_graph.minF(), global_graph.maxF(), "shape=ellipse,fontsize=10");

  fclose(output);

/*
  count = 0;
  std::map<GlobalIndexType,LocalIndexType> seg_map;
  std::vector<LocalIndexType> critical_ids;
  std::vector<LocalIndexType>::iterator vIt;

 // Get a list of id's of all active critical points
  global_graph.createGlobalIndices(critical_ids);
  for (vIt=critical_ids.begin();vIt!=critical_ids.end();vIt++)
    seg_map[*vIt] = count++;


  // Remap all segmentation indices of all vertices. Note that
  // the seg.seg() is a mapped segmentation so the [] operator
  // will do a find()
  for (GlobalIndexType ii=0;ii<global_indices.size();ii++)
    seg_indices[ii] = segmentation.segmentation()[seg_indices[ii]];


  // Finally we resort the vertices into segments according to the map
  // we build
  std::vector<std::vector<GlobalIndexType> > segments(seg_map.size());
  for (GlobalIndexType ii=0;ii<global_indices.size();ii++) {

    mIt = seg_map.find(seg_indices[ii]);
    assert (mIt!=seg_map.end());

    segments[mIt->second].push_back(global_indices[ii]);
  }

  sprintf(filename, "../post/topology/output-%d.seg", tstep);
  TopologyFileFormat::ClanHandle clan2(filename);
  clan2.dataset();

  clan2.dataset("Test");

  TopologyFileFormat::FamilyHandle family2;
  family2.timeIndex(0);
  family2.time(1.0);
  family2.variableName(gInsituVarNames_ghost[0]);

  char descriptor[100];

  sprintf(descriptor,"%d %d %d",globalDimensions[0],globalDimensions[1],globalDimensions[2]);

  std::string domain(descriptor);


  TopologyFileFormat::SegmentationHandle seg_handle;
  seg_handle.domainType(TopologyFileFormat::REGULAR_GRID);
  seg_handle.domainDescription(domain);
  seg_handle.setSegmentation(&segments);
  seg_handle.encoding(true);

  family2.add(seg_handle);
  clan2.add(family2);

  clan2.write();
*/

  std::cout << "Finished\n" << flush;

  return 1;

};

