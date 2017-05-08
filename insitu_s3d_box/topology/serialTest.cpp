#include <cstdio>
#include <iostream>
#include <string>
#include <vector>
#include "ParallelTopology/PointIndex.h"
#include "ParallelTopology/TBox.h"
#include "ParallelTopology/Patch.h"
#include "ParallelTopology/SerialControl.h"
#include "ParallelTopology/ModuloGather.h"
#include "ParallelTopology/ModuloScatter.h"
#include "ParallelTopology/DataSpaceOutputStream.h"
#include "ParallelTopology/DataSpaceInputStream.h"
#include "ParallelTopology/LocalComputeAlgorithm.h"
#include "mpi.h"


#include "StreamingTopology/EnhancedSegMergeTree.h"
#include "StreamingTopology/MultiResGraph.h"
#include "StreamingTopology/MappedSegmentation.h"
#include "StreamingTopology/GraphIO.h"
#include "StreamingTopology/ArcMetrics.h"
//#include "TopologyFileParser/TopoFileDefinitions.h"
#include "StreamingTopology/FeatureFamily.h"
#include "FlexArray/BlockedArray.h"

// dummy values for now
PointIndex globalDimensions(0, 0, 0);

class SharedVertex : public SegmentedUnionVertex<GlobalIndexType>
{
public:

  typedef SetBranch<SharedVertex> BranchType;


  SharedVertex(GlobalIndexType id=GNULL, FunctionType f=0, uint8_t mult=0) :
    Vertex(id,f), SegmentedUnionVertex<GlobalIndexType>(id,f), mMult(mult) {}

  SharedVertex(const SharedVertex& vertex) :
    Vertex(vertex), SegmentedUnionVertex<GlobalIndexType>(vertex), mMult(vertex.mMult),
    mBranch(vertex.mBranch) {}

  ~SharedVertex() {}

  SharedVertex& operator=(const SharedVertex& vertex) {
    SegmentedUnionVertex<GlobalIndexType>::operator=(vertex);
    mMult = vertex.mMult;
    mBranch = vertex.mBranch;
    return *this;
  }

  BranchType* branch() {return mBranch.branch();}

  void branch(BranchType* b) {mBranch.branch(b);}

  //! Return the number of copies remaining
  uint8_t multiplicity() const {return mMult;}

  //! Decrease the multiplicity
  void integrate() {mMult--;}

private:

  //! The remaining multiplicity
  uint8_t mMult;

  BranchInfo<BranchType> mBranch;

};

class GlobalMergeTree : public SegmentedUnionTree<SharedVertex>
{
public:

  GlobalMergeTree(TopoGraphInterface* graph, UnionSegmentation* segmentation) :
    SegmentedUnionTree<SharedVertex>(graph,segmentation) {
    this->mAlgorithm = new EnhancedSegUnionAlgorithm<SharedVertex>(new VertexCompare(1));
  }

};

class MappedMTSegmentation : public MappedSegmentation
{
public:

  //! Constructor
  MappedMTSegmentation(const MappedSegmentation::FunctionArrayType& function) : MappedSegmentation(function) {}

  ~MappedMTSegmentation() {}
private:

  Node* child(Node* node)  {return node->down()[0];}

  bool smaller(const Node& node, GlobalIndexType i, FunctionType f) {
    if (node.f() < f)
      return true;
    else if ((node.f() == f) && (node.id() < i))
      return true;

    return false;
  }

  bool greater(const Node& node, GlobalIndexType i, FunctionType f) {
    if (node.f() > f)
      return true;
    else if ((node.f() == f) && (node.id() > i))
      return true;

    return false;
  }

};





using namespace std;
int main(int argc, char** argv) {

  if(argc < 9) {
    printf("Usage: %s inputfile gXDim gYDim gZDim nBucketsX nBucketsY nBucketsZ npes <additional inputfiles> \n", argv[0]);
    return 0;
  }

  MPI_Init(&argc, &argv);
  int rank, size;
  MPI_Comm_rank( MPI_COMM_WORLD, &rank ); 
  MPI_Comm_size( MPI_COMM_WORLD, &size );
    
  // initialize based off of parameters
  vector<string> filenames;
  PointIndex globalDimensions(atoi(argv[2]), atoi(argv[3]), atoi(argv[4]));
  PointIndex numBuckets(atoi(argv[5]), atoi(argv[6]), atoi(argv[7]));
  int npes = atoi(argv[8]);

  GlobalIndexType globalSize = globalDimensions.size(); 
  int numFiles = 1;
  if(argc > 9) numFiles += argc-9;

  filenames.resize(numFiles);
  filenames[0] = string(argv[1]);

  if(argc > 9) {
    for(int i=9; i < argc; i++) filenames[i-9+1] = string(argv[i]);
  }

  int numGhost = 1;
  PointIndex zeroPoint(0,0,0);
  PointIndex globalLow(-1*numGhost, -1*numGhost, -1*numGhost);
  PointIndex globalHigh(globalDimensions[0] + numGhost, 
	  globalDimensions[1] + numGhost, 
	  globalDimensions[2] + numGhost); 

  TBox* globalBox = new TBox(zeroPoint, globalDimensions, globalLow, globalHigh);

 


  // compute block sizes
  // Note: if the block size is not evenly divisible by numBuckets, the processing 
  // code below will handle this with the last bucket containing the addtitional elements 
  PointIndex blockSize;
  for(int i=0; i < 3; i++) {
    blockSize[i] = globalDimensions[i] / numBuckets[i]; 
  }

  std::cout << "blocksize = " << blockSize[0] << ", " << blockSize[1] << ", " << blockSize[2] << std::endl;

  // read in global data
  vector<float*> globalData(numFiles); 
  for(int i=0; i < numFiles; i++) {
    FILE* inputFile = fopen(filenames[i].c_str(), "rb");
    globalData[i] = new float[globalSize];
    std::cout << "on file " << i << " = " << filenames[i].c_str() << std::endl;
    fread(globalData[i], sizeof(float), globalSize, inputFile);
    std::cout << "just read data" << std::endl;
    fclose(inputFile);
  }

  // compute vector of patches
  vector<Patch*> patches;
  PointIndex lowerLeft, upperRight, low, high;
  for(uint32_t x=0; x < numBuckets[0]; x++) {
    lowerLeft[0] = x * blockSize[0]-numGhost;
    upperRight[0] = (x+1) * blockSize[0] + numGhost;

    low[0] = x * blockSize[0];
    if(x != numBuckets[0]-1)  high[0] = (x+1) * blockSize[0] + 1;
    else high[0] = globalDimensions[0];
	
    for(uint32_t y=0; y < numBuckets[1]; y++) {
      lowerLeft[1] = y * blockSize[1]-numGhost;
      upperRight[1] = (y+1) * blockSize[1] + numGhost;

      low[1] = y * blockSize[1];
      if(y != numBuckets[1]-1)  high[1] = (y+1) * blockSize[1] + 1;
      else high[1] = globalDimensions[1];

      for(uint32_t z=0; z < numBuckets[2]; z++) {
        lowerLeft[2] = z * blockSize[2]-numGhost;
        upperRight[2] = (z+1) * blockSize[2] + numGhost;
	
	low[2] = z * blockSize[2];
	if(z != numBuckets[2]-1)  high[2] = (z+1) * blockSize[2] + 1;
	else high[2] = globalDimensions[2];

       
        SubBox box(globalBox, low, high, lowerLeft, upperRight);
        vector<const FunctionType * > patchData;


        cout << "box Dimensions: " << endl;
        cout << "lower left: ";
        for(int z=0; z < 3; z++) {
          cout << lowerLeft[z] << " ";
        }
        cout << endl;
        cout << "low: ";
        for(int zz=0; zz < 3; zz++) {
          cout << low[zz] << " ";
        }
        cout << endl;


        cout << "upper right: ";
        for(int z=0; z < 3; z++) {
          cout << upperRight[z] << " ";
        }
        cout << endl;
        cout << "high: ";
        for(int zz=0; zz < 3; zz++) {
          cout << high[zz] << " ";
        }
        cout << endl;



        cout << "global dimensions: ";
        for(int z=0; z < 3; z++) {
          cout << globalDimensions[z] << " ";
        }
        cout << endl;

        // need to compute to make sure to account for ghost cells and 
        // varying block sizes (i.e. globalDimension%numBuckets > 0)
        PointIndex curBlockSize;
        for(int z=0; z < 3; z++) curBlockSize[z] = upperRight[z]-lowerLeft[z];
        cout << "curBlockSize: ";
        for(int z=0; z < 3; z++) {
          cout << curBlockSize[z] << " ";
        }
        cout << endl;

        for(int b=0; b < numFiles; b++) {
          cout << "data size = " << curBlockSize.size() << endl;
          FunctionType* data = new FunctionType[curBlockSize.size()];

          int count=0;
          for(int k=lowerLeft[2]; k < upperRight[2]; k++) {
            for(int j=lowerLeft[1]; j < upperRight[1]; j++) {
              for(int i=lowerLeft[0]; i < upperRight[0]; i++) {
                // note since lowerLeft & upperRight are with 
                // respect to the globalDimensisions, we need 
                // to use the local flat indexing function
                PointIndex curIndex(i, j, k);
                if(curIndex.inRange(globalDimensions)) { 
                  data[count++] = (FunctionType) globalData[b][curIndex.local(globalDimensions)];
//		  cout << "setting point index " << i << ", " << j << ", " << k << " = " << data[count-1] << endl;
		} else {  
                  data[count++] = 0.0;
//		  cout << "hard setting point index " << i << ", " << j << ", " << k << " = " << data[count-1] << endl;
		}		
              }
            }
          }
          patchData.push_back(data);
        }
        patches.push_back(new Patch(patchData, box));

      }

    }
  }
   // std::cout << "created patches" << std::endl;


  std::vector<SegmentedMergeTree*> local_trees;
  std::vector<DataSpaceOutputStream*> outputs;
  uint8_t block_bits = 10;
  uint32_t i;
  LocalComputeAlgorithm* algorithm = LocalComputeAlgorithm::make(LOCAL_SORTED_UF,false);


  local_trees.resize(patches.size());
  outputs.resize(patches.size());

  //cout << "initializing head" << endl;
  INIT_LIST_HEAD(&DataSpaceOutputStream::sDataList);

  // Create all the local trees and the coresponding streams
  for (i=0;i<patches.size();i++) {

    local_trees[i] = new SegmentedMergeTree(i, block_bits);
    outputs[i] = new DataSpaceOutputStream(local_trees[i]->id(), 0, patches.size(), true);
  }

  // Compute all the local trees
  for (i=0;i<patches.size();i++) {
 //   std::cout << "applying local algorithm to patch " << i << std::endl;
    algorithm->apply(patches[i],0,*local_trees[i],outputs[i]);

    // Flush the info to the stream
    std::cout << "flushing  info to stream" << i << std::endl;
    outputs[i]->flush();
    delete outputs[i];
  }


  // Now the DataSpaceOutputStream::DataList has all the local
  // trees serialized into a list of DataElements


  MultiResGraph<> global_graph;
  MappedSegmentation::FunctionArrayType function_values;
  std::map<GlobalIndexType,LocalIndexType> index_map;
  std::map<GlobalIndexType,LocalIndexType>::iterator mIt,mIt2;
  MappedMTSegmentation segmentation(function_values);
  GlobalMergeTree global_tree(&global_graph,&segmentation);
  FlexArray::BlockedArray<GlobalIndexType> global_indices(22);
  FlexArray::BlockedArray<GlobalIndexType> seg_indices(22);



  LocalIndexType count=0;
  Token::Token* t = NULL;
  
  struct data_item *item;
  list_for_each_entry(item, &DataSpaceOutputStream::sDataList, item_entry) {

    DataSpaceInputStream input(item);

    input >> t;
    while (t->type() > 0 && t->type() < Token::EMPTY) {
    printf("processing a token %d\n",t->type());
  
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

          fprintf(stderr,"Processing token %d %d\n",t->type(),f.index());
          mIt = index_map.find(f.index());
          if (mIt != index_map.end()) {

            v = global_tree.findVertex(mIt->second);

            // We now force a multiplicity of 0 on local minima. This may result in
            // a vertex that is part of the index map but not of the tree.
	    if ((v != NULL)  && (v->multiplicity() > 0)) {

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
	 printf("empty fbomb\n");
          break;

        case Token::UNDEFINED:
          sterror(true,"Undefined token.");
          break;
	default:
	 printf("fbomb\n");
         break;
      }
      input >> t;
    }
  } // For all boxes

  global_tree.cleanup();

  segmentation.complete(global_graph);

  HighestSaddleFirst<> metric(&global_graph);
  global_graph.constructHierarchy(metric,MAXIMA_HIERARCHY);

  TopologyFileFormat::ClanHandle clan("output.family");

  clan.dataset("Test");

  TopologyFileFormat::FamilyHandle family;
  family.timeIndex(0);
  family.time(1.0);
  family.variableName("my_variable");
  clan.add(family);

  write_feature_family<DefaultNodeData>(clan,global_graph);

  FILE* output = fopen("output.dot","w");

  write_dot<DefaultNodeData>(output,global_graph,30,global_graph.minF(), global_graph.maxF(), "shape=ellipse,fontsize=10");

  fclose(output);

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

  TopologyFileFormat::ClanHandle clan2("output.seg");
  clan2.dataset();

  clan2.dataset("Test");

  TopologyFileFormat::FamilyHandle family2;
  family2.timeIndex(0);
  family2.time(1.0);
  family2.variableName("my_variable");

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


  std::cout << "Finished\n" << flush;

  return 1;

  // Free all the memory
  for(i=0; i < patches.size(); i++) {

    delete local_trees[i];
    delete[] patches[i];
  }

  return 1;
};

