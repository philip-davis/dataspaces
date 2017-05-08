#include "dataspaces_api.h"
#include "StreamingTopology/EnhancedSegMergeTree.h"
#include "StreamingTopology/MultiResGraph.h"
#include "StreamingTopology/MappedSegmentation.h"
#include "StreamingTopology/GraphIO.h"
#include "StreamingTopology/ArcMetrics.h"

int perform_intransit_topology(struct list_head *data_list);

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


