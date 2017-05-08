set(PMT_HEADERS
    Comparison.h
    KWayMerge.h
    KWayTaskMap.h
    TreeNode.h
    MergeTree.h
    SortedUnionFindAlgorithm.h
    Neighborhood.h
    FullNeighborhood.h
    UnionFind.h
    BoundaryTreeExtractor.h
    AugmentedMergeTree.h
    SortedJoinAlgorithm.h
    LocalCorrectionAlgorithm.h
    )
    
set(PMT_SRC
    KWayMerge.cpp
    KWayTaskMap.cpp
    TreeNode.cpp
    MergeTree.cpp
    SortedUnionFindAlgorithm.cpp
    Neighborhood.cpp
    FullNeighborhood.cpp
    UnionFind.cpp
    BoundaryTreeExtractor.cpp
    AugmentedMergeTree.cpp
    SortedJoinAlgorithm.cpp
    LocalCorrectionAlgorithm.cpp
)

include_directories(${DATAFLOW_INCLUDE_DIR})
include_directories(${MPI_CXX_INCLUDE_DIR})

if (ENABLE_TOPO_FILE_PARSER)
    include_directories(${TOPO_PARSER_INCLUDE_DIR})
endif()


add_library(pmt STATIC ${PMT_SRC})

