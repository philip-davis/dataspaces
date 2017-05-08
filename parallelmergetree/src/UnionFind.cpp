#include "UnionFind.h"
#include <iostream>


GlobalIndexType UnionFind::rep(GlobalIndexType id)
{
  LocalIndexType local;

  //! Sanity check to make sure we ask only for existsing labels
  assert(mIndexMap.find(id) != mIndexMap.end());

  //! Get the local index of the label in question
  local = mIndexMap.find(id)->second;

  //! Jump "upward" until you find the current representative
  std::stack<LocalIndexType> s;
  while (mLabel[local] != id) {
    s.push(local);
    id = mLabel[local];

    assert(mIndexMap.find(id) != mIndexMap.end());
    local = mIndexMap.find(id)->second;
  }

  //! Shortcut the structure
  if (!s.empty()) {
    s.pop();
    while (!s.empty()) {
      mLabel[s.top()] = id;
      s.pop();
    }
  }
  return id;
}

void UnionFind::addLabel(GlobalIndexType label)
{
  mLabel.push_back(label);
  mIndexMap[label] = mLabel.size()-1;
}

void UnionFind::mergeLabel(GlobalIndexType from, GlobalIndexType to)
{
  assert(mIndexMap.find(from) != mIndexMap.end());
  assert(mIndexMap.find(to) != mIndexMap.end());

  // Make sure the "newer" label survives
  if (mIndexMap.find(from)->second > mIndexMap.find(to)->second) {
    std::cerr << "from : " << from << " to " << to << "\n";
    assert(mIndexMap.find(from)->second < mIndexMap.find(to)->second);
  }
  
  mLabel[mIndexMap.find(from)->second] = to;
}
