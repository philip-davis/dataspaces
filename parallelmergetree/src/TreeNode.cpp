/*
 * TreeNode.cpp
 *
 *  Created on: Jan 26, 2015
 *      Author: bremer5
 */

#include "TreeNode.h"
#include <iostream>

bool TreeNode::operator<(const TreeNode& n) const
{
  if (mValue < n.value())
    return true;

  if ((mValue == n.value()) && (mId < n.id()))
    return true;

  return false;
}

bool TreeNode::regular() const
{
  if ((mDown == LNULL) || (mUp == LNULL))
    return false;

  if (up()->next() != up())
    return false;

  if (this->boundary())
    return false;

  if (this->finalizedSaddle())
    return false;

  if (this->id() == GNULL)
    return false;
  
  return true;
}



