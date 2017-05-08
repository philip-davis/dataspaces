/*
 * RelayTask.h
 *
 *  Created on: Dec 12, 2014
 *      Author: bremer5
 */

#ifndef RELAYTASK_H
#define RELAYTASK_H

#include <cassert>

#include "Controller.h"

int relay_message(std::vector<DataBlock>& inputs, std::vector<DataBlock>& outputs, TaskId task)
{
  assert (inputs.size() == 1);
  assert (outputs.size() == 1);


  //fprintf(stderr,"Relay message: %d \n", task);
  outputs[0] = inputs[0];

  return 1;
}



#endif /* RELAYTASK_H_ */
