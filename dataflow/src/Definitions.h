/*
 * Definitions.h
 *
 *  Created on: Dec 12, 2014
 *      Author: bremer5
 */

#ifndef DEFINITIONS_H_
#define DEFINITIONS_H_

#include <stdint.h>

//! Index type used to identify tasks
typedef uint32_t TaskId;

//! The NULL element indicating an outside input
const TaskId TNULL = (TaskId)-1;

//! Index type used to identify a controller
typedef uint32_t ControllerId;

//! The NULL element indicating no controller
const ControllerId CNULL = (ControllerId)-1;

//! Index type used to register callbacks
typedef uint8_t CallbackId;


#endif /* DEFINITIONS_H_ */
