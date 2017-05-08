/*
 * Definitions.h
 *
 *  Created on: Jan 26, 2015
 *      Author: bremer5
 */

#ifndef TYPEDEFINITIONS_H_
#define TYPEDEFINITIONS_H_

#include <stdint.h>

#if USE_TOPO_FILE_PARSER
#include "TalassConfig.h"
#else
//! The local index type
typedef uint32_t LocalIndexType;

//! The signed local index type
typedef int32_t SignedLocalIndexType;

//! The NULL element for local indices
const LocalIndexType LNULL = (LocalIndexType)(-1);

//! The global index type for large grids
typedef uint32_t GlobalIndexType;

//! The NULL element for global indices
const GlobalIndexType GNULL = (GlobalIndexType)(-1);

//! The function type of the values
typedef float FunctionType;

#endif


#endif /* TYPEDEFINITIONS_H_ */
