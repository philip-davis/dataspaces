/*
 * Copyright (c) 2009, NSF Cloud and Autonomic Computing Center, Rutgers University
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification, are permitted provided
 * that the following conditions are met:
 *
 * - Redistributions of source code must retain the above copyright notice, this list of conditions and
 * the following disclaimer.
 * - Redistributions in binary form must reproduce the above copyright notice, this list of conditions and
 * the following disclaimer in the documentation and/or other materials provided with the distribution.
 * - Neither the name of the NSF Cloud and Autonomic Computing Center, Rutgers University, nor the names of its
 * contributors may be used to endorse or promote products derived from this software without specific prior
 * written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 *
 */

/*
*  Shaohua Duan (2018)  RDI2 Rutgers University
*  shaohua.duan@rutgers.edu
*/

#ifndef __SLOM_H_
#define __SLOM_H_

#include <stdlib.h>
/*comment below for debugging*/
#include "bbox.h"

/*
//>>>>>>>> decomment below for debugging >>>>>>>>>>>
#define BBOX_MAX_NDIM		10
typedef unsigned long long uint64_t;

struct coord {
	uint64_t c[BBOX_MAX_NDIM];
};

struct bbox {
	int num_dims;
	struct coord lb, ub;
};
//<<<<<<<<< decomment below for debugging <<<<<<<<<<<<
*/
//Note: include point itself. 1D = 3, 2D = 9, 3D = 27, 4D = 3^4  ...
#define NUM_MAX_NEIB			27				//duan

struct data_point {							//duan
	double				attri;     /* Aligned pointer */
	double				slom_value;
	double				measure_d;
	int					num_dims;
	uint64_t			*c;
	int					num_neib;
	struct data_point	**neib_list;
};

uint64_t SLOM(double *buf, struct bbox *bb);
#endif /* __SLOM_H_ */
