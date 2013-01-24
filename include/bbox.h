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
*  Ciprian Docan (2009)  TASSL Rutgers University
*  docan@cac.rutgers.edu
*  Tong Jin (2011) TASSL Rutgers University
*  tjin@cac.rutgers.edu
*/

#ifndef __BBOX_H_
#define __BBOX_H_

#include <stdio.h>

#define max(a,b) (a) > (b) ? (a):(b)
#define min(a,b) (a) < (b) ? (a):(b)

enum bb_dim {
        bb_x = 0,
        bb_y = 1,
        bb_z = 2
};

struct coord {
        int c[3];
};

struct bbox {
        int num_dims;
        struct coord lb, ub;
};

struct intv {
        // unsigned int lb, ub;
        unsigned long lb, ub;
};

int bbox_dist(struct bbox *, int);
void bbox_divide(struct bbox *b0, struct bbox *b_tab);
int bbox_include(const struct bbox *, const struct bbox *);
int bbox_does_intersect(const struct bbox *, const struct bbox *);
void bbox_intersect(struct bbox *, const struct bbox *, struct bbox *);
int bbox_equals(const struct bbox *, const struct bbox *);

unsigned long bbox_volume(struct bbox *);
void bbox_to_intv(const struct bbox *, int, int, struct intv **, int *);
void bbox_to_intv2(const struct bbox *, int, int, struct intv **, int *);
void bbox_to_origin(struct bbox *, const struct bbox *);

int intv_do_intersect(struct intv *, struct intv *);
unsigned long intv_size(struct intv *);

static unsigned int next_pow_2(int n)
{
        unsigned int i;

        if (n < 0)
                return 0;

        i = ~(~0U >> 1);
        while (i && !(i&n)) {
                i = i >>1;
        }

        i = i << 1;

        return i;
}


extern char *str_append_const(char *, const char *);
extern char *str_append(char *, char *);

static void coord_print(struct coord *c, int num_dims)
{
        switch (num_dims) {
        case 3:
                printf("{%d, %d, %d}", c->c[0], c->c[1], c->c[2]);
                break;
        case 2:
                printf("{%d, %d}", c->c[0], c->c[1]);
                break;
        case 1:
                printf("{%d}", c->c[0]);
        }
}

/*
  Routine to return a string representation of the 'coord' object.
*/
static char * coord_sprint(const struct coord *c, int num_dims)
{
	char *str;

	if (num_dims == 3)
		asprintf(&str, "{%d, %d, %d}", c->c[0], c->c[1], c->c[2]);
	else if (num_dims == 2)
		asprintf(&str, "{%d, %d}", c->c[0], c->c[1]);
	else if (num_dims == 1)
                asprintf(&str, "{%d}", c->c[0]);

	return str;
}

static void bbox_print(struct bbox *bb)
{
        printf("{lb = ");
        coord_print(&bb->lb, bb->num_dims);
        printf(", ub = ");
        coord_print(&bb->ub, bb->num_dims);
        printf("}");
}

static char * bbox_sprint(const struct bbox *bb)
{
	char *str;

	asprintf(&str, "{lb = ");
        str = str_append(str, coord_sprint(&bb->lb, bb->num_dims));
        str = str_append_const(str, ", ub = ");
        str = str_append(str, coord_sprint(&bb->ub, bb->num_dims));
        str = str_append_const(str, "}\n");

	return str;
}

#endif /* __BBOX_H_ */
