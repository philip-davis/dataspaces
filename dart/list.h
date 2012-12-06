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
*/

#ifndef __LIST_H_
#define __LIST_H_

#include <stddef.h>

/* New list implementation 10/31/2008. */
/* Ref: see the list.h implementation in the Linux kernel. */

struct list_head {
        struct list_head *prev, *next;
};

#define LIST_HEAD_INIT(name) {&(name), &(name)}

#define LIST_HEAD(name) \
        struct list_head name = LIST_HEAD_INIT(name)

static inline void __list_add(struct list_head *new, 
                              struct list_head *prev, struct list_head *next)
{
        new->next = next;
        new->prev = prev;
        prev->next = new;
        next->prev = new;
}

static inline void INIT_LIST_HEAD(struct list_head *list)
{
        list->next = list;
        list->prev = list;
}

/* Add element 'new' after element 'head'. */
static inline void list_add(struct list_head *new, struct list_head *head)
{
        __list_add(new, head, head->next);
}

/* Add element 'new' before element 'head'. */
static inline void list_add_tail(struct list_head *new, struct list_head *head)
{
        __list_add(new, head->prev, head);
}

/* Unlink element 'old' from the list it belongs. */
static inline void list_del(struct list_head *old)
{
        old->prev->next = old->next;
        old->next->prev = old->prev;
        old->next = NULL;
        old->prev = NULL;
}

static inline int list_empty( struct list_head *head)
{
        return head->next == head;
}

#define list_entry(ptr, type, member)                                   \
        (type *) ((char *) ptr - offsetof(type, member))

#define list_for_each(pos, head)                                        \
        for (pos = (head)->next; pos != (head); pos = pos->next)

#define list_for_each_safe(pos, n, head)                                \
        for (pos = (head)->next, n = pos->next; pos != (head);          \
             pos = n, n = pos->next)

#define list_for_each_entry(pos, head, member)                          \
        for (pos = list_entry((head)->next, typeof(*pos), member);      \
             &pos->member !=( head);                                    \
             pos = list_entry(pos->member.next, typeof(*pos), member))

#define list_for_each_entry_safe(pos, n, head, member)                  \
        for (pos = list_entry((head)->next, typeof(*pos), member),      \
                n = list_entry(pos->member.next, typeof(*pos), member); \
             &pos->member != (head);                                 \
             pos = n, n = list_entry(pos->member.next, typeof(*pos), member))

#endif

