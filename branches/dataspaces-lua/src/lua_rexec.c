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
*  Fan Zhang (2012)  TASSL Rutgers University
*  zhangfan@cac.rutgers.edu
*/

#ifdef DS_HAVE_LUA_REXEC

#include <stdlib.h>
#include <stdio.h>
#include <time.h>

/* Include the Lua API header files. */
#include <lua.h>
#include <lauxlib.h>
#include <lualib.h>

#include "lua_rexec.h"
#include "list.h"

struct load_block {
	struct list_head entry;
	size_t size;
	char buff[LUAL_BUFFERSIZE];
};

//TODO(fanc): add support of multiple memory buffers.
typedef struct DSpaceObj {
	void *data;
	size_t len;
	//TODO(fan): add permission flag.
} DSpaceObj;

static DSpaceObj input_data_array[10];
static DSpaceObj output_data_array[10];
static int num_data_obj = 0;

static DSpaceObj *toDSpaceObj(lua_State *L, int index)
{
	DSpaceObj *obj = (DSpaceObj *)lua_touserdata(L, index);
	if (obj == NULL) luaL_error(L, "DSpaceObj: NULL");
		return obj;
}

static DSpaceObj *checkDSpaceObj(lua_State *L, int index)
{
	DSpaceObj *obj;
	luaL_checktype(L, index, LUA_TUSERDATA);
	obj = (DSpaceObj *)luaL_checkudata(L, index, "DSpaceObj");
	if (obj == NULL) luaL_error(L, "DSpaceObj: NULL");
		return obj;
}

static DSpaceObj *pushDSpaceObj(lua_State *L)
{
	DSpaceObj *obj = (DSpaceObj *)lua_newuserdata(L, sizeof(DSpaceObj));
	luaL_getmetatable(L, "DSpaceObj");
	lua_setmetatable(L, -2);
	return obj;
}

static int DSpaceObj_input(lua_State *L)
{
	int index = luaL_optint(L, 1, 0);
	DSpaceObj *obj = pushDSpaceObj(L);
	obj->data = input_data_array[index].data;
	obj->len = input_data_array[index].len;
	//TODO(fan): add error check.
	return 1;
}

static int DSpaceObj_output(lua_State *L)
{
	int index = luaL_optint(L, 1, 0);
	DSpaceObj *obj = pushDSpaceObj(L);
	obj->data = output_data_array[index].data;
	obj->len = output_data_array[index].len;
	//TODO(fan): add error check.
	return 1;
}

static int DSpaceObj_setval(lua_State *L)
{
	DSpaceObj *obj = checkDSpaceObj(L, 1);
	int dim = luaL_optint(L, 2, 0);
	int i = luaL_optint(L, 3, 0);
	int j = luaL_optint(L, 4, 0);
	int k = luaL_optint(L, 5, 0);
	double value = luaL_optnumber(L, 6, 0);
	// TODO(fan): add support of multi-diemnsional layout.
	((double*)obj->data)[i] = value;
	return 1;
}

static int DSpaceObj_getval(lua_State *L)
{
	DSpaceObj *obj = checkDSpaceObj(L, 1);
	int dim = luaL_optint(L, 2, 0);
	int i = luaL_optint(L, 3, 0);
	int j = luaL_optint(L, 4, 0);
	int k = luaL_optint(L, 5, 0);
	// TODO(fan): add support of multi-diemnsional layout.
	double value = ((double*)obj->data)[i];
	lua_pushnumber(L, value);
	return 1;
}

static int DSpaceObj_getlen(lua_State *L)
{
	DSpaceObj *obj = checkDSpaceObj(L, 1);
	lua_pushnumber(L, obj->len);
	return 1;
}

static const luaL_Reg DSpaceObj_methods[] = {
	{"get_input_obj", DSpaceObj_input},
	{"get_output_obj", DSpaceObj_output},
	{"setval", DSpaceObj_setval},
	{"getval", DSpaceObj_getval},
	{"getlen", DSpaceObj_getlen},
	{0, 0}
};

static int DSpaceObj_gc(lua_State *L)
{
	return 0;
}

static int DSpaceObj_tostring(lua_State *L)
{
	char buff[32];
	DSpaceObj *obj = toDSpaceObj(L, 1);
	sprintf(buff, "data=%p, len=%u", obj->data, obj->len);
	lua_pushfstring(L, "DSpaceObj (%s)", buff);
	return 1;
}

static const luaL_Reg DSpaceObj_meta[] = {
	{"__gc", DSpaceObj_gc},
	{"__tostring", DSpaceObj_tostring},
	{0, 0}
};

int DSpaceObj_register(lua_State *L)
{
	luaL_openlib(L, "DSpaceObj", DSpaceObj_methods, 0);
	luaL_newmetatable(L, "DSpaceObj");
	luaL_openlib(L, 0, DSpaceObj_meta, 0);
	lua_pushliteral(L, "__index");
	lua_pushvalue(L, -3);
	lua_rawset(L, -3);
	lua_pushliteral(L, "__metatable");
	lua_pushvalue(L, -3);
	lua_rawset(L, -3);

	lua_pop(L, 1);
	return 1;
}

struct code_buffer {
	size_t size_bytes;
	size_t read_bytes;
	void * data;
};

static const char *lua_code_reader(lua_State *L, void *ud, size_t *size)
{
		size_t offset;

        struct code_buffer *ptr = (struct code_buffer*)ud;
        offset = ptr->read_bytes;
        if ( (ptr->size_bytes - ptr->read_bytes) >= LUAL_BUFFERSIZE ) {
                *size = LUAL_BUFFERSIZE;
                ptr->read_bytes += *size;
        } else {
                *size = ptr->size_bytes - ptr->read_bytes;
                ptr->read_bytes += *size;
        }

        return ptr->data+offset;
}

static void copy_data(lua_State *l, double *data, int num_elem)
{
        int i;

        lua_newtable(l);

        for (i = 1; i <= num_elem; i++) {
                lua_pushnumber(l, i); //push the table index
                lua_pushnumber(l, data[i]); //push the cell values
                lua_rawset(l, -3); //stores the pair in the table
        }

        lua_setglobal(l, "data");

        return;
}

// Public APIs.

int lua_exec(void *code_buff, size_t code_size)
{
        lua_State *l;
        // Create a new Lua state.
        l = luaL_newstate();
        // Opens all standard Lua libraries into the given state.
        luaL_openlibs(l);

        int num_output_elem = -1;
        struct code_buffer buf;
        buf.data = code_buff;
        buf.size_bytes = code_size;
        buf.read_bytes = 0;

        // printf("lua_script_size= %u\n", code_size);

        DSpaceObj_register(l);

        /* load the in-memory lua script */
        int status = lua_load(l, lua_code_reader, &buf, "LUA_CODE", NULL);
        if (status != LUA_OK) {
                fprintf(stderr, "lua_load() failed!\n");
        }

        status = lua_pcall(l, 0, 1, 0);
        if (status != LUA_OK) {
                fprintf(stderr, "lua_pcall() failed!\n");
                goto err_out;
        }

        /* Get the returned value at the top of the stack. */
        num_output_elem = lua_tonumber(l, -1);
        lua_pop(l, 1);  // Take the returned value out of the stack.

        /* Remember to destroy the Lua State */
        lua_close(l);

        return num_output_elem;
err_out:
        lua_close(l);
        return -1;
}

void lua_exec_set_input_data(void *data, size_t len) {
	input_data_array[0].data = data;
	input_data_array[0].len = len;
	num_data_obj++;
}

void lua_exec_unset_input_data() {
	input_data_array[0].data = NULL;
	input_data_array[0].len = 0;
	if (--num_data_obj < 0)
		num_data_obj = 0;
}

void lua_exec_set_output_data(void *data, size_t len) {
	output_data_array[0].data = data;
	output_data_array[0].len = len;
	num_data_obj++;
}

void lua_exec_unset_output_data() {
	output_data_array[0].data = NULL;
	output_data_array[0].len = 0;
	if (--num_data_obj < 0)
		num_data_obj = 0;
}

int lua_load_script_file(const char *fname, void **code_buf, size_t *code_size)
{
		struct list_head blk_list;
		INIT_LIST_HEAD(&blk_list);

        FILE *f;
        f = fopen(fname, "r");
        if (f == NULL) {
                fprintf(stderr, "Failed to open '%s'!\n", fname);
                return -1;
        }

        *code_size = 0;
        while ( !feof(f) ) {
                struct load_block *ptr =
                        (struct load_block*) malloc(sizeof(struct load_block));
                ptr->size = fread(ptr->buff, 1, sizeof(ptr->buff), f);
                list_add_tail(&ptr->entry, &blk_list);
                *code_size += ptr->size;
        }

        // code_size needs to be multiple of 8 bytes.
        *code_buf = malloc(*code_size);
        fclose(f);

        struct load_block *blk, *tmp;
        size_t offset = 0;
        list_for_each_entry_safe(blk, tmp, &blk_list, struct load_block, entry) {
                memcpy(*code_buf+offset, blk->buff, blk->size);
                offset += blk->size;
                list_del(&blk->entry);
                free(blk);
        }

        return 0;
}

#endif
