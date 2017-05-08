/***************************************************
 ** ViSUS Visualization Project                    **
 ** Copyright (c) 2010 University of Utah          **
 ** Scientific Computing and Imaging Institute     **
 ** 72 S Central Campus Drive, Room 3750           **
 ** Salt Lake City, UT 84112                       **
 **                                                **
 ** For information about this project see:        **
 ** http://www.pascucci.org/visus/                 **
 **                                                **
 **      or contact: pascucci@sci.utah.edu         **
 **                                                **
 ****************************************************/

#include "PIDX_rst.h"
#define PIDX_MAX_NEIGHBOR_PROC 8096
#define PIDX_MAX_DIMS 5
#undef DEBUG_RST 

static int mem_create_required = 1;

//Struct for a generic cube with dims simensions with
//bounds specified by lower_bound and upper_bound

struct NDim_chunk_bound {
    int dims;
    int lower_bound[PIDX_MAX_DIMENSIONS];
    int upper_bound[PIDX_MAX_DIMENSIONS];
};
typedef struct NDim_chunk_bound NDim_chunk;

//Struct for restructuring ID

struct PIDX_rst_struct {
    //Passed by PIDX API
#ifdef MPI
    MPI_Comm comm; //Communicator
#endif
    int dims; //dimension of the dataset (1d, 2d, 3d, 4d, 5d)
    int* local_count; //offset of every process (0, 32, 32, 0, 0) for a 3d data set 
    int* local_offset; //count for the process (32, 32, 32, 0, 0) for a 3d data set
    int* global_extents; //global extent of data volume (256, 256, 256, 0, 0)

    //Exact count and info of neighboring processes for every process
    int nbor_proc_count; //Neighboring process count for every process
    int nbor_proc_rank[PIDX_MAX_NEIGHBOR_PROC]; //Rank of neighboring processes
    int nbor_proc_lbound[PIDX_MAX_NEIGHBOR_PROC][PIDX_MAX_DIMS]; //lower bound of intersecting chunk of neighbor process with imposed regular box
    int nbor_proc_ubound[PIDX_MAX_NEIGHBOR_PROC][PIDX_MAX_DIMS]; //upper bound of intersecting chunk of neighbor process with imposed regular box

    //There could be more than one regular imposed box a process intersects with, hence a mantain a list of it.
    int regular_box_dim[PIDX_MAX_DIMS];
    int regular_nbor_proc_lbound[PIDX_MAX_NEIGHBOR_PROC][PIDX_MAX_DIMS]; //lower bound of imposed regular box for every neighboring process
    int regular_nbor_proc_ubound[PIDX_MAX_NEIGHBOR_PROC][PIDX_MAX_DIMS]; //lower bound of imposed regular box for every neighboring process
    int volume[PIDX_MAX_NEIGHBOR_PROC]; //Intersecting volume of imposed regular box and intersecting neighboring processes.
    int to_send_rank[PIDX_MAX_NEIGHBOR_PROC]; //Rank of process to which a process needs to send its data chunk
    int recieve_send[PIDX_MAX_NEIGHBOR_PROC]; //1 if the neighbor is a reciever or 0 if it is a sender

    //keeping track of multiple regular boxes
    int block_offset[PIDX_MAX_NEIGHBOR_PROC];
    int block_count[PIDX_MAX_NEIGHBOR_PROC];

};

//Function to check if NDimensional data chunks A and B intersects

int intersectNDChunk(NDim_chunk* A, NDim_chunk* B) {
    int d = 0, check_bit = 0;
    for (d = 0; d < /*A->dims*/PIDX_MAX_DIMS; d++) {
        check_bit = check_bit || A->upper_bound[d] < B->lower_bound[d] || B->upper_bound[d] < A->lower_bound[d];
    }
    //return !(A->upper_bound[0] < B->lower_bound[0]  || B->upper_bound[0] < A->lower_bound[0] || A->upper_bound[1] < B->lower_bound[1] || B->upper_bound[1] < A->lower_bound[1] || A->upper_bound[2] < B->lower_bound[2] || B->upper_bound[2] < A->lower_bound[2] );
    return !(check_bit);
}

//Function to find the power of 2 of an integer value (example 5->8)

int getPowerOftwo(int x) {
    int n = 1;
    while (n < x)
        n <<= 1;
    return n;
}

//Function to find the dimension of the imposing regular box

void set_default_box_size(PIDX_rst_id rst_id, int* process_bounds, int nprocs) {
    int i = 0, average_count = 0, j = 0;
    int check_bit = 0;
    int* max_dim_length;
    int equal_partiton = 1;


    max_dim_length = (int*) malloc(sizeof (int) * rst_id->dims);
    assert(max_dim_length);
    memset(max_dim_length, 0, sizeof (int) * rst_id->dims);

    for (i = 0; i < rst_id->dims; i++) {
        max_dim_length[i] = process_bounds[PIDX_MAX_DIMENSIONS * 0 + i];
        for (j = 0; j < nprocs; j++) {
            if (max_dim_length[i] <= process_bounds[PIDX_MAX_DIMENSIONS * j + i])
                max_dim_length[i] = process_bounds[PIDX_MAX_DIMENSIONS * j + i];
        }
    }

    for (i = 0; i < rst_id->dims; i++)
    {
        average_count = average_count + max_dim_length[i];
	
    }

    average_count = average_count / rst_id->dims;
    average_count = getPowerOftwo(average_count);
    

    for (i = 0; i < rst_id->dims; i++)
        check_bit = check_bit || ((double) rst_id->global_extents[i] / average_count > (double) rst_id->global_extents[i] / max_dim_length[i]);

    while (check_bit) {
        average_count = average_count * 2;
        check_bit = 0;
        for (i = 0; i < rst_id->dims; i++)
            check_bit = check_bit || ((double) rst_id->global_extents[i] / average_count > (double) rst_id->global_extents[i] / max_dim_length[i]);
    }
    //regular_box_dim =  average_count;
    if (equal_partiton == 1) {
        rst_id->regular_box_dim[0] = average_count * 1;
        rst_id->regular_box_dim[1] = average_count * 1;
        rst_id->regular_box_dim[2] = average_count * 1;
        rst_id->regular_box_dim[3] = average_count * 1;
        rst_id->regular_box_dim[4] = average_count * 1;
    } else {
        rst_id->regular_box_dim[0] = getPowerOftwo(process_bounds[0]) * 1;
        rst_id->regular_box_dim[1] = getPowerOftwo(process_bounds[1]) * 1;
        rst_id->regular_box_dim[2] = getPowerOftwo(process_bounds[2]) * 1;
        rst_id->regular_box_dim[3] = getPowerOftwo(process_bounds[3]) * 1;
        rst_id->regular_box_dim[4] = getPowerOftwo(process_bounds[4]) * 1;
    }

    free(max_dim_length);
    max_dim_length = 0;

    //regular_box_dim = regular_box_dim * 4;
}


int* PIDX_rst_get_box_dimension(PIDX_rst_id id) {
    return id->regular_box_dim;
}

/* output value: num_output_buffers (number of buffers this process will hold after restructuring given the above parameters) */
PIDX_rst_id PIDX_rst_init(
#ifdef MPI
        MPI_Comm comm,
#endif
        int dimension, int* global_dimension, int* count, int* global_index, int set_box_dim, int* box_dim, int* num_output_buffers) {

    int r, d, i, j, k, l, m, t, total_volume = 0, nprocs, rank, ret;
    int pcount = 0, starting_index = 0, pseudo_pcount = 0;
    int *rank_r_offset, *rank_r_count;

    //Creating the restructuring ID
    PIDX_rst_id rst_id;
    rst_id = (PIDX_rst_id)malloc(sizeof (*rst_id));
    if (!rst_id) PIDX_rst_print_error("Error Creating Rst ID", __FILE__, __LINE__);
    memset(rst_id, 0, sizeof (*rst_id));

    //copying parameters passed to function from PIDX
    rst_id->dims = dimension;
    rst_id->global_extents = global_dimension;
    rst_id->local_count = count;
    rst_id->local_offset = global_index;

#ifdef MPI
    ret = MPI_Comm_dup(comm, &rst_id->comm);
    if (ret != MPI_SUCCESS) PIDX_rst_print_error("Communicator Duplication", __FILE__, __LINE__);

    ret = MPI_Comm_rank(rst_id->comm, &rank);
    if (ret != MPI_SUCCESS) PIDX_rst_print_error("Rank ", __FILE__, __LINE__);

    ret = MPI_Comm_size(rst_id->comm, &nprocs);
    if (ret != MPI_SUCCESS) PIDX_rst_print_error("nprocs ", __FILE__, __LINE__);
#else
    rank = 0;
    nprocs = 1;
#endif

    //creating rank_r_count and rank_r_offset to hold the offset and count of every process
    rst_id->nbor_proc_count = 0;

    rank_r_offset = (int*) malloc(sizeof (int) * nprocs * PIDX_MAX_DIMENSIONS);
    if (!rank_r_offset) PIDX_rst_print_error("Memory : rank_r_offset", __FILE__, __LINE__);
    memset(rank_r_offset, 0, (sizeof (int) * nprocs * PIDX_MAX_DIMENSIONS));

    rank_r_count = (int*) malloc(sizeof (int) * nprocs * PIDX_MAX_DIMENSIONS);
    if (!rank_r_count) PIDX_rst_print_error("Memory : rank_r_count", __FILE__, __LINE__);
    memset(rank_r_count, 0, (sizeof (int) * nprocs * PIDX_MAX_DIMENSIONS));

    //STEP 1 : Doing an all to all Communication to get extents of all processes.
#ifdef MPI
    ret = MPI_Allgather((int*) rst_id->local_offset, PIDX_MAX_DIMENSIONS, MPI_INT, rank_r_offset, PIDX_MAX_DIMENSIONS, MPI_INT, MPI_COMM_WORLD);
    if (ret != MPI_SUCCESS) PIDX_rst_print_error("MPI_Allgather : rank_r_offset", __FILE__, __LINE__);

    MPI_Allgather((int*) rst_id->local_count, PIDX_MAX_DIMENSIONS, MPI_INT, rank_r_count, PIDX_MAX_DIMENSIONS, MPI_INT, MPI_COMM_WORLD);
    if (ret != MPI_SUCCESS) PIDX_rst_print_error("MPI_Allgather : rank_r_count", __FILE__, __LINE__);
#else
    for (i = 0; i < PIDX_MAX_DIMENSIONS; i++) {
        rank_r_count[i] = rst_id->local_count[i];
        rank_r_offset[i] = rst_id->local_offset[i];
    }
#endif

    //STEP 2 : Compute the dimension of the regular BOX
    if(set_box_dim == 0)
    {
	set_default_box_size(rst_id, rank_r_count, nprocs);
	if(rst_id->regular_box_dim[0] == rank_r_count[PIDX_MAX_DIMENSIONS * 0 + 0]  &&   rst_id->regular_box_dim[1] == rank_r_count[PIDX_MAX_DIMENSIONS * 0 + 1] 
	  && rst_id->regular_box_dim[2] == rank_r_count[PIDX_MAX_DIMENSIONS * 0 + 2])
	  mem_create_required = 0;
    }
    else
      memcpy(rst_id->regular_box_dim, box_dim, 5 * sizeof(int));
      
    if(rank == 0)
        printf("[%d] Imposed Box Dimension : %d %d %d %d %d\n", rank, rst_id->regular_box_dim[0], rst_id->regular_box_dim[1], rst_id->regular_box_dim[2],
            rst_id->regular_box_dim[3], rst_id->regular_box_dim[4]);
    //extents for the local process(rank)
    NDim_chunk* local_proc_bound = (NDim_chunk*) malloc(sizeof (NDim_chunk));
    if (!local_proc_bound) {
        fprintf(stderr, "[Rank : %d] [File : %s] [Line : %d] local_proc_bound\n", rank, __FILE__, __LINE__);
        MPI_Abort(MPI_COMM_WORLD, -1);
    }
    memset(local_proc_bound, 0, sizeof (*local_proc_bound));
    for (d = 0; d < PIDX_MAX_DIMENSIONS; d++) {
        local_proc_bound->lower_bound[d] = rank_r_offset[PIDX_MAX_DIMENSIONS * rank + d];
        local_proc_bound->upper_bound[d] = rank_r_offset[PIDX_MAX_DIMENSIONS * rank + d] + rank_r_count[PIDX_MAX_DIMENSIONS * rank + d] - 1;
    }
    local_proc_bound->dims = rst_id->dims;

    *num_output_buffers = 0;

    //STEP 3 : iterate through extents of all imposed regular boxes, and find all the regular boxes a process (local_proc_bound) intersects with
    for (i = 0; i < rst_id->global_extents[0]; i = i + rst_id->regular_box_dim[0])
        for (j = 0; j < rst_id->global_extents[1]; j = j + rst_id->regular_box_dim[1])
            for (k = 0; k < rst_id->global_extents[2]; k = k + rst_id->regular_box_dim[2])
                for (l = 0; l < rst_id->global_extents[3]; l = l + rst_id->regular_box_dim[3])
                    for (m = 0; m < rst_id->global_extents[4]; m = m + rst_id->regular_box_dim[4]) {
			NDim_chunk* regular_box_bound = (NDim_chunk*) malloc(sizeof (NDim_chunk));
                       if (!regular_box_bound) PIDX_rst_print_error("Memory : regular_box_bound", __FILE__, __LINE__);
                       memset(regular_box_bound, 0, sizeof (*regular_box_bound));

                       regular_box_bound->dims = rst_id->dims;

			//Interior regular boxes
			regular_box_bound->lower_bound[0] = i;
			regular_box_bound->lower_bound[1] = j;
			regular_box_bound->lower_bound[2] = k;
			regular_box_bound->lower_bound[3] = l;
			regular_box_bound->lower_bound[4] = m;
//  			regular_box_bound->upper_bound[0] = i + rst_id->regular_box_dim[0] - 1;
//  			regular_box_bound->upper_bound[1] = j + rst_id->regular_box_dim[1] - 1;
//  			regular_box_bound->upper_bound[2] = k + rst_id->regular_box_dim[2] - 1;
//  			regular_box_bound->upper_bound[3] = l + rst_id->regular_box_dim[3] - 1;
//  			regular_box_bound->upper_bound[4] = m + rst_id->regular_box_dim[4] - 1;
			
  			regular_box_bound->upper_bound[0] = i + rst_id->regular_box_dim[0] - 1 + 1;
  			regular_box_bound->upper_bound[1] = j + rst_id->regular_box_dim[1] - 1 + 1;
  			regular_box_bound->upper_bound[2] = k + rst_id->regular_box_dim[2] - 1 + 1;
  			regular_box_bound->upper_bound[3] = l + rst_id->regular_box_dim[3] - 1 + 1;
  			regular_box_bound->upper_bound[4] = m + rst_id->regular_box_dim[4] - 1 + 1;

			//Edge regular boxes
			if ((i + rst_id->regular_box_dim[0]) >= rst_id->global_extents[0])
			    regular_box_bound->upper_bound[0] = rst_id->global_extents[0] - 1;
			if ((j + rst_id->regular_box_dim[1]) >= rst_id->global_extents[1])
			    regular_box_bound->upper_bound[1] = rst_id->global_extents[1] - 1;
			if ((k + rst_id->regular_box_dim[2]) >= rst_id->global_extents[2])
			    regular_box_bound->upper_bound[2] = rst_id->global_extents[2] - 1;
			if ((l + rst_id->regular_box_dim[3]) >= rst_id->global_extents[3])
			    regular_box_bound->upper_bound[3] = rst_id->global_extents[3] - 1;
			if ((m + rst_id->regular_box_dim[4]) >= rst_id->global_extents[4])
			    regular_box_bound->upper_bound[4] = rst_id->global_extents[4] - 1;


                        //STEP 4: If local process intersects with regular box, then find all other process that intersects with the regular box.
                        if (intersectNDChunk(regular_box_bound, local_proc_bound)) {
#ifdef DEBUG_RST 
                            if (rank == 63)
                                printf("BOUNDS [%d %d %d %d %d] : [%d %d %d %d %d]\n", regular_box_bound->lower_bound[0], regular_box_bound->lower_bound[1], regular_box_bound->lower_bound[2],
                                    regular_box_bound->lower_bound[3], regular_box_bound->lower_bound[4], regular_box_bound->upper_bound[0], regular_box_bound->upper_bound[1],
                                    regular_box_bound->upper_bound[2], regular_box_bound->upper_bound[3], regular_box_bound->upper_bound[4]);
#endif

                            pseudo_pcount = 0;
                            //Iterate through all processes
                            for (r = 0; r < nprocs; r++) {
                                //Extent of process with rank r
                                NDim_chunk* rank_r_bound = (NDim_chunk*) malloc(sizeof (NDim_chunk));
                                if (!rank_r_bound) PIDX_rst_print_error("Memory : rank_r_bound", __FILE__, __LINE__);
                                memset(rank_r_bound, 0, sizeof (*rank_r_bound));

                                rank_r_bound->dims = rst_id->dims;
                                for (d = 0; d < PIDX_MAX_DIMENSIONS; d++) {
                                    rank_r_bound->lower_bound[d] = rank_r_offset[PIDX_MAX_DIMENSIONS * r + d];
                                    rank_r_bound->upper_bound[d] = rank_r_offset[PIDX_MAX_DIMENSIONS * r + d] + rank_r_count[PIDX_MAX_DIMENSIONS * r + d] - 1;
                                }

                                //If process with rank r intersects with the regular box, then calculate the offset, count and volume of the intersecting volume
                                if (intersectNDChunk(regular_box_bound, rank_r_bound)) {
#ifdef DEBUG_RST
                                    if (regular_box_bound->upper_bound[0] == 99 && regular_box_bound->upper_bound[1] == 99 && regular_box_bound->upper_bound[2] == 99)
                                        printf("target case : [%d %d %d %d %d] :: [%d %d %d %d %d]\n", rank_r_bound->lower_bound[0], rank_r_bound->lower_bound[1], rank_r_bound->lower_bound[2],
                                            rank_r_bound->lower_bound[3], rank_r_bound->lower_bound[4], rank_r_bound->upper_bound[0], rank_r_bound->upper_bound[1], rank_r_bound->upper_bound[2],
                                            rank_r_bound->upper_bound[3], rank_r_bound->upper_bound[4]);
#endif

                                    for (d = 0; d < PIDX_MAX_DIMENSIONS; d++) {
                                        //STEP 5 : offset and count of intersecting chunk of process with rank r and regular box
                                        if (rank_r_bound->lower_bound[d] <= regular_box_bound->lower_bound[d] && rank_r_bound->upper_bound[d] <= regular_box_bound->upper_bound[d]) {
                                            rst_id->nbor_proc_ubound[pcount][d] = rank_r_bound->upper_bound[d] - regular_box_bound->lower_bound[d] + 1;
                                            rst_id->nbor_proc_lbound[pcount][d] = regular_box_bound->lower_bound[d];
                                        } else if (regular_box_bound->lower_bound[d] <= rank_r_bound->lower_bound[d] && rank_r_bound->upper_bound[d] >= regular_box_bound->upper_bound[d]) {
                                            rst_id->nbor_proc_ubound[pcount][d] = regular_box_bound->upper_bound[d] - rank_r_bound->lower_bound[d] + 1;
                                            rst_id->nbor_proc_lbound[pcount][d] = rank_r_bound->lower_bound[d];
                                        } else if (regular_box_bound->upper_bound[d] <= rank_r_bound->upper_bound[d] && regular_box_bound->lower_bound[d] >= rank_r_bound->lower_bound[d]) {
                                            rst_id->nbor_proc_ubound[pcount][d] = regular_box_bound->upper_bound[d] - regular_box_bound->lower_bound[d] + 1;
                                            rst_id->nbor_proc_lbound[pcount][d] = regular_box_bound->lower_bound[d];
                                        } else if (rank_r_bound->upper_bound[d] <= regular_box_bound->upper_bound[d] && rank_r_bound->lower_bound[d] >= regular_box_bound->lower_bound[d]) {
                                            rst_id->nbor_proc_ubound[pcount][d] = rank_r_bound->upper_bound[d] - rank_r_bound->lower_bound[d] + 1;
                                            rst_id->nbor_proc_lbound[pcount][d] = rank_r_bound->lower_bound[d];
                                        }

                                        //offset and count of intersecting regular box
                                        rst_id->regular_nbor_proc_lbound[pcount][d] = regular_box_bound->lower_bound[d];
                                        rst_id->regular_nbor_proc_ubound[pcount][d] = regular_box_bound->upper_bound[d] - regular_box_bound->lower_bound[d] + 1;
                                    }


                                    //Volume of intersecting chunk of process with rank r and the regular box
                                    rst_id->volume[pcount] = 1;
                                    for (d = 0; d < rst_id->dims; d++)
                                        rst_id->volume[pcount] = rst_id->volume[pcount] * rst_id->nbor_proc_ubound[pcount][d];

                                    total_volume = rst_id->volume[pcount] + total_volume;
                                    rst_id->nbor_proc_rank[pcount] = r;
                                    pcount++;
                                    pseudo_pcount++;
                                    rst_id->nbor_proc_count++;

                                    //case with more than 1024 neighboring process
                                    if (pcount >= PIDX_MAX_NEIGHBOR_PROC || pseudo_pcount >= PIDX_MAX_NEIGHBOR_PROC || rst_id->nbor_proc_count >= PIDX_MAX_NEIGHBOR_PROC)
                                        PIDX_rst_print_error("Logical Error : case with more than 1024 neighboring process", __FILE__, __LINE__);
                                }
                                free(rank_r_bound);
                            }

                            int max_rank = rst_id->nbor_proc_rank[starting_index];
                            int max_vol = rst_id->volume[starting_index];


                            //For each regular box (index controlled by starting_index, pseudo_pcount), find the process that has the maximum contribution
                            //pseudo_pcount: is the total number of processes intersecting the regular box
                            //starting_index: it is the index where the intersecting process count list starts for every regular box
                            //the process with maximum contribution will the the reciever and the rest will be the sender
                            for (t = starting_index; t < starting_index + pseudo_pcount; t++) {
                                if (rst_id->volume[t] > max_vol) {
                                    max_rank = rst_id->nbor_proc_rank[t];
                                    max_vol = rst_id->volume[t];
                                }
                            }
                            //assign to each neighboring process if it is the sender (recieve_send = 0) or the reciever (recieve_send = 1)
                            //and also assign the rank (to_send_rank) of the reviever 
                            for (t = starting_index; t < starting_index + pseudo_pcount; t++) {
                                if (rst_id->nbor_proc_rank[t] == max_rank)
                                    rst_id->recieve_send[t] = 1;
                                else
                                    rst_id->recieve_send[t] = 0;

                                rst_id->to_send_rank[t] = max_rank;

#if 0
                                if (regular_box_bound->upper_bound[0] == 99 && regular_box_bound->upper_bound[1] == 99 && regular_box_bound->upper_bound[2] == 99)
                                    printf("[COUNT %d] : [%d %d %d %d %d] [%d %d %d %d %d] : voolume %d :: max rank %d max volume %d starting index %d Pseudo COunt %d neighbor rank %d send rank %d\n", t, rst_id->nbor_proc_lbound[t][0], rst_id->nbor_proc_lbound[t][1],
                                        rst_id->nbor_proc_lbound[t][2], rst_id->nbor_proc_lbound[t][3], rst_id->nbor_proc_lbound[t][4],
                                        rst_id->nbor_proc_ubound[t][0], rst_id->nbor_proc_ubound[t][1], rst_id->nbor_proc_ubound[t][2],
                                        rst_id->nbor_proc_ubound[t][3], rst_id->nbor_proc_ubound[t][4], rst_id->volume[t], max_rank, max_vol, starting_index, pseudo_pcount,
                                        rst_id->nbor_proc_rank[t], rst_id->to_send_rank[t]);
#endif

                                //store the starting_index and pseudo_pcount for later reuse during the actual restructuring
                                rst_id->block_count[t] = pseudo_pcount;
                                rst_id->block_offset[t] = starting_index;
                            }

                            //If a process is the reciever then update the buffer count
                            //the buffer count is increased by the total number of processes intersecting with the regular box (pseudo_pcount) 
                            if (max_rank == rank) {
                                *num_output_buffers = *num_output_buffers + pseudo_pcount;
                            }

                            //updating the starting index for the next regular box
                            starting_index = starting_index + pseudo_pcount;
                        }
                        free(regular_box_bound);
                    }

    free(local_proc_bound);
    free(rank_r_offset);
    free(rank_r_count);

    return (rst_id);
}

int PIDX_rst_buf_init(PIDX_Ndim_buffer* in_buf, int dims, int* lb, int* ub, dataType* buffer, int spv, MPI_Datatype datatype, char* var_name, int index) {
    int d = 0;

    in_buf->dims = dims;
    in_buf->datatype = datatype;
    for (d = 0; d < PIDX_MAX_DIMENSIONS; d++)
        in_buf->lower_bounds[d] = lb[d];
    for (d = 0; d < PIDX_MAX_DIMENSIONS; d++)
        in_buf->upper_bounds[d] = ub[d];

    in_buf->buffer = (dataType*) buffer;
    in_buf->create_flag = 0;
    in_buf->sample_per_variable = spv;
    in_buf->var_name = strdup(var_name);
    in_buf->variable_index = index;

    return 0;
}

void verify_before(PIDX_rst_id rst_id, PIDX_Ndim_buffer* in_buf, int num_output_buffers) {
    int a1, b1, k1, j1, i1, t, x, rank;
    long long send_c = 0, temp_count_r = 0, temp_count_s = 0;
    long long data_rec = 0, data_mem = 0;
    long long all_data_recv = 0, all_data_mem = 0;

#ifdef MPI
    MPI_Comm_rank(rst_id->comm, &rank);
#else
    rank = 0;
#endif

    for (t = 0; t < rst_id->nbor_proc_count; t++) {
        if (rank == rst_id->to_send_rank[t]) {
            for (x = rst_id->block_offset[t]; x < rst_id->block_offset[t] + rst_id->block_count[t]; x++) {
                if (rst_id->nbor_proc_rank[x] == rank) {
                    for (a1 = rst_id->nbor_proc_lbound[x][4]; a1 < rst_id->nbor_proc_lbound[x][4] + rst_id->nbor_proc_ubound[x][4]; a1++)
                        for (b1 = rst_id->nbor_proc_lbound[x][3]; b1 < rst_id->nbor_proc_lbound[x][3] + rst_id->nbor_proc_ubound[x][3]; b1++)
                            for (k1 = rst_id->nbor_proc_lbound[x][2]; k1 < rst_id->nbor_proc_lbound[x][2] + rst_id->nbor_proc_ubound[x][2]; k1++)
                                for (j1 = rst_id->nbor_proc_lbound[x][1]; j1 < rst_id->nbor_proc_lbound[x][1] + rst_id->nbor_proc_ubound[x][1]; j1++)
                                    for (i1 = rst_id->nbor_proc_lbound[x][0]; i1 < rst_id->nbor_proc_lbound[x][0] + rst_id->nbor_proc_ubound[x][0]; i1 = i1 + rst_id->nbor_proc_ubound[x][0]) {
                                        send_c = rst_id->nbor_proc_ubound[x][0] * in_buf->sample_per_variable;
                                        temp_count_r = temp_count_r + send_c;
                                        data_mem = data_mem + send_c;
                                        temp_count_s = temp_count_s + send_c;
                                    }
                } else {
                    temp_count_r = temp_count_r + rst_id->volume[x] * in_buf->sample_per_variable;
                    data_rec = data_rec + rst_id->volume[x] * in_buf->sample_per_variable;
                }
            }
            t = rst_id->block_offset[t] + rst_id->block_count[t];
        }
    }

#ifdef MPI
    MPI_Allreduce(&data_mem, &all_data_mem, 1, MPI_LONG_LONG, MPI_SUM, MPI_COMM_WORLD);
    MPI_Allreduce(&data_rec, &all_data_recv, 1, MPI_LONG_LONG, MPI_SUM, MPI_COMM_WORLD);
#else
    all_data_recv = data_rec;
    all_data_mem = data_mem;
#endif

    if ((long long) rst_id->global_extents[0] * rst_id->global_extents[1] * rst_id->global_extents[2] * in_buf->sample_per_variable != all_data_mem + all_data_recv) {
        if (rank == 0)
            fprintf(stderr, "[MEMORY MODELLING] : Volume Copied on network %lld Volume Copied from Memory %lld and GV %lld [%d %d %d : %d] \n", all_data_recv, all_data_mem, (long long) rst_id->global_extents[0] * rst_id->global_extents[1] * rst_id->global_extents[2] * in_buf->sample_per_variable, rst_id->global_extents[0], rst_id->global_extents[1], rst_id->global_extents[2], in_buf->sample_per_variable);
        PIDX_rst_print_error("[VERIFY] : Logical Error : Restructuring Failed", __FILE__, __LINE__);
    }

    long long global_volume_r = 0;
#ifdef MPI
    MPI_Allreduce(&temp_count_r, &global_volume_r, 1, MPI_LONG_LONG, MPI_SUM, MPI_COMM_WORLD);
#else
    global_volume_r = temp_count_r;
#endif

    if (global_volume_r != (long long) rst_id->global_extents[0] * rst_id->global_extents[1] * rst_id->global_extents[2] * in_buf->sample_per_variable) {
        fprintf(stderr, "[%d] Volume Error %lld %lld\n", rank, global_volume_r, (long long) rst_id->global_extents[0] * rst_id->global_extents[1] * rst_id->global_extents[2] * in_buf->sample_per_variable);
        PIDX_rst_print_error("[VERIFY] : Logical Error : Restructuring Failed", __FILE__, __LINE__);
    }
    assert(global_volume_r == (long long) rst_id->global_extents[0] * rst_id->global_extents[1] * rst_id->global_extents[2] * in_buf->sample_per_variable);

    long long tot_cnt = 0;
    for (t = 0; t < rst_id->nbor_proc_count; t++) {
        if (rst_id->recieve_send[t] == 0) {
            if (rst_id->nbor_proc_rank[t] == rank) {
                for (a1 = rst_id->nbor_proc_lbound[t][4]; a1 < rst_id->nbor_proc_lbound[t][4] + rst_id->nbor_proc_ubound[t][4]; a1++)
                    for (b1 = rst_id->nbor_proc_lbound[t][3]; b1 < rst_id->nbor_proc_lbound[t][3] + rst_id->nbor_proc_ubound[t][3]; b1++)
                        for (k1 = rst_id->nbor_proc_lbound[t][2]; k1 < rst_id->nbor_proc_lbound[t][2] + rst_id->nbor_proc_ubound[t][2]; k1++)
                            for (j1 = rst_id->nbor_proc_lbound[t][1]; j1 < rst_id->nbor_proc_lbound[t][1] + rst_id->nbor_proc_ubound[t][1]; j1++)
                                for (i1 = rst_id->nbor_proc_lbound[t][0]; i1 < rst_id->nbor_proc_lbound[t][0] + rst_id->nbor_proc_ubound[t][0]; i1 = i1 + rst_id->nbor_proc_ubound[t][0])
                                    tot_cnt = tot_cnt + rst_id->nbor_proc_ubound[t][0] * in_buf->sample_per_variable;
            }
        }
    }
    temp_count_s = temp_count_s + tot_cnt;
    long long global_volume_s = 0;

#ifdef MPI
    MPI_Allreduce(&temp_count_s, &global_volume_s, 1, MPI_LONG_LONG, MPI_SUM, MPI_COMM_WORLD);
#else
    global_volume_s = temp_count_s;
#endif

    if (global_volume_s != (long long) rst_id->global_extents[0] * rst_id->global_extents[1] * rst_id->global_extents[2] * in_buf->sample_per_variable) {
        fprintf(stderr, "[%d] Volume Error %lld %lld\n", rank, global_volume_s, (long long) rst_id->global_extents[0] * rst_id->global_extents[1] * rst_id->global_extents[2] * in_buf->sample_per_variable);
        if (rank == 0) {
            fprintf(stderr, "[SEND VERIFY] : Element Count!!!!! [%lld] : [%lld]\n", global_volume_r, (long long) rst_id->global_extents[0] * rst_id->global_extents[1] * rst_id->global_extents[2] * in_buf->sample_per_variable);
            fprintf(stderr, "[RECV VERIFY] : Passed 1st Phase of Element Count!!!!! [%lld] : [%lld]\n", global_volume_r, (long long) rst_id->global_extents[0] * rst_id->global_extents[1] * rst_id->global_extents[2] * in_buf->sample_per_variable);
        }
        PIDX_rst_print_error("[VERIFY] : Logical Error : Restructuring Failed", __FILE__, __LINE__);
    }
}

/* actually do the restructuring, using pre-calculated data associated with the rst_id */
int PIDX_rst_restructure(PIDX_rst_id rst_id, PIDX_Ndim_buffer* in_buf, PIDX_Ndim_buffer** out_buf_array, int num_output_buffers) {
    //verify_before(rst_id, in_buf, num_output_buffers);

    int x = 0, t = 0, d = 0, j = 0, ret = 0;
    int rank, nprocs, counter = 0, bytes_per_sample;
    MPI_Datatype var_datatype;
    bytes_per_sample = sizeof(dataType);

    //rank and nprocs
#ifdef MPI
    ret = MPI_Comm_rank(rst_id->comm, &rank);
    if (ret != MPI_SUCCESS) PIDX_rst_print_error("Rank ", __FILE__, __LINE__);

    MPI_Comm_size(rst_id->comm, &nprocs);
    if (ret != MPI_SUCCESS) PIDX_rst_print_error("nprocs ", __FILE__, __LINE__);
#else
    rank = 0;
    nprocs = 1;
#endif

    //Creating the output Ndim buffers
    for (j = 0; j < num_output_buffers; j++) {
        out_buf_array[j] = (PIDX_Ndim_buffer*) malloc(sizeof (PIDX_Ndim_buffer));
        if (!out_buf_array[j]) PIDX_rst_print_error("Memory Error : out_buf_array ", __FILE__, __LINE__);
        memset(out_buf_array[j], 0, sizeof (sizeof (PIDX_Ndim_buffer)));
    }

    
    //assigning the sample per variable to all the output Ndim chunks 
    for (t = 0; t < num_output_buffers; t++) {
        out_buf_array[t]->sample_per_variable = in_buf->sample_per_variable;
        out_buf_array[t]->var_name = strdup(in_buf->var_name);
        out_buf_array[t]->variable_index = in_buf->variable_index;
    }

    //Bytes per sample for this datatype
    var_datatype = in_buf->datatype;
    MPI_Type_size(var_datatype, &bytes_per_sample);

    
    for (t = 0; t < rst_id->nbor_proc_count; t++) {
        if (rank == rst_id->to_send_rank[t]) {
            for (x = rst_id->block_offset[t]; x < rst_id->block_offset[t] + rst_id->block_count[t]; x++) {
                assert(counter < num_output_buffers);
		
		if(mem_create_required == 1)
		{
		    out_buf_array[counter]->buffer = (dataType*)malloc(bytes_per_sample * rst_id->volume[x] * in_buf->sample_per_variable);
		    if (!out_buf_array[counter]->buffer) PIDX_rst_print_error("Memory Error : out_buf_array ", __FILE__, __LINE__);
		    memset(out_buf_array[counter]->buffer, 0, bytes_per_sample * rst_id->volume[x] * in_buf->sample_per_variable);
		    out_buf_array[counter]->create_flag = 1;
		}
		else
		{
		  out_buf_array[counter]->create_flag = 0;
		  out_buf_array[counter]->buffer= in_buf->buffer;
		  if (rank == 0)
		      printf("Did not create Memory!!!!!!!\n");
		}
		

                for (d = 0; d < PIDX_MAX_DIMENSIONS; d++) {
                    out_buf_array[counter]->lower_bounds[d] = rst_id->nbor_proc_lbound[x][d];
                    out_buf_array[counter]->upper_bounds[d] = rst_id->nbor_proc_lbound[x][d] + rst_id->nbor_proc_ubound[x][d];
                    out_buf_array[counter]->regular_lower_bounds[d] = rst_id->regular_nbor_proc_lbound[x][d];
                    out_buf_array[counter]->regular_upper_bounds[d] = rst_id->regular_nbor_proc_lbound[x][d] + rst_id->regular_nbor_proc_ubound[x][d];
                }
                

#ifdef DEBUG_RST
                if (rank == 63)
                    printf("[%d] [%d] [%d] INPUT : [%d %d %d %d %d] :: [%d %d %d %d %d]\n", rst_id->to_send_rank[t], counter, rst_id->nbor_proc_count, out_buf_array[counter]->lower_bounds[0], out_buf_array[counter]->lower_bounds[1],
                        out_buf_array[counter]->lower_bounds[2], out_buf_array[counter]->lower_bounds[3], out_buf_array[counter]->lower_bounds[4],
                        out_buf_array[counter]->upper_bounds[0], out_buf_array[counter]->upper_bounds[1], out_buf_array[counter]->upper_bounds[2],
                        out_buf_array[counter]->upper_bounds[3], out_buf_array[counter]->upper_bounds[4]);
#endif
                counter++;
            }
            t = rst_id->block_offset[t] + rst_id->block_count[t] - 1;
        }
    }
    
    //assert(num_output_buffers == counter);
    return 0;
}

int PIDX_rst_restructure_IO(PIDX_rst_id rst_id, PIDX_Ndim_buffer* in_buf, PIDX_Ndim_buffer** out_buf_array, int num_output_buffers, int WRITE) {
    int a1 = 0, b1 = 0, x = 0, k1 = 0, t = 0, i1 = 0, j1 = 0, index, count1 = 0, ret = 0;
    int *send_count, *send_offset;
    int rank, nprocs, send_c = 0, send_o = 0, counter = 0, req_counter = 0, bytes_per_sample;
#ifdef MPI
    MPI_Request *req;
    MPI_Status *status;
#endif

    bytes_per_sample = sizeof(dataType);
    //rank and nprocs
#ifdef MPI
    ret = MPI_Comm_rank(rst_id->comm, &rank);
    if (ret != MPI_SUCCESS) PIDX_rst_print_error("Rank", __FILE__, __LINE__);

    MPI_Comm_size(rst_id->comm, &nprocs);
    if (ret != MPI_SUCCESS) PIDX_rst_print_error("nprocs", __FILE__, __LINE__);
#else
    rank = 0;
    nprocs = 1;
#endif


    //creating ample requests and statuses
#ifdef MPI
    req = (MPI_Request*) malloc(sizeof (*req) * rst_id->nbor_proc_count * 2);
    if (!req) PIDX_rst_print_error("Memory Error : req", __FILE__, __LINE__);

    status = (MPI_Status*) malloc(sizeof (*status) * rst_id->nbor_proc_count * 2);
    if (!status) PIDX_rst_print_error("Memory Error : status", __FILE__, __LINE__);
#endif

    for (t = 0; t < rst_id->nbor_proc_count; t++) {
        if (rank == rst_id->to_send_rank[t]) {
            for (x = rst_id->block_offset[t]; x < rst_id->block_offset[t] + rst_id->block_count[t]; x++) {
                assert(counter < num_output_buffers);
                if (rst_id->nbor_proc_rank[x] == rank) {
                    count1 = 0;
		    if(mem_create_required == 1)
		    {
			for (a1 = rst_id->nbor_proc_lbound[x][4]; a1 < rst_id->nbor_proc_lbound[x][4] + rst_id->nbor_proc_ubound[x][4]; a1++)
			    for (b1 = rst_id->nbor_proc_lbound[x][3]; b1 < rst_id->nbor_proc_lbound[x][3] + rst_id->nbor_proc_ubound[x][3]; b1++)
				for (k1 = rst_id->nbor_proc_lbound[x][2]; k1 < rst_id->nbor_proc_lbound[x][2] + rst_id->nbor_proc_ubound[x][2]; k1++)
				    for (j1 = rst_id->nbor_proc_lbound[x][1]; j1 < rst_id->nbor_proc_lbound[x][1] + rst_id->nbor_proc_ubound[x][1]; j1++)
					for (i1 = rst_id->nbor_proc_lbound[x][0]; i1 < rst_id->nbor_proc_lbound[x][0] + rst_id->nbor_proc_ubound[x][0]; i1 = i1 + rst_id->nbor_proc_ubound[x][0]) {
					    index =
						    (in_buf->upper_bounds[0] * in_buf->upper_bounds[1] * in_buf->upper_bounds[2] * in_buf->upper_bounds[3] * (a1 - in_buf->lower_bounds[4])) +
						    (in_buf->upper_bounds[0] * in_buf->upper_bounds[1] * in_buf->upper_bounds[2] * (b1 - in_buf->lower_bounds[3])) +
						    (in_buf->upper_bounds[0] * in_buf->upper_bounds[1] * (k1 - in_buf->lower_bounds[2])) +
						    (in_buf->upper_bounds[0] * (j1 - in_buf->lower_bounds[1])) +
						    (i1 - in_buf->lower_bounds[0]);

					    send_o = index * in_buf->sample_per_variable;
					    send_c = rst_id->nbor_proc_ubound[x][0] * in_buf->sample_per_variable;
					    if (WRITE == 1)
						memcpy(out_buf_array[counter]->buffer + (count1 * send_c), in_buf->buffer + send_o, send_c * bytes_per_sample);
					    else
					      	memcpy(in_buf->buffer + send_o, out_buf_array[counter]->buffer + (count1 * send_c), send_c * bytes_per_sample);

					    count1++;
					}
		    }
		    else
		    {
		      if (WRITE == 1)
			  out_buf_array[counter]->buffer= in_buf->buffer;
		      else{
			  in_buf->buffer = out_buf_array[counter]->buffer;
		      }
		    }
		      
                                    
                }
#ifdef MPI
                else {
                    if (WRITE == 1) {
                        ret = MPI_Irecv(out_buf_array[counter]->buffer, rst_id->volume[x] * in_buf->sample_per_variable, in_buf->datatype, rst_id->nbor_proc_rank[x], 123, rst_id->comm, &req[req_counter]);
                        if (ret != MPI_SUCCESS) PIDX_rst_print_error("MPI_Irecv", __FILE__, __LINE__);
                    } else {
                        ret = MPI_Isend(out_buf_array[counter]->buffer, rst_id->volume[x] * in_buf->sample_per_variable, in_buf->datatype, rst_id->nbor_proc_rank[x], 123, rst_id->comm, &req[req_counter]);
                        if (ret != MPI_SUCCESS) PIDX_rst_print_error("MPI_Isend", __FILE__, __LINE__);
                    }
                    req_counter++;
                }
#endif
                counter++;
            }
            t = rst_id->block_offset[t] + rst_id->block_count[t] - 1;
        }
    }

#ifdef MPI
    for (t = 0; t < rst_id->nbor_proc_count; t++) {
        if (rst_id->recieve_send[t] == 0) {
            if (rst_id->nbor_proc_rank[t] == rank) {
                send_offset = (int*) malloc(sizeof (int) * rst_id->nbor_proc_ubound[t][4] * rst_id->nbor_proc_ubound[t][3] * rst_id->nbor_proc_ubound[t][2] * rst_id->nbor_proc_ubound[t][1]);
                if (!send_offset) PIDX_rst_print_error("Memory Error : send_offset", __FILE__, __LINE__);
                memset(send_offset, 0, sizeof (int) * rst_id->nbor_proc_ubound[t][4] * rst_id->nbor_proc_ubound[t][3] * rst_id->nbor_proc_ubound[t][2] * rst_id->nbor_proc_ubound[t][1]);

                send_count = (int*) malloc(sizeof (int) * rst_id->nbor_proc_ubound[t][4] * rst_id->nbor_proc_ubound[t][3] * rst_id->nbor_proc_ubound[t][2] * rst_id->nbor_proc_ubound[t][1]);
                if (!send_count) PIDX_rst_print_error("Memory Error : send_count", __FILE__, __LINE__);
                memset(send_count, 0, sizeof (int) * rst_id->nbor_proc_ubound[t][4] * rst_id->nbor_proc_ubound[t][3] * rst_id->nbor_proc_ubound[t][2] * rst_id->nbor_proc_ubound[t][1]);

                count1 = 0;
                int tot_cnt = 0;
                for (a1 = rst_id->nbor_proc_lbound[t][4]; a1 < rst_id->nbor_proc_lbound[t][4] + rst_id->nbor_proc_ubound[t][4]; a1++)
                    for (b1 = rst_id->nbor_proc_lbound[t][3]; b1 < rst_id->nbor_proc_lbound[t][3] + rst_id->nbor_proc_ubound[t][3]; b1++)
                        for (k1 = rst_id->nbor_proc_lbound[t][2]; k1 < rst_id->nbor_proc_lbound[t][2] + rst_id->nbor_proc_ubound[t][2]; k1++)
                            for (j1 = rst_id->nbor_proc_lbound[t][1]; j1 < rst_id->nbor_proc_lbound[t][1] + rst_id->nbor_proc_ubound[t][1]; j1++)
                                for (i1 = rst_id->nbor_proc_lbound[t][0]; i1 < rst_id->nbor_proc_lbound[t][0] + rst_id->nbor_proc_ubound[t][0]; i1 = i1 + rst_id->nbor_proc_ubound[t][0]) {
                                    index = (in_buf->upper_bounds[0] * in_buf->upper_bounds[1] * in_buf->upper_bounds[2] * in_buf->upper_bounds[3] * (a1 - in_buf->lower_bounds[4])) +
                                            (in_buf->upper_bounds[0] * in_buf->upper_bounds[1] * in_buf->upper_bounds[2] * (b1 - in_buf->lower_bounds[3])) +
                                            (in_buf->upper_bounds[0] * in_buf->upper_bounds[1] * (k1 - in_buf->lower_bounds[2])) +
                                            (in_buf->upper_bounds[0] * (j1 - in_buf->lower_bounds[1])) +
                                            (i1 - in_buf->lower_bounds[0]);
                                    send_offset[count1] = index * in_buf->sample_per_variable;
                                    send_count[count1] = rst_id->nbor_proc_ubound[t][0] * in_buf->sample_per_variable;
                                    tot_cnt = tot_cnt + send_count[count1];
                                    count1++;
                                }

                MPI_Datatype chunk_data_type;
		if((int)sizeof(dataType) == (int)sizeof(double))
		    MPI_Type_indexed(count1, send_count, send_offset, MPI_DOUBLE, &chunk_data_type);
		if((int)sizeof(dataType) == (int)sizeof(float))
		    MPI_Type_indexed(count1, send_count, send_offset, MPI_FLOAT, &chunk_data_type);
                MPI_Type_commit(&chunk_data_type);

                if (rst_id->to_send_rank[t] != rank) {
                    if (WRITE == 1) {
                        ret = MPI_Isend(in_buf->buffer, 1, chunk_data_type, rst_id->to_send_rank[t], 123, rst_id->comm, &req[req_counter]);
                        if (ret != MPI_SUCCESS) PIDX_rst_print_error("MPI_Isend", __FILE__, __LINE__);
                    } else {
                        ret = MPI_Irecv(in_buf->buffer, 1, chunk_data_type, rst_id->to_send_rank[t], 123, rst_id->comm, &req[req_counter]);
                        if (ret != MPI_SUCCESS) PIDX_rst_print_error("MPI_Irecv", __FILE__, __LINE__);
                    }
                    req_counter++;
                }
                MPI_Type_free(&chunk_data_type);

                free(send_offset);
                free(send_count);
            }
        }
    }
    ret = MPI_Waitall(req_counter, req, status);
    if (ret != MPI_SUCCESS) PIDX_rst_print_error("MPI_Waitall", __FILE__, __LINE__);

    free(req);
    req = 0;
    free(status);
    status = 0;
#endif
    return ret;
}

/* tear down the various buffer structs. In the case of the output structs this function should also free the memory buffers as well */
int PIDX_rst_buf_destroy(PIDX_Ndim_buffer* in_buf) {
    if (in_buf->create_flag == 1) {
        free(in_buf->buffer);
        in_buf->buffer = 0;

        in_buf->buffer_size = 0;

        free(in_buf);
        in_buf = 0;
    }

    return 0;
}

/* tear down whatever was calculated for this particular combination of dimensions and bounds */
int PIDX_rst_finalize(PIDX_rst_id id/*, int r, int ts_count, int ts, int var*/) {
    free(id);
    id = 0;

    return 0;
}

int HELPER_rst(PIDX_Ndim_buffer** out_buf_array1, PIDX_rst_id rst_id, int num_output_buffers, int spv) {
    int i, j, k, rank = 0, v = 0, u = 0, d = 0, s = 0;
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    long long element_count = 0;
    long long lost_element_count = 0;
    long long per_process_exact = 0;
    dataType **temp_buffer;
    
    temp_buffer = (dataType**) malloc(sizeof (*temp_buffer) * num_output_buffers);
    for (d = 0; d < num_output_buffers; d++) {
        temp_buffer[d] = (dataType*) out_buf_array1[d]->buffer;

        for (v = 0; v < (out_buf_array1[d]->upper_bounds[4] - out_buf_array1[d]->lower_bounds[4]); v++) {
            for (u = 0; u < (out_buf_array1[d]->upper_bounds[3] - out_buf_array1[d]->lower_bounds[3]); u++) {
                for (k = 0; k < (out_buf_array1[d]->upper_bounds[2] - out_buf_array1[d]->lower_bounds[2]); k++) {
                    for (j = 0; j < (out_buf_array1[d]->upper_bounds[1] - out_buf_array1[d]->lower_bounds[1]); j++) {
                        for (i = 0; i < (out_buf_array1[d]->upper_bounds[0] - out_buf_array1[d]->lower_bounds[0]); i++) {
                            int index = ((out_buf_array1[d]->upper_bounds[0] - out_buf_array1[d]->lower_bounds[0]) * (out_buf_array1[d]->upper_bounds[1] - out_buf_array1[d]->lower_bounds[1]) * (out_buf_array1[d]->upper_bounds[2] - out_buf_array1[d]->lower_bounds[2]) * (out_buf_array1[d]->upper_bounds[3] - out_buf_array1[d]->lower_bounds[3]) * v) +
                                    ((out_buf_array1[d]->upper_bounds[0] - out_buf_array1[d]->lower_bounds[0]) * (out_buf_array1[d]->upper_bounds[1] - out_buf_array1[d]->lower_bounds[1]) * (out_buf_array1[d]->upper_bounds[2] - out_buf_array1[d]->lower_bounds[2]) * u) +
                                    ((out_buf_array1[d]->upper_bounds[0] - out_buf_array1[d]->lower_bounds[0]) * (out_buf_array1[d]->upper_bounds[1] - out_buf_array1[d]->lower_bounds[1]) * k) +
                                    ((out_buf_array1[d]->upper_bounds[0] - out_buf_array1[d]->lower_bounds[0]) * j) +
                                    i;
                            int check_bit = 1;
                            for (s = 0; s < spv; s++)
                                check_bit = check_bit && ((int) temp_buffer[d][spv * index + s] == s + 100 + (rst_id->global_extents[0] * rst_id->global_extents[1] * rst_id->global_extents[2] * rst_id->global_extents[3]*(out_buf_array1[d]->lower_bounds[4] + v)) + (rst_id->global_extents[0] * rst_id->global_extents[1] * rst_id->global_extents[2]*(out_buf_array1[d]->lower_bounds[3] + u)) + (rst_id->global_extents[0] * rst_id->global_extents[1]*(out_buf_array1[d]->lower_bounds[2] + k))+(rst_id->global_extents[0] * (out_buf_array1[d]->lower_bounds[1] + j)) + out_buf_array1[d]->lower_bounds[0] + i);

                            if (check_bit == 0) {
                                lost_element_count++;
                                // 					    printf("LOST Element : %d\n", (int)out_buf_array1[d]->buffer[spv * index + s]);
                            } else {
                                //printf("Element : %d\n", (int)out_buf_array1[d]->buffer[spv * index + s]);
                                element_count++;
                            }
                        }
                    }
                }
            }
        }
    }
    //printf("RANK [%d] : Count %lld\n", rank, el_count);
    //element_count = element_count + lost_element_count;
    if (num_output_buffers != 0) {
        per_process_exact = (long long) (out_buf_array1[0]->regular_upper_bounds[0] - out_buf_array1[0]->regular_lower_bounds[0]) * (out_buf_array1[0]->regular_upper_bounds[1] - out_buf_array1[0]->regular_lower_bounds[1]) * (out_buf_array1[0]->regular_upper_bounds[2] - out_buf_array1[0]->regular_lower_bounds[2]) * (out_buf_array1[0]->regular_upper_bounds[3] - out_buf_array1[0]->regular_lower_bounds[3]) * (out_buf_array1[0]->regular_upper_bounds[4] - out_buf_array1[0]->regular_lower_bounds[4]);
        printf("[%d] EXACT %lld EXPECTED %lld ERROR %lld \n", rank, element_count, per_process_exact, (per_process_exact - element_count));
        //printf("[%d] Block Count [%d] Buffer Count [%d] : Element Count = %d\n\n", rank, num_regular_block, num_output_buffers, element_count);
    }

    long long global_volume;

    MPI_Allreduce(&element_count, &global_volume, 1, MPI_LONG_LONG, MPI_SUM, MPI_COMM_WORLD);
    if (global_volume != (long long) rst_id->global_extents[0] * rst_id->global_extents[1] * rst_id->global_extents[2]) {
        fprintf(stderr, "[%d] RST Volume Error %lld %lld\n", rank, global_volume, (long long) rst_id->global_extents[0] * rst_id->global_extents[1] * rst_id->global_extents[2]);
        MPI_Abort(MPI_COMM_WORLD, -1);
    }
    if (rank == 0)
        if (global_volume == (long long) rst_id->global_extents[0] * rst_id->global_extents[1] * rst_id->global_extents[2])
            printf("[%d] RST Volume %lld %lld\n", rank, global_volume, (long long) rst_id->global_extents[0] * rst_id->global_extents[1] * rst_id->global_extents[2]);
    return 1;
}

void PIDX_rst_print_error(char *error_message, char* file, int line) {
    fprintf(stderr, "File [%s] Line [%d] Error [%s]\n", error_message, line, file);
#ifdef MPI
    MPI_Abort(MPI_COMM_WORLD, -1);
#else
    exit(0);
#endif
}

rstData* PIDX_restructure_data(MPI_Comm comm, int nprocs, int rank, 
                               int extents[5],
                               int set_expanded_box_dimension[5],
                               int count_local[5],
                               int offset_local[5],
                               dataType *buff ) {
  PIDX_rst_id rst_id;
  PIDX_Ndim_buffer*** rst_output_buffer;
  PIDX_Ndim_buffer** in_buf;
  int rst_output_buffer_count = 0, var = 0;
  int sub_div[5];
  //int offset_local[5];
  int slice;
  int number_of_variables = 1;
  int i,j, ret;
  dataType** write_data;
  //int *set_expanded_box_dimension;
  dataType* expanded_box;
  int send_c = 0, send_o = 0 ;
  int holding_data = 0;
  int local_rank_count[5] = {1,1,1,1,1};
  int local_rank_offset[5] = {0,0,0,0,0,};
  rstData *rst_data;

  // Structure to be returned
  rst_data = (rstData*)malloc(sizeof(rstData));

  write_data = (dataType**)malloc(sizeof(dataType*));
  write_data[0] = buff;

  //if (rank == 0)
    //fprintf(stderr, "from :: %f\t%f \n", write_data[0][0], write_data[0][1]);
  
  
  //sub_div[0] = (extents[0] / count_local[0]);
  //sub_div[1] = (extents[1] / count_local[1]);
  //sub_div[2] = (extents[2] / count_local[2]);
  //offset_local[2] = (rank / (sub_div[0] * sub_div[1])) * count_local[2];
  //slice = rank % (sub_div[0] * sub_div[1]);
  //offset_local[1] = (slice / sub_div[0]) * count_local[1];
  //offset_local[0] = (slice % sub_div[0]) * count_local[0];

  offset_local[3] = 0;
  offset_local[4] = 0;
  count_local[3] = 1;
  count_local[4] = 1;

  assert(offset_local[0] < extents[0]);
  assert(offset_local[1] < extents[1]);
  assert(offset_local[2] < extents[2]);
  assert(offset_local[3] < extents[3]);
  assert(offset_local[4] < extents[4]);

  /*
  if(rank == 0)
  std::cout << "Rank:: " << rank
            << " :: " << offset_local[0]
            << " :: " << offset_local[1]
            << " :: " << offset_local[2]
            << " :: " << offset_local[3]
            << " :: " << offset_local[4]
            << " Count:: "
            << " :: " << count_local[0]
            << " :: " << count_local[1]
            << " :: " << count_local[2]
            << " :: " << count_local[3]
            << " :: " << count_local[4] 
            << " Expand box"
            << " :: " << set_expanded_box_dimension[0]
            << " :: " << set_expanded_box_dimension[1]
            << " :: " << set_expanded_box_dimension[2]
            << " :: " << set_expanded_box_dimension[3]
            << " :: " << set_expanded_box_dimension[4]
            << " Global :" 
            << " :: " << extents[0] 
            << " :: " << extents[1] 
            << " :: " << extents[2] 
            << " :: " << extents[3] 
            << " :: " << extents[4] 
            << "\n";
  */

  in_buf = (PIDX_Ndim_buffer**) malloc(number_of_variables * 
                                         sizeof (PIDX_Ndim_buffer*));
  int *sample_per_variable_buffer = (int*) malloc(sizeof (int) * 
                                                  number_of_variables);
  sample_per_variable_buffer[0] = 1;
  
  for (i = 0; i < number_of_variables; i++)
    in_buf[i] = (PIDX_Ndim_buffer*) malloc(sizeof (PIDX_Ndim_buffer));

  rst_output_buffer = (PIDX_Ndim_buffer***)malloc(sizeof(*rst_output_buffer)*
                                                         number_of_variables);
  memset(rst_output_buffer, 0, sizeof (*rst_output_buffer) * number_of_variables);
      
  rst_output_buffer_count = 0;
  rst_id = PIDX_rst_init(MPI_COMM_WORLD, 3, extents, count_local, 
                         offset_local, 1, set_expanded_box_dimension, 
                         &rst_output_buffer_count);

  
  for (var = 0; var < number_of_variables; var++) 
  {
    if((int)sizeof(dataType) == (int)sizeof(double)) 
      PIDX_rst_buf_init(in_buf[var], 3, offset_local, count_local, write_data[var], 
                        sample_per_variable_buffer[var], MPI_DOUBLE, 
                        "var_name", var);
    
    if((int)sizeof(dataType) == (int)sizeof(float))
      PIDX_rst_buf_init(in_buf[var], 3, offset_local, count_local, write_data[var], 
                        sample_per_variable_buffer[var], MPI_FLOAT, 
                        "var_name", var);
            
    rst_output_buffer[var] = (PIDX_Ndim_buffer**)malloc((rst_output_buffer_count) 
                                                    * sizeof (PIDX_Ndim_buffer*));
    memset(rst_output_buffer[var], 0, (rst_output_buffer_count) * 
          sizeof (PIDX_Ndim_buffer*));
    
    
    PIDX_rst_restructure(rst_id, in_buf[var], rst_output_buffer[var], 
                         (rst_output_buffer_count));
    
    PIDX_rst_restructure_IO(rst_id, in_buf[var], rst_output_buffer[var], 
                            (rst_output_buffer_count), 1);

    
    if (rst_output_buffer_count != 0) 
    {
      expanded_box = (dataType*) malloc(sizeof (dataType) *  
                    (rst_output_buffer[var][0]->regular_upper_bounds[2] - 
                     rst_output_buffer[var][0]->regular_lower_bounds[2]) * 
                     (rst_output_buffer[var][0]->regular_upper_bounds[1] - 
                      rst_output_buffer[var][0]->regular_lower_bounds[1]) * 
                     (rst_output_buffer[var][0]->regular_upper_bounds[0] - 
                      rst_output_buffer[var][0]->regular_lower_bounds[0]) * 
                     sample_per_variable_buffer[var]);
      
      memset(expanded_box, 0, 
             (sizeof (dataType) * 
              (rst_output_buffer[var][0]->regular_upper_bounds[2] - 
               rst_output_buffer[var][0]->regular_lower_bounds[2]) * 
               (rst_output_buffer[var][0]->regular_upper_bounds[1] - 
                rst_output_buffer[var][0]->regular_lower_bounds[1]) * 
               (rst_output_buffer[var][0]->regular_upper_bounds[0] - 
                rst_output_buffer[var][0]->regular_lower_bounds[0]) * 
               sample_per_variable_buffer[var]));


      int k1, j1, i1, r, index = 0, recv_o = 0;
      for (r = 0; r < rst_output_buffer_count; r++) 
      {
        for (k1 = rst_output_buffer[var][r]->lower_bounds[2]; 
             k1 < rst_output_buffer[var][r]->upper_bounds[2]; k1++)
          for (j1 = rst_output_buffer[var][r]->lower_bounds[1]; 
               j1 < rst_output_buffer[var][r]->upper_bounds[1]; j1++)
              for (i1 = rst_output_buffer[var][r]->lower_bounds[0]; 
                   i1 < rst_output_buffer[var][r]->upper_bounds[0]; 
                   i1 = i1 + rst_output_buffer[var][r]->upper_bounds[0] - 
                   rst_output_buffer[var][r]->lower_bounds[0]) {

                index = ((rst_output_buffer[var][r]->upper_bounds[0] - 
                          rst_output_buffer[var][r]->lower_bounds[0])* 
                          (rst_output_buffer[var][r]->upper_bounds[1] - 
                           rst_output_buffer[var][r]->lower_bounds[1]) * 
                           (k1 - rst_output_buffer[var][r]->lower_bounds[2])) +
                          ((rst_output_buffer[var][r]->upper_bounds[0] - 
                            rst_output_buffer[var][r]->lower_bounds[0]) * 
                            (j1 - rst_output_buffer[var][r]->lower_bounds[1])) +
                            (i1 - rst_output_buffer[var][r]->lower_bounds[0]);

                send_o = index * sample_per_variable_buffer[var];
                send_c = (rst_output_buffer[var][r]->upper_bounds[0] - 
                          rst_output_buffer[var][r]->lower_bounds[0]) * 
                          sample_per_variable_buffer[var];


                recv_o = ((rst_output_buffer[var][r]->regular_upper_bounds[0] - 
                        rst_output_buffer[var][r]->regular_lower_bounds[0]) * 
                        (rst_output_buffer[var][r]->regular_upper_bounds[1] - 
                         rst_output_buffer[var][r]->regular_lower_bounds[1]) * 
                        (k1 - rst_output_buffer[var][r]->regular_lower_bounds[2]))
                        + ((rst_output_buffer[var][r]->regular_upper_bounds[0] - 
                        rst_output_buffer[var][r]->regular_lower_bounds[0])* 
                        (j1 - rst_output_buffer[var][r]->regular_lower_bounds[1]))
                        + (i1 - rst_output_buffer[var][r]->regular_lower_bounds[0]);

                memcpy(expanded_box + (recv_o * sample_per_variable_buffer[var]), 
                      rst_output_buffer[var][r]->buffer + send_o, 
                      send_c * sizeof (dataType));
            }
      }
    }
    
  
    if (rst_output_buffer_count != 0) {
      holding_data = 1;
      rst_data->hasData = 1;
      //std::cout << "Ex " << expanded_box[0] << " " << expanded_box[1] << "\n";
      local_rank_count[0] = rst_output_buffer[var][0]->regular_upper_bounds[0] 
                          - rst_output_buffer[var][0]->regular_lower_bounds[0];
      local_rank_count[1] = rst_output_buffer[var][0]->regular_upper_bounds[1] 
                          - rst_output_buffer[var][0]->regular_lower_bounds[1];
      local_rank_count[2] = rst_output_buffer[var][0]->regular_upper_bounds[2]
                          - rst_output_buffer[var][0]->regular_lower_bounds[2];
      local_rank_count[3] = rst_output_buffer[var][0]->regular_upper_bounds[3]
                          - rst_output_buffer[var][0]->regular_lower_bounds[3];
      local_rank_count[4] = rst_output_buffer[var][0]->regular_upper_bounds[4]
                          - rst_output_buffer[var][0]->regular_lower_bounds[4];

      local_rank_offset[0] = rst_output_buffer[var][0]->regular_lower_bounds[0];
      local_rank_offset[1] = rst_output_buffer[var][0]->regular_lower_bounds[1];
      local_rank_offset[2] = rst_output_buffer[var][0]->regular_lower_bounds[2];
      local_rank_offset[3] = rst_output_buffer[var][0]->regular_lower_bounds[3];
      local_rank_offset[4] = rst_output_buffer[var][0]->regular_lower_bounds[4];
    }
    else {
      rst_data->hasData = 0;
    }

    MPI_Comm_split(MPI_COMM_WORLD, rst_data->hasData, rank, &(rst_data->comm));
        
    //if (rst_output_buffer_count != 0) 
    //{
      //free(expanded_box);
      //expanded_box = 0;
    //}

    ret = PIDX_rst_buf_destroy(in_buf[var]);
    for (j = 0; j < rst_output_buffer_count; j++) {
      ret = PIDX_rst_buf_destroy(rst_output_buffer[var][j]);
      if (ret == -1) {
        fprintf(stderr, "PIDX : [%d] Error in PIDX_rst_buf_destroy\n", rank);
        MPI_Abort(MPI_COMM_WORLD, -1);
      }
    }
    if (rst_output_buffer_count != 0) {
      free(rst_output_buffer[var]);
      rst_output_buffer[var] = 0;
    }

   // int *rank_status;
   // int *rank_offset;
   // int *rank_count;

   // rank_status = (int*) malloc(sizeof (int) * nprocs);
   // if (!rank_status) PIDX_rst_print_error("Memory : rank_status", 
   //                                         __FILE__, __LINE__);
   // 
   // memset(rank_status, 0, (sizeof (int) * nprocs));
   // rank_offset = (int*) malloc(sizeof (int) * nprocs * PIDX_MAX_DIMENSIONS);
   // 
   // if (!rank_offset) PIDX_rst_print_error("Memory : rank_offset",
   //                                         __FILE__, __LINE__);
   // 
   // memset(rank_offset, 0, (sizeof (int) * nprocs * PIDX_MAX_DIMENSIONS));
   // rank_count = (int*) malloc(sizeof (int) * nprocs * PIDX_MAX_DIMENSIONS);

   // if (!rank_count) PIDX_rst_print_error("Memory : rank_count", 
   //                                       __FILE__, __LINE__);
   // 
   // memset(rank_count, 0, (sizeof (int) * nprocs * PIDX_MAX_DIMENSIONS));
   // ret = MPI_Gather( &holding_data, 1, MPI_INT, rank_status, 1, MPI_INT, 
   //                   0, MPI_COMM_WORLD);
   // 
   // if (ret != MPI_SUCCESS) PIDX_rst_print_error("MPI_Allgather : rank_status"
   //                                               , __FILE__, __LINE__);

   // ret = MPI_Gather( local_rank_offset, PIDX_MAX_DIMENSIONS, MPI_INT, 
   //                   rank_offset, PIDX_MAX_DIMENSIONS, MPI_INT, 0, 
   //                   MPI_COMM_WORLD);

   // if (ret != MPI_SUCCESS) PIDX_rst_print_error("MPI_Allgather : rank_offset",
   //                                               __FILE__, __LINE__);

   // ret = MPI_Gather( local_rank_count, PIDX_MAX_DIMENSIONS, MPI_INT, 
   //                   rank_count, PIDX_MAX_DIMENSIONS, MPI_INT, 0, 
   //                   MPI_COMM_WORLD);

   // if (ret != MPI_SUCCESS) PIDX_rst_print_error("MPI_Allgather : rank_count",
   //                                               __FILE__, __LINE__);

    rst_data->data = expanded_box;
    rst_data->dataOffset[0] = local_rank_offset[0];
    rst_data->dataOffset[1] = local_rank_offset[1];
    rst_data->dataOffset[2] = local_rank_offset[2];
    
    rst_data->dataDim[0] = local_rank_count[0];
    rst_data->dataDim[1] = local_rank_count[1];
    rst_data->dataDim[2] = local_rank_count[2];

    //fprintf(stderr, "rst_data: rank: %d [%d %d %d] [%d %d %d]\n", rank, 
    //           rst_data->dataOffset[0], 
    //           rst_data->dataOffset[1], 
    //           rst_data->dataOffset[2],  
    //           rst_data->dataOffset[0] + rst_data->dataDim[0], 
    //           rst_data->dataOffset[1] + rst_data->dataDim[1], 
    //           rst_data->dataOffset[2] + rst_data->dataDim[2]);

   // int i1;
   // if(rank == 0)
   // {
   //   FILE* fp = fopen("output", "w");

   //   //float *read_buffer = (float*)malloc(sizeof(float)*rank_count[0]*
   //   //                                    rank_count[1]*rank_count[2]);
   //   
   //   for(i1 = 0 ; i1 < nprocs ; i1++)
   //     fprintf(fp, "%d %d %d %d %d %d %d %d %d\n", var, i1, 
   //           rank_status[i1], 
   //           rank_offset[PIDX_MAX_DIMENSIONS * i1 + 0], 
   //           rank_offset[PIDX_MAX_DIMENSIONS * i1 + 1], 
   //           rank_offset[PIDX_MAX_DIMENSIONS * i1 + 2],
   //           rank_count[PIDX_MAX_DIMENSIONS * i1 + 0], 
   //           rank_count[PIDX_MAX_DIMENSIONS * i1 + 1], 
   //           rank_count[PIDX_MAX_DIMENSIONS * i1 + 2]);

   //   fclose(fp);
   //   //FILE* fptr = fopen("test.raw", "wb");
   //   /*
   //   FILE* fptr = fopen("/home/sid/research/pird/data/blob2_50_50_50.raw", "rb");
   //   fread(read_buffer, sizeof(dataType),
   //               rank_count[0]*rank_count[1]*rank_count[2], fptr);
   //   fclose(fptr);
   //   int equal_count = 0;
   //   int non_equal_count = 0;
   //   for(i = 0 ; i < rank_count[0]*rank_count[1]*rank_count[2] ; i++)
   //   {
   //       if(read_buffer[i] == expanded_box[i])
   //         equal_count++;
   //       else
   //         non_equal_count++;
   //   }

   //   */
   //   //printf("Matching Elements %d :: Unequal Elements %d\n", equal_count, non_equal_count);
   //   //std::cout << "Rank COunt: " << rank_count[0] << rank_count[1] <<
   //   //rank_count[2] << expanded_box[0] << "\n";
   //   
   // }
  }
  
  free(rst_output_buffer);
  rst_output_buffer = 0;

  return rst_data;
}

