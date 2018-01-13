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
*  Pradeep Subedi (2017) Rutgers University
*  pradeep.subedi@rutgers.edu
*/

#include<unistd.h>
#include<stdio.h>
#include<stdlib.h>
#include<time.h>
#include "debug.h"
#include "/home/shared/scratch/ps917/include/fann.h"
#include "ml_pthread.h"

//unsigned int num_data = 30;
int num_variables = 1; //number of variables in dataspaces
int inps = 0;

struct fann **ann;
struct fann_train_data **data;
pthread_mutex_t ml_mutex = PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t  ml_cond = PTHREAD_COND_INITIALIZER;
int *init_retrain;
int *data_counter;
int *retrain;
int complete = 0;

void *machine_learning(void *attr){
	const unsigned int num_input = inps*2;
	const unsigned int num_output = inps*2;
	const unsigned int num_layers = 3;
	const unsigned int num_neurons_hidden = inps*2;
	const float desired_error = (const float) 0.001;
	const unsigned int max_epochs = 10000;
	const unsigned int epochs_between_reports = 100;
	
	unsigned int max_neurons = 10;
	unsigned int neurons_between_reports = 1;
	enum fann_activationfunc_enum activation;
	enum fann_train_enum training_algorithm = FANN_TRAIN_RPROP;

	ann = malloc(sizeof(struct fann *)*num_variables);
	data = malloc(sizeof(struct fann_train_data *)*num_variables);
	init_retrain = (int *)malloc(sizeof(int)*num_variables);
	retrain = (int *)malloc(sizeof(int)*num_variables);
	data_counter = (int *)malloc(sizeof(int)*num_variables);
	int i,j,k;
	for (i = 0; i < num_variables; ++i)
	{
		ann[i] = fann_create_standard(num_layers, num_input, num_neurons_hidden, num_output);
		//ann[i] = fann_create_shortcut(num_layers, num_input,num_neurons_hidden, num_output);
		//fann_set_activation_function_output(ann[i], FANN_SIGMOID_SYMMETRIC);
		//fann_set_activation_function_hidden(ann[i], FANN_SIGMOID_SYMMETRIC);
		//fann_set_activation_function_hidden(ann[i], FANN_SIGMOID_SYMMETRIC);
		//fann_set_activation_function_output (ann[i], FANN_SIGMOID);
		//ann[i] = fann_create_shortcut(2, num_input, num_output);
		//fann_set_training_algorithm(ann[i], training_algorithm);
		//fann_set_activation_function_hidden(ann[i], FANN_SIGMOID_SYMMETRIC);
		//fann_set_activation_function_output(ann[i], FANN_LINEAR);
		//fann_set_train_error_function(ann[i], FANN_ERRORFUNC_LINEAR);
		//fann_set_bit_fail_limit(ann[i], (fann_type)0.9);
		//fann_set_train_stop_function(ann[i], FANN_STOPFUNC_BIT);
		data[i] = fann_create_train(3000, num_input, num_output);
		init_retrain[i] = 0;
		retrain[i] = 0;
		data_counter[i] = 0;
		data[i]->num_data = 0;
	}


	int var = 0;
	while(var == 0){
		if(complete==1){
			for (i = 0; i < num_variables; ++i)
			{
				fann_destroy(ann[i]);
			}
			pthread_mutex_unlock(&ml_mutex);
			return NULL;
		}
		
		pthread_mutex_lock(&ml_mutex);
		for (i = 0; i < num_variables; ++i)
		{
			if(init_retrain[i]==1 && retrain[i]==1){
				
				for (j = 0; j < data[i]->num_data; ++j)
				{
					for (k = 0; k < inps*2; ++k)
					{
						printf("Inp %f \t", data[i]->input[j][k]);
						printf("Oup %f \t", data[i]->output[j][k]);
					}
					printf("Peer %f \t", data[i]->input[j][inps*2]);
					printf("Peer %f \t", data[i]->input[j][inps*2+1]);
					printf("\n");
				}
				
				data[i]->num_data--;
				fann_set_scaling_params(ann[i], data[i], -1, 1, -1, 1);
				fann_scale_input(ann[i], data[i]);
				fann_scale_output(ann[i], data[i]);
				//fann_scale_train(ann[i], data[i]);
				//fann_shuffle_train_data(data[i]);
				//fann_cascadetrain_on_data(ann[i], data[i], max_neurons, neurons_between_reports, desired_error);
				fann_train_on_data(ann[i], data[i], max_epochs, epochs_between_reports, desired_error);
				fann_descale_input(ann[i], data[i]);
				fann_descale_output(ann[i], data[i]);
				data[i]->num_data++;
				retrain[i] = 0;
				//data[i]->num_data = data[i]->num_data+1;
			}
		}
		
		pthread_cond_wait(&ml_cond, &ml_mutex);
		pthread_mutex_unlock(&ml_mutex);

	}
	return NULL;

}

