#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "hstaging_def.h"
#include "hstaging_strutil.h"

static void print_str_decimal(const char *str)
{
	int j = 0;
	while (j < strlen(str)) {
		printf("%d ", str[j++]);
	}	
	printf("\n");
}

static int str_to_var_type(const char *str, enum hstaging_var_type *type)
{
	if (0 == strcmp(str, "DEPEND")) {
		*type = DEPEND;
		return 0;
	}
 
	if (0 == strcmp(str, "PUT")) {
		*type = PUT;
		return 0;
	}

	if (0 == strcmp(str, "GET")) {
		*type = GET;
		return 0;
	}

	fprintf(stderr, "Unknown variable type string '%s'\n", str);	
	return -1;
}

void update_task_instance_status(struct task_instance *ti, enum hstaging_task_status status)
{
	ti->status = status;
}

static int is_task_instance_ready(struct task_instance *ti)
{
	if (ti->status == READY) return 1;
	else if (ti->status == NOT_READY) {
		struct var_instance *vi;
		list_for_each_entry(vi, &ti->input_vars_list, struct var_instance, entry)
		{
			if (vi->status == NOT_AVAILABLE)
				return 0;
		}

		update_task_instance_status(ti, READY);
		return 1;
	}

	return 0;
}

int get_ready_tasks(struct hstaging_workflow *wf,
	struct task_instance **tasks, int *n)
{
	struct hstaging_task *t = NULL;
	int i;
	*n = 0;
	for (i = 0; i < wf->num_tasks; i++) {
		t = &(wf->tasks[i]);
		struct task_instance *ti;
		list_for_each_entry(ti, &t->instances_list, struct task_instance, entry)
		{
			if (is_task_instance_ready(ti)) {
				tasks[*n] = ti;
				*n = *n + 1;
			}
		}
	}

	return 0;
}

static struct hstaging_var *lookup_var(struct hstaging_task *task, const char *var_name)
{
	int i;
	for (i = 0; i < task->num_vars; i++) {
		if (0 == strcmp(var_name, task->vars[i].name))
			return &task->vars[i];
	}

	return NULL;
}

static struct var_instance *lookup_var_instance(struct task_instance *ti, struct hstaging_var *var) 
{
	struct var_instance *vi;
	list_for_each_entry(vi, &ti->input_vars_list, struct var_instance, entry)
	{
		if (vi->var) {
			if (0 == strcmp(vi->var->name, var->name))
				return vi;
		}
	}

	return NULL;
}

static struct var_instance *new_var_instance(struct task_instance *ti, struct hstaging_var *var)
{
	struct var_instance *vi = (struct var_instance*)malloc(sizeof(*vi));
	vi->var = var;
	vi->status = NOT_AVAILABLE;
	
	list_add(&vi->entry, &ti->input_vars_list);
	return vi;
}

static struct task_instance *lookup_task_instance(struct hstaging_task *task, int step)
{
	struct task_instance *ti;
	list_for_each_entry(ti, &task->instances_list, struct task_instance, entry)
	{
		if (ti->step == step)
			return ti;
	}

	return NULL;
}

static struct task_instance *new_task_instance(struct hstaging_task *task, int step)
{
	struct task_instance *ti = (struct task_instance*)malloc(sizeof(*ti));
	ti->tid = task->tid;
	ti->step = step; //TODO: this the unique id for instance?
	ti->status = NOT_READY;

	// Init the input_vars_list
	INIT_LIST_HEAD(&ti->input_vars_list);
	int i;
	for (i = 0; i < task->num_vars; i++) {
		if (task->vars[i].type == DEPEND) {
			new_var_instance(ti, &task->vars[i]); 	
		}
	}

	list_add(&ti->entry, &task->instances_list);
	return ti;
}

static int evaluate_task_by_available_var(struct hstaging_task *task, struct hstaging_var *var, struct var_descriptor *var_desc)
{
	struct task_instance *ti;
	ti = lookup_task_instance(task, var_desc->step);
	if (ti == NULL) {
		ti = new_task_instance(task, var_desc->step);
	}

	struct var_instance *vi;
	vi = lookup_var_instance(ti, var);
	if (vi == NULL) {
		fprintf(stderr, "%s(): vi == NULL should not happen\n", __func__);
		return -1;
	}
	
	vi->status = AVAILABLE;
	vi->size = var_desc->size;
	vi->bb = var_desc->bb;
	return 0;
}

// Evaluate the dataflow through the newly available variable
int evaluate_dataflow_by_available_var(struct hstaging_workflow *wf, struct var_descriptor *var_desc)
{
	// Update the variable and task status
	struct hstaging_task *task = NULL;
	int i;
	for (i = 0; i < wf->num_tasks; i++) {
		task = &(wf->tasks[i]);
		struct hstaging_var *var = lookup_var(task, var_desc->var_name);
		if (var && var->type == DEPEND) { 
			evaluate_task_by_available_var(task, var, var_desc);
		}
	}
	
	return 0;
}

static struct hstaging_task* task_lookup_by_name(struct hstaging_workflow *wf, const char *name)
{
	struct hstaging_task *task = NULL;
	int i;
	for (i = 0; i < wf->num_tasks; i++) {
		if (0 == strcmp(wf->tasks[i].name, name)) {
			task = &(wf->tasks[i]);
			return task;
		}
	}

	return NULL;
}

static void task_add_var(struct hstaging_task *task, const enum hstaging_var_type type, const char *name)
{
	if (task == NULL) {
		fprintf(stderr, "%s(): task == NULL\n", __func__);
		return;
	}

	if (task->num_vars >= MAX_NUM_VARS) {
		fprintf(stderr, "%s(): exceeds MAX_NUM_VARS\n", __func__);
		return;
	}

	// New var
	struct hstaging_var *var = &task->vars[task->num_vars];
	var->type = type;
	strcpy(var->name, name);
	task->num_vars++;
	return;
}

static int read_workflow_task(struct hstaging_workflow *wf, char *fields[], int num_fields)
{
	// TODO: remove the magic number
	if (num_fields < 4) {
		fprintf(stderr, "Can NOT be valid task information\n");
		return -1;
	}

	struct hstaging_task *task = task_lookup_by_name(wf, fields[1]);
	if (task == NULL) {
		// New task
		task = &(wf->tasks[wf->num_tasks]);
		strcpy(task->name, fields[1]);
		task->tid = wf->num_tasks + 1; //TODO: fix me
		INIT_LIST_HEAD(&task->instances_list);
		wf->num_tasks++;
	}

	// Get var type
	enum hstaging_var_type type;
	if (str_to_var_type(fields[2], &type) < 0) {
		wf->num_tasks--;
		return -1;
	}

	// Get the variables
	int var_field_start_at = 3;
	int i = var_field_start_at, j = 0;
	while (i < num_fields) {
		task_add_var(task, type, fields[i]);
		i++;
	}

	return 0;
}

static struct hstaging_workflow* new_workflow(const char *fname)
{
	struct hstaging_workflow *wf = (struct hstaging_workflow*)
			malloc(sizeof(*wf));
	if (wf == NULL) {
		fprintf(stderr, "%s(): malloc failed\n", __func__);
		return NULL;
	}

/*
	size_t len = strlen(fname);
	wf->config_file = (char*)malloc(len+1);
	strcpy(wf->config_file, fname);
*/
	wf->num_tasks = 0;

	return wf;
}

static int free_var_instance(struct task_instance *ti)
{
	struct var_instance *vi, *temp;
	list_for_each_entry_safe(vi, temp, &ti->input_vars_list, struct var_instance, entry)
	{
		list_del(&vi->entry);
		free(vi);
	}
	
	return 0;
}

static int free_task_instance(const struct hstaging_task *t)
{
	struct task_instance *ti, *temp;
	list_for_each_entry_safe(ti, temp, &t->instances_list, struct task_instance, entry)
	{
		list_del(&ti->entry);
		free_var_instance(ti);
		free(ti);
	}

	return 0;
}

int free_workflow(struct hstaging_workflow *wf)
{
	int i;
	for (i = 0; i < wf->num_tasks; i++) {
		struct hstaging_task *t = &(wf->tasks[i]);
		free_task_instance(t);
	}

	free(wf);
	return 0;
}

struct hstaging_workflow* read_workflow_conf_file(const char *fname)
{
	int err = -1;
	const size_t MAX_LINE = 4096;
	const char *DELIM = " \t\n\r";
	const int MAX_FIELDS = 50;

	struct hstaging_workflow *wf = new_workflow(fname);
	if (wf == NULL) {
		return NULL;
	}

	FILE *file = fopen(fname, "r");
	if (!file) {
		fprintf(stderr, "%s(): unable to open file %s\n", __func__, fname);
		free(wf);
		return NULL;  
	}

	char line[MAX_LINE];
	int i = 1;
	while (fgets(line, MAX_LINE, file) != NULL) {
		// Trim the line
		trim(line, DELIM);
		i++;

		// Blank line
		if (strlen(line) == 0)
			continue;

		// Comment line
		if (line[0] == '#')
			continue;

		printf("line: %s\n", line);

		// Split line into fields
		int n = 0;
		char *fields[MAX_FIELDS];
		char *tok;
		tok = strtok(line, DELIM);
		while (tok != NULL) {
			if (n < MAX_FIELDS) {
				fields[n] = (char *) malloc(strlen(tok)+1);
				strcpy(fields[n], tok);			
				tok = strtok(NULL, DELIM);
				n++;
			} else {
				fprintf(stderr, "Exceeds the max number of fields %d\n",
					MAX_FIELDS);
				break;
			}
		}

		// Read workflow task information
		if (0 == strcmp("TASK", fields[0])) {
			read_workflow_task(wf, fields, n);
		}

		// Free the fields array
		int j = 0;
		while (j < n) {
			free(fields[j]);
			j++;
		}	

	}

	fclose(file);

	err = 0;
	return wf;	
}

void print_workflow(struct hstaging_workflow *wf)
{
	int i;
	for (i = 0; i < wf->num_tasks; i++) {
		struct hstaging_task *task = &(wf->tasks[i]);
		printf("\ntask name: %s\n", task->name);
		int j;
		for (j = 0; j < task->num_vars; j++) {
			printf("var '%s' type '%s'\n", task->vars[j].name,
				var_type_name[task->vars[j].type]);
		}
	}
}

int read_emulated_vars_sequence(struct hstaging_workflow *wf, const char *fname)
{
	int err = -1;
	const size_t MAX_LINE = 4096;
	const char *DELIM = " \t\n\r";
	const int MAX_FIELDS = 50;

	FILE *file = fopen(fname, "r");
	if (!file) {
		fprintf(stderr, "%s(): unable to open file %s\n", __func__, fname);
		free(wf);
		return -1;  
	}

	char line[MAX_LINE];
	int i = 1;
	while (fgets(line, MAX_LINE, file) != NULL) {
		// Trim the line
		trim(line, DELIM);
		i++;

		// Blank line
		if (strlen(line) == 0)
			continue;

		// Comment line
		if (line[0] == '#')
			continue;

		// Split line into fields
		int n = 0;
		char *fields[MAX_FIELDS];
		char *tok;
		tok = strtok(line, DELIM);
		while (tok != NULL) {
			if (n < MAX_FIELDS) {
				fields[n] = (char *) malloc(strlen(tok)+1);
				strcpy(fields[n], tok);			
				tok = strtok(NULL, DELIM);
				n++;
			} else {
				fprintf(stderr, "Exceeds the max number of fields %d\n",
					MAX_FIELDS);
				break;
			}
		}

		// Update the tasks
		int step = atoi(fields[1]);
		struct var_descriptor var_desc;
		strcpy(var_desc.var_name, fields[0]);
		var_desc.step = step;
		evaluate_dataflow_by_available_var(wf, &var_desc);

		// Get READY tasks
		printf("Available var %s step= %d\n", fields[0], step);
		struct task_instance *ready_tasks[MAX_NUM_TASKS];
		int num_ready_tasks = 0;
		get_ready_tasks(wf, ready_tasks, &num_ready_tasks);
		if (num_ready_tasks > 0) {
			int j;
			for (j = 0; j < num_ready_tasks; j++) {
				printf("Execute tid= %d step= %d\n",
					ready_tasks[j]->tid, ready_tasks[j]->step);
				update_task_instance_status(ready_tasks[j], FINISH);
			}
		}
	}

	fclose(file);

	return 0;
} 
