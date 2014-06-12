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

// TODO: use more generic approach to convert strings to type id
static int str_to_var_type(const char *str, enum hstaging_var_type *type)
{
	if (0 == strcmp(str, "depend")) {
		*type = var_type_depend;
		return 0;
	}
 
	if (0 == strcmp(str, "put")) {
		*type = var_type_put;
		return 0;
	}

	if (0 == strcmp(str, "get")) {
		*type = var_type_get;
		return 0;
	}

	fprintf(stderr, "Unknown variable type string '%s'\n", str);	
	return -1;
}

static int str_to_placement_hint(const char *str, enum hstaging_placement_hint *hint)
{
	if (0 == strcmp(str, "insitu")) {
		*hint = hint_insitu;
		return 0;
	}

	if (0 == strcmp(str, "intransit")) {
		*hint = hint_intransit;
		return 0;
	}

	if (0 == strcmp(str, "none")) {
		*hint = hint_none;
        return 0;
	}

	fprintf(stderr, "Unknown placement hint string '%s'\n", str);
	return -1;
}

void update_task_status(struct hstaging_task *task, enum hstaging_task_status status)
{
    task->status = status;
}

int is_task_finish(struct hstaging_task *task)
{
    return task->status == task_finish;
}

int is_task_ready(struct hstaging_task *task)
{
	if (task->status == task_ready) return 1;
	else if (task->status == task_not_ready) {
        int i;
        for (i = 0; i < task->num_vars; i++) {
            if (task->vars[i].type == var_type_depend &&
                task->vars[i].status == var_not_available) {
                return 0;
            }
        }

		update_task_status(task, task_ready);
		return 1;
	}

	return 0;
}

int get_ready_tasks(struct hstaging_workflow *wf,
	struct hstaging_task **tasks, int *n /*num_ready_tasks*/)
{
	*n = 0;
    if (wf == NULL) return 0;

	struct hstaging_task *task = NULL;
    list_for_each_entry(task, &wf->task_list, struct hstaging_task, entry) {
        if (is_task_ready(task)) {
            tasks[*n] = task;
            *n = *n + 1;
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

static int evaluate_task_by_available_var(struct hstaging_task *task, const struct hstaging_var *var_desc)
{
    if (!task) return 0;
    struct hstaging_var *var = lookup_var(task, var_desc->name);
    if (var) {
        var->status = var_available;
        var->elem_size = var_desc->elem_size;
        var->bb = var_desc->bb;
    } 

	return 0;
}

// Evaluate the dataflow through the newly available variable
int evaluate_dataflow_by_available_var(struct hstaging_workflow *wf, const struct hstaging_var *var_desc) 
{
    if (wf == NULL) return 0;

	// Update variable and task status
	struct hstaging_task *task = NULL;
    list_for_each_entry(task, &wf->task_list, struct hstaging_task, entry) {
        evaluate_task_by_available_var(task, var_desc);
	}
	
	return 0;
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

static int read_task_var_info(struct hstaging_task *task, char *fields[], int num_fields)
{
	int index_to_var_type = 3;
	// Get var type
	enum hstaging_var_type type;
	if (str_to_var_type(fields[index_to_var_type], &type) < 0) {
		return -1;
	}

	// Get the variables
	int vars_start_at = 4;
	int i = vars_start_at, j = 0;
	while (i < num_fields) {
		task_add_var(task, type, fields[i]);
		i++;
	}

	return 0;
}

static int read_task_placement_hint(struct hstaging_task *task, char *fields[], int num_fields)
{
	int index_to_hint = 3;
	// Get placement hint
	if (str_to_placement_hint(fields[index_to_hint], &task->placement_hint) < 0 ) {
		return -1;
	}

	return 0;
}

static int read_task_size_hint(struct hstaging_task *task, char *fields[], int num_fields)
{
    int index_to_hint = 3;
    int size_hint = atoi(fields[index_to_hint]);
    if (size_hint < 0) {
        size_hint = 0;
    }

    task->size_hint = size_hint;
    return 0;
}

static int read_workflow_task(struct hstaging_task *task, char *fields[], int num_fields)
{
	int index_to_appid = 1;
	int index_to_desc = 2;
	int required_fields = 4;

	if (num_fields < required_fields) {
		fprintf(stderr, "Can NOT be valid task information\n");
		return -1;
	}

	int appid = atoi(fields[index_to_appid]);
    task->appid = appid;

	char *t_desc = fields[index_to_desc];
	if (0 == strcmp(t_desc, "variable_info")) {
		if (read_task_var_info(task, fields, num_fields) < 0) {
			return -1;
		}
	} else if (0 == strcmp(t_desc, "placement_hint")) {
		if (read_task_placement_hint(task, fields, num_fields) < 0) {
			return -1;	
		}
	} else if (0 == strcmp(t_desc, "size_hint")) {
        if (read_task_size_hint(task, fields, num_fields) < 0 ) {
            return -1;
        } 
    }

	return 0;
}

int parse_task_conf_file(struct hstaging_task *task, const char *fname)
{
	const size_t MAX_LINE = 4096;
	const char *DELIM = " \t\n\r"; //space, tab, line feed, carriage return
	const int MAX_FIELDS = 50;

    int err = -1;
    if (!task) return err;

	FILE *file = fopen(fname, "r");
	if (!file) {
		fprintf(stderr, "%s(): unable to open file %s\n", __func__, fname);
		return err;  
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

		//printf("line: %s\n", line);

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
		if (0 == strcmp("task", fields[0])) {
			read_workflow_task(task, fields, n);
		}

		// Free the fields array
		int j = 0;
		while (j < n) {
			free(fields[j]);
			j++;
		}	
	}

	fclose(file);
	return 0;	
}

void print_workflow(struct hstaging_workflow *wf)
{
    if (!wf) return;
    printf("\nworkflow tasks:\n");
    struct hstaging_task *task;
    list_for_each_entry(task, &wf->task_list, struct hstaging_task, entry) {
		printf("task tid= %u appid= %d size_hint= %d placement_hint= %d submitter_dart_id= %d ",
			task->tid, task->appid, task->size_hint, task->placement_hint, task->submitter_dart_id);
		int i;
		for (i = 0; i < task->num_vars; i++) {
			printf("var '%s' type '%s' ", task->vars[i].name,
				var_type_name[task->vars[i].type]);
		}
        printf("\n");
	}
}

/*
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
				update_task_instance_status(ready_tasks[j], task_finish);
			}
		}
	}

	fclose(file);

	return 0;
}
*/