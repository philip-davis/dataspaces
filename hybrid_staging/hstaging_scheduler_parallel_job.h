#ifndef __HSTAGING_SCHEDULER_PARALLEL_H__
#define __HSTAGING_SCHEDULER_PARALLEL_H__

#ifdef __cplusplus
extern "C" {
#endif

int hstaging_scheduler_parallel_parse_args(int argc, char** argv);
void hstaging_scheduler_parallel_usage();

int hstaging_scheduler_parallel_init();
int hstaging_scheduler_parallel_run();
int hstaging_scheduler_parallel_finish();

#ifdef __cplusplus
}
#endif

#endif
