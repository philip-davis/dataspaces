#ifndef __HSTAGING_SCHEDULER_H__
#define __HSTAGING_SCHEDULER_H__

#ifdef __cplusplus
extern "C" {
#endif

int hstaging_scheduler_parse_args(int argc, char** argv);
void hstaging_scheduler_usage();

int hstaging_scheduler_init();
int hstaging_scheduler_run();
int hstaging_scheduler_finish();

#ifdef __cplusplus
}
#endif

#endif
