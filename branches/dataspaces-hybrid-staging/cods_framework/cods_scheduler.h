#ifndef __CODS_SCHEDULER_H__
#define __CODS_SCHEDULER_H__

#ifdef __cplusplus
extern "C" {
#endif

int cods_scheduler_parse_args(int argc, char** argv);
void cods_scheduler_usage();

int cods_scheduler_init();
int cods_scheduler_run();
int cods_scheduler_finish();

#ifdef __cplusplus
}
#endif

#endif
