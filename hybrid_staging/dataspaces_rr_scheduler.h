#ifndef __DATASPACES_RR_SCHEDULER_H__
#define __DATASPACES_RR_SCHEDULER_H__

#ifdef __cplusplus
extern "C" {
#endif

int dspaces_rr_parse_args(int argc, char** argv);
void dspaces_rr_usage();

int dspaces_rr_init();
int dspaces_rr_run();
int dspaces_rr_finish();

#ifdef __cplusplus
}
#endif

#endif
