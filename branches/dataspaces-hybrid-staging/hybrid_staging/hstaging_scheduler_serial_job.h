#ifndef __HSTAGING_SCHEDULER_SERIAL_H__
#define __HSTAGING_SCHEDULER_SERIAL_H__

#ifdef __cplusplus
extern "C" {
#endif

int hstaging_scheduler_serial_parse_args(int argc, char** argv);
void hstaging_scheduler_serial_usage();

int hstaging_scheduler_serial_init();
int hstaging_scheduler_serial_run();
int hstaging_scheduler_serial_finish();

#ifdef __cplusplus
}
#endif

#endif
