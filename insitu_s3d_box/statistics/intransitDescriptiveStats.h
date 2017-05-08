#include "dataspaces_api.h"

struct stats {
  double mean;
  double mom2;
  double mom3;
  double mom4;
  double minF;
  double maxF;
};

struct variables {
  int numSamples;
  int numStats;
  struct stats st[0];
};

void dumpPrimary(int tstep, int numRows, int *nameIndex, int *ns,  double *mean, double *mom2, double *mom3, double *mom4, double *minF, double*maxF);

void dumpDerived(int tstep, int numRows, double *stdDev, double *var,  double *skew, double *kurt, double *sum);

int perform_intransit_stat(struct list_head *data_list);
