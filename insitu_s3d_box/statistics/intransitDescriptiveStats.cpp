#include "intransitDescriptiveStats.h"
#include "insitu_data_description.h"
#include <iostream>
#include <fstream>
#include <cmath>
using namespace std;

int perform_intransit_stat(struct list_head *data_list)
{
	struct data_item *item;
	int count=0, numRows, *ns, tstep;
  	double *mean, *mom2, *mom3, *mom4, *minF, *maxF;

	list_for_each_entry(item, data_list, item_entry) {
    		tstep = item->desc.tstep;
		struct variables *localData = (struct variables *) item->buf;
		struct stats *st = (struct stats*) (localData->st);
    		if(count == 0) {
			// initialize
			numRows = localData->numStats;
			mean = new double[numRows];
			mom2 = new double[numRows];
			mom3 = new double[numRows];
			mom4 = new double[numRows];
			minF = new double[numRows];
			maxF = new double[numRows];
 			ns = new int[numRows]; 
			for(int r=0; r < numRows; r++) {
				ns[r] = localData->numSamples;
				mean[r] = st[r].mean;
				mom2[r] = st[r].mom2;
				mom3[r] = st[r].mom3;
				mom4[r] = st[r].mom4;
				minF[r] = st[r].minF;
				maxF[r] = st[r].maxF;
			}
		} else {
			for(int r=0; r < numRows; r++) {
    				int ns_l = localData->numSamples;
      				int N = ns[r] + ns_l;
        			double mean_part = st[r].mean;
        			double mom2_part = st[r].mom2;
        			double mom3_part = st[r].mom3;
        			double mom4_part = st[r].mom4;
	
       		 		double delta = mean_part - mean[r];
       		 		double delta_sur_N = delta / static_cast<double>( N );
       		 		double delta2_sur_N2 = delta_sur_N * delta_sur_N;
	
       	 			int ns2 = ns[r] * ns[r];
       	 			int ns_l2 = ns_l * ns_l;
       		 		int prod_ns = ns[r] * ns_l;

        			mom4[r] += mom4_part
          			+ prod_ns * ( ns2 - prod_ns + ns_l2 ) * delta * delta_sur_N * delta2_sur_N2
          			+ 6. * ( ns2 * mom2_part + ns_l2 * mom2[r] ) * delta2_sur_N2
          			+ 4. * ( ns[r] * mom3_part - ns_l * mom3[r] ) * delta_sur_N;

        			mom3[r] += mom3_part
          			+ prod_ns * ( ns[r] - ns_l ) * delta * delta2_sur_N2
          			+ 3. * ( ns[r] * mom2_part - ns_l * mom2[r] ) * delta_sur_N;

        			mom2[r] += mom2_part
          			+ prod_ns * delta * delta_sur_N;

        			mean[r] += ns_l * delta_sur_N;

        			ns[r] = N;
				if(minF[r] > st[r].minF) minF[r] = st[r].minF;
				if(maxF[r] < st[r].maxF) maxF[r] = st[r].maxF;
			}
		}
		count++;
	}

	int nameIndex[3] = {6, 7, 8};
  	// add link to data names
	dumpPrimary(tstep, numRows, nameIndex, ns, mean, mom2, mom3, mom4, minF, maxF);
  
	double *stdDev = new double[numRows];
	double *var = new double[numRows];
	double *skew = new double[numRows];
	double *kurt = new double[numRows];
	double *sum = new double[numRows];
	
	// compute derived
	for(int r=0; r < numRows; r++) {
    		if ( ns[r] == 1 || mom2[2] < 1.e-150 ) {
      			stdDev[r] = 0.;
      			var[r] = 0.;
      			skew[r] = 0.;
      			kurt[r] = 0.;
      			sum[r] = 0.;
      		} else {
      			double n = static_cast<double>( ns[r] );
      			double inv_n = 1. / n;
      			double nm1 = n - 1.;

      			// Variance
        		var[r] = mom2[r] / nm1;
      			// Standard deviation
      			stdDev[r] = sqrt( var[r] );

      			// Skeweness and kurtosis
      			double var_inv = nm1 / mom2[r];
      			double nvar_inv = var_inv * inv_n;
      			skew[r] = nvar_inv * sqrt( var_inv ) * mom3[r];
      			kurt[r] = nvar_inv * var_inv * mom4[r] - 3.;
        	}
    		// Sum
    		sum[r] = ns[r] * mean[r];
      	}


	dumpDerived(tstep, numRows, stdDev, var,  skew, kurt, sum);

	printf("%s(): Bucket %d, get %d data objs\n", __func__, count);

	delete [] ns;
	delete [] mean;
	delete [] mom2;
	delete [] mom3;
	delete [] mom4;
	delete [] stdDev;
	delete [] var;
	delete [] skew;
	delete [] kurt;
	delete [] sum;

	return 0;
}

void dumpPrimary(int tstep, int numRows, int *nameIndex, int *ns,  double *mean, double *mom2, double *mom3, double *mom4, double *minF, double*maxF) {
  	char fname[256];
  	sprintf(fname, "../post/descriptiveStats/time-%d_primary_intransit.vtk", tstep);
	ofstream outfile(fname);

	outfile << "# vtk DataFile Version 3.0" << endl;
	outfile << "vtk output" << endl;
	outfile << "ASCII" << endl;
	outfile << "DATASET TABLE" << endl;
	outfile << "ROW_DATA " << numRows << endl;
	outfile << "FIELD FieldData 8" << endl;
	outfile << "Variable 1 " << numRows << " string" << endl;
        for(int r=0; r < numRows; r++) {
		outfile << gInsituVarNames_noghost[nameIndex[r]] << endl;
	}
	outfile << endl;
	outfile << "Cardinality 1 " << numRows << " vtkIdType" << endl;	
	for(int r = 0; r < numRows; r++) {
 		outfile << ns[r] << " ";
	}
	outfile << endl;
	outfile << "Minimum 1 " << numRows << " double" << endl;	
	for(int r = 0; r < numRows; r++) {
 		outfile << minF[r] << " ";
	}
	outfile << endl;
	outfile << "Maximum 1 " << numRows << " double" << endl;	
	for(int r = 0; r < numRows; r++) {
 		outfile << maxF[r] << " ";
	}
	outfile << endl;
	outfile << "Mean 1 " << numRows << " double" << endl;	
	for(int r = 0; r < numRows; r++) {
 		outfile << mean[r] << " ";
	}
	outfile << endl;
	outfile << "M2 1 " << numRows << " double" << endl;	
	for(int r = 0; r < numRows; r++) {
 		outfile << mom2[r] << " ";
	}
	outfile << endl;
	outfile << "M3 1 " << numRows << " double" << endl;	
	for(int r = 0; r < numRows; r++) {
 		outfile << mom3[r] << " ";
	}
	outfile << endl;
	outfile << "M4 1 " << numRows << " double" << endl;	
	for(int r = 0; r < numRows; r++) {
 		outfile << mom4[r] << " ";
	}
	outfile << endl;
}

void dumpDerived(int tstep, int numRows, double *stdDev, double *var,  double *skew, double *kurt, double *sum) {
  	char fname[256];
  	sprintf(fname, "../post/descriptiveStats/time-%d_derived_intransit.vtk", tstep);
	ofstream outfile(fname);

	outfile << "# vtk DataFile Version 3.0" << endl;
	outfile << "vtk output" << endl;
	outfile << "ASCII" << endl;
	outfile << "DATASET TABLE" << endl;
	outfile << "ROW_DATA " << numRows  << endl;
	outfile << "FIELD FieldData 5" << endl;
	outfile << "StandardDeviation 1 " << numRows << " double" << endl;	
	for(int r = 0; r < numRows; r++) {
 		outfile << stdDev[r] << " ";
	}
	outfile << endl;
	outfile << "Variance 1 " << numRows << " double" << endl;	
	for(int r = 0; r < numRows; r++) {
 		outfile << var[r] << " ";
	}
	outfile << endl;
	outfile << "Skewness 1 " << numRows << " double" << endl;	
	for(int r = 0; r < numRows; r++) {
 		outfile << skew[r] << " ";
	}
	outfile << endl;
	outfile << "Kurtosis 1 " << numRows << " double" << endl;	
	for(int r = 0; r < numRows; r++) {
 		outfile << kurt[r] << " ";
	}
	outfile << endl;
	outfile << "Sum 1 " << numRows << " double" << endl;	
	for(int r = 0; r < numRows; r++) {
 		outfile << sum[r] << " ";
	}
	outfile << endl;
}
