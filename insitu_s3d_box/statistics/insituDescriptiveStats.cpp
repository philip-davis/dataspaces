#include "vtkTable.h"
#include "vtkS3DSource.h"
#include "vtkDataObjectToTable.h"
#include "vtkTable.h"
#include "vtkTableWriter.h"
#include "vtkPDescriptiveStatistics.h"
#include "vtkMultiBlockDataSet.h"
#include "insitu_data_description.h"
#include <stdio.h>
#include <unistd.h>

vtkS3DSource* s3dSourceInsitu = 0;
vtkS3DSource* s3dSourceIntransit = 0;
vtkDataObjectToTable* tableConversionInsitu = 0;
vtkDataObjectToTable* tableConversionIntransit = 0;
vtkPDescriptiveStatistics* insituStats = 0;
vtkPDescriptiveStatistics* intransitStats = 0;
vtkTableWriter* statWriter = 0;

void descriptiveStats_init_entirely_insitu(int *myid, MPI_Comm *comm, int *xyzpes, int *global_dims, MultiFab *insitu_vars)
{
  if ( ! *myid ) printf("Initializing descriptive_stats entirely_insitu\n" );
  // Initialize Source
  s3dSourceInsitu = vtkS3DSource::New();
  s3dSourceInsitu->Initialize( insitu_vars, myid, global_dims );

  tableConversionInsitu = vtkDataObjectToTable::New();
  tableConversionInsitu->SetInputConnection( s3dSourceInsitu->GetOutputPort() );

  // Initialize insitu stats
  insituStats = vtkPDescriptiveStatistics::New();
  insituStats->SetInputConnection( tableConversionInsitu->GetOutputPort() );
  insituStats->SetLearnOption( true );
  insituStats->SetDeriveOption( true );
  insituStats->SetAssessOption( false );
  insituStats->SetTestOption( false );
  insituStats->SetCommunicator( comm );
  insituStats->SetNumProcs( xyzpes[0]*xyzpes[1]*xyzpes[2] );
  insituStats->SetInTransit(false);
  insituStats->SetMyID( *myid );
  statWriter = vtkTableWriter::New();
}

void descriptiveStats_init_partial_insitu(int *myid, MPI_Comm *comm, int *xyzpes, int *global_dims, MultiFab *insitu_vars)
{
  if ( ! *myid ) printf("Initializing descriptive_stats partial_insitu\n" );
  // Initialize Source
  s3dSourceIntransit = vtkS3DSource::New();
  s3dSourceIntransit->Initialize( insitu_vars, myid, global_dims );

  tableConversionIntransit = vtkDataObjectToTable::New();
  tableConversionIntransit->SetInputConnection( s3dSourceIntransit->GetOutputPort() );

  // Initialize insitu stats
  intransitStats = vtkPDescriptiveStatistics::New();
  intransitStats->SetInputConnection( tableConversionIntransit->GetOutputPort() );
  intransitStats->SetLearnOption( true );
  intransitStats->SetDeriveOption( true );
  intransitStats->SetAssessOption( false );
  intransitStats->SetTestOption( false );
  intransitStats->SetCommunicator( comm );
  intransitStats->SetNumProcs( xyzpes[0]*xyzpes[1]*xyzpes[2] );
  intransitStats->SetInTransit(true);
  intransitStats->SetMyID( *myid );
}

void descriptiveStats_tstep_entirely_insitu( int *myid, int tstep)
{

  if ( ! *myid ) printf("Running descriptive_stats %d entirely insitu \n", tstep );
  // Update Simulation Time
  if ( s3dSourceInsitu->IsInitialized() ) s3dSourceInsitu->UpdateSimulationTime( tstep);
  else {
    printf( "S3D source not properly initialized\n" );
    return;
  }
  
  int varIds[3] = {6, 7, 8};
  // Request Columns
  for(int j = 0; j < 3; j++)  insituStats->AddColumn( gInsituVarNames_noghost[varIds[j]]);
  insituStats->SetTimestep(tstep);
  insituStats->Update();

  if ( ! *myid ) {
    vtkMultiBlockDataSet* outDS = vtkMultiBlockDataSet::SafeDownCast( insituStats->GetOutputDataObject( 1 ) );
    // dump results.
    vtkTable* primaryOut = vtkTable::SafeDownCast( outDS->GetBlock( 0 ) );
    char primaryName[256];
    sprintf(primaryName, "../post/descriptiveStats/time-%d_primary_insitu.vtk",tstep);
    statWriter->SetFileName( primaryName );
    statWriter->SetInputData( primaryOut );
    statWriter->Write();
    vtkTable* derivedOut = vtkTable::SafeDownCast( outDS->GetBlock( 1 ) );
    char derivedName[256];
    sprintf(derivedName, "../post/descriptiveStats/time-%d_derived_insitu.vtk",tstep);
    statWriter->SetFileName( derivedName );
    statWriter->SetInputData( derivedOut );
    statWriter->Write();
  }
  return;
}

void descriptiveStats_tstep_partial_insitu( int *myid, int tstep)
{

  if ( ! *myid ) printf("Running descriptive_stats %d partial insitu\n", tstep );
  // Update Simulation Time
  if ( s3dSourceIntransit->IsInitialized() ) s3dSourceIntransit->UpdateSimulationTime( tstep);
  else {
    printf( "S3D source not properly initialized\n" );
    return;
  }

  int varIds[3] = {6, 7, 8};
  // Request Columns
  for(int j = 0; j < 3; j++) intransitStats->AddColumn( gInsituVarNames_noghost[varIds[j]]);
  intransitStats->SetTimestep(tstep);
  intransitStats->Update();

  return;
}

void descriptiveStats_finalize_entirely_insitu( int * myid )
{
  if(! *myid) printf("finalizing stats entirely insitu!\n");
  // Delete stats constructs
  statWriter->Delete();
  insituStats->Delete();
  tableConversionInsitu->Delete();

  // Delete Source 
  s3dSourceInsitu->Delete();

}

void descriptiveStats_finalize_partial_insitu( int * myid )
{
  if(! *myid) printf("finalizing stats partial insitu!\n");
  // Delete stats constructs
  intransitStats->Delete();
  tableConversionIntransit->Delete();


  // Delete Source 
  s3dSourceIntransit->Delete();
}

