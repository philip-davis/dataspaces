#include "vtkS3DSource.h"

#include "vtkPointData.h"
#include "vtkDoubleArray.h"
#include "vtkImageData.h"
#include "vtkInformation.h"
#include "vtkInformationVector.h"
#include "vtkStreamingDemandDrivenPipeline.h"
#include "vtkInformationIntegerVectorKey.h"
#include "insitu_data_description.h"

vtkCxxRevisionMacro(vtkS3DSource,"$Revision:$");

vtkS3DSource* vtkS3DSource::Singleton = 0;

vtkS3DSource* vtkS3DSource::New()
{
  if ( vtkS3DSource::Singleton )
    {
    return vtkS3DSource::Singleton;
    }

  // This sets vtkS3DSource::Singleton:
  vtkS3DSource* src = new vtkS3DSource;
  // Add an extra reference so the singleton isn't destroyed
  // until vtkS3DSource::DestroySingleton() is called:
  src->Register( 0 );
  return src;
}

vtkS3DSource::vtkS3DSource()
{
  this->Initialized = 0;
  this->SimulationData = vtkImageData::New();
  this->SetNumberOfInputPorts( 0 );
  this->SetNumberOfOutputPorts( 2 );
  // Port 0: vtkImageData
  // Port 1: vtkTable

  vtkS3DSource::Singleton = this;
}

vtkS3DSource::~vtkS3DSource()
{
  vtkS3DSource::Singleton = 0;

  this->SimulationData->Delete();
}

void vtkS3DSource::DestroySingleton()
{
  if ( vtkS3DSource::Singleton )
    {
    int refCount = vtkS3DSource::Singleton->GetReferenceCount();
    if ( refCount > 1 )
      {
      printf( "Attempting to destroy the S3D singleton while there are outstanding references.\n Destruction will be delayed until their release.\n" );
      }
    vtkS3DSource::Singleton->Delete();
    }
}

void vtkS3DSource::PrintSelf( ostream& os, vtkIndent indent )
{
  this->Superclass::PrintSelf( os, indent );
  os << "Initialized: " << this->Initialized << "\n";
  os << "SimulationData: " << this->SimulationData << "\n";
  os << "SimulationTime: " << this->SimulationTime << "\n";
  os << "[Singleton]: " << vtkS3DSource::Singleton << "\n";
}


// Description:
// Prepare the singleton using data from the simulation.
void vtkS3DSource::Initialize(MultiFab *insitu_vars, int *myid, int *global_dim) 
{
  if ( this->Initialized )
    {
    return;
    }
  // FIXME: Really, we may need to present the simulation as either
  // a rectilinear grid or image data depending on whether the
  // point coordinates are evenly spaced or not.
  // One big gotcha is that there are no volume rendering algorithms
  // for vtkRectilinearGrid so if the spacing is anywhere close to
  // regular, it might be desirable to approximate the grid with
  // image data.

  
  // JCB Extract relevant info from MultiFab

  double *dataPtr=NULL;
  Box domain;
  for( MFIter mfi(*(insitu_vars)); mfi.isValid(); ++mfi){
    BL_ASSERT( data == NULL );
    dataPtr = ((*insitu_vars)[mfi]).dataPtr();
    domain = ((*insitu_vars)[mfi]).box();
  }

  BL_ASSERT( dataPtr != NULL );
  int numInsituVars = insitu_vars->nComp();

  if(numInsituVars != gNumInsituVars_noghost) printf("blar -- fix me!\n");

  int nx = (domain.bigEnd(0)-domain.smallEnd(0))+1; 
  int ny = (domain.bigEnd(1)-domain.smallEnd(1))+1; 
  int nz = (domain.bigEnd(2)-domain.smallEnd(2))+1;
  printf("%d, x [%d %d]  %d, y [%d %d] %d, z [%d %d] %d\n ",*myid, domain.smallEnd(0), domain.bigEnd(0), nx, domain.smallEnd(1), domain.bigEnd(1), ny, domain.smallEnd(2), domain.bigEnd(2), nz); 

  vtkImageData* sim = this->SimulationData;
  vtkInformation* simInfo = sim->GetInformation();
  vtkPointData *pdata = sim->GetPointData();
  int extent[6] = { 0, 0, 0, 0, 0, 0 };
  double origin[3] = { 0., 0., 0. };
  double spacing[3] = { 1., 1., 1. };

  vtkIdType npts = nx * ny * nz;
  
  extent[1] = nx - 1;
  extent[3] = ny - 1;
  extent[5] = nz - 1;

  // Reset the image data:
  sim->Initialize();
  // FIXME: need whole extent but S3d doesn't provide them.
  simInfo->Set( vtkStreamingDemandDrivenPipeline::WHOLE_EXTENT(), extent, 6 );
  sim->SetExtent( extent );
  sim->SetOrigin( origin );
  sim->SetSpacing( spacing );

  for ( int i = 0; i < numInsituVars; ++ i )
    {
    vtkDoubleArray* arr = vtkDoubleArray::New();
    arr->SetNumberOfComponents( 1 );
    arr->SetArray( dataPtr + npts * i, npts, 1 );
    arr->SetName( gInsituVarNames_noghost[i] );
    pdata->AddArray( arr );
    if ( i == 0 )
      {
      pdata->SetScalars( 0 );
      }
    arr->Delete();
    }
  this->Initialized = 1;
}

void vtkS3DSource::UpdateSimulationTime( int time )
{
  // Unlike SetXXX() methods, this always marks the dataset as modified.
  // This is necessary since the simulation will have changed values in
  // the underlying arrays without VTK being involved, making a recompute
  // of the pipeline necessary.
  this->SimulationTime = time;
  this->Modified();
}

int vtkS3DSource::FillOutputPortInformation( int port, vtkInformation* info )
{
  switch ( port )
    {
  case 0:
    info->Set( vtkDataObject::DATA_TYPE_NAME(), "vtkImageData" );
    break;
  case 1:
    info->Set( vtkDataObject::DATA_TYPE_NAME(), "vtkTable" );
    break;
  default:
    return 0;
    }

  return 1;
}

int vtkS3DSource::RequestInformation( vtkInformation* request, vtkInformationVector** inInfoVec, vtkInformationVector* ouInfoVec )
{
  vtkInformation* ouInfo0 = ouInfoVec->GetInformationObject( 0 );
  vtkImageData* sim = this->SimulationData;
  if ( sim )
    {
    vtkInformation* simInfo = sim->GetInformation();
    vtkStreamingDemandDrivenPipeline::WHOLE_EXTENT()->ShallowCopy( simInfo, ouInfo0 );
    ouInfo0->Set( vtkDataObject::ORIGIN(), sim->GetOrigin(), 3 );
    ouInfo0->Set( vtkDataObject::SPACING(), sim->GetSpacing(), 3 );

    // FIXME: Assumes the active scalars will have a single component:
    vtkDataObject::SetPointDataActiveScalarInfo( ouInfo0, VTK_DOUBLE, 1 );
    }
  else
    {
    int wholeExtent[6] = { 0, 0, 0, 0, 0, 0 };
    double origin[3] = { 0., 0., 0. };
    double spacing[3] = { 1., 1., 1. };
    ouInfo0->Set( vtkStreamingDemandDrivenPipeline::WHOLE_EXTENT(), wholeExtent, 6 );
    ouInfo0->Set( vtkDataObject::ORIGIN(), origin, 3 );
    ouInfo0->Set( vtkDataObject::SPACING(), spacing, 3 );
    }
  return 1;
}

int vtkS3DSource::RequestUpdateExtent( vtkInformation* request, vtkInformationVector** inInfoVec, vtkInformationVector* ouInfoVec )
{
  return 1;
}

int vtkS3DSource::RequestData( vtkInformation* request, vtkInformationVector** inInfoVec, vtkInformationVector* ouInfoVec )
{
  vtkImageData* sim = this->SimulationData;
  if ( sim )
    {
    vtkInformation* ouInfo0 = ouInfoVec->GetInformationObject( 0 );
    if ( ! ouInfo0 )
      {
      return 0;
      }

    vtkDataObject* ouData0 = ouInfo0->Get( vtkDataObject::DATA_OBJECT() );
    if ( ouData0 )
      {
      ouData0->ShallowCopy( sim );
      }
    }
  return 1;
}

