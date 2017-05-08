#ifndef __vtkS3DSource_h
#define __vtkS3DSource_h

#include "vtkImageAlgorithm.h"
#include <MultiFab.H>

class VTK_EXPORT vtkS3DSource : public vtkImageAlgorithm
{
public:
  static vtkS3DSource* New();
  vtkTypeRevisionMacro(vtkS3DSource,vtkImageAlgorithm);
  virtual void PrintSelf( ostream& os, vtkIndent indent );

  // Description:
  // Return a pointer to the S3D data object.
  // This may return NULL, while New() may not.
  static vtkS3DSource* GetSingleton() { return vtkS3DSource::Singleton; }

  // Description:
  // This will destroy the source by releasing an additional reference on the singleton when it was created.
  // An error message will be reported if the reference count on the singleton is non-zero after this
  // supposedly-final reference is released.
  static void DestroySingleton();

  // Description:
  // Prepare the singleton using data from the simulation.
  virtual void Initialize( MultiFab *insitu_vars, int *myid, int *global_dims);

  // Description:
  // Has Initialize() been called at least once?
  virtual int IsInitialized() { return this->Initialized != 0; }

  // Description:
  // Update the time associated with the current output of the vtkS3DSource filter.
  // This will cause a re-execution of the entire downstream pipeline when Update() is called.
  virtual void UpdateSimulationTime( int time );

protected:
  vtkS3DSource();
  virtual ~vtkS3DSource();

  virtual int FillOutputPortInformation( int port, vtkInformation* info );

  virtual int RequestInformation( vtkInformation* request, vtkInformationVector** inInfoVec, vtkInformationVector* ouInfoVec );
  virtual int RequestUpdateExtent( vtkInformation* request, vtkInformationVector** inInfoVec, vtkInformationVector* ouInfoVec );
  virtual int RequestData( vtkInformation* request, vtkInformationVector** inInfo, vtkInformationVector* ouInfo );

  vtkImageData* SimulationData; // Container pointing to arrays owned by the simulation.
  int SimulationTime;
  int Initialized;

private:
  vtkS3DSource( const vtkS3DSource& ); // Not implemented.
  void operator = ( const vtkS3DSource& ); // Not implemented.

  static vtkS3DSource* Singleton;
};

#endif // __vtkS3DSource_h
