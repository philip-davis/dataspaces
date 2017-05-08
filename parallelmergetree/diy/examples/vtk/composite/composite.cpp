//----------------------------------------------------------------------------
//
// Tom Peterka
// Argonne National Laboratory
// 9700 S. Cass Ave.
// Argonne, IL 60439
// tpeterka@mcs.anl.gov
//
// (C) 2012 by Argonne National Laboratory.
// See COPYRIGHT in top-level directory.
//
//----------------------------------------------------------------------------

//
// This example creates a polygonal model of cones and cylinders in parallel
// and composites the images using DIY
//

#include "vtkConeSource.h"
#include "vtkCylinderSource.h"
#include "vtkPolyDataMapper.h"
#include "vtkRenderWindow.h"
#include "vtkCamera.h"
#include "vtkActor.h"
#include "vtkRenderer.h"
#include "vtkRenderWindowInteractor.h"
#include "vtkProperty.h"
#include "vtkCallbackCommand.h"
#include "vtkCommand.h"
#include "vtkRendererCollection.h"

#include <mpi.h>
#include "diy.h"

// window size
#define INIT_WIN_WIDTH 300
#define INIT_WIN_HEIGHT 300
#define MAX_WIN_MPIX 8 // max window size in megapixels

// camera parameters
#define NUM_CAM_PARAMS 16 // total number of camera parameters
#define CAM_POS 0 // camera position starting index (3 values)
#define CAM_FOC 3 // focal point (3 values)
#define CAM_UP 6 // up vector (3 values)
#define CAM_CLIP 9 // near and far clip planes (2 values)
#define CAM_VIEW_ANGLE 11 // viewing angle (1 value)
#define CAM_WIN_CTR 12 // window center (2 values)
#define CAM_WIN_SIZE 14 // window size (2 values)

// near, far clipping planes
#define NEAR_FACTOR .01
#define FAR_FACTOR 500

// color and depth buffer
struct ColorDepth {
  unsigned char *color_buf;
  float *depth_buf;
} color_depth;

// function prototypes
void Master(double *bounds);
void Composite(char **items, int *gids, int num_items, int *hdr);
char *CreateItem(int *hdr);
void CreateType(void *item, DIY_Datatype *dtype, int *hdr);
void DestroyItem(void *item);
void Scene(vtkRenderer *renderer, double *bounds, bool destroy);
void HandleEvent(vtkObject* caller, long unsigned int event_id, 
		 void* client_data, void* call_data);
void SetWindowParams(vtkCamera *camera, double *cam_params);
void GetWindowParams(vtkCamera *camera, double *cam_params);
void ExportAndMerge();
void InitRenderCamera(vtkRenderer *renderer, double *bounds);

// globals
static int rank, groupsize; // MPI usual
static MPI_Comm comm = MPI_COMM_WORLD; // MPI communicator
static vtkRenderer *slave_ren; // slave window renderer
static vtkRenderWindow *slave_win; // slave window
static int cur_win_width, cur_win_height; // current window size
static vtkUnsignedCharArray *slave_pixel_data; // slave pixel data
static int did; // domain id

//----------------------------------------------------------------------------
//
int main(int argc, char **argv) {

  double cam_params[NUM_CAM_PARAMS]; // camera parameters
  double bounds[6]; // scene bounds
  int did; // domain id

  // init MPI and DIY
  int numblocks; // local number of blocks
  int unused[3] = {1, 1, 1}; // global data size, unused
  int ghost[6] = {0, 0, 0, 0, 0, 0}; // unused
  int given[3] = {0, 0, 0}; // unused
  MPI_Init(&argc, &argv);
  MPI_Comm_rank(comm, &rank);
  MPI_Comm_size(comm, &groupsize);
  int glo_numblocks = groupsize; // global number of blocks
  DIY_Init(3, unused, 1, comm);
  did = DIY_Decompose(ROUND_ROBIN_ORDER, glo_numblocks, &numblocks, 0, 
		      ghost, given);
  
  // init renderer
  slave_ren = vtkRenderer::New();

  // create the scene
  Scene(slave_ren, bounds, false);

  // init render window
  slave_win = vtkRenderWindow::New();
  slave_win->AddRenderer(slave_ren);
  slave_win->SetSize(INIT_WIN_WIDTH, INIT_WIN_HEIGHT);
  cur_win_width = INIT_WIN_WIDTH;
  cur_win_height = INIT_WIN_HEIGHT;
  color_depth.color_buf = new unsigned char[MAX_WIN_MPIX * 1048576 * 3];
  color_depth.depth_buf = new float[MAX_WIN_MPIX * 1048576];
  slave_pixel_data = vtkUnsignedCharArray::New(); // slave pixel data
//   slave_win->SetOffScreenRendering(1);

  // init slave render camera
  vtkCamera *slave_cam = slave_ren->GetActiveCamera(); // slave camera
  InitRenderCamera(slave_ren, bounds);
 
  // slave processes (excludes slave window on master process)
  if (rank < groupsize - 1) {

    bool first_time = true;
    while (1) {

      // get camera params from master and update them on the slave
      if (!first_time) {
	MPI_Bcast(cam_params, NUM_CAM_PARAMS, MPI_DOUBLE, groupsize - 1, comm);
	SetWindowParams(slave_cam, cam_params);
      }

      // redraw
      slave_win->Render();

      // composite
      ExportAndMerge();

      usleep(1000);
      first_time = false;

    } // main loop for slaves

  }

  // master process includes master window and slave window
  if (rank == groupsize - 1) {
    slave_win->Render(); // slave window
    ExportAndMerge();
    Master(bounds); // master window
  } // master

  // cleanup
  Scene(slave_ren, bounds, true);
  slave_cam->Delete();
  slave_ren->Delete();
  slave_win->Delete();
  slave_pixel_data->Delete();

  DIY_Finalize();
  MPI_Finalize();

  return 0;

}
//----------------------------------------------------------------------------
//
// init and run master window
//
void Master(double *bounds) {
 
  vtkRenderWindow *window = vtkRenderWindow::New();
  window->SetSize(INIT_WIN_WIDTH, INIT_WIN_HEIGHT);
  vtkRenderer *renderer = vtkRenderer::New(); 
  window->AddRenderer(renderer);
  vtkRenderWindowInteractor *interactor = vtkRenderWindowInteractor::New();
  interactor->SetRenderWindow(window);

  // init event handler
  vtkCallbackCommand *event_callback = vtkCallbackCommand::New();
  event_callback->SetCallback(HandleEvent);
  window->AddObserver(vtkCommand::EndEvent, event_callback);

  // turn off buffer swapping so that we can paste an image without flicker
  window->SwapBuffersOff();

  // setup camera
  InitRenderCamera(renderer, bounds);

  // run
  interactor->Initialize();
  window->SetWindowName("Result");  
  interactor->Start();

  // cleanup
  renderer->Delete();
  window->Delete();
  interactor->Delete();

}
//----------------------------------------------------------------------------
//
// master window event handler
//
void HandleEvent(vtkObject* caller, long unsigned int event_id, 
		 void* client_data, void* call_data ) {

  static double cam_params[NUM_CAM_PARAMS]; // camera parameters
  static bool first_time = true;

  // get master window size
  vtkRenderWindow *master_win = static_cast<vtkRenderWindow*>(caller);
  int *master_wsize = master_win->GetSize();
  cam_params[CAM_WIN_SIZE]     = master_wsize[0];
  cam_params[CAM_WIN_SIZE + 1] = master_wsize[1];
 
  // get master camera parameters
  vtkRendererCollection *ren_collect = master_win->GetRenderers();
  vtkRenderer *master_ren = ren_collect->GetFirstRenderer();
  vtkCamera *master_cam = master_ren->GetActiveCamera(); // master window camera
  GetWindowParams(master_cam, cam_params);

  // master broadcast camera parameters to all processes
  MPI_Bcast(cam_params, NUM_CAM_PARAMS, MPI_DOUBLE, groupsize - 1, comm);

  // set camera parameters for slave on this process
  vtkCamera *slave_camera = slave_ren->GetActiveCamera();
  SetWindowParams(slave_camera, cam_params);

  // update rendering for slave on this process
  slave_ren->GetRenderWindow()->Render();

  // export slave window image and merge images
  ExportAndMerge();

  // paste pixel data, swap buffers, and turn swapping back off again
  master_win->SetPixelData(0, 0, cur_win_width - 1, cur_win_height - 1, 
			   color_depth.color_buf, 0);
  master_win->SwapBuffersOn();
  master_win->Frame();
  master_win->SwapBuffersOff();

}
//----------------------------------------------------------------------------
//
// export images and merge them
//
void ExportAndMerge() {

  // get depth buffer and image buffer for slave window
  slave_win->SwapBuffersOff();
  slave_win->GetPixelData(0, 0, cur_win_width - 1, cur_win_height - 1, 0,
			  slave_pixel_data);
  color_depth.color_buf = slave_pixel_data->GetPointer(0);
  slave_win->GetZbufferData(0, 0, cur_win_width - 1, cur_win_height - 1,
			    color_depth.depth_buf);
  slave_win->SwapBuffersOn();

  // composite
  int rounds = 1; // direct send for now
  int kvalues[1];
  int unused;
  kvalues[0] = groupsize;
  char *p = (char *)(&color_depth);
  DIY_Merge_blocks(did, &p, NULL, rounds, kvalues, 
		   &Composite, &CreateItem, &DestroyItem, &CreateType, &unused);

}
//----------------------------------------------------------------------------
//
// callback function for compositing merged images
//
// items: pointers to input / output items, result in items[0]
//   char * is used as a generic pointers to bytes, not necessarily to strings
// gids: gloabl ids of items to be reduced
// num_items: total number of input items
// hdr: quantity information (unused)
//
void Composite(char **items, int *gids, int num_items, int *hdr) {

  unsigned char *image; // current image buffer
  float *depth; // current depth buffer
  unsigned char *image0; // first image buffer
  float *depth0; // first depth buffer
  int num_pixels = cur_win_width * cur_win_height;
  float min_depth; // minimum depth

  image0 = ((struct ColorDepth *)(items[0]))->color_buf;
  depth0 = ((struct ColorDepth *)(items[0]))->depth_buf;

  // for all pixels
  for(int i = 0; i < num_pixels; i++) {

    min_depth = depth0[i];

    // for all images
    for (int j = 0; j < num_items; j++) {

      image = ((struct ColorDepth *)(items[j]))->color_buf;
      depth = ((struct ColorDepth *)(items[j]))->depth_buf;

      if (depth[i] < min_depth) {
	min_depth = depth[i];
	image0[3 * i]     = image[3 * i];
	image0[3 * i + 1] = image[3 * i + 1];
	image0[3 * i + 2] = image[3 * i + 2];
      }

    } // for all images

  } // for all pixels

}
//----------------------------------------------------------------------------
//
// user-defined callback function for creating a received item
//
// hdr: unused
//
// side effects: allocates the item
//
// returns: pointer to the item
// char * is used as a generic pointers to bytes, not necessarily to strings
//
char *CreateItem(int *hdr) {

  int num_pixels = cur_win_width * cur_win_height;

  // since destroy function is custom defined, can allocate however I want
  struct ColorDepth *item = new struct ColorDepth;
  item->color_buf = new unsigned char[num_pixels * 3];
  item->depth_buf = new float[num_pixels];

  return (char *)item;

}
//----------------------------------------------------------------------------
//
// user-defined callback function for destroying a received item
//
// item: item to be destroyed
//
void DestroyItem(void *item) {

  struct ColorDepth *p = (struct ColorDepth *)item;
  delete[] p->color_buf;
  delete[] p->depth_buf;
  delete p;

}
//----------------------------------------------------------------------------
//
// user-defined callback function for creating a DIY datatype for the
//   received item being merged
//
// item: pointer to the item
// dtype: pointer to the datatype
// hdr: quantity information (unused)
//
// side effects: commits the datatype but DIY will cleanup datatype for you
//
void CreateType(void *item, DIY_Datatype *dtype, int *hdr) {

  struct map_block_t map[] = {
    {DIY_BYTE,  ADDR, cur_win_width * cur_win_height * 3, 
     DIY_Addr(((struct ColorDepth *)item)->color_buf) },
    {DIY_FLOAT, ADDR, cur_win_width * cur_win_height, 
     DIY_Addr(((struct ColorDepth *)item)->depth_buf) },
  };

  DIY_Create_struct_datatype(0, 2, map, dtype);

}
//----------------------------------------------------------------------------
//
// create / destroy the scene
//
void Scene(vtkRenderer *renderer, double *bounds, bool destroy) {

  static bool first_time = true;
  static vtkConeSource *cone;
  static vtkCylinderSource *cyl;
  static vtkPolyDataMapper *cone_mapper;
  static vtkPolyDataMapper *cyl_mapper;
  static vtkActor *cone_actor;
  static vtkActor *cyl_actor;

  if (first_time) {
    cone = vtkConeSource::New();
    cyl = vtkCylinderSource::New();
    cone_mapper = vtkPolyDataMapper::New();
    cyl_mapper = vtkPolyDataMapper::New();
    cone_actor = vtkActor::New();
    cyl_actor = vtkActor::New();
  }

  if (destroy) {
    cone->Delete();
    cyl->Delete();
    cone_mapper->Delete();
    cyl_mapper->Delete();
    cone_actor->Delete();
    cyl_actor->Delete();
    return;
  }

  // my rank as a fraction of the total
  float rank_frac = rank / (float)(groupsize - 1); 

  int row_width = 4; // number of cylinders in a row
  int row_pos = rank % row_width;
  float radius = 2.5 / (float)row_width;
  float cone_height = 2.0;
  float cyl_height = 8.0;
  int row = rank / row_width;
  float ofst = (row % 2 == 0 ? 0.0 : radius);

  // cone
  cone->SetHeight(cone_height);
  cone->SetRadius(radius);
  cone->SetResolution(100);
  cone->SetDirection(0.0, 1.0, 0.0);
  cone->SetCenter(ofst + row_pos * 2 * radius, cyl_height + cone_height / 2.0,
		  row * radius);

  // cylinder
  cyl->SetHeight(cyl_height);
  cyl->SetRadius(radius);
  cyl->SetResolution(100);
  cyl->SetCenter(ofst + row_pos * 2 * radius, cyl_height / 2.0, row * radius);

  // map polygonal data into graphics primitives
  cone_mapper->SetInputConnection(cone->GetOutputPort());
  cyl_mapper->SetInputConnection(cyl->GetOutputPort());

  // actor orchestrates rendering
  cone_actor->SetMapper(cone_mapper);
  cyl_actor->SetMapper(cyl_mapper);
  if (rank_frac < 0.5) { // red to green color
    cone_actor->GetProperty()->SetColor(1.0 - 2 * rank_frac, 2 * rank_frac, 
					0.0);
    cyl_actor->GetProperty()->SetColor(1.0 - 2 * rank_frac, 2 * rank_frac, 
				       0.0);
  }
  else { // green to blue color
    cone_actor->GetProperty()->SetColor(0.0, 1.0 - 2 * (rank_frac - 0.5), 
					2 * (rank_frac - 0.5));
    cyl_actor->GetProperty()->SetColor(0.0, 1.0 - 2 * (rank_frac - 0.5), 
				       2 * (rank_frac - 0.5));
  }

  // add actors to renderer and set becakground
  slave_ren->AddActor(cone_actor);
  slave_ren->AddActor(cyl_actor);
  slave_ren->SetBackground(0.1, 0.2, 0.4);

  // set bounds
  bounds[0] = radius; // x
  bounds[1] = row_width * radius * 2;
  bounds[2] = 0.0; // y
  bounds[3] = cyl_height + cone_height;
  bounds[4] = 0; // z
  bounds[5] = groupsize / row_width;

}
//----------------------------------------------------------------------------
//
// init renderer camera
//
void InitRenderCamera(vtkRenderer *renderer, double *bounds) {

  double cam_params[NUM_CAM_PARAMS]; // camera parameters

  vtkCamera *camera = renderer->GetActiveCamera();
  renderer->ResetCamera(bounds);
  double z_size = fabs(bounds[5] - bounds[4]);
  cam_params[CAM_CLIP] = z_size * NEAR_FACTOR;
  cam_params[CAM_CLIP + 1] = z_size * FAR_FACTOR;
  camera->SetClippingRange(&cam_params[CAM_CLIP]);

}
//----------------------------------------------------------------------------
//
// set window parameters
//
void SetWindowParams(vtkCamera *camera, double *cam_params) {

  // camera
  camera->SetPosition(&cam_params[CAM_POS]);
  camera->SetFocalPoint(&cam_params[CAM_FOC]);
  camera->SetViewUp(&cam_params[CAM_UP]);
  camera->SetClippingRange(&cam_params[CAM_CLIP]);
  camera->SetViewAngle(cam_params[CAM_VIEW_ANGLE]);
  camera->SetWindowCenter(cam_params[CAM_WIN_CTR], 
			  cam_params[CAM_WIN_CTR + 1]);
  // window size
  cur_win_width = cam_params[CAM_WIN_SIZE];
  cur_win_height = cam_params[CAM_WIN_SIZE + 1];
  slave_win->SetSize(cur_win_width, cur_win_height);

}
//----------------------------------------------------------------------------
//
// get window parameters
//
void GetWindowParams(vtkCamera *camera, double *cam_params) {

  // camera
  camera->GetPosition(&cam_params[CAM_POS]);
  camera->GetFocalPoint(&cam_params[CAM_FOC]);
  camera->GetViewUp(&cam_params[CAM_UP]);
  camera->GetClippingRange(&cam_params[CAM_CLIP]);
  cam_params[CAM_VIEW_ANGLE] = camera->GetViewAngle();
  camera->GetWindowCenter(cam_params[CAM_WIN_CTR], 
			  cam_params[CAM_WIN_CTR + 1]);

  // window size
  cur_win_width = cam_params[CAM_WIN_SIZE];
  cur_win_height = cam_params[CAM_WIN_SIZE + 1];

}
//----------------------------------------------------------------------------
