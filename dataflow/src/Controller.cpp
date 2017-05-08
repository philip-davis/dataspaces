/*
 * Controller.cpp
 *
 *  Created on: Dec 12, 2014
 *      Author: bremer5
 */

#include <unistd.h>
#include <assert.h>
#include <cstring>
#include "mpi.h"
#include <iostream>

#include "Controller.h"
#include "RelayTask.h"

//#define DEBUG_PRINTS
const int RANKID=0;
#ifdef DEBUG_PRINTS
# define PRINT(x) do { std::cerr << x << std::endl; } while (0)
# define PRINT_RANK(x) if (mRank == RANKID) \
                       {std::cerr << "Rank :: " << RANKID << " " << x << std::endl; }

#else
# define PRINT(x) do {} while (0)
# define PRINT_RANK(x) do {} while (0)
#endif

DataBlock::DataBlock(const DataBlock& block)
{
  size = block.size;
  buffer = block.buffer;
}


DataBlock DataBlock::clone() const
{
  DataBlock data_copy;
  data_copy.size = size;
  data_copy.buffer = new char[size];

  memcpy(data_copy.buffer, buffer, size);
  return data_copy;
}


Controller::TaskWrapper::TaskWrapper(const Task& t) : mTask(t)
{
  mInputs.resize(t.fanin());
  mOutputs.resize(t.fanout());
}

Controller::TaskWrapper::TaskWrapper(const TaskWrapper& t) : mTask(t.mTask),
    mInputs(t.mInputs), mOutputs(t.mOutputs)
{
}

Controller::TaskWrapper& Controller::TaskWrapper::operator=(const TaskWrapper& t)
{
  mTask = t.mTask;
  mInputs = t.mInputs;
  mOutputs = t.mOutputs;

  return *this;
}

//! Adds the new input to the task. If all inputs have arrived returns true as
//! this task can now be staged
bool Controller::TaskWrapper::addInput(TaskId source, DataBlock data)
{
  TaskId i;
  bool is_ready = true;
  bool input_added = false;
  
  mTaskReadyMutex.lock();
  for (i=0;i<mTask.fanin();i++) {
    if (mTask.incoming()[i] == source) {
      assert(mInputs[i].buffer == NULL);
      mInputs[i] = data;
      input_added = true;
    }
    if (mInputs[i].buffer == NULL)
      is_ready = false;
  }
  mTaskReadyMutex.unlock();

  if (!input_added) {
    fprintf(stderr,"Unknown sender %d in TaskWrapper::addInput for task %d\n",source,mTask.id());
    assert (false);
  }

  return is_ready;
}


Controller::Controller() : mRecvBufferSize(1024*1024*128)
{
  mId = CNULL;
  mTaskMap = NULL;
  mControllerMap = NULL;
  mCallbacks.push_back(&relay_message);
  mRank = TNULL;
}

int Controller::initialize(const TaskGraph& graph, const TaskMap* task_map, 
                           MPI_Comm comm, const ControllerMap* controller_map)
{
  mTaskMap = task_map;
  mControllerMap = controller_map;
  mComm = comm;

  //mRank = rank;
  //! First lets find our id
  MPI_Comm_rank(mComm, &mRank);
  mId = mControllerMap->controller(mRank);
 // PRINT(" mId :: " << mId);

  // Get all tasks assigned to this controller
  std::vector<Task> tasks = graph.localGraph(mId,task_map);
  std::vector<Task>::const_iterator tIt;
  std::vector<TaskId>::const_iterator it;
  std::map<int,uint32_t>::iterator mIt;
  ControllerId cId;
  int c_rank;

  // Now collect the message log
  // For all tasks
  for (tIt=tasks.begin();tIt!=tasks.end();tIt++) {
    PRINT("Rank : " << mRank << " cid : " << mId << " task id :: " << tIt->id());
    // Loop through all incoming tasks
    for (it=tIt->incoming().begin();it!=tIt->incoming().end();it++) {

      //PRINT_RANK(" Incoming: " << *it);
      // If this is an input that will come from the dataflow
      if (*it != TNULL) {
        cId = mTaskMap->controller(*it);

        // And one that comes from the outside
        if (cId != mId) {

          c_rank = mControllerMap->rank(cId);
          mIt = mMessageLog.find(c_rank);

          // If we have not seen a message from this rank before
          if (mIt == mMessageLog.end())
            mMessageLog[c_rank] = 1; // Create an entry
          else // Otherwise
            mIt->second = mIt->second+1; // just increase the count
        }
      }
    } //end-for all incoming messages

    // Finally, create the tasks wrappers
    mTasks.insert(std::pair<TaskId,TaskWrapper>(tIt->id(),TaskWrapper(*tIt)));
  } // end-for all local tasks

  return 1;
}

int Controller::registerCallback(CallbackId id, Callback func)
{
  assert (id > 0); // Callback 0 is reserved for relays

  if (mCallbacks.size() <= id)
    mCallbacks.resize(id+1);

  mCallbacks[id] = func;

  return 1;
}

//! Start the computation
int Controller::run(std::map<TaskId,DataBlock>& initial_inputs)
{
  std::map<int,uint32_t>::iterator mIt;
  std::map<TaskId,DataBlock>::iterator tIt;
  std::map<TaskId,TaskWrapper>::iterator wIt;

  int num_processes;
  //MPI_Comm_size(MPI_COMM_WORLD, &num_processes);
  MPI_Comm_size(mComm, &num_processes);

  if (num_processes > 1) {
    // First we post receives for all ranks
    for (mIt=mMessageLog.begin();mIt!=mMessageLog.end();mIt++) {
      // Post receive
      postRecv(mIt->first);
    }
  }

  // Look through all tasks to find leaf tasks that need outside inputs
  for (wIt=mTasks.begin();wIt!=mTasks.end();wIt++) {

    // For now we assume that leaf tasks only hvae a single input
    // indicated by TNULL task id
    if (wIt->second.task().incoming()[0] == TNULL) { // If this is a leaf task
      // Look for the approriate input
      tIt = initial_inputs.find(wIt->second.task().id());

      if (tIt == initial_inputs.end()) { // If you can't find one
        fprintf(stderr,"Error. Found leaf task without input. Controller %d Rank %d Task %d\n",
                mId,mRank,wIt->second.task().id());
        assert (false);
      }

      // Pass on the input using TNULL as source indicating an outside input
      wIt->second.addInput(TNULL,tIt->second);

      // Start the task (note that for now we assume that all leaf tasks
      // have only a single input)
      startTask(wIt->second);
    }
  }

  // Finally, start the while loop that tests for MPI events and
  // asynchronously starts inputs
  do {

    // Test for MPI stuff happening
    testMPI();

    // Start all recently ready tasks
    processQueuedTasks();

    // Check for completed threads and join them
    uint32_t i = 0;
    while (i < mThreads.size()) {
      if (mThreads[i]->joinable()) {// If we have found a thread that is finished
        mThreads[i]->join(); // Join it to the main thread

        std::swap(mThreads[i],mThreads.back());
        PRINT_RANK("Joining thread: " << i ); 

        delete mThreads.back();
        mThreads.pop_back();
      }
      else {
        i++;
      }
    } // end-while
  
  } while ((mFreeMessagesQ.size() != mMPIreq.size()) || !mTaskQueue.empty() || 
            !mOutgoing.empty() || !mThreads.empty()); 

  PRINT("Rank : " << mRank << "***Done with run");
  return 1;
}

int Controller::stageTask(TaskId t)
{
  mQueueMutex.lock();
  mTaskQueue.push_back(t);
  mQueueMutex.unlock();

  return 1;
}


int Controller::startTask(TaskWrapper& task)
{
  //assert (task.ready());

  std::thread* t = new std::thread(&execute,this,task);

  PRINT("Task Started:: " << task.task().id());

  mThreads.push_back(t);

  return 1;
}



TaskId* Controller::unPackMessage(char* message, DataBlock* data_block, 
                                  TaskId* source_task, uint32_t* num_tasks_msg) {

  uint32_t message_size = *(uint32_t*)(message + sizeof(uint32_t));
  *num_tasks_msg        = *(uint32_t*)(message + 2*sizeof(uint32_t));
  *source_task          = *(TaskId*)(message + 3*sizeof(uint32_t));
  TaskId* task_ids      = (TaskId*)(message + 3*sizeof(uint32_t) + sizeof(TaskId));
  char* data_ptr        = (char*)(task_ids + *num_tasks_msg);

  // Create DataBlock from message
  data_block->size = (int)message_size - 
                    (sizeof(uint32_t)*3 + sizeof(TaskId)*(1 + *num_tasks_msg));
  data_block->buffer = new char[data_block->size];
  memcpy(data_block->buffer, data_ptr, data_block->size);

  //PRINT("Rank : " << mRank << "Data Size :: " << data_block->size << \
   " Msg Size :: " << message_size << \
   " Source task :: " << *source_task << \
   " Num task :: " << *num_tasks_msg << \
   " task :: " << task_ids[0] << \
   " Data :: " << data_ptr);

  return task_ids;
}



char* Controller::packMessage(std::map<uint32_t,std::vector<TaskId> >::iterator pIt,
                              TaskId source, DataBlock data ) {

  uint32_t size = 3*sizeof(uint32_t)                  // dest rank, size, no. dest tasks
                  + sizeof(TaskId)                    // source taskId
                  + pIt->second.size()*sizeof(TaskId) // the destination tasks
                  + data.size;                        // payload

  char* msg = new char[size];
  *(uint32_t*)msg = pIt->first;
  *(uint32_t*)(msg + sizeof(uint32_t)) = size;
  char* ptr = msg + sizeof(uint32_t)*2;

  *((uint32_t*)ptr) = static_cast<uint32_t>(pIt->second.size());
  
  // Store the source taskId first
  TaskId* source_task = (TaskId*)(ptr + sizeof(uint32_t));
  *source_task = source;                            

  // Now store the dest taskIds
  TaskId* t = source_task + 1; 
  for (uint32_t i=0;i<pIt->second.size();i++)
    t[i] = pIt->second[i];

  // This memcpy is annoying but appears necessary if we want to encode multiple
  // destination task ids. Might look at this later
  memcpy(ptr + sizeof(uint32_t) + sizeof(TaskId) + 
         pIt->second.size()*sizeof(TaskId), data.buffer, data.size);

  return msg;
}


int Controller::initiateSend(TaskId source, 
                             const std::vector<TaskId>& destinations, 
                             DataBlock data)
{
  std::vector<TaskId>::const_iterator it;
  std::map<TaskId,TaskWrapper>::iterator tIt;
  std::map<uint32_t,std::vector<TaskId> > packets;
  std::map<uint32_t,std::vector<TaskId> >::iterator pIt;
  int rank;

  for (it=destinations.begin();it!=destinations.end();it++) {
    
    // if the destination task is NULL we do nothing
    if (*it == TNULL) {
      continue;
    }

    // First, we check whether the destination is a local task
    tIt = mTasks.find(*it);
    if (tIt != mTasks.end()) {// If it is a local task
      bool task_ready =  tIt->second.addInput(source,data.clone()); // Pass on the data
      if (task_ready) { // If this task is now ready to execute stage it
        stageTask(*it); // Note that we can't start the task as that would 
        // require locks on the threads
      }
    }
    else { // If this is a remote task
      //PRINT_RANK("Source task: " << source << " Dest Task: " << *it);

      // Figure out where it needs to go
      rank = mControllerMap->rank(mTaskMap->controller(*it));

      // See whether somebody else already send to the same MPI rank
      pIt = packets.find(rank);
      if (pIt==packets.end()) {
        packets[rank] = std::vector<TaskId>(1,*it);
      }
      else {
        pIt->second.push_back(*it);
      }
    }
  }

  // Assemble the messages
  for (pIt=packets.begin();pIt!=packets.end();pIt++) {

    char* msg = packMessage(pIt, source, data);
    //PRINT_RANK("Packing: Rank: " << pIt->first << " dest task :: " << pIt->second[0]);
    // Now we need to post this for sending
    // First, get the lock
    mOutgoingMutex.lock();
    mOutgoing.push_back(msg);
    mOutgoingMutex.unlock();
    //PRINT_RANK("Outgoing : " << mOutgoing.size());
  }
  
  delete[] data.buffer;
  return 1;
}

int Controller::postRecv(int32_t source_rank) {
  MPI_Request req;
  char* buffer = new char[mRecvBufferSize];

  MPI_Irecv((void*)buffer, mRecvBufferSize, MPI_BYTE, source_rank, 0,
            mComm, &req);
            //MPI_COMM_WORLD, &req);
  PRINT_RANK(" Posting recv for rank :: " << source_rank << " : Req : " << req);

  // Since only the master thread accesses the message buffer, no need to lock
  if (!mFreeMessagesQ.empty()) {
    mMessages[mFreeMessagesQ.front()] = buffer;
    mMPIreq[mFreeMessagesQ.front()] = req;
    mFreeMessagesQ.pop();
  }
  else {
    mMessages.push_back(buffer);
    mMPIreq.push_back(req);
  }

  return 1;
}


int Controller::testMPI()
{
  int index = -1;
  MPI_Status mpi_status;
  int status;
  int req_complete_flag = 0;

  //need to post receives first so do a testany first
  //then do the Isends for the new messages
  status = MPI_Testany(mMPIreq.size(), &mMPIreq[0], &index, &req_complete_flag,
                       &mpi_status);

  if (status != MPI_SUCCESS) {
    fprintf(stderr, "Error in Test any!!\n");
    assert(status != MPI_SUCCESS);
  }

  if (req_complete_flag && index != MPI_UNDEFINED) {
    assert(index >= 0);

    char* message = mMessages[index];
    uint32_t dest = (*(uint32_t*)message);
    
    if (dest == mRank) { // This is a received message

      // Unpack the message
      DataBlock data_block;
      TaskId* task_ids;
      uint32_t num_tasks_msg;
      TaskId source_task=0;

      task_ids = unPackMessage(message, &data_block, &source_task, &num_tasks_msg);

      // Add DataBlock to TaskWrapper
      std::map<TaskId,TaskWrapper>::iterator wIt;

      // We make copies of the data block and give it to the tasks. Tasks are
      // responsible for destroying the block after use and can modify
      // the data blocks
      bool task_ready = false;
      for (int i=0; i< num_tasks_msg; i++) {
        wIt = mTasks.find(task_ids[i]);

        // We make copies for all the tasks that need this data except for the
        // last one to whom we pass the original data block
        if (i < num_tasks_msg-1)
          task_ready = wIt->second.addInput(source_task, data_block.clone());
        else
          task_ready = wIt->second.addInput(source_task, data_block);

        if (task_ready) {
          stageTask(wIt->first);
        }
      }

      //PRINT("Rank " << mRank << " Message recved!");

      // Update the message log and check for more messages from this rank
      std::map<int,uint32_t>::iterator mIt;
      mIt = mMessageLog.find(mpi_status.MPI_SOURCE);
      mIt->second = mIt->second - 1; // Update the number of messages 
      // to be received from this rank

      // If more messages to be received from this rank we will post recv
      if (mIt->second > 0)
        postRecv(mIt->first);

      if (mIt->second == 0) 
        mMessageLog.erase(mIt->first);
    }
    //else { // This is a completed send message

      // Delete the completed send message
      delete[] mMessages[index];
      mFreeMessagesQ.push(index);
      mMPIreq[index] = MPI_REQUEST_NULL;
      //PRINT("Rank " << mRank << " Send complete... Deleting message!");
    //}
  }
  // Now post the Isends
  mOutgoingMutex.lock();
  for (int i=0; i < mOutgoing.size(); i++) {

    MPI_Request req;
    // Get the dest rank and size from the message header
    
    uint32_t destination = *(uint32_t*)mOutgoing[i];
    uint32_t size = *(uint32_t*)(mOutgoing[i] + sizeof(uint32_t));
    MPI_Isend((void*)mOutgoing[i], size, MPI_BYTE, destination, 0, 
              mComm, &req);
              //MPI_COMM_WORLD, &req);
    PRINT_RANK(" Sending to " << destination << " : Req : " << req);

    if (!mFreeMessagesQ.empty()) {
      mMessages[mFreeMessagesQ.front()] = (char*)mOutgoing[i];
      mMPIreq[mFreeMessagesQ.front()] = req;
      mFreeMessagesQ.pop();
    }
    else {
      mMessages.push_back((char*)mOutgoing[i]);
      mMPIreq.push_back(req);
    }
  }
  
  if (!mOutgoing.empty())
    mOutgoing.clear();
  
  mOutgoingMutex.unlock();
  return 1;
}


int Controller::processQueuedTasks()
{
  std::map<TaskId,TaskWrapper>::iterator mIt;

  mQueueMutex.lock();
  while (!mTaskQueue.empty()) {
    mIt = mTasks.find(mTaskQueue.front());
    startTask(mIt->second);
    mTaskQueue.pop_front();
  }
  mQueueMutex.unlock();

  return 1;
}

int execute(Controller *c,Controller::TaskWrapper task)
{
  assert (task.task().callback() < c->mCallbacks.size());

  // Call the appropriate callback
  c->mCallbacks[task.task().callback()](task.mInputs,task.mOutputs,task.task().id());

  // Now we "own" all the BlockData for the outputs and we must send
  // them onward. So for all outputs we start a send
  for (uint32_t i=0;i<task.task().fanout();i++) {
    //PRINT("Task : " << task.task().id() << " sending output");
    c->initiateSend(task.task().id(),task.task().outgoing(i),task.mOutputs[i]);
  }

  return 1;
}



