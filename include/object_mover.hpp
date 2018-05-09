#ifndef __OBJECT_MOVER_HPP
#define __OBJECT_MOVER_HPP

#include <boost/asio/io_service.hpp>
#include <boost/thread.hpp>

#include <iostream>
#include <fstream>

#include <rados/librados.hpp>

//#define USE_MICRO_TIERING

class SessionPool;

class Session {
public:
  enum Tier {
    CACHE,
    STORAGE,
    ARCHIVE,
  };
  Session(SessionPool* session_pool, const std::string &ceph_conf_file);
  ~Session();
  void Connect();
  void Reconnect();
  int AioOperate(Tier tier, const std::string& oid, librados::ObjectWriteOperation *op, int flags = 0);
  librados::Rados cluster_;
  librados::IoCtx io_ctx_cache_;
  librados::IoCtx io_ctx_storage_;
  librados::IoCtx io_ctx_archive_;
private:
  SessionPool* session_pool_;
  std::string ceph_conf_file_;
};

class SessionPool {
public:
  SessionPool(const std::string &ceph_conf_file, int session_pool_size) {
    for (int i = 0; i < session_pool_size; i++) {
      pool_.insert(std::make_pair(new Session(this, ceph_conf_file), true));
    }
  }
  ~SessionPool() {
    for (std::map<Session*, bool>::iterator it = pool_.begin(); it != pool_.end(); it++) {
      // delete it->first;
    }
  }
  void ReserveSession(boost::thread::id id) {
    boost::mutex::scoped_lock l(lock_);
    for (std::map<Session*, bool>::iterator it = pool_.begin(); it != pool_.end(); it++) {
      if (it->second) {
  it->second = false;
  reserve_map_[id] = it->first;
  return;
      }
    }
    abort();
  }
  Session* GetSession(boost::thread::id id) {
    boost::mutex::scoped_lock l(lock_);
    return reserve_map_[id];
  }
private:
  boost::mutex lock_;
  std::map<Session*, bool> pool_;
  std::map<boost::thread::id, Session*> reserve_map_;
};

/**
 * Wrapper functions for moving Ceph objects across tiers
 */
class ObjectMover {
public:
  enum Tier {
    FAST = 0,
    SLOW,
    ARCHIVE,
  };
  /**
   * Constructor
   *
   * @param[in] thread_pool_size numbuer of threads that execute asynchronous I/Os
   */
  ObjectMover(const std::string &ceph_conf_file, int thread_pool_size = 32, const std::string &trace_filename = "");
  ~ObjectMover();
  /**
   * Create an object in the specified tier
   *
   * @param[in] tier the tier in which to create the object
   * @param[in] object_name the name of the object
   * @param[in] value the data of the object
   * @param[out] err  0 success
   * @param[out] err <0 failure
   */
  void Create(Tier tier, const std::string &object_name, const librados::bufferlist &bl, int *err, unsigned long tid);
  void CreateAsync(Tier tier, const std::string &object_name, const librados::bufferlist &bl, int *err) {
    unsigned long tid = task_manager_->GetTid();
    auto f = std::bind(&ObjectMover::Create, this, tier, object_name, bl, err, tid);
    task_manager_->StartTask(tid, object_name, "w", TierToString(tier), f);
    ios_.post(f);
  }
  /**
   * Move an object to the specified tier
   *
   * @param[in] tier the tier to which to move the object
   * @param[in] object_name the name of the object
   * @param[out] err  0 success
   * @param[out] err <0 failure
   */
  void Move(Tier tier, const std::string &object_name, int *err, unsigned long tid);
  void MoveAsync(Tier tier, const std::string &object_name, int *err) {
    unsigned long tid = task_manager_->GetTid();
    auto f = std::bind(&ObjectMover::Move, this, tier, object_name, err, tid);
    task_manager_->StartTask(tid, object_name, "m", TierToString(tier), f);
    ios_.post(f);
  }
  /**
   * Read an object
   *
   * @param[in] object_name the name of the object
   * @param[out] err  0 success
   * @param[out] err <0 failure
   */
  void CRead(const std::string &object_name, char *buf, uint64_t off, size_t len, int *err, unsigned long tid);
  void CReadAsync(const std::string &object_name, char *buf, uint64_t off, size_t len, int *err) {
    unsigned long tid = task_manager_->GetTid();
    auto f = std::bind(&ObjectMover::CRead, this, object_name, buf, off, len, err, tid);
    task_manager_->StartTask(tid, object_name, "r", "-", f);
    ios_.post(f);
  }
  void Read(const std::string &object_name, librados::bufferlist *bl, uint64_t off, int *err, unsigned long tid);
  void ReadAsync(const std::string &object_name, librados::bufferlist *bl, uint64_t off, int *err) {
    unsigned long tid = task_manager_->GetTid();
    auto f = std::bind(&ObjectMover::Read, this, object_name, bl, off, err, tid);
    task_manager_->StartTask(tid, object_name, "r", "-", f);
    ios_.post(f);
  }
  /**
   * Delete an object
   *
   * @param[in] object_name the name of the object
   * @param[out] err  0 success
   * @param[out] err <0 failure
   */
  void Delete(const std::string &object_name, int *err, unsigned long tid);
  void DeleteAsync(const std::string &object_name, int *err) {
    unsigned long tid = task_manager_->GetTid();
    auto f = std::bind(&ObjectMover::Delete, this, object_name, err, tid);
    task_manager_->StartTask(tid, object_name, "d", "-", f);
    ios_.post(f);
  }

  /**
   * Get the Location of the object
   *
   * @param object_name the name of the object
   * @retval err  0 success
   * @retval err <0 failure
   */
  int GetLocation(const std::string &object_name);
private:
  class TaskManager {
  public:
    struct TaskInfo {
      std::string oid;
      std::string mode;
      std::string tier;
      boost::function<void ()> task;
      unsigned long start;
    };
    TaskManager(const std::string &trace_filename, ObjectMover *om) : tid_counter_(0), flag_(false), om_(om) {
      if (!trace_filename.empty()) {
  trace_.open(trace_filename);
      }
    }
    ~TaskManager() {
      if (trace_.is_open()) {
  trace_.close();
      }
    }
    unsigned long GetCurrentTime() {
      struct timeval tv;
      ::gettimeofday(&tv, NULL);
      return tv.tv_sec * 1000 + tv.tv_usec / 1000;
    }
    void StartTask(unsigned long tid,
       const std::string &oid,
       const std::string &mode,
       const std::string &tier,
       boost::function<void ()> task) {
      boost::mutex::scoped_lock l(lock_);
      unsigned long now = GetCurrentTime();
      assert(task_table_.count(tid) == 0);
      TaskInfo t = {oid, mode, tier, task, now};
      task_table_[tid] = t;
    }
    bool FinishTask(unsigned long tid) {
      boost::mutex::scoped_lock l(lock_);
      unsigned long now = GetCurrentTime();
      if (task_table_.count(tid) == 1) {
  TaskInfo t = task_table_[tid];
  if (trace_.is_open()) {
    trace_ << t.oid << "," << t.mode << "," << t.tier << ",s," << t.start << std::endl;
    trace_ << t.oid << "," << t.mode << "," << t.tier << ",f," << now << std::endl;
  }
  task_table_.erase(tid);
  return true;
      } else {
  return false;
      }
    }
    void WatchTasks() {
      while (true) {
  std::multiset<unsigned long> latencies;
  {
    boost::mutex::scoped_lock l(lock_);
    unsigned long now = GetCurrentTime();
    std::map<unsigned long, TaskInfo>::iterator it;
    for (it = task_table_.begin(); it != task_table_.end(); it++) {
      TaskInfo t = it->second;
      assert(now >= t.start);
      latencies.insert(now - t.start);
      if (now - t.start > 10*1000) {
        // retry
        t.start = now;
        it->second = t;
        om_->ios_.post(t.task);
      }
    }
  }
  int index = 0;
  unsigned long median;
  std::vector<unsigned long> l;
  std::multiset<unsigned long>::reverse_iterator rit = latencies.rbegin();
  while (rit != latencies.rend()) {
    if (index < 5) {
      l.push_back(*rit);
    }
    if (index == 5) {
      median = *rit;
      break;
    }
    rit++;
    index++;
  }
  if (l.size() == 5) {
    std::cout << "latencies [s] ";
    std::cout << "median = ";
    std::cout << std::setw(4) << std::right << median/1000 << " ";
    std::cout << "max = [" ;
    std::cout << std::setw(4) << std::right << l[0]/1000 << " ";
    std::cout << std::setw(4) << std::right << l[1]/1000 << " ";
    std::cout << std::setw(4) << std::right << l[2]/1000 << " ";
    std::cout << std::setw(4) << std::right << l[3]/1000 << " ";
    std::cout << std::setw(4) << std::right << l[4]/1000 << " ";
    std::cout << "]" << std::endl;
  }
  if (flag_) {
    return;
  }
  sleep(1);
      }
    }
    unsigned long GetTid() {
      boost::mutex::scoped_lock l(lock_);
      unsigned long tid = tid_counter_;
      tid_counter_++;
      return tid;
    }
    void End() {
      flag_ = true;
    }
  private:
    boost::mutex lock_;
    unsigned long tid_counter_;
    std::map<unsigned long, TaskInfo> task_table_;
    std::ofstream trace_;
    ObjectMover *om_;
    bool flag_;
  };
  /**
   *
   */
  SessionPool* session_pool_;
  /**
   *
   */
  TaskManager* task_manager_;
  /**
   * Lock advisory lock
   *
   * @param object_name the name of the object
   */
  void Lock(const std::string &object_name);
  /**
   * Unlock advisory lock
   *
   * @param object_name the name of the object
   */
  void Unlock(const std::string &object_name);
  /**/
  boost::asio::io_service ios_;
  boost::asio::io_service::work *w_;
  boost::thread_group thr_grp_;
  /**/
  std::string TierToString(Tier tier) {
    switch(tier) {
    case FAST:
      return "f";
    case SLOW:
      return "s";
    case ARCHIVE:
      return "a";
    default:
      abort();
    }
  }
  /**/
  std::ofstream trace_;
  boost::mutex lock_;
};

#endif