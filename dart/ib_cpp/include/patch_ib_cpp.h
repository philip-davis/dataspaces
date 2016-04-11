/*
*  Mehmet Aktas (2015-2016) RDI2, mfatihaktas@gmail.com
* 
*  The redistribution of the source code is subject to the terms of version 
*  2 of the GNU General Public License: http://www.gnu.org/licenses/gpl.html.
*/

#ifndef _PATCH_IB_CPP_H_
#define _PATCH_IB_CPP_H_

// To solve; deque error: expected unqualified-id before '(' token
#undef max
#undef min

// for sockaddr_in_to_str
#include <sys/socket.h>
#include <arpa/inet.h>

#include <iostream>
#include <cstdio>
#include <stdlib.h>
#include <math.h> // for log2
#include <sstream>
#include <string>
#include <cstring>
#include <utility>
#include <map>
#include <vector>
#include <deque>
#include <pthread.h>

#include "patch_ib.h"

#define str_cstr_equals(x, y) (strcmp(x.c_str(), y) == 0)
#define cstr_cstr_equals(x, y) (strcmp(x, y) == 0)

// #ifndef _TEST_MACROS_
// #define _TEST_MACROS_
#define TEST_NZ(x) if (x) {log(ERROR, #x << "failed!") exit(EXIT_FAILURE); }
#define TEST_Z(x)  if (!(x)) {log(ERROR, #x << "failed!") exit(EXIT_FAILURE); }
// #endif // _TEST_MACROS_

#define DEBUG_IB
#ifdef DEBUG_IB
// static pthread_mutex_t log_m = PTHREAD_MUTEX_INITIALIZER;

const int DEBUG = 0;
const int INFO = 1;
const int WARNING = 2;
const int ERROR = 3;

const int LOG_LEVEL = DEBUG; // ERROR;

// std::cerr
#define log(type, msg) \
  if (type >= LOG_LEVEL) \
    std::clog << #type " "<< __FILE__ << ":" << __LINE__ << "] " << __func__ << ":: " << msg << std::endl; \
  else ;
  
  // pthread_mutex_lock(&log_m); \
  // pthread_mutex_unlock(&log_m);
  
  // TEST_NZ(pthread_mutex_lock(&log_m) ) \
  // TEST_NZ(pthread_mutex_unlock(&log_m) )
#else // DEBUG_IB
  #define log(type, msg)
#endif // DEBUG_IB

#define return_if_err(x, err, a...) \
  err = x; \
  if (err) { \
    log(ERROR, #x " failed!") \
    a \
    return err; \
  }

#define return_err_if_ret_cond_flag(x, ret, cond, flag, err, a...) \
  ret = x; \
  if (ret cond flag) { \
    log(ERROR, #x " failed!") \
    a \
    return err; \
  }

#define try_n_times__return_if_err(x, err, n, a...) \
  for (int c = 0; c < n; c++) { \
    err = x; \
    if (err) { \
      if (c < n - 1) { \
        log(WARNING, #x "failed, sleeping 1 sec before trying again") \
        sleep(1); \
      } \
      else { \
        log(ERROR, #x "failed " << n << " times, no more trying!") \
        a \
        return err; \
      } \
    } \
    else \
      break; \
  }

#define try_n_times__return_err_if_ret_cond_flag(x, ret, cond, flag, err, n, a...) \
  for (int c = 0; c < n; c++) { \
    ret = x; \
    if (ret cond flag) { \
      if (c < n - 1) { \
        log(WARNING, #x "failed, sleeping 1 sec before trying again") \
        sleep(1); \
      } \
      else { \
        log(ERROR, #x "failed " << n << " times, no more trying!") \
        a \
        return err; \
      } \
    } \
    else \
      break; \
  }

#define def_arr_to_str(size, arr_, type, stream) \
for (int i = 0; i < size; i++) { \
  stream << ((type*)arr_)[i]; \
  if (i < size - 1) \
    stream << ","; \
}

namespace patch_ib {
  template <typename T>
  std::string to_str(T in) {
    std::stringstream ss;
    ss << in;
    
    return "" + ss.str();
  }
  
  static char* str_to_char_(std::string str) {
    char* char_ = (char*)malloc(str.length()*sizeof(char) );
    strcpy(char_, str.c_str() );
    return char_;
  }
  
  std::string sockaddr_in_to_str(struct sockaddr_in addr);
  
  template<typename Tk, typename Tv>
  std::string map_to_str(std::map<Tk, Tv> m) {
    std::stringstream ss;
    for (typename std::map<Tk, Tv>::iterator it = m.begin(); it != m.end(); it++)
      ss << "\t" << it->first << " : " << it->second << "\n";
    
    return ss.str();
  }
  
  template <typename T>
  std::string arr_to_str(int size, const T* arr_) {
    std::stringstream ss;
    for (int i = 0; i < size; i++) {
      ss << arr_[i];
      if (i < size - 1)
        ss << ",";
    }
    
    return ss.str();
  }
  
  template <typename Tk, typename Tv, class Compare = std::less<Tk> >
  struct map {
    private:
      typename std::map<Tk, Tv, Compare> m;
    public:
      map() {}
      ~map() {}
      
      int clear() {
        m.clear();
      }
      
      Tv& operator[](Tk k) {
        Tv& r = m[k];
        return r;
      }
      
      int del(Tk k) {
        m.erase(m.find(k) );
      }
      
      bool contains(Tk k) {
        bool r = (m.count(k) != 0);
        return r;
      }
      
      typename std::map<Tk, Tv>::iterator begin() {
        typename std::map<Tk, Tv>::iterator it = m.begin();
        return it;
      }
      
      typename std::map<Tk, Tv>::iterator end() {
        typename std::map<Tk, Tv>::iterator it = m.end();
        return it;
      }
      
      int size() {
        int r = m.size();
        return r;
      }
      
      std::string to_str() {
        std::stringstream ss;
        for (typename std::map<Tk, Tv>::iterator it = m.begin(); it != m.end(); it++)
          ss << "\t" << it->first << " : " << it->second << "\n";
        
        return ss.str();
      }
  };
  
  template <typename Tk, typename Tv, class Compare = std::less<Tk> >
  struct thread_safe_map {
    private:
      pthread_mutex_t mutex;
      typename std::map<Tk, Tv, Compare> map;
    public:
      thread_safe_map() {
        TEST_NZ(pthread_mutex_init(&mutex, NULL) )
      }
      
      ~thread_safe_map() {
        TEST_NZ(pthread_mutex_destroy(&mutex) )
      }
      
      int clear() {
        map.clear();
      }
      
      Tv& operator[](Tk k) {
        TEST_NZ(pthread_mutex_lock(&mutex) )
        Tv& r = map[k];
        TEST_NZ(pthread_mutex_unlock(&mutex) )
        
        return r;
      }
      
      int del(Tk k) {
        TEST_NZ(pthread_mutex_lock(&mutex) )
        map.erase(map.find(k) );
        TEST_NZ(pthread_mutex_unlock(&mutex) )
      }
      
      bool contains(Tk k) {
        TEST_NZ(pthread_mutex_lock(&mutex) )
        bool r = (map.count(k) != 0);
        TEST_NZ(pthread_mutex_unlock(&mutex) )
        
        return r;
      }
      
      typename std::map<Tk, Tv>::iterator begin() {
        TEST_NZ(pthread_mutex_lock(&mutex) )
        typename std::map<Tk, Tv>::iterator it = map.begin();
        TEST_NZ(pthread_mutex_unlock(&mutex) )
        
        return it;
      }
      
      typename std::map<Tk, Tv>::iterator end() {
        TEST_NZ(pthread_mutex_lock(&mutex) )
        typename std::map<Tk, Tv>::iterator it = map.end();
        TEST_NZ(pthread_mutex_unlock(&mutex) )
        
        return it;
      }
      
      int size() {
        TEST_NZ(pthread_mutex_lock(&mutex) )
        int r = map.size();
        TEST_NZ(pthread_mutex_unlock(&mutex) )
        
        return r;
      }
      
      std::string to_str() {
        std::stringstream ss;
        for (typename std::map<Tk, Tv>::iterator it = map.begin(); it != map.end(); it++)
          ss << "\t" << it->first << " : " << it->second << "\n";
        
        return ss.str();
      }
  };
  
  template <typename T, class Compare = std::less<T> >
  struct syncer {
    protected:
      thread_safe_map<T, pthread_cond_t*, Compare> point_cv_map;
      thread_safe_map<T, pthread_mutex_t*, Compare> point_m_map;
      thread_safe_map<T, int, Compare> point__num_peers_map;
      bool closed;
    public:
      syncer()
      : closed(false)
      {}
      
      ~syncer() {
        if (!closed)
          close();
      }
      
      int close() {
        for (typename std::map<T, pthread_cond_t*>::iterator it = point_cv_map.begin(); it != point_cv_map.end(); it++)
          TEST_NZ(pthread_cond_destroy(it->second) )
        for (typename std::map<T, pthread_mutex_t*>::iterator it = point_m_map.begin(); it != point_m_map.end(); it++)
          TEST_NZ(pthread_mutex_destroy(it->second) )
        // 
        log(INFO, "closed:: closed.")
      }
      
      int add_sync_point(T point, int num_peers) {
        if (point_cv_map.contains(point) ) {
          log(ERROR, "add_sync_point:: already added point; point= " << point)
          return 1;
        }
        pthread_cond_t* cv_ = new pthread_cond_t();
        TEST_NZ(pthread_cond_init(cv_, NULL) )
        point_cv_map[point] = cv_;
        
        pthread_mutex_t* m_ = new pthread_mutex_t();
        TEST_NZ(pthread_mutex_init(m_, NULL) )
        point_m_map[point] = m_;
        
        point__num_peers_map[point] = num_peers;
        
        return 0;
      }
      
      int del_sync_point(T point) {
        if (!point_cv_map.contains(point) ) {
          log(ERROR, "del_sync_point:: non-existing point= " << point)
          return 1;
        }
        point_cv_map.del(point);
        point_m_map.del(point);
        point__num_peers_map.del(point);
        
        return 0;
      }
      
      int wait(T point) {
        TEST_NZ(pthread_mutex_lock(point_m_map[point] ) )
        TEST_NZ(pthread_cond_wait(point_cv_map[point], point_m_map[point] ) )
        TEST_NZ(pthread_mutex_unlock(point_m_map[point] ) )
        
        return 0;
      }
      
      int notify(T point) {
        if (!point_cv_map.contains(point) ) {
          // log(ERROR, "notify:: non-existing point.")
          return 1;
        }
        
        int num_peers_to_wait = point__num_peers_map[point];
        --num_peers_to_wait;
        
        if (num_peers_to_wait == 0) {
          TEST_NZ(pthread_mutex_lock(point_m_map[point] ) )
          TEST_NZ(pthread_cond_signal(point_cv_map[point] ) )
          TEST_NZ(pthread_mutex_unlock(point_m_map[point] ) )
          
          return 0;
        }
        point__num_peers_map[point] = num_peers_to_wait;
        
        return 0;
      }
  };
  
  template <typename T>
  class BQueue { // Blocking
    private:
      pthread_mutex_t mutex;
      pthread_cond_t cv;
      std::deque<T> dq;
    public:
      BQueue() {
        TEST_NZ(pthread_mutex_init(&mutex, NULL) )
        TEST_NZ(pthread_cond_init(&cv, NULL) )
      }
      
      int size() {
        TEST_NZ(pthread_mutex_lock(&mutex) )
        int size = dq.size();
        TEST_NZ(pthread_mutex_unlock(&mutex) )
        
        return size;
      }
      
      void push(T const& value) {
        TEST_NZ(pthread_mutex_lock(&mutex) )
        dq.push_front(value);
        TEST_NZ(pthread_cond_signal(&cv) )
        TEST_NZ(pthread_mutex_unlock(&mutex) )
      }
      
      T pop() {
        TEST_NZ(pthread_mutex_lock(&mutex) )
        while (dq.empty() )
          TEST_NZ(pthread_cond_wait(&cv, &mutex) )
        TEST_NZ(pthread_mutex_unlock(&mutex) )
        
        TEST_NZ(pthread_mutex_lock(&mutex) )
        T rc = dq.back();
        dq.pop_back();
        TEST_NZ(pthread_mutex_unlock(&mutex) )
        
        return rc;
      }
  };
  
  // template <typename T>
  // intptr_t hash(const T* p_) {
  //   const int shift = (int)log2(1 + sizeof(T) );
  //   return (intptr_t)(p_) >> shift;
  // }
  
  #define HASH_PRIME 54059
  #define HASH_PRIME_2 76963
  #define HASH_PRIME_3 86969
  unsigned int hash_str(std::string str);
};

#ifndef _LIST_
#define _LIST_

// static patch_ib::thread_safe_map<unsigned int, pthread_mutex_t*> hash__list_m_map;

#define LIST_HEAD_INIT(name) \
  {&(name), &(name) } \
  
#define LIST_HEAD(name) \
  struct ib_list_head name = LIST_HEAD_INIT(name)

static inline void __list_add(struct ib_list_head* np_, struct ib_list_head* prev_, struct ib_list_head* next_) {
  np_->next_ = next_;
  np_->prev_ = prev_;
  prev_->next_ = np_;
  next_->prev_ = np_;
}

static inline void INIT_LIST_HEAD(std::string list_name, struct ib_list_head* head_) {
  // unsigned int hash = patch_ib::hash_str(list_name);
  // if (!hash__list_m_map.contains(hash) ) {
  //   pthread_mutex_t* m_ = new pthread_mutex_t();
  //   TEST_NZ(pthread_mutex_init(m_, NULL) )
  //   hash__list_m_map[hash] = m_;
    
  //   log(INFO, "added to hash__list_m_map list_name= " << list_name)
  // }
  
  head_->next_ = head_;
  head_->prev_ = head_;
}

// #define CHECK__HEAD_HASH__M_MAP(list_name, head_) \
//   if (!hash__list_m_map.contains(patch_ib::hash_str(list_name) ) ) { \
//     log(ERROR, "hash__list_m_map does not contain list_name= " << list_name << ", adding now.") \
//     INIT_LIST_HEAD(list_name, head_); \
//   }

/* Add element 'np_' after element 'head_'. */
static inline void list_add(std::string list_name, struct ib_list_head* np_, struct ib_list_head* head_) {
  // CHECK__HEAD_HASH__M_MAP(list_name, head_)
  // unsigned int hash = patch_ib::hash_str(list_name);
  // TEST_NZ(pthread_mutex_lock(hash__list_m_map[hash] ) )
  __list_add(np_, head_, head_->next_);
  // TEST_NZ(pthread_mutex_unlock(hash__list_m_map[hash] ) )
}

/* Add element 'np_' before element 'head_'. */
static inline void list_add_tail(std::string list_name, struct ib_list_head* np_, struct ib_list_head* head_) {
  // CHECK__HEAD_HASH__M_MAP(list_name, head_)
  // unsigned int hash = patch_ib::hash_str(list_name);
  // TEST_NZ(pthread_mutex_lock(hash__list_m_map[hash] ) )
  __list_add(np_, head_->prev_, head_);
  // TEST_NZ(pthread_mutex_unlock(hash__list_m_map[hash] ) )
}

/* Unlink element 'head_' from the list it belongs. */
static inline void list_del(std::string list_name, struct ib_list_head* head_) {
  // CHECK__HEAD_HASH__M_MAP(list_name, head_)
  // unsigned int hash = patch_ib::hash_str(list_name);
  // TEST_NZ(pthread_mutex_lock(hash__list_m_map[hash] ) )
  head_->prev_->next_ = head_->next_;
  head_->next_->prev_ = head_->prev_;
  head_->next_ = NULL;
  head_->prev_ = NULL;
  // TEST_NZ(pthread_mutex_unlock(hash__list_m_map[hash] ) )
}

static inline int list_empty(std::string list_name, struct ib_list_head* head_) {
  // CHECK__HEAD_HASH__M_MAP(list_name, head_)
  // unsigned int hash = patch_ib::hash_str(list_name);
  // TEST_NZ(pthread_mutex_lock(hash__list_m_map[hash] ) )
  int r = head_->next_ == head_;
  // TEST_NZ(pthread_mutex_unlock(hash__list_m_map[hash] ) )
  
  return r;
}

#define list_entry(ptr, type, member) \
  (type *) ((uint8_t*) ptr - offsetof(type, member) ) 

#define list_for_each(pos_, head_) \
  for (pos_ = (head_)->next_; pos_ != (head_); pos_ = pos_->next_)

#define list_for_each_safe(pos_, n, head_) \
  for (pos_ = (head_)->next_, n = pos_->next_; pos_ != (head_); \
     pos_ = n, n = pos_->next_)

#define list_for_each_entry(pos_, head_, type, member) \
  for (pos_ = list_entry((head_)->next_, type, member); \
     &(pos_->member) !=(head_); \
     pos_ = list_entry(pos_->member.next_, type, member) )

#define list_for_each_entry_safe(pos_, n, head_, type, member) \
  for (pos_ = list_entry((head_)->next_, type, member), \
      n = list_entry((pos_->member).next_, type, member); \
     &(pos_->member) != (head_); \
     pos_ = n, n = list_entry((pos_->member).next_, type, member) )

#define list_to_str(head_, type, type_str, member, stream, func_to_str) \
  type *pos_, *temp_; \
  list_for_each_entry_safe(pos_, temp_, head_, type, member) \
    stream << "> item= " << func_to_str(type_str, pos_) << "\n";

#endif // _LIST_

// For locking in functions in rpc, dc and ds
// static patch_ib::thread_safe_map<unsigned int, pthread_mutex_t*> hash__func_m_map;

#define CHECK__HASH__FUNC_M_MAP(str) \
  unsigned int hash = patch_ib::hash_str(str); \
  if (!hash__func_m_map.contains(hash) ) { \
    log(WARNING, "hash__func_m_map does not contain str= " << str << ", adding now.") \
    pthread_mutex_t* m_ = new pthread_mutex_t(); \
    TEST_NZ(pthread_mutex_init(m_, NULL) ) \
    hash__func_m_map[hash] = m_; \
  }

#define LOCK_FUNC_M(str) \
  CHECK__HASH__FUNC_M_MAP(str) \
  // log(INFO, "locking hash__func_m_map[" << str << "]...; pthread_self= " << pthread_self) \
  TEST_NZ(pthread_mutex_lock(hash__func_m_map[patch_ib::hash_str(str) ] ) ) \
  // log(INFO, "locked hash__func_m_map[" << str << "]; pthread_self= " << pthread_self)

#define UNLOCK_FUNC_M(str) \
  // log(INFO, "unlocking hash__func_m_map[" << str << "]...; pthread_self= " << pthread_self) \
  TEST_NZ(pthread_mutex_unlock(hash__func_m_map[patch_ib::hash_str(str) ] ) ) \
  // log(INFO, "unlocked hash__func_m_map[" << str << "]; pthread_self= " << pthread_self)

#endif // _PATCH_IB_CPP_H_
