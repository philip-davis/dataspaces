/*
*  Mehmet Aktas (2015-2016) RDI2, mfatihaktas@gmail.com
* 
*  The redistribution of the source code is subject to the terms of version 
*  2 of the GNU General Public License: http://www.gnu.org/licenses/gpl.html.
*/

#include "patch_ib_cpp.h"

namespace patch_ib {
  unsigned int hash_str(std::string str)
  {
    unsigned int h = 31; // Also prime
    const char* s_ = str.c_str();
    while (*s_) {
      h = (h * HASH_PRIME) ^ (s_[0] * HASH_PRIME_2);
      s_++;
    }
    return h; // return h % HASH_PRIME_3;
  }
  
  std::string sockaddr_in_to_str(struct sockaddr_in addr)
  {
    char lip_[INET_ADDRSTRLEN];
    TEST_Z(inet_ntop(AF_INET, &(addr.sin_addr), lip_, sizeof(lip_) ) );
    
    std::stringstream ss;
    ss << lip_ << ":" << addr.sin_port;
    
    return ss.str();
  }
};