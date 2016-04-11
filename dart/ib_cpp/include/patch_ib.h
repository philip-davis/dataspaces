/*
*  Mehmet Aktas (2015-2016) RDI2, mfatihaktas@gmail.com
* 
*  The redistribution of the source code is subject to the terms of version 
*  2 of the GNU General Public License: http://www.gnu.org/licenses/gpl.html.
*/

#ifndef _PATCH_IB_H_
#define _PATCH_IB_H_

struct ib_list_head {
  struct ib_list_head *prev_, *next_;
};

#endif // _PATCH_IB_H_
