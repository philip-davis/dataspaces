/*
*  For experimenting while debugging under ib_cpp.
*
*  Mehmet Aktas (2015-2016) RDI2, mfatihaktas@gmail.com
* 
*  The redistribution of the source code is subject to the terms of version 
*  2 of the GNU General Public License: http://www.gnu.org/licenses/gpl.html.
*/

#include "string"
#include "iostream"
#include "stdio.h"
#include <map>
#include <getopt.h>

#include "patch_ib.h"
#include "ib_server.h"
#include "ib_client.h"

std::map<std::string, std::string> parse_opts(int argc, char** argv)
{
  std::map<std::string, std::string> opt_map;
  // 
  int c;
  
  static struct option long_options[] =
  {
    {"type", optional_argument, NULL, 0},
    {0, 0, 0, 0}
  };
  
  while (1)
  {
    int option_index = 0;
    c = getopt_long (argc, argv, "", long_options, &option_index);

    if (c == -1) // Detect the end of the options.
      break;
    
    switch (c)
    {
      case 0:
        opt_map["type"] = optarg;
        break;
      case '?':
        break; //getopt_long already printed an error message.
      default:
        break;
    }
  }
  if (optind < argc) {
    std::cout << "non-option ARGV-elements: \n";
    while (optind < argc)
      std::cout << "\t" << argv[optind++] << "\n";
  }
  // 
  std::cout << "parse_opts:: opt_map= \n" << patch_ib::map_to_str<>(opt_map);
  
  return opt_map;
}

int main(int argc , char **argv)
{
  std::string temp;
  // 
  std::map<std::string, std::string> opt_map = parse_opts(argc, argv);
  
  std::cout << "Enter \n";
  getline(std::cin, temp);
  
  return 0;
}
