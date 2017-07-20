#include <iostream>
#include <map>
#include <string>
#include <algorithm>

class MapWrap {
        public:
                void mp_insert(const char *pred, const char *succ);
                const char* get_value(const char *pred);
                MapWrap(int i);

        private:
                 std::map <std::string, std::map<std::string, int> > cMap;
};