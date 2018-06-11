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

class MapOnly {
        public:
                int ml_insert(const char *pred, int i);
                int ml_update(const char *pred, int i);
                int ml_get_id(const char *pred);
                MapOnly(int i);

        private:
                 std::map <std::string, int> cMap;
};

class PredictOnly {
        public:
                int pm_insert(const char *pred, const char *lbub);
                const char* pm_get_lbub(const char *pred);
                PredictOnly(int i);

        private:
                 std::map <std::string, std::string> cMap;
};