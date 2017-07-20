#include "MapWrap.hh"
using namespace std;

template <typename A, typename B>
multimap<B, A> flip_map(map<A,B> & src) {

    multimap<B,A> dst;
    typename map<A, B>::const_iterator itrs;
    for(itrs= src.begin(); itrs != src.end(); ++itrs)
        dst.insert(pair<B, A>(itrs -> second, itrs -> first));

    return dst;
}

MapWrap::MapWrap(int i){
	map <string, map <string, int> > mymap;
	this->cMap = mymap;
}

void MapWrap::mp_insert(const char *pred, const char *succ){

	const string c_pred(pred);
	const string c_succ(succ);
	map<string, map <string, int> >::iterator it;
	it=cMap.find(c_pred);
	if(it != cMap.end()){
		//pred item already exists
		map<string, int> inner_map;
		inner_map = it->second;
		map<string, int>::iterator iit;
		iit=inner_map.find(c_succ);
		if(iit!=inner_map.end()){
			//succ item already exists
			//update the counter
			int new_count = iit->second+1;
			inner_map.erase(iit);
			inner_map.insert(pair<string, int>(c_succ, new_count));

		}else{
			//succ item doesn't exist
			inner_map.insert(pair<string, int>(c_succ, 1));

		}
		cMap.erase(it);
		cMap.insert(pair<string, map<string, int> >(c_pred, inner_map));

	}else{
		//pred item doesn't exist
		map<string, int> inner_map;
		inner_map.insert(pair<string, int>(c_succ, 1));
		cMap.insert(pair<string, map<string, int> >(c_pred, inner_map));

	}

}

const char* MapWrap::get_value(const char *pred){
	string c_pred(pred);
	map<string, map<string, int> >::iterator it;
	it=cMap.find(c_pred);
	const char* rstr = "0000";
	if(it!=cMap.end()){
		map<string, int> inner_map;
		inner_map = it->second;
		multimap<int, string> reverse_map = flip_map(inner_map);
		multimap<int, string>::const_reverse_iterator rit = reverse_map.rbegin();
		string return_str = rit->second;
		rstr = return_str.c_str();
	}
	return rstr;
}
