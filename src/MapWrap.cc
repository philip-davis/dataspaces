#include "MapWrap.hh"
#include <stdio.h>
#include<string.h>
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

	string c_pred(pred);
	string c_succ(succ);
	
	map<string, int> inner_map = cMap[c_pred];
	inner_map[c_succ]++;
	cMap[c_pred] = inner_map;
	
	/*
	map<string, map <string, int> >::iterator it;
	it=cMap.find(c_pred);
	if(it != cMap.end()){
		//pred item already exists
		printf("Map Key Found %s\n", pred);
		map<string, int> inner_map;
		inner_map = it->second;
		map<string, int>::iterator iit;
		iit=inner_map.find(c_succ);
		if(iit!=inner_map.end()){
			//succ item already exists
			//update the counter
			iit->second = iit->second+1; 
			//int new_count = iit->second+1;
			//inner_map.erase(iit);
			//inner_map.insert(pair<string, int>(c_succ, new_count));

		}else{
			//succ item doesn't exist
			inner_map.insert(pair<string, int>(c_succ, 1));

		}
		it->second = inner_map;
		//uloga("Map Inserted  key %s, inner_map %s val %d \n", c_pred, c_succ, 1);
		//cMap.erase(it);
		//cMap.insert(pair<string, map<string, int> >(c_pred, inner_map));

	}else{
		//pred item doesn't exist
		map<string, int> inner_map;
		inner_map.insert(pair<string, int>(c_succ, 1));
		cMap.insert(pair<string, map<string, int> >(c_pred, inner_map));
		printf("Map Inserted  key %s, inner_map %s val %d \n", c_pred.c_str(), c_succ.c_str(), 1);
	}
	*/

}

const char* MapWrap::get_value(const char *pred){
	string c_pred(pred);
	map<string, map<string, int> >::iterator it;
	it=cMap.find(c_pred);
	char* rstr;
	rstr = (char*) malloc (sizeof(char)*50);
	if(it!=cMap.end()){
		map<string, int> inner_map;
		inner_map = it->second;
		multimap<int, string> reverse_map = flip_map(inner_map);
		multimap<int, string>::const_reverse_iterator rit = reverse_map.rbegin();
		string return_str = rit->second;
		strcpy(rstr, return_str.c_str());
	}else{
		rstr = "0000";
	}
	return rstr;
}

MapOnly::MapOnly(int i){
	map <string, int>  mymap;
	this->cMap = mymap;
}

int MapOnly::ml_insert(const char *var, int arr_index){

	string var_name(var);
	
	map <string, int>::iterator it;
	it=cMap.find(var_name);
	if(it == cMap.end()){
		//variable doesn't exist
		cMap.insert(pair<string, int> (var_name, arr_index));
		return 0;
	}
	return 1;
		
}

int MapOnly::ml_update(const char *var, int arr_index){

	string var_name(var);
	cMap[var_name]++;
	return 1;

}


int MapOnly::ml_get_id(const char *var){
	int arr_index = 0;
	string var_name(var);
	map <string, int>::iterator it;
	it=cMap.find(var_name);
	if(it != cMap.end()){
		arr_index = it->second;
	}
	return arr_index;
}

PredictOnly::PredictOnly(int i){
	map <string, string>  mymap;
	this->cMap = mymap;
}

int PredictOnly::pm_insert(const char *var, const char *lbub){

	string var_name(var);
	string lb_ub(lbub);
	cMap[var_name] = lb_ub;
	/*
	map <string, string>::iterator it;
	it=cMap.find(var_name);
	if(it == cMap.end()){
		//variable doesn't exist
		cMap.insert(pair<string, string> (var_name, lb_ub));
		//return 0;
	}else{
		//cMap.erase(it);
		it->second = lb_ub;
		//cMap.insert(pair<string, string> (var_name, lb_ub));
	}*/
	return 1;
		

}

const char* PredictOnly::pm_get_lbub(const char *var){
	string var_name(var);
	const char* rstr;
	map <string, string>::iterator it;
	it=cMap.find(var_name);
	if(it != cMap.end()){
		//string return_str = it->second;
		//rstr = return_str.cstr();
		rstr = (it->second).c_str();
	}else{
		rstr = (char*) malloc (sizeof(char)*4);
		rstr = "0000";
	}
	return rstr;
}
