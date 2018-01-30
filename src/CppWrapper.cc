#include "CppWrapper.h"
#include "MapWrap.hh"

extern "C" {

	WrapperMap * map_new(int i) {
		MapWrap *t = new MapWrap(i);

		return (WrapperMap *)t;
	}

	void map_insert(const WrapperMap *test, const char *pred, const char *succ) {
		MapWrap *t = (MapWrap*)test;
		t->mp_insert(pred, succ);
	}

	const char* map_get_value(const WrapperMap *test, const char *pred){
		MapWrap *t = (MapWrap*)test;
		return t->get_value(pred);
	}

	void map_delete(WrapperMap *test) {
		MapWrap *t = (MapWrap *)test;

		delete t;
	}

	OnlyMap * map_only(int i) {
		MapOnly *t = new MapOnly(i);

		return (OnlyMap *)t;
	}

	int arr_insert( const OnlyMap *test, const char *pred, int arr){
		MapOnly *t = (MapOnly*)test;
		t->ml_insert(pred, arr);
	}

	int arr_update( const OnlyMap *test, const char *pred, int arr){
		MapOnly *t = (MapOnly*)test;
		t->ml_update(pred, arr);
	}

	int arr_value(const OnlyMap *test, const char *pred){
		MapOnly *t = (MapOnly*)test;
		return t->ml_get_id(pred);
	}

	void arr_delete(OnlyMap *test) {
		MapOnly *t = (MapOnly *)test;

		delete t;
	}

	PredictMap * predict_only(int i) {
		PredictOnly *t = new PredictOnly(i);

		return (PredictMap *)t;
	}

	int predict_insert( const PredictMap *test, const char *pred, const char *lbub){
		PredictOnly *t = (PredictOnly*)test;
		t->pm_insert(pred, lbub);
	}

	const char* predict_value(const PredictMap *test, const char *pred){
		PredictOnly *t = (PredictOnly*)test;
		return t->pm_get_lbub(pred);
	}

	void predict_delete(PredictMap *test) {
		PredictOnly *t = (PredictOnly *)test;
		delete t;
	}
}