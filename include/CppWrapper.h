typedef void WrapperMap;
typedef void OnlyMap; 
typedef void PredictMap;

#ifdef __cplusplus
extern "C" {
#endif
	WrapperMap * map_new(int i);
	void map_insert( const WrapperMap *t, const char *pred, const char *succ);
	const char* map_get_value(const WrapperMap *t, const char *pred);
	void map_delete(WrapperMap *t);

	OnlyMap * map_only(int i);
	int arr_insert( const OnlyMap *t, const char *pred, int arr);
	int arr_update( const OnlyMap *t, const char *pred, int arr);
	int arr_value(const OnlyMap *t, const char *pred);
	void arr_delete(OnlyMap *t);

	PredictMap * predict_only(int i);
	int predict_insert( const PredictMap *t, const char *pred, const char *lbub);
	const char* predict_value(const PredictMap *t, const char *pred);
	void predict_delete(PredictMap *t);
#ifdef __cplusplus
}
#endif