typedef void WrapperMap;

#ifdef __cplusplus
extern "C" {
#endif
	WrapperMap * map_new(int i);
	void map_insert( const WrapperMap *t, const char *pred, const char *succ);
	const char* map_get_value(const WrapperMap *t, const char *pred);
	void map_delete(WrapperMap *t);
#ifdef __cplusplus
}
#endif