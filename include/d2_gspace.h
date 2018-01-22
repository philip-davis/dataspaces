#ifndef __D2SPACES_GSPACE__
#define __D2SPACES_GSPACE__

enum dspace_rpc_type {
	ss_obj_get_desc = 1,
	ss_obj_get_dht_peers,
	ss_obj_get,
	ss_obj_put,
	ss_obj_update,
	ss_obj_filter,
	ss_obj_cq_register,
	ss_obj_cq_notify,
	ss_obj_info,
	ss_obj_query,
	ss_info,
	ss_code_reply,
	ss_code_put,
	cp_lock,
	cp_remove,
	cn_timing,
	CN_TIMING_AVG
};

#endif /* __D2SPACES_GSPACE__ */
