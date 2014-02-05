/** file: carraysearch.cpp

    Implementing the functions defined in carraysearch.h.  This file needs
    to be compiled with C++.
*/
#include "carraysearch.h"

#include <ibis.h>	// the main FastBit header
#include <bord.h>	// ibis::bord
#include <sstream>	// std::ostringstream

#include "part.h"	// ibis::part, ibis::column, ibis::tablex
#include "query.h"	// ibis::query
#include "bundle.h"	// ibis::query::result

/// A data table for C Array Search.  This class is necessary to convert a
/// raw pointer into an in-memory data table to satisfy the structural
/// requirements for using FastBit functions to build index and perform
/// queries.  The void* returned from the index building functions are
/// actually pointers to objects of this class.
    struct FastBitQuery {
	const ibis::part *t; ///< The ibis::part this query refers to.
	ibis::query q; ///< The ibis::query object
	typedef std::map< int, void* > typeValues;
	typedef std::map< const char*, typeValues*, ibis::lessi > valList;
	/// List of values that has been selected and sent to user.
	valList vlist;

	/// For storing null-terminated strings.
	struct NullTerminatedStrings {
	    const char * * pointers; ///< The pointer passed to the caller.
	    std::vector<std::string> *values; ///< Actual string values.
	}; // NullTerminatedStrings
    };

    /// A @c FastBitResultSet holds the results of a query in memory and
    /// provides a row-oriented access mechanism for the results.
    ///@note An important limitation is that the current implementation
    /// requires all selected values to be in memory.
    struct FastBitResultSet {
	/// The ibis::query::result object to hold the results in memory.
	ibis::query::result *results;
	/// A place-holder for all the string objects.
	std::vector<std::string> strbuf;
    };

class cas_array : public ibis::bord {
public:
    cas_array(size_t, ibis::TYPE_T, void*, void*);

    using ibis::bord::buildIndexes;
    virtual int buildIndexes(const char*);
};

/// Constructor.  The incoming argument raw is an array_t object of the
/// specified type (following the convention used by ibis::bord).
cas_array::cas_array(size_t n, ibis::TYPE_T t, void *raw, void *colname = NULL) {
    nEvents = n;
    switchTime = time(0);
    {
	std::ostringstream oss;
	oss << 'T' << ibis::fileManager::iBeat();
	name_ = oss.str();
	m_name = ibis::util::strnewdup(oss.str().c_str());
    }
    {
	char abuf[32];
	ibis::util::secondsToString(switchTime, abuf);
	desc_ = "unamed in-memory data partition constructed at ";
	desc_ += abuf;
	m_desc = desc_;
    }

    std::string cname;
    {
	std::ostringstream oss;
	oss << 'C' << ibis::fileManager::iBeat();
	cname = oss.str();
    }
    //cname = *static_cast<std::string*>(colname);
    cname = (char*)colname;
    //std::cout << "-----------" << cname << "------------";
    ibis::bord::column *tmp =
	new ibis::bord::column(this, t, cname.c_str(), raw);
    columns[tmp->name()] = tmp;
    amask.set(1, n);

    LOGGER(ibis::gVerbose > 1)
	<< "Constructed a cas_array " << name_ << " with " << n << " row"
	<< (n>1?"s":"") << " and one column named " << cname;
} // ctor

/// Construct an index based on the given index specification.
int cas_array::buildIndexes(const char *opt) {
    if (columns.empty()) return -1;
    ibis::bord::column *col =
	static_cast<ibis::bord::column*>(columns.begin()->second);
    if (col == 0) return -2;

    // call the function to build the index
    struct timeval tv1, tv2;
    gettimeofday(&tv1, NULL); 
    col->loadIndex(opt);
    gettimeofday(&tv2, NULL);
/*    printf("Only loadIndex time = %f seconds\n",
             (double) (tv2.tv_usec - tv1.tv_usec)/1000000 +
             (double) (tv2.tv_sec - tv1.tv_sec));
*/    // check the index size
    if (col->indexSize() > 0) {
	printf("The Index size is=%ld\n", col->indexSize());
	LOGGER(ibis::gVerbose > 2)
	    << "cas_array(" << name_ << '.' << col->name()
	    << ") completed building an index";
	return 0;
    }
    else {
	LOGGER(ibis::gVerbose > 0)
	    << "Warning -- cas_array(" << name_ << '.' << col->name()
	    << ") failed to create an in-memory index";
	return -3;
    }
} // cas_array::buildIndexes

/// Build index for a series of values in memory.
void* fb_build_index_double(double *vals, size_t nvals, const char *opt, void *colname) {
    // arr is taken over by cas_array
    ibis::array_t<double> *arr = new ibis::array_t<double>(vals, nvals);
    cas_array *ca = new cas_array(nvals, ibis::DOUBLE, arr, colname);
    if (ca == 0) {
	delete arr;
	return ca;
    }

    (void) ca->buildIndexes(opt);
    return ca;
} // fb_build_index_double

/// Build index for a series of values in memory.
void* fb_build_index_float(float *vals, size_t nvals, const char *opt) {
    ibis::gVerbose = 10;
    ibis::array_t<float> *arr = new ibis::array_t<float>(vals, nvals);
    cas_array *ca = new cas_array(nvals, ibis::FLOAT, arr);
    if (ca == 0) {
	delete arr;
	return ca;
    }

    (void) ca->buildIndexes(opt);
    return ca;
} // fb_build_index_float

/// Build index for a series of values in memory.
void* fb_build_index_int32(int32_t *vals, size_t nvals, const char *opt) {
    ibis::array_t<int32_t> *arr = new ibis::array_t<int32_t>(vals, nvals);
    cas_array *ca = new cas_array(nvals, ibis::INT, arr);
    if (ca == 0) {
	delete arr;
	return ca;
    }

    (void) ca->buildIndexes(opt);
    return ca;
} // fb_build_index_int32

void fb_free_data(void *cas) {
    delete static_cast<cas_array*>(cas);
} // fb_free_data

/// Count the number of values satisfying the range condition defined by op
/// and bound.  The operator op must be one of "=", ">", ">=", "<" and
/// "<=".  For example, if op is '=' and bound is 5, then this function
/// counts the number of values equal to 5.  If op is '>=' and bound is
/// 5.6, this function counts the number of values >= 5.6.
int fb_count_hits(void *ca, const char *op, double bound) {
    if (ca == 0) return -1;
    const cas_array *CA = static_cast<const cas_array*>(ca);
    if (CA->nColumns() == 0) {
	LOGGER(ibis::gVerbose > 0)
	    << "Warning -- fb_count_hits has encountered an empty data "
	    "table at " << ca << ", can not proceed";
	return -2;
    }
    const ibis::column *col = CA->getColumn(static_cast<uint32_t>(0));
    if (col == 0) {
	LOGGER(ibis::gVerbose > 0)
	    << "Warning -- fb_count_hits encounted an invalid column "
	    "object in data table " << CA->ibis::part::name()
	    << ", can not proceed";
	return -3;
    }

    ibis::qExpr::COMPARE cmp = ibis::qExpr::OP_UNDEFINED;
    if (*op == '=') {
	cmp = ibis::qExpr::OP_EQ;
    }
    else if (*op == '>') {
	if (op[1] == '=')
	    cmp = ibis::qExpr::OP_GE;
	else
	    cmp = ibis::qExpr::OP_GT;
    }
    else if (*op == '<') {
	if (op[1] == '=')
	    cmp = ibis::qExpr::OP_LE;
	else
	    cmp = ibis::qExpr::OP_LT;
    }
    if (cmp == ibis::qExpr::OP_UNDEFINED) {
	LOGGER(ibis::gVerbose > 0)
	    << "Warning -- fb_count_hits can understand operation \"" << op
	    << "\", only support =, >, >=, <, and <=";
	return -4;
    }
    ibis::qContinuousRange expr(col->name(), cmp, bound);
    ibis::countQuery cq(CA);
    int ierr = cq.setWhereClause(&expr);
    if (ierr < 0) {
	LOGGER(ibis::gVerbose > 0)
	    << "Warning -- fb_count_hits failed to assign express \""
	    << expr << "\" to a query object";
	return -5;
    }

    ierr = cq.evaluate();
    if (ierr < 0) {
	LOGGER(ibis::gVerbose > 0)
	    << "Warning -- fb_count_hits failed to evaluate \""
	    << expr << "\" on data table " << CA->ibis::part::name();
	return -6;
    }

    return cq.getNumHits();
} // fb_count_hits

/// Locate the positions of all values greater than bound.  The argument
/// mids indicates the number of elements that can be stored in ids.  If
/// mids is less than the number of hits, then the first mids positions
/// (first mids smallest values of the positions) are returned.  The return
/// value is the number of values placed in ids.  This actually limits the
/// number of elements could be handled to be 2^31-1.  Given that this is
/// meant to work with in memory data only, this limitation should be fine
/// for most current applications.
int fb_find_hits(void *ca, const char *op, double bound,
	      uint32_t *ids, size_t mids) {
    if (ca == 0) return -1;
    const cas_array *CA = static_cast<const cas_array*>(ca);
    if (CA->nColumns() == 0) {
	LOGGER(ibis::gVerbose > 0)
	    << "Warning -- fb_find_hits has encountered an empty data "
	    "table at " << ca << ", can not proceed";
	return -2;
    }
    const ibis::column *col = CA->getColumn(static_cast<uint32_t>(0));
    if (col == 0) {
	LOGGER(ibis::gVerbose > 0)
	    << "Warning -- fb_find_hits encounted an invalid column "
	    "object in data table " << CA->ibis::part::name()
	    << ", can not proceed";
	return -3;
    }

    ibis::qExpr::COMPARE cmp = ibis::qExpr::OP_UNDEFINED;
    if (*op == '=') {
	cmp = ibis::qExpr::OP_EQ;
    }
    else if (*op == '>') {
	if (op[1] == '=')
	    cmp = ibis::qExpr::OP_GE;
	else
	    cmp = ibis::qExpr::OP_GT;
    }
    else if (*op == '<') {
	if (op[1] == '=')
	    cmp = ibis::qExpr::OP_LE;
	else
	    cmp = ibis::qExpr::OP_LT;
    }
    if (cmp == ibis::qExpr::OP_UNDEFINED) {
	LOGGER(ibis::gVerbose > 0)
	    << "Warning -- fb_count_hits can understand operation \"" << op
	    << "\", only support =, >, >=, <, and <=";
	return -4;
    }
    ibis::qContinuousRange expr(col->name(), cmp, bound);
    ibis::countQuery cq(CA);
    int ierr = cq.setWhereClause(&expr);
    if (ierr < 0) {
	LOGGER(ibis::gVerbose > 0)
	    << "Warning -- fb_find_hits failed to assign express \""
	    << expr << "\" to a query object";
	return -5;
    }

    ierr = cq.evaluate();
    if (ierr < 0) {
	LOGGER(ibis::gVerbose > 0)
	    << "Warning -- fb_find_hits failed to evaluate \""
	    << expr << "\" on data table " << CA->ibis::part::name();
	return -6;
    }

    const ibis::bitvector *hits = cq.getHitVector();
    ierr = 0;
    for (ibis::bitvector::indexSet is = hits->firstIndexSet();
	 is.nIndices() > 0; ++ is) {
	if (is.isRange()) {
	    int end = ierr + is.nIndices();
	    uint32_t p = is.indices()[0];
	    if (end <= ierr) {
		LOGGER(ibis::gVerbose > 0)
		    << "Warning -- fb_find_hits found too many values, "
		    "return the first " << ierr;
		return ierr;
	    }
	    while (ierr < end) {
		ids[ierr] = p;
		++ ierr;
		++ p;
	    }
	}
	else {
	    for (unsigned j = 0; j < is.nIndices(); ++ j) {
		ids[ierr] = is.indices()[j];
		if (ierr+1 < ierr) {
		    LOGGER(ibis::gVerbose > 0)
			<< "Warning -- fb_find_hits found too many values, "
			"return the first " << ierr;
		    return ierr;
		}
		++ ierr;
	    }
	}
    }
    return ierr;
} // fb_find_hits

//return a string including the result of query
int fb_build_result_set(void *ca, const char *select, const char *from, const char *where, int *size, void *qret)
{
	const cas_array *CA = static_cast<const cas_array*>(ca);
	const ibis::column *col = CA->getColumn(static_cast<uint32_t>(0));
	//printf("name is attri=%s\n", col->name());

	FastBitQueryHandle qh = new FastBitQuery;
	qh->t = CA; //TODO CA is ibis::part*???

	qh->q.setPartition(qh->t);
	qh->q.setWhereClause(where);
	qh->q.setSelectClause(select);
	qh->q.evaluate();	//do query

	if(strstr(select, "count") != NULL){	//count(var)
		int num = qh->q.getNumHits();
		*(double*)qret = num; //qh->q.getNumHits();
		*size = sizeof(double);
		if(num == 0)
			*size = 0;
	}
	else{
	ibis::array_t<double> *getret;
        getret = qh->q.getQualifiedDoubles("temp");
	*size = getret->size()*sizeof(double);
	std::copy(getret->begin(),getret->end(), (double*)qret);
	
//	int i;
//	for(i = 0; i < *size; i++);
//		*((double*)qret + i) = *(getret + i);
        delete(getret);
	}

/*	FastBitResultSetHandle ret = new FastBitResultSet;
	ret->results = new ibis::query::result(qh->q);	//store result set
	ret->strbuf.resize(qh->q.components().aggSize());	//vector to store the result...responsible for pick up one 
								//value from ret->result, store in strbuf, then print out
	//return ret;
	//std::stringstream ss;
	int ret_count = 0;
	int ncols = qh->q.components().numTerms();
	while(fastbit_result_set_next(ret) == 0){
		int i = 0;
		//ss << ret->results->getDouble(i);	//TODO get the other type
		*((double*)qret+ret_count) = ret->results->getDouble(i);
		ret_count++;
		//qret.append(ret->results->getDouble(i));
		for (i = 1; i < ncols; ++i){
			//ss << " " << ret->results->getDouble(i);
			*((double*)qret+ret_count) = ret->results->getDouble(i);
			ret_count++;
		}
			//qret.append(" " + ret->results->getDouble(i));
		//ss << " ";
		//qret.append(" ");
	}
	//static std::string qret = ss.str();
	//printf("%s\n", qret.c_str());

	*size = ret_count*sizeof(double);
*/	//printf("size is %d\n", *size);
	//return (void*)qret.c_str();
	return 0;

	/* store data in double array *//*
	if(data_type == DOUBLE){
		double* query_return = malloc(sizeof(double) * total_size);
		int idx = 0, j = 0;
		while(fastbit_result_set_next(ret) == 0){
			query_return[idx] = ret->results->getDouble(j);
			idx++;
		}
	}
	*size = sizeof(double) * idx;
	return (void*)query_return;
	*/
}
