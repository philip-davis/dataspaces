#include "carraysearch.h"

#include <ibis.h>       // the main FastBit header
#include <bord.h>       // ibis::bord
#include <sstream>      // std::ostringstream

#include "part.h"       // ibis::part, ibis::column, ibis::tablex
#include "query.h"      // ibis::query
#include "bundle.h"     // ibis::query::result

//ibis::gVerbose = 3;

/// A data table for C Array Search.  This class is necessary to convert a
/// raw pointer into an in-memory data table to satisfy the structural
/// requirements for using FastBit functions to build index and perform
/// queries.  The void* returned from the index building functions are
/// actually pointers to objects of this class.
class cas_array : public ibis::bord {
public:
	cas_array(int, size_t, ibis::TYPE_T*, void**, void**);
	using ibis::bord::buildIndexes;	
	virtual int buildIndexes(const char*);
};

/// Constructor.  The incoming argument raw is an array_t object of the
/// specified type (following the convention used by ibis::bord).
cas_array::cas_array(int numcols, size_t n, ibis::TYPE_T *t, void **raw, void **colname=NULL) {
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

	std::string *cname = new std::string[numcols];
	for(int i=0; i<numcols; i++){
        	cname[i] = (char*)colname[i];
        	//std::cout << "-----------" << cname[i].c_str() << "------------" << "\n";
    	}

    	for(int i=0; i<numcols; i++){
    		ibis::bord::column *tmp =
        		new ibis::bord::column(this, *(t+i), cname[i].c_str(), *(raw+i));
    		columns[tmp->name()] = tmp;
    		amask.set(1, n);
    	}
    
	LOGGER(ibis::gVerbose > 1)
        	<< "Constructed a cas_array " << name_ << " with " << n << " row"
        	<< (n>1?"s":"") << " and one column named " << cname;
} // ctor

/// Construct an index based on the given index specification.
int cas_array::buildIndexes(const char *opt) 
{  	//TODO:opt should be different
    	if (columns.empty()) return -1;

    	for(columnList::const_iterator it = columns.begin(); it != columns.end(); it++){
		ibis::bord::column *col = static_cast<ibis::bord::column*>((*it).second);
    		if (col == 0) return -2;
    		
		// call the function to build the index
    		col->loadIndex(opt);
    		// check the index size
    		if (col->indexSize() > 0) {
#ifdef DEBUG
        		printf("The Index size is=%ld\n", col->indexSize());
#endif
        		LOGGER(ibis::gVerbose > 2)
            			<< "cas_array(" << name_ << '.' << col->name()
            			<< ") completed building an index";
		}
		else{
        		LOGGER(ibis::gVerbose > 0)
        	    		<< "Warning -- cas_array(" << name_ << '.' << col->name()
            			<< ") failed to create an in-memory index";
        		return -3;
		}
	}
} // cas_array::buildIndexes

/// Build index for a series of values in memory.
void* fb_build_index(int numVars, void **vals, void *dataType, size_t varSize, const char *opt, void **colName)
{
	void *raw[numVars];
	ibis::TYPE_T *type = new ibis::TYPE_T[numVars];

	for(int i = 0; i < numVars; i++){
		//if(strcmp((char*)dataType[i], "double")==0){
		if(*((char*)dataType+i) == 'd'){
			ibis::array_t<double> *arr;
			arr = new ibis::array_t<double>((double*)vals[i], varSize);
			raw[i] = arr;
			type[i] = ibis::DOUBLE;
		}
	}

        cas_array *ca = new cas_array(numVars, varSize, type, raw, colName);
        if (ca == 0) {
                //delete arr;
                delete raw;
                return ca;
        }

        struct timeval tv1, tv2;
        gettimeofday(&tv1, NULL);
        (void) ca->buildIndexes(opt);
        //*size = ca->buildIndexes(opt); //get the index size
        gettimeofday(&tv2, NULL);
#ifdef DEBUG
        printf("ca->buildIndexes time = %f seconds\n",
             (double) (tv2.tv_usec - tv1.tv_usec)/1000000 +
             (double) (tv2.tv_sec - tv1.tv_sec));
#endif
        return ca;
}

void* fb_build_index_double(int numvars, double **vals, size_t nvals, const char *opt, void **colname) {
	//ibis::gVerbose = 8;
    	void *raw[numvars];
    	ibis::TYPE_T *type = new ibis::TYPE_T[numvars];
    	ibis::array_t<double> *arr;

    	for(int i=0; i<numvars; i++){
        	arr = new ibis::array_t<double>(*(vals+i), nvals);
        	raw[i] = arr;
        	type[i] = ibis::DOUBLE;
    	}

    	cas_array *ca = new cas_array(numvars, nvals, type, raw, colname);
    	if (ca == 0) {
        	delete arr;
        	delete raw;
        	return ca;
    	}

	struct timeval tv1, tv2;
	gettimeofday(&tv1, NULL);
    	(void) ca->buildIndexes(opt);
    	//*size = ca->buildIndexes(opt); //get the index size
	gettimeofday(&tv2, NULL);
#ifdef DEBUG
	printf("ca->buildIndexes time = %f seconds\n",
             (double) (tv2.tv_usec - tv1.tv_usec)/1000000 +
             (double) (tv2.tv_sec - tv1.tv_sec));
#endif
    	return ca;
} // fb_build_index_double


int fb_build_result_set(void *ca, const char *select, const char *from, const char *where, int *size, void *qret)
{
        ibis::gVerbose = 0; //SET gVerbose!!!!
        const cas_array *CA = static_cast<const cas_array*>(ca);
        const ibis::column *col = CA->getColumn(static_cast<uint32_t>(0));

        struct timeval tv1, tv2;
        gettimeofday(&tv1, NULL);

        ibis::query aquery(0, CA, 0);
        aquery.setWhereClause(where);
        aquery.setSelectClause(select);
        aquery.evaluate();

        gettimeofday(&tv2, NULL);
#ifdef DEBUG
     	printf("Evaluate time = %f seconds\n",
             (double) (tv2.tv_usec - tv1.tv_usec)/1000000 +
             (double) (tv2.tv_sec - tv1.tv_sec));
#endif

        gettimeofday(&tv1, NULL);
        if(strstr(select, "count") != NULL){
                long int num = aquery.getNumHits();
                printf("count=%ld\n", num);
        }
        gettimeofday(&tv2, NULL);
     	printf("Evaluate time = %f seconds\n",
             (double) (tv2.tv_usec - tv1.tv_usec)/1000000 +
             (double) (tv2.tv_sec - tv1.tv_sec));

	//=========explore acquire query result==============
	/*ibis::colValues *col;
	ibis::selectClause::AGREGADO aggr;
	ibis::selectClause& comps;
	aggr = comps.getAggregator(0);
	const ibis::bitvector *hits = aquery.getHitVector();
	if(hits != 0){
		if(aggr == ibis::selectClause::NIL_AGGR){
			col = ibis::colValues::create(c, *hits);
		}
	}else{
		switch(aggr){
		case ibis::selectClause::AVG:
		case ibis::selectClause::SUM:
			col = new ibis::colDoubles(c, *hits);
		break;
		}
	}
	col.getArray();
	*/
	return 0;
}
