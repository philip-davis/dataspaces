
import numpy as np
import dspaceswrapper.dspaces as dspaces 
import copy

class dataspaceClient:

    # input a multidimensional array and get the size of it
    # input data should be an nparray
    def getUpBound(this,lb,data):

        shape = data.shape
        
        if(len(lb)!=len(shape)):
            print ('shape data shoule be in the same dimention with lb')
            exit(-1)

        dim = len(shape)
        offset = np.ones(dim)

        ub = np.asarray(lb)+np.asarray(shape) - offset
        ub.astype(int)
        return ub.tolist(),dim

    
    def dspaces_init(this,comm,num_peers,appid):
        dspaces.wrapper_dspaces_init(comm,num_peers,appid) 

    def dspaces_wrapper_finalize(this):
        dspaces.wrapper_finalize()

    def dspaces_lock_on_read(this,lock_name):
        dspaces.wrapper_dspaces_lock_on_read(lock_name)

    def dspaces_lock_on_write(this,lock_name):
        dspaces.wrapper_dspaces_lock_on_write(lock_name)

    def dspaces_get_data(this,var_name,ver,elemsize,lb,data):

        originalShape = data.shape
        ub,ndim = this.getUpBound(lb,data)
        

        # transfer to one dimention
        arraydataFlaten = data.flatten()

        dspaces.wrapper_get_data(var_name,ver,elemsize,ndim,lb,ub,arraydataFlaten)
        
        # reshape into original format
        getdata = arraydataFlaten.reshape(originalShape)
        return getdata


    def dspaces_put_data(this,var_name,ver,elemsize,lb,data):

        arraydata = np.asarray(data)
        ub,ndim = this.getUpBound(lb,arraydata)
        
        # transfer to one dimention
        arraydata = arraydata.flatten()
        
        dspaces.wrapper_put_data(var_name,ver,elemsize,ndim,lb,ub,arraydata)

    def dspaces_unlock_on_read(this,lock_name):
        dspaces.wrapper_dspaces_unlock_on_read(lock_name)

    def dspaces_unlock_on_write(this,lock_name):
        dspaces.wrapper_dspaces_unlock_on_write(lock_name)





