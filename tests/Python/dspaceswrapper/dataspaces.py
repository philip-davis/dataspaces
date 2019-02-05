
import numpy as np
import dspaceswrapper.dspaces as dspaces 
import ctypes
import copy

class dataspaceClient:
    def __init__(self, appid, comm):
        self.appid = appid
        self.comm = comm
        self.num_peers = comm.Get_size()
        print("init num_peers")
        print(num_peers)
        #init(comm,num_peers,appid)

    
    def getElemNum(self,lb,ub):
        # get elem number between lb and ub
        dim = len(lb)
        elemNum = 1
        for i in range (dim):
            dis = ub[i]-lb[i] + 1
            elemNum = elemNum*dis

        return elemNum

    def getDataShape(self,lb,ub):
        shape = []
        dim = len(lb)
        for i in range (dim):
            shape.append(ub[i]-lb[i] + 1)
        
        return tuple(shape)

    # input a multidimensional array and get the size of it
    # input data should be an nparray
    def getUpBound(self,lb,data):

        shape = data.shape
        
        if(len(lb)!=len(shape)):
            print ('shape data shoule be in the same dimention with lb')
            exit(-1)

        dim = len(shape)
        offset = np.ones(dim)

        ub = np.asarray(lb)+np.asarray(shape) - offset
        ub = ub.astype(int)
        return ub.tolist(),dim

    def init(self,comm,num_peers,appid):
        dspaces.wrapper_dspaces_init(comm,num_peers,appid) 

    def finalize(self):
        dspaces.wrapper_finalize()

    def lock_on_write(self,lock_name):
        dspaces.wrapper_dspaces_lock_on_read(lock_name)

    def unlock_on_write(self,lock_name):
        dspaces.wrapper_dspaces_lock_on_write(lock_name)

    def get(self,var_name,ver,lb,ub):

        # originalShape = data.shape
        # ub,ndim = this.getUpBound(lb,data)
        if(len(lb)!=len(ub)):
            sys.exit("lb and ub should in same dimention")

        ndim = len(lb)

        originalShape = self.getDataShape(lb,ub)
        elemNum = self.getElemNum(lb,ub)

        arraydataFlaten = np.empty(elemNum)
        arraydataFlaten.fill(0.0)


        elemsize = ctypes.sizeof(ctypes.c_double)

        # the elemsize here is the size for each element
        dspaces.wrapper_get_data(var_name,ver,elemsize,ndim,lb,ub,arraydataFlaten)
        
        # reshape into original format
        getdata = arraydataFlaten.reshape(originalShape)
        return getdata


    def put(self,var_name,ver,lb,putdata):

        arraydata = np.asarray(putdata)
        ub,ndim = self.getUpBound(lb,arraydata)
        
        # transfer to one dimention
        arraydata = arraydata.flatten()

        elemsize = ctypes.sizeof(ctypes.c_double)
        
        # the elemsize here is the size for each element
        dspaces.wrapper_put_data(var_name,ver,elemsize,ndim,lb,ub,arraydata)

    def lock_on_read(self,lock_name):
        dspaces.wrapper_dspaces_unlock_on_read(lock_name)

    def unlock_on_read(self,lock_name):
        dspaces.wrapper_dspaces_unlock_on_write(lock_name)





