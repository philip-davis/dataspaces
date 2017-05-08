
#include <winstd.H>
#include <iostream>
#include <algorithm>

#include <RealBox.H>
#include <StateData.H>
#include <StateDescriptor.H>
#include <ParallelDescriptor.H>

const Real INVALID_TIME = -1.0e200;

const int MFNEWDATA = 0;
const int MFOLDDATA = 1;

StateData::StateData () 
{
   desc = 0;
   new_data = old_data = 0;
   new_time.start = INVALID_TIME;
   new_time.stop  = INVALID_TIME;
   old_time.start = INVALID_TIME;
   old_time.stop  = INVALID_TIME;
}

StateData::StateData (const Box&             p_domain,
                      const BoxArray&        grds,
                      const StateDescriptor* d,
                      Real                   cur_time,
                      Real                   dt)
{
    define(p_domain, grds, *d, cur_time, dt);
}

void
StateData::define (const Box&             p_domain,
                   const BoxArray&        grds,
                   const StateDescriptor& d,
                   Real                   time,
                   Real                   dt)
{
    BL_PROFILE("StateData::define()");
    domain = p_domain;
    desc = &d;
    grids.define(grds);
    //
    // Convert to proper type.
    //
    IndexType typ(desc->getType());
    StateDescriptor::TimeCenter t_typ(desc->timeType());
    if (!typ.cellCentered())
    {
        domain.convert(typ);
        grids.convert(typ);
    }
    if (t_typ == StateDescriptor::Point)
    {
        new_time.start = new_time.stop = time;
        old_time.start = old_time.stop = time - dt;
    }
    else
    {
        new_time.start = time;
        new_time.stop  = time+dt;
        old_time.start = time-dt;
        old_time.stop  = time;
    }
    int ncomp = desc->nComp();

    new_data = new MultiFab(grids,ncomp,desc->nExtra(),Fab_allocate);

    old_data = 0;
}

void
StateData::define (const Box&             p_domain,
                   const BoxArray&        grds,
		   const DistributionMapping& dm,
                   const StateDescriptor& d,
                   Real                   time,
                   Real                   dt)
{
    BL_PROFILE("StateData::define()");
    domain = p_domain;
    desc = &d;
    grids.define(grds);
    //
    // Convert to proper type.
    //
    IndexType typ(desc->getType());
    StateDescriptor::TimeCenter t_typ(desc->timeType());
    if (!typ.cellCentered())
    {
        domain.convert(typ);
        grids.convert(typ);
    }
    if (t_typ == StateDescriptor::Point)
    {
        new_time.start = new_time.stop = time;
        old_time.start = old_time.stop = time - dt;
    }
    else
    {
        new_time.start = time;
        new_time.stop  = time+dt;
        old_time.start = time-dt;
        old_time.stop  = time;
    }
    int ncomp = desc->nComp();

    new_data = new MultiFab(grids,ncomp,desc->nExtra(),dm,Fab_allocate);

    old_data = 0;
}

void
StateData::reset ()
{
    new_time = old_time;
    old_time.start = old_time.stop = INVALID_TIME;
    std::swap(old_data, new_data);
}

void
StateData::restart (std::istream&          is,
                    const StateDescriptor& d,
                    const std::string&     chkfile,
                    bool                   bReadSpecial)
{
    BL_PROFILE("StateData::restart()");
    if (bReadSpecial)
    {
	std::cerr << "StateData:: restart:: w/bReadSpecial not implemented" << std::endl;
        ParallelDescriptor::Abort();  // not implemented
    }

    desc = &d;

    is >> domain;

    if (bReadSpecial)
    {
        BoxLib::readBoxArray(grids, is, bReadSpecial);
    }
    else
    {
        grids.readFrom(is);
    }

    is >> old_time.start;
    is >> old_time.stop;
    is >> new_time.start;
    is >> new_time.stop;

    int nsets;
    is >> nsets;

    old_data = 0;
    new_data = 0;
    //
    // If no data is written then we just allocate the MF instead of reading it in. 
    // This assumes that the application will do something with it.
    // We set it to zero in case a compiler complains about uninitialized data.
    //
    if (nsets == 0)
    {
       new_data = new MultiFab(grids,desc->nComp(),desc->nExtra(),Fab_allocate);
       new_data->setVal(0.);
    }

    std::string mf_name;
    std::string FullPathName;
    //
    // This reads the "new" data, if it's there.
    //
    if (nsets >= 1)
    {
        new_data = new MultiFab;
        is >> mf_name;
        //
        // Note that mf_name is relative to the Header file.
        // We need to prepend the name of the chkfile directory.
        //
        FullPathName = chkfile;
        if (!chkfile.empty() && chkfile[chkfile.length()-1] != '/')
            FullPathName += '/';
        FullPathName += mf_name;
        VisMF::Read(*new_data, FullPathName);
    }
    //
    // This reads the "old" data, if it's there.
    //
    if (nsets == 2)
    {
        old_data = new MultiFab;
        is >> mf_name;
        //
        // Note that mf_name is relative to the Header file.
        // We need to prepend the name of the chkfile directory.
        //
        FullPathName = chkfile;
        if (!chkfile.empty() && chkfile[chkfile.length()-1] != '/')
            FullPathName += '/';
        FullPathName += mf_name;
        VisMF::Read(*old_data, FullPathName);
    }
}

StateData::~StateData()
{
   desc = 0;
   delete new_data;
   delete old_data;
}

void
StateData::allocOldData ()
{
    if (old_data == 0)
    {
        old_data = new MultiFab(grids,desc->nComp(),desc->nExtra());
    }
}

const BCRec
StateData::getBC (int comp, int i) const
{
    BCRec bcr;
    BoxLib::setBC(grids[i],domain,desc->getBC(comp),bcr);
    return bcr;
}

void
StateData::setOldTimeLevel (Real time)
{
    if (desc->timeType() == StateDescriptor::Point)
    {
        old_time.start = old_time.stop = time;
    }
    else
    {
        BoxLib::Error("StateData::setOldTimeLevel called with Interval");
    }
}

void
StateData::setNewTimeLevel (Real time)
{
    if (desc->timeType() == StateDescriptor::Point)
    {
        new_time.start = new_time.stop = time;
    }
    else
    {
        BoxLib::Error("StateData::setNewTimeLevel called with Interval");
    }
}

void
StateData::setTimeLevel (Real time,
                         Real dt_old,
                         Real dt_new)
{
    if (desc->timeType() == StateDescriptor::Point)
    {
        new_time.start = new_time.stop = time;
        old_time.start = old_time.stop = time - dt_old;
    }
    else
    {
        new_time.start = time;
        new_time.stop  = time+dt_new;
        old_time.start = time-dt_old;
        old_time.stop  = time;
    }
}

void
StateData::swapTimeLevels (Real dt)
{
    old_time = new_time;
    if (desc->timeType() == StateDescriptor::Point)
    {
        new_time.start += dt;
        new_time.stop  += dt;
   }
    else
    {
        new_time.start = new_time.stop;
        new_time.stop += dt;
    }
    std::swap(old_data, new_data);
}

void
StateData::FillBoundary (FArrayBox&     dest,
                         Real           time,
                         const Real*    dx,
                         const RealBox& prob_domain,
                         int            dest_comp,
                         int            src_comp,
                         int            num_comp)
{
    BL_PROFILE("StateData::FillBoundary()");
    BL_ASSERT(dest.box().ixType() == desc->getType());
   
    if (domain.contains(dest.box())) return;

    const Box& bx  = dest.box();
    const int* dlo = dest.loVect();
    const int* dhi = dest.hiVect();
    const int* plo = domain.loVect();
    const int* phi = domain.hiVect();

    Array<int> bcrs;

    Real xlo[BL_SPACEDIM];
    BCRec bcr;
    const Real* problo = prob_domain.lo();

    for (int i = 0; i < BL_SPACEDIM; i++)
    {
        xlo[i] = problo[i] + dx[i]*(dlo[i]-plo[i]);
    }
    for (int i = 0; i < num_comp; )
    {
        const int dc  = dest_comp+i;
        const int sc  = src_comp+i;
        Real*     dat = dest.dataPtr(dc);

        if (desc->master(sc))
        {
            const int groupsize = desc->groupsize(sc);

            BL_ASSERT(groupsize != 0);

            if (groupsize+i <= num_comp)
            {
                //
                // Can do the whole group at once.
                //
                bcrs.resize(2*BL_SPACEDIM*groupsize);

                int* bci  = bcrs.dataPtr();

                for (int j = 0; j < groupsize; j++)
                {
                    BoxLib::setBC(bx,domain,desc->getBC(sc+j),bcr);

                    const int* bc = bcr.vect();

                    for (int k = 0; k < 2*BL_SPACEDIM; k++)
                        bci[k] = bc[k];

                    bci += 2*BL_SPACEDIM;
                }
                //
                // Use the "group" boundary fill routine.
                //
                desc->bndryFill(sc)(dat,dlo,dhi,plo,phi,dx,xlo,&time,bcrs.dataPtr(),true);

                i += groupsize;
            }
            else
            {
                BoxLib::setBC(bx,domain,desc->getBC(sc),bcr);
                desc->bndryFill(sc)(dat,dlo,dhi,plo,phi,dx,xlo,&time,bcr.vect());
                i++;
            }
        }
        else
        {
            BoxLib::setBC(bx,domain,desc->getBC(sc),bcr);
            desc->bndryFill(sc)(dat,dlo,dhi,plo,phi,dx,xlo,&time,bcr.vect());
            i++;
        }
    }
}

void
StateData::RegisterData (MultiFabCopyDescriptor& multiFabCopyDesc,
                         Array<MultiFabId>&      mfid)
{
    mfid.resize(2);
    mfid[MFNEWDATA] = multiFabCopyDesc.RegisterFabArray(new_data);
    mfid[MFOLDDATA] = multiFabCopyDesc.RegisterFabArray(old_data);
}

void
StateData::InterpAddBox (MultiFabCopyDescriptor& multiFabCopyDesc,
			 Array<MultiFabId>&      mfid,
			 BoxList*                unfillableBoxes,
			 Array<FillBoxId>&       returnedFillBoxIds,
			 const Box&              subbox,
			 Real                    time,
			 int                     src_comp,
			 int                     dest_comp,
			 int                     num_comp,
			 bool                    extrap)
{
    if (desc->timeType() == StateDescriptor::Point)
    {
        if (old_data == 0)
        {
            returnedFillBoxIds.resize(1);
            returnedFillBoxIds[0] = multiFabCopyDesc.AddBox(mfid[MFNEWDATA],
                                                            subbox,
                                                            unfillableBoxes,
                                                            src_comp,
                                                            dest_comp,
                                                            num_comp);
        }
        else
        {
            BoxLib::InterpAddBox(multiFabCopyDesc,
				 unfillableBoxes,
				 returnedFillBoxIds,
				 subbox,
				 mfid[MFOLDDATA],
				 mfid[MFNEWDATA],
				 old_time.start,
				 new_time.start,
				 time,
				 src_comp,
				 dest_comp,
				 num_comp,
				 extrap);
        }
    }
    else
    {
        const Real teps = (new_time.start - old_time.start)/1000.0;

        if (time > new_time.start-teps && time < new_time.stop+teps)
        {
            returnedFillBoxIds.resize(1);
            returnedFillBoxIds[0] = multiFabCopyDesc.AddBox(mfid[MFNEWDATA],
                                                            subbox,
                                                            unfillableBoxes,
                                                            src_comp,
                                                            dest_comp,
                                                            num_comp);
        }
        else if (old_data != 0              &&
                 time > old_time.start-teps &&
                 time < old_time.stop+teps)
        {
            returnedFillBoxIds.resize(1);
            returnedFillBoxIds[0] = multiFabCopyDesc.AddBox(mfid[MFOLDDATA],
                                                            subbox,
                                                            unfillableBoxes,
                                                            src_comp,
                                                            dest_comp,
                                                            num_comp);
        }
        else
        {
            BoxLib::Error("StateData::Interp(): cannot interp");
        }
   }
}

void
StateData::InterpFillFab (MultiFabCopyDescriptor&  multiFabCopyDesc,
			  const Array<MultiFabId>& mfid,
			  const Array<FillBoxId>&  fillBoxIds,
			  FArrayBox&               dest,
			  Real                     time,
			  int                      src_comp,
			  int                      dest_comp,
			  int                      num_comp,
			  bool                     extrap)
{
    BL_PROFILE("StateData::InterpFillFab()");
    if (desc->timeType() == StateDescriptor::Point)
    {
        if (old_data == 0)
        {
            multiFabCopyDesc.FillFab(mfid[MFNEWDATA], fillBoxIds[0], dest);
        }
        else
        {
            BoxLib::InterpFillFab(multiFabCopyDesc,
				  fillBoxIds,
				  mfid[MFOLDDATA],
				  mfid[MFNEWDATA],
				  dest,
				  old_time.start,
				  new_time.start,
				  time,
				  src_comp,
				  dest_comp,
				  num_comp,
				  extrap);
        }
    }
    else
    {
        const Real teps = (new_time.start - old_time.start)/1000.0;

        if (time > new_time.start-teps && time < new_time.stop+teps)
        {
            multiFabCopyDesc.FillFab(mfid[MFNEWDATA], fillBoxIds[0], dest);
        }
        else if (old_data != 0              &&
                 time > old_time.start-teps &&
                 time < old_time.stop+teps)
        {
            multiFabCopyDesc.FillFab(mfid[MFOLDDATA], fillBoxIds[0], dest);
        }
        else
        {
            BoxLib::Error("StateData::Interp(): cannot interp");
        }
    }
}

void
StateData::checkPoint (const std::string& name,
                       const std::string& fullpathname,
                       std::ostream&  os,
                       VisMF::How     how,
                       bool           dump_old)
{
    BL_PROFILE("StateData::checkPoint()");
    static const std::string NewSuffix("_New_MF");
    static const std::string OldSuffix("_Old_MF");

    if (dump_old == true && old_data == 0)
    {
        dump_old = false;
    }

    if (ParallelDescriptor::IOProcessor())
    {
        //
        // The relative name gets written to the Header file.
        //
        std::string mf_name_old = name; mf_name_old += OldSuffix;
        std::string mf_name_new = name; mf_name_new += NewSuffix;

        os << domain << '\n';

        grids.writeOn(os);

        os << old_time.start << '\n'
           << old_time.stop  << '\n'
           << new_time.start << '\n'
           << new_time.stop  << '\n';

        if (desc->store_in_checkpoint()) 
        {
           if (dump_old)
           {
               os << 2 << '\n' << mf_name_new << '\n' << mf_name_old << '\n';
           }
           else
           {
               os << 1 << '\n' << mf_name_new << '\n';
           }
        }
        else
        {
               os << 0 << '\n';
        }
    }

    if (desc->store_in_checkpoint())
    {
       BL_ASSERT(new_data);
       std::string mf_fullpath_new = fullpathname; mf_fullpath_new += NewSuffix;
       VisMF::Write(*new_data,mf_fullpath_new,how);

       if (dump_old)
       {
           BL_ASSERT(old_data);
           std::string mf_fullpath_old = fullpathname; mf_fullpath_old += OldSuffix;
           VisMF::Write(*old_data,mf_fullpath_old,how);
       }
    }
}

void
StateData::printTimeInterval (std::ostream &os) const
{
    os << '['
       << old_time.start
       << ' '
       << old_time.stop
       << "] ["
       << new_time.start
       << ' '
       << new_time.stop
       << ']'
       << '\n';
}

