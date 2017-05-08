      subroutine vkp(ketest,le,kdtest,ld,upsl,ltdf,
     &               re,sl,df,l11df,io)
!     old version pre Evatt Hawkes mods JUN 2004
!      subroutine vkp(ketest,kdtest,up,eps,upsl,ltdf,
!     &               nxx,xmin,xmax,re,sl,df,io,l11df,
!     &               l_ref,a_ref)
************************************************************************
*   ce programme permet d'obtenir un jeu de donnees (up,eps,ke,kd)     *
*   coherent pour le spectre vkp (von karman avec modification de pao) *
*   et calcule ensuite la longueur integrale associee. on entre soit   *
*   (ke,kd) soit (up,eps), le code determine alors l'ensemble du jeu   *
*   de donnees tel que l'energie cinetique turbulente up*up (2d) et    *
*   le taux de dissipation eps soient egaux a ceux calcules a partir   * 
*   du spectre.                                                        *
*                                                                      *
*      up   -->  vitesse turbulente                                    *
*      eps  -->  taux de dissipation                                   *
*      ke   -->  nombre d'onde des tourbillons les plus energetiques   *
*      kd   -->  nombre d'onde ou se produit la dissipation            *
*      re   -->  nombre de reynolds acoustique                         *
*      l11  -->  longueur integrale definie a partir du spectre        * 
*      lt   -->  longueur integrale definie par up3/eps                *
*                                                                      *
************************************************************************
!     modifications by Evatt Hawkes
      use param_m, only : numdim, vary_in_x,vary_in_y,vary_in_z
      use param_m, only : nx_g,ny_g,nz_g
      use grid_m, only : xmax,xmin,ymax,ymin,zmax,zmin

      parameter(a1=1.5,a2=1.5,nx=5000)
      real re,ke,kd,up,eps,kemin,kdmax,ketest,kdtest,re_t
!     Evatt Hawkes JUN 2004
!      real delta,xmax,xmin,l11,lt,ltdf,l11df,upsl,tau,ltlim,uplim
!      real l_ref, a_ref
      real delta,l11,lt,ltdf,l11df,upsl,tau,ltlim,uplim
      real le, ld ! energy containing and dissipation length scales
      integer nxx, io
!      open(11,file='mesh_in',status='old')
      pi = 4. * atan(1.)
!      read(11,*) nxx,xmin,xmax
!      read(11,*) re,sl,df

!     modifications Evatt Hawkes JUN 2004
!     increase generality in box sizes
      delta = (xmax-xmin)/float(nx-1)
      kemin = 2.*pi/(xmax-xmin)
      kdmax = 1./delta   

      if(vary_in_x==1)then
        delta = (xmax-xmin)/real(nx_g-1)
        keminx = 2.*pi/(xmax-xmin)
        kdmaxx = 1./delta   
      endif

!      if(vary_in_y==1)then
!        delta = (ymax-ymin)/real(ny_g-1)
!        keminy = 2.*pi/(ymax-ymin)
!        kdmaxy = 1./delta   
!      endif

!      if(vary_in_z==1)then
!        delta = (zmax-zmin)/real(nz_g-1)
!        keminz = 2.*pi/(zmax-zmin)
!        kdmaxz = 1./delta   
!      endif

!      write(io,*) 'nx = ',nxx,' (xmin,xmax) = (',xmin,',',xmax,')'
!      write(io,*) kemin,'<=  ke <<  kd  <=',kdmax
!      write(io,*)

!     over-ride choice

!      write(io,*)
!     &' 1 - entree:(ke,kd)  --> sortie:(up,eps) - formules directes'
!      write(io,*)
!     &' 2 - entree:(up,eps) --> sortie:(ke,kd)  - methode de newton' 
!      write(io,*)
!      write(io,*)'  votre choix (1/2) ?'
!      read(*,*) irep
      irep=2

!     over-ride range inputs

      if (irep.eq.2) then     
!        write(io,*) 'generating a table of values ...'
!        write(io,*) 'enter low/high/inc up/sl ... '
!        read(*,*) upsl,uplim,dupsl

!        write(io,*) 'enter up/sl'
!        read(*,*) upsl
!        upsl=1       !user set
        uplim=upsl   !set for no range
        dupsl=0      !set for no range

!        write(io,*) 'enter low/high/inc lt/df ... '
!        read(*,*) ltdf,ltlim,dltdf

!        write(io,*) 'enter lt/df'
!        read(*,*) ltdf
!        ltdf=10      !user set
        ltlim=ltdf   !set for no range
        dltdf=0      !set for no range

!        write(io,*) 'ke initial ?'
!        read(*,*) ke

        ke=100

!        write(io,*) 'kd initial ?'
!        read(*,*) kd 

        kd=100

        tolx = 0.0001
        ntrial = 100
        upsl1 = upsl
        ltdf1 = ltdf
10      up = upsl * sl
        lt = ltdf * df
        eps = (up*up)**1.5 / lt
        b=a2*(ke/kd)**(4./3.)
        call newton(ntrial,b,tolx,up,eps,re,io)
        if (b.gt.a2) then
!          write(io,*)'incoherent data, verify ke and kd.'
!          write(io,*)' execution continues ...'
          neff=nx/10
        elseif (b.ge.0.01) then
          neff=nx/5
        elseif (b.ge.0.001) then
          neff=nx
        else
!          write(io,*)'ke/kd too small, take larger xmax.'
!          write(io,*)' execution continues ...'
          neff=nx
        endif
        call fbeta(b,fb,neff,io)
        up3=up*up*up
        ke=eps/(a1*up3*fb)
        kd=ke*(a2/b)**0.75
!        if((vary_in_x==1) .and. (ke.lt.keminx.or.kd.gt.kdmaxx)) then
!         write(io,*)
!         write(io,*)'WARNING!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
!         write(io,*)'all scales may not be resolvable with this choice'
!         write(io,*)'problem is X-direction'
!         write(io,*) 'ke ', ke, ' keminx' , keminx
!         write(io,*) 'kd ', kd, ' kdmaxx' , kdmaxx  
!         write(io,*)
!        endif
!        if((vary_in_y==1) .and. (ke.lt.keminy.or.kd.gt.kdmaxy)) then
!         write(io,*)
!         write(io,*)'WARNING!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
!         write(io,*)'all scales may not be resolvable with this choice'
!         write(io,*)'problem is Y-direction'
!         write(io,*)
!        endif
!        if((vary_in_z==1) .and. (ke.lt.keminz.or.kd.gt.kdmaxz)) then
!         write(io,*)
!         write(io,*)'WARNING!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
!         write(io,*)'all scales may not be resolvable with this choice'
!         write(io,*)'problem is Z-direction'
!         write(io,*)
!        endif
!        if(ke.gt.kd)then
!         write(io,*)
!         write(io,*)'WARNING!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
!         write(io,*)'kd should be > ke'
!         write(io,*)'i.e. ld<le'
!         write(io,*)
!        endif
!        write(io,*) 'initialize with the following values ...'
!        write(io,*) 'ke  ',ke
!        write(io,*) 'kd  ',kd
        call spectre(re,ke,kd,up,eps,l11,lt,io)
        upsl = up / sl
        ltdf = lt / df
        l11df= l11/ df
        re_t = re * up * lt
        tau = 0.4*up*up/eps*sl/df
!       write(io,*) 'up/sl, lt/df, l11/df and tau/tf are: ',
!    &              upsl,  ltdf,  l11df,     tau
!       write(*,*) 're_t = ',re_t
!       Evatt Hawkes mods JUN 2004
!       it was confusing to always swap between length and wavenumber
!       therefore i am separating into two variables
!        ketest = 2*pi/ke
!        kdtest = 2*pi/kd
        le = 2*pi/ke
        ld = 2*pi/kd
        ketest = ke
        kdtest = kd
        if (upsl .lt. uplim) then
          upsl = upsl + dupsl
          goto 10
        else
          upsl = upsl1
          if (ltdf .lt. ltlim) then
            ltdf = ltdf + dltdf
            goto 10
          endif
        endif
!-----------------------------------------------------------------------------------------
!       write turbulence parameters to io and file

!       Evatt Hawkes JUN 2004 - moved this to the main code
!        write(io,*) 'BASED ON YOUR REFERENCE VISCOSITY, the '
!        write(io,*) 'initial turbulence parameters are as follows:'
!        write(io,*)
!        write(io,*) 'up/sl            =',upsl
!        write(io,*) 'lt/df            =',ltdf
!        write(io,*) 'l11/df           =',l11df
!        write(io,*) 'l11 (cm)         =',l11*100.*l_ref
!        write(io,*) 'df (cm)          =',df*100.*l_ref
!        write(io,*) 're_t             =',re_t
!        write(io,*) 're_l11           =',re_t*l11df/ltdf
!        write(io,*) 're_taylor        =',(re_t*l11df/ltdf)**0.5
!!       change by evatt hawkes - write ke and kd in dimensional units of cm
!!        write(io,*) 'le for ke (cm)   =',ketest*l_ref*100.0
!!        write(io,*) 'ld for kd (cm)   =',kdtest*l_ref*100.0
!         Evatt Hawkes JUN 2004 - now length scales instead
!        write(io,*) 'le for ke (cm)   =',le*l_ref*100.0
!        write(io,*) 'ld for kd (cm)   =',ld*l_ref*100.0
!
!        write(io,*) 'up (cm/sec)      =',up*a_ref*100.0
!        write(io,*) 'eps (cm^2/sec^3) =',eps*a_ref**3/l_ref*(100.0**2)
!        write(io,*)
!
!        write(io,*) 'various turbulence time scales are as follows:'
!        write(io,*)
!        write(io,*) 't_flame (sec)        =', df*l_ref/(sl*a_ref)
!        write(io,*) 'tau/t_flame          =', tau
!        write(io,*) 'tau=lt/up (sec)      =', tau*df*l_ref/(sl*a_ref)
!        write(io,*) 't_l11=l11/up (sec)   =', l11*l_ref/(up*a_ref)
!        write(io,*)

c        write(io,*) 'autre choix pour up et eps (1/0) ?'
c        read(*,*) irep
c        if (irep.eq.1) then
c          goto 10
c        endif
      else
!     case of irep ne 2
!     this version never goes in here
  11    write(io,*) 'ke ?'
        read(*,*) ke
        write(io,*) 'kd ?'
        read(*,*) kd
        if(ke.lt.kemin.or.kd.gt.kdmax) then
         write(io,*)'all scales may not be resolvable with this choice'
        endif
        call compat(re,ke,kd,up,eps,io)
        write(io,*) 'initialisez avec les valeurs suivantes:'
        write(io,*) 'up  ',up
        write(io,*) 'eps ',eps
        call spectre(re,ke,kd,up,eps,l11,lt,io)
        upsl = up / sl
        ltdf = lt / df
        l11df= l11/ df
        re_t = re * up * lt
        tau = 0.4*up*up/eps*sl/df
        write(io,*) 'up/sl, lt/df, l11/df and tau/tf are: ',
     &              upsl,  ltdf,  l11df,     tau
        write(io,*) 're_t = ',re_t

!        call write_header(io,'-')
!        write(io,*) 'up/sl  =',upsl
!        write(io,*) 'lt/df  =',ltdf
!        write(io,*) 'l11/df =',l11df
!        write(io,*) 're_t   =',re_t
!        call write_header(io,'-')

        ketest = 2*pi/ke
        kdtest = 2*pi/kd
        write(io,*) 'autre choix pour ke et kd (1/0) ?'
        read(*,*) irep
        if (irep.eq.1) goto 11
      endif
501   format('[up,eps,ke,kd] = [',f12.8,',', f12.8,',',f12.8,
     1',',f12.8,']')
502   format('[upsl,ltdf,l11d,t] = [',f12.8,',', f12.8,',',
     1f12.8,',',f12.8,']')
503   format(1x,f9.6,1x,f9.7,1x,f9.7,1x,e9.4,1x,2(f6.2,1x),f7.3,1x,f7.1)
510   format(3x,'in vkp',6x,'in test_in')
511   format(5x,'ke',8x,'kd',8x,'up',8x,'eps',4x,
     1'ltdf',4x,'l11df',3x,'tau',5x,'re_t')
      end
*
      subroutine spectre(re,ke,kd,up,eps,l11,lt,io)
*
************************************************************************
*   cette subroutine calcule les grandeurs suivantes a partir du       *
*   nombre de reynolds re et du spectre e(k) defini par (ke,kd,up,eps) * 
*                                                                      *
*   en_cin l'energie cinetique turbulente                              *
*   --> calcule par integration sur k de spp1(k)=e(k)                   *
*                                                                      *
*   dissip la dissipation turbulente                                   *
*   --> calcule par integration sur k de spp2(k)=(2/re)*k*k*e(k)        * 
*                                                                      *
*   l11   la longueur integrale longitudinale                          *
*   --> calcule par integration sur k de spp3(k)=(2/up*up)*e(k)/k       *
*                                                                      *
*   lt    la longueur integrale                                        *
*   --> calcule par up*up*up/eps                                       *
*                                                                      *
************************************************************************
!     modification Evatt Hawkes JUN 2004: 3D case
      use param_m, only : numdim
      
      parameter(a1=1.5,a2=1.5,nk=10000,xkmax=1000.)
      real re,ke,kd,up,eps
      real xk(0:nk)
      real spp1(0:nk),spp2(0:nk),spp3(0:nk)
      real dissip,en_cin,l11,lt,coef,v_tur
      integer io
      real pi
c      character*30 fich1,fich2,fich3,fich4
c      write(io,*) 'fichier spectre 1?'
c      read(*,*) fich1
c      write(io,*) 'fichier spectre 2?'
c      read(*,*) fich2
c      write(io,*) 'fichier spectre 3?'
c      read(*,*) fich3
c      open(11,file=fich1,form='formatted')
c      open(12,file=fich2,form='formatted')
c      open(13,file=fich3,form='formatted')
c      open(9,file='spec.data',form='formatted')
      do 20 k=0,nk
      xk(k)=(k*1./nk)*xkmax
   20 continue
      do 21 k=0,nk
      vk=(a1*up**5./eps)*(xk(k)/ke)**4./((1+(xk(k)/ke)**2)**(17./6.))
      pao=exp(-1.5*a2*(xk(k)/kd)**(4./3.))
      spp1(k)=vk*pao
      spp2(k)=xk(k)*xk(k)*spp1(k)*2./re
      if (xk(k).eq.0.) then
      spp3(k)=0.
      else
      spp3(k)=spp1(k)/xk(k)
      endif
   21 continue
      call trapeze(nk,spp1,xk,en_cin,io)
      call trapeze(nk,spp2,xk,dissip,io)
      call trapeze(nk,spp3,xk,l11,io)
!     Evatt Hawkes modification JUN 2004
!     allowing for 3D case
!      l11=2.*l11/(up*up)
      if(numdim==2)then
        l11=2.*l11/(up*up)
      else
        pi = 4.0*atan(1.0)
        l11=0.5*pi*l11/(up*up)
      endif
      lt=(up*up)**(1.5)/eps
      coef=l11/lt
      v_tur=sqrt(en_cin)
c      write(io,*) 'vitesse turbulente     :',v_tur      
c      write(io,*) 'energ. cin. turbulente :',en_cin
c      write(io,*) 'dissipation turbulente :',dissip 
c      write(io,*) 'longueur integrale l11 :',l11
c      write(io,*) 'longueur integrale lt  :',lt 
c      write(io,*) 'coefficient l11/lt     :',coef
c      write(io,*) 
c      zero=1.e-10
c      do 30 k=1,nk
c      if (spp1(k).lt.zero) goto 40
c      write(11,*) xk(k),spp1(k)
c   30 continue
c   40 continue
c      do 31 k=1,nk
c      if (spp2(k).lt.zero) goto 41
c   31 continue
c   41 continue
c      do 32 k=1,nk
c      if (spp3(k).lt.zero) goto 42
c      write(13,*) xk(k),spp3(k)
c   32 continue
c   42 continue
      return
      end
*
      subroutine compat(re,ke,kd,up,eps,io)
*

      parameter(a1=1.5,a2=1.5,nx=5000)
      real re,ke,kd,up,eps
      real b,fb,gb
      real re3,ke4,fb2,fb5,gb3
      integer io
      b=a2*(ke/kd)**(4./3.)
c      write(io,*) 'beta',b
      if (b.gt.a2) then
        write(io,*)'donnees incoherentes, verifiez ke et kd.'
        write(io,*)'l''execution continue ...'
        neff=nx/10
      elseif (b.ge.0.01) then
        neff=nx/5
      elseif (b.ge.0.001) then
        neff=nx
      else
        write(io,*)'ke/kd trop petit, prenez xmax plus grand.'
        write(io,*)'l''execution continue ...'
        neff=nx
      endif
c      write(io,*) neff
      call fbeta(b,fb,neff,io)
      call gbeta(b,gb,neff,io)
      re3=re*re*re
      ke4=ke*ke*ke*ke
      fb2=fb*fb
      fb5=fb*fb*fb*fb*fb
      gb3=gb*gb*gb
      up=2.*ke*gb/(a1*re*fb2)
      eps=8.*ke4*gb3/(a1*a1*re3*fb5)
      return
      end
*
      subroutine newton(ntrial,x,tolx,up,eps,re,io)
*
      real x,tolx,up,eps,re
      real alpha,beta     
      integer io
      do 10 k=1,ntrial
      call usrfun(x,alpha,beta,up,eps,re,io)
c        errf=abs(beta)
c        if(errf.le.tolf)return
      h=beta/alpha
      x=x+h
!      write(io,*) k,' b=1.5*(ke/kd)**(4./3.)=',x
        errx=abs(h)
        if(errx.le.tolx)return
  10  continue
         write(io,*)
         write(io,*) 'WARNING!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
         write(io,*) 'Newton iteration for ke/kd did not converge'
         write(io,*) 'May result in unphysical initial u-field'
         write(io,*)
      return
      end
*
      subroutine usrfun(x,alpha,beta,up,eps,re,io)
*
      parameter(a1=1.5,a2=1.5,nx=5000)
      integer io
      if (x.ge.a2) then
!        write(io,*)'donnees incoherentes, verifiez ke et kd.'
!        write(io,*)'l''execution continue ...'
        neff=nx/10
      elseif (x.ge.0.01) then
        neff=nx/5
      elseif (x.ge.0.001) then
        neff=nx
      else
        write(io,*)'ke/kd trop petit, prenez xmax plus grand.'
        write(io,*)'l''execution continue ...'
        neff=nx
      endif
      delta=0.05
      call fbeta(x,fb,neff,io)
      call gbeta(x,gb,neff,io)
      up4=up*up*up*up
      pb=fb*fb*fb/gb
      xx=x+delta
      call fbeta(xx,fb_del,neff,io)
      call gbeta(xx,gb_del,neff,io)
      pb_del=fb_del*fb_del*fb_del*fb_del/gb_del      
      dpb=(pb_del-pb)/delta
      beta=1.-a1*a1*up4*pb*re/(2.*eps)
      alpha=a1*a1*up4*dpb*re/(2.*eps)
      return
      end
      
*
      subroutine fbeta(b,fb,neff,io)
*
      parameter(nx=80000,xmax=8000.)
      real b,fb,x4
      integer neff, io
      dimension x(0:nx)
      dimension sp(0:nx)
      do 20 k=0,neff
      x(k)=(k*1./nx)*xmax
      x4=x(k)**4.
      vk=(1+x(k)*x(k))**(17./6.)
      sp(k)=(x4/vk)*exp(-1.5*b*x(k)**(4./3.))
   20 continue
      call trapeze(neff,sp,x,sf,io)
      fb=sf
      return
      end
* 
      subroutine gbeta(b,gb,neff,io)
*
      parameter(nx=80000,xmax=8000.)
      integer neff,io
      real b,gb,x6
      dimension x(0:nx)
      dimension sp(0:nx)
      do 20 k=0,neff
      x(k)=(k*1./nx)*xmax
      x6=x(k)**6.
      vk=(1+x(k)*x(k))**(17./6.)
      sp(k)=(x6/vk)*exp(-1.5*b*x(k)**(4./3.))
   20 continue
      call trapeze(neff,sp,x,sf,io)
      gb=sf
      return
      end
*
      subroutine trapeze(nf,f,x,sf,io)
*
      parameter(nx=10000)
      dimension f(0:nx),x(0:nx)
      real sf
      integer nf, io
      som=0.
      do 50 n=0,nf-1
      tra=(x(n+1)-x(n))*(f(n+1)+f(n))/2.
!      som=som++tra
      som=som+tra
   50 continue
      sf=som
      return
      end
