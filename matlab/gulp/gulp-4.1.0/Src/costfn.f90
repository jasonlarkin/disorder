  subroutine costfn(n,xc,cost,iflag)
!
!  Subroutine used to calculate Pannetier Cost Function
!
!   5/98 allow anions other than oxygen (smw)
!        added and commented out common block "elements" (smw)
!   6/98 added smoothing fn + corrected gradients (smw)
!   8/98 added ka kqc kqa and li to bias how structure is formed 
!   7/00 adapted for Gulp1.3 [gradients removed] (smw)
!   3/09 small replaced by global value smallself from general module
!   4/12 dsqrt replaced by sqrt
!
!  l111    = if .true. all interactions lie within unit cell and immediately
!            adjacent cells only => use short cut
!
!  keyword 'smo1'
!  old smooth fn= (exp(1 - r /rc)-1)/(exp(1 - rm/rc)-1)
!
!  keyword 'smo2'
!  smoothing fn = (cosh(r-rp)-1)/(cosh(rm-rp)-1) for coulombic terms
!  smoothing fn = 1/2 (1+cos(pi(r-rm)/(rm-rp))) for bond valence terms
!
!                where rc=cutoff radius for initial r search
!                      rm=rmax which is dependent upon bval
!
!  Scott Woodley, R.I.G.B. , June 1997
!  Julian Gale, Curtin University, April 2012
!
  use configurations, only : ioptcfg, rvcfg
  use constants
  use control
  use costfunction
  use current
  use datatypes
  use element
  use general,        only : nadd, smallself
  use genetic
  use iochannels
  use optimisation
  use parallel
  use realvectors
  use symmetry
  use times
  use two
  implicit none
!
!  Passed variables
!
  integer(i4) :: iflag
  integer(i4) :: n
  real(dp)    :: xc(*)
  real(dp)    :: cost
!
!  Local variables
!
  integer(i4) :: i,j,ii,nati,natk,natj
  integer(i4) :: nor,max1u,max1l,max1l1,max2u,max2l,max2l1,jj
  integer(i4) :: max3u,max3l,max3l1,kk,ir,ver
  logical     :: lnadd,l111,lopi,lopj,li,lprint
  real(dp)    :: time1,cputime,rp,cut2,dox,kccf
  real(dp)    :: ra,rb,rc,ru1x,ru1y,ru1z,ru2x,ru2y,ru2z,ru3x,ru3y,ru3z
  real(dp)    :: xal,yal,zal,qli,oci,cni,ci,ri,bval,bond,coord
  real(dp)    :: xcrd,ycrd,zcrd,qlj
  real(dp)    :: ocj,ofct,factor,cj,rj,cij,rij,dr
  real(dp)    :: rmin,rmax,xcd,xcd2,ycd,ycd2,zcd,r,dr2(maxdis,4),sm0
  real(dp)    :: rpres2,rpres,rpres3,xcdi,ycdi,zcdi,xcdj,ycdj,zcdj
  real(dp)    :: projr,perpr,dcost,dc,time2,doxi,oxi,oxj,sm1
  real(dp)    :: ionic,oxij,rm1,dcoul
!1.1  real(dp)  :: coord2m,coord2o,dcm,smin,smax
  real(dp)    :: costcut
!
  lprint = (index(keyword,'debug').ne.0)
  if (.not.ioproc) lprint = .false.
  if (iflag.ne.0) then
    if (ioproc) write(ioout,*) &
    'Sorry: Gradients of Cost function not available.'
    call stopnow('costfn')
  endif
  ver = 0
  if (index(keyword,'smo1').ne.0) ver = 1
  if (index(keyword,'smo2').ne.0) ver = 2
  if (lprint.and.index(keyword,'smo').ne.0) write(ioout,*) 'smoothing'
!
!  First substitute parameters into place
!
  do i = 1,3
    rv(1,i) = rvcfg(1,i,ncf)
    rv(2,i) = rvcfg(2,i,ncf)
    rv(3,i) = rvcfg(3,i,ncf)
  enddo
  do i = 1,6
    x0(i) = 1.0_dp
  enddo
  do i = 1,n
    j = ioptcfg(i+nfst)
    x0(j) = xc(i)
  enddo
!
!  Make sure all fractional coords are between 0 and 1
!
  do i = 1,3*nasym
    x0(i+6) = mod(x0(i+6)+10.0_dp,1.0_dp)
    if (x0(i+6).lt.0.0_dp) x0(i+6) = 1.0_dp + x0(i+6)
  enddo
  lfirst = .true.
!
!  Set up local variables
!
  r1x = rv(1,1)
  r1y = rv(2,1)
  r1z = rv(3,1)
  r2x = rv(1,2)
  r2y = rv(2,2)
  r2z = rv(3,2)
  r3x = rv(1,3)
  r3y = rv(2,3)
  r3z = rv(3,3)
  do i = 1,numat
    xfrac(i) = x0(3*i+4)
    yfrac(i) = x0(3*i+5)
    zfrac(i) = x0(3*i+6)
  enddo
!
!  Convert cell parameters and internal coordinates into cartesian coordinates
!
  do i = 1,numat
    xclat(i) = xfrac(i)*r1x+yfrac(i)*r2x+zfrac(i)*r3x
    yclat(i) = xfrac(i)*r1y+yfrac(i)*r2y+zfrac(i)*r3y
    zclat(i) = xfrac(i)*r1z+yfrac(i)*r2z+zfrac(i)*r3z
  enddo
  do i = 1,nasym
    xalat(i) = xclat(i)
    yalat(i) = yclat(i)
    zalat(i) = zclat(i)
  enddo
!
!  Local variables
!
  time1 = cputime()
  cost = 0.0_dp
  lnadd = .true.
  if (nadd.eq.0) then
    lnadd = .false.
    if (lra) then
      nadd = 1
    else
      if (alpha.lt.70.0_dp.or.beta.lt.70.0_dp.or.gamma.lt.70.0_dp) then
        nadd = 3
      elseif (alpha.gt.110.0_dp.or.beta.gt.110.0_dp.or.gamma.gt.110.0_dp) then
        nadd = 3
      else
        nadd = 2
      endif
    endif
  endif
!
!  Decide whether all interactions lie within unit cell and first
!  neighbours - saves time for large systems
!
  l111 = .true.
  if (a.lt.rpmax.or.b.lt.rpmax.or.c.lt.rpmax) l111 = .false.
  if (alpha.lt.80.0_dp.or.beta.lt.80.0_dp.or.gamma.lt.80.0_dp) l111 = .false.
  if (alpha.gt.100.0_dp.or.beta.gt.100.0_dp.or.gamma.gt.100.0_dp) l111 = .false.
!
!  Create unit vectors
!
  ra = 1.0_dp/a
  rb = 1.0_dp/b
  rc = 1.0_dp/c
  ru1x = r1x*ra
  ru1y = r1y*ra
  ru1z = r1z*ra
  ru2x = r2x*rb
  ru2y = r2y*rb
  ru2z = r2z*rb
  ru3x = r3x*rc
  ru3y = r3y*rc
  ru3z = r3z*rc
!
!  Set up cutoffs
!
  costcut = 5.0_dp
  if (costcut.gt.a) costcut = a-1.0d-8
  if (costcut.gt.b) costcut = b-1.0d-8
  if (costcut.gt.c) costcut = c-1.0d-8
  if (lprint.and.lpredict) then
    if (lopt) then
      write(ioout,*)'Cut off within cost function 5/98  = ',costcut
    endif
  endif
  rp = costcut
  cut2 = rp*rp
!
!  Outer loop over sites
!
  do i = 1,numat
    xal = xclat(i)
    yal = yclat(i)
    zal = zclat(i)
    nati = nat(i)
    natk = 0
    qli = qf(i)
    oci = occuf(i)
    lopi = (.not.lfreeze.or.lopf(nrelat(i)))
!
!  Initialise variables for Pannetier Cost Function
!
    oxi = oxf(i)
    doxi = 0.0_dp
    cni = cnf(i)
    ci = cc(nati)
    ri = rr(nati)
    bval = abs(oxi/cni)
    bond = 2.702702702_dp
    ionic = 0.75_dp*bond
    coord = 0.0_dp
    li = .false.
!1.1    coord2o = 0.0_dp
!1.1    coord2m = 0.0_dp
!
!  Start of second atom loop
!
    do j = 1,numat
      lopj = (.not.lfreeze.or.lopf(nrelat(j)))
      if (.not.lopi.and..not.lopj) goto 1110
      natj = nat(j)
      xcrd = xclat(j) - xal
      ycrd = yclat(j) - yal
      zcrd = zclat(j) - zal
      oxj = oxf(j)
      oxij = oxi*oxj
      qlj = qf(j)
      ocj = occuf(j)
      ofct = oci*ocj
      factor = qli*qlj*ofct*angstoev
!
!  If charge product is zero no need to search for distances
!
      if (abs(factor).lt.1.0d-8) goto 1110
!
!  Initialise variables for Pannetier Cost Function
!
      cj = cc(natj)
      rj = rr(natj)
      cij = (sqrt(ci)-sqrt(cj))
      cij = cij*cij
      rij = bond*(ri+rj-((ri*rj*cij)/(ci*ri+cj*rj)))
      if (lprint) then
        write(ioout,*) 'Parameter for publication ',rij/bond
      endif
!
!  Calculate range of first and 2nd coordination shell
!
!1.1      dr = 0.30_dp
      dr = 0.50_dp
      rmax = 0.37_dp*(rij-dlog(bval))
      rmin = rmax - dr
      rmax = rmax + dr
!1.1      smax = 1.4_dp*rmax
!1.1      if(oxi.lt.0)then
!1.1        smin = 2.7d0-dr
!1.1        smax = 2.7d0+dr
!1.1      else
!1.1        smin = 3.1d0-dr
!1.1        smax = 3.1d0+dr
!1.1      endif
      if (lprint.and.rp.lt.rmax) write(ioout,*) 'rmax > rcutoff'
      if (lprint.and.rp.lt.rmax) write(ioout,*) rmax,rp
      if (ver.eq.1) then
        sm0 = 1.0_dp/(dexp(1.0_dp-(rmax/rp))-1.0_dp)
      elseif (ver.eq.2) then
        sm0 = 1.0_dp/(dcosh(rmax-rp)-1.0_dp)
        sm1 = pi/(rp-rmax)
      endif
!
!  Print out cordination information if requested on a single point calc
!
      if (lopt.and.lpredict) then
        if (natj.ne.natk.and.nati.ne.natj) then
          natk  =  natj
          if (lprint)  &
          write(ioout,'('' Coordination sphere of '',i2,''-'',i2,'' has a radial range of'',f6.3,'' to'',f6.3,'' Angs'')') &
            nati,natj,rmin,rmax
         endif
!1.1        if (nati.eq.natj) then
!1.1         natk = natj
!1.1         if (lprint) 
!1.1 &       write(ioout,'('' 2nd Co-ord`n sphere of '',i2,''-'',i2,
!1.1 &             '' has a radial range of'',f6.3,'' to'',f6.3,
!1.1 &             '' Angs'')') nati,natj,smin,smax
!1.1        endif
       endif
!
!  Possible core-shell flag
!
      if (abs(nati-natj).eq.maxele.or.(oci+ocj).lt.1.0001_dp) then
        if (ioproc) write(ioout,*)'core-shell not possible within costfn'
        call stopnow('costfn')
      endif
!
!  Loop around unit cell
!
      nor = 0
      if (lra) then
!
!  Right angled cell
!
        if (l111) then
          xcd = xcrd - 2.0_dp*r1x
          do ii = -1,1
            xcd = xcd + r1x
            xcd2 = xcd*xcd
            ycd = ycrd - 2.0_dp*r2y
            do jj = -1,1
              ycd = ycd + r2y
              ycd2 = ycd*ycd
              zcd = zcrd - 2.0_dp*r3z
              do kk = -1,1
                zcd = zcd + r3z
                if (ii.ne.0.or.jj.ne.0.or.kk.ne.0.or.i.ne.j) then
                r = xcd2 + ycd2 + zcd*zcd
                if (r.lt.smallself) then
                  if (lprint) write(ioout,*)'atoms too close!'
                  cost = cost + 5000000.0_dp
                elseif (r.le.cut2) then
                  nor = nor + 1
                  dr2(nor,1) = r
                  dr2(nor,2) = xcd
                  dr2(nor,3) = ycd
                  dr2(nor,4) = zcd
                endif
              endif
            enddo
           enddo
          enddo
        else
          max1u = (rp-xcrd)*ra + nadd
          max1l = (rp+xcrd)*ra + nadd
          max1l1 = max1l + 1
          xcd = xcrd-max1l1*r1x
          do ii = -max1l,max1u
           xcd = xcd + r1x
           if (abs(xcd).lt.rp) then
             xcd2 = xcd*xcd
             rpres2 = rp*rp - xcd2
             rpres = sqrt(rpres2)
             max2u = (rpres-ycrd)*rb + nadd
             max2l = (rpres+ycrd)*rb + nadd
             max2l1 = max2l + 1
             ycd = ycrd-max2l1*r2y
             do jj = -max2l,max2u
              ycd = ycd + r2y
              ycd2 = ycd*ycd
              rpres3 = rpres2 - ycd2
              if (rpres3.gt.0.0_dp) then
                rpres3 = sqrt(rpres3)
                max3u = (rpres3-zcrd)*rc + 1
                max3l = (rpres3+zcrd)*rc + 1
                max3l1 = max3l + 1
                zcd = zcrd-max3l1*r3z
                do kk = -max3l,max3u
                zcd = zcd + r3z
                if (ii.ne.0.or.jj.ne.0.or.kk.ne.0.or.i.ne.j) then
                  r = xcd2 + ycd2 + zcd*zcd
                  if (r.lt.smallself) then
                    if (lprint) write(ioout,*)'atoms too close!'
                    cost = cost + 5000000.0_dp
                  elseif (r.le.cut2) then
                    nor = nor + 1
                    dr2(nor,1) = r
                    dr2(nor,2) = xcd
                    dr2(nor,3) = ycd
                    dr2(nor,4) = zcd
                  endif
                endif
                enddo
              endif
             enddo
           endif
          enddo
        endif
      else
!
!  General cell
!
        if (l111) then
          xcdi = xcrd - 2.0_dp*r1x
          ycdi = ycrd - 2.0_dp*r1y
          zcdi = zcrd - 2.0_dp*r1z
!
!  Loop over unit cells to find interatomic distances
!
          do ii = -1,1
            xcdi = xcdi + r1x
            ycdi = ycdi + r1y
            zcdi = zcdi + r1z
!
            xcdj = xcdi - 2.0_dp*r2x
            ycdj = ycdi - 2.0_dp*r2y
            zcdj = zcdi - 2.0_dp*r2z
            do jj = -1,1
              xcdj = xcdj + r2x
              ycdj = ycdj + r2y
              zcdj = zcdj + r2z
              xcd = xcdj - 2.0_dp*r3x
              ycd = ycdj - 2.0_dp*r3y
              zcd = zcdj - 2.0_dp*r3z
              do kk = -1,1
               xcd = xcd + r3x
               ycd = ycd + r3y
               zcd = zcd + r3z
               if (ii.ne.0.or.jj.ne.0.or.kk.ne.0.or.i.ne.j) then
                r = xcd*xcd + ycd*ycd + zcd*zcd
                if (r.lt.smallself) then
                  if (lprint) write(ioout,*)'atoms too close!'
                  cost = cost + 5000000.0_dp
                elseif (r.le.cut2) then
                  nor = nor + 1
                  dr2(nor,1) = r
                  dr2(nor,2) = xcd
                  dr2(nor,3) = ycd
                  dr2(nor,4) = zcd
                endif
               endif
              enddo
            enddo
          enddo
        else
          projr = xcrd*ru1x + ycrd*ru1y + zcrd*ru1z
          max1u = (rp-projr)*ra + nadd
          max1l = (rp+projr)*ra + nadd
          max1l1 = max1l + 1
          xcdi = xcrd - max1l1*r1x
          ycdi = ycrd - max1l1*r1y
          zcdi = zcrd - max1l1*r1z
!
!  Loop over unit cells to find interatomic distances
!
          do ii = -max1l,max1u
            xcdi = xcdi + r1x
            ycdi = ycdi + r1y
            zcdi = zcdi + r1z
!
            projr = xcdi*ru2x + ycdi*ru2y + zcdi*ru2z
            max2u = (rp-projr)*rb + nadd
            max2l = (rp+projr)*rb + nadd
            max2l1 = max2l + 1
!
            xcdj = xcdi - max2l1*r2x
            ycdj = ycdi - max2l1*r2y
            zcdj = zcdi - max2l1*r2z
            do jj = -max2l,max2u
              xcdj = xcdj + r2x
              ycdj = ycdj + r2y
              zcdj = zcdj + r2z
!
              projr = xcdj*ru3x + ycdj*ru3y + zcdj*ru3z
              perpr = xcdj*xcdj + ycdj*ycdj + zcdj*zcdj
              perpr = perpr - projr*projr
              perpr = cut2 - perpr
              perpr = sqrt(abs(perpr))
              max3u = (perpr-projr)*rc + nadd
              max3l = (perpr+projr)*rc + nadd
              max3l1 = max3l + 1
!
              xcd = xcdj - max3l1*r3x
              ycd = ycdj - max3l1*r3y
              zcd = zcdj - max3l1*r3z
              do kk = -max3l,max3u
               xcd = xcd + r3x
               ycd = ycd + r3y
               zcd = zcd + r3z
               if (ii.ne.0.or.jj.ne.0.or.kk.ne.0.or.i.ne.j) then
                r = xcd*xcd + ycd*ycd + zcd*zcd
                if (r.lt.smallself) then
                  if (lprint) write(ioout,*) 'atoms too close!'
                  cost = cost + 5000000.0_dp
                elseif (r.le.cut2) then
                  nor = nor + 1
                  dr2(nor,1) = r
                  dr2(nor,2) = xcd
                  dr2(nor,3) = ycd
                  dr2(nor,4) = zcd
                endif
              endif
              enddo
            enddo
          enddo
        endif
      endif
      if (nor.eq.0) goto 1110
      if (nor.gt.maxdis) then
        if (ioproc) &
        write(ioout,'(/,''  **** Number of distances in costfn exceeds limit ****'',/)')
        call stopnow('costfn')
      endif
!
!  Calculate Cost Function
!
      if (oxi.lt.0.and.oxj.lt.0) then
!           anion---anion penalise more heavily

        do ir = 1,nor
          r = dr2(ir,1)
          r = sqrt(r)
          rm1 = 1/r
          dcost = kscf*10.0_dp*dexp(rij-r*ionic)
          dcoul = kqacf*abs(oxij)*rm1
          if(ver.eq.1)then
           dcoul = (sm0*dexp(1.0_dp-(r/rp))-sm0)*dcoul
          elseif(ver.eq.2)then
           dcoul = (sm0*dcosh(r-rp)-sm0)*dcoul
          endif
          cost = cost+dcost+dcoul
!1.1          if (smin.lt.r.and.r.lt.smax) then
!1.1            coord2o = coord2o+1
!1.1          elseif (r.lt.smin) then
!1.1            cost = cost+200.0_dp
!1.1          endif
        enddo

      elseif (oxi.lt.0.or.oxj.lt.0) then
!           cation-anion include/compute coord

        do ir = 1,nor
          r = dr2(ir,1)
          r = sqrt(r)
          dcost = kbcf*dexp(rij-r*bond)
          if (r.gt.rmax) then
           if(ver.eq.2)then
            dcost = 0.5_dp*(1.0_dp+dcos(sm1*(r-rmax)))*dcost
            doxi = doxi+dcost
           else
            cost = cost+dcost
           endif
          else
           doxi = doxi+dcost
           if (r.gt.rmin) then
             coord = coord+1
             if(j.eq.1)li = .true.
           endif
          endif
        enddo

      else
!           metal---metal
!           cation-cation

        do ir = 1,nor
          r = dr2(ir,1)
          r = sqrt(r)
          rm1 = 1.0_dp/r
          dcost = kscf*dexp(rij-r*ionic)
          dcoul = kqccf*abs(oxij)*rm1
          if(ver.eq.1)then
           dcoul = (sm0*dexp(1.0_dp-(r/rp))-sm0)*dcoul
          elseif(ver.eq.2)then
           dcoul = (sm0*dcosh(r-rp)-sm0)*dcoul
          endif
          cost = cost+dcost+dcoul
!1.1          if (smin.lt.r.and.r.lt.smax) then
!1.1            coord2m = coord2m+1
!1.1          elseif (r.lt.smin) then
!1.1            cost = cost+200.0_dp
!1.1          endif
        enddo

      endif

1110  continue
    enddo
!
!  end of j loop
!
!  now write out calculaed co-ordination numbers and adjust cost fn
!
    if (lprint.and.lopt.and.lpredict) then
      write(ioout,'('' coordination number of'',i3,'' is'',f6.2)') &
      nati,coord
!1.1     if(oxi.lt.0) then
!1.1      write(ioout,'('' second coord number of'',i3,'' is'',f6.2)')
!1.1 &    nati,coord2o
!1.1     else
!1.1      write(ioout,'('' second coord number of'',i3,'' is'',f6.2)')
!1.1 &    nati,coord2m
!1.1     endif
    endif
    dc = coord-cni
!1.1    dcm = k2ccf*(coord2m-4)
!1.1    dco = k2acf*(coord2o-6)
    if (oxi.ge.0) then
!1.1      cost = cost + dcm*dcm
      kccf = kcccf
      dox = (doxi-oxi)
    else
!1.1      cost = cost + dco*dco
      kccf = kcacf
      dox = (doxi+oxi)
    endif
    cost = cost + kbcf*dox*dox
    if(i.eq.1)then
      cost = cost + kacf*kacf*kccf*dc*dc
    elseif(li)then
      cost = cost + kacf*kccf*dc*dc
    else
      cost = cost + kccf*dc*dc
    endif

  enddo
!
!  end of i loop
!
  if (.not.lnadd) nadd = 0
  time2 = cputime()
  tatom = tatom + time2 - time1
!
  return
  end
