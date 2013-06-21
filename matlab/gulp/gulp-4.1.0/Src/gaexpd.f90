  subroutine gaexpd(ncfg,mcfg,nvar,na,nb,ibest,xc,gc,xmin,xmax,ndiscret,iseed)
!
!  Subroutine to develope the children into parents. Here
!  the possibility of expansion is considered. ibest used
!  to find worse children (new parents). New random parents
!  replace worst na new parents. Return ibest with the
!  current best(1) and worst(2) parent (configuration).
!  mcfg = max # configs    ncfg = current # configs
!
!  Scott Woodley, R.I.G.B. , Sept 1997
!
!  6/98 modified to expand with random foreigners (smw)
!
  use gaconf, only : xconf, fconf
  use iochannels
  use parallel
!
  implicit none
  integer(i4) ncfg,mcfg,nvar,na,nb,ibest(nb),ndiscret(*),iseed
  real(dp)    xc(*),gc(*),xmin(*),xmax(*)
!
  integer(i4) i,j,k,ihigh,ilow,iflag,ii
  real(dp)    fhigh,flow,fc
  logical     ldiff
!
  iflag=0
!
!  Test array sizes are okay
!
  if (na.gt.nb) then
    call outerror('array bounds problem in gaexpd - na > nb',0_i4)
    call stopnow('gaexpd')
  endif
  if (2*na.gt.ncfg) then
    call outerror('array bounds problem in gaexpd - 2*na > ncfg',0_i4)
    call stopnow('gaexpd')
  endif
!
!  Evaluate Pannetier type cost function or energy of system
!  for new children and find the best and worst config(child)
!
  if (na.eq.0) then
    ilow=0
    flow=1.0d20
    ihigh=0
    fhigh=-1.0d20
  else
    do i=1,na
      do j=1,nvar
         xc(j)=xconf(j,mcfg+i)
         xconf(j,i)=xc(j)
      enddo
      call funct(iflag,nvar,xc,fc,gc)
      fconf(i)=fc
    enddo
    ilow=1
    flow=fconf(ilow)
    ihigh=1
    fhigh=fconf(ihigh)
  endif
  do i=1+na,ncfg
    do j=1,nvar
      xc(j)=xconf(j,i+mcfg)
      xconf(j,i)=xc(j)
    enddo
    call funct(iflag,nvar,xc,fc,gc)
    fconf(i)=fc
!  Find current best configuration (initially best old parent)
    if (fc.lt.flow) then
      ilow=i
      flow=fc
    endif
!  Find current worst configuration (initially unknown)
    if (na.eq.0.and.fc.gt.fhigh) then
      ihigh=i
      fhigh=fc
    endif
  enddo
!
!  If na<>0 then generate some random/new parents (Emmits!)
!  note that it is now possible to overwrite old children
!
  ldiff=.false.
  call gacreate(na,2_i4*mcfg,nvar,ndiscret,xmin,xmax,iseed,ldiff)
!
!  Need to find the worse na new parents
!
  call gasort(ibest,na,fconf,ncfg,0.0_dp,-1_i4)
  ihigh=ibest(1)
  fhigh=fconf(ihigh)
!
!  Now kill worst new parents for the foreigners
!
  ii=0
  k=2*mcfg+1
  do i=1,na
     if (ncfg+i.le.mcfg) then
       do j=1,nvar
          xc(j)=xconf(j,k-i)
          xconf(j,ncfg+i)=xc(j)
       enddo
       call funct(iflag,nvar,xc,fc,gc)
       fconf(ncfg+i)=fc
       if (fc.lt.flow) then
          ilow=ncfg+i
          flow=fc
       endif
       if (fc.gt.fhigh) then
          ihigh=ncfg+i
          fhigh=fc
       endif
     else
       ii=ii+1
       do j=1,nvar
          xc(j)=xconf(j,k-i)
          xconf(j,ibest(ii))=xc(j)
       enddo
       call funct(iflag,nvar,xc,fc,gc)
       fconf(ibest(ii))=fc
       if (fc.lt.flow) then
          ilow=ibest(ii)
          flow=fc
       endif
       if (fc.gt.fhigh) then
          ihigh=ibest(ii)
          fhigh=fc
       endif
     endif
  enddo
!
!  Check for expansion of population
!
  if (ncfg.ne.mcfg) then
    ncfg=ncfg+na
    if (ncfg.gt.mcfg) then
      ncfg=mcfg
    endif
    if (ioproc) then
      write(ioout,*)' Expanded population to ',ncfg
    endif
  endif
!
!  Fill ibest(1) with best parent, ibest(2) with 'worst' parent
!
  ibest(1)=ilow
  ibest(2)=ihigh
  return
  end
