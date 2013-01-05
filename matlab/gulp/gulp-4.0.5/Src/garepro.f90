  subroutine garepro(ngacfg,mcfg,nvar,lgaexpw,indax,maxbest,pts,iseed,udif,nspar)
!
!  Reproduction step of genetic algorithm by tournament selection
!  with either a probability pts or exponentially weighted wrt fn
!
!   6/98 Changed where parents trying again are kept (smw)
!   1/08 random -> GULP_random
!
!  Scott Woodley  Royal Institution   September 1997
!   Julian Gale   of Great Britain    September 1993
!
  use gaconf, only : fconf, xconf
  use iochannels
  use parallel
  implicit none
!
!  Passed variables
!
  integer(i4) :: ngacfg,mcfg,nvar,maxbest,iseed,nspar
  integer(i4) :: indax(maxbest)
  real(dp)    :: pts,udif
  logical     :: lgaexpw
!
!  Local variables
!
  integer(i4) :: i,j,k,isub,next,n1,n2,nt,nsel,ii
  real(dp)    :: bestfn,avfn,theshold,a,b,sumfit
  real(dp)    :: rn1,rn2,parfit,rnts,GULP_random
  parameter(theshold=5000.0_dp)
  logical     :: localgew
!
  localgew = lgaexpw
!
!  Check that there are candidates under theshold
!
  if (lgaexpw) then
    lgaexpw = .false.
    do i = 1,ngacfg
      if (fconf(i).lt.theshold) lgaexpw = .true.
    enddo
    if (ioproc.and..not.lgaexpw)then
      write(ioout,*)'theshold too low switching to tourn'
    endif
  endif
!
!  Find best parent for weight fn or nspar parents trying again
!
  if (nspar.gt.0) then
    call gasort(indax,nspar,fconf,ngacfg,udif,1_i4)
  elseif (lgaexpw) then
    nspar = 1
    call gasort(indax,nspar,fconf,ngacfg,udif,1_i4)
    nspar = 0
  endif
!
!  Set up exponential weights if required
!  weight(i)=exp{-0.95*(bestfn-fn(i))/(bestfn-avfn)}
!
  if (lgaexpw) then
    bestfn = fconf(indax(1))
    avfn = 0.0_dp
    sumfit = 0.0_dp
    isub = 0
    do i = 1,ngacfg
      if (fconf(i).lt.theshold) then
        avfn = avfn + fconf(i)
      else
        isub = isub + 1
      endif
    enddo
    avfn = avfn/(dble(ngacfg-isub))
    b = 0.95_dp/(bestfn-avfn)
    a = dexp(-b*bestfn)
    do i = 1,ngacfg
      if (fconf(i).lt.theshold) then
        fconf(i) = a*dexp(b*fconf(i))
        sumfit = sumfit + fconf(i)
      else
        fconf(i) = 0.0_dp
      endif
    enddo
    next = 2
  else
    next = 1
  endif
!
!  Loop over #configurations - #parents trying again
!
  do i = 1+nspar,ngacfg,next
!
!  Select two different configurations at GULP_random
!
    rn1 = GULP_random(iseed,1_i4)
    if (lgaexpw) then
      n1 = ngacfg
      parfit = 0.0_dp
      rn1 = rn1*sumfit
      do while (parfit.lt.rn1.and.n1.ne.1)
        n1 = n1 - 1
        parfit = parfit + fconf(n1)
      enddo
    else
      n1 = rn1*dble(ngacfg) + 1
    endif
    n2 = n1
    do while (n2.eq.n1)
      rn2 = GULP_random(iseed,1_i4)
      if (lgaexpw) then
        nt = 0
        parfit = 0.0_dp
        do while (parfit.lt.rn2*sumfit)
          nt = nt + 1
          parfit = parfit + fconf(nt)
        enddo
        n2 = nt
        if (n2.eq.0)then
          if (ioproc)then
            write(ioout,*) 'sumfit=',sumfit
            write(ioout,*) 'parfit=',parfit
            write(ioout,*) 'rn1=',rn1
          endif
          call stopnow('garepro')
        endif
      else
        n2 = rn2*dble(ngacfg) + 1
      endif
    enddo
!
!  If tournament based select which one succeeds to next cycle
!
    if (lgaexpw) then
      nsel = n1
    else
      rnts = GULP_random(iseed,1_i4)
      if (rnts.lt.pts) then
        if (fconf(n1).lt.fconf(n2)) then
          nsel = n1
        else
          nsel = n2
        endif
      else
        if (fconf(n1).gt.fconf(n2)) then
          nsel = n1
        else
          nsel = n2
        endif
      endif
    endif
!
!  Store selected configuration(s)
!
    ii = i
    do j = 1,next
      do k = 1,nvar
        xconf(k,ii+mcfg) = xconf(k,nsel)
      enddo
      nsel = n2
      ii = ii + 1
    enddo
  enddo
!
!  Move best Parents trying again into Children array space
!
  do i = 1,nspar
    do j = 1,nvar
      xconf(j,i+mcfg) = xconf(j,indax(i))
    enddo
  enddo

  lgaexpw = localgew
  return
  end
