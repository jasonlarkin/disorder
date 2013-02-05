!  The smaller subroutines used in gaopt.f are kept here gasubs.f
!
!  Scott Woodley, Royal Institution of G.B., June 1998
!
!  precj   : Set up arrays before calling GULP's conjugate grad. min.
!  gastore : Store ngabest optimal configurations in xbest() 
!

  subroutine precj(nbest,nvar,xc,ithbest,ncfg,m)

!
!  Set up arrays before calling gulp to optimise using conjugate grads.
!
  use control
  use current, only : x0
  use gaconf,  only : xconf, fconf
  use iochannels
  use parallel
!
  implicit none
  real(dp)           :: xc(*)
  integer(i4)        :: nbest,ithbest(*),nvar,m
  real(dp)           :: fc
  real(dp)           :: gcd
  character(len=400) :: ksafe
  integer(i4)        :: mc,j,iflag,i,ncfg

  iflag = 0
!
!  Trick gulp into executing a conjugate minimisation
!
  ksafe = keyword
!  For more info keyword="opti conj conp verb cost"
  if (m.ne.0) then
    keyword = "opti conj conp hhhhs cost"
  else
    keyword = "opti conj conp hhhh cost"
!
!  Find the top ten predicted structures
!
    call gasort(ithbest,nbest,fconf,ncfg,0.0_dp,1_i4)
  endif
!
!  Minimise only the odd configs out of top nbest
!
  do mc = 1,nbest,2
!
!  Load coords into gulp arrays
!
    call gabcfg(mc,nbest)
    do j = 1,nvar
      xc(j) = xconf(j,ithbest(mc))
    enddo
    call funct(iflag,nvar,xc,fc,gcd)
    if (ioproc) write(ioout,*)'costfn(',ithbest(mc),') =',fc
!
!  Perform minimisation
!
    call optim(.false.,.false.,0_i4)
!
!  Print and store results
!
    do i = 1,nvar
      xconf(i,ithbest(mc)) = x0(i+9)
    enddo
    do j = 1,nvar
      xc(j) = xconf(j,ithbest(mc))
    enddo
    call funct(iflag,nvar,xc,fc,gcd)
    fconf(ithbest(mc)+m) = fc
    if (ioproc)write(ioout,*)'costfn(',ithbest(mc),') =',fc
  enddo
  if (ioproc) then
    write(ioout,'(''++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'')')
  endif
!
!  Return to genetic algorithm
!
  keyword = ksafe
  return
  end

!==GASTORE====GASTORE====GASTORE====GASTORE====GASTORE====GASTORE==

  subroutine gastore(ngabest,ibest,iworst,ithbest,mgacfg,nvar)
!
!  Store optimal configurations
!
  use datatypes
  use gaconf, only : fconf, xconf, xbest
!
  implicit none
  integer(i4) :: ngabest,ibest,iworst,ithbest(*),mgacfg,nvar

  integer(i4) :: i,j,k
  real(dp)    :: fbest,fworst,fconfj

  fbest = fconf(ibest)
  fworst = fconf(iworst)
  do i = 1,ngabest
    j = ithbest(i)
    fconfj = fconf(j)
    if (fconfj.lt.fbest) then
      fbest = fconfj
      ibest = iworst
    endif
    if (fconfj.lt.fworst) then
!
!  Check that we have a new configuration
!
      do k = mgacfg+1,mgacfg+ngabest
        if (fconfj.eq.fconf(k)) goto 1
      enddo
!
!  Switch in new configuration
!
      fconf(iworst) = fconfj
      iworst = iworst - mgacfg
      do k = 1,nvar
        xbest(k,iworst) = xconf(k,j)
      enddo
!
!  Find new worst configuration
!
      fworst = - 1.0d20
      do j = mgacfg+1,mgacfg+ngabest
        if (fconf(j).gt.fworst) then
          fworst = fconf(j)
          iworst = j
        endif
      enddo
1     continue
    endif
  enddo
  return
  end

!==GAPROB=====GAPROB=====GAPROB=====GAPROB=====GAPROB=====GAPROB===

  subroutine gaprob(same,pts,pcross,pmuta,prob,nbsl,ndi,lgaexpw,lanneal)
!
!  Change probabilities to simulate annealing
!
  use iochannels
  use parallel
!
  implicit none
  integer(i4) :: same,nbsl,ndi
  real(dp)    :: pts,pcross,pmuta,prob(*)
  logical     :: lgaexpw,lanneal

  same = same + 1
  if (same.lt.0) return
  if (same.eq.0) then
    if (ndi.eq.-1) ndi = 20
    same = - ndi
    if (prob(3).lt.1.0_dp) pts = pts + prob(3)
    if (prob(6).lt.1.0_dp) pcross = pcross + prob(6)
    if (prob(9).lt.1.0_dp) pmuta = pmuta + prob(9)
    if (pts.gt.prob(2))then
      pts = prob(2)
    endif
    if (pcross.lt.prob(5))then
      pcross = prob(5)
    endif
    if (pmuta.lt.prob(8))then
      pmuta = prob(8)
    endif
    if ((pts.lt.prob(2).and..not.lgaexpw).or.pcross.lt.prob(5).or.pmuta.lt.prob(8)) then
      continue
    else
      lanneal = .false.
      same = 1
      return
    endif
  else
    pts = prob(1)
    pcross = prob(4)
    pmuta = prob(7)
  endif

  if (ioproc) then
    write(ioout,'(''  Probabilities have been updated to :     '')')
    if (.not.lgaexpw) then
      write(ioout,'(''  Tournament = '',f16.6)') pts
    endif
    write(ioout,'(''  Crossover  = '',f16.6)') pcross
    write(ioout,'(''  Mutation   = '',f16.6)') pmuta
  endif

  return
  end

!==GASTUCK====GASTUCK====GASTUCK====GASTUCK====GASTUCK====GASTUCK==

  subroutine gastuck(same,fmark,flow,prob,md,nd,pmuta,nbsl,lanneal,lgrid)
!
!  Change probabilities to simulate annealing
!
  use iochannels
  use parallel
!
  implicit none
  integer(i4) :: same,md,nd,nbsl
  real(dp)    :: fmark,flow,prob(*),pmuta
  logical     :: lanneal,lgrid

  real(dp)    :: df,small

  same = same + 1
  if (same.lt.0) return

  df = dabs(fmark-flow)
  small = 0.000001_dp

  if (same.eq.0.and.df.lt.small) then
    if (ioproc) then
      write(ioout,*)' ************  STILL STUCK!  ***********', &
        '*************************************'
      write(ioout,*)' ************  SWITCHING ON LOCAL OPTIMI', &
        'SER! ********************************'
    endif
    same = 1000
    return
  endif
  if (same.eq.1005) then
    same = 0
    if (ioproc) then
      write(ioout,*)' ************  RETURN TO NORMAL ********', &
        '*************************************'
    endif
  endif

  if (same.eq.0) then
    fmark = flow
    return
  endif
  if (df.gt.small) then
    fmark = flow
    same = 1
    return
  endif

  if (same.eq.50) then
    if (ioproc) then
      write(ioout,*)' ************  STUCK  ******************', &
        '*************************************'
    endif
    if (prob(3).lt.1.0_dp) lanneal = .true.
    if (prob(6).lt.1.0_dp) lanneal = .true.
    if (prob(9).lt.1.0_dp) lanneal = .true.

    if (lgrid) then
      nd = md
      same = - 50
      if (ioproc) then
        write(ioout,*)' ************  CHANGING GRID SIZE ***', &
          '****************************************'
      endif
    elseif (lanneal) then
      same = - 50
      if (ioproc) then
        write(ioout,*)' ************  CHANGING MUTATION RATE', &
          '  ****(SIMULATE ANNEALING)**************'
      endif
    else
      pmuta = 0.5_dp/nbsl
      if (ioproc) then
        write(ioout,*)' ************  CHANGING MUTATION RATE', &
          '  **************************************'
      endif
    endif
  elseif (mod(same,10_i4).eq.0.and.same.gt.50) then
    pmuta = 2.0_dp*pmuta
    if (ioproc) then
      write(ioout,*)' ************  CHANGING MUTATION RATE', &
        '  **************************************'
    endif
  elseif (same.eq.99) then
    same = -50
    pmuta = prob(7)
    if (ioproc) then
      write(ioout,*)' ************  CHANGING MUTATION RATE', &
        '  **************************************'
    endif
  endif

  return
  end

!==GATIME=====GATIME=====GATIME=====GATIME=====GATIME=====GATIME===

  subroutine gatime(t1,ic,nbest,m,flow,fhigh,pb,ifail)
!
!  Check remaining cputime and output info
!
  use control
  use gaconf, only : fconf
  use general
  use iochannels
  use parallel
  use times
  implicit none
!
  integer(i4)    :: ic,nbest,m,pb,ifail
  real(dp)       :: t1,flow,fhigh
!
  logical        :: lprint
  real(dp)       :: t2,cputime
  real(dp), save :: tmax = 0.0_dp
  integer(i4)    :: i
!
  lprint = ((index(keyword,'debug').ne.0).and.ioproc)
  if(lprint.and.ic.eq.1)then
    open(unit=133,status='unknown',file='costfn')
    open(unit=134,status='unknown',file='bestfn')
  endif
  t2 = cputime()
  tdel = t2 - t1
! 1.1 tmax = t2/dble(ic)
  if (tdel.gt.tmax) tmax = tdel
  if (lprint.and.pb.ne.0) then
    if (index(keyword,'cost').ne.0) then
     write(134,'(''Cycle: '',i4,'' Best Cost functions: '')')ic
    else
     write(134,'(''Cycle: '',i4,'' Best Energies      : '')')ic
    endif
    do i = 1,nbest
     write(134,*) fconf(m+i)
    enddo
  endif
  pb = 0
  if (index(keyword,'cost').ne.0) then
    write(ioout,'(''  Cycle: '',i4,'' Cost fn : Min '',f16.6,'' Max '',f16.6,'' CPU:'',f9.3)') ic,flow,fhigh,t2
  else
    write(ioout,'(''  Cycle: '',i4,'' Energies: Min '',f16.6,'' Max '',f16.6,'' CPU:'',f9.3)') ic,flow,fhigh,t2
  endif
  if (lprint) write(133,*) ic,flow
  if (lprint) call gflush(ioout)
! 1.1 if ((timmax-t2).lt.tmax) then
  if (timmax.gt.0.0_dp.and.(timmax-t2).lt.tmax) then
    ifail = -1
  endif

  return
  end

!==GAGRID=====GAGRID=====GAGRID=====GAGRID=====GAGRID=====GAGRID===

  subroutine gagrid(nd,ndiscret,nvar,nbsl,pmuta,lanneal)
!
!  Create grid and find DNA binary string length
!
  use iochannels
  use parallel
  implicit none
!
!  Passed variables
!
  integer(i4) :: nd,ndiscret(*),nvar,nbsl
  logical     :: lanneal
  real(dp)    :: pmuta
!
!  Local variables
!
  integer(i4) :: i
  real(dp)    :: p1
!
  nd = nd + 1
  do i = 1,nvar
    ndiscret(i) = nd
  enddo
  nbsl = 0
  do i = 1,nvar
    nbsl = nbsl + ndiscret(i)
  enddo
!  Calculate the probability of one mutation per child; p1
!  using pmuta = p1 we expect to get one coord changing by
!  1,3,7,15,31 or 63 grid points for example if nd = 6
  p1 = 1.0_dp/nbsl
  if (ioproc) then
    write(ioout,*)'The number of grid points across the unit cell in'
    write(ioout,*)'any one direction has now changed to ',2**nd
  endif
  if (.not.lanneal) then
    pmuta = p1
    if (ioproc) write(ioout,*)'Thus pmuta has also changed becoming ',pmuta
  endif
  if (ioproc.and.pmuta.ne.p1) then
    write(ioout,*)'we have pmuta =',pmuta
    write(ioout,*)'and not pmuta =',p1
  endif
!
  return
  end

!==GAEVAL=====GAEVAL=====GAEVAL=====GAEVAL=====GAEVAL=====GAEVAL===

  subroutine gaeval(n1,n2,iflag,nvar,xc)
!
!  Evaluate the cost function for the configurations n1 to n2
!
  use gaconf
  use parallel
!
  implicit none
  integer(i4) :: n1,n2,iflag,nvar
  real(dp)    :: xc(*)

  integer(i4) :: i,j
  real(dp)    :: fc,gcd

  if (ioproc.and.iflag.ne.0) then
    call outerror('iflag has wrong value in gaeval',0_i4)
    call stopnow('gasubs')
  endif

  do i = n1,n2
    do j = 1,nvar
      xc(j) = xconf(j,i)
    enddo
    call funct(iflag,nvar,xc,fc,gcd)
    fconf(i) = fc
  enddo

  return
  end

!==GAHARD=====GAHARD=====GAHARD=====GAHARD=====GAHARD=====GAHARD===

  subroutine gahard(n,nvar,iflag)
!
!  Copy/read back n configurations to/from hard disc.
!
  use configurations, only : ncfg
  use gaconf
  use iochannels
  use parallel
!
  implicit none
  integer(i4) :: n,nvar,iflag

  integer(i4) :: i,j,nv

  if (ncfg.gt.1) return

  if (ioproc) then
    open(unit=99,file='ga_xconf',status='unknown')
    rewind(99)
    if (iflag.eq.1) then
      write(99,*)'Genetic configurations from pre-GULP'
      write(99,*) n,nvar
      do i = 1,n
         write(99,*)(xconf(j,i),j=1,nvar)
      enddo
    else
      read(99,*,err=98)
      write(ioout,'(''  ** Using old configurations from ga_xconf  **'')')
      read(99,*,err=99) n,nv
      if (nv.ne.nvar) then
        write(ioout,'(''ERROR  :  The number of variables in ga_xconf'')')
        write(ioout,'(''doesn`t match that implied in your input file'')')
        call stopnow('gasubs')
      endif
      do i = 1,n
        read(99,*,err=99)(xconf(j,i),j=1,nvar)
      enddo
    endif
    close(99)
  endif
  return
!
!  Error handling
!
98 n = 0
  return
99 call outerror('cannot read input data - check file ga_xconf',0_i4)
  call stopnow('gasubs')
  end
