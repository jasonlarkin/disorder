  subroutine predict(mcfg)
!
!  Subroutine for obtaining possible crystal structure
!  Structures to be optimised are stored in xbest(,)
!
!  Scott Woodley, R.I.G.B. , June 1997
!  Julian Gale, Curtin University, May 2009
!
!  07/08/97 call to gasort added (smw)
!  06/03/98 allow origin change + no energy min (smw)
!  27/07/00 converted s.t. multi configs allowed (smw)
!  29/05/09 Calls to x0tostr routines moved from energy to calling routine
!
!  mvar = max #variables and thus used in dimensions of arrays
!  nvar = actual #variables and thus used in do loops of calcs
!  mcfg = #configurations to optimise upon return
!  xconf(mvar,2*maxgcfg) array to store parent+children configs
!  xbest(mvar,maxbcfg)   array with best configs to optimise
!
  use configurations
  use control
  use current
  use energies, only : fcstore
  use files
  use fitting,  only : maxfit
  use gaconf
  use general
  use genetic
  use iochannels
  use optimisation
  use parallel
  use two
  use xcgc
  implicit none
!
!  Passed variables
!
  integer(i4)       :: mcfg
!
!  Local variables
!
  integer(i4)       :: i
  integer(i4), save :: icost
  integer(i4)       :: ifail
  integer(i4)       :: iflag
  integer(i4)       :: ind
  integer(i4)       :: indc
  integer(i4), save :: isseed
  integer(i4)       :: j
  integer(i4)       :: mc
  integer(i4)       :: nbestcfg
  logical           :: lchanged
  logical           :: lfreeloc
  logical           :: lfreezeok
  logical           :: lgaopt
  logical           :: lglobal
  logical           :: lprint
  real(dp)          :: fc
  real(dp)          :: cputime
  real(dp)          :: estore
  real(dp)          :: time1
!
  lglobal = (index(keyword,'glob').ne.0)
!
  if (.not.lgadef) then
    call outerror('no contents specified for prediction',0_i4)
    call stopnow('predict')
  endif
!
!  Setup of parameters for arrays
!
  mvar = 3*nascfg(ncf) + 6
!
!  Check array dimensions
!
  lchanged = .false.
  if (ngacfg.gt.maxgcfg) then
    maxgcfg = ngacfg
    lchanged = .true.
  endif
  if (ngabest.gt.maxbcfg) then
    maxbcfg = ngabest
    lchanged = .true.
  endif
  if (mvar.gt.maxmvar) then
    maxmvar = mvar
    lchanged = .true.
  endif
  if (lchanged) then
    call changemaxgbcfg
  endif
  if (nvar.gt.maxfit) then
    maxfit = nvar 
    call changemaxfit
  endif
!
!  Banner
!
  if (ioproc.and.ncf.eq.1) then
    write(ioout,'(/,'' Welcome to GULPs General Algorithm for Structure Prediction'')')
    write(ioout,'(''              by Simulated Annealing or Genetics!'',/)')
    write(ioout,'(/,''--------------------------------------------------------------------------------'')')
  endif
!
!  Check whether potentials other than cost function specified within
!  input file. If so then switch on cost function keyword.
!
  i = icost
  if (ncf.gt.1.and.npote.ne.0.and.i.ne.0) then
    keyword(i:i+4) = 'cost'
  endif
  if (ncf.eq.1) then
    isseed = iseed
  else
    iseed = isseed
  endif

  lprint = (index(keyword,'debug').ne.0)
  if (.not.ioproc) lprint = .false.
  lfreeloc = (lfree.and.temperature.gt.0.0_dp)
  if (ndim.ne.3) lfreeloc = .false.
  lfreezeok = (index(keyword,'noex').eq.0.and.ncell.eq.0.and..not.lfreeloc)
  lgaopt = (index(keyword,'gene').ne.0)
!
  if (nbsmat.gt.0.or.lbulknoopt.or.lfreeloc.or..not.lopt.or.lfit) then
    if (ioproc) then
      write(ioout,'(/,''There exists a problem in predict.f!!'',/)')
    endif
!       call stopnow('predict')
  endif
!
!  Set initial atom (which is not randomised) at (0,0,0)?
!
  if (dabs(xafrac(1)).gt.1.0d-5.or.dabs(yafrac(1)).gt.1.0d-5.or.dabs(zafrac(1)).gt.1.0d-5) then
    if (lprint) then
      write(ioout,'(''            origin or 1st atom moved to'')')
      write(ioout,*)xafrac(1),yafrac(1),zafrac(1)
    endif
    x0(7) = xafrac(1)
    x0(8) = yafrac(1)
    x0(9) = zafrac(1)
  else
    x0(7) = 0.0_dp
    x0(8) = 0.0_dp
    x0(9) = 0.0_dp
  endif
  if (mvar.ne.3*nasym+6) call stopnow('predict')
  if (nvar.gt.0) then
    if (lfreezeok) then
!
!  Set freezing flags
!
      do i = 1,nasym
        lopf(i) = .false.
      enddo
      do i = 1,nvar
        ind = iopt(i)
        if (ind.gt.mvar) then
          ind = ind-mvar
          lopf(ind) = .true.
        elseif (ind.gt.6) then
          ind = ind-7
          ind = (ind/3) + 1
          lopf(ind) = .true.
        endif
!
!  Check for constrained atoms
!
        if (ncon.gt.0) then
          do j = 1,ncon
            if (iopt(i).eq.ncvar(j + ncfst)) then
              indc = ncfix(j + ncfst)
              if (indc.gt.mvar) then
                indc = indc-mvar
                lopf(indc) = .true.
              elseif (indc.gt.6) then
                indc = indc-7
                indc = (indc/3) + 1
                lopf(indc) = .true.
              endif
            endif
          enddo
        endif
      enddo
    endif
  elseif (nvar.eq.0) then
    if (ioproc) then
      write(ioout,'(''  ** ERROR - no variables specified for Prediction Routines**'')')
    endif
    call stopnow('predict')
  endif
  lfirst = .true.
  ifail = 1
  lfreeze = lfreezeok
  if (ioproc) call gflush(ioout)
!
!  Choose method and start calculations
!
  if (lgaopt) then
    call gaopt(nvar,ifail,xc,fc)
    lfreeze = .false.
    iflag = 0
    if (ngabest.eq.0) then
      nbestcfg = ngacfg
      do j = 1,nbestcfg
        ithbest(j) = j
      enddo
    elseif (ngabset.ne.0) then
      nbestcfg = ngabest*maxgacyc/ngabset
      do j = 1,nbestcfg
        ithbest(j) = j + mgacfg
      enddo
    else
      nbestcfg = ngabest
      do j = 1,nbestcfg
        ithbest(j) = j + mgacfg
      enddo
    endif
  elseif (lanneal) then
    if (ngabest.eq.0) then
      nbestcfg = 10
    else
      nbestcfg = ngabest
    endif
    do j = 1,nbestcfg
      ithbest(j) = j
    enddo
    do i = 1,nbestcfg
!          call anneal(nvar,ifail,xc,fc)
! xconf(j,1) = current best   xc(j)=best ever
      call funct(iflag,nvar,xc,fc,gc)
      do j = 1,nvar
        xbest(j,i) = xc(j)
      enddo
    enddo
    iflag = 0
    if (index(keyword,'cost').ne.0) lgacost = .true.
    do i = 1,nbestcfg
      do j = 1,nvar
        xc(j) = xbest(j,i)
      enddo
      call funct(iflag,nvar,xc,fc,gc)
      fconf(i) = fc
    enddo
    lgacost = .false.
  else
    if (ioproc) then
      write(ioout,'(''KEYWORD ERROR: predict keyword specified thus either'')')
      write(ioout,'(''                  keyword genetic or anneal required'')')
      write(ioout,'(''where genetic  = > use a genetic algorithm'')')
      write(ioout,'(''      anneal   = > use a simulated annealing algorithm'')')
    endif
    call stopnow('predict')
  endif
!
!  Report results
!
  if (lprint) then
    if (.not.lglobal) write(ioout,*)'Will optimise structures with the following' 
    if (index(keyword,'cost').ne.0) then
      write(ioout,*)'values of Pannetier type cost function:-'
    else
      write(ioout,*)'values of ground state energy:-'
    endif
    do j = 1,nbestcfg
      write(ioout,*)j,ithbest(j),fconf(ithbest(j))
    enddo
  endif
  if (iflag.eq.1.and.lgaopt) then
    do j = 1,nbestcfg
      do i = 1,nvar
        xbest(i,j) = xconf(i,ithbest(j))
      enddo
    enddo
  endif
!
!  Has the calculation been successful?
!
  if (ioproc) then
    write(ioout,'(/)')
  endif
  if (ifail.lt.0) then
    if (ioproc) then
      write(ioout,'(''  **** CPU limit has been exceeded - restart optimisation ****'',/)')
    endif
    call stopnow('predict')
  endif
  time1 = cputime()
  time1 = time1-time0
!
!  Check whether potentials other than cost function specified within
!  input file. If so then switch off cost function keyword.
!
  icost = index(keyword,'cost')
  i = icost
  if (npote.ne.0.and.i.ne.0) then
    keyword(i:i+4) = '    '
  endif
!
!  Output data
!
  if (ioproc) then
    write(ioout,'(/,''  Time for Global Optimiser  =  '',f12.4,''   seconds'',/)') time1
  endif
  mcfg = nbestcfg
  if (index(keyword,'glob').ne.0) then
    estore = fcstore
    mc = 1
    call gabcfg(mc,mcfg)
    call outfile
    if (larc) then
      call setup(.false.)
!
!  Convert linear structure array to main structure arrays
!
      if (lx0centroid) then
        call x0tostrcentroid
      else
        call x0tostr
      endif
      call energy(fcstore,.false.,.false.)
      call outarc(16_i4,.false.,.false.)
    endif
    do mc = 2,mcfg
      call gabcfg(mc,mcfg)
      call outfile
      if (larc) then
        call setup(.false.)
!
!  Convert linear structure array to main structure arrays
!
        if (lx0centroid) then
          call x0tostrcentroid
        else
          call x0tostr
        endif
        call energy(fcstore,.false.,.false.)
        call outarc(16_i4,.true.,.false.)
      endif
    enddo
    mcfg = 0
    fcstore = estore
  endif
!
  return
  end
