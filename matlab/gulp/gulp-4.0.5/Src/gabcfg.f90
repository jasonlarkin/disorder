  subroutine gabcfg(mc,mcfg)
!
!  Subroutine for obtaining possible crystal structure
!  Structures to be optimised are stored in xbest(,)
!
!  11/06 lfirst argument added to equpos call
!
!  Scott Woodley, R.I.G.B., June 1997
!  Julian Gale, NRI, Curtin University, November 2006
!
  use configurations
  use control
  use current
  use dump
  use files
  use gaconf, only : xbest, ithbest, mvar
  use genetic
  use iochannels
  use parallel
  use symmetry
  implicit none
!
!  Passed variables
!
  integer(i4)                            :: mc
  integer(i4)                            :: mcfg
!
!  Local variables
!
  integer(i4)                            :: i
  integer(i4)                            :: i0
  integer(i4)                            :: iend
  integer(i4)                            :: ii
  integer(i4)                            :: j
  integer(i4)                            :: k
  integer(i4)                            :: mmc
  integer(i4)                            :: nc
  integer(i4), dimension(:), allocatable :: ncount
  integer(i4),                      save :: nend
  integer(i4)                            :: status
  logical                                :: lfound
  logical                                :: lstuck
  real(dp)                               :: diff
  real(dp),                         save :: x, y, z
  real(dp)                               :: xstr(6)
!
  lstuck = (index(keyword,'hhhh').ne.0)
!
  if (ioproc) then
    write(ioout,'(''++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'')')
    if (.not.lstuck) then
      write(ioout,'(''################################################################################'')')
    endif
    write(ioout,*)'         ',mc-1,' structures optimised out of ',mcfg
    if (.not.lstuck) then
      write(ioout,'(''################################################################################'')')
      write(ioout,'(''++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'')')
    endif
  endif

  if ((lrelax.or.lcomp).and.mcfg.ne.1) then
    call outerror('problem in gabcfg',0_i4)
    call stopnow('gabcfg')
  endif
!
!  Select appropriate element
!
  if (lgaconjg) then
    mmc = ithbest(mc)
  else
    mmc = mc
  endif
!
!  Set initial atom (which is not randomised) at (0,0,0)?
!
  if (mc.eq.1) then
    x = x0(7)
    y = x0(8)
    z = x0(9)
  else
    x0(7) = x
    x0(8) = y
    x0(9) = z
  endif
!
!  Substitute parameters into place
!
  do i = 1,6
    x0(i) = 1.0_dp
  enddo
  do i = 1,nvar
    x0(ioptcfg(i+nfst)) = xbest(i,mmc)
  enddo
!
!  Change output filename for insight package
!
  if (.not.lgaconjg) then
    if (lxtl) then
      if (ncf.eq.1.and.mc.eq.1) then
        nend = index(xtlfile,'.xtl')
        if (nend.gt.1) then
          if (nend.gt.20) then
            nend = 20
            xtlfile(20:30) = '.xtl       '
          endif
          xtlfile(nend-1:nend-1) = char(48+ncf)
        elseif (index(names(ncf),' ').ne.1) then
          xtlfile(1:4) = names(ncf)(1:4)
          xtlfile(5:8) = '.xtl'
        else
          xtlfile(1:3) = 'str'
          xtlfile(4:4) = char(48+ncf)
          xtlfile(5:8) = '.xtl'
        endif
      elseif (mc.eq.1) then
        if (nend.gt.0) then
          xtlfile(nend-1:nend-1) = char(48+ncf)
        elseif (index(names(ncf),' ').ne.1) then
          xtlfile(1:4) = names(ncf)(1:4)
          xtlfile(5:8) = '.xtl'
        else
          xtlfile(1:3) = 'str'
          xtlfile(4:4) = char(48+ncf)
          xtlfile(5:8) = '.xtl'
        endif
      endif
      iend = index(xtlfile,'.xtl')
      if (mc.ne.1) iend = iend - 2
      if (iend.gt.25) iend = iend - 5
      if (mc.gt.30) then
        if (ioproc) write(ioout,*)'Too many structures to optimise!'
        call stopnow('gabcfg')
      elseif (mc.gt.20) then
        xtlfile(iend:iend) = '2'
        nc = mc - 20
      elseif (mc.gt.10)  then
        xtlfile(iend:iend) = '1'
        nc = mc - 10
      else
        xtlfile(iend:iend) = '0'
        nc = mc
      endif
      iend = iend + 1
      i0 = ichar('0')
      i0 = i0 - 1
      xtlfile(iend:iend) = char(nc+i0)
      xtlfile(iend+1:iend+4) = '.xtl'
    endif
!
!  Similarly for (individual restart) dump files
!
    if (idump.eq.12) then
      iend = index(dfile,' ')
      if (mc.ne.1) then
        iend = iend - 2
      endif
      if (iend.gt.25) iend = iend - 5
      if (mc.gt.30) then
        if (ioproc) write(ioout,*)'Too many structures to optimise!'
        call stopnow('gabcfg')
      elseif (mc.gt.20) then
        dfile(iend:iend) = '2'
        nc = mc - 20
      elseif (mc.gt.10)  then
        dfile(iend:iend) = '1'
        nc = mc - 10
      else
        dfile(iend:iend) = '0'
        nc = mc
      endif
      iend = iend + 1
      i0 = ichar('0')
      i0 = i0 - 1
      dfile(iend:iend)=char(nc+i0)
    endif
  endif
!
!  Apply constraints
!
  if (ncon.gt.0) then
    do i = 1,ncon
      x0(ncfix(i+ncfst)) = 0.0_dp
    enddo
    do i = 1,ncon
      x0(ncfix(i+ncfst)) = x0(ncvar(i+ncfst))*conco(i+ncfst) + conadd(i+ncfst) + x0(ncfix(i+ncfst))
    enddo
!
!  Handle additive constraints for fractional coordinates
!  - take nearest pair of images
!
    allocate(ncount(mvar),stat=status)
    if (status/=0) call outofmemory('gabcfg','ncount')
    do i = 1,mvar
      ncount(i) = 0
    enddo
    do i = 1,ncon
      ii = ncfix(i+ncfst)
      ncount(ii) = ncount(ii) + 1
    enddo
    do i = 7,mvar
      if (ncount(i).ge.2) then
        lfound = .false.
        j = 0
        do while (.not.lfound.and.j.lt.ncon-1)
          j = j + 1
          if (ncfix(j+ncfst).eq.i) then
            k = j
            do while (.not.lfound.and.k.lt.ncon) 
              k = k + 1
              lfound = (ncfix(k+ncfst).eq.i)
            enddo
          endif
        enddo
        if (lfound) then
          diff = abs(x0(ncvar(j+ncfst))-x0(ncvar(k+ncfst)))
          if (diff.gt.0.5) then
            x0(i) = x0(i) + 0.5_dp
            x0(i) = mod(x0(i),1.0_dp)
          endif
        endif
      endif
    enddo
    deallocate(ncount,stat=status)
    if (status/=0) call deallocate_error('gabcfg','ncount')
  endif
  if (ncell.gt.0) then
!
!  Apply strains
!
    do i = 1,3
      rv(1,i) = rvcfg(1,i,ncf)
      rv(2,i) = rvcfg(2,i,ncf)
      rv(3,i) = rvcfg(3,i,ncf)
    enddo
    do i = 1,6
      xstr(i) = x0(i) - 1.0_dp
    enddo
    call strain3D(xstr,rv)
    call uncell3D(rv,a,b,c,alpha,beta,gamma)
    if (.not.lrelax) then
      do i = 1,3
        rvcfg(1,i,ncf) = rv(1,i)
        rvcfg(2,i,ncf) = rv(2,i)
        rvcfg(3,i,ncf) = rv(3,i)
      enddo
    endif
  endif
!
  if (.not.lrelax) then
    do i = 1,nasym
      xafrac(i) = dmod(x0(3*i+4)+1.0_dp,1.0_dp)
      yafrac(i) = dmod(x0(3*i+5)+1.0_dp,1.0_dp)
      zafrac(i) = dmod(x0(3*i+6)+1.0_dp,1.0_dp)
    enddo
    if (lsymopt) then
      call equpos(.true.,.false.)
    else
      do i = 1,numat
        xfrac(i) = xafrac(i)
        yfrac(i) = yafrac(i)
        zfrac(i) = zafrac(i)
      enddo
    endif
  endif
!
  return
  end
