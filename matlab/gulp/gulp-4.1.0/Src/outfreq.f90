  subroutine outfreq(leigloc,imode,freq,ncfoc)
!
!  Output frequencies plot for quick visualisation
!
!  imode = 1 => defect calculation
!  imode = 2 => cluster calculation
!
!  Channel 4 is allocated to a scratch file written by deffreq
!
!  Phonon density of states:
!
!  nbox   = no. of boxes allowed in phonon density => controls the
!           resolution of the plot
!  fbox   = box size
!  fmax   = maximum frequency
!  lbroad = if .true. then broaden density of states curves to simulate
!           experiment
!  bfactor= broadening factor for density of states
!
!  Scratch channels:
!
!  51 = frequencies passed from deffreq
!  59 = coefficients for projected frequencies
!
!   3/97 Corrected for partial occupancy frequencies
!   1/05 Memory deallocation order improved
!   6/12 File handling changed to allow for spaces in names
!
!  Conditions of use:
!
!  GULP is available free of charge to academic institutions
!  and non-commerical establishments only. Copies should be
!  obtained from the author only and should not be distributed
!  in any form by the user to a third party without the express
!  permission of the author. This notice applies to all parts
!  of the program, except any library routines which are
!  distributed with the code for completeness. All rights for
!  such routines remain with the original distributor.
!
!  No claim is made that this program is free from errors and
!  no liability will be accepted for any loss or damage that
!  may result. The user is responsible for checking the validity
!  of their results.
!
!  Copyright Curtin University 2012
!
!  Julian Gale, NRI, Curtin University, June 2012
!
  use constants
  use control
  use current
  use defects
  use files
  use general, only : bfactor
  use iochannels
  use phonout
  use projectdos
  use shell
  use times
  implicit none
!
!  Passed variables
!
  integer(i4)                                  :: imode
  integer(i4)                                  :: ncfoc
  logical                                      :: leigloc
  real(dp)                                     :: freq(*)
!
!  Local variables
!
  character(len=1)                             :: blank(60)
  character(len=30)                            :: pword
  character(len=1)                             :: star(60)
  integer(i4)                                  :: i
  integer(i4), dimension(:), allocatable       :: iatpro
  integer(i4)                                  :: ibox
  integer(i4)                                  :: iend
  integer(i4)                                  :: ii
  integer(i4)                                  :: imoddb
  integer(i4)                                  :: inat
  integer(i4), dimension(:), allocatable       :: itmp
  integer(i4)                                  :: itype
  integer(i4)                                  :: j
  integer(i4)                                  :: k
  integer(i4)                                  :: mint
  integer(i4)                                  :: napro
  integer(i4)                                  :: nd
  integer(i4)                                  :: np
  integer(i4)                                  :: npc
  integer(i4)                                  :: npfirst
  integer(i4)                                  :: npi
  integer(i4)                                  :: npifirst
  integer(i4)                                  :: npilast
  integer(i4)                                  :: nplast
  integer(i4)                                  :: npro
  integer(i4)                                  :: nproj
  integer(i4)                                  :: nprojd
  integer(i4)                                  :: nr1
  integer(i4)                                  :: nstar
  integer(i4)                                  :: nt
  integer(i4)                                  :: status
  logical                                      :: ldenout
  real(dp)                                     :: cputime
  real(dp)                                     :: fdiff
  real(dp)                                     :: fmax
  real(dp)                                     :: fpart
  real(dp)                                     :: fq
  real(dp)                                     :: frq
  real(dp)                                     :: rtrm
  real(dp)                                     :: rwmax
  real(dp)                                     :: rws
  real(dp)                                     :: rwsum
  real(dp)                                     :: t1
  real(dp)                                     :: t2
  real(dp)                                     :: trm
  real(dp)                                     :: w
  real(dp),    dimension(:), allocatable       :: wght
  real(dp),    dimension(:), allocatable       :: wghtb
  real(dp)                                     :: wmax
  real(dp)                                     :: wsum
  real(dp)                                     :: wt
  real(dp)                                     :: wtn
!
!  Local variables
!
  if (imode.eq.1) then
    nr1 = nreg1
    imoddb = 2
  else
    nr1 = numat
    imoddb = 1
  endif
  mint = 3*ncfoc
  ldenout = (index(keyword,'node').eq.0)
!
!  Read in frequencies from scratch file
!
  t1 = cputime()
  rewind(51)
  do i = 1,mint
    read(51) freq(i)
  enddo
  t2 = cputime()
  tdisk = tdisk + t2 - t1
!*******************
!  Frequency plot  *
!*******************
!
!  Frequencies :
!
!  Find largest frequency
!
  fmax = 0.0_dp
  do i = 1,mint
    if (freq(i).gt.fmax) fmax = freq(i)
  enddo
!
!  Determine either the box size or the number of boxes
!  if either is not input by the user
!
  if (nbox.eq.0.and.fbox.gt.0.0_dp) then
    nbox = (fmax/fbox) + 1
  elseif (nbox.gt.0.and.fbox.eq.0.0_dp) then
    nt = ((int(fmax)+1)/nbox) + 1
    fbox = float(nt)
  endif
  if (nbox.eq.0.and.fbox.eq.0.0_dp) then
    nbox = 64
    nt = ((int(fmax)+1)/nbox) + 1
    fbox = float(nt)
  endif
  do i = 1,60
    star(i) = '*'
    blank(i) = ' '
  enddo
!
!  Allocate local memory
!
  allocate(wght(nbox),stat=status)
  if (status/=0) call outofmemory('outfreq','wght')
  allocate(wghtb(nbox),stat=status)
  if (status/=0) call outofmemory('outfreq','wghtb')
!**********************
!  Total frequencies  *
!**********************
  do i = 1,nbox
    wght(i) = 0.0_dp
  enddo
  do j = 1,mint
!
!  Determine relative weights of each box, excluding imaginary frequencies
!
    frq = freq(j)
    if (frq.gt.0.0_dp) then
      wt = 1.0_dp
      if (lbroad) then
!****************
!  Broaden DOS  *
!****************
        do k = 1,nbox
          wghtb(k) = 0.0_dp
        enddo
        fq = - fbox
        do k = 1,nbox
          fq = fq + fbox
          fdiff = fq - frq
          trm = bfactor*bfactor*fdiff*fdiff + 1.0_dp
          trm = pi*trm
          rtrm = bfactor/trm
          wghtb(k) = rtrm
        enddo
        wsum = 0.0_dp
        do k = 1,nbox
          wsum = wsum + wghtb(k)
        enddo
        rws = wt/wsum
        do k = 1,nbox
          wght(k) = wght(k) + rws*wghtb(k)
        enddo
      else
!*******************
!  Delta function  *
!*******************
        ibox = (frq/fbox) + 1
        if (ibox.ge.1.and.ibox.le.nbox) then
          wght(ibox) = wght(ibox) + wt
        endif
      endif
    endif
  enddo
!
!  Find maximum weight
!
  wmax = 0.0_dp
  wsum = 0.0_dp
  do i = 1,nbox
    if (wght(i).gt.wmax) wmax = wght(i)
    wsum = wsum + wght(i)
  enddo
  if (wmax.gt.0.0_dp) then
    rwmax = 1.0_dp/wmax
  else
    rwmax = 0.0_dp
  endif
  if (wsum.gt.0.0_dp) then
    rwsum = 1.0_dp/wsum
  else
    rwsum = 0.0_dp
  endif
!
!  Output phonon density of states
!
  if (ldenout) then
    write(ioout,'(/,''  Vibrational density of states : '',/)')
    if (lbroad) then
      write(ioout,'(''  Broadening factor  =  '',f10.4,/)')bfactor
    endif
    write(ioout,'(''--------------------------------------------------------------------------------'')')
    write(ioout,'('' Frequency     Density of States                                                '')')
    write(ioout,'(''--------------------------------------------------------------------------------'')')
  endif
  if (lphono) then
    if (phonfile(1:1).ne.' ') then
      call endstring(phonfile,len(phonfile),iend)
      pword = phonfile(1:30)
      pword(iend:iend+4) = '.dens'
      open(17,file = pword,status='unknown')
    endif
    write(17,'('' Total Frequency  '')')
    write(17,'('' Structure number '',i2)')ncf
  endif
  do i = 1,nbox
    wt = wght(i)
    wtn = wght(i)*rwsum
    nstar = 60*wt*rwmax
    fpart = (i-1)*fbox
    if (ldenout) write(ioout,'(f11.5,1x,''|'',60a,f7.3)') fpart,(star(j),j = 1,nstar),(blank(j),j=nstar+1,60),wtn
    if (lphono) write(17,'(f11.5,f12.6)') fpart,wtn
  enddo
  if (ldenout) then
    write(ioout,'(''--------------------------------------------------------------------------------'')')
    write(ioout,'(/)')
  endif
!**************************
!  Projected frequencies  *
!**************************
  nproj = nprojcfg(ncf)
  nprojd = nprojdef(ncf)
  if (nprojd.gt.0.and.leigloc) then
    allocate(iatpro(nr1),stat=status)
    if (status/=0) call outofmemory('outfreq','iatpro')
    allocate(itmp(nr1),stat=status)
    if (status/=0) call outofmemory('outfreq','itmp')
    if (ldenout) then
      write(ioout,'(/,''  Projected vibrational density of states : '',/)')
    endif
    npfirst = 1
    npifirst = 1
    ii = 0
    do i = 1,ncf-1
      npc = nprojcfg(i)
      npfirst = npfirst+npc
      do j = 1,npc
        npifirst = npifirst + nprojit(ii+j)
      enddo
      ii = ii + npc
    enddo
    nplast = npfirst + nproj - 1
    npilast = npifirst
    do i = 1,nproj
      npilast = npilast + nprojit(ii+i)
    enddo
    npilast = npilast - 1
    npro = 0
    do np = npfirst,nplast
      if (nprojdb(np).eq.imoddb) then
      npro = npro + 1
      do i = 1,nr1
        itmp(i) = 0
      enddo
!
!  Find atoms of projection
!
      do npi = npifirst,npilast
        if (nprojptr(npi).eq.np) then
          if (nprojtyp(npi).gt.99) then
            itmp(nprojnat(npi)) = 1
          else
            inat = nprojnat(npi)
            itype = nprojtyp(npi)
            if (imode.eq.1) then
              do i = 1,nr1
                if (inat.eq.iatn(i).and.(itype.eq.natype(i).or.itype.eq.0)) itmp(i) = 1
              enddo
            else
              do i = 1,nr1
                if (inat.eq.nat(i).and.(itype.eq.nftype(i).or.itype.eq.0)) itmp(i) = 1
              enddo
            endif
          endif
        endif
      enddo
      napro = 0
      do i = 1,nr1
        if (itmp(i).eq.1) then
          napro = napro + 1
          iatpro(napro) = i
        endif
      enddo
      do i = 1,nbox
        wght(i) = 0.0_dp
      enddo
      rewind(59)
      do nd = 1,nprojd
        if (nd.eq.npro) then
          do j = 1,mint
!
!  Determine relative weights of each box, excluding imaginary frequencies
!
            read(59) w
            frq = freq(j)
            if (frq.gt.0.0_dp) then
              wt = 1.0_dp
              if (lbroad) then
!**************************
!  Broaden Projected DOS  *
!**************************
                do k = 1,nbox
                  wghtb(k) = 0.0_dp
                enddo
                fq = - fbox
                do k = 1,nbox
                  fq = fq + fbox
                  fdiff = fq - frq
                  trm = bfactor*bfactor*fdiff*fdiff + 1.0_dp
                  trm = pi*trm
                  rtrm = bfactor/trm
                  wghtb(k) = rtrm
                enddo
                wsum = 0.0_dp
                do k = 1,nbox
                  wsum = wsum + wghtb(k)
                enddo
                rws = w*wt/wsum
                do k = 1,nbox
                  wght(k) = wght(k) + rws*wghtb(k)
                enddo
              else
                ibox = (frq/fbox) + 1
                wght(ibox) = wght(ibox) + wt*w
              endif
            endif
          enddo
        else
          do j = 1,mint
            read(59) w
          enddo
        endif
      enddo
!
!  Output projected vibrational density of states
!
      if (ldenout) then
        write(ioout,'(/,''  Projection number  =  '',i2,/)') npro
        write(ioout,'(''  Atoms included :'',/)')
        write(ioout,'(20i4)')(iatpro(j),j = 1,napro)
        write(ioout,'(/,''--------------------------------------------------------------------------------'')')
        write(ioout,'('' Frequency     Density of States                                                '')')
        write(ioout,'(''--------------------------------------------------------------------------------'')')
      endif
      if (lphono) then
        write(17,'('' Frequency Projection '',i2)') npro
        write(17,'('' Structure number '',i2)') ncf
      endif
      do i = 1,nbox
        wt = wght(i)
        wtn = wght(i)*rwsum
        nstar = 60*wt*rwmax
        fpart = (i-1)*fbox
        if (ldenout) write(ioout,'(f11.5,1x,''|'',60a,f7.3)') fpart,(star(j),j = 1,nstar),(blank(j),j=nstar+1,60),wtn
        if (lphono) write(17,'(f11.5,f12.6)') fpart,wtn
      enddo
      if (ldenout) then
        write(ioout,'(''--------------------------------------------------------------------------------'')')
        write(ioout,'(/)')
      endif
      endif
    enddo
    close(59,status='delete')
    deallocate(itmp,stat=status)
    if (status/=0) call deallocate_error('outfreq','itmp')
    deallocate(iatpro,stat=status)
    if (status/=0) call deallocate_error('outfreq','iatpro')
  endif
!*****************************
!  Close phonon output file  *
!*****************************
  if (lphono) then
    close(17)
    if (phonfile(1:1).ne.' ') then
      write(ioout,'(''  Vibrational density of states written as '',a30)')pword
    else
      write(ioout,'(''  Vibrational density of states written on channel 17'')')
    endif
    write(ioout,'(/)')
  endif
!
!  Free local memory
!
  deallocate(wghtb,stat=status)
  if (status/=0) call deallocate_error('outfreq','wghtb')
  deallocate(wght,stat=status)
  if (status/=0) call deallocate_error('outfreq','wght')
!
  return
  end
