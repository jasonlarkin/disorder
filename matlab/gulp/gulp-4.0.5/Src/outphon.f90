  subroutine outphon(nlkpt,nukpt,leigloc,freq,ncfoc)
!
!  Output (1) phonon density of states plot for quick visualisation
!         (2) phonon dispersion along selected lines
!
!  Phonon density of states:
!
!  nbox     = no. of boxes allowed in phonon density => controls the
!             resolution of the plot
!  fbox     = box size
!  fextra   = extra frequency range due to broadening factor
!  fmaxdos  = maximum frequency in DOS
!  lbroad   = if .true. then broaden density of states curves to simulate
!             experiment
!  bfactor  = broadening factor for density of states
!
!  Phonon dispersion curves:
!
!  nkline   = no. of line sections through k space (max=10)
!  nkfin    = number of k point at the end of each line
!  nlbox    = no. of boxes allowed in phonon dispersion (normally
!           controlled by screen width)
!  flbox    = box size
!  fmaxdisp = maximum frequency in dispersion curves
!
!  Scratch channels:
!
!  59 = coefficients for projected densities of states
!
!   6/95 Bug fixes to allow multiple structures to work properly
!   3/97 Corrected for partial occupancy phonons
!   4/98 Note that overlaying of frequencies on second derivatives
!        is no longer used and so restriction on maxkpt is removed
!   4/98 Use of channel 51 scratch file for frequencies removed
!        as it was causing errors on DEC systems. As a result the
!        array freq must now contain all the frequencies at all K
!        points on entry.
!   7/00 Visualisation of dispersion curves corrected as it failed
!        to handle more than 60 points per line correctly.
!   6/01 Referencing of K points restricted to this configuration
!   5/02 freq now a 2-D array with second dimension being K point
!   7/02 Checking for .disp/.dens already being in file name added
!   8/02 Extra comments added to .disp file
!   1/05 Memory deallocation order improved
!  12/07 Unused variables removed
!   3/09 lkptdispersion added so that dispersion points are excluded
!        from the DOS
!   3/09 Separate fmax introduced for DOS and dispersion curves
!   3/09 Range of frequencies increased to allow for broadening
!   3/09 Factor used for determining extra range of frequencies due
!        to broadening decreased to 1/1000
!  10/09 Handling of density of states output modified so that it is
!        not output if there are no density of states K points present
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
!  Copyright Curtin University 2009
!
!  Julian Gale, NRI, Curtin University, October 2009
!
  use constants
  use control
  use current
  use dispersion
  use files
  use general, only : bfactor
  use iochannels
  use ksample
  use phonout
  use projectdos
  use shell
  implicit none
!
!  Passed variables
!
  integer(i4)                                  :: ncfoc
  integer(i4)                                  :: nlkpt
  integer(i4)                                  :: nukpt
  logical                                      :: leigloc
  real(dp)                                     :: freq(3*maxat,*)
!
!  Local variables
!
  character(len=1)                             :: blank(60)
  character(len=1)                             :: star(60)
  character(len=30),                      save :: pword
  integer(i4)                                  :: i
  integer(i4), dimension(:), allocatable       :: iatpro
  integer(i4)                                  :: ibox
  integer(i4)                                  :: iend
  integer(i4)                                  :: ii
  integer(i4)                                  :: inat
  integer(i4)                                  :: indf
  integer(i4)                                  :: istep1
  integer(i4), dimension(:), allocatable       :: itmp
  integer(i4)                                  :: itype
  integer(i4)                                  :: j
  integer(i4)                                  :: k
  integer(i4)                                  :: l
  integer(i4)                                  :: ll
  integer(i4)                                  :: mint
  integer(i4)                                  :: napro
  integer(i4)                                  :: nd
  integer(i4)                                  :: ndcf
  integer(i4)                                  :: ndcs
  integer(i4)                                  :: ndenpoints       ! Number of K points for density of states
  integer(i4)                                  :: nhkpt
  integer(i4)                                  :: nk
  integer(i4)                                  :: nlk
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
  integer(i4)                                  :: nptl
  integer(i4)                                  :: nstar
  integer(i4)                                  :: status
  logical,                                save :: lfrst = .true.
  logical                                      :: ldenout          ! If true then output density of states
  logical                                      :: lfboxzero
  logical                                      :: lnboxzero
  real(dp)                                     :: fdiff
  real(dp)                                     :: fextra
  real(dp)                                     :: fmaxdisp
  real(dp)                                     :: fmaxdos
  real(dp)                                     :: fpart
  real(dp)                                     :: fq
  real(dp)                                     :: freqj
  real(dp)                                     :: frq
  real(dp)                                     :: rstep
  real(dp)                                     :: rtrm
  real(dp)                                     :: rwmax
  real(dp)                                     :: rws
  real(dp)                                     :: rwsum
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
  nlk = nukpt - nlkpt + 1
  mint = 3*ncfoc
  ldenout = (index(keyword,'node').eq.0)
!*****************************
!  Phonon density of states  *
!*****************************
!
!  Frequencies :
!
!  Find largest frequency for dispersion curve and density of states separately
!
  fmaxdisp = 0.0_dp
  fmaxdos = 0.0_dp
  ndenpoints = 0
  do i = 1,nlk
    do j = 1,mint
      if (lkptdispersion(i+nlkpt-1)) then
        if (freq(j,i).gt.fmaxdisp) fmaxdisp = freq(j,i)
      else
        ndenpoints = ndenpoints + 1
        if (freq(j,i).gt.fmaxdos) fmaxdos = freq(j,i)
      endif
    enddo
  enddo
!
!  If there are no points for the density of states then turn off output
!
  if (ndenpoints.eq.0) ldenout = .false.
!
!  Compute extend frequency range based on decay of Lorentzian to 1/1000 of original magnitude
!
  if (lbroad) then
    fextra = sqrt((1000.0_dp*bfactor/pi) - 1.0_dp)/bfactor
  else
    fextra = 0.0_dp
  endif
  fmaxdos = fmaxdos + fextra
!
!  Determine either the box size or the number of boxes
!  if either is not input by the user
!
  lnboxzero = .true.
  lfboxzero = .true.
  if (nbox.eq.0.and.fbox.gt.0.0_dp) then
    lfboxzero = .false.
    nbox = (fmaxdos/fbox) + 1
  elseif (nbox.gt.0.and.fbox.eq.0.0_dp) then
    lnboxzero = .false.
    fbox = fmaxdos/dble(nbox) + 1.0d-6      ! Add a small amount to fbox to prevent rounding error leading to fmax lying outside nbox range
  endif
  if (nbox.eq.0.and.fbox.eq.0.0_dp) then
    nbox = 64
    fbox = fmaxdos/dble(nbox) + 1.0d-6      ! Add a small amount to fbox to prevent rounding error leading to fmax lying outside nbox range
  else
    lnboxzero = .false.
    lfboxzero = .false.
  endif
  do i = 1,60
    star(i)  = '*'
    blank(i) = ' '
  enddo
!
!  Allocate local memory
!
  allocate(wght(nbox),stat=status)
  if (status/=0) call outofmemory('outphon','wght')
  allocate(wghtb(nbox),stat=status)
  if (status/=0) call outofmemory('outphon','wghtb')
!***********************************
!  Total phonon density of states  *
!***********************************
  do i = 1,nbox
    wght(i) = 0.0_dp
  enddo
  do i = nlkpt,nukpt
    if (.not.lkptdispersion(i)) then
      wt = wkpt(i)
      do j = 1,mint
!
!  Determine relative weights of each box, excluding imaginary frequencies
!
        frq = freq(j,i-nlkpt+1)
        if (frq.gt.0.0_dp) then
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
            wght(ibox) = wght(ibox) + wt
          endif
        endif
      enddo
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
    write(ioout,'(/,''  Phonon density of states : '',/)')
    if (lbroad) then
      write(ioout,'(''  Broadening factor = '',f10.4,/)') bfactor
    endif
    write(ioout,'(''--------------------------------------------------------------------------------'')')
    write(ioout,'('' Frequency (cm-1) Density of States                                             '')')
    write(ioout,'(''--------------------------------------------------------------------------------'')')
  endif
  if (lphono) then
    if (phonfile(1:1).ne.' '.and.lfrst) then
      iend = index(phonfile,' ')
      pword = phonfile(1:30)
      if (index(pword,'.dens').eq.0) then
        pword(iend:iend+4) = '.dens'
      endif
      open(17,file=pword,status='unknown')
    endif
    write(17,'(''# Total Phonon DOS '')')
    write(17,'(''# Structure number '',i2)') ncf
  endif
!
!  Only output if there were K points for this
!
  if (ndenpoints.gt.0) then
    do i = 1,nbox
      wt = wght(i)
      wtn = wght(i)*rwsum
      nstar = 60*wt*rwmax
      fpart = (i-1)*fbox
      if (ldenout) write(ioout,'(f11.5,1x,''|'',60a,f7.3)') fpart,(star(j),j=1,nstar),(blank(j),j=nstar+1,60),wtn
      if (lphono) write(17,'(f11.5,f12.6)') fpart,wtn
    enddo
  endif
  if (ldenout) then
    write(ioout,'(''--------------------------------------------------------------------------------'')')
    write(ioout,'(/)')
  endif
  if (ndenpoints.gt.0) then
!*****************************************
!  Projected phonon densities of states  *
!*****************************************
    nproj = nprojcfg(ncf)
    nprojd = nprojdef(ncf)
    if ((nproj-nprojd).gt.0.and.leigloc) then
      allocate(iatpro(nasym),stat=status)
      if (status/=0) call outofmemory('outphon','iatpro')
      allocate(itmp(nasym),stat=status)
      if (status/=0) call outofmemory('outphon','itmp')
      if (ldenout) then
        write(ioout,'(/,''  Projected phonon density of states : '',/)')
      endif
      npfirst = 1
      npifirst = 1
      ii = 0
      do i = 1,ncf-1
        npc = nprojcfg(i)
        npfirst = npfirst + npc
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
        if (nprojdb(np).eq.1) then
          npro = npro + 1
          do i = 1,nasym
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
                do i = 1,nasym
                  if (inat.eq.iatn(i).and.(itype.eq.natype(i).or.itype.eq.0)) itmp(i)=1
                enddo
              endif
            endif
          enddo
          napro = 0
          do i = 1,nasym
            if (itmp(i).eq.1) then
              napro = napro + 1
              iatpro(napro) = i
            endif
          enddo
          do i = 1,nbox
            wght(i) = 0.0_dp
          enddo
          rewind(59)
          do i = nlkpt,nukpt
            wt = wkpt(i)
            do nd = 1,(nproj-nprojd)
              if (nd.eq.npro) then
!
!  Exclude values if this point is from a dispersion curve
!
                if (.not.lkptdispersion(i)) then
                  do j = 1,mint
!
!  Determine relative weights of each box, excluding imaginary frequencies
!
                    read(59) w
                    frq = freq(j,i-nlkpt+1)
                    if (frq.gt.0.0_dp) then
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
              else
                do j = 1,mint
                  read(59) w
                enddo
              endif
            enddo
          enddo
!
!  Output phonon density of states
!
          if (ldenout) then
            write(ioout,'(/,''  Projection number = '',i2,/)')npro
            write(ioout,'(''  Atoms included :'',/)')
            write(ioout,'(20i4)')(iatpro(j),j=1,napro)
            write(ioout,'(/,''--------------------------------------------------------------------------------'')')
            write(ioout,'('' Frequency (cm-1) Density of States                                             '')')
            write(ioout,'(''--------------------------------------------------------------------------------'')')
          endif
          if (lphono) then
            write(17,'(''# Phonon DOS Projection '',i2)')npro
            write(17,'(''# Structure number '',i2)')ncf
          endif
          do i = 1,nbox
            wt = wght(i)
            wtn = wght(i)*rwsum
            nstar = 60*wt*rwmax
            fpart = (i-1)*fbox
            if (ldenout) write(ioout,'(f11.5,1x,''|'',60a,f7.3)') fpart,(star(j),j=1,nstar),(blank(j),j=nstar+1,60),wtn
            if (lphono) write(17,'(f11.5,f12.6)') fpart,wtn
          enddo
          if (ldenout) then
            write(ioout,'(''--------------------------------------------------------------------------------'')')
            write(ioout,'(/)')
          endif
        endif
      enddo
      close(59,status='delete')
      deallocate(iatpro,stat=status)
      if (status/=0) call deallocate_error('many','iatpro')
      deallocate(itmp,stat=status)
      if (status/=0) call deallocate_error('many','itmp')
    endif
!*****************************
!  Close phonon output file  *
!*****************************
    if (lphono) then
      write(ioout,'(/)')
      if (phonfile(1:1).ne.' ') then
        if (lfrst) then
          write(ioout,'(''  Phonon density of states written as '',a30)') pword
        else
          write(ioout,'(''  Phonon density of states appended to '',a30)') pword
        endif
      else
        if (lfrst) then
          write(ioout,'(''  Phonon density of states written on channel 17'')')
        else
          write(ioout,'(''  Phonon density of states appended to channel 17'')')
        endif
      endif
      write(ioout,'(/)')
    endif
  endif
!*****************************
!  Phonon dispersion curves  *
!*****************************
  if (ndline.gt.0) then
!
!  Find whether any are relevant to this configuration
!
    ndcs = 0
    do i = 1,ndline
      nk = ndispcfg(i)
      if (ndcs.eq.0.and.nk.eq.ncf) ndcs = i
      if (nk.eq.ncf) ndcf = i
    enddo
    if (lphono) then
      if (phonfile(1:1).ne.' '.and.lfrst) then
        iend = index(phonfile,' ')
        pword = phonfile(1:30)
        if (index(pword,'.disp').eq.0) then
          pword(iend:iend+4) = '.disp'
        endif
        open(18,file=pword,status='unknown')
      endif
    endif
    if (ndcs.gt.0) then
      write(ioout,'(/,''  Phonon dispersion curves : '',/)')
      write(ioout,'(''--------------------------------------------------------------------------------'')')
!
!  Find the range of k points that are relevant to the dispersion
!
      do i = ndcs,ndcf
!
!  Calculate the horizontal k space resolution
!
        nhkpt = ndde(i) - ndds(i) + 1
        indf = ndds(i) - nlkpt
!
!  Output header to .disp file
!
        write(ioout,'(''  Section number = '',i3)')(i-ndcs+1)
        if (lphono) then
          write(18,'(''#  Section number = '',i3,''  Configuration = '',i3)')i,ncf
          write(18,'(''#  Start K point = '',3f10.6)')xkpt(indf+1),ykpt(indf+1),zkpt(indf+1)
          write(18,'(''#  Final K point = '',3f10.6)')xkpt(indf+nhkpt),ykpt(indf+nhkpt),zkpt(indf+nhkpt)
        endif
!
!  Calculate the vertical frequency resolution
!
        if (nlbox.eq.0.and.flbox.eq.0.0_dp) then
          nlbox = 25
          flbox = (fmaxdisp+10.0_dp)/float(nlbox)
        elseif (nlbox.eq.0) then
          nlbox = (fmaxdisp/flbox) + 1
        elseif (flbox.eq.0.0_dp) then
          flbox = (fmaxdisp+10.0_dp)/float(nlbox)
        endif
!
!  Output header
!
        write(ioout,'(''--------------------------------------------------------------------------------'')')
        write(ioout,'(''  Frequency (cm-1) Phonon dispersion                                            '')')
        write(ioout,'(''--------------------------------------------------------------------------------'')')
!
!  Loop over frequency scale
!
        do j = nlbox,1,-1
          freqj = flbox*(j-1)
!
!  Blank string
!
          do l = 1,60
            blank(l) = ' '
          enddo
!
!  Loop over the k space boxes
!
          rstep = 60.0_dp/dble(nhkpt)
          do l = 1,nhkpt
            do ll = 1,mint
              nptl = (freq(ll,indf+l)/flbox) + 1
              if (nptl.eq.j) then
                istep1 = nint(l*rstep)
                istep1 = min(istep1,60)
                blank(istep1) = '*'
              endif
            enddo
          enddo
!
!  Output horizontal line
!
          write(ioout,'(f9.2,3x,''|'',60a1)')freqj,(blank(l),l=1,60)
        enddo
        write(ioout,'(''--------------------------------------------------------------------------------'')')
        if (lphono) then
          do l = 1,nhkpt
            do ll = 1,mint
              write(18,'(i4,f13.6)')l,freq(ll,indf+l)
            enddo
          enddo
        endif
      enddo
      if (lphono) then
        write(ioout,'(/)')
        if (phonfile(1:1).ne.' ') then
          if (lfrst) then
            write(ioout,'(''  Phonon dispersion written as '',a30)') pword
          else
            write(ioout,'(''  Phonon dispersion appended to '',a30)') pword
          endif
        else
          if (lfrst) then
            write(ioout,'(''  Phonon dispersion written on channel 18'')')
          else
            write(ioout,'(''  Phonon dispersion appended to channel 18'')')
          endif
        endif
      endif
    endif
  endif
  if (lphono) lfrst = .false.
  if (lnboxzero) nbox = 0
  if (lfboxzero) fbox = 0.0_dp
!
!  Free local memory
!
  deallocate(wghtb,stat=status)
  if (status/=0) call deallocate_error('many','wghtb')
  deallocate(wght,stat=status)
  if (status/=0) call deallocate_error('many','wght')
!
  return
  end
