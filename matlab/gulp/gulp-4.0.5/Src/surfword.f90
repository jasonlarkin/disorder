  subroutine surfword(nru,word,lwordok,iline,line,l55,l1000,ncurr,lbflags)
!
!  Processes input for structures
!
!  nru = fortran channel for reading input
!
!  12/00 Created from strword.f
!   6/01 Initialisation of line added for benefit of some compilers
!   8/01 Mode for setting regions changed
!  10/01 Read of dhklcfg/lsliceatom added
!   1/02 Cell parameters now abs'd to avoid problems in algorithms
!   4/02 Option to put cell parameters on same line added
!   9/02 Bug in flag handling for pfrac input corrected
!   9/02 Regions added for 1-D case
!  10/02 Translate markers checked for
!   9/03 Rigid region sub-option added
!  10/03 Handling of region numbers altered
!   5/04 lmodco option introduced
!  11/04 tag for sasparticle option fixed
!  12/05 nstrains set when cell is read in
!   8/06 nru passed to linepro
!   2/07 Electric field option added
!   5/07 nregiontype added
!   5/07 option to specify a region as nonrigid added
!  10/08 COSMIC modifications merged in 
!  12/08 Module input renamed to gulpinput
!   2/09 Missing argument added to stolc calls from COSMO code
!   6/11 Electric field enabled for periodic systems
!   9/11 Analytic second derivatives turned off for field/EEM combination
!  10/11 When coordinates are put through mod on input the change is save in icosx/y/z
!        for use in bond information reading.
!  10/11 Option to read in Cartesian electric field added
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
!  Copyright Curtin University 2011
!
!  Julian Gale, NRI, Curtin University, October 2011
!
  use cellinputflag
  use constants
  use control
  use configurations
  use cosmo
  use current
  use element,        only : maxele
  use field
  use freeze
  use general
  use gulpinput
  use parallel
  use reallocate
  use scan
  use shifts
  use symmetry,       only : lsym
  implicit none
!
!  Passed variables
!
  character(len=20)                        :: word
  character(len=maxlinelength)             :: line
  integer(i4)                              :: iline
  integer(i4)                              :: ncurr
  integer(i4)                              :: nru
  logical                                  :: l55
  logical                                  :: l1000
  logical                                  :: lbflags
  logical                                  :: lwordok
!
!  Local variables
!
  integer(i4)                              :: i
  integer(i4)                              :: icx
  integer(i4)                              :: icy
  integer(i4)                              :: icz
  integer(i4)                              :: ierror
  integer(i4)                              :: ind
  integer(i4)                              :: inat
  integer(i4), dimension(:), pointer, save :: itmp
  integer(i4)                              :: itype
  integer(i4)                              :: j
  integer(i4)                              :: nai
  integer(i4)                              :: nbeg
  integer(i4)                              :: nflag
  integer(i4)                              :: nregioncurr
  integer(i4)                              :: nw
  integer(i4)                              :: status
  logical                                  :: lsymbol
  real(dp)                                 :: norm
  real(dp)                                 :: units
  real(dp)                                 :: xf
  real(dp)                                 :: yf
  real(dp)                                 :: zf
  real(dp)                                 :: xold
  real(dp)                                 :: yold
!
  nullify(itmp)
!
!  Search for valid option
!
  if (index(word,'svec').eq.1) goto 110
  if (index(word,'scel').eq.1) goto 120
  if (index(word,'sfra').eq.1) goto 130
  if (index(word,'pvec').eq.1) goto 140
  if (index(word,'pcel').eq.1) goto 150
  if (index(word,'pfra').eq.1) goto 160
  if (index(word,'sreg').eq.1) goto 170
  if (index(word,'sbul').eq.1) goto 180
  if (index(word,'tota').eq.1) goto 190
  if (index(word,'dhkl').eq.1) goto 200
  if (index(word,'fiel').eq.1) goto 210
!
  if (index(word,'poin').eq.1) goto 220    
  if (index(word,'solvente').eq.1) goto 230
  if (index(word,'solventra').eq.1) goto 240
  if (index(word,'solventrm').eq.1) goto 250
  if (index(word,'segm').eq.1) goto 260    
  if (index(word,'rang').eq.1) goto 270
  if (index(word,'cosmof').eq.1) goto 280
  if (index(word,'cosmos').eq.1) goto 290
  if (index(word,'sasp').eq.1) goto 300    
  if (index(word,'sase').eq.1) goto 310
!
  return
!********************************************
!  Read surface cell parameters as vectors  *
!********************************************
110 if (ncfg+1.gt.maxcfg) then
    maxcfg = ncfg + 1
    call changemaxcfg
  endif
  ndimen(ncfg+1) = 2
  nstrains = 3
  units = 1.0_dp
  if (nword.gt.1) then
    call stolc(words(2),maxword)
    if (index(words(2),'au').eq.1) then
      units=autoangs
    endif
  endif
  lvecin(ncfg+1) = .true.
  do i = 1,2
    read(nru,*,err=99) (rvcfg(j,i,ncfg+1),j=1,2)
    iline = iline + 1
  enddo
  rvcfg(3,1,ncfg+1) = 0.0_dp
  rvcfg(3,2,ncfg+1) = 0.0_dp
  rvcfg(1,3,ncfg+1) = 0.0_dp
  rvcfg(2,3,ncfg+1) = 0.0_dp
  rvcfg(3,3,ncfg+1) = 0.0_dp
!
!  Scale cell if necessary
!
  if (scalefactor*units.ne.1.0_dp) then
    do i = 1,2
      rvcfg(1,i,ncfg+1) = scalefactor*units*rvcfg(1,i,ncfg+1)
      rvcfg(2,i,ncfg+1) = scalefactor*units*rvcfg(2,i,ncfg+1)
      rvcfg(3,i,ncfg+1) = scalefactor*units*rvcfg(3,i,ncfg+1)
    enddo
  endif
  ind = 6*(ncfg)
  if (lbflags) then
    allocate(itmp(3_i4),stat=status)
    if (status/=0) call outofmemory('surfword','itmp')
    read(nru,*,err=99) (itmp(i),i=1,3)
    iline = iline + 1
    do i = 1,3
      lopfc(i+ind) = (itmp(i).eq.1)
      lopfc(i+ind+3) = .false.
    enddo
    deallocate(itmp,stat=status)
    if (status/=0) call deallocate_error('surfword','itmp')
  endif
  do i = 1,3
    rv(1,i) = rvcfg(1,i,ncfg+1)
    rv(2,i) = rvcfg(2,i,ncfg+1)
    rv(3,i) = rvcfg(3,i,ncfg+1)
  enddo
  lwordok = .true.
  lcelllasttime = .true.
  return
!**********************************************
!  Read surface cell parameters as a,b,alpha  *
!**********************************************
120 if (ncfg+1.gt.maxcfg) then
    maxcfg = ncfg + 1
    call changemaxcfg
  endif
  ndimen(ncfg+1) = 2
  nstrains = 3
  units = 1.0_dp
  if (nword.gt.1) then
    call stolc(words(2),maxword)
    if (index(words(2),'au').eq.1) then
      units = autoangs
    endif
  endif
  if (nfloat.lt.3) then
    line = '  '
    read(nru,'(a)') line
    iline = iline + 1
    call linepro(nru,line,iline)
  endif
  if (nfloat.lt.3) then
    call outerror('Insufficient cell parameters in input',iline)
    call stopnow('surfword')
  endif
  a = abs(floats(1))*units
  b = abs(floats(2))*units
  alpha = abs(floats(3))
  if (lbflags) then
    ind = 6*(ncfg)
    nflag = nfloat - 3
    allocate(itmp(3),stat=status)
    if (status/=0) call outofmemory('surfword','itmp')
    do i = 1,nflag
      itmp(i) = nint(floats(3+i))
    enddo
    if (nflag.lt.3) then
      do i = nflag+1,3
        itmp(i) = 0
      enddo
    endif
    do i = 1,3
      lopfc(i+ind) = (itmp(i).eq.1)
      lopfc(i+ind+3) = .false.
    enddo
    deallocate(itmp,stat=status)
    if (status/=0) call deallocate_error('surfword','itmp')
  endif
  call cell2D(rv,a,b,alpha)
  do i = 1,3
    do j = 1,3
      rvcfg(j,i,ncfg+1) = rv(j,i)
    enddo
  enddo
  lwordok = .true.
  lcelllasttime = .true.
  return
!********************************
!  Read fractional coordinates  *
!********************************
130 nregioncurr = 1
  if (nword.gt.1.and.nfloat.ge.1) then
    call stolc(words(2),maxword)
    if (index(words(2),'regi').eq.1) then
      nregioncurr = nint(floats(1))
      if (nregioncurr.gt.nregions(ncurr)) nregions(ncurr) = nregioncurr
      if (nregioncurr.gt.maxregion) then
        maxregion = nregioncurr
        call changemaxregion
      endif
      if (nregions(ncurr).gt.maxregion) then
        maxregion = nregions(ncurr)
        call changemaxregion
      endif
      if (nword.gt.2) then
!
!  Look for rigid / QM / MM options
!         
        do nw = 3,nword
          call stolc(words(nw),maxword)
          if (index(words(nw),'rig').eq.1) then
            lregionrigid(nregioncurr,ncurr) = .true.
          elseif (index(words(nw),'nonr').eq.1) then
            lregionrigid(nregioncurr,ncurr) = .false.
          elseif (index(words(nw),'qm').eq.1) then
            nregiontype(nregioncurr,ncurr) = 1 
          elseif (index(words(nw),'mm').eq.1) then
            nregiontype(nregioncurr,ncurr) = 2
          endif
        enddo
      endif
      if (lregionrigid(nregioncurr,ncurr)) then
        do j = 1,3
          lopfreg(3*(nregions(ncurr)-1)+j,ncurr) = .false.
        enddo
        if (nword.gt.2) then
          do nw = 3,nword
            if (index(words(nw),'x').ne.0) lopfreg(3*(nregions(ncurr)-1)+1,ncurr) = .true.
            if (index(words(nw),'y').ne.0) lopfreg(3*(nregions(ncurr)-1)+2,ncurr) = .true.
            if (index(words(nw),'z').ne.0) lopfreg(3*(nregions(ncurr)-1)+3,ncurr) = .true.
          enddo
        endif
      endif
    endif
  endif
  if (nregioncurr.eq.1.or.lcelllasttime) then
    if (ncfg+1.gt.maxcfg) then
      maxcfg = ncfg + 1
      call changemaxcfg
    endif
    ncfg = ncfg + 1
    if (ncfg.gt.1) ncurr = ncurr + 1
    n1con(ncfg) = ncontot + 1
  endif
!
!  Only valid for 2D case
!
  if (ndimen(ncfg).ne.2) then
    call outerror('sfractional coordinates supplied for non 2-D case',iline)
    call stopnow('surfword')
  endif
  nasym = 0
  if (lbflags) then
    allocate(itmp(3*maxat+3),stat=status)
    if (status/=0) call outofmemory('surfword','itmp')
  endif
135 line = '  '
  read(nru,'(a)',end=138)line
  iline = iline + 1
  call linepro(nru,line,iline)
!
!  Check whether input is symbol or option
!
  if ((nword+nfloat).eq.0) goto 135
!
!  Check for old fashion specification of number of atoms
!
  if (nword.eq.0.and.nfloat.eq.1) goto 135
  if (nword.gt.0) then
    word = words(1)(1:20)
    call stolc(word,20_i4)
    call worsy(word,lsymbol,.false.)
    if (.not.lsymbol) then
      l55 = .true.
      goto 138
    endif
  endif
  nasym = nasym + 1
  if (nasym.gt.maxat) then
    maxat = nint(1.2_dp*dble(nasym))
    call changemaxat
    if (lbflags) then
      call realloc(itmp,3_i4*maxat+3_i4,ierror)
      if (ierror.ne.0) call outofmemory('strword','itmp')
    endif
  endif
  if (nasum+nasym.gt.maxatot) then
    maxatot = nint(1.2_dp*dble(nasum + nasym))
    call changemaxatot
  endif
!
!  Optimisation flags
!
  if (lbflags) then
    itmp(3*(nasym-1)+1) = int(floats(nfloat-2))
    itmp(3*(nasym-1)+2) = int(floats(nfloat-1))
    itmp(3*(nasym-1)+3) = int(floats(nfloat))
    nfloat = nfloat - 3
  endif
!
!  Symbols used in input
!
  if (nword.gt.0) then
    call ltont(words(1),inat,itype)
    nat(nasym) = inat
    if (nword.ge.2) then
      if ((index(words(2),'s').eq.1).or.(index(words(2),'S').eq.1)) nat(nasym) = nat(nasym) + maxele
      if ((index(words(2),'q').eq.1).or.(index(words(2),'Q').eq.1)) then
        lqmatom(nasum+nasym) = .true.
        if ((index(words(2),'b').eq.2).or.(index(words(2),'B').eq.2)) then
          lbsmat(nasum+nasym) = .true.
          if ((index(words(2),'s').eq.3).or.(index(words(2),'S').eq.3)) nat(nasym) = nat(nasym) + maxele
        elseif ((index(words(2),'s').eq.2).or.(index(words(2),'S').eq.2)) then
          nat(nasym) = nat(nasym) + maxele
        endif
      endif
      if ((index(words(2),'b').eq.1).or.(index(words(2),'B').eq.1)) then
        lbsmat(nasum+nasym) = .true.
        if ((index(words(2),'s').eq.2).or.(index(words(2),'S').eq.2)) nat(nasym) = nat(nasym) + maxele
      endif
!  
!  Check for translate or slice marker
!     
      do i = 2,nword
        if (index(words(i),'t').eq.1.or.index(words(i),'T').eq.1.and.nregioncurr.eq.1) &
          ltranat(nasum+nasym) = .true.
        if (index(words(i),'%').eq.1.and.nregioncurr.eq.1.and.ndimen(ncfg).eq.2) &
          lsliceatom(nasum+nasym) = .true.
      enddo
    endif
    natype(nasym) = itype
    nbeg = 0
  else
!
!  Numeric input
!
    nai = int(floats(1))
    if (nai.gt.100) nai = nai - 100 + maxele
    nat(nasym) = nai
    natype(nasym) = 0
    nbeg = 1
    nfloat = nfloat - 1
  endif
  if (nfloat.gt.6) nfloat = nfloat - 3
!
!  Assign coefficients and cutoffs
!
  if (nfloat.eq.6) then
    xfrac(nasym) = floats(1+nbeg)
    yfrac(nasym) = floats(2+nbeg)
    zfrac(nasym) = floats(3+nbeg)
    qa(nasym) = floats(4+nbeg)
    occuf(nasym) = floats(5+nbeg)
    radcfg(nasym+nasum) = floats(6+nbeg)
  elseif (nfloat.eq.5) then
    xfrac(nasym) = floats(1+nbeg)
    yfrac(nasym) = floats(2+nbeg)
    zfrac(nasym) = floats(3+nbeg)
    qa(nasym) = floats(4+nbeg)
    occuf(nasym) = floats(5+nbeg)
    radcfg(nasym+nasum) = 0.0_dp
  elseif (nfloat.eq.4) then
    xfrac(nasym) = floats(1+nbeg)
    yfrac(nasym) = floats(2+nbeg)
    zfrac(nasym) = floats(3+nbeg)
    qa(nasym) = floats(4+nbeg)
    occuf(nasym) = 1.0_dp
    radcfg(nasym+nasum) = 0.0_dp
  elseif (nfloat.eq.3) then
    xfrac(nasym) = floats(1+nbeg)
    yfrac(nasym) = floats(2+nbeg)
    zfrac(nasym) = floats(3+nbeg)
    qa(nasym) = 0.0_dp
    occuf(nasym) = 1.0_dp
    radcfg(nasym+nasum) = 0.0_dp
  else
    call outerror('Insufficient data in fractional coordinate input',iline)
    call stopnow('surfword')
  endif
  goto 135
!
!  End of input loop
!
138 nascfg(ncfg) = nascfg(ncfg) + nasym
  if (nascfg(ncfg).gt.maxat) then
    maxat = nint(1.2_dp*dble(nascfg(ncfg)))
    call changemaxat
  endif
  if (.not.l55) l1000 = .true.
!
!  Ensure all fractional coordinates are between 0 and 1
!
  if (lmodco) then
    do i = 1,nasym
      xold = xfrac(i)
      yold = yfrac(i)
      xfrac(i) = dmod(xfrac(i)+10.0_dp,1.0_dp)
      yfrac(i) = dmod(yfrac(i)+10.0_dp,1.0_dp)
!
!  Save the cell shift in icosx/y/z
!
      icosx(i) = nint(xfrac(i) - xold)
      icosy(i) = nint(yfrac(i) - yold)
      icosz(i) = 0_i4
    enddo
  endif
  do i = 1,nasym
    nasum = nasum + 1
    natcfg(nasum) = nat(i)
    ntypcfg(nasum) = natype(i)
    nregionno(nasum) = nregioncurr
    xcfg(nasum) = xfrac(i)
    ycfg(nasum) = yfrac(i)
    zcfg(nasum) = zfrac(i)
    qlcfg(nasum) = qa(i)
    occucfg(nasum) = occuf(i)
    if (lbflags) then
      lopfi(3*(nasum-1)+1) = (itmp(3*(i-1)+1).eq.1)
      lopfi(3*(nasum-1)+2) = (itmp(3*(i-1)+2).eq.1)
      lopfi(3*(nasum-1)+3) = (itmp(3*(i-1)+3).eq.1)
    endif
  enddo
  lwordok = .true.
  lcelllasttime = .false.
  nshcfg(ncfg) = max(nshift,1)
  if (lbflags) then
    deallocate(itmp,stat=status)
    if (status/=0) call deallocate_error('surfword','itmp')
  endif
  return
!********************************************
!  Read polymer cell parameters as vectors  *
!********************************************
140 if (ncfg+1.gt.maxcfg) then
    maxcfg = ncfg + 1
    call changemaxcfg
  endif
  ndimen(ncfg+1) = 1
  nstrains = 1
  units = 1.0_dp
  if (nword.gt.1) then
    call stolc(words(2),maxword)
    if (index(words(2),'au').eq.1) then
      units=autoangs
    endif
  endif
  lvecin(ncfg+1) = .true.
  read(nru,*,err=99) rvcfg(1,1,ncfg+1)
  iline = iline + 1
  rvcfg(2,1,ncfg+1) = 0.0_dp
  rvcfg(3,1,ncfg+1) = 0.0_dp
  rvcfg(1,2,ncfg+1) = 0.0_dp
  rvcfg(2,2,ncfg+1) = 0.0_dp
  rvcfg(3,2,ncfg+1) = 0.0_dp
  rvcfg(1,3,ncfg+1) = 0.0_dp
  rvcfg(2,3,ncfg+1) = 0.0_dp
  rvcfg(3,3,ncfg+1) = 0.0_dp
!
!  Scale cell if necessary
!
  if (scalefactor*units.ne.1.0_dp) then
    rvcfg(1,1,ncfg+1) = scalefactor*units*rvcfg(1,1,ncfg+1)
  endif
  ind = 6*(ncfg)
  if (lbflags) then
    allocate(itmp(1_i4),stat=status)
    if (status/=0) call outofmemory('surfword','itmp')
    read(nru,*,err=99) itmp(1)
    iline = iline + 1
    lopfc(1+ind) = (itmp(1).eq.1)
    lopfc(1+ind+3) = .false.
    deallocate(itmp,stat=status)
    if (status/=0) call deallocate_error('surfword','itmp')
  endif
  do i = 1,3
    rv(1,i) = rvcfg(1,i,ncfg+1)
    rv(2,i) = rvcfg(2,i,ncfg+1)
    rv(3,i) = rvcfg(3,i,ncfg+1)
  enddo
  lwordok = .true.
  lcelllasttime = .false.
  return
!*************************************
!  Read polymer cell parameter as a  *
!*************************************
150 if (ncfg+1.gt.maxcfg) then
    maxcfg = ncfg + 1
    call changemaxcfg
  endif
  ndimen(ncfg+1) = 1
  nstrains = 1
  units = 1.0_dp
  if (nword.gt.1) then
    call stolc(words(2),maxword)
    if (index(words(2),'au').eq.1) then
      units = autoangs
    endif
  endif
  if (nfloat.lt.1) then
    line = '  '
    read(nru,'(a)') line
    iline = iline + 1
    call linepro(nru,line,iline)
  endif
  if (nfloat.lt.1) then
    call outerror('Insufficient cell parameters in input',iline)
    call stopnow('surfword')
  endif
  a = abs(floats(1))*units
  if (lbflags) then
    ind = 6*(ncfg)
    nflag = nfloat - 1
    allocate(itmp(1),stat=status)
    if (status/=0) call outofmemory('surfword','itmp')
    do i = 1,nflag
      itmp(i) = nint(floats(3+i))
    enddo
    if (nflag.lt.1) then
      do i = nflag+1,1
        itmp(i) = 0
      enddo
    endif
    lopfc(1+ind) = (itmp(1).eq.1)
    lopfc(1+ind+3) = .false.
    deallocate(itmp,stat=status)
    if (status/=0) call deallocate_error('surfword','itmp')
  endif
  call cell1D(rv,a)
  do i = 1,3
    do j = 1,3
      rvcfg(j,i,ncfg+1) = rv(j,i)
    enddo
  enddo
  lwordok = .true.
  lcelllasttime = .true.
  return
!********************************
!  Read fractional coordinates  *
!********************************
160 nregioncurr = 1
  if (nword.gt.1.and.nfloat.ge.1) then 
    call stolc(words(2),maxword)
    if (index(words(2),'regi').eq.1) then
      nregioncurr = nint(floats(1)) 
      if (nregioncurr.gt.nregions(ncurr)) nregions(ncurr) = nregioncurr
      if (nregioncurr.gt.maxregion) then
        maxregion = nregioncurr
        call changemaxregion
      endif
      if (nregions(ncurr).gt.maxregion) then
        maxregion = nregions(ncurr)
        call changemaxregion
      endif
      if (nword.gt.2) then       
!
!  Look for rigid / QM / MM options
!         
        do nw = 3,nword
          call stolc(words(nw),maxword)
          if (index(words(nw),'rig').eq.1) then
            lregionrigid(nregioncurr,ncurr) = .true.
          elseif (index(words(nw),'nonr').eq.1) then
            lregionrigid(nregioncurr,ncurr) = .false.
          elseif (index(words(nw),'qm').eq.1) then
            nregiontype(nregioncurr,ncurr) = 1 
          elseif (index(words(nw),'mm').eq.1) then
            nregiontype(nregioncurr,ncurr) = 2
          endif
        enddo
      endif
      if (lregionrigid(nregioncurr,ncurr)) then
        do j = 1,3
          lopfreg(3*(nregions(ncurr)-1)+j,ncurr) = .false.
        enddo
        if (nword.gt.2) then       
          do nw = 3,nword    
            if (index(words(nw),'x').ne.0) lopfreg(3*(nregions(ncurr)-1)+1,ncurr) = .true.
            if (index(words(nw),'y').ne.0) lopfreg(3*(nregions(ncurr)-1)+2,ncurr) = .true.
            if (index(words(nw),'z').ne.0) lopfreg(3*(nregions(ncurr)-1)+3,ncurr) = .true.
          enddo
        endif
      endif
    endif
  endif
  if (nregioncurr.eq.1.or.lcelllasttime) then
    if (ncfg+1.gt.maxcfg) then
      maxcfg = ncfg + 1
      call changemaxcfg
    endif
    ncfg = ncfg + 1
    if (ncfg.gt.1) ncurr = ncurr + 1
    n1con(ncfg) = ncontot + 1
  endif
!
!  Only valid for 1D case
!
  if (ndimen(ncfg).ne.1) then
    call outerror('pfractional coordinates supplied for non 1-D case',iline)
    call stopnow('surfword')
  endif
  nasym = 0
  if (lbflags) then
    allocate(itmp(3*maxat+1),stat=status)
    if (status/=0) call outofmemory('surfword','itmp')
  endif
165 line = '  '
  read(nru,'(a)',end=168) line
  iline = iline + 1
  call linepro(nru,line,iline)
!
!  Check whether input is symbol or option
!
  if ((nword+nfloat).eq.0) goto 165
!
!  Check for old fashion specification of number of atoms
!
  if (nword.eq.0.and.nfloat.eq.1) goto 165
  if (nword.gt.0) then
    word = words(1)(1:20)
    call worsy(word,lsymbol,.false.)
    if (.not.lsymbol) then
      l55 = .true.
      goto 168
    endif
  endif
  nasym = nasym + 1
  if (nasym.gt.maxat) then
    maxat = nint(1.2_dp*dble(nasym))
    call changemaxat
    if (lbflags) then
      call realloc(itmp,3_i4*maxat+1_i4,ierror)
      if (ierror.ne.0) call outofmemory('strword','itmp')
    endif
  endif
  if (nasum+nasym.gt.maxatot) then
    maxatot = nint(1.2_dp*dble(nasum + nasym))
    call changemaxatot
  endif
!
!  Optimisation flags
!
  if (lbflags) then
    itmp(3*(nasym-1)+1) = int(floats(nfloat-2))
    itmp(3*(nasym-1)+2) = int(floats(nfloat-1))
    itmp(3*(nasym-1)+3) = int(floats(nfloat))
    nfloat = nfloat - 3
  endif
!
!  Symbols used in input
!
  if (nword.gt.0) then
    call ltont(words(1),inat,itype)
    nat(nasym) = inat
    if (nword.ge.2) then
      if ((index(words(2),'s').eq.1).or.(index(words(2),'S').eq.1)) nat(nasym) = nat(nasym) + maxele
      if ((index(words(2),'q').eq.1).or.(index(words(2),'Q').eq.1)) then
        lqmatom(nasum+nasym) = .true.
        if ((index(words(2),'b').eq.2).or.(index(words(2),'B').eq.2)) then
          lbsmat(nasum+nasym) = .true.
          if ((index(words(2),'s').eq.3).or.(index(words(2),'S').eq.3)) nat(nasym) = nat(nasym) + maxele
        elseif ((index(words(2),'s').eq.2).or.(index(words(2),'S').eq.2)) then
          nat(nasym) = nat(nasym) + maxele
        endif
      endif
      if ((index(words(2),'b').eq.1).or.(index(words(2),'B').eq.1)) then
        lbsmat(nasum+nasym) = .true.
        if ((index(words(2),'s').eq.2).or.(index(words(2),'S').eq.2)) nat(nasym) = nat(nasym) + maxele
      endif
! 
!  Check for translate or slice marker
!
      do i = 2,nword
        if (index(words(i),'t').eq.1.or.index(words(i),'T').eq.1.and.nregioncurr.eq.1) &
          ltranat(nasum+nasym) = .true.
      enddo
    endif
    natype(nasym) = itype
    nbeg = 0
  else
!
!  Numeric input
!
    nai = int(floats(1))
    if (nai.gt.100) nai = nai - 100 + maxele
    nat(nasym) = nai
    natype(nasym) = 0
    nbeg = 1
    nfloat = nfloat - 1
  endif
  if (nfloat.gt.6) nfloat = nfloat - 3
!
!  Assign coefficients and cutoffs
!
  if (nfloat.eq.6) then
    xfrac(nasym) = floats(1+nbeg)
    yfrac(nasym) = floats(2+nbeg)
    zfrac(nasym) = floats(3+nbeg)
    qa(nasym) = floats(4+nbeg)
    occuf(nasym) = floats(5+nbeg)
    radcfg(nasym+nasum) = floats(6+nbeg)
  elseif (nfloat.eq.5) then
    xfrac(nasym) = floats(1+nbeg)
    yfrac(nasym) = floats(2+nbeg)
    zfrac(nasym) = floats(3+nbeg)
    qa(nasym) = floats(4+nbeg)
    occuf(nasym) = floats(5+nbeg)
    radcfg(nasym+nasum) = 0.0_dp
  elseif (nfloat.eq.4) then
    xfrac(nasym) = floats(1+nbeg)
    yfrac(nasym) = floats(2+nbeg)
    zfrac(nasym) = floats(3+nbeg)
    qa(nasym) = floats(4+nbeg)
    occuf(nasym) = 1.0_dp
    radcfg(nasym+nasum) = 0.0_dp
  elseif (nfloat.eq.3) then
    xfrac(nasym) = floats(1+nbeg)
    yfrac(nasym) = floats(2+nbeg)
    zfrac(nasym) = floats(3+nbeg)
    qa(nasym) = 0.0_dp
    occuf(nasym) = 1.0_dp
    radcfg(nasym+nasum) = 0.0_dp
  else
    call outerror('Missing data in fractional coordinate input',iline)
    call stopnow('surfword')
  endif
  goto 165
!
!  End of input loop
!
168 nascfg(ncfg) = nascfg(ncfg) + nasym
  if (nascfg(ncfg).gt.maxat) then
    maxat = nint(1.2_dp*dble(nascfg(ncfg)))
    call changemaxat
  endif
  if (.not.l55) l1000 = .true.
!
!  Ensure all fractional coordinates are between 0 and 1
!
  if (lmodco) then
    do i = 1,nasym
      xold = xfrac(i)
      xfrac(i) = dmod(xfrac(i)+10.0_dp,1.0_dp)
!
!  Save the cell shift in icosx/y/z
!
      icosx(i) = nint(xfrac(i) - xold)
      icosy(i) = 0_i4
      icosz(i) = 0_i4
    enddo
  endif
  do i = 1,nasym
    nasum = nasum + 1
    natcfg(nasum) = nat(i)
    ntypcfg(nasum) = natype(i)
    nregionno(nasum) = nregioncurr
    xcfg(nasum) = xfrac(i)
    ycfg(nasum) = yfrac(i)
    zcfg(nasum) = zfrac(i)
    qlcfg(nasum) = qa(i)
    occucfg(nasum) = occuf(i)
    if (lbflags) then
      lopfi(3*(nasum-1)+1) = (itmp(3*(i-1)+1).eq.1)
      lopfi(3*(nasum-1)+2) = (itmp(3*(i-1)+2).eq.1)
      lopfi(3*(nasum-1)+3) = (itmp(3*(i-1)+3).eq.1)
    endif
  enddo
  lwordok = .true.
  lcelllasttime = .false.
  nshcfg(ncfg) = max(nshift,1)
  if (lbflags) then
    deallocate(itmp,stat=status)
    if (status/=0) call deallocate_error('surfword','itmp')
  endif
  return
!*********************
!  Surface region 2  *
!*********************
170 if (nfloat.gt.0) then
    nsregion2(ncurr) = nint(floats(1))
    nregions(ncurr) = nregions(ncurr) + 1
  else
    read(nru,*,err=99) nsregion2(ncurr)
    nregions(ncurr) = nregions(ncurr) + 1
    iline = iline + 1
  endif
  if (maxregion.eq.1) then
    maxregion = 2_i4
    call changemaxregion
  endif
!
!  Apply labels for regions now to beat sort
!
  nsft = 0
  do i = 1,ncurr-1
    nsft = nsft + nascfg(i)
  enddo
  do i = nsft+nsregion2(ncurr),nasum
    nregionno(i) = 2_i4
  enddo
  lwordok = .true.
  lcelllasttime = .false.
  return
!**********************
!  Bulk energy value  *
!**********************
180 units = 1.0_dp
  if (nword.gt.1) then
    call stolc(words(2),maxword)
    if (index(words(2),'au').eq.1) then
      units = autoev
    elseif (index(words(2),'kcal').eq.1) then
      units = kcaltoev
    elseif (index(words(2),'kjmo').eq.1) then
      units = kjmtoev
    endif
  endif
  if (nfloat.gt.0) then
    sbulkecfg(ncurr) = floats(1)*units
  else
    line = '  '
    read(nru,'(a)') line
    iline = iline + 1
    call linepro(nru,line,iline)
    sbulkecfg(ncurr) = floats(1)
    if (nword.ge.1) then
      call stolc(words(1),maxword)
      if (index(words(1),'au').eq.1) then
        units = autoev
      elseif (index(words(1),'kcal').eq.1) then
        units = kcaltoev
      elseif (index(words(1),'kjmo').eq.1) then
        units = kjmtoev
      endif
    endif
    sbulkecfg(ncurr) = sbulkecfg(ncurr)*units
  endif
  lwordok = .true.
  return
!*****************************
!  Total energy - info only  *
!*****************************
190 lwordok = .true.
  return
!**************************
!  Dhkl for growth slice  *
!**************************
200 units = 1.0_dp
  if (nword.gt.1) then
    call stolc(words(2),maxword)
    if (index(words(2),'au').eq.1) then
      units = autoangs
    endif
  endif
  if (nfloat.gt.0) then
    dhklcfg(ncurr) = floats(1)*units
  else
    call outerror('Dhkl value missing',iline)
    call stopnow('surfword')
  endif
  lwordok = .true.
  return
!*******************
!  Electric field  *
!*******************
210 units = 1.0_dp
  if (nfloat.eq.0) then
    call outerror('Electric field value missing',iline)
    call stopnow('surfword')
  else
    fieldcfg(ncurr) = floats(1)*units
    if (ndimen(ncurr).eq.3) then
!
!  If field is specified then this will break the symmetry and so symmetry must be turned off
!
      lsym = .false.
      if (nword.gt.1) then
        if (index(words(2),'cart').eq.1) then
          if (nfloat.ge.4) then
            xf = floats(2)
            yf = floats(3)
            zf = floats(4)
            call cart2frac(ndimen(ncfg),xf,yf,zf,rvcfg(1,1,ncfg),fielddirectioncfg(1,ncurr), &
                           fielddirectioncfg(2,ncurr),fielddirectioncfg(3,ncurr),icx,icy,icz)
            norm = fielddirectioncfg(1,ncurr)**2 + fielddirectioncfg(2,ncurr)**2 + fielddirectioncfg(3,ncurr)**2
            if (abs(norm).lt.1.0d-12) then
              call outerror('Norm of electric field direction is too close to zero',iline)
              call stopnow('surfword')
            endif
          else
            call outerror('Missing input for electric field',iline)
            call stopnow('surfword')
          endif
        elseif (index(words(2),'c').eq.1) then
          fielddirectioncfg(1,ncurr) = 0.0_dp
          fielddirectioncfg(2,ncurr) = 0.0_dp
          fielddirectioncfg(3,ncurr) = 1.0_dp
        elseif (index(words(2),'C').eq.1) then
          fielddirectioncfg(1,ncurr) = 0.0_dp
          fielddirectioncfg(2,ncurr) = 0.0_dp
          fielddirectioncfg(3,ncurr) = 1.0_dp
        elseif (index(words(2),'b').eq.1) then
          fielddirectioncfg(1,ncurr) = 0.0_dp
          fielddirectioncfg(2,ncurr) = 1.0_dp
          fielddirectioncfg(3,ncurr) = 0.0_dp
        elseif (index(words(2),'B').eq.1) then
          fielddirectioncfg(1,ncurr) = 0.0_dp
          fielddirectioncfg(2,ncurr) = 1.0_dp
          fielddirectioncfg(3,ncurr) = 0.0_dp
        elseif (index(words(2),'a').eq.1) then
          fielddirectioncfg(1,ncurr) = 1.0_dp
          fielddirectioncfg(2,ncurr) = 0.0_dp
          fielddirectioncfg(3,ncurr) = 0.0_dp
        elseif (index(words(2),'A').eq.1) then
          fielddirectioncfg(1,ncurr) = 1.0_dp
          fielddirectioncfg(2,ncurr) = 0.0_dp
          fielddirectioncfg(3,ncurr) = 0.0_dp
        endif
      elseif (nfloat.ge.4) then
        fielddirectioncfg(1,ncurr) = floats(2)
        fielddirectioncfg(2,ncurr) = floats(3)
        fielddirectioncfg(3,ncurr) = floats(4)
        norm = fielddirectioncfg(1,ncurr)**2 + fielddirectioncfg(2,ncurr)**2 + fielddirectioncfg(3,ncurr)**2
        if (abs(norm).lt.1.0d-12) then
          call outerror('Norm of electric field direction is too close to zero',iline)
          call stopnow('surfword')
        endif
      endif
    elseif (ndimen(ncurr).eq.1) then
      if (nword.gt.1) then
        if (index(words(2),'z').eq.1) then
          fielddirectioncfg(1,ncurr) = 0.0_dp
          fielddirectioncfg(2,ncurr) = 0.0_dp
          fielddirectioncfg(3,ncurr) = 1.0_dp
        elseif (index(words(2),'Z').eq.1) then
          fielddirectioncfg(1,ncurr) = 0.0_dp
          fielddirectioncfg(2,ncurr) = 0.0_dp
          fielddirectioncfg(3,ncurr) = 1.0_dp
        elseif (index(words(2),'y').eq.1) then
          fielddirectioncfg(1,ncurr) = 0.0_dp
          fielddirectioncfg(2,ncurr) = 1.0_dp
          fielddirectioncfg(3,ncurr) = 0.0_dp
        elseif (index(words(2),'Y').eq.1) then
          fielddirectioncfg(1,ncurr) = 0.0_dp
          fielddirectioncfg(2,ncurr) = 1.0_dp
          fielddirectioncfg(3,ncurr) = 0.0_dp
        endif
      elseif (nfloat.ge.4) then
        fielddirectioncfg(1,ncurr) = floats(2)
        fielddirectioncfg(2,ncurr) = floats(3)
        fielddirectioncfg(3,ncurr) = floats(4)
        norm = fielddirectioncfg(1,ncurr)**2 + fielddirectioncfg(2,ncurr)**2 + fielddirectioncfg(3,ncurr)**2
        if (abs(norm).lt.1.0d-12) then
          call outerror('Norm of electric field direction is too close to zero',iline)
          call stopnow('surfword')
        endif
      endif
    elseif (ndimen(ncurr).eq.2) then
      if (nfloat.ge.4) then
        fielddirectioncfg(1,ncurr) = floats(2)
        fielddirectioncfg(2,ncurr) = floats(3)
        fielddirectioncfg(3,ncurr) = floats(4)
        norm = fielddirectioncfg(1,ncurr)**2 + fielddirectioncfg(2,ncurr)**2 + fielddirectioncfg(3,ncurr)**2
        if (abs(norm).lt.1.0d-12) then
          call outerror('Norm of electric field direction is too close to zero',iline)
          call stopnow('surfword')
        endif
      else
        fielddirectioncfg(1,ncurr) = 0.0_dp
        fielddirectioncfg(2,ncurr) = 0.0_dp
        fielddirectioncfg(3,ncurr) = 1.0_dp
      endif
    elseif (ndimen(ncurr).eq.0) then
      if (nword.gt.1) then
        if (index(words(2),'z').eq.1) then
          fielddirectioncfg(1,ncurr) = 0.0_dp
          fielddirectioncfg(2,ncurr) = 0.0_dp
          fielddirectioncfg(3,ncurr) = 1.0_dp
        elseif (index(words(2),'Z').eq.1) then
          fielddirectioncfg(1,ncurr) = 0.0_dp
          fielddirectioncfg(2,ncurr) = 0.0_dp
          fielddirectioncfg(3,ncurr) = 1.0_dp
        elseif (index(words(2),'y').eq.1) then
          fielddirectioncfg(1,ncurr) = 0.0_dp
          fielddirectioncfg(2,ncurr) = 1.0_dp
          fielddirectioncfg(3,ncurr) = 0.0_dp
        elseif (index(words(2),'Y').eq.1) then
          fielddirectioncfg(1,ncurr) = 0.0_dp
          fielddirectioncfg(2,ncurr) = 1.0_dp
          fielddirectioncfg(3,ncurr) = 0.0_dp
        elseif (index(words(2),'x').eq.1) then
          fielddirectioncfg(1,ncurr) = 1.0_dp
          fielddirectioncfg(2,ncurr) = 0.0_dp
          fielddirectioncfg(3,ncurr) = 0.0_dp
        elseif (index(words(2),'X').eq.1) then
          fielddirectioncfg(1,ncurr) = 1.0_dp
          fielddirectioncfg(2,ncurr) = 0.0_dp
          fielddirectioncfg(3,ncurr) = 0.0_dp
        endif
      elseif (nfloat.ge.4) then
        fielddirectioncfg(1,ncurr) = floats(2)
        fielddirectioncfg(2,ncurr) = floats(3)
        fielddirectioncfg(3,ncurr) = floats(4)
        norm = fielddirectioncfg(1,ncurr)**2 + fielddirectioncfg(2,ncurr)**2 + fielddirectioncfg(3,ncurr)**2
        if (abs(norm).lt.1.0d-12) then
          call outerror('Norm of electric field direction is too close to zero',iline)
          call stopnow('surfword')
        endif
      endif
    endif
  endif
  if (abs(fieldcfg(ncurr)).gt.0.0_dp) then
    lfieldcfg(ncurr) = .true.
!
!  If this is a variable charge model then disable second derivatives
!
    if (leem) lnoanald2 = .true.
    if (lreaxff) then
      call outerror('Electric field cannot be used with ReaxFF at present',iline)
      call stopnow('surfword')
    endif
  endif
  lwordok = .true.
  return
!*************************************************
!  Number of points per atom in solvation model  *
!*************************************************
220 if (nfloat.gt.0) then
    nppa = nint(abs(floats(1)))
  else
    line = '  '
    read(nru,'(a)') line
    iline = iline + 1
    call linepro(nru,line,iline)
    nppa = nint(abs(floats(1)))
  endif
  lwordok = .true.
  return
!********************************
!  Solvent dielectric constant  *
!********************************
230 if (nfloat.gt.0) then
    cosmoepsilon(ncurr) = abs(floats(1))
  elseif (nword.ge.2) then
!
!  Look for standard solvent names
!
    if (index(words(2),'wa').eq.1) then
      cosmoepsilon(ncurr) = 78.400_dp
    elseif (index(words(2),'chlorof').eq.1) then
      cosmoepsilon(ncurr) = 4.806_dp
    elseif (index(words(2),'chlorob').eq.1) then
      cosmoepsilon(ncurr) = 5.621_dp
    elseif (index(words(2),'metha').eq.1) then
      cosmoepsilon(ncurr) = 32.630_dp
    elseif (index(words(2),'ethy').eq.1) then
      cosmoepsilon(ncurr) = 4.335_dp
    elseif (index(words(2),'ace').eq.1) then
      cosmoepsilon(ncurr) = 20.700_dp
    elseif (index(words(2),'cyc').eq.1) then
      cosmoepsilon(ncurr) = 2.015_dp
    elseif (index(words(2),'ben').eq.1) then
      cosmoepsilon(ncurr) = 2.274_dp
    elseif (index(words(2),'tet').eq.1) then
      cosmoepsilon(ncurr) = 2.228_dp
    elseif (index(words(2),'con').eq.1) then
      cosmoepsilon(ncurr) = 1000.0_dp
    endif
  else
    line = '  '
    read(nru,'(a)') line
    iline = iline + 1
    call linepro(nru,line,iline)
    if (nfloat.gt.0) then
      cosmoepsilon(ncurr) = abs(floats(1))
    else
      return
    endif
  endif
!
!  Check that epsilon is sensible
!
  if (cosmoepsilon(ncurr).lt.1.0_dp) then
    call outerror('Solvent dielectric constant must be >= 1',iline)
    call stopnow('surfword')
  endif
  lwordok = .true.
  return
!******************
!  Solvent radii  *
!******************
240 units = 1.0_dp
  if (nword.gt.1) then
    call stolc(words(2),maxword)
    if (index(words(2),'au').eq.1) then
      units = autoangs
    endif
  endif
  if (nfloat.gt.0) then
    cosmorsolv(ncurr) = abs(floats(1))*units
    if (nfloat.gt.1) then
      cosmodrsolv(ncurr) = abs(floats(2))*units
    endif
  else
    line = '  '
    read(nru,'(a)')line
    iline = iline + 1
    call linepro(nru,line,iline)
    if (nword.ge.1) then
      call stolc(words(1),maxword)
      if (index(words(1),'au').eq.1) then
        units = autoangs
      endif
    endif
    if (nfloat.gt.0) then
      cosmorsolv(ncurr) = abs(floats(1))*units
      if (nfloat.gt.1) then
        cosmodrsolv(ncurr) = abs(floats(2))*units
      endif
    else
      return
    endif
  endif
  if (cosmodrsolv(ncurr).gt.cosmorsolv(ncurr)+0.5_dp) then
    call outerror('DeltaRsolv is too large - should be < Rsolv + 0.5',iline)
    call stopnow('surfword')
  endif
  lwordok = .true.
  return
!***************
!  COSMO Rmax  *
!***************
250 units = 1.0_dp
  if (nword.gt.1) then
    call stolc(words(2),maxword)
    if (index(words(2),'au').eq.1) then
      units = autoangs
    endif
  endif
  if (nfloat.gt.0) then
    cosmormax = abs(floats(1))*units
    if (nfloat.gt.1) cosmormaxs = abs(floats(2))*units
  else
    line = '  '
    read(nru,'(a)') line
    iline = iline + 1
    call linepro(nru,line,iline)
    if (nword.ge.1) then
      call stolc(words(1),maxword)
      if (index(words(1),'au').eq.1) then
        units = autoangs
      endif
    endif
    if (nfloat.gt.0) then
      cosmormax = abs(floats(1))*units
      if (nfloat.gt.1) cosmormaxs = abs(floats(2))*units
    else
      return
    endif
  endif
  if (cosmormaxs.gt.cosmormax) then
    call outerror('Smoothing range is too large in solventrmax',iline)
    call stopnow('surfword')
  endif
  lwordok = .true.
  return
!***************************************************
!  Number of segments per atom in solvation model  *
!***************************************************
260 if (nfloat.gt.0) then
    nspa = nint(abs(floats(1)))
    if (nfloat.gt.1) then
      nspah = nint(abs(floats(2)))
    else
      nspah = nspa
    endif
  else
    line = '  '
    read(nru,'(a)') line
    iline = iline + 1
    call linepro(nru,line,iline)
    if (nfloat.gt.0) then
      nspa = nint(abs(floats(1)))
      if (nfloat.gt.1) then
        nspah = nint(abs(floats(2)))
      else
        nspah = nspa
      endif
    else
      return
    endif
  endif
  lwordok = .true.
  return
!****************
!  COSMO range  *
!****************
270 units = 1.0_dp
  if (nword.gt.1) then
    call stolc(words(2),maxword)
    if (index(words(2),'au').eq.1) then
      units = autoangs
    endif
  endif
  if (nfloat.gt.0) then
    cosmorange = abs(floats(1))*units
  else
    line = '  '
    read(nru,'(a)') line
    iline = iline + 1
    call linepro(nru,line,iline)
    if (nword.ge.1) then
      call stolc(words(1),maxword)
      if (index(words(1),'au').eq.1) then
        units = autoangs
      endif
    endif
    if (nfloat.gt.0) then
      cosmorange = abs(floats(1))*units
    else
      return
    endif
  endif
  lwordok = .true.
  return
!*****************************
!  COSMO frame of reference  *
!*****************************
280 lcosmoeigin(ncurr) = .true.
  do i = 1,3
    line = '  '
    read(nru,'(a)')line
    iline=iline+1
    call linepro(nru,line,iline)
    if (nfloat.lt.3) then
      call outerror('Insufficient data for cosmoframe',iline)
      call stopnow('surfword')
    endif
    cosmoeigen(1,i,ncurr) = floats(1)
    cosmoeigen(2,i,ncurr) = floats(2)
    cosmoeigen(3,i,ncurr) = floats(3)
  enddo
  lwordok = .true.
  return
!*******************************
!  COSMO shape of atomic mesh  *
!*******************************
290 if (nword.ge.2) then
    if (index(words(2),'do').eq.1) then
      ldodeca = .true.
      if (nppa.eq.110) nppa = 92
      if (nspa.eq.110) nspa = 92
    elseif (index(words(2),'oc').eq.1) then
      ldodeca = .false.
      if (nppa.eq.92) nppa = 110
      if (nspa.eq.92) nspa = 110
    endif
  endif
  lwordok = .true.
  return
!************************
!  SAS particle option  *
!************************
300 if (nword.gt.1) then
    call stolc(words(2),maxword)
    if (index(words(2),'co').eq.1) then
      isasatomoption = 1
    elseif (index(words(2),'bo').eq.1) then
      isasatomoption = 2
    endif
  endif
  lwordok = .true.
  return
!***********************
!  SAS exclude option  *
!***********************
310 if (nfloat.ge.2) then
    nsasexcludemin(ncurr) = abs(nint(floats(1)))
    nsasexcludemax(ncurr) = abs(nint(floats(2)))
!
!  Ensure that max >= min
!
    if (nsasexcludemax(ncurr).lt.nsasexcludemin(ncurr)) then
      ind = nsasexcludemax(ncurr)
      nsasexcludemax(ncurr) = nsasexcludemin(ncurr)
      nsasexcludemin(ncurr) = ind
    endif
  elseif (nfloat.eq.1) then
    nsasexcludemax(ncurr) = abs(nint(floats(1)))
    nsasexcludemin(ncurr) = abs(nint(floats(1)))
  endif
  lwordok = .true.
  return
!
!  Error handling
!
99 call outerror('error reading input data - check format near '//word,iline)
  call stopnow('surfword')
!
  end
