  subroutine strword(nru,word,lwordok,iline,line,l55,l1000,ncurr,lbflags)
!
!  Processes input for structures
!
!  nru = fortran channel for reading input
!
!  10/00 All fractional coordinates will now be between 0 and 1
!   6/97 Added option 'contents' for genetic structure prediction (smw)
!   6/97 Added logicals 'loxygen' and 'lstruc' (smw)
!   3/98 Allow origin for contents to be specified (smw)
!  12/00 1-D/2-D mods added to "cart" option
!   4/01 Connectivity lists added
!   6/01 Initialisation of line added for benefit of some compilers
!  10/01 Read of dhklcfg/lsliceatom added
!   1/02 Cell parameters now abs'd to avoid problems in algorithms
!   4/02 Ditto option added
!  12/02 Setting of type to zero with atomic number in specification added for cart
!   5/03 Region 3 modifications added
!   9/03 lregionrigid suboption added
!  10/03 Handling of region numbers altered
!  11/03 Input of space group operators added
!   2/04 Time-dependent forces added
!   5/04 lmodco option introduced
!   8/06 nru passed to linepro
!   8/06 Connection types added to connect input
!   9/06 Error output for missing cell flags
!   1/07 Amide bond type added
!   4/07 Aromatic bond type changed to resonant
!   5/07 nregiontype added
!   5/07 option to specify a region as nonrigid added
!  12/08 Module input renamed to gulpinput
!   7/09 Image numbers for connect option now checked to ensure that they are in the
!        range of -4 -> 4
!  10/11 When coordinates are put through mod on input the change is save in icosx/y/z
!        for use in bond information reading.
!  10/11 Call to cart2frac modified by adding cell indices
!  12/12 Units scaling removed from time constants in td_external_force
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
!  Julian Gale, NRI, Curtin University, December 2012
!  Scott Woodley, R.I.G.B., June 1997
!
  use cellinputflag
  use constants
  use control
  use configurations
  use current
  use element, only : maxele
  use freeze
  use general
  use genetic
  use gulpinput
  use iochannels
  use molecule
  use parallel
  use reallocate
  use scan
  use shifts
  use symmetry
  implicit none
!
!  Passed variables
!
  character(len=maxlinelength)             :: line
  character(len=20)                        :: word
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
  character(len=16)                        :: string
  integer(i4)                              :: i
  integer(i4)                              :: ierror
  integer(i4)                              :: ii
  integer(i4)                              :: imagedeltax
  integer(i4)                              :: imagedeltay
  integer(i4)                              :: imagedeltaz
  integer(i4)                              :: imagex
  integer(i4)                              :: imagey
  integer(i4)                              :: imagez
  integer(i4)                              :: inat
  integer(i4)                              :: ind
  integer(i4)                              :: itype
  integer(i4), dimension(:), pointer       :: itmp
  integer(i4)                              :: j
  integer(i4)                              :: jj
  integer(i4)                              :: kk
  integer(i4)                              :: nai
  integer(i4)                              :: nbeg
  integer(i4)                              :: ncfcopy
  integer(i4)                              :: nflag
  integer(i4)                              :: nlsft
  integer(i4)                              :: nn
  integer(i4),                        save :: nregioncurr = 1
  integer(i4)                              :: nw
  integer(i4)                              :: nwi
  integer(i4)                              :: status
  logical                                  :: lprint
  logical                                  :: lreverse
  logical                                  :: lsymbol
  real(dp)                                 :: units
  real(dp)                                 :: xold
  real(dp)                                 :: yold
  real(dp)                                 :: zold
!
  nullify(itmp)
!
!  Search for valid option
!
  if (index(word,'ditt').eq.1) goto 100
  if (index(word,'vect').eq.1) goto 110
  if (index(word,'cell').eq.1) goto 120
  if (index(word,'cart').eq.1) goto 130
  if (index(word,'frac').eq.1) goto 140
  if (index(word,'cont').eq.1) goto 150
  if (index(word,'exte').eq.1) goto 160
  if (index(word,'td_e').eq.1) goto 170
  if (index(word,'spac').eq.1) goto 270
  if (index(word,'symmetry_o').eq.1) goto 280
  if (index(word,'symmetry_c').eq.1) goto 290
  if (index(word,'scal').eq.1) goto 380
  if (index(word,'orig').eq.1) goto 570
  if (index(word,'supe').eq.1) goto 630
  if (index(word,'tran').eq.1) goto 640
  if (index(word,'conn').eq.1) goto 650
  if (index(word,'unfr').eq.1) goto 750
  return
!************************************************************
!  Ditto option to copy previous configuration information  *
!************************************************************
100 if (nword.eq.1) then
    words(2) = 'all'
  endif
  if (nfloat.gt.0) then
    ncfcopy = nint(floats(1))
  else
    ncfcopy = ncurr
  endif
!
!  If this is the first configuration then cannot use ditto
!
  if (ncfg.eq.0) then
    call outerror('Ditto option is not allowed for first structure',iline)
    call stopnow('strword')
  endif
!
!  Increment configuration pointers
!
  ncfg = ncfg + 1
  ncurr = ncurr + 1
!
!  Copy data from previous configuration to this one
!
  call copycfg(ncfcopy,ncurr,words(2))
  nshcfg(ncfg) = max(nshift,1)
  lwordok = .true.
  return
!************************************
!  Read cell parameters as vectors  *
!************************************
110 if (ncfg+1.gt.maxcfg) then
    maxcfg = ncfg + 1
    call changemaxcfg
  endif
  ndimen(ncfg+1) = 3
  nstrains = 6
  units = 1.0_dp
  if (nword.gt.1) then
    call stolc(words(2),maxword)
    if (index(words(2),'au').eq.1) then
      units = autoangs
    endif
  endif
  lvecin(ncfg+1) = .true.
  do i = 1,3
    read(nru,*,err=99)(rvcfg(j,i,ncfg+1),j=1,3)
    iline = iline + 1
  enddo
!
!  Scale cell if necessary
!
  if (scalefactor*units.ne.1.0_dp) then
    do i = 1,3
      rvcfg(1,i,ncfg+1) = scalefactor*units*rvcfg(1,i,ncfg+1)
      rvcfg(2,i,ncfg+1) = scalefactor*units*rvcfg(2,i,ncfg+1)
      rvcfg(3,i,ncfg+1) = scalefactor*units*rvcfg(3,i,ncfg+1)
    enddo
  endif
  ind = 6*(ncfg)
  if (lbflags) then
    allocate(itmp(6_i4),stat=status)
    if (status/=0) call outofmemory('strword','itmp')
    read(nru,*,err=99)(itmp(i),i=1,6)
    iline = iline + 1
    do i = 1,6
      lopfc(i+ind) = (itmp(i).eq.1)
    enddo
    deallocate(itmp,stat=status)
    if (status/=0) call deallocate_error('strword','itmp')
  endif
  do i = 1,3
    rv(1,i) = rvcfg(1,i,ncfg+1)
    rv(2,i) = rvcfg(2,i,ncfg+1)
    rv(3,i) = rvcfg(3,i,ncfg+1)
  enddo
  if (lstruc) lstruc = .false.
  lwordok = .true.
  lcelllasttime = .true.
  return
!***************************************************
!  Read cell parameters as a,b,c,alpha,beta,gamma  *
!***************************************************
120 if (ncfg+1.gt.maxcfg) then
    maxcfg = ncfg + 1
    call changemaxcfg
  endif
  ndimen(ncfg+1) = 3
  nstrains = 6
  units = 1.0_dp
  if (nword.gt.1) then
    call stolc(words(2),maxword)
    if (index(words(2),'au').eq.1) then
      units = autoangs
    endif
  endif
  if (nfloat.lt.6) then
121 line = '  '
    read(nru,'(a)') line
    iline = iline + 1
    call linepro(nru,line,iline)
    if (nfloat.eq.0) goto 121
  endif
  if (nfloat.lt.6) then
    call outerror('Insufficient cell parameters in input',iline)
    call stopnow('strword')
  endif
  a = abs(floats(1))*units
  b = abs(floats(2))*units
  c = abs(floats(3))*units
  alpha = abs(floats(4))
  beta = abs(floats(5))
  gamma = abs(floats(6))
  if (lbflags) then
    ind = 6*(ncfg)
    nflag = nfloat - 6
    allocate(itmp(nflag+6),stat=status)
    if (status/=0) call outofmemory('strword','itmp')
    do i = 1,nflag
      itmp(i) = nint(floats(6+i))
    enddo
    if (nflag.lt.6) then
      call outerror('Insufficient cell flags in input',iline)
      call stopnow('strword')
    endif
    do i = 1,6
      lopfc(i+ind) = (itmp(i).eq.1)
    enddo
    deallocate(itmp,stat=status)
    if (status/=0) call deallocate_error('strword','itmp')
  endif
  call cell3D(rv,a,b,c,alpha,beta,gamma)
  do i = 1,3
    do j = 1,3
      rvcfg(j,i,ncfg+1) = rv(j,i)
    enddo
  enddo
  if (lstruc) lstruc = .false.
  lwordok = .true.
  lcelllasttime = .true.
  return
!*******************************
!  Read cartesian coordinates  *
!*******************************
130 if (lstruc) goto 159
  if (index(keyword,'cost').ne.0) then
    call outerror('To calculate cost function use contents option not cart',iline)
    call stopnow('strword')
  endif
  if (lpredict) lstruc = .true.
  units = 1.0_dp
  if (nword.gt.1) then
    do nw = 2,nword
      call stolc(words(nw),maxword)
      if (index(words(nw),'au').eq.1) then
        units = autoangs
      endif
    enddo
  endif
  nregioncurr = 1
  if (nword.gt.1.and.nfloat.ge.1) then
    do nw = 2,nword
      call stolc(words(nw),maxword)
      if (index(words(nw),'regi').eq.1) then
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
          do nwi = 3,nword
            call stolc(words(nwi),maxword)
            if (index(words(nwi),'rig').eq.1) then
              lregionrigid(nregioncurr,ncurr) = .true.
            elseif (index(words(nwi),'nonr').eq.1) then
              lregionrigid(nregioncurr,ncurr) = .false.
            elseif (index(words(nwi),'qm').eq.1) then
              nregiontype(nregioncurr,ncurr) = 1
            elseif (index(words(nwi),'mm').eq.1) then
              nregiontype(nregioncurr,ncurr) = 2
            endif
          enddo
        endif
        if (lregionrigid(nregioncurr,ncurr)) then
          do j = 1,3
            lopfreg(3*(nregions(ncurr)-1)+j,ncurr) = .false.
          enddo
          if (nword.gt.2) then       
            do nwi = 3,nword    
              if (index(words(nwi),'x').ne.0) lopfreg(3*(nregions(ncurr)-1)+1,ncurr) = .true.
              if (index(words(nwi),'y').ne.0) lopfreg(3*(nregions(ncurr)-1)+2,ncurr) = .true.
              if (index(words(nwi),'z').ne.0) lopfreg(3*(nregions(ncurr)-1)+3,ncurr) = .true.
            enddo
          endif
        endif
      endif
    enddo
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
  nasym = 0
  if (lbflags) then
    allocate(itmp(3_i4*maxat+6_i4),stat=status)
    if (status/=0) call outofmemory('use','itmp')
  endif
!
!  Start of input loop
!
135 line = '  '
  read(nru,'(a)',end=138) line
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
      call realloc(itmp,3_i4*maxat+6_i4,ierror)
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
        if (index(words(i),'t').eq.1.or.index(words(i),'T').eq.1) ltranat(nasum+nasym) = .true.
        if (index(words(i),'%').eq.1.and.nregioncurr.eq.1.and.ndimen(ncfg).eq.2) lsliceatom(nasum+nasym) = .true.
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
    xclat(nasym) = floats(1+nbeg)*units
    yclat(nasym) = floats(2+nbeg)*units
    zclat(nasym) = floats(3+nbeg)*units
    qa(nasym) = floats(4+nbeg)
    occuf(nasym) = floats(5+nbeg)
    radcfg(nasym+nasum) = floats(6+nbeg)
  elseif (nfloat.eq.5) then
    xclat(nasym) = floats(1+nbeg)*units
    yclat(nasym) = floats(2+nbeg)*units
    zclat(nasym) = floats(3+nbeg)*units
    qa(nasym) = floats(4+nbeg)
    occuf(nasym) = floats(5+nbeg)
    radcfg(nasym+nasum) = 0.0_dp
  elseif (nfloat.eq.4) then
    xclat(nasym) = floats(1+nbeg)*units
    yclat(nasym) = floats(2+nbeg)*units
    zclat(nasym) = floats(3+nbeg)*units
    qa(nasym) = floats(4+nbeg)
    occuf(nasym) = 1.0_dp
    radcfg(nasym+nasum) = 0.0_dp
  elseif (nfloat.eq.3) then
    xclat(nasym) = floats(1+nbeg)*units
    yclat(nasym) = floats(2+nbeg)*units
    zclat(nasym) = floats(3+nbeg)*units
    qa(nasym) = 0.0_dp
    occuf(nasym) = 1.0_dp
    radcfg(nasym+nasum) = 0.0_dp
  else
    call outerror('Missing data in Cartesian coordinate input',iline)
    if (ioproc) then
      write(ioout,'(''!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'')')
      write(ioout,'(''!! This is probably caused by missing flags if the following keywords are missing:'')')
      write(ioout,'(''!! conp conv noflag '')')
      write(ioout,'(''!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'',/)')
    endif
    call stopnow('strword')
  endif
  goto 135
!
!  End of input loop
!
138 if (.not.l55) l1000 = .true.
  nascfg(ncfg) = nascfg(ncfg) + nasym
  if (nascfg(ncfg).gt.maxat) then
    maxat = nint(1.2_dp*dble(nascfg(ncfg)))
    call changemaxat
  endif
!
!  Convert coordinates from Cartesian to fractional
!
  if (ndimen(ncfg).eq.3) then
    do i = 1,nasym
      xclat(i) = xclat(i)*scalefactor
      yclat(i) = yclat(i)*scalefactor
      zclat(i) = zclat(i)*scalefactor
    enddo
  elseif (ndimen(ncfg).eq.2) then
    do i = 1,nasym
      xclat(i) = xclat(i)*scalefactor
      yclat(i) = yclat(i)*scalefactor
    enddo
  elseif (ndimen(ncfg).eq.1) then
    do i = 1,nasym
      xclat(i) = xclat(i)*scalefactor
    enddo
  endif
  do i = 1,nasym
    call cart2frac(ndimen(ncfg),xclat(i),yclat(i),zclat(i),rv,xfrac(i),yfrac(i),zfrac(i),icosx(i),icosy(i),icosz(i))
  enddo
!
!  Add data to overall configuration lists
!
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
  if (lbflags) then
    deallocate(itmp,stat=status)
    if (status/=0) call deallocate_error('use','itmp')
  endif
  lwordok = .true.
  lcelllasttime = .false.
  nshcfg(ncfg) = max(nshift,1)
  return
!********************************
!  Read fractional coordinates  *
!********************************
140 if (lstruc) goto 159
  if (index(keyword,'cost').ne.0) then
    call outerror('To calculate cost function use contents not fractional option',iline)
    call stopnow('strword')
  endif
  if (lpredict) lstruc=.true.
  if (ncfg+1.gt.maxcfg) then
    maxcfg = ncfg + 1
    call changemaxcfg
  endif
  ncfg = ncfg + 1
  if (ncfg.gt.1) ncurr = ncurr + 1
!
!  Only valid for 3D case
!
  if (ndimen(ncfg).ne.3) then
    call outerror('Fractional coordinates supplied for non 3-D case',iline)
    call stopnow('strword')
  endif
  n1con(ncfg) = ncontot + 1
  nasym = 0
  if (lbflags) then
    allocate(itmp(3_i4*maxat+6_i4),stat=status)
    if (status/=0) call outofmemory('use','itmp')
  endif
145 line = '  '
  read(nru,'(a)',end=148) line
  iline = iline + 1
  call linepro(nru,line,iline)
!
!  Check whether input is symbol or option
!
  if ((nword+nfloat).eq.0) goto 145
!
!  Check for old fashion specification of number of atoms
!
  if (nword.eq.0.and.nfloat.eq.1) goto 145
  if (nword.gt.0) then
    word = words(1)(1:20)
    call worsy(word,lsymbol,.false.)
    if (.not.lsymbol) then
      l55 = .true.
      goto 148
    endif
  endif
  nasym = nasym + 1
  if (nasym.gt.maxat) then
    maxat = nint(1.2_dp*dble(nasym))
    call changemaxat
    if (lbflags) then
      call realloc(itmp,3_i4*maxat+6_i4,ierror)
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
        if (index(words(i),'t').eq.1.or.index(words(i),'T').eq.1) ltranat(nasum+nasym) = .true.
        if (index(words(i),'%').eq.1.and.nregioncurr.eq.1.and.ndimen(ncfg).eq.2) lsliceatom(nasum+nasym) = .true.
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
    if (ioproc) then
      write(ioout,'(''!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'')')
      write(ioout,'(''!! This is probably caused by missing flags if the following keywords are missing:'')')
      write(ioout,'(''!! conp conv noflag '')')
      write(ioout,'(''!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'',/)')
    endif
    call stopnow('strword')
  endif
  goto 145
!
!  End of input loop
!
148 nascfg(ncfg) = nasym
  if (.not.l55) l1000 = .true.
!
!  Ensure all fractional coordinates are between 0 and 1
!
  if (lmodco) then
    do i = 1,nasym
      xold = xfrac(i)
      yold = yfrac(i)
      zold = zfrac(i)
      xfrac(i) = mod(xfrac(i)+10.0_dp,1.0_dp)
      yfrac(i) = mod(yfrac(i)+10.0_dp,1.0_dp)
      zfrac(i) = mod(zfrac(i)+10.0_dp,1.0_dp)
!
!  Save the cell shift in icosx/y/z
!
      icosx(i) = nint(xfrac(i) - xold)
      icosy(i) = nint(yfrac(i) - yold)
      icosz(i) = nint(zfrac(i) - zold)
    enddo
  endif
  do i = 1,nasym
    nasum = nasum + 1
    natcfg(nasum) = nat(i)
    ntypcfg(nasum) = natype(i)
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
    if (status/=0) call deallocate_error('use','itmp')
  endif
  return
!**********************************
!  Read contents for predict      *
!**********************************
150 if (lstruc) goto 159
  lstruc = .true.
  ncfg = ncfg + 1
  if (ncfg.gt.1) ncurr = ncurr + 1
  n1con(ncfg) = ncontot + 1
  nasym = 0
  loxygen = .false.
  lprint = (index(keyword,'debug').ne.0.and.ioproc)
155 line = '  '
  read(nru,'(a)',end=158) line
  iline = iline + 1
  call linepro(nru,line,iline)
!
!  Check whether input is symbol or option
!
  if ((nword+nfloat).eq.0) goto 155
  if (nword.gt.0) then
    word = words(1)(1:20)
    call worsy(word,lsymbol,.false.)
    if (.not.lsymbol) then
      l55 = .true.
      goto 158
    endif
  endif
  if (nword.eq.0.or.nfloat.lt.1) then
    call outerror('Missing data in the contents option',iline)
    call stopnow('strword')
  endif
  nasym = nasym + 1
  if (nasym.gt.maxat) then
    maxat = nint(1.2_dp*dble(nasym))
    call changemaxat
    if (lbflags) then
      call realloc(itmp,3_i4*maxat+6_i4,ierror)
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
  call ltont(words(1),inat,itype)
  if (inat.eq.8) loxygen = .true.
  nat(nasym) = inat
  if (nword.lt.2) goto 152
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
    if (index(words(i),'t').eq.1.or.index(words(i),'T').eq.1) ltranat(nasum+nasym) = .true.
    if (index(words(i),'%').eq.1.and.nregioncurr.eq.1.and.ndimen(ncfg).eq.2) lsliceatom(nasum+nasym) = .true.
  enddo
  natype(nasym) = itype
  xfrac(nasym) = 1.0_dp/nasym
  yfrac(nasym) = 1.0_dp/nasym
  zfrac(nasym) = 1.0_dp/nasym
  if (nasym.eq.1) then
    xfrac(nasym) = 0.0_dp
    yfrac(nasym) = 0.0_dp
    zfrac(nasym) = 0.0_dp
  endif
  occuf(nasym) = 1.0_dp
  radcfg(nasym+nasum) = 0.0_dp
!
!  Check for correct input style
!
  if (nfloat.lt.3.and.index(keyword,'pred').eq.0) then
    call outerror('Keyword predict is required with content option',iline)
    call stopnow('strword')
  endif
  if ((index(keyword,'gene').ne.0.or.index(keyword,'anne').ne.0.).and.nfloat.gt.2.and.nasym.gt.1) then
    call outerror('Only 0-1 coordinate required with keywords genetic or anneal',iline)
    call stopnow('strword')
  endif
  if (nfloat.lt.3.and.index(keyword,'gene').eq.0.and.index(keyword,'anne').eq.0) then
    call outerror('Contents specified but not appropriate keywords - genetic/anneal',iline)
    call stopnow('strword')
  endif
!
!  Assign oxidation state and either the coordination number
!                or the average observed coordination number
!                and fractional co-ordinates if necessary
!
  if (nfloat.gt.3.and.nfloat.lt.6) then
    xfrac(nasym) = floats(1)
    yfrac(nasym) = floats(2)
    zfrac(nasym) = floats(3)
    qa(nasym) = floats(4)
    oxa(nasym) = qa(nasym)
    if (nfloat.gt.4)then
      cna(nasym) = floats(5)
      if (lprint) write(ioout,'(''  Atomic number ='',i3,''  Oxidation = '',f4.1,'' Coordination = '',f6.3)') &
        inat,oxa(nasym),cna(nasym)
    else
      if(index(keyword,'cost').ne.0) call aocn2(nasym)
    endif
  elseif (nfloat.lt.3.and.nfloat.ne.0) then
    qa(nasym) = floats(1)
    oxa(nasym) = qa(nasym)
    if (nfloat.eq.2)then
      cna(nasym) = floats(2)
      if (lprint) write(ioout,'(''  Atomic number ='',i3,''  Oxidation = '',f4.1,'' Coordination = '',f6.3)') &
        inat,oxa(nasym),cna(nasym)
    else
      if (index(keyword,'cost').ne.0) call aocn2(nasym)
    endif
  else
    call stopnow('strword')
  endif
  goto 155
!
!  End of input loop
!
158 nascfg(ncfg) = nasym
  if (.not.l55) l1000 = .true.
  do i = 1,nasym
    nasum = nasum + 1
    natcfg(nasum) = nat(i)
    ntypcfg(nasum) = natype(i)
    xcfg(nasum) = xfrac(i)
    ycfg(nasum) = yfrac(i)
    zcfg(nasum) = zfrac(i)
    qlcfg(nasum) = qa(i)
    oxcfg(nasum) = oxa(i)
    cncfg(nasum) = cna(i)
    occucfg(nasum) = occuf(i)
    if (lbflags) then
      lopfi(3*(nasum-1)+1) = (itmp(3*(i-1)+1).eq.1)
      lopfi(3*(nasum-1)+2) = (itmp(3*(i-1)+2).eq.1)
      lopfi(3*(nasum-1)+3) = (itmp(3*(i-1)+3).eq.1)
    endif
  enddo
  nshcfg(ncfg) = max(nshift,1)
  lwordok = .true.
  return
!********************************************************
!  Check contents of cell not specified more than once  *
!********************************************************
152 call outerror('Error within input file',iline)
159 call outerror('Contents of cell specified more than once',iline)
  call stopnow('strword')
!*************************
!  Read external forces  *
!*************************
160 units = 1.0_dp
  if (nword.gt.1) then
    do nw = 2,nword
      call stolc(words(nw),maxword)
      if (index(words(nw),'au').eq.1) then
        units = autoev/autoangs
      endif
    enddo
  endif
!
!  Start of input loop
!
165 line = '  '
  read(nru,'(a)',end=168) line
  iline = iline + 1
  call linepro(nru,line,iline)
!
!  Check whether input is symbol or option
!
  if ((nword+nfloat).eq.0) goto 165
  if (nword.gt.0) then
    word = words(1)(1:20)
    call stolc(word,20_i4)
    l55 = .true.
    goto 168
  endif
!
!  Find start of current configuration atoms
!
  nlsft = nasum - nascfg(ncurr)
!
!  Assign values - check atom number first though
!
  if (nfloat.ge.4) then
    nn = abs(nint(floats(1)))
    if (nn.gt.nascfg(ncurr)) then
      call outerror('Atom number for external force is too large',iline)
      call stopnow('strword')
    endif
    forcecfg(1,nlsft+nn) = floats(2)*units
    forcecfg(2,nlsft+nn) = floats(3)*units
    forcecfg(3,nlsft+nn) = floats(4)*units
  else
    call outerror('Missing data in external force input',iline)
    call stopnow('strword')
  endif
  goto 165
!
!  End of input loop
!
168 if (.not.l55) l1000 = .true.
  lwordok = .true.
  return
!****************************************
!  Read time-dependent external forces  *
!****************************************
170 units = 1.0_dp
  if (nword.gt.1) then
    do nw = 2,nword
      call stolc(words(nw),maxword)
      if (index(words(nw),'au').eq.1) then
        units = autoev/autoangs
      endif
    enddo
  endif
!
!  Start of input loop
!
175 line = '  '
  read(nru,'(a)',end=178) line
  iline = iline + 1
  call linepro(nru,line,iline)
!
!  Check whether input is symbol or option
!
  if ((nword+nfloat).eq.0) goto 175
  if (nword.gt.0) then
    word = words(1)(1:20)
    call stolc(word,20_i4)
    if (word(1:2).eq.'x ') then
      j = 1
    elseif (word(1:2).eq.'y ') then
      j = 2
    elseif (word(1:2).eq.'z ') then
      j = 3
    else
      l55 = .true.
      goto 178
    endif
  endif
!
!  Find start of current configuration atoms
!
  nlsft = nasum - nascfg(ncurr)
!
!  Assign values - check atom number first though
!
  if (nfloat.ge.4) then
    nn = abs(nint(floats(1)))
    if (nn.gt.nascfg(ncurr)) then
      call outerror('Atom number for external force is too large',iline)
      call stopnow('strword')
    endif
    ltdforcecfg(j,nlsft+nn) = .true.
    tdforcecfg(1,j,nlsft+nn) = floats(2)*units
    tdforcecfg(2,j,nlsft+nn) = floats(3)
    tdforcecfg(3,j,nlsft+nn) = floats(4)
  else
    call outerror('Missing data in external force input',iline)
    call stopnow('strword')
  endif
  goto 175
!
!  End of input loop
!
178 if (.not.l55) l1000 = .true.
  lwordok = .true.
  return
!*******************************
!  Space group symmetry input  *
!*******************************
!  Input resembles CRYSTAL
!  iflags .eq. 0 => space group indicated by number
!         .ne. 0 => space group identified by Hermann-Maugain symbol
!  ifhr   .eq. 0 => hexagonal unit cell
!         .ne. 0 => rhombohedral unit cell
!  ifso   .eq. 0 => standard origin setting
!         .eq. 1 => standard origin shift
!         .gt. 1 => non-standard shift to be supplied on next line
!
270 line = '  '
  read(nru,'(a)',err=99) line
  iline = iline + 1
  call linepro(nru,line,iline)
  if (nword.eq.0.and.nfloat.eq.0) goto 270
  if (ndimen(ncurr).ne.3) then
    call outerror('Space group specifed for non 3-D case',iline)
    call stopnow('strword')
  endif
  if (nword.eq.0) then
!
!  General and space group number only case
!
    if (nfloat.eq.3) then
      iflags(ncurr) = int(floats(1))
      ifhr(ncurr) = int(floats(2))
      ifso(ncurr) = int(floats(3))
    elseif (nfloat.eq.2) then
      iflags(ncurr) = int(floats(1))
      ifhr(ncurr) = int(floats(2))
      ifso(ncurr) = 0
    elseif (nfloat.eq.1) then
      iflags(ncurr) = 0
      ifhr(ncurr) = 0
      ifso(ncurr) = 0
    else
      call outerror('Unable to read space group info',iline)
      call stopnow('strword')
    endif
    if (nfloat.eq.1) then
      nspcg(ncurr) = int(floats(1))
    else
      if (ifso(ncurr).gt.1) then
        read(nru,*,err=99)(ivso(j,ncurr),j=1,3)
        iline = iline + 1
      endif
      if (iflags(ncurr).eq.0) then
        read(nru,*,err=99) nspcg(ncurr)
        iline = iline + 1
      else
        read(nru,'(a)') string
        iline = iline + 1
        do i = 1,16
          hmssg(i,ncurr) = string(i:i)
          call stouc(hmssg(i,ncurr))
        enddo
      endif
    endif
  else
!
!  Space group symbol only case
!
    do i = 1,16
      hmssg(i,ncurr) = line(i:i)
      call stouc(hmssg(i,ncurr))
    enddo
    iflags(ncurr) = 1
    ifhr(ncurr) = 0
    ifso(ncurr) = 0
  endif
  lsymset(ncurr) = .true.
  lwordok = .true.
  return
!**************************************
!  Manual input of symmetry operator  *
!**************************************
280 ngocfg(ncurr) = ngocfg(ncurr) + 1
  lreverse = .false.
  if (nword.gt.1) then
    if (index(words(2),'rev').eq.1) lreverse = .true.
  endif
  if (ngocfg(ncurr).gt.maxsymop) then
    maxsymop = ngocfg(ncurr)
    call changemaxsymop
  endif
  do i = 1,3
    line = '  '
    read(nru,'(a)',err=99) line
    iline = iline + 1
    call linepro(nru,line,iline)
    if (nfloat.ge.4) then
      if (lreverse) then
        ropcfg(i,1,ngocfg(ncurr),ncurr) = floats(1)
        ropcfg(i,2,ngocfg(ncurr),ncurr) = floats(2)
        ropcfg(i,3,ngocfg(ncurr),ncurr) = floats(3)
      else
        ropcfg(1,i,ngocfg(ncurr),ncurr) = floats(1)
        ropcfg(2,i,ngocfg(ncurr),ncurr) = floats(2)
        ropcfg(3,i,ngocfg(ncurr),ncurr) = floats(3)
      endif
      vitcfg(i,ngocfg(ncurr),ncurr) = floats(4)
    elseif (nfloat.eq.3) then
      if (lreverse) then
        ropcfg(i,1,ngocfg(ncurr),ncurr) = floats(1)
        ropcfg(i,2,ngocfg(ncurr),ncurr) = floats(2)
        ropcfg(i,3,ngocfg(ncurr),ncurr) = floats(3)
      else
        ropcfg(1,i,ngocfg(ncurr),ncurr) = floats(1)
        ropcfg(2,i,ngocfg(ncurr),ncurr) = floats(2)
        ropcfg(3,i,ngocfg(ncurr),ncurr) = floats(3)
      endif
      vitcfg(i,ngocfg(ncurr),ncurr) = 0.0_dp
    else
      call outerror('insufficent values specified for symmetry operator',iline)
    endif
  enddo
  lsymset(ncurr) = .true.
  lwordok = .true.
  return
!**************************************
!  Manual input of symmetry cell type *
!**************************************
290 if (index(words(2),'tri').eq.1) then
    nccscfg(ncurr) = 1
  elseif (index(words(2),'mon').eq.1) then
    nccscfg(ncurr) = 2
  elseif (index(words(2),'ort').eq.1) then
    nccscfg(ncurr) = 3
  elseif (index(words(2),'tet').eq.1) then
    nccscfg(ncurr) = 4
  elseif (index(words(2),'hex').eq.1) then
    nccscfg(ncurr) = 5
    ifhr(ncurr) = 0
  elseif (index(words(2),'rho').eq.1) then
    nccscfg(ncurr) = 5
    ifhr(ncurr) = 1
  elseif (index(words(2),'cub').eq.1) then
    nccscfg(ncurr) = 6
  endif
  lwordok = .true.
  return
!*****************************************************************
!  Scaling factor for cartesian input - for THBREL compatablity  *
!*****************************************************************
380 if (nfloat.gt.0) then
    scalefactor = floats(1)
  else
    read(nru,*,err=99) scalefactor
    iline = iline + 1
  endif
  lwordok = .true.
  return
!***********************************************************
!  Origin setting :                                        *
!    1 or 2 => standard origin settings                    *
!    3 numbers => general origin shift                     *
!      if numbers are >= 1 then numbers are divided by 24  *
!***********************************************************
570 if (nfloat.eq.0) then
    line = '  '
    read(nru,'(a)',err=99) line
    iline=iline+1
    call linepro(nru,line,iline)
  endif
  if (nfloat.eq.1) then
    ifso(ncurr) = nint(floats(1)) - 1
  elseif (nfloat.ge.3) then
    do i = 1,3
      if (floats(i).lt.1.0_dp) then
        floats(i) = 24.0_dp*floats(i)
      endif
    enddo
    ivso(1,ncurr) = nint(floats(1))
    ivso(2,ncurr) = nint(floats(2))
    ivso(3,ncurr) = nint(floats(3))
    ifso(ncurr) = 2
  else
    call outerror('Missing data in origin shift input',iline)
    call stopnow('strword')
  endif
  lwordok = .true.
  return
!*********************
!  Supercell option  *
!*********************
630 if (ndimen(ncurr).eq.2) then
    if (nfloat.ge.2) then
      ii = nint(floats(1))
      jj = nint(floats(2))
      kk = 1
    else
      line = '  '
      read(nru,'(a)')line
      iline = iline + 1
      call linepro(nru,line,iline)
      if (nfloat.ge.2) then
        ii = nint(floats(1))
        jj = nint(floats(2))
        kk = 1
      else
        call outerror('Insufficient data for supercell input',iline)
        call stopnow('strword')
      endif
    endif
    if (ii.eq.0.or.jj.eq.0) then
      call outerror('Zero supplied for supercell input',iline)
      call stopnow('strword')
    endif
  elseif (ndimen(ncurr).eq.1) then
    if (nfloat.ge.1) then
      ii = nint(floats(1))
      jj = 1
      kk = 1
    else
      line = '  '
      read(nru,'(a)') line
      iline = iline + 1
      call linepro(nru,line,iline)
      if (nfloat.ge.1) then
        ii = nint(floats(1))
        jj = 1
        kk = 1
      else
        call outerror('Insufficient data for supercell input',iline)
        call stopnow('strword')
      endif
    endif
    if (ii.eq.0) then
      call outerror('Zero supplied for supercell input',iline)
      call stopnow('strword')
    endif
  else
    if (nfloat.ge.3) then
      ii = nint(floats(1))
      jj = nint(floats(2))
      kk = nint(floats(3))
    else
      line = '  '
      read(nru,'(a)') line
      iline = iline + 1
      call linepro(nru,line,iline)
      if (nfloat.ge.3) then
        ii = nint(floats(1))
        jj = nint(floats(2))
        kk = nint(floats(3))
      else
        call outerror('Insufficient data for supercell input',iline)
        call stopnow('strword')
      endif
    endif
    if (ii.eq.0.or.jj.eq.0.or.kk.eq.0) then
      call outerror('Zero supplied for supercell input',iline)
      call stopnow('strword')
    endif
  endif
  nsuper(ncurr) = ii*10000 + jj*100 + kk
  lwordok = .true.
  return
!****************************
!  Translation scan option  *
!****************************
640 if (nfloat.eq.0) then
    line = '  '
    read(nru,'(a)') line
    iline = iline + 1
    call linepro(nru,line,iline)
  endif
  if (nfloat.ge.4) then
    xtran(ncurr) = floats(1)
    ytran(ncurr) = floats(2)
    ztran(ncurr) = floats(3)
    ntran(ncurr) = nint(floats(4))
  else
    call outerror('Insufficient data for translate option',iline)
    call stopnow('strword')
  endif
  lwordok = .true.
  return
!***********************
!  Connectivity lists  *
!***********************
650 if (nfloat.eq.0) then
    line = '  '
    read(nru,'(a)') line
    iline = iline + 1
    call linepro(nru,line,iline)
  endif
  if (nfloat.ge.2) then
    if (nconnect+1.gt.maxconnect) then
      maxconnect = nconnect + 10
      call changemaxconnect
    endif
    nconnect = nconnect + 1
    n1connect(nconnect) = nint(abs(floats(1)))
    n2connect(nconnect) = nint(abs(floats(2)))
    nconnectcfg(nconnect) = ncurr
    if (nfloat.ge.3) then
!
!  Read image numbers
!
      imagedeltax = nint(floats(3))
      if (nfloat.ge.4) then
        imagedeltay = nint(floats(4))
      else
        imagedeltay = 0
      endif
      if (nfloat.ge.5) then
        imagedeltaz = nint(floats(5))
      else
        imagedeltaz = 0
      endif
!
!  Check that they are in the range of -4 -> 4
!
      if (abs(imagedeltax).gt.4.or.abs(imagedeltay).gt.4.or.abs(imagedeltaz).gt.4) then
        call outerror('Image number for connect exceeds allowed range',iline)
        call stopnow('strword')
      endif
!
      imagex = 5 + imagedeltax
      imagey = 5 + imagedeltay
      imagez = 5 + imagedeltaz
!
!  Correct for any mod operation during reading of coordinates
!
      i = n1connect(nconnect)
      j = n2connect(nconnect)
      imagex = imagex + icosx(i) - icosx(j)
      imagey = imagey + icosy(i) - icosy(j)
      imagez = imagez + icosz(i) - icosz(j)
!
      nconnectind(nconnect) = imagex + 10*imagey + 100*imagez
    else
      nconnectind(nconnect) = 0
    endif
  else
    call outerror('Insufficient input for connect option',iline)
    call stopnow('strword')
  endif
  if (nword.gt.1) then
    do i = 2,nword
      if (index(words(i),'si').eq.1) then
        nconnecttype(1,nconnect) = 1
      elseif (index(words(i),'do').eq.1) then
        nconnecttype(1,nconnect) = 2
      elseif (index(words(i),'tr').eq.1) then
        nconnecttype(1,nconnect) = 3
      elseif (index(words(i),'qu').eq.1) then
        nconnecttype(1,nconnect) = 4
      elseif (index(words(i),'re').eq.1) then
        nconnecttype(1,nconnect) = 5
      elseif (index(words(i),'am').eq.1) then
        nconnecttype(1,nconnect) = 6
      elseif (index(words(i),'cyc').eq.1) then
        nconnecttype(2,nconnect) = 2
      elseif (index(words(i),'exo').eq.1) then
        nconnecttype(2,nconnect) = 3
      endif
    enddo
  endif
  lwordok = .true.
  return
!********************************
!  Unfreeze a spherical region  *
!********************************
750 if (nfloat.eq.0) then
    line = '  '
    read(nru,'(a)') line
    iline = iline + 1
    call linepro(nru,line,iline)
  endif
  if (nfloat.ge.4) then
!
!  Read as coordinate and radius
!
    xufree(1,ncurr) = floats(1)
    xufree(2,ncurr) = floats(2)
    xufree(3,ncurr) = floats(3)
    rufree(ncurr) = floats(4)
  elseif (nfloat.ge.2) then
    iufree(ncurr) = int(floats(1))
    rufree(ncurr) = floats(2)
  else
    call outerror('Insufficient input for unfreeze option',iline)
    call stopnow('strword')
  endif
  lufree(ncurr) = .true.
  lwordok = .true.
  return
!
!  Error handling
!
99 call outerror('Error reading input data - check format near '//word,iline)
  call stopnow('strword')
  end
