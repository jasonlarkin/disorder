  subroutine mcword(nru,word,lwordok,iline,line,l55,l1000,ncurr)
!
!  Processes input for Monte Carlo related words
!
!   1/01 Created from mdword
!   6/01 Initialisation of line added for benefit of some compilers
!  10/02 Option to specify volume added
!  11/04 Pi accessed from module
!   8/06 nru passed to linepro
!   5/07 Lowest sub-option added to mcsample
!   5/07 Probability of straining cell added and maximum displacement
!   5/07 Target strain added
!   6/07 Rotation types added
!   7/07 Default rotation type added
!   7/07 Input of gcmcexistingmolecules option added
!  12/08 Module input renamed to gulpinput
!   1/09 swap move added to Monte Carlo
!   1/09 mclowest option added
!   8/10 Reading of existing GCMC molecules modified
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
!  Copyright Curtin University 2010
!
!  Julian Gale, NRI, Curtin University, August 2010
!
  use constants
  use element,       only : maxele
  use files
  use gulpinput
  use molecule,      only : lgcmcmol, maxmol, nmollist, nmolatom, nmolptr, nmol, natmol
  use montecarlo
  use parallel
  use species
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
  logical                                  :: lwordok
!
!  Local variables
!
  integer(i4)                              :: i
  integer(i4)                              :: iend
  integer(i4)                              :: inat
  integer(i4)                              :: ind2
  integer(i4)                              :: itype
  integer(i4)                              :: j
  integer(i4), dimension(:), allocatable   :: listtmp 
  integer(i4)                              :: n1
  integer(i4)                              :: n2
  integer(i4)                              :: nai
  integer(i4)                              :: nbeg
  integer(i4)                              :: nlisttmp
  integer(i4)                              :: nm
  integer(i4)                              :: nmlast
  integer(i4)                              :: nmcmat
  integer(i4)                              :: nword1
  integer(i4)                              :: status
  logical                                  :: lsymbol
  real(dp)                                 :: units
!
  if (index(word,'mccr').eq.1) goto 100
  if (index(word,'mcde').eq.1) goto 110
  if (index(word,'mcmo').eq.1) goto 120
  if (index(word,'mcro').eq.1) goto 130
  if (index(word,'mctr').eq.1) goto 140
  if (index(word,'mcou').eq.1) goto 150
  if (index(word,'mcmaxd').eq.1) goto 160
  if (index(word,'gcmcs').eq.1) goto 170
  if (index(word,'mcsa').eq.1) goto 180
  if (index(word,'mcch').eq.1) goto 190
  if (index(word,'mcmaxr').eq.1) goto 200
  if (index(word,'gcmcm').eq.1) goto 210
  if (index(word,'mcste').eq.1) goto 220
  if (index(word,'mcme').eq.1) goto 230
  if (index(word,'mcvo').eq.1) goto 240
  if (index(word,'mcstr').eq.1) goto 250
  if (index(word,'mcmaxs').eq.1) goto 260
  if (index(word,'gcmce').eq.1) goto 270
  if (index(word,'mcsw').eq.1) goto 280
  if (index(word,'mclo').eq.1) goto 290
  return
!*************************
!  Creation probability  *
!*************************
100 if (nfloat.eq.0) then
    call outerror('creation probability is missing from input',iline)
    call stopnow('mcword')
  endif
  pcreate = floats(1)
  lwordok = .true.
  return
!****************************
!  Destruction probability  *
!****************************
110 if (nfloat.eq.0) then
    call outerror('destruction probability is missing from input',iline)
    call stopnow('mcword')
  endif
  pdestroy = floats(1)
  lwordok = .true.
  return
!*********************
!  Move probability  *
!*********************
120 if (nfloat.eq.0) then
    call outerror('move probability is missing from input',iline)
    call stopnow('mcword')
  endif
  pmove = floats(1)
  lwordok = .true.
  return
!*************************
!  Rotation probability  *
!*************************
130 if (nfloat.eq.0) then
    call outerror('rotation probability is missing from input',iline)
    call stopnow('mcword')
  endif
  protate = floats(1)
!
! Check for rotation types
!
  if (nword.gt.1) then
    nrotationtype = 0
    do i = 2,nword
      word = words(i)(1:20)
      call stolc(word,20_i4)
      if (index(word,'cent').ne.0) then
        nrotationtype = nrotationtype + 1
        if (nrotationtype.gt.3) then
          call outerror('too many rotation types in input',iline)
          call stopnow('mcword')
        endif
        nptrrotationtype(nrotationtype) = 1
      elseif (index(word,'atom').ne.0) then
        nrotationtype = nrotationtype + 1
        if (nrotationtype.gt.3) then
          call outerror('too many rotation types in input',iline)
          call stopnow('mcword')
        endif
        nptrrotationtype(nrotationtype) = 2
      elseif (index(word,'line').ne.0) then
        nrotationtype = nrotationtype + 1
        if (nrotationtype.gt.3) then
          call outerror('too many rotation types in input',iline)
          call stopnow('mcword')
        endif
        nptrrotationtype(nrotationtype) = 3
      endif
    enddo
  else
    nrotationtype = 1
    nptrrotationtype(1) = 1
  endif
  lwordok = .true.
  return
!*************************
!  Number of trial steps *
!*************************
140 if (nfloat.eq.0) then
    call outerror('number of trials is missing from input',iline)
    call stopnow('mcword')
  endif
  nmctrial = nint(floats(1))
  lwordok = .true.
  return
!************************
!  Frequency of output  *
!************************
150 if (nfloat.eq.0) then
    call outerror('output frequency is missing from input',iline)
    call stopnow('mcword')
  endif
  nmcoutfreq = nint(floats(1))
  lwordok = .true.
  return
!***********************************
!  Maximum Cartesian displacement  *
!***********************************
160 if (nfloat.eq.0) then
    call outerror('maximum displacement is missing from input',iline)
    call stopnow('mcword')
  endif
  units = 1.0_dp
  if (nword.gt.1) then
    do i = 1,nword
      if (index(words(i),'au').eq.1) then
        units = autoangs
      elseif (index(words(i),'tar').eq.1) then
        ntargetfreq = 10
        targetmove = 0.5_dp
      endif
    enddo
  endif
  if (nfloat.ge.3) then
    dmaxmc = floats(1)*units
    targetmove = floats(2)
    ntargetfreq = nint(floats(3))
  elseif (nfloat.eq.2) then
    dmaxmc = floats(1)*units
    targetmove = floats(2)
    ntargetfreq = 10
  elseif (nfloat.eq.1) then
    dmaxmc = floats(1)*units
  endif
  lwordok = .true.
  return
!**********************
!  GCMC Species info  *
!**********************
170 continue
175 line = '  '
  read(nru,'(a)',end=178) line
  iline = iline + 1
  call linepro(nru,line,iline)
!
!  Check whether input is symbol or option
!
  if ((nword+nfloat).eq.0) goto 175
!
!  Check for old fashion specification of number of atoms
!
  if (nword.eq.0.and.nfloat.eq.1) goto 175
  if (nword.gt.0) then
    word = words(1)(1:20)
    call worsy(word,lsymbol,.false.)
    if (.not.lsymbol) then
      call stolc(word,20_i4)
      if (index(word,'end').eq.1) then
        lwordok = .true.
        return
      else
        l55 = .true.
        goto 178
      endif
    endif
  endif
  ngcmcspec = ngcmcspec + 1
  if (ngcmcspec.gt.maxspec) then
    maxspec = ngcmcspec + 20
    call changemaxspec
  endif
  if (nword.eq.0) then
    inat = int(floats(1))
    if (inat.gt.100) inat = inat - 100 + maxele
    ngcmcnat(ngcmcspec) = inat
  elseif (nword.eq.1) then
    call ltont(words(1),inat,itype)
    ngcmcnat(ngcmcspec) = inat
    ngcmctype(ngcmcspec) = itype
  elseif (nword.eq.2) then
    call ltont(words(1),inat,itype)
    ngcmctype(ngcmcspec) = itype
    call stolc(words(2),maxword)
    if (index(words(2),'bco').eq.1.or.index(words(2),'bsh').eq.1) then
      ind2 = 2
    else
      ind2 = 1
    endif
    if (index(words(2),'she').eq.ind2) inat = inat + maxele
    ngcmcnat(ngcmcspec) = inat
  elseif (nword.ge.3) then
    call ltont(words(1),inat,itype)
    ngcmctype(ngcmcspec) = itype
    call stolc(words(2),maxword)
    if (index(words(2),'bco').eq.1.or.index(words(2),'bsh').eq.1) then
      ind2 = 2
    else
      ind2 = 1
    endif
    if (index(words(2),'she').eq.ind2) inat = inat + maxele
    ngcmcnat(ngcmcspec) = inat
  endif
  goto 175
178 if (.not.l55) l1000 = .true.
  lwordok = .true.
  return
!*********************
!  MC Sampling info  *
!*********************
180 continue
  lmcout = .true.
!
!  Filename
!
  if (nword.gt.1) then
    if (index(words(2),'lowe').eq.1) then
      lmclowestwrite = .true.
      if (nword.gt.2) then
        mcfile = words(3)
        if (index(mcfile,'.gmc').eq.0) then
          iend = index(mcfile,' ')
          if (iend.eq.0) iend = 57
          mcfile(iend:iend+3) = '.gmc'
        endif
      endif
    else
      mcfile = words(2)
      if (index(mcfile,'.gmc').eq.0) then
        iend = index(mcfile,' ')
        if (iend.eq.0) iend = 57
        mcfile(iend:iend+3) = '.gmc'
      endif
    endif
  endif
!
!  Sampling frequency
!
  if (nfloat.gt.0) then
    nmcsample = abs(nint(floats(1)))
  endif
  lwordok = .true.
  return
!***********************
!  Chemical potential  *
!***********************
190 if (nfloat.eq.0) then
    call outerror('chemical potential is missing from input',iline)
    call stopnow('mcword')
  endif
  chempot = floats(1)
  lwordok = .true.
  return
!************************************
!  Maximum rotational displacement  *
!************************************
200 if (nfloat.eq.0) then
    call outerror('maximum rotation is missing from input',iline)
    call stopnow('mcword')
  endif
  units = 1.0_dp/180.0_dp
  if (nword.gt.1) then
    do i = 1,nword
      if (index(words(i),'rad').eq.1) then
        units = 1.0_dp/pi
      elseif (index(words(i),'tar').eq.1) then
        ntargetfreqr = 10
        targetrota = 0.5_dp
      endif
    enddo
  endif
  if (nfloat.ge.3) then
    rmaxmc = floats(1)*units
    targetrota = floats(2)
    ntargetfreqr = nint(floats(3))
  elseif (nfloat.eq.2) then
    rmaxmc = floats(1)*units
    targetrota = floats(2)
    ntargetfreqr = 10
  elseif (nfloat.eq.1) then
    rmaxmc = floats(1)*units
  endif
  lwordok = .true.
  return
!******************************
!  GCMC molecule coordinates  *
!******************************
210 units = 1.0_dp
  if (nword.gt.1) then
    call stolc(words(2),maxword)
    if (index(words(2),'au').eq.1) then
      units = autoangs
    endif
  endif
  if (ngcmcmol+1.gt.maxgcmcmol) then
    maxgcmcmol = ngcmcmol + 1
    call changemaxgcmcmol
  endif
  ngcmcmol = ngcmcmol + 1
  ngcmcmolat(ngcmcmol) = 0
  nmcmat = 0
!
!  Start of input loop
!
215 line = '  '
  read(nru,'(a)',end=218) line
  iline = iline + 1
  call linepro(nru,line,iline)
!
!  Check whether input is symbol or option
!
  if ((nword+nfloat).eq.0) goto 215
!
!  Check for old fashion specification of number of atoms
!
  if (nword.eq.0.and.nfloat.eq.1) goto 215
  if (nword.gt.0) then
    word = words(1)(1:20)
    call worsy(word,lsymbol,.false.)
    if (.not.lsymbol) then
      l55 = .true.
      goto 218
    endif
  endif
  ngcmcmolat(ngcmcmol) = ngcmcmolat(ngcmcmol) + 1
  nmcmat = nmcmat + 1
  if (nmcmat.gt.maxgcmcmolat) then
    maxgcmcmolat = nmcmat + 10
    call changemaxgcmcmol
  endif
!
!  Symbols used in input
!
  if (nword.gt.0) then
    call ltont(words(1),inat,itype)
    ngcmcmolnat(nmcmat,ngcmcmol) = inat
    if (nword.ge.2) then
      if ((index(words(2),'s').eq.1).or.(index(words(2),'S').eq.1)) &
        ngcmcmolnat(nmcmat,ngcmcmol) = ngcmcmolnat(nmcmat,ngcmcmol) + maxele
    endif
    ngcmcmoltype(nmcmat,ngcmcmol) = itype
    nbeg = 0
  else
!
!  Numeric input
!
    nai = int(floats(1))
    if (nai.gt.100) nai = nai - 100 + maxele
    ngcmcmolnat(nmcmat,ngcmcmol) = nai
    nbeg = 1
    nfloat = nfloat - 1
  endif
!
!  Assign coefficients and cutoffs
!
  if (nfloat.eq.3) then
    xgcmcmol(nmcmat,ngcmcmol) = floats(1+nbeg)*units
    ygcmcmol(nmcmat,ngcmcmol) = floats(2+nbeg)*units
    zgcmcmol(nmcmat,ngcmcmol) = floats(3+nbeg)*units
  else
    call outerror('error in input for GCMC molecule atoms',iline)
    call stopnow('mcword')
  endif
  goto 215
!
!  End of input loop
!
218 if (.not.l55) l1000 = .true.
  lwordok = .true.
  return
!*****************************
!  Number of MC steps so far *
!*****************************
220 if (nfloat.lt.2) then
    call outerror('number of steps is missing from input',iline)
    call stopnow('mcword')
  endif
  nmcstep = nint(floats(1))
  nmcaccepted = nint(floats(2))
  lwordok = .true.
  return
!***************************
!  Current MC mean values  *
!***************************
230 if (nfloat.lt.2) then
    call outerror('mean MC value is missing from input',iline)
    call stopnow('mcword')
  endif
  mcemean = floats(1)
  mcnmean = floats(2)
  lwordok = .true.
  return
!***********************
!  Monte Carlo volume  *
!***********************
240 if (nfloat.eq.0) then
    call outerror('Monte Carlo volume is missing from input',iline)
    call stopnow('mcword')
  endif
  mcvolume = floats(1)
  lwordok = .true.
  return
!***********************
!  Strain probability  *
!***********************
250 if (nfloat.eq.0) then
    call outerror('strain probability is missing from input',iline)
    call stopnow('mcword')
  endif
  pstrain = floats(1)
  lwordok = .true.
  return
!********************************
!  Maximum strain displacement  *
!********************************
260 if (nfloat.eq.0) then
    call outerror('maximum strain is missing from input',iline)
    call stopnow('mcword')
  endif
  if (nword.gt.1) then
    do i = 1,nword
      if (index(words(i),'tar').eq.1) then
        ntargetfreqs = 10
        targetstrain = 0.5_dp
      endif
    enddo
  endif
  if (nfloat.ge.3) then
    smaxmc = abs(floats(1))
    targetstrain = floats(2)
    ntargetfreqs = nint(floats(3))
  elseif (nfloat.eq.2) then
    smaxmc = abs(floats(1))
    targetstrain = floats(2)
    ntargetfreqs = 10
  elseif (nfloat.eq.1) then
    smaxmc = abs(floats(1))
  endif
  if (smaxmc.ge.1.0_dp) then
    call outerror('strain maximum is too large',iline)
    call stopnow('mcword')
  endif
  lwordok = .true.
  return
!****************************
!  GCMC existing molecules  *
!****************************
270 if (nfloat.eq.0) then
    call outerror('numbers of GCMC existing molecules missing from input',iline)
    call stopnow('mcword')
  endif
!
  nlisttmp = 0
  if (nword.ge.2) then
    if (index(words(2),'to').eq.1.and.nfloat.ge.2) then
!
!  Range of values input
!
      n1 = nint(abs(floats(1)))
      n2 = nint(abs(floats(2)))
      if (n1.gt.n2) then
        nm = n1
        n1 = n2
        n2 = nm
      endif
      if (n2.gt.maxmol) then
        maxmol = n2
        call changemaxmol
      endif
      allocate(listtmp(n2-n1+1),stat=status)
      if (status/=0) call outofmemory('mcword','listtmp')
      do nm = n1,n2
        lgcmcmol(nm) = .true.
        nlisttmp = nlisttmp + 1
        listtmp(nlisttmp) = nm
      enddo
    else
!
!  List of values
!
      allocate(listtmp(nfloat),stat=status)
      if (status/=0) call outofmemory('mcword','listtmp')
      do i = 1,nfloat
        nm = nint(abs(floats(i)))
        if (nm.gt.maxmol) then
          maxmol = nm
          call changemaxmol
        endif
        lgcmcmol(nm) = .true.
        nlisttmp = nlisttmp + 1
        listtmp(nlisttmp) = nm
      enddo
    endif
  else
!
!  List of values
!
    allocate(listtmp(nfloat),stat=status)
    if (status/=0) call outofmemory('mcword','listtmp')
    do i = 1,nfloat
      nm = nint(abs(floats(i)))
      if (nm.gt.maxmol) then
        maxmol = nm
        call changemaxmol
      endif
      lgcmcmol(nm) = .true.
      nlisttmp = nlisttmp + 1
      listtmp(nlisttmp) = nm
    enddo
  endif
!
!  Read lists of atoms in molecules
!
  do i = 1,nlisttmp
275 line = '  '
    read(nru,'(a)') line
    iline = iline + 1
    call linepro(nru,line,iline)
!
!  Check whether line is blank
!
    if ((nword+nfloat).eq.0) goto 275
!
!  Assign molecule information
!
    nm = listtmp(i)
    nmolatom(nm) = nint(floats(1))
    if (nfloat.lt.nmolatom(nm)+1) then
      call outerror('molecule information is missing from input',iline)
      call stopnow('mcword')
    endif
!
!  Find point to insert molecule lists
!
    if (i.eq.1) then
      nmolptr(nm) = 0
    else
      nmlast = listtmp(i-1)
      nmolptr(nm) = nmolptr(nmlast) + nmolatom(nmlast)
    endif
!
    do j = 1,nmolatom(nm)
      nmollist(nmolptr(nm)+j) = nint(floats(1+j))
      natmol(nmollist(nmolptr(nm)+j)) = nm
    enddo
!
!  Set number of molecules to be largest value in list
!
    nmol = max(nmol,nm)
  enddo
!
!  Free temporary array
!
  deallocate(listtmp,stat=status)
  if (status/=0) call deallocate_error('mcword','listtmp')
!
  lwordok = .true.
  return
!*********************
!  Swap probability  *
!*********************
280 if (nfloat.eq.0) then
    call outerror('swap probability is missing from input',iline)
    call stopnow('mcword')
  endif
  pswap = floats(1)
!
!  Are any sub-options specified?
!
  if (nword.gt.0) then
    if (index(words(1),'any').eq.0) then
!
!  Any sub-option
!
      lmcswapany = .true.
    else
!
!  Only sub-option
!
      lmcswapany = .false.
      nword1 = 1
      if (index(words(1),'onl').eq.1) nword1 = 2
!
!  Read species list
!
      nmcswapspec = 0
      do i = nword1,nword
        call worsy(words(i)(1:20),lsymbol,.false.)
        if (lsymbol) then
          nmcswapspec = nmcswapspec + 1
!
!  Check that there is space in the array for this species
!
          if (nmcswapspec.gt.maxmcswapspec) then
            maxmcswapspec = nmcswapspec + 5
            call changemaxmcswapspec
          endif
!
!  Add details of specie
!
          call ltont(words(i),inat,itype)
          nmcswapnat(nmcswapspec) = inat
          nmcswaptype(nmcswapspec) = itype
        endif
      enddo
!
!  If number of actual species is zero then stop
!
      if (nmcswapspec.eq.0) then
        call outerror('number of species specified for swap only is zero',iline)
        call stopnow('mcword')
      endif
    endif
  endif
  lwordok = .true.
  return
!*****************************
!  Current MC lowest values  *
!*****************************
290 if (nfloat.lt.1) then
    call outerror('lowest MC value is missing from input',iline)
    call stopnow('mcword')
  endif
  mcelowest = floats(1)
  lwordok = .true.
  return
!
  end
