  subroutine defword(nru,word,lwordok,iline,line,l55,l1000,ncurr)
!
!  Processes input for defects
!
!  ndef    = total number of defects
!  ndefcfg = pointer to structure for defects
!  ndeftyp = defect type number
!  ndefnat = defect ion atomic number
!  ndeftp  = defect ion type number
!  nreg1   = number of ions in region 1
!  nreg1old= number of ions in region 1 for perfect lattice
!  ndcentyp= defect centre type specifier
!  xdcent  = x coordinate of defect centre
!  ydcent  = y coordinate of defect centre
!  zdcent  = z coordinate of defect centre
!  reg1    = region 1 radius
!  reg2    = region 2 radius
!  reg1last= region 1 radius from the last run when restarting
!  reg2a1  = radius for transfer of 2a ions to region 1 at end
!  xdef    = x coordinate for defect or atom/molecule number
!  ydef    = y coordinate for defect
!  zdef    = z coordinate for defect
!  ndptr   = pointer to vacancies and interstitials for fast region 2a
!  nvaca   = no. of vacancies
!  ninte   = no. of interstitials
!  nreldef = perfect region 1 atom to which defect is equivalent
!  ldeflin = .true. if list of vacancies and interstitials is input
!            for restart
!  lreldin = .true. if list of nreldef is input
!  ldeffix = .true. if impurity/intersitial is to be fixed during opt
!  ldcellr = if .true. then regions are constructed from unit cell
!            blocks => modification for Sasha!
!
!  Defect type specification numbers:
!
!  0 = explicit specification of region 1 (normally restart)
!
!  Vacancy;
!
!  1 = atom number to be removed
!  2 = atom symbol to be removed
!  3 = fractional coordinates to be removed
!  4 = cartesian coordinates to be removed
!  5 = molecule number to be removed
!
!  Impurities;
!
!  11 = atom number to be replaced
!  12 = atom symbol to be replaced
!  13 = fractional coordinates of atom to be replaced
!  14 = cartesian coordinates of atom to be replaced
!
!  Interstitials;
!
!  21 = fractional coordinates used
!  22 = cartesian coordinates used
!  23 = bond directive - symbol
!  24 = bond directive - fractional coordinates
!  25 = bond directive - cartesian coordinates
!
!  mode2a => controls the method for determining the region 2a displacements
!
!  11/96 Option to fix position of impurity or intersitial added
!   4/98 Input channel for reading generalised
!   7/00 lflags now in control module
!   6/01 Initialisation of line added for benefit of some compilers
!  10/02 Maxat now incremented if region 1 being read in to appropriate size
!   8/06 nru passed to linepro
!  11/07 Implicit type conversion made explicit
!  12/08 Module input renamed to gulpinput
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
!  Copyright Curtin University 2008
!
!  Julian Gale, NRI, Curtin University, December 2008
!
  use datatypes
  use constants
  use control
  use configurations
  use current
  use defects
  use element
  use general
  use gulpinput
  use iochannels
  use molecule
  use parallel
  use reallocate
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
  integer(i4)                              :: ierror
  integer(i4)                              :: inat
  integer(i4)                              :: ind
  integer(i4)                              :: ind2
  integer(i4)                              :: indb
  integer(i4)                              :: iptr
  integer(i4), dimension(:), pointer       :: itmp
  integer(i4)                              :: itype
  integer(i4)                              :: j
  integer(i4)                              :: nbeg
  integer(i4), save                        :: ndexplicit = 0
  integer(i4)                              :: nstart
  integer(i4)                              :: ntot
  integer(i4)                              :: status
  logical                                  :: lfound
  logical                                  :: lsymbol
  real(dp)                                 :: units
!
  nullify(itmp)
!
!  Check for defect words
!
  if (index(word,'cent').eq.1) goto 100
  if (index(word,'size').eq.1) goto 110
  if (index(word,'regi').eq.1) goto 120
  if (index(word,'vaca').eq.1) goto 130
  if (index(word,'impu').eq.1) goto 140
  if (index(word,'inters').eq.1) goto 150
  if (index(word,'move').eq.1) goto 160
  if (index(word,'mode').eq.1) goto 170
  if (index(word,'defl').eq.1) goto 180
  if (index(word,'reld').eq.1) goto 190
  return
!******************
!  Defect centre  *
!******************
100 ind = 2
  if (nfloat+nword.eq.1) then
    line = '  '
    read(nru,'(a)') line
    iline = iline + 1
    call linepro(nru,line,iline)
    ind=1
  endif
  if (nfloat+nword.eq.ind-1) then
    call outerror('Defect centre missing from input',iline)
    call stopnow('defword')
  endif
  if (nword.gt.ind-1) then
    if (index(words(ind),'cart').eq.1) then
!
!  Cartesian coordinates input
!
      ndcentyp(ncurr) = 4
      if (nfloat.ge.3) then
        xdcent(ncurr) = floats(1)*scalefactor
        ydcent(ncurr) = floats(2)*scalefactor
        zdcent(ncurr) = floats(3)*scalefactor
      else
        call outerror('Insufficient coordinates supplied',iline)
        call stopnow('defword')
      endif
    elseif (index(words(ind),'frac').eq.1) then
!
!  Fractional coordinates input
!
      ndcentyp(ncurr) = 3
      if (nfloat.ge.3) then
        xdcent(ncurr) = floats(1)
        ydcent(ncurr) = floats(2)
        zdcent(ncurr) = floats(3)
      else
        call outerror('Insufficient coordinates supplied',iline)
        call stopnow('defword')
      endif
    elseif (index(words(ind),'mol').eq.1) then
!
!  Molecule centroid
!
      if (.not.lmol) then
        call outerror('Molecule used to specify defect centre without molecules active',iline)
        call stopnow('defword')
      endif
      ndcentyp(ncurr) = 5
      if (nfloat.ge.1) then
        xdcent(ncurr) = floats(1)
      else
        call outerror('Molecule number missing for defect centre',iline)
        call stopnow('defword')
      endif
    else
!
!  Symbol input
!
      ndcentyp(ncurr) = 2
      call ltont(words(ind),inat,itype)
      lfound = .false.
      nstart = nasum + 1 - nasym
      do i = nstart,nasum
        if (inat.eq.natcfg(i).and.(itype.eq.ntypcfg(i).or.itype.eq.0)) then
          if (lfound) then
            call outerror('Ambiguous defect centre specification',iline)
            call stopnow('defword')
          endif
          lfound = .true.
          iptr = i
        endif
      enddo
      if (lfound) then
        xdcent(ncurr) = iptr
      else
        call outerror('Defect centre species not found',iline)
        call stopnow('defword')
      endif
    endif
  else
    if (nfloat.ge.3) then
!
!  Three numbers - assume fractional coordinate input
!
      ndcentyp(ncurr) = 3
      xdcent(ncurr) = floats(1)
      ydcent(ncurr) = floats(2)
      zdcent(ncurr) = floats(3)
    else
!
!  One number - assume to be an atom number
!
      ndcentyp(ncurr) = 1
      xdcent(ncurr) = floats(1)
    endif
  endif
  lwordok = .true.
  return
!*************************
!  Region 1 and 2 sizes  *
!*************************
110 units = 1.0_dp
  if (nword.gt.1) then
    do i=2,nword
      if (index(words(i),'au').eq.1) units = autoangs
      if (index(words(i),'neu').eq.1) ldcellr = .true.
    enddo
  endif
  if (nfloat.gt.0) then
    reg1(ncurr) = floats(1)*units
    if (nfloat.gt.1) then
      reg2(ncurr) = floats(2)*units
    else
      reg2(ncurr) = reg1(ncurr)
    endif
    if (nfloat.gt.2) then
      reg1last(ncurr) = floats(3)*units
    endif
  else
    line = '  '
    read(nru,'(a)') line
    iline = iline + 1
    call linepro(nru,line,iline)
    if (nword.gt.0) then
      if (index(words(1),'au').eq.1) units = autoangs
    endif
    if (nfloat.gt.0) then
      reg1(ncurr) = floats(1)*units
      if (nfloat.gt.1) then
        reg2(ncurr) = floats(2)*units
      endif
      if (nfloat.gt.2) then
        reg1last(ncurr) = floats(3)*units
      endif
    else
      call outerror('Region 1 and 2 sizes are missing',iline)
      call stopnow('defword')
    endif
  endif
!
!  Check that restart region is larger than previous region 1
!
  if (reg1last(ncurr).gt.reg1(ncurr)) then
    call outerror('Region 1 for restart is smaller than original region 1',iline)
    call stopnow('defword')
  endif
  lwordok = .true.
  return
!***************************************
!  Explicit specification of region 1  *
!***************************************
120 units = 1.0_dp
  if (nword.gt.1) then
    call stolc(words(2),maxword)
    if (index(words(2),'au').eq.1) then
      units = autoangs
    endif
  endif
  allocate(itmp(3*maxr1at),stat=status)
  if (status/=0) call outofmemory('defword','itmp')
  nreg1 = 0
  ndef = ndef + 1
  ndefcfg(ndef) = ncurr
  ndeftyp(ndef) = 0
125  line = '  '
  read(nru,'(a)',end=128) line
  iline = iline + 1
  call linepro(nru,line,iline)
!
!  Test whether another atom has been read or an option
!
  if ((nword+nfloat).eq.0) goto 125
  if (nword.gt.0) then
    word = words(1)(1:20)
    call worsy(word,lsymbol,.false.)
    if (.not.lsymbol) then
      l55 = .true.
      goto 128
    endif
  endif
  nreg1 = nreg1 + 1
  if (nreg1.gt.maxat) then
    maxat = nreg1 + 20
    call changemaxat
  endif
  if (nreg1.gt.maxr1at) then
    maxr1at = nreg1 + 20
    call changemaxr1at
    call realloc(itmp,3_i4*maxr1at,ierror)
    if (ierror.ne.0) call outofmemory('defword','itmp')
  endif
!
!  Optimisation flags
!
  if (lflags) then
    itmp(3*(nreg1-1)+1) = int(floats(nfloat-2))
    itmp(3*(nreg1-1)+2) = int(floats(nfloat-1))
    itmp(3*(nreg1-1)+3) = int(floats(nfloat))
    nfloat = nfloat - 3
  endif
!
!  Symbols used in input
!
  if (nword.gt.0) then
    call ltont(words(1),inat,itype)
    nat(nreg1) = inat
    ldefbsmat(nreg1) = .false.
    if (nword.ge.2) then
      if ((index(words(2),'s').eq.1).or.(index(words(2),'S').eq.1)) nat(nreg1) = nat(nreg1) + maxele
      if ((index(words(2),'q').eq.1).or.(index(words(2),'Q').eq.1)) then
        ldqmatom(nreg1) = .true.
        if ((index(words(2),'b').eq.2).or.(index(words(2),'B').eq.2)) then
          ldefbsmat(nreg1) = .true.
          if ((index(words(2),'s').eq.3).or.(index(words(2),'S').eq.3)) nat(nreg1) = nat(nreg1) + maxele
        elseif ((index(words(2),'s').eq.2).or.(index(words(2),'S').eq.2)) then
          nat(nreg1) = nat(nreg1) + maxele
        endif
      endif
      if ((index(words(2),'b').eq.1).or.(index(words(2),'B').eq.1)) then
        ldefbsmat(nreg1) = .true.
        if ((index(words(2),'s').eq.2).or.(index(words(2),'S').eq.2)) nat(nreg1) = nat(nreg1) + maxele
      endif
    endif
    nftype(nreg1) = itype
    nbeg = 0
  else
!
!  Numeric input
!
    inat = int(floats(1))
    if (inat.gt.100) inat = inat - 100 + maxele
    nat(nreg1) = inat
    nbeg = 1
    nfloat = nfloat - 1
  endif
  if (nfloat.gt.8) nfloat = nfloat - 3
!
!  Assign coefficients and cutoffs
!
  if (nfloat.eq.8) then
    xdefe(nreg1) = floats(1+nbeg)*units
    ydefe(nreg1) = floats(2+nbeg)*units
    zdefe(nreg1) = floats(3+nbeg)*units
    qdefe(nreg1) = floats(4+nbeg)
    occdefe(nreg1) = floats(5+nbeg)
    radefe(nreg1) = floats(6+nbeg)
    natmol(nreg1) = nint(floats(7+nbeg))
    nmolind(nreg1) = nint(floats(8+nbeg))
  elseif (nfloat.eq.6.or.nfloat.eq.7) then
    xdefe(nreg1) = floats(1+nbeg)*units
    ydefe(nreg1) = floats(2+nbeg)*units
    zdefe(nreg1) = floats(3+nbeg)*units
    qdefe(nreg1) = floats(4+nbeg)
    occdefe(nreg1) = floats(5+nbeg)
    radefe(nreg1) = floats(6+nbeg)
    natmol(nreg1) = 0
    nmolind(nreg1) = 0
  elseif (nfloat.eq.5) then
    xdefe(nreg1) = floats(1+nbeg)*units
    ydefe(nreg1) = floats(2+nbeg)*units
    zdefe(nreg1) = floats(3+nbeg)*units
    qdefe(nreg1) = floats(4+nbeg)
    occdefe(nreg1) = floats(5+nbeg)
    radefe(nreg1) = 0.0_dp
    natmol(nreg1) = 0
    nmolind(nreg1) = 0
  elseif (nfloat.eq.4) then
    xdefe(nreg1) = floats(1+nbeg)*units
    ydefe(nreg1) = floats(2+nbeg)*units
    zdefe(nreg1) = floats(3+nbeg)*units
    qdefe(nreg1) = floats(4+nbeg)
    occdefe(nreg1) = 1.0_dp
    radefe(nreg1) = 0.0_dp
    natmol(nreg1) = 0
    nmolind(nreg1) = 0
  elseif (nfloat.eq.3) then
    xdefe(nreg1) = floats(1+nbeg)*units
    ydefe(nreg1) = floats(2+nbeg)*units
    zdefe(nreg1) = floats(3+nbeg)*units
    qdefe(nreg1) = 0.0_dp
    occdefe(nreg1) = 1.0_dp
    radefe(nreg1) = 0.0_dp
    natmol(nreg1) = 0
    nmolind(nreg1) = 0
  else
    call outerror('Cartesian coordinates missing for region 1',iline)
    call stopnow('defword')
  endif
  goto 125
!
!  Write out scratch file for region 1 storage
!
128 if (ioproc) write(41) ncurr,nreg1
  do i = 1,nreg1
    write(41) nat(i),nftype(i),xdefe(i),ydefe(i),zdefe(i),qdefe(i), &
      radefe(i),occdefe(i),natmol(i),nmolind(i),ldefbsmat(i),ldqmatom(i)
    if (lflags) then
      if (ioproc) write(41) itmp(3*(i-1)+1),itmp(3*(i-1)+2),itmp(3*(i-1)+3)
    endif
  enddo
  lwordok = .true.
  if (.not.l55) l1000 = .true.
  ndexplicit = ncurr
  deallocate(itmp,stat=status)
  if (status/=0) call deallocate_error('defword','itmp')
  return
!************
!  Vacancy  *
!************
130 ind = 2
  if (nfloat+nword.eq.1) then
    line = '  '
    read(nru,'(a)') line
    iline = iline + 1
    call linepro(nru,line,iline)
    ind = 1
  endif
  if (nfloat+nword.eq.ind-1) then
    call outerror('Vacancy specification is missing',iline)
    call stopnow('defword')
  endif
  ndef = ndef + 1
  if (ndef.gt.maxdef) then
    maxdef = ndef + 10
    call changemaxdef
  endif
  ndefcfg(ndef) = ncurr
  if (nword.gt.ind-1) then
    if (index(words(ind),'cart').eq.1) then
!
!  Cartesian coordinates input
!
      ndeftyp(ndef) = 4
      if (nfloat.ge.3) then
        xdef(ndef) = floats(1)*scalefactor
        ydef(ndef) = floats(2)*scalefactor
        zdef(ndef) = floats(3)*scalefactor
      else
        call outerror('Insufficient coordinates supplied',iline)
        call stopnow('defword')
      endif
    elseif (index(words(ind),'frac').eq.1) then
!
!  Fractional coordinates input
!
      ndeftyp(ndef) = 3
      if (nfloat.ge.3) then
        xdef(ndef) = floats(1)
        ydef(ndef) = floats(2)
        zdef(ndef) = floats(3)
      else
        call outerror('Insufficient coordinates supplied',iline)
        call stopnow('defword')
      endif
    elseif (index(words(ind),'mol').eq.1) then
!
!  Molecule number input
!
      if (.not.lmol) then
        call outerror('Molecule used to specify vacancy without molecules active',iline)
        call stopnow('defword')
      endif
      ndeftyp(ndef) = 5
      if (nfloat.ge.1) then
        xdef(ndef) = floats(1)
      else
        call outerror('Molecule number missing for vacancy',iline)
        call stopnow('defword')
      endif
    else
!
!  Symbol input
!
      ndeftyp(ndef) = 2
      call ltont(words(ind),inat,itype)
      if (nword.ge.ind+1) then
        if (index(words(ind+1),'she').ne.0) then
          inat = inat + maxele
        elseif (index(words(ind+1),'cor').eq.0) then
          inat = inat + 2*maxele
        endif
      else
        inat = inat + 2*maxele
      endif
      ndefnat(ndef) = inat
      ndeftp(ndef) = itype
      lfound = .false.
      nstart = nasum+1-nasym
      if (inat.gt.2*maxele) inat = inat - 2*maxele
      do i = nstart,nasum
        if (inat.eq.natcfg(i).and.(itype.eq.ntypcfg(i).or.itype.eq.0)) then
          lfound = .true.
        endif
      enddo
      if (.not.lfound) then
        call outerror('Vacancy species not found',iline)
      endif
    endif
  else
    if (nfloat.ge.3) then
!
!  Three numbers - assume fractional coordinate input
!
      ndeftyp(ndef) = 3
      xdef(ndef) = floats(1)
      ydef(ndef) = floats(2)
      zdef(ndef) = floats(3)
    else
!
!  One number - assume to be an atom number
!
      ndeftyp(ndef) = 1
      xdef(ndef) = floats(1)
    endif
  endif
  lwordok = .true.
  return
!*************
!  Impurity  *
!*************
140 ind = 2
  if (nfloat+nword.eq.1) then
    line = '  '
    read(nru,'(a)') line
    iline = iline + 1
    call linepro(nru,line,iline)
    ind = 1
  endif
  if (nfloat+nword.eq.ind-1) then
    call outerror('Impurity specification missing',iline)
    call stopnow('defword')
  endif
  ndef = ndef + 1
  if (ndef.gt.maxdef) then
    maxdef = ndef + 10
    call changemaxdef
  endif
  ndefcfg(ndef) = ncurr
!
!  Check for fix option word
!
  i = ind
  lfound = .false.
  inddeffix(ndef) = 0
  do while (.not.lfound.and.i.le.nword)
    call stolc(words(i),maxword)
    if (index(words(i),'fix').ne.0) then
      lfound = .true.
      ldeffix(ndef) = .true.
      do j = i+1,nword
        words(j-1) = words(j)
      enddo
      nword = nword - 1
    endif
    i = i + 1
  enddo
  if (ldeffix(ndef)) then
    i = ind
    lfound = .false.
    do while (.not.lfound.and.i.le.nword)
      call stolc(words(i),maxword)
      if (index(words(i),'x ').eq.1) then
        lfound = .true.
        inddeffix(ndef) = 1
      elseif (index(words(i),'y ').eq.1) then
        inddeffix(ndef) = 2
      elseif (index(words(i),'z ').eq.1) then
        inddeffix(ndef) = 3
      elseif (index(words(i),'xy ').eq.1) then
        inddeffix(ndef) = 4
      elseif (index(words(i),'xz ').eq.1) then
        inddeffix(ndef) = 5
      elseif (index(words(i),'yz ').eq.1) then
        inddeffix(ndef) = 6
      elseif (index(words(i),'xyz ').eq.1) then
        inddeffix(ndef) = 0
      endif
      i = i + 1
    enddo
    if (lfound) then
      do j = i+1,nword
        words(j-1) = words(j)
      enddo
      nword = nword - 1
    endif
  endif
  if (index(words(ind),'cart').eq.1) then
!
!  Cartesian coordinates input
!
    ndeftyp(ndef) = 14
    if (nfloat.ge.4) then
      inat = nint(floats(1))
      if (inat.gt.100) inat = inat - 100 + maxele
      ndefnat(ndef) = inat
      ndeftp(ndef) = 0
      xdef(ndef) = floats(2)*scalefactor
      ydef(ndef) = floats(3)*scalefactor
      zdef(ndef) = floats(4)*scalefactor
    elseif (nfloat.eq.3) then
      if (nword.ge.ind+1) then
        call ltont(words(ind+1),inat,itype)
        ndefnat(ndef) = inat
        ndeftp(ndef) = itype
      else
        call outerror('Atom specification missing for impurity',iline)
        call stopnow('defword')
      endif
      xdef(ndef) = floats(1)*scalefactor
      ydef(ndef) = floats(2)*scalefactor
      zdef(ndef) = floats(3)*scalefactor
    else
      call outerror('Insufficient coordinates supplied',iline)
      call stopnow('defword')
    endif
  elseif (index(words(ind),'frac').eq.1) then
!
!  Fractional coordinates input
!
    ndeftyp(ndef) = 13
    if (nfloat.ge.4) then
      inat = nint(floats(1))
      if (inat.gt.100) inat = inat - 100 + maxele
      ndefnat(ndef) = inat
      ndeftp(ndef) = 0
      xdef(ndef) = floats(2)
      ydef(ndef) = floats(3)
      zdef(ndef) = floats(4)
    elseif (nfloat.eq.3) then
      if (nword.ge.ind+1) then
        call ltont(words(ind+1),inat,itype)
        ndefnat(ndef) = inat
        ndeftp(ndef) = itype
      else
        call outerror('Atom specification missing for impurity',iline)
        call stopnow('defword')
      endif
      xdef(ndef) = floats(1)
      ydef(ndef) = floats(2)
      zdef(ndef) = floats(3)
    else
      call outerror('Insufficient coordinates supplied',iline)
      call stopnow('defword')
    endif
  else
!
!  Symbol input first
!
    call ltont(words(ind),inat,itype)
    ndeftp(ndef) = itype
    if (nword.ge.ind+1) then
      ind2 = 1
      if (index(words(ind+1),'bs').eq.1.or.index(words(ind+1),'bc').eq.1) then
        inat = inat + 3*maxele
        ind2 = 2
      endif
      if (index(words(ind+1),'sh').eq.ind2) then
        inat = inat + maxele
        ind = ind + 1
      elseif (index(words(ind+1),'co').ne.ind2) then
        inat = inat + 2*maxele
      else
        ind = ind + 1
      endif
    else
      inat = inat + 2*maxele
    endif
    ndefnat(ndef) = inat
    if (nword.ge.ind+1) then
      if (index(words(ind+1),'cart').ne.0) then
        ndeftyp(ndef) = 14
        if (nfloat.ge.3) then
          xdef(ndef) = floats(1)*scalefactor
          ydef(ndef) = floats(2)*scalefactor
          zdef(ndef) = floats(3)*scalefactor
        else
          call outerror('Insufficient coordinates supplied',iline)
          call stopnow('defword')
        endif
      elseif (index(words(ind+1),'frac').ne.0) then
        ndeftyp(ndef) = 13
        if (nfloat.ge.3) then
          xdef(ndef) = floats(1)
          ydef(ndef) = floats(2)
          zdef(ndef) = floats(3)
        else
          call outerror('Insufficient coordinates supplied',iline)
          call stopnow('defword')
        endif
      else
        ndeftyp(ndef) = 12
        call ltont(words(ind+1),inat,itype)
        lfound = .false.
        nstart = nasum + 1 - nasym
        do i = nstart,nasum
          if (inat.eq.natcfg(i).and.(itype.eq.ntypcfg(i).or.itype.eq.0)) then
            if (.not.lfound) then
              lfound = .true.
              iptr = i
            endif
          endif
        enddo
        if (lfound) then
          xdef(ndef) = iptr
        else
          call outerror('Impurity species not found',iline)
          call stopnow('defword')
        endif
      endif
    else
      if (nfloat.ge.3) then
!
!  Three numbers - assume fractional coordinate input
!
        ndeftyp(ndef) = 13
        xdef(ndef) = floats(1)
        ydef(ndef) = floats(2)
        zdef(ndef) = floats(3)
      elseif (nfloat.ge.1) then
!
!  One number - assume to be an atom number
!
        ndeftyp(ndef) = 11
        xdef(ndef) = floats(1)
      else
        call outerror('Insufficient coordinates supplied',iline)
        call stopnow('defword')
      endif
    endif
  endif
  lwordok = .true.
  return
!*****************
!  Interstitial  *
!*****************
150 ind = 2
  if (nfloat+nword.eq.1) then
    line = '  '
    read(nru,'(a)') line
    iline = iline + 1
    call linepro(nru,line,iline)
    ind = 1
  endif
  if (nfloat+nword.eq.ind-1) then
    call outerror('Interstitial specification missing',iline)
    call stopnow('defword')
  endif
  ndef = ndef + 1
  if (ndef.gt.maxdef) then
    maxdef = ndef + 10
    call changemaxdef
  endif
!
!  Check for fix option word
!
  i = ind
  lfound = .false.
  inddeffix(ndef) = 0
  do while (.not.lfound.and.i.le.nword)
    call stolc(words(i),maxword)
    if (index(words(i),'fix').ne.0) then
      lfound = .true.
      ldeffix(ndef) = .true.
      do j = i+1,nword
        words(j-1) = words(j)
      enddo
      nword = nword - 1
    endif
    i = i + 1
  enddo
  if (ldeffix(ndef)) then
    i = ind
    lfound = .false.
    do while (.not.lfound.and.i.le.nword)
      call stolc(words(i),maxword)
      if (index(words(i),'x ').eq.1) then
        lfound = .true.
        inddeffix(ndef) = 1
      elseif (index(words(i),'y ').eq.1) then
        inddeffix(ndef) = 2
      elseif (index(words(i),'z ').eq.1) then
        inddeffix(ndef) = 3
      elseif (index(words(i),'xy ').eq.1) then
        inddeffix(ndef) = 4
      elseif (index(words(i),'xz ').eq.1) then
        inddeffix(ndef) = 5
      elseif (index(words(i),'yz ').eq.1) then
        inddeffix(ndef) = 6
      elseif (index(words(i),'xyz ').eq.1) then
        inddeffix(ndef) = 0
      endif
      i = i + 1
    enddo
    if (lfound) then
      do j = i+1,nword
        words(j-1) = words(j)
      enddo
      nword = nword - 1
    endif
  endif
  ndefcfg(ndef) = ncurr
  if (nword.gt.ind-1) then
    if (index(words(ind),'cart').eq.1) then
!
!  Cartesian coordinates input
!
      ndeftyp(ndef) = 22
      if (nfloat.ge.4) then
        inat = nint(floats(1))
        if (inat.gt.100) inat = inat - 100 + maxele
        ndefnat(ndef) = inat
        ndeftp(ndef) = 0
        xdef(ndef) = floats(2)*scalefactor
        ydef(ndef) = floats(3)*scalefactor
        zdef(ndef) = floats(4)*scalefactor
      elseif (nfloat.eq.3) then
        if (nword.ge.ind+1) then
          call ltont(words(ind+1),inat,itype)
          if (nword.ge.ind+2) then
            ind2 = 1
            if (index(words(ind+2),'bs').eq.1.or.index(words(ind+2),'bc').eq.1) then
              inat = inat + 3*maxele
              ind2 = 2
            endif
            if (index(words(ind+2),'sh').eq.ind2) then
              inat = inat + maxele
              ind = ind + 1
            elseif (index(words(ind+2),'co').ne.ind2) then
              inat = inat + 2*maxele
            else
              ind = ind + 1
            endif
          else
            inat = inat + 2*maxele
          endif
          ndefnat(ndef) = inat
          ndeftp(ndef) = itype
        else
          call outerror('Atom specification missing for interstitial',iline)
          call stopnow('defword')
        endif
        xdef(ndef) = floats(1)*scalefactor
        ydef(ndef) = floats(2)*scalefactor
        zdef(ndef) = floats(3)*scalefactor
      else
        call outerror('Insufficient coordinates supplied',iline)
        call stopnow('defword')
      endif
    elseif (index(words(ind),'frac').eq.1) then
!
!  Fractional coordinates input
!
      ndeftyp(ndef) = 21
      if (nfloat.ge.4) then
        inat = nint(floats(1))
        if (inat.gt.100) inat = inat - 100 + maxele
        ndefnat(ndef) = inat
        ndeftp(ndef) = 0
        xdef(ndef) = floats(2)
        ydef(ndef) = floats(3)
        zdef(ndef) = floats(4)
      elseif (nfloat.eq.3) then
        if (nword.ge.ind+1) then
          call ltont(words(ind+1),inat,itype)
          if (nword.ge.ind+2) then
            ind2 = 1
            if (index(words(ind+2),'bs').eq.1.or.index(words(ind+2),'bc').eq.1) then
              inat = inat + 3*maxele
              ind2 = 2
            endif
            if (index(words(ind+2),'she').eq.ind2) then
              inat = inat + maxele
              ind = ind + 1
            elseif (index(words(ind+2),'cor').ne.ind2) then
              inat = inat + 2*maxele
            else
              ind = ind + 1
            endif
          else
            inat = inat + 2*maxele
          endif
          ndefnat(ndef) = inat
          ndeftp(ndef) = itype
        else
          call outerror('Atom specification missing for interstitial',iline)
          call stopnow('defword')
        endif
        xdef(ndef) = floats(1)
        ydef(ndef) = floats(2)
        zdef(ndef) = floats(3)
      else
        call outerror('Insufficient coordinates supplied',iline)
        call stopnow('defword')
      endif
    elseif (index(words(ind),'bond').eq.1) then
!
!  Bond directive
!
      ndeftyp(ndef) = 23
      indb = ind
      if (nword.ge.ind+1) then
        call ltont(words(ind+1),inat,itype)
        if (nword.ge.ind+2) then
          ind2 = 1
          if (index(words(ind+2),'bs').eq.1.or.index(words(ind+2),'bc').eq.1) then
            inat = inat + 3*maxele
            ind2 = 2
          endif
          if (index(words(ind+2),'sh').eq.ind2) then
            inat = inat + maxele
            ind = ind + 1
          elseif (index(words(ind+2),'co').ne.ind2) then
            inat = inat + 2*maxele
          else
            ind = ind + 1
          endif
        else
          inat = inat + 2*maxele
        endif
        ndefnat(ndef) = inat
        ndeftp(ndef) = itype
      else
        call outerror('Atom specification missing for interstitial',iline)
        call stopnow('defword')
      endif
      if (nword.ge.ind+2) then
        call ltont(words(ind+2),inat,itype)
        xdef(ndef) = inat
        ydef(ndef) = itype
      elseif (nfloat.ge.3) then
        if (index(words(indb),'bondc').ne.0) then
          ndeftyp(ndef) = 25
        else
          ndeftyp(ndef) = 24
        endif
        xdef(ndef) = floats(1)
        ydef(ndef) = floats(2)
        zdef(ndef) = floats(3)
      else
        call outerror('Bonded atom specification missing for interstitial',iline)
        call stopnow('defword')
      endif
    else
!
!  Symbol first
!
      call ltont(words(ind),inat,itype)
      ndeftp(ndef) = itype
      if (nword.ge.ind+1) then
        ind2 = 1
        if (index(words(ind+1),'bs').eq.1.or.index(words(ind+1),'bc').eq.1) then
          inat = inat + 3*maxele
          ind2 = 2
        endif
        if (index(words(ind+1),'sh').eq.ind2) then
          inat = inat + maxele
          ind = ind + 1
        elseif (index(words(ind+1),'co').ne.ind2) then
          inat = inat + 2*maxele
        else
          ind = ind + 1
        endif
      else
        inat = inat+2*maxele
      endif
      ndefnat(ndef) = inat
      if (nword.ge.ind+1.and.index(words(ind+1),'cart').eq.1) then
        ndeftyp(ndef) = 22
        if (nfloat.ge.3) then
          xdef(ndef) = floats(1)*scalefactor
          ydef(ndef) = floats(2)*scalefactor
          zdef(ndef) = floats(3)*scalefactor
        else
          call outerror('Insufficient coordinates supplied',iline)
          call stopnow('defword')
        endif
      elseif (nword.ge.ind+1.and.index(words(ind+1),'bond').eq.1) then
        ndeftyp(ndef) = 23
        indb = ind + 1
        if (nword.ge.ind+2) then
          call ltont(words(ind+2),inat,itype)
          xdef(ndef) = inat
          ydef(ndef) = itype
        elseif (nfloat.ge.3) then
          if (index(words(indb),'bondc').ne.0) then
            ndeftyp(ndef) = 25
          else
            ndeftyp(ndef) = 24
          endif
          xdef(ndef) = floats(1)
          ydef(ndef) = floats(2)
          zdef(ndef) = floats(3)
        else
          call outerror('Bonded atom specification missing for interstitial',iline)
          call stopnow('defword')
        endif
      else
        ndeftyp(ndef) = 21
        if (nfloat.ge.3) then
          xdef(ndef) = floats(1)
          ydef(ndef) = floats(2)
          zdef(ndef) = floats(3)
        else
          call outerror('Insufficient coordinates supplied',iline)
          call stopnow('defword')
        endif
      endif
    endif
  else
    if (nfloat.ge.4) then
!
!  All number point input
!
      ndeftyp(ndef) = 21
      inat = nint(floats(1))
      if (inat.gt.100) inat = inat - 100 + maxele
      ndefnat(ndef) = inat
      ndeftp(ndef) = 0
      xdef(ndef) = floats(2)
      ydef(ndef) = floats(3)
      zdef(ndef) = floats(4)
    else
      call outerror('Insufficient coordinates supplied',iline)
      call stopnow('defword')
    endif
  endif
  lwordok = .true.
  return
!*****************
!  Move_2a_to_1  *
!*****************
160 if (nfloat.eq.0) then
    reg2a1(ncurr) = 10000.0_dp
  else
    reg2a1(ncurr) = floats(1)
  endif
  lwordok = .true.
  return
!***********
!  Mode2a  *
!***********
170 if (nfloat.ge.1) then
    mode2a = nint(floats(1))
  else
    iline = iline + 1
    read(nru,*,err=99) mode2a
  endif
  if (mode2a.lt.1.or.mode2a.gt.5) then
    call outerror('Invalid region 2a mode specified',iline)
    call stopnow('defword')
  endif
  lmodeset = .true.
  lwordok = .true.
  return
!************
!  Deflist  *
!************
!
!  Check that explicit region 1 has been supplied else deflist is
!  redundant
!
180 if (ndexplicit.ne.ncurr) then
    nwarn = nwarn + 1
    if (ioproc) then
      call outwarning('deflist supplied without region_1 - list ignored',iline)
    endif
  else
    ldeflin(ncurr) = .true.
    if (nfloat.eq.0) then
      line = '  '
      read(nru,'(a)')line
      iline = iline + 1
      call linepro(nru,line,iline)
    endif
    nvaca = nint(floats(1))
    ninte = nint(floats(2))
    ntot = nvaca + ninte
    read(nru,*,err=99) (ndptr(i),i=1,ntot)
!
!  Save to disk in case of more than one configuration
!
    if (ioproc) write(41) nvaca,ninte
    if (ioproc) write(41) (ndptr(i),i=1,ntot)
  endif
  lwordok = .true.
  return
!***********
!  Reldef  *
!***********
!
!  Check that explicit region 1 has been supplied else nreldef is
!  redundant. Previous execution of "region_1" is also needed to
!  set nreg1 value.
!
190 if (ndexplicit.ne.ncurr) then
    nwarn = nwarn + 1
    if (ioproc) then
      call outwarning('reldef supplied without region_1 - list ignored',iline)
    endif
  else
    lreldin(ncurr) = .true.
    read(nru,*,err=99) (nreldef(i),i=1,nreg1)
!
!  Save to disk in case of more than one configuration
!
    if (ioproc) write(41) (nreldef(i),i=1,nreg1)
  endif
  lwordok = .true.
  return
!
!  Error handling
!
99 call outerror('Error reading input data - check format near '//word,iline)
  call stopnow('defword')
!
  end
