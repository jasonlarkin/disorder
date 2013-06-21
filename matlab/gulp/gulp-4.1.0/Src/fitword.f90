  subroutine fitword(nru,word,lwordok,iline,line,l55,l1000,ncurr)
!
!  Processes input for fitting words
!
!   4/95 Electrostatic potential added as observable
!   8/96 Bulk/shear modulus added for cubic case only
!   3/98 Heat capacity and entropy added as observables
!   4/98 Generalisation of input channel added
!   8/98 Refractive indices added
!   8/98 Martin Braendle's modification of duplication checking added
!  10/98 Codes for fitting simplified
!   6/01 Initialisation of line added for benefit of some compilers
!   7/01 Handling of case of words read in corrected
!   8/01 Piezoelectric constant format changed
!   2/02 Error in memory allocation for weight array / maxobs fixed
!   3/02 Born effective charges added as observable
!   8/02 Bug in weighting of frequencies for fitting corrected
!   2/03 oldunits option added - default for elastic constants now GPa
!   3/03 Default elastic constant weight changed to reflect change in
!        magnitude to GPa
!   6/05 Ambiguity with split/spline fixed
!   6/05 Monopole charge added as an observable
!   9/05 Conversion of Cartesian gradients to fractional corrected
!   9/05 Reading of fitting constraint power added
!   5/06 Specification of stresses for fitting added
!   8/06 nru passed to linepro
!   4/07 Optional weights added for gradients and stresses
!  12/07 Unused variables removed
!   1/08 Bug in handling of shear modulus input fixed
!   1/08 Trap for silly k point numbers added
!   4/08 ReaxFF charges, bond lengths and bond angles added
!   4/08 reaction observable added
!  12/08 Module input renamed to gulpinput
!   1/10 Young's moduli and Poisson ratios added to observables
!   3/10 Default weight option added
!   7/10 Fitting of coordination numbers added
!   9/10 Fitting of S(Q,omega) added
!   6/12 Input for mode observable added
!   7/12 Default k point for frequency observable made 1
!   1/13 Bug in normalisation of input eigenvectors corrected
!   5/13 Volume observable added
!
!  nru = fortran channel for reading input
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
!  Copyright Curtin University 2013
!
!  Julian Gale, NRI, Curtin University, May 2013
!
  use configurations
  use constants
  use control
  use current
  use element,           only : maxele
  use fitting
  use general,           only : nwarn
  use gulpinput
  use iochannels
  use observables
  use parallel
  use potentialpoints
  use scatterdata,       only : sofomega_filename, nq_step_fit, nw_step_fit
  use scatterdata,       only : sofomega_fit, maxnq_step_fit, maxnw_step_fit
  use shifts
  use symmetry
  implicit none
!
!  Passed variables
!
  character(len=maxlinelength)                 :: line
  character(len=20)                            :: word
  integer(i4)                                  :: iline
  integer(i4)                                  :: ncurr
  integer(i4)                                  :: nru
  logical                                      :: l55
  logical                                      :: l1000
  logical                                      :: lwordok
!
!  Local variables
!
  character(len=20)                            :: word2
  integer(i4)                                  :: i
  integer(i4)                                  :: iadd
  integer(i4)                                  :: ii
  integer(i4)                                  :: inc1
  integer(i4)                                  :: inc2
  integer(i4)                                  :: ind
  integer(i4)                                  :: ind1
  integer(i4)                                  :: ind2
  integer(i4)                                  :: iline72
  integer(i4)                                  :: itype
  integer(i4)                                  :: j
  integer(i4)                                  :: k
  integer(i4)                                  :: l
  integer(i4)                                  :: n
  integer(i4)                                  :: nr
  integer(i4)                                  :: nati
  integer(i4)                                  :: nflo
  integer(i4)                                  :: nin
  integer(i4)                                  :: nmax
  integer(i4)                                  :: nread
  integer(i4)                                  :: nreadadd
  integer(i4), dimension(:), allocatable       :: ntemp
  integer(i4)                                  :: ntot
  integer(i4)                                  :: nwo
  integer(i4)                                  :: status
  logical                                      :: lcart
  logical                                      :: lfound
  logical                                      :: lnumbers_only
  logical                                      :: lreverse
  logical                                      :: lvchar
  logical                                      :: lvspli
  real(dp)                                     :: normalise
  real(dp)                                     :: rnormexp
  real(dp)                                     :: units
!
!  Initialise local variables
!
  lvchar = .false.
  lvspli = .false.
  if (index(word,'defa').eq.1) goto 250
  if (index(word,'obse').eq.1) goto 260
  if (index(word,'ener').eq.1) goto 261
  if (index(word,'weig').eq.1) goto 261
  if (index(word,'elas').eq.1) goto 261
  if (index(word,'hfdl').eq.1) goto 261
  if (index(word,'sdlc').eq.1) goto 261
  if (index(word,'hfre').eq.1) goto 261
  if (index(word,'sref').eq.1) goto 261
  if (index(word,'piez').eq.1) goto 261
  if (index(word,'freq').eq.1) goto 261
  if (index(word,'grad').eq.1) goto 261
  if (index(word,'stre').eq.1) goto 261
  if (index(word,'pote').eq.1) goto 261
  if (index(word,'bulk').eq.1) goto 261
  if (index(word,'shea').eq.1) goto 261
  if (index(word,'entr').eq.1) goto 261
  if (index(word,'born').eq.1) goto 261
  if (index(word,'mono').eq.1) goto 261
  if (index(word,'qrea').eq.1) goto 261
  if (index(word,'fbon').eq.1) goto 261
  if (index(word,'fang').eq.1) goto 261
  if (index(word,'reac').eq.1) goto 261
  if (index(word,'cv ') .eq.1) goto 261
  if (index(word,'youn').eq.1) goto 261
  if (index(word,'pois').eq.1) goto 261
  if (index(word,'coor').eq.1) goto 261
  if (index(word,'sqom').eq.1) goto 261
  if (index(word,'mode ').eq.1) goto 261
  if (index(word,'vol').eq.1) goto 261
  if (index(word,'vari').eq.1) goto 290
  if (index(word,'vary').eq.1) goto 290
  if (index(word,'char').eq.1) goto 291
  if (index(word,'cons').eq.1) goto 291
  return
!********************
!  Default weights  *
!********************
250 continue
  if (nword.ge.2.and.nfloat.ge.1) then
    call stolc(words(2),maxword)
    if (index(words(2),'ang').eq.1) then
      delwht_angle = abs(floats(1))
    elseif (index(words(2),'bond').eq.1) then
      delwht_bond = abs(floats(1))
    elseif (index(words(2),'cell_l').eq.1) then
      delwht_cell_length = abs(floats(1))
    elseif (index(words(2),'cell_a').eq.1) then
      delwht_cell_angle = abs(floats(1))
    elseif (index(words(2),'coor').eq.1) then
      delwht_coord = abs(floats(1))
    elseif (index(words(2),'diel').eq.1) then
      delwht_dielectric = abs(floats(1))
    elseif (index(words(2),'elas').eq.1) then
      delwht_elastic = abs(floats(1))
    elseif (index(words(2),'ener').eq.1) then
      delwht_energy = abs(floats(1))
    elseif (index(words(2),'frac').eq.1) then
      delwht_frac = abs(floats(1))
    elseif (index(words(2),'freq').eq.1) then
      delwht_freq = abs(floats(1))
    elseif (index(words(2),'grad').eq.1) then
      delwht_grad = abs(floats(1))
    elseif (index(words(2),'mod').eq.1) then
      delwht_modulus = abs(floats(1))
    elseif (index(words(2),'str').eq.1) then
      delwht_stress = abs(floats(1))
    endif
  else
    call outerror('error reading input for default weights',iline)
    call stopnow('fitword')
  endif
  lwordok = .true.
  return
!**********************************
!  Input observables for fitting  *
!**********************************
!  nobtyp indicates type of property
!  1 => energy (in eV)
!  2 => gradient (assigned through optimisation flags)
!  3 => elastic constant (GPa)
!  4 => high frequency dielectric constant
!  5 => static dielectric constant
!  6 => structure (assign through optimisation flags)
!  7 => piezoelectric strain constant (C/m**2)
!  8 => piezoelectric stress constant (10**-9 C/dyne)
!  9 => vibrational frequency
! 10 => electrostatic potential
! 11 => bulk modulus
! 12 => shear modulus
! 13 => heat capacity (Cv)
! 14 => entropy
! 15 => high frequency refractive index
! 16 => static refractive index
! 17 => S(Q,omega)
! 18 => Born effective charges
! 19 => Monopole charge
! 20 => Stress
! 21 => ReaxFF monopole charge
! 22 => Bond length
! 23 => Bond angle
! 24 => Reaction
! 25 => Young's modulus
! 26 => Poisson's ratio
! 27 => Coordination number
! 29 => Mode (frequency with eigenvector)
! 30 => Volume
!
260 line = '  '
  read(nru,'(a)',err=98) line
  iline = iline + 1
  if (index(line,'#').eq.1) goto 260
  call linepro(nru,line,iline)
261 continue
  do i = 1,nword
    call stolc(words(i),maxword)
  enddo
  word = words(1)(1:20)
  if (index(word,'ener').eq.1) then
!
!  Energy value
!
    nobs = nobs + 1
    if (nobs.gt.maxobs) then
      maxobs = nobs + 10
      call changemaxobs
    endif
    nobtyp(nobs) = 1
    nobcfg(nobs) = ncurr
    units = 1.0_dp
    if (nword.gt.1) then
      if (index(words(2),'au ').eq.1) then
        units = autoev
      elseif (index(words(2),'kc').eq.1) then
        units = kcaltoev
      elseif (index(words(2),'kj').eq.1) then
        units = kjmtoev
      endif
    endif
    if (nfloat.eq.0) then
      line = '  '
      read(nru,'(a)',err=99) line
      iline = iline + 1
      call linepro(nru,line,iline)
    endif
    fobs(nobs) = floats(1)*units
    if (nword.ge.1) then
      call stolc(words(1),maxword)
      if (index(words(1),'au ').eq.1) then
        fobs(nobs) = fobs(nobs)*autoev
      elseif (index(words(1),'kcal').eq.1) then
        fobs(nobs) = fobs(nobs)*kcaltoev
      elseif (index(words(1),'kjmo').eq.1) then
        fobs(nobs) = fobs(nobs)*kjmtoev
      endif
    endif
    if (nfloat.ge.2) then
      weight(nobs) = floats(2)
    else
      weight(nobs) = delwht_energy
    endif
    goto 260
  elseif (index(word,'weig').eq.1) then
!
!  Weighting factor
!
    if (nfloat.ge.1) then
      n = nint(floats(1))
    else
      read(nru,*,err=99) n
      iline = iline + 1
    endif
    if (n.eq.0) then
      read(nru,*,err=99) delwht
      iline = iline + 1
    else
      allocate(ntemp(n),stat=status)
      if (status/=0) call outofmemory('fitword','ntemp')
      read(nru,*,err=99)(ntemp(i),i=1,n)
!
!  Check to see what the largest observable number is
!  and increase maxobs if necessary
!
      nmax = 0
      do i = 1,n
        if (ntemp(i).gt.nmax) nmax = ntemp(i)
      enddo
      if (nmax.gt.maxobs) then
        maxobs = nmax
        call changemaxobs
      endif
      iline = iline + 1
      read(nru,*,err=99)(weight(ntemp(i)),i=1,n)
      iline = iline + 1
      deallocate(ntemp,stat=status)
      if (status/=0) call deallocate_error('fitword','ntemp')
      if (status/=0) call deallocate_error('fitword','ntemp')
    endif
    goto 260
  elseif (index(word,'elas').eq.1) then
!
!  Elastic constant
!
    if (ndimen(ncurr).ne.3) then
      call outerror('Elastic constant observable given for a non 3-D case',iline)
      call stopnow('fitword')
    endif
    n = 1
    if (nfloat.ge.1) then
      n = nint(floats(1))
    endif
    units = 1.0_dp
    if (index(keyword,'oldu').ne.0) then
      units = 10.0_dp
    endif
    do k = 1,n
      nobs = nobs + 1
      if (nobs.gt.maxobs) then
        maxobs = nobs + 10
        call changemaxobs
      endif
      nobtyp(nobs) = 3
      nobcfg(nobs) = ncurr
      line = '  '
      read(nru,'(a)',err=99) line
      iline = iline + 1
      call linepro(nru,line,iline)
      if (nfloat.lt.3) then
        call outerror('Missing data for elastic constant input',iline)
        call stopnow('fitword')
      endif
      i = nint(floats(1))
      j = nint(floats(2))
      fobs(nobs) = floats(3)*units
      if (nfloat.ge.4) then
        weight(nobs) = floats(4)
      else
        weight(nobs) = delwht_elastic
      endif
!
!  Elastic constant pointer = 6*i+j
!
      nobptr(nobs) = 7*i + j
    enddo
    if (lfit) then
      lprop = .true.
      lstr = .true.
    endif
    goto 260
  elseif (index(word,'hfdl').eq.1) then
!
!  High frequency dielectric constant
!
    if (ndimen(ncurr).ne.3) then
      call outerror('Dielectric constant observable given for non 3-D case',iline)
      call stopnow('fitword')
    endif
    n = 1
    if (nfloat.ge.1) then
      n = nint(floats(1))
    endif
    do k = 1,n
      nobs = nobs + 1
      if (nobs.gt.maxobs) then
        maxobs = nobs + 10
        call changemaxobs
      endif
      nobtyp(nobs) = 4
      nobcfg(nobs) = ncurr
      line = '  '
      read(nru,'(a)',err=99) line
      iline = iline + 1
      call linepro(nru,line,iline)
      if (nfloat.lt.3) then
        call outerror('Missing data in dielectric constant input',iline)
        call stopnow('fitword')
      endif
      i = nint(floats(1))
      j = nint(floats(2))
      fobs(nobs) = floats(3)
      if (nfloat.ge.4) then
        weight(nobs) = floats(4)
      else
        weight(nobs) = delwht_dielectric
      endif
!
!  Dielectric constant pointer = 4*i+j
!
      nobptr(nobs) = 4*i + j
    enddo
    if (lfit) then
      lprop = .true.
      lstr = .true.
    endif
    goto 260
  elseif (index(word,'sdlc').eq.1) then
!
!  Static dielectric constant
!
    if (ndimen(ncurr).ne.3) then
      call outerror('Dielectric constant observable given for a non 3-D case',iline)
      call stopnow('fitword')
    endif
    n = 1
    if (nfloat.ge.1) then
      n = nint(floats(1))
    endif
    do k = 1,n
      nobs = nobs + 1
      if (nobs.gt.maxobs) then
        maxobs = nobs + 10
        call changemaxobs
      endif
      nobtyp(nobs) = 5
      nobcfg(nobs) = ncurr
      line = '  '
      read(nru,'(a)',err=99) line
      iline = iline + 1
      call linepro(nru,line,iline)
      if (nfloat.lt.3) then
        call outerror('Missing data in dielectric constant input',iline)
        call stopnow('fitword')
      endif
      i = nint(floats(1))
      j = nint(floats(2))
      fobs(nobs) = floats(3)
      if (nfloat.ge.4) then
        weight(nobs) = floats(4)
      else
        weight(nobs) = delwht_dielectric
      endif
!
!  Dielectric constant pointer = 4*i+j
!
      nobptr(nobs) = 4*i + j
    enddo
    if (lfit) then
      lprop = .true.
      lstr = .true.
    endif
    goto 260
  elseif (index(word,'piez').eq.1) then
!
!  Piezoelectric constant
!
    if (ndimen(ncurr).ne.3) then
      call outerror('Piezoelectric constant observable given for non 3-D case',iline)
      call stopnow('fitword')
    endif
    n = 1
    iadd = 0
    if (nword.ge.2.and.index(words(2),'stre').eq.1) iadd=1
    if (nfloat.ge.1) then
      n = nint(floats(1))
    endif
    do k = 1,n
      nobs = nobs + 1
      if (nobs.gt.maxobs) then
        maxobs = nobs + 10
        call changemaxobs
      endif
      nobtyp(nobs) = 7 + iadd
      nobcfg(nobs) = ncurr
      line = '  '
      read(nru,'(a)',err=99) line
      iline = iline + 1
      call linepro(nru,line,iline)
      if (nfloat.lt.2.or.nword.eq.0) then
        call outerror('Missing data in piezoelectric constant input',iline)
        call stopnow('fitword')
      endif
      i = nint(floats(1))
      call stolc(words(1),maxword)
      if (words(1)(1:1).eq.'x') then
        j = 1
      elseif (words(1)(1:1).eq.'y') then
        j = 2
      elseif (words(1)(1:1).eq.'z') then
        j = 3
      else
        call outerror('Unknown coordinate specifier for piezoelectric const',iline)
        call stopnow('fitword')
      endif
      fobs(nobs) = floats(2)
      if (nfloat.ge.3) then
        weight(nobs) = floats(3)
      else
        weight(nobs) = delwht_dielectric
      endif
!
!  Piezoelectric constant pointer = 6*i+j
!
      if (i.gt.6) then
        call outerror('Invalid piezoelectric index supplied',iline)
        call stopnow('fitword')
      endif
      nobptr(nobs) = 7*i+j
    enddo
    if (lfit) then
      lprop = .true.
      lstr = .true.
    endif
    goto 260
  elseif (index(word,'freq').eq.1) then
!
!  Vibrational frequency
!
    n = 1
    if (nfloat.ge.1) then
      n = nint(floats(1))
    endif
    do k = 1,n
      nobs = nobs + 1
      if (nobs.gt.maxobs) then
        maxobs = nobs + 10
        call changemaxobs
      endif
      nobtyp(nobs) = 9
      nobcfg(nobs) = ncurr
      line = '  '
      read(nru,'(a)',err=99) line
      iline = iline + 1
      call linepro(nru,line,iline)
      if (nfloat.lt.2) then
        call outerror('Missing data in vibrational frequency input',iline)
        call stopnow('fitword')
      endif
      i = nint(floats(1))
      fobs(nobs) = floats(2)
      if (fobs(nobs).lt.0.0_dp.and.lrelax) then
        call outerror('Relax fitting will not work with imaginary frequencies',iline)
        call stopnow('fitword')
      endif
      nobptr2(nobs) = 1
      if (ndimen(ncurr).eq.0) then
        if (nfloat.ge.3) then
          weight(nobs) = floats(3)
        else
          weight(nobs) = delwht_freq
        endif
      else
        if (nfloat.ge.4) then
          nobptr2(nobs) = abs(nint(floats(3)))
          weight(nobs) = floats(4)
        elseif (nfloat.eq.3) then
          if (abs(floats(3)-nint(floats(3))).lt.1.0d-10) then
            nobptr2(nobs) = abs(nint(floats(3)))
            weight(nobs) = delwht_freq
          else
            weight(nobs) = floats(3)
          endif
        else
          nobptr2(nobs) = 1
          weight(nobs) = delwht_freq
        endif
        if (nobptr2(nobs).eq.0) then
          call outerror('K point specified to be equal to zero',iline)
          call stopnow('fitword')
        endif
      endif
!
!  Vibrational frequency pointer
!
!  nobptr points to mode
!  nobptr2 points to K point
!
      nobptr(nobs) = i
    enddo
    if (lfit) then
      lprop = .true.
      lstr = .true.
    endif
    goto 260
  elseif (index(word,'grad').eq.1) then
!
!  Gradients
!
    units = 1.0_dp
    lcart = (ndimen(ncurr).ne.3)
    if (nword.gt.1) then
      if (index(words(2),'ev/an').eq.1.or.index(words(2),'eV/An').eq.1) then
        lcart = .true.
      elseif (index(words(2),'au/an').eq.1.or.index(words(2),'au/An').eq.1) then
        lcart = .true.
        units = autoev
      elseif (index(words(2),'au').eq.1) then
        units = autoev
      endif
    endif
265 line = '  '
    read(nru,'(a)',err=98) line
    iline = iline + 1
    call linepro(nru,line,iline)
    if (nword.gt.0) then
      goto 261
    endif
    nfgrad = nfgrad + 1
    if (nfgrad.gt.maxfgrad) then
      maxfgrad = nfgrad + 10
      call changemaxfgrad
    endif
    if (nfloat.lt.4) then
      call outerror('Insufficient gradients specified',iline)
      call stopnow('fitword')
    endif
    nfgracfg(nfgrad) = ncurr
    nfgrat(nfgrad) = nint(floats(1))
    ind = 3*(nfgrad-1)
    if (lcart.and.ndimen(ncurr).gt.0) then
      if (ndimen(ncurr).eq.3) then
        floats(2) = floats(2)*units
        floats(3) = floats(3)*units
        floats(4) = floats(4)*units
        fgrad(ind+1) = rv(1,1)*floats(2) + rv(2,1)*floats(3) + rv(3,1)*floats(4)
        fgrad(ind+2) = rv(1,2)*floats(2) + rv(2,2)*floats(3) + rv(3,2)*floats(4)
        fgrad(ind+3) = rv(1,3)*floats(2) + rv(2,3)*floats(3) + rv(3,3)*floats(4)
      elseif (ndimen(ncurr).eq.2) then
        floats(2) = floats(2)*units
        floats(3) = floats(3)*units
        fgrad(ind+1) = rv(1,1)*floats(2) + rv(2,1)*floats(3) 
        fgrad(ind+2) = rv(1,2)*floats(2) + rv(2,2)*floats(3) 
        fgrad(ind+3) = floats(4)*units
      elseif (ndimen(ncurr).eq.1) then
        fgrad(ind+1) = floats(2)*units*rv(1,1)
        fgrad(ind+2) = floats(3)*units
        fgrad(ind+3) = floats(4)*units
      endif
    else
      fgrad(ind+1) = floats(2)*units
      fgrad(ind+2) = floats(3)*units
      fgrad(ind+3) = floats(4)*units
    endif
    if (nfloat.ge.5) then
      fgradweight(nfgrad) = abs(floats(5))
    else
      fgradweight(nfgrad) = delwht_grad
    endif
    goto 265
  elseif (index(word,'pote').eq.1) then
!
!  Electrostatic potential
!
    units = 1.0_dp
    lreverse = .false.
    if (nword.gt.1) then
      do i = 2,nword
        if (index(words(i),'au').eq.1) then
          units = autoev
        elseif (index(words(i),'re').eq.1) then
          lreverse = .true.
        endif
      enddo
    endif
270 line = '  '
    read(nru,'(a)',err=98) line
    iline = iline + 1
    call linepro(nru,line,iline)
    if (nword.gt.0) then
      goto 261
    endif
    npotpt = npotpt + 1
    if (npotpt.gt.maxppt) then
      maxppt = npotpt + 10
      call changemaxppt
    endif
    npotptcfg(ncurr) = npotptcfg(ncurr) + 1
    if (nfloat.lt.4) then
      call outerror('Insufficient data specified',iline)
      call stopnow('fitword')
    endif
    nobs = nobs + 1
    if (nobs.gt.maxobs) then
      maxobs = nobs + 10
      call changemaxobs
    endif
    nobtyp(nobs) = 10
    nobcfg(nobs) = ncurr
    nobptr(nobs) = npotptcfg(ncurr)
    if (lreverse) then
      fobs(nobs) = floats(1)
      xpotpt(npotpt) = floats(2)
      ypotpt(npotpt) = floats(3)
      zpotpt(npotpt) = floats(4)
    else
      xpotpt(npotpt) = floats(1)
      ypotpt(npotpt) = floats(2)
      zpotpt(npotpt) = floats(3)
      fobs(nobs) = floats(4)
    endif
    if (nfloat.gt.4) then
      weight(nobs) = floats(5)
    else
      weight(nobs) = delwht
    endif
    goto 270
  elseif (index(word,'bulk').eq.1) then
!
!  Bulk modulus
!
    if (ndimen(ncurr).ne.3) then
      call outerror('Bulk modulus observable given for non 3-D case',iline)
      call stopnow('fitword')
    endif
    nobs = nobs + 1
    if (nobs.gt.maxobs) then
      maxobs = nobs + 10
      call changemaxobs
    endif
    nobtyp(nobs) = 11
    nobcfg(nobs) = ncurr
    units = 1.0_dp
    if (index(keyword,'oldu').ne.0) then
      units = 10.0_dp
    endif
    if (nfloat.eq.0) then
      line = '  '
      read(nru,'(a)',err=99) line
      iline = iline + 1
      call linepro(nru,line,iline)
      if (nfloat.lt.1) then
        call outerror('Missing data in bulk modulus input',iline)
        call stopnow('fitword')
      endif
    endif
    fobs(nobs) = floats(1)*units
    if (nfloat.ge.2) then
      weight(nobs) = floats(2)
    else
      weight(nobs) = delwht_modulus
    endif
    goto 260
  elseif (index(word,'shea').eq.1) then
!
!  Shear modulus
!
    if (ndimen(ncurr).ne.3) then
      call outerror('Shear modulus observable given for non 3-D case',iline)
      call stopnow('fitword')
    endif
    nobs = nobs + 1
    if (nobs.gt.maxobs) then
      maxobs = nobs + 10
      call changemaxobs
    endif
    nobtyp(nobs) = 12
    nobcfg(nobs) = ncurr
    units = 1.0_dp
    if (index(keyword,'oldu').ne.0) then
      units = 10.0_dp
    endif
    if (nfloat.eq.0) then
      line = '  '
      read(nru,'(a)',err=99) line
      iline = iline + 1
      call linepro(nru,line,iline)
      if (nfloat.lt.1) then
        call outerror('Missing data in shear modulus input',iline)
        call stopnow('fitword')
      endif
    endif
    fobs(nobs) = floats(1)*units
    if (nfloat.ge.2) then
      weight(nobs) = floats(2)
    else
      weight(nobs) = delwht_modulus
    endif
    goto 260
  elseif (index(word,'cv').eq.1) then
!
!  Heat Capacity (Cv)
!
    nobs = nobs + 1
    if (nobs.gt.maxobs) then
      maxobs = nobs + 10
      call changemaxobs
    endif
    nobtyp(nobs) = 13
    nobcfg(nobs) = ncurr
    units = 1.0_dp
    if (nword.gt.1) then
      if (index(words(2),'j').eq.1) then
        units = 1.0_dp/(evtoj*avogadro)
      endif
    endif
    if (nfloat.eq.0) then
      line = '  '
      read(nru,'(a)',err=99) line
      iline = iline + 1
      call linepro(nru,line,iline)
      if (nfloat.lt.1) then
        call outerror('Missing data in heat capacity input',iline)
        call stopnow('fitword')
      endif
      if (nword.ge.1) then
        call stolc(words(1),maxword)
        if (index(words(1),'j').eq.1) then
          units = 1.0_dp/(evtoj*avogadro)
        else
          units = 1.0_dp
        endif
      endif
    endif
    fobs(nobs) = floats(1)*units
    if (nfloat.ge.2) then
      weight(nobs) = floats(2)
    else
      weight(nobs) = delwht
    endif
    if (lfit) then
      lprop = .true.
      lstr = .true.
    endif
    goto 260
  elseif (index(word,'entr').eq.1) then
!
!  Entropy
!
    nobs = nobs + 1
    if (nobs.gt.maxobs) then
      maxobs = nobs + 10
      call changemaxobs
    endif
    nobtyp(nobs) = 14
    nobcfg(nobs) = ncurr
    units = 1.0_dp
    if (nword.gt.1) then
      if (index(words(2),'j').eq.1) then
        units = 1.0_dp/(evtoj*avogadro)
      endif
    endif
    if (nfloat.eq.0) then
      line = '  '
      read(nru,'(a)',err=99) line
      iline = iline + 1
      call linepro(nru,line,iline)
      if (nfloat.lt.1) then
        call outerror('Missing data in entropy input',iline)
        call stopnow('fitword')
      endif
      if (nword.ge.1) then
        call stolc(words(1),maxword)
        if (index(words(1),'j').eq.1) then
          units = 1.0_dp/(evtoj*avogadro)
        else
          units = 1.0_dp
        endif
      endif
    endif
    fobs(nobs) = floats(1)*units
    if (nfloat.ge.2) then
      weight(nobs) = floats(2)
    else
      weight(nobs) = delwht
    endif
    if (lfit) then
      lprop = .true.
      lstr = .true.
    endif
    goto 260
  elseif (index(word,'hfre').eq.1) then
!
!  High frequency refractive index
!
    if (ndimen(ncurr).ne.3) then
      call outerror('Refractive index observable given for non 3-D case',iline)
      call stopnow('fitword')
    endif
    n = 1
    if (nfloat.ge.1) then
      n = nint(floats(1))
    endif
    do k = 1,n
      nobs = nobs + 1
      if (nobs.gt.maxobs) then
        maxobs = nobs + 10
        call changemaxobs
      endif
      nobtyp(nobs) = 15
      nobcfg(nobs) = ncurr
      line = '  '
      read(nru,'(a)',err=99) line
      iline = iline + 1
      call linepro(nru,line,iline)
      if (nfloat.lt.2) then
        call outerror('Missing data in refractive index input',iline)
        call stopnow('fitword')
      endif
      nobptr(nobs) = nint(floats(1))
      fobs(nobs) = floats(2)
      if (nfloat.ge.3) then
        weight(nobs) = floats(3)
      else
        weight(nobs) = delwht_dielectric
      endif
    enddo
    if (lfit) then
      lprop = .true.
      lstr = .true.
    endif
    goto 260
  elseif (index(word,'sref').eq.1) then
!
!  Static frequency refractive index
!
    if (ndimen(ncurr).ne.3) then
      call outerror('Refractive index observable given for non 3-D case',iline)
      call stopnow('fitword')
    endif
    n = 1
    if (nfloat.ge.1) then
      n = nint(floats(1))
    endif
    do k = 1,n
      nobs = nobs + 1
      if (nobs.gt.maxobs) then
        maxobs = nobs + 10
        call changemaxobs
      endif
      nobtyp(nobs) = 16
      nobcfg(nobs) = ncurr
      line = '  '
      read(nru,'(a)',err=99) line
      iline = iline + 1
      call linepro(nru,line,iline)
      if (nfloat.lt.2) then
        call outerror('Missing data in refractive index input',iline)
        call stopnow('fitword')
      endif
      nobptr(nobs) = nint(floats(1))
      fobs(nobs) = floats(2)
      if (nfloat.ge.3) then
        weight(nobs) = floats(3)
      else
        weight(nobs) = delwht_dielectric
      endif
    enddo
    if (lfit) then
      lprop = .true.
      lstr = .true.
    endif
    goto 260
  elseif (index(word,'born').eq.1) then
!
!  Born effective charges
!
    n = 1
    if (nfloat.ge.1) then
      n = nint(floats(1))
    endif
    do k = 1,n
      nobs = nobs + 1
      if (nobs.gt.maxobs) then
        maxobs = nobs + 10
        call changemaxobs
      endif
      nobtyp(nobs) = 18
      nobcfg(nobs) = ncurr
      line = '  '
      read(nru,'(a)',err=99) line
      iline = iline + 1
      call linepro(nru,line,iline)
      if (nfloat.lt.2.or.nword.eq.0) then
        call outerror('Missing data in Born effective charge input',iline)
        call stopnow('fitword')
      endif
      i = nint(floats(1))
      call stolc(words(1),maxword)
!
!  Allow for whether pointers have been given as two letters or 1 word of 2 letters
!
      if (nword.ge.2) then
        if (index(words(1),' ').eq.2) then
          call stolc(words(2),maxword)
          words(1)(2:2) = words(2)(1:1)
        endif
      endif
      if (words(1)(1:2).eq.'xx') then
        j = 1
        l = 1
      elseif (words(1)(1:2).eq.'yy') then
        j = 2
        l = 2
      elseif (words(1)(1:2).eq.'zz') then
        j = 3
        l = 3
      elseif (words(1)(1:2).eq.'yz') then
        j = 2
        l = 3
      elseif (words(1)(1:2).eq.'zy') then
        j = 3
        l = 2
      elseif (words(1)(1:2).eq.'xz') then
        j = 1
        l = 3
      elseif (words(1)(1:2).eq.'zx') then
        j = 3
        l = 1
      elseif (words(1)(1:2).eq.'yx') then
        j = 2
        l = 1
      elseif (words(1)(1:2).eq.'xy') then
        j = 1
        l = 2
      else
        call outerror('Unknown coordinate specifier for Born charge',iline)
        call stopnow('fitword')
      endif
      fobs(nobs) = floats(2)
      if (nfloat.ge.3) then
        weight(nobs) = floats(3)
      else
        weight(nobs) = delwht
      endif
!
!  Born charge pointer = 9*(i-1)+3*(j-1)+l
!
!  where i = number of atom
!
      nobptr(nobs) = 9*(i-1) + 3*(j-1) + l
    enddo
    if (lfit) then
      lborn = .true.
    endif
    goto 260
  elseif (index(word,'mono').eq.1) then
!
!  Monopole charges
!
    n = 1
    if (nfloat.ge.1) then
      n = nint(floats(1))
    endif
    do k = 1,n
      nobs = nobs + 1
      if (nobs.gt.maxobs) then
        maxobs = nobs + 10
        call changemaxobs
      endif
      nobtyp(nobs) = 19
      nobcfg(nobs) = ncurr
      line = '  '
      read(nru,'(a)',err=99) line
      iline = iline + 1
      call linepro(nru,line,iline)
      if (nfloat.lt.2) then
        call outerror('Missing data in monopole charge input',iline)
        call stopnow('fitword')
      endif
      nobptr(nobs) = nint(floats(1))
      fobs(nobs) = floats(2)
      if (nfloat.ge.3) then
        weight(nobs) = floats(3)
      else
        weight(nobs) = delwht
      endif
    enddo
    goto 260
  elseif (index(word,'stre').eq.1) then
!
!  Stresses
!
    units = 1.0_dp
267 line = '  '
    read(nru,'(a)',err=98) line
    iline = iline + 1
    call linepro(nru,line,iline)
    if (nword.gt.0) then
      goto 261
    endif
    nfstress = nfstress + 1
    if (nfstress.gt.maxfstress) then
      maxfstress = nfstress + 10
      call changemaxfstress
    endif
    if (nfloat.lt.2) then
      call outerror('Insufficient info specified for stress',iline)
      call stopnow('fitword')
    endif
    nfstrcfg(nfstress) = ncurr
    nfstrt(nfstress) = nint(floats(1))
    fstress(nfstress) = floats(2)
    if (nfloat.ge.3) then
      fstressweight(nfstress) = abs(floats(3))
    else
      fstressweight(nfstress) = delwht_stress
    endif
    goto 267
  elseif (index(word,'qrea').eq.1) then
!
!  ReaxFF monopole charges
!
    n = 1
    if (nfloat.ge.1) then
      n = nint(floats(1))
    endif
    do k = 1,n
      nobs = nobs + 1
      if (nobs.gt.maxobs) then
        maxobs = nobs + 10
        call changemaxobs
      endif
      nobtyp(nobs) = 21
      nobcfg(nobs) = ncurr
      line = '  '
      read(nru,'(a)',err=99) line
      iline = iline + 1
      call linepro(nru,line,iline)
      if (nfloat.lt.2) then
        call outerror('Missing data in ReaxFF monopole charge input',iline)
        call stopnow('fitword')
      endif
      nobptr(nobs) = nint(floats(1))
      fobs(nobs) = floats(2)
      if (nfloat.ge.3) then
        weight(nobs) = floats(3)
      else
        weight(nobs) = delwht
      endif
    enddo
    goto 260
  elseif (index(word,'fbon').eq.1) then
!   
!  Bond length
!
    n = 1
    if (nfloat.ge.1) then
      n = nint(floats(1))
    endif
    units = 1.0_dp
    if (index(keyword,'au').ne.0) then
      units = autoangs
    endif
    do k = 1,n
      nobs = nobs + 1 
      if (nobs.gt.maxobs) then
        maxobs = nobs + 10
        call changemaxobs
      endif
      nobtyp(nobs) = 22
      nobcfg(nobs) = ncurr
      line = '  '
      read(nru,'(a)',err=99) line
      iline = iline + 1
      call linepro(nru,line,iline)
      if (nfloat.lt.3) then
        call outerror('Missing data for bond length input',iline)
        call stopnow('fitword')
      endif
      i = nint(floats(1))
      j = nint(floats(2))
      fobs(nobs) = floats(3)*units
      if (nfloat.ge.4) then
        weight(nobs) = floats(4)
      else
        weight(nobs) = delwht_bond
      endif
!
!  Bond length pointers
!
      nobptr(nobs)  = i
      nobptr2(nobs) = j
    enddo
    if (lfit) then
      lstr = .true.
    endif
    goto 260
  elseif (index(word,'fang').eq.1) then
!     
!  Bond angle
!   
    n = 1
    if (nfloat.ge.1) then
      n = nint(floats(1))
    endif
    units = 1.0_dp
    if (index(keyword,'rad').ne.0) then
      units = radtodeg
    endif
    do l = 1,n
      nobs = nobs + 1 
      if (nobs.gt.maxobs) then
        maxobs = nobs + 10
        call changemaxobs
      endif
      nobtyp(nobs) = 23
      nobcfg(nobs) = ncurr
      line = '  '
      read(nru,'(a)',err=99) line
      iline = iline + 1
      call linepro(nru,line,iline)
      if (nfloat.lt.4) then
        call outerror('Missing data for bond angle input',iline)
        call stopnow('fitword')
      endif
      i = nint(floats(1))
      j = nint(floats(2))
      k = nint(floats(3))
      fobs(nobs) = floats(4)*units
      if (nfloat.ge.5) then
        weight(nobs) = floats(5)
      else
        weight(nobs) = delwht_angle
      endif
!     
!  Bond angle pointers
!     
      nobptr(nobs)  = i
      nobptr2(nobs) = j
      nobptr3(nobs) = k
    enddo
    if (lfit) then
      lstr = .true.
    endif
    goto 260
  elseif (index(word,'reac').eq.1) then
!     
!  Reaction energy
!     
    n = 1
    if (nfloat.ge.1) then
      n = nint(floats(1))
    endif
    units = 1.0_dp 
    if (index(keyword,'au').ne.0) then
      units = autoev
    elseif (index(keyword,'kcal').ne.0) then
      units = kcaltoev
    elseif (index(keyword,'kj').ne.0) then
      units = kjmtoev
    endif
    do l = 1,n
      nobs = nobs + 1 
      if (nobs.gt.maxobs) then
        maxobs = nobs + 10
        call changemaxobs
      endif
      nobtyp(nobs) = 24
      nobcfg(nobs) = 0
      line = '  '
      read(nru,'(a)',err=99) line
      iline = iline + 1
      call linepro(nru,line,iline)
      if (nfloat.lt.1) then
        call outerror('Missing data for reaction observable input',iline)
        call stopnow('fitword')
      endif
!
!  Set number of reaction configurations to be read in and check that there is sufficient data
!
      nr = nint(floats(1))
      if (nfloat.lt.2*nr+2) then
        call outerror('Missing data for reaction observable input',iline)
        call stopnow('fitword')
      endif
      fobs(nobs) = floats(2)*units
      if (nfloat.ge.2*nr+3) then
        weight(nobs) = floats(3)
        nreadadd = 1
      else
        weight(nobs) = delwht_energy
        nreadadd = 0
      endif
      do i = 1,nr
        j = nint(floats(2*i+1+nreadadd))
        if (j.gt.ncfg) then
          call outerror('Reaction input involves non-existent configuration',iline)
          call stopnow('fitword')
        endif
        freaction(j,nobs) = floats(2*i+2+nreadadd)
      enddo
      nobptr(nobs)  = 1
      nobptr2(nobs) = 1
      nobptr3(nobs) = 1
    enddo
    goto 260
  elseif (index(word,'youn').eq.1) then
!
!  Young's modulus
!
    if (ndimen(ncurr).ne.3) then
      call outerror('Youngs modulus observable given for non 3-D case',iline)
      call stopnow('fitword')
    endif
    n = 1
    if (nfloat.ge.1) then
      n = nint(floats(1))
    endif
    do k = 1,n
      nobs = nobs + 1
      if (nobs.gt.maxobs) then
        maxobs = nobs + 10
        call changemaxobs
      endif
      nobtyp(nobs) = 25
      nobcfg(nobs) = ncurr
      line = '  '
      read(nru,'(a)',err=99) line
      iline = iline + 1
      call linepro(nru,line,iline)
      if (nfloat.lt.1.or.nword.eq.0) then
        call outerror('Missing data in Youngs modulus input',iline)
        call stopnow('fitword')
      endif
!
!  Young's modulus pointer :
!
!  1 = x
!  2 = y
!  3 = z
!
      call stolc(words(1),maxword)
      if (words(1)(1:1).eq.'x') then
        j = 1
      elseif (words(1)(1:1).eq.'y') then
        j = 2
      elseif (words(1)(1:1).eq.'z') then
        j = 3
      else
        call outerror('Unknown coordinate specifier for Youngs modulus',iline)
        call stopnow('fitword')
      endif
      nobptr(nobs) = j
!
      fobs(nobs) = floats(1)
      if (nfloat.ge.2) then
        weight(nobs) = floats(2)
      else
        weight(nobs) = delwht_modulus
      endif
    enddo
    if (lfit) then
      lprop = .true.
      lstr = .true.
    endif
    goto 260
  elseif (index(word,'pois').eq.1) then
!
!  Poisson's ratio
!
    if (ndimen(ncurr).ne.3) then
      call outerror('Poisson ratio observable given for non 3-D case',iline)
      call stopnow('fitword')
    endif
    n = 1
    if (nfloat.ge.1) then
      n = nint(floats(1))
    endif
    do k = 1,n
      nobs = nobs + 1
      if (nobs.gt.maxobs) then
        maxobs = nobs + 10
        call changemaxobs
      endif
      nobtyp(nobs) = 26
      nobcfg(nobs) = ncurr
      line = '  '
      read(nru,'(a)',err=99) line
      iline = iline + 1
      call linepro(nru,line,iline)
      if (nfloat.lt.1.or.nword.eq.0) then
        call outerror('Missing data in Poisson ratio input',iline)
        call stopnow('fitword')
      endif
!
!  Poisson ratio pointer :
!
!  1 = xy
!  2 = xz
!  3 = yz
!
      call stolc(words(1),maxword)
      if (words(1)(1:2).eq.'xy') then
        j = 1
      elseif (words(1)(1:2).eq.'yx') then
        j = 1
      elseif (words(1)(1:2).eq.'xz') then
        j = 2
      elseif (words(1)(1:2).eq.'zx') then
        j = 2
      elseif (words(1)(1:2).eq.'yz') then
        j = 3
      elseif (words(1)(1:2).eq.'zy') then
        j = 3
      else
        call outerror('Unknown coordinate specifier for Poisson ratio',iline)
        call stopnow('fitword')
      endif
      nobptr(nobs) = j
!
      fobs(nobs) = floats(1)
      if (nfloat.ge.2) then
        weight(nobs) = floats(2)
      else
        weight(nobs) = delwht
      endif
    enddo
    if (lfit) then
      lprop = .true.
      lstr = .true.
    endif
    goto 260
  elseif (index(word,'coor').eq.1) then
!
!  Coordination number
!
    n = 1
    if (nfloat.ge.1) then
      n = nint(floats(1))
    endif
    do k = 1,n
      nobs = nobs + 1
      if (nobs.gt.maxobs) then
        maxobs = nobs + 10
        call changemaxobs
      endif
      nobtyp(nobs) = 27
      nobcfg(nobs) = ncurr
      line = '  '
      read(nru,'(a)',err=99) line
      iline = iline + 1
      call linepro(nru,line,iline)
      if (nfloat.lt.3) then
        call outerror('Missing data in coordination number input',iline)
        call stopnow('fitword')
      endif
      nobptr(nobs) = nint(floats(1))
      fparameter(nobs) = floats(2)
      fobs(nobs) = abs(floats(3))
      if (nfloat.ge.4) then
        weight(nobs) = floats(4)
      else
        weight(nobs) = delwht
      endif
    enddo
    goto 260
  elseif (index(word,'mode').eq.1) then
!
!  Vibrational frequency
!
    nobs = nobs + 1
    if (nobs.gt.maxobs) then
      maxobs = nobs + 10
      call changemaxobs
    endif
    nobtyp(nobs) = 29
    nobcfg(nobs) = ncurr
    line = '  '
    read(nru,'(a)',err=99) line
    iline = iline + 1
    call linepro(nru,line,iline)
    if (nfloat.lt.1) then
      call outerror('Missing data in vibrational mode input',iline)
      call stopnow('fitword')
    endif
    fobs(nobs) = floats(1)
    if (fobs(nobs).lt.0.0_dp.and.lrelax) then
      call outerror('Relax fitting will not work with imaginary frequencies',iline)
      call stopnow('fitword')
    endif
    nobptr2(nobs) = 1
    if (nfloat.ge.3) then
      nobptr2(nobs) = abs(nint(floats(2)))
      weight(nobs) = floats(3)
    elseif (nfloat.eq.2) then
      if (abs(floats(2)-nint(floats(2))).lt.1.0d-10) then
        nobptr2(nobs) = abs(nint(floats(2)))
        weight(nobs) = delwht_freq
      else
        weight(nobs) = floats(2)
      endif
    else
      weight(nobs) = delwht_freq
    endif
    if (nobptr2(nobs).eq.0) then
      call outerror('K point specified to be equal to zero',iline)
      call stopnow('fitword')
    endif
!
!  Vibrational mode pointer
!
!  nobptr  points to position of eigenvectors in fobsmode
!  nobptr2 points to K point
!
    nobsmode = nobsmode + 1
    nobsmodecfg(ncurr) = nobsmodecfg(ncurr) + 1
    if (nobsmode.gt.maxobsmode) then
      maxobsmode = nobsmode + 10_i4
      call changemaxobsmode
    endif
    nobptr(nobs) = nobsmode
!
    if (lfit) then
      lprop = .true.
      lstr = .true.
    endif
!
!  Read in eigenvectors
!
    lnumbers_only = .true.
    normalise = 0.0_dp
    do while (lnumbers_only)
      line = '  '
      read(nru,'(a)',err=99) line
      iline = iline + 1
      call linepro(nru,line,iline)
      if (nword.gt.0) then
!
!  Word on line means that this is the end of the eigenvector input
!
        normalise = 1.0_dp/sqrt(normalise)
        do i = 1,nobsmodeat(nobsmode)
          fobsmode(1,i,nobsmode) = normalise*fobsmode(1,i,nobsmode)
          fobsmode(2,i,nobsmode) = normalise*fobsmode(2,i,nobsmode)
          fobsmode(3,i,nobsmode) = normalise*fobsmode(3,i,nobsmode)
        enddo
        goto 261
      elseif (nfloat.eq.0) then
!
!  No numbers on line means that this is the end of the eigenvector input
!
!
!  Enforce normalisation of eigenvectors
!
        normalise = 1.0_dp/sqrt(normalise)
        do i = 1,nobsmodeat(nobsmode)
          fobsmode(1,i,nobsmode) = normalise*fobsmode(1,i,nobsmode)
          fobsmode(2,i,nobsmode) = normalise*fobsmode(2,i,nobsmode)
          fobsmode(3,i,nobsmode) = normalise*fobsmode(3,i,nobsmode)
        enddo
        goto 260
      endif
      if (nfloat.lt.3) then
        call outerror('Missing data in vibrational mode input',iline)
        call stopnow('fitword')
      endif
      nobsmodeat(nobsmode) = nobsmodeat(nobsmode) + 1
      if (nobsmodeat(nobsmode).gt.maxat) then
        maxat = nobsmodeat(nobsmode) + 10_i4
        call changemaxat
      endif
      fobsmode(1,nobsmodeat(nobsmode),nobsmode) = floats(1)
      fobsmode(2,nobsmodeat(nobsmode),nobsmode) = floats(2)
      fobsmode(3,nobsmodeat(nobsmode),nobsmode) = floats(3)
!
      normalise = normalise + fobsmode(1,nobsmodeat(nobsmode),nobsmode)**2 + &
                              fobsmode(2,nobsmodeat(nobsmode),nobsmode)**2 + &
                              fobsmode(3,nobsmodeat(nobsmode),nobsmode)**2
    enddo
    goto 260
  elseif (index(word,'sqom').eq.1) then
!
!  S(Q,omega) data 
!
    nobs = nobs + 1
    if (nobs.gt.maxobs) then
      maxobs = nobs + 10
      call changemaxobs
    endif
    nobtyp(nobs) = 17
    nobcfg(nobs) = ncurr
    if (nfloat.ge.1) then
      weight(nobs) = floats(1)
    else
      weight(nobs) = delwht
    endif
!
!  Later on nobptr can be used to point to the right place in the array for sqomega
!
!    nobptr(nobs) = nint(floats(1))
!
    fobs(nobs) = 0.0_dp
    if (nq_step_fit.ne.0) then
      call outerror('at present S(Q,omega) data can only be specified for one structure',iline)
      call stopnow('fitword')
    endif
!
!  Check that filename has been given
!
    if (nword.lt.2) then
      call outerror('file name is missing for S(Q,omega) data',iline)
      call stopnow('fitword')
    endif
!
!  Set filename
!
    sofomega_filename = words(2)
!
!  Try to open file for reading
!
    open(72,file=sofomega_filename,status='old',form='formatted',err=263)
!
!  Read header
!
    line = ' '
    iline72 = 0
    read(72,'(a)',err=99) line
    iline72 = iline72 + 1
    read(72,'(a)',err=99) line
    iline72 = iline72 + 1
    read(72,'(a)',err=99) line
    iline72 = iline72 + 1
!
!  Read lines that control the dimensions of the sofomega_fit array
!
    line = ' '
    read(72,'(a)',err=99) line
    iline72 = iline72 + 1
    call linepro(72_i4,line,iline72)
    if (nfloat.lt.2) then
      call outerror('Missing dimensions in sqw file for fitting',iline)
      call stopnow('fitword')
    endif
    nw_step_fit = nint(abs(floats(1)))
    nq_step_fit = nint(abs(floats(2)))
!
!  Check that array is of finite size
!
    if (nw_step_fit*nq_step_fit.eq.0) then
      call outerror('Dimensions for sqw file for fitting are zero',iline)
      call stopnow('fitword')
    endif
!
!  Allocate sofomega_fit
!
    if (nq_step_fit.gt.maxnq_step_fit) then
      maxnq_step_fit = nq_step_fit
      call changemaxnqstepfit
    endif
    if (nw_step_fit.gt.maxnw_step_fit) then
      maxnw_step_fit = nw_step_fit
      call changemaxnwstepfit
    endif
!
!  Read lines from sqw file
!
    do i = 1,nq_step_fit
      do j = 1,nw_step_fit
        line = '  '
        read(72,'(a)',err=99) line
        call linepro(72_i4,line,iline72)
        iline72 = iline72 + 1
        if (nfloat.lt.3) then
          call outerror('Missing data in S(Q,omega) file',iline)
          call stopnow('fitword')
        endif
        sofomega_fit(j,i) = abs(floats(3))
      enddo
    enddo
!
!  Close sqw file
!
    close(72)
!
!  Normalise S(Q,omega) for fitting
!
    rnormexp  = 0.0_dp
    do i = 1,nq_step_fit
      do j = 1,nw_step_fit
        rnormexp  = rnormexp  + (sofomega_fit(j,i))**2
      enddo
    enddo
    if (rnormexp.gt.1.0d-12) then
      rnormexp = 1.0_dp/sqrt(rnormexp)
    endif
!
    do i = 1,nq_step_fit
      do j = 1,nw_step_fit
        sofomega_fit(j,i) = rnormexp*sofomega_fit(j,i)
      enddo
    enddo
!
    goto 260
!
!  Errors from file reading
!
263 call outerror('S(Q,omega) file cannot be opened',iline)
    call stopnow('fitword')
  elseif (index(word,'vol').eq.1) then
!
!  Volume
!
    if (ndimen(ncurr).ne.3) then
      call outerror('Volume observable given for non 3-D case',iline)
      call stopnow('fitword')
    endif
    nobs = nobs + 1
    if (nobs.gt.maxobs) then
      maxobs = nobs + 10
      call changemaxobs
    endif
    nobtyp(nobs) = 30
    nobcfg(nobs) = ncurr
    if (nfloat.eq.0) then
      line = '  '
      read(nru,'(a)',err=99) line
      iline = iline + 1
      call linepro(nru,line,iline)
      if (nfloat.lt.1) then
        call outerror('Missing data in volume input',iline)
        call stopnow('fitword')
      endif
    endif
    fobs(nobs) = floats(1)
    if (nfloat.ge.2) then
      weight(nobs) = floats(2)
    else
      weight(nobs) = 1.0_dp
    endif
    goto 260
  elseif (index(word,'end').eq.1) then
    lwordok = .true.
    return
  else
    l55 = .true.
    return
  endif
!***************************************
!  Specification of species variables  *
!***************************************
!  For variables: nftyp => 1
!                 nfpot => 3 = shift
!                          4 = charge
!                          5 = polarisability
!                          6 = split of charge between core and shell
!                 nfvar => label of species
!
290 line = '  '
  read(nru,'(a)',err=98) line
  iline = iline + 1
  if (index(line,'#').eq.1) goto 290
  call linepro(nru,line,iline)
  if (nword.eq.0) goto 290
291 continue
  do i = 1,nword
    call stolc(words(i),maxword)
  enddo
  word = words(1)(1:20)
  if (index(word,'char').eq.1) then
!
!  Vary charge distribution
!
    lvchar = .true.
    if (lvspli) then
      if (ioproc) then
        write(ioout,'(''  **** Charge distribution on different ions cannot be ****'')')
        write(ioout,'(''  **** varied simultaneously with core-shell split     ****'')')
      endif
      call stopnow('fitword')
    endif
    if (nfloat.ge.1) then
      nin = nint(floats(1))
    else
      read(nru,*,err=99) nin
      iline = iline + 1
    endif
    if (nin.lt.2) then
      call outerror('Can only fit charges when 2 or more are allowed to vary',iline)
      call stopnow('fitword')
    endif
!
!  Process line by line until require number of data items have been
!  entered - mixed words and numbers may be used
!
    nread = 0
    do while (nread.lt.nin)
      line = '  '
      read(nru,'(a)',err=99) line
      iline = iline + 1
      call linepro(nru,line,iline)
      ntot = nword + nfloat
      i = 1
      nwo = 0
      nflo = 0
      do while (i.le.ntot)
        if (nlorder(i).eq.1) then
          nwo = nwo + 1
          word2 = words(nwo)(1:20)
          call stolc(word2,20_i4)
          if (index(word2,'she').eq.1.or.index(word2,'bsh').eq.1) then
            if (nfvar(nfit).lt.maxele) nfvar(nfit) = nfvar(nfit) + maxele
          elseif (index(word2,'cor').eq.0.and.index(word2,'bco').eq.0) then
            if (nread.lt.nin) then
              nread = nread + 1
              nfit = nfit + 1
              if (nfit.ge.maxfit) then
                maxfit = nfit + 10
                call changemaxfit
              endif
              nftyp(nfit) = 1
              nfpot(nfit) = 4
              call ltont(words(nwo),nati,itype)
              nfvar(nfit) = nati
              nfatyp(nfit) = itype
            endif
          endif
        else
          nflo = nflo + 1
          if (nread.lt.nin) then
            nread = nread + 1
            nfit = nfit + 1
            if (nfit.ge.maxfit) then
              maxfit = nfit + 10
              call changemaxfit
            endif
            nftyp(nfit) = 1
            nfpot(nfit) = 4
            nfvar(nfit) = nint(floats(nflo))
            nfatyp(nfit) = 0
          endif
        endif
        i = i + 1
      enddo
    enddo
!
!  Set points to redundant charge variable, lost due to
!  charge neutrality constraint
!
    lastq = nfvar(nfit)
    lastt = nfatyp(nfit)
    nfit = nfit - 1
    goto 290
  elseif (index(word,'split').eq.1) then
!
!  Vary core-shell charge split
!
    lvspli = .true.
    if (lvchar) then
      if (ioproc) then
        write(ioout,'(''  **** Charge split cannot be varied simultaneously ****'')')
        write(ioout,'(''  **** with charge distribution on different ions   ****'')')
      endif
      call stopnow('fitword')
    endif
    if (nfloat.ge.1) then
      nin = nint(floats(1))
    else
      read(nru,*,err=99) nin
      iline = iline + 1
    endif
!
!  Process line by line until require number of data items have been
!  entered - mixed words and numbers may be used
!
    nread = 0
    do while (nread.lt.nin)
      line = '  '
      read(nru,'(a)',err=99) line
      iline = iline + 1
      call linepro(nru,line,iline)
      ntot = nword + nfloat
      i = 1
      nwo = 0
      nflo = 0
      do while (i.le.ntot.and.nread.lt.nin)
        if (nlorder(i).eq.1) then
          nwo = nwo + 1
          word2 = words(nwo)(1:20)
          call stolc(word2,20_i4)
          if (index(word2,'she').eq.0.and.index(word2,'cor').eq.0.and. &
              index(word2,'bsh').eq.0.and.index(word2,'bco').eq.0) then
            nread = nread + 1
            nfit = nfit + 1
            if (nfit.ge.maxfit) then
              maxfit = nfit + 10
              call changemaxfit
            endif
            nftyp(nfit) = 1
            nfpot(nfit) = 6
            call ltont(words(i),nati,itype)
            nfvar(nfit) = nati
            nfatyp(nfit) = itype
!
!  Check for duplication
!
            j = 1
            lfound = .false.
            do while (j.lt.nfit.and..not.lfound)
              if (nfvar(nfit).eq.nfvar(j).and.nfatyp(nfit).eq.nfatyp(j).and.nfpot(j).eq.6) then
                nwarn = nwarn + 1
                call outwarning('duplication exists in split species',iline)
                nfit = nfit - 1
                lfound = .false.
              endif
              j = j + 1
            enddo
          endif
        else
          nflo = nflo + 1
          nread = nread + 1
          nfit = nfit + 1
          if (nfit.ge.maxfit) then
            maxfit = nfit + 10
            call changemaxfit
          endif
          nftyp(nfit) = 1
          nfpot(nfit) = 6
          nfvar(nfit) = nint(floats(nflo))
          if (nfvar(nfit).gt.maxele) nfvar(nfit) = nfvar(nfit) - maxele
          nfatyp(nfit) = 0
!
!  Check for duplication
!
          j = 1
          lfound = .false.
          do while (j.lt.nfit.and..not.lfound)
            if (nfvar(nfit).eq.nfvar(j).and.nfatyp(nfit).eq.nfatyp(j).and.nfpot(j).eq.6) then
              nwarn = nwarn + 1
              call outwarning('duplication exists in split species',iline)
              nfit = nfit - 1
              lfound = .false.
            endif
            j = j + 1
          enddo
        endif
        i = i + 1
      enddo
    enddo
    goto 290
  elseif (index(word,'pola').eq.1) then
!
!  Vary polarisability - not really supported anymore
!
    if (nfloat.ge.1) then
      nin = nint(floats(1))
    else
      read(nru,*,err=99) nin
      iline = iline + 1
    endif
    if (ioproc) then
      write(ioout,'(''  **** Fitting of polarisabilites is not currently supported ****'')')
    endif
    goto 290
  elseif (index(word,'shif').eq.1) then
!
!  Vary energy shift
!
    do k = 1,nshift
      nfit = nfit + 1
      if (nfit.ge.maxfit) then
        maxfit = nfit + 10
        call changemaxfit
      endif
      nftyp(nfit) = 1
      nfpot(nfit) = 3
    enddo
    goto 290
  elseif (index(word,'cons').eq.1) then
!****************
!  Constraints  *
!****************
    if (nword.gt.1.and.index(words(2),'fit').eq.1) then
!
!  Fitted parameter constraints
!
      if (nfloat.ge.1) then
        nread = nint(floats(1))
      else
        nread = 1
      endif
      do i = 1,nread
        nfcon = nfcon + 1
        if (nfcon.ge.maxfcon) then
          maxfcon = nfcon + 2
          call changemaxfcon
        endif
        line = '  '
        read(nru,'(a)') line
        iline = iline + 1
        call linepro(nru,line,iline)
        if (nword.gt.0) then
          do ii = 1,nword
            call stolc(words(ii),maxword)
          enddo
        endif
        if (nword.gt.0.and.index(words(1),'mean').eq.1) then
!
!  Mean constraint : A3 = sqrt(A1*A2)
!
          nfcotyp(nfcon) = 2
          if (nfloat.ge.4) then
            nfcvar(nfcon) = int(floats(1))
            fconadd(nfcon) = floats(2)
            nfcfix(nfcon) = int(floats(3))
            fconco(nfcon) = floats(4)
          elseif (nfloat.eq.3) then
            nfcvar(nfcon) = int(floats(1))
            fconadd(nfcon) = floats(2)
            nfcfix(nfcon) = int(floats(3))
            fconco(nfcon) = 1.0_dp
          else
            call outerror('Missing data for constraint',iline)
            call stopnow('fitword')
          endif
        else
!
!  Linear or non-linear constraint : A2 = coeff*A1**power + const
!
          nfcotyp(nfcon) = 1
          nfcvar(nfcon) = int(floats(1))
          nfcfix(nfcon) = int(floats(2))
          if (nfloat.ge.5) then
            fconco(nfcon) = floats(3)
            fconadd(nfcon) = floats(4)
            fconpower(nfcon) = floats(5)
          elseif (nfloat.eq.4) then
            fconco(nfcon) = floats(3)
            fconadd(nfcon) = floats(4)
            fconpower(nfcon) = 1.0_dp
          elseif (nfloat.eq.3) then
            fconco(nfcon) = floats(3)
            fconadd(nfcon) = 0.0_dp
            fconpower(nfcon) = 1.0_dp
          else
            call outerror('Missing data for constraint',iline)
            call stopnow('fitword')
          endif
        endif
      enddo
    else
!
!  Structural parameter constraints
!
      if (nfloat.ge.1) then
        nread = nint(floats(1))
      else
        nread = 1
      endif
      do i = 1,nread
        ncontot = ncontot + 1
        if (ncontot.gt.maxcontot) then
          maxcontot = ncontot + 10
          call changemaxcontot
        endif
        line = '  '
        read(nru,'(a)') line
        iline = iline + 1
        call linepro(nru,line,iline)
        if (nword.gt.0) then
          do ii = 1,nword
            call stolc(words(ii),maxword)
          enddo
        endif
        if (nword.gt.0) then
          if (index(words(1),'r').eq.1) then
!
!  Radial constraints
!
            ind1 = 3*nascfg(ncurr) + int(floats(1)) + nstrains
            ind2 = 3*nascfg(ncurr) + int(floats(2)) + nstrains
            ncvarcfg(ncontot) = ind1
            ncfixcfg(ncontot) = ind2
          else
!
!  Cartesian constraint
!
            ind1 = 3*(int(floats(1))-1) + nstrains
            ind2 = 3*(int(floats(2))-1) + nstrains
            if (index(words(1),'x').eq.1) then
              inc1 = 1
            elseif (index(words(1),'y').eq.1) then
              inc1 = 2
            elseif (index(words(1),'z').eq.1) then
              inc1 = 3
            else
              call outerror('Missing data for constraint',iline)
              call stopnow('fitword')
            endif
            if (index(words(2),'x').eq.1) then
              inc2 = 1
            elseif (index(words(2),'y').eq.1) then
              inc2 = 2
            elseif (index(words(2),'z').eq.1) then
              inc2 = 3
            else
              call outerror('Missing data for constraint',iline)
              call stopnow('fitword')
            endif
            ncvarcfg(ncontot) = ind1 + inc1
            ncfixcfg(ncontot) = ind2 + inc2
          endif
        else
          ncvarcfg(ncontot) = int(floats(1))
          ncfixcfg(ncontot) = int(floats(2))
        endif
!
!  Check that strain and internal constraints are not mixed
!
        if ((ncvarcfg(ncontot).le.nstrains.and.ncfixcfg(ncontot).gt.nstrains).or.(ncvarcfg(ncontot).gt. &
            nstrains.and.ncfixcfg(ncontot).le.nstrains)) then
          call outerror('Cannot mix internals and strains in a constraint',iline)
          call stopnow('fitword')
        endif
        if (nfloat.ge.4) then
          concocfg(ncontot) = floats(3)
          conaddcfg(ncontot) = floats(4)
        elseif (nfloat.eq.3) then
          concocfg(ncontot) = floats(3)
          conaddcfg(ncontot) = 0.0_dp
        else
          call outerror('Missing data for constraint',iline)
          call stopnow('fitword')
        endif
        nconcfg(ncontot) = ncurr
      enddo
    endif
    goto 290
  elseif (index(word,'end').eq.1) then
    lwordok = .true.
    return
  else
    l55 = .true.
    return
  endif
!
!  Error handling
!
98 l1000 = .true.
  return
99 call outerror('Error reading input data - check format near '//word,iline)
  call stopnow('fitword')
  end
