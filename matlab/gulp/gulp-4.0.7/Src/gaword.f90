  subroutine gaword(nru,word,lwordok,iline,line,l55,l1000,ncurr)
!
!  Processes input for genetic algorithm words
!
!  nru = fortran channel for reading input
!
!   6/01 Initialisation of line added for benefit of some compilers
!  11/03 ncurr added as an argument
!  11/03 Default boundary values for 0 - 2-D cases added 
!   8/06 nru passed to linepro
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
  use configurations, only : ndimen
  use control
  use costfunction
  use general,        only : nwarn
  use genetic
  use gulpinput
  use iochannels
  use parallel
  implicit none
!
!  Passed variables
!
  character(len=20)                            :: word
  character(len=maxlinelength)                 :: line
  integer(i4)                                  :: iline
  integer(i4)                                  :: ncurr
  integer(i4)                                  :: nru
  integer(i4), dimension(:), allocatable       :: ntemp
  integer(i4)                                  :: status
  logical                                      :: l55
  logical                                      :: l1000
  logical                                      :: lwordok
!
!  Local variables
!
  integer(i4)                                  :: i
  integer(i4)                                  :: n
!
  if (index(word,'predict').eq.1) goto 50
  if (index(word,'genet').eq.1) goto 50
  return
!**********************
!  Genetic Algorithm  *
!**********************
!
!  Set annealing defaults here!
!
50 continue
  lgadef = .true.
  iseed = - 1
  antemp = 100.0_dp
  anftol = 0.000001_dp
  anfac = 0.9_dp
  antlimit = 0.01_dp
!
!  Now process input data
!
100 line = '  '
  read(nru,'(a)',err=99) line
  iline = iline + 1
  if (index(line,'#').eq.1) goto 100
  call linepro(nru,line,iline)
  if (index(words(1),'gexp').eq.1) then
!
!  Exponentially weight the success of better parents?
!
    lgaexpw = .true.
    goto 100
  elseif (index(words(1),'tpxo').eq.1) then
!
!  Exponentially weight the success of better parents?
!
    l2pxo = .true.
    goto 100
  elseif (index(words(1),'maxc').eq.1) then
!
!  Maximum cycles within Prediction Routines
!
    if (nfloat.ge.1) then
      maxgacyc = nint(floats(1))
    else
      read(nru,*,err=99) maxgacyc
      iline = iline + 1
    endif
    goto 100
  elseif (index(words(1),'mini').eq.1) then
!
!  Minimum parameter boundary
!
    if (nfloat.ge.1) then
      n = nint(floats(1))
    else
      read(nru,*,err=99) n
      iline = iline + 1
    endif
    allocate(ntemp(n),stat=status)
    if (status/=0) call outofmemory('gaword','ntemp')
    read(nru,*,err=99) (ntemp(i),i=1,n)
    iline = iline + 1
    read(nru,*,err=99) (xmin(ntemp(i)),i=1,n)
    iline = iline + 1
    deallocate(ntemp,stat=status)
    if (status/=0) call deallocate_error('gaword','ntemp')
    goto 100
  elseif (index(words(1),'maxi').eq.1) then
!
!  Maximum parameter boundary
!
    if (nfloat.ge.1) then
      n = nint(floats(1))
    else
      read(nru,*,err=99) n
      iline = iline + 1
    endif
    allocate(ntemp(n),stat=status)
    if (status/=0) call outofmemory('gaword','ntemp')
    read(nru,*,err=99) (ntemp(i),i=1,n)
    iline = iline + 1
    read(nru,*,err=99) (xmax(ntemp(i)),i=1,n)
    iline = iline + 1
    deallocate(ntemp,stat=status)
    if (status/=0) call deallocate_error('gaword','ntemp')
    goto 100
  elseif (index(words(1),'dmax').eq.1) then
!
!  Default maximum boundary point
!
    if (ndimen(ncurr).eq.2) then
      if (nfloat.ge.1) then
        xmaxcfg(3,ncurr) = floats(1)
      else
        call outerror('insufficient maximum boundary values provided',iline)
        call stopnow('gaword')
      endif
    elseif (ndimen(ncurr).eq.1) then
      if (nfloat.ge.2) then
        xmaxcfg(2,ncurr) = floats(1)
        xmaxcfg(3,ncurr) = floats(2)
      else
        call outerror('insufficient maximum boundary values provided',iline)
        call stopnow('gaword')
      endif
    elseif (ndimen(ncurr).eq.0) then
      if (nfloat.ge.3) then
        xmaxcfg(1,ncurr) = floats(1)
        xmaxcfg(2,ncurr) = floats(2)
        xmaxcfg(3,ncurr) = floats(3)
      else
        call outerror('insufficient maximum boundary values provided',iline)
        call stopnow('gaword')
      endif
    endif
    goto 100
  elseif (index(words(1),'dmin').eq.1) then
!
!  Default minimum boundary point
!
    if (ndimen(ncurr).eq.2) then
      if (nfloat.ge.1) then
        xmincfg(3,ncurr) = floats(1)
      else
        call outerror('insufficient minimum boundary values provided',iline)
        call stopnow('gaword')
      endif
    elseif (ndimen(ncurr).eq.1) then
      if (nfloat.ge.2) then
        xmincfg(2,ncurr) = floats(1)
        xmincfg(3,ncurr) = floats(2)
      else
        call outerror('insufficient minimum boundary values provided',iline)
        call stopnow('gaword')
      endif
    elseif (ndimen(ncurr).eq.0) then
      if (nfloat.ge.3) then
        xmincfg(1,ncurr) = floats(1)
        xmincfg(2,ncurr) = floats(2)
        xmincfg(3,ncurr) = floats(3)
      else
        call outerror('insufficient minimum boundary values provided',iline)
        call stopnow('gaword')
      endif
    endif
    goto 100
  elseif (index(words(1),'disc').eq.1) then
!
!  Discretisation interval
!
    if (nfloat.ge.1) then
      n = nint(floats(1))
    else
      read(nru,*,err=99) n
      iline = iline + 1
    endif
    allocate(ntemp(n),stat=status)
    if (status/=0) call outofmemory('gaword','ntemp')
    read(nru,*,err=99) (ntemp(i),i=1,n)
    iline = iline + 1
    read(nru,*,err=99) (ndiscret(ntemp(i)),i=1,n)
    iline = iline + 1
    do i = 1,n
      if (ndiscret(ntemp(i)).gt.30) then
        nwarn = nwarn + 1
        if (ioproc) then
          write(ioout,'(/)')
          write(ioout,'(''  **** Warning - discretisation number for parameter '',i4,'' exceeds maximum ****'')')
          write(ioout,'(''  **** Reseting value to maximum allowed value = 30                       ****'')')
          write(ioout,'(/)')
        endif
      endif
    enddo
    deallocate(ntemp,stat=status)
    if (status/=0) call deallocate_error('gaword','ntemp')
    goto 100
  elseif (index(words(1),'conf').eq.1) then
!
!  Number of configurations
!
    if (nfloat.eq.1) then
      ngacfg = nint(floats(1))
      mgacfg = ngacfg
      nspar = 0
    elseif (nfloat.eq.2) then
      ngacfg = nint(floats(1))
      mgacfg = nint(floats(2))
      nspar = 2
    elseif (nfloat.gt.2) then
      ngacfg = nint(floats(1))
      mgacfg = nint(floats(2))
      nspar = nint(floats(3))
    else
      read(nru,*,err=99) ngacfg
      mgacfg = ngacfg
      nspar = 0
      iline = iline + 1
    endif
    if (ngacfg.lt.1) then
      call outerror('Less than one configuration in GA is not possible',iline)
      call stopnow('gaword')
    endif
    if (mod(ngacfg,2_i4).eq.1) then
      nwarn = nwarn + 1
      if (ioproc) then
        write(ioout,'(/)')
        write(ioout,'(''  **** Warning - number of configurations must be even for ga ****'')')
        write(ioout,'(''  **** Number of configurations has been changed to '',i6,''    ****'')')
        write(ioout,'(/)')
      endif
      ngacfg  = ngacfg + 1
    endif
    if (mod(nspar,2_i4).eq.1) then
      nwarn = nwarn + 1
      if (ioproc) then
        write(ioout,'(/)')
        write(ioout,'(''  **** Warning - number of successful parents must be even    ****'')')
        write(ioout,'(''  **** no of successful parents has been changed to '',i6,''    ****'')')
        write(ioout,'(/)')
      endif
      nspar = nspar + 1
    endif
    if (ngacfg.gt.mgacfg) then
      call outerror('Initial population must not be greater than max',iline)
      call stopnow('gaword')
    endif
    if (ngacfg.lt.mgacfg.and.nspar.lt.1) then
      call outerror('Initial population cannot expand - 3rd '//'integer < 1',iline)
      call stopnow('gaword')
    endif
    if (2*nspar.gt.ngacfg) then
      call outerror('ngacfg (1st integer) < twice 3rd integer',iline)
      call stopnow('gaword')
    endif
    goto 100
   elseif (index(words(1),'best').eq.1) then
!
!  Number of configurations to optimise
!
    if (nfloat.ge.2) then
      ngabest = nint(floats(1))
      ngabset = nint(floats(2))
    elseif (nfloat.ge.1) then
      ngabest = nint(floats(1))
    else
      read(nru,*,err=99) ngabest
      iline = iline + 1
    endif
    if (ngabest.lt.0.or.ngabset.lt.0) then
      call outerror('Negative no. of structures to optimise',iline)
      call stopnow('gaword')
    endif
    if (index(keyword,'genet').eq.0) ngabset=0
    do i = 1,nword
      if (index(words(i),'only').eq.1) lgabest=.true.
    enddo
    goto 100
  elseif (index(words(1),'uniq').eq.1) then
    if (nfloat.ge.1) then
      udif = floats(1)
    else
      read(nru,*,err=99) udif
      iline = iline + 1
    endif
    udif = dabs(udif)
    goto 100
  elseif (index(words(1),'tour').eq.1) then
!
!  Tournament selection probability
!
    if (nfloat.eq.1) then
      prob(1)=floats(1)
    elseif (nfloat.eq.2) then
      prob(1)=floats(1)
      prob(2)=floats(2)
      prob(3)=(prob(2)-prob(1))/10.0_dp
    elseif (nfloat.eq.3) then
      prob(1)=floats(1)
      prob(2)=floats(2)
      prob(3)=floats(3)
    else
      read(nru,*,err=99)prob(1)
      iline=iline+1
    endif
    if (prob(1).lt.0.0_dp.or.prob(1).gt.1.0_dp) then
      call outerror('Tournament probability is outside range 0-1',iline)
      call stopnow('gaword')
    endif
    if (lgaexpw) then
      if (ioproc) then
        write(ioout,'(''  *** Warning - ignoring pts because keyword gexp specified ***'')')
        write(ioout,'(/,''  **** Error is apparently on line '',i5,'' ****'',/)')iline
      endif
    endif
    goto 100
  elseif (index(words(1),'cros').eq.1) then
!
!  Crossover probability
!
    if (nfloat.eq.1) then
      prob(4) = floats(1)
    elseif (nfloat.eq.2) then
      prob(4) = floats(1)
      prob(5) = floats(2)
      prob(6) = (prob(5)-prob(4))/10.0_dp
    elseif (nfloat.eq.3) then
      prob(4) = floats(1)
      prob(5) = floats(2)
      prob(6) = floats(3)
    else
      read(nru,*,err=99) prob(4)
      iline = iline + 1
    endif
    if (prob(4).lt.0.0_dp.or.prob(4).gt.1.0_dp) then
      call outerror('Crossover probability is outside range 0-1',iline)
      call stopnow('gaword')
    endif
    goto 100
  elseif (index(words(1),'muta').eq.1) then
!
!  Mutation probability
!
    if (nfloat.eq.1) then
      prob(7) = floats(1)
    elseif (nfloat.eq.2) then
      prob(7) = floats(1)
      prob(8) = floats(2)
      prob(9) = (prob(8)-prob(7))/10.0_dp
    elseif (nfloat.eq.3) then
      prob(7) = floats(1)
      prob(8) = floats(2)
      prob(9) = floats(3)
    else
      read(nru,*,err=99) prob(7)
      iline = iline + 1
    endif
    if (prob(7).lt.0.0_dp.or.prob(7).gt.1.0_dp) then
      call outerror('Mutation probability is outside range 0-1',iline)
      call stopnow('gaword')
    endif
    goto 100
  elseif (index(words(1),'seed').eq.1) then
!
!  Seed for random number generator
!
    if (nfloat.ge.1) then
      iseed = nint(floats(1))
    else
      read(nru,*,err=99) iseed
      iline = iline + 1
    endif
    goto 100
  elseif (index(words(1),'grid').eq.1) then
!
!  How the grid size changes with iterations
!
    if (ndi.ne.-1) goto 99
    if (nfloat.eq.1) then
      nd=nint(floats(1))
      ndi=20
    elseif (nfloat.eq.2) then
      nd=nint(floats(1))
      ndmax=nint(floats(2))
      ndi=20
    elseif (nfloat.eq.3) then
      nd=nint(floats(1))
      ndmax=nint(floats(2))
      ndi=nint(floats(2))
    else
      read(nru,*,err=99)nd
      iline=iline+1
      ndmax=nd
    endif
    goto 100
  elseif (index(words(1),'anne').eq.1) then
!
!  How many iterations before changing pmuta
!
    if (nfloat.eq.1) then
      ndi=nint(float(1))
    else
      read(nru,*,err=99)nd
      iline=iline+1
      ndmax=nd
    endif
    if (nd.ne.ndmax) goto 99
    goto 100
  elseif (index(words(1),'antemp').eq.1) then
!
!  Parameters for simulated annealing
!
    if (nfloat.ge.1) then
      antemp=floats(1)
    else
      read(nru,*,err=99)antemp
      iline=iline+1
    endif
    if (antemp.le.0.0_dp) then
      call outerror('Initial temperature must be positive',iline)
      call stopnow('gaword')
    endif
    goto 100
  elseif (index(words(1),'anftol').eq.1) then
    if (nfloat.ge.1) then
      anftol=floats(1)
    else
      read(nru,*,err=99)anftol
      iline=iline+1
    endif
    if (anftol.le.0.0_dp) then
      call outerror('Tolerance of fn change must be positive',iline)
      call stopnow('gaword')
    endif
    goto 100
  elseif (index(words(1),'anfac').eq.1) then
    if (nfloat.ge.1) then
      anfac=floats(1)
    else
      read(nru,*,err=99)anfac
      iline=iline+1
    endif
    if (anfac.le.0.0_dp.or.anfac.ge.1.0_dp) then
      call outerror('Temperature must be able to decrease',iline)
      call stopnow('gaword')
    endif
    goto 100
  elseif (index(words(1),'ttol').eq.1) then
    if (nfloat.ge.1) then
      antlimit=floats(1)
    else
      read(nru,*,err=99)antlimit
      iline=iline+1
    endif
    if (antlimit.le.0.0_dp) then
      call outerror('Penultimate temperature must be positive',iline)
      call stopnow('gaword')
    endif
    goto 100
  elseif (index(words(1),'conj').eq.1) then
!
!  Conjugate Gradient Minimiser
!
    if (nfloat.ge.1) then
      ngacjg=int(floats(1))
    else
      read(nru,*,err=99)ngacjg
      iline=iline+1
    endif
    if (ngacjg.lt.1.or.ngacjg.gt.mgacfg) then
      if (ioproc) then
        write(ioout,'(''  **** Warning - conjugate minimiser will never be used  ****'')')
        write(ioout,'(/,''  **** Error is apparently on line '',i5,'' ****'',/)')iline
      endif
    endif
    goto 100
  elseif (index(words(1),'cost').eq.1) then
!
!  Cost function parameters
!
    if (nfloat.ge.1) then
      if (index(words(2),'kq').eq.1) then
        if (index(words(2),'kqc').eq.1) then
          kqccf = dabs(floats(1))
        elseif (index(words(2),'kqa').eq.1) then
          kqacf = dabs(floats(1))
        else
          kqccf = dabs(floats(1))
          kqacf = dabs(floats(1))
        endif
      endif
      if (index(words(2),'kc').eq.1) then
        if (index(words(2),'kcc').eq.1) then
          kcccf = dabs(floats(1))
        elseif (index(words(2),'kca').eq.1) then
          kcacf = dabs(floats(1))
        else
          kcccf = dabs(floats(1))
          kcacf = dabs(floats(1))
        endif
      endif
      if (index(words(2),'kb').eq.1) kbcf = dabs(floats(1))
      if (index(words(2),'ks').eq.1) kscf = dabs(floats(1))
      if (index(words(2),'ka').eq.1) kacf = dabs(floats(1))
    else
      call outerror('Missing data in cost function input',iline)
      call stopnow('gaword')
    endif
    goto 100
  elseif (index(words(1),'end').eq.1) then
    lwordok = .true.
    return
  else
    l55=.true.
  endif
  lwordok=.true.
  return
!
!  Error handling
!
99 call outerror('Error reading input data - check format near '//word,iline)
  call stopnow('gaword')
  end
