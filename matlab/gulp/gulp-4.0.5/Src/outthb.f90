  subroutine outthb(iout)
!
!  Write out thbrel input file
!
!   5/06 Species specific mass substituted for atmass
!   6/07 lall set to false in calls to setup to avoid
!        potential recursive call issue
!   1/09 Arrays apot, bpot, cpot & dpot replaced by new 2-D array twopot
!   6/09 Module name changed from three to m_three
!
!  Needs updating for non-type specific potential handling
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
!  Julian Gale, NRI, Curtin University, June 2009
!
  use configurations
  use constants
  use control
  use current
  use element
  use files
  use four
  use general
  use ksample
  use m_three
  use shell
  use species
  use splinedata
  use two
  implicit none
!
!  Passed variables
!
  integer(i4)                                  :: iout
!
!  Local variables
!
  character(len=5)                             :: lab1
  character(len=5)                             :: lab2
  character(len=5)                             :: lab3
  character(len=5)                             :: lab4
  character(len=4)                             :: stype(2)
  integer(i4)                                  :: i
  integer(i4)                                  :: im
  integer(i4)                                  :: in
  integer(i4)                                  :: j
  integer(i4)                                  :: k
  integer(i4)                                  :: nc
  integer(i4)                                  :: ni
  integer(i4)                                  :: npco
  integer(i4), dimension(:), allocatable       :: npcosh
  integer(i4)                                  :: npt
  integer(i4)                                  :: npt1
  integer(i4)                                  :: npt2
  integer(i4)                                  :: npt3
  integer(i4)                                  :: npt4
  integer(i4)                                  :: nptr
  integer(i4)                                  :: ns1
  integer(i4)                                  :: ns2
  integer(i4)                                  :: ns3
  integer(i4)                                  :: ns4
  integer(i4)                                  :: nss1
  integer(i4)                                  :: nss2
  integer(i4)                                  :: ntp
  integer(i4)                                  :: nts1
  integer(i4)                                  :: nts2
  integer(i4)                                  :: nts3
  integer(i4)                                  :: nts4
  integer(i4)                                  :: status
  real(dp)                                     :: accm
  real(dp)                                     :: am
  real(dp)                                     :: cutm
!
  data stype/'CORE','SHEL'/
!
!  If thbfile name has been given then open file
!
  if (thbfile(1:1).ne.' ') then
    open(iout,file=thbfile,status='unknown')
  endif
!**********************************************************************
!  Write out control line
!**********************************************************************
!
!  Write out title line
!
  write(iout,'(''TITLE'')')
  if (ntitle.gt.0) then
    do i = 1,ntitle
      write(iout,'(a)')titleword(i)
    enddo
  else
    if (lphon.and..not.lopt) then
      write(iout,'(''THBPHON RESTART FILE WRITTEN FROM GULP'')')
    else
      write(iout,'(''THBREL RESTART FILE WRITTEN FROM GULP'')')
    endif
  endif
  write(iout,'(''ENDS'')')
!
!  Dummy DIME statement
!
  write(iout,'(''DIME 180000'')')
!
!  Madelung accuracy
!
  if (accuracy.ne.6.0) then
    accm = 10.0**accuracy
    write(iout,'(''ACCM '',f16.1)')accm
  endif
  if (index(keyword,'shel').ne.0) then
    write(iout,'(''OPTICAL'')')
  else
    write(iout,'(''THERMAL'')')
  endif
  write(iout,'(''PRIN MINI 0 BASI 0'')')
!
!  Cutoff line
!
  if (cutp.eq.50.0) then
    cutm = 0.0_dp
    do i = 1,npote
      if (rpot(i).gt.cutm) cutm = rpot(i)
    enddo
    if (cutm.eq.0.0_dp) cutm = 10.0_dp
    cutp = cutm
  else
    cutm = cutp
  endif
  write(iout,'(''CUTO 1.0 '',f6.3,1x,f6.4,'' 20.0 20.0 1.0'')') cutm,cuts
!*****************************
!  Loop over configurations  *
!*****************************
  do nc = 1,ncfg
    if (ndimen(nc).eq.3) then
    ncf = nc
    call setup(.false.)
    do i = 1,3
      do j = 1,3
        rv(j,i) = rvcfg(j,i,ncf)
      enddo
    enddo
!
!  Crystal structure info first
!
    write(iout,'(''LATT'')')
    do j = 1,3
      write(iout,'(3f11.6)')(rv(k,j),k=1,3)
    enddo
    write(iout,'(''BASIS'')')
!
!  Convert cell parameters and internal coordinates
!  into cartesian coordinates
!
    do i = 1,numat
      xclat(i) = xfrac(i)*rv(1,1) + yfrac(i)*rv(1,2) + zfrac(i)*rv(1,3)
      yclat(i) = xfrac(i)*rv(2,1) + yfrac(i)*rv(2,2) + zfrac(i)*rv(2,3)
      zclat(i) = xfrac(i)*rv(3,1) + yfrac(i)*rv(3,2) + zfrac(i)*rv(3,3)
    enddo
    do i = 1,numat
      ni = nat(i)
      ntp = nftype(i)
      call label(ni,ntp,lab1)
      if (ni.gt.maxele) then
        npt = 2
      else
        npt = 1
      endif
      write(iout,'(a5,1x,a4,2x,3(f11.6,2x))') lab1,stype(npt),xclat(i),yclat(i),zclat(i)
    enddo
    write(iout,'(''ENDS'')')
    endif
!******************************
!  End of configuration loop  *
!******************************
  enddo
  write(iout,'(''POTE'')')
!**********************
!  Species parameters *
!**********************
  write(iout,'(''SPEC'')')
  do i = 1,nspec
    ni = natspec(i)
    ntp = ntypspec(i)
    call label(ni,ntp,lab1)
    if (ni.gt.maxele) then
      nptr = 2
      am = 0.0_dp
    else
      nptr = 1
      am = massspec(i)
    endif
    write(iout,'(a5,1x,a4,1x,f10.6,f9.4)') lab1,stype(nptr),qlspec(i),am
  enddo
  write(iout,'(''ENDS'')')
!**************************
!  Interatomic potentials *
!**************************
  if (npote.gt.0) then
    npco = 0
    allocate(npcosh(npote),stat=status)
    if (status/=0) call outofmemory('outthb','npcosh')
    do i = 1,npote
      npcosh(i) = 0
      nss1 = nspec1(i)
      nss2 = nspec2(i)
      nts1 = nptyp1(i)
      nts2 = nptyp2(i)
      if (nss1.gt.maxele) then
        npt1 = 2
        ns1 = nss1 - maxele
      else
        npt1 = 1
        ns1 = nss1
      endif
      if (nss2.gt.maxele) then
        npt2 = 2
        ns2 = nss2 - maxele
      else
        npt2 = 1
        ns2 = nss2
      endif
      call label(ns1,nts1,lab1)
      call label(ns2,nts2,lab2)
      if (nptype(i).eq.1) then
!
!  Buckingham
!
        write(iout,'(''BUCK '',2(a5,1x,a4,1x))') lab1,stype(npt1),lab2,stype(npt2)
        write(iout,'(3(1x,f13.6),2(1x,f8.4))') twopot(1,i),twopot(2,i),twopot(3,i),rpot2(i),rpot(i)
        if (rpot(i).lt.cutp) then
          write(iout,'(''0.0 0.3 0.0 '',f12.6)') cutp
        endif
        write(iout,'(''ENDS'')')
      elseif (nptype(i).eq.2) then
!
!  Lennard-Jones
!
        im = nint(tpot(1,i))
        in = nint(tpot(2,i))
        write(iout,'(''LENN '',2(a5,1x,a4,1x),2i3)') lab1,stype(npt1),lab2,stype(npt2),im,in
        write(iout,'(2(1x,f13.6),2(1x,f8.4))') twopot(1,i),twopot(2,i),rpot2(i),rpot(i)
        if (rpot(i).lt.cutp) then
          write(iout,'(''0.0 0.0 '',f12.6)') cutp
        endif
        write(iout,'(''ENDS'')')
      elseif (nptype(i).eq.3.or.nptype(i).eq.4) then
!
!  Morse
!
        write(iout,'(''MORQ '',2(a5,1x,a4,1x))') lab1,stype(npt1),lab2,stype(npt2)
        write(iout,'(5(1x,f12.6))') twopot(1,i),twopot(2,i),twopot(3,i),twopot(4,i),rpot(i)
        if (rpot(i).lt.cutp) then
          write(iout,'(''0.0 0.0 0.0 0.0 '',f12.6)') cutp
        endif
        write(iout,'(''ENDS'')')
      elseif (nptype(i).eq.5.or.nptype(i).eq.6) then
!
!  Harmonic
!  Need to distinguish between core-shell and bond harmonic
!
        if (abs(nss1-nss2).eq.maxele.and.nts1.eq.nts2.and.rpot2(i).eq.0.0d0) then
          npco = npco + 1
          npcosh(npco) = i
        else
          write(iout,'(''SPRI '',2(a5,1x,a4,1x))') lab1,stype(npt1),lab2,stype(npt2)
          write(iout,'(5(1x,f12.6))') twopot(1,i),twopot(2,i),twopot(4,i),rpot2(i),rpot(i)
          if (rpot(i).lt.cutp) then
            write(iout,'(''0.0 0.0 0.0 '',f12.6)') cutp
          endif
          write(iout,'(''ENDS'')')
        endif
      elseif (nptype(i).eq.8) then
!
!  Spring
!
        npco = npco + 1
        npcosh(npco) = i
      elseif (nptype(i).eq.9) then
!
!  Coulomb subtraction
!
        write(iout,'(''SPRI '',2(a5,1x,a4,1x))') lab1,stype(npt1),lab2,stype(npt2)
        write(iout,'(''0.0 0.0 1.0'',2(1x,f12.6))') rpot2(i),rpot(i)
        if (rpot(i).lt.cutp) then
          write(iout,'(''0.0 0.0 0.0 '',f12.6)') cutp
        endif
        write(iout,'(''ENDS'')')
      elseif (nptype(i).eq.10) then
!
!  Four Range Buckingham
!
        write(iout,'(''BUC4 '',2(a5,1x,a4,1x))') lab1,stype(npt1),lab2,stype(npt2)
        write(iout,'(3(1x,f13.6),5(1x,f6.3))')  &
          twopot(1,i),twopot(2,i),twopot(3,i),tpot(1,i),twopot(4,i),tpot(2,i),rpot2(i),rpot(i)
        write(iout,'(''ENDS'')')
      elseif (nptype(i).eq.11) then
!
!  Spline
!
        write(iout,'(''SPLINE '',2(a5,1x,a4,1x),2(1x,f8.4))') &
          lab1,stype(npt1),lab2,stype(npt2),rpot2(i),rpot(i)
        do j = 1,nsplpt(i)
          write(iout,'(f15.8,2x,f10.6)') splf(j,i),splr(j,i)
        enddo
        write(iout,'(''ENDS'')')
      endif
    enddo
!
!  Now handle core-shell harmonics
!
    if (npco.gt.0) then
      do i = 1,npco
        ni = npcosh(i)
        ns1 = nspec1(ni)
        nts1 = nptyp1(ni)
        call label(ns1,nts1,lab1)
        if (twopot(2,ni).ne.0.0_dp) then
          write(iout,'(''HARM '',2(a5,1x,a4,1x),2f12.6)') lab1,stype(1),lab1,stype(2),twopot(1,ni),twopot(2,ni)
        else
          write(iout,'(''HARM '',2(a5,1x,a4,1x),f12.6)') lab1,stype(1),lab1,stype(2),twopot(1,ni)
        endif
      enddo
    endif
  endif
!*************************
!  Three-body potentials *
!*************************
  if (nthb.gt.0) then
    do i = 1,nthb
      if (nthrty(i).eq.1) then
        write(iout,'(''BOHA '',i2,1x,f8.5,1x,f8.4)')i,thbk(i),theta(i)
      elseif (nthrty(i).eq.2) then
        write(iout,'(''MOLD '',i2,1x,f10.5,2(1x,f8.6),1x,f8.4)')i,thbk(i),thrho1(i),thrho2(i),theta(i)
      endif
    enddo
    write(iout,'(''ENDS'')')
    write(iout,'(''THBO'')')
    do i = 1,nthb
      ns1 = ntspec1(i)
      nts1 = ntptyp1(i)
      call label(ns1,nts1,lab1)
      if (ns1.gt.maxele) then
        npt1 = 2
      else
        npt1 = 1
      endif
      ns2 = ntspec2(i)
      nts2 = ntptyp2(i)
      call label(ns2,nts2,lab2)
      if (ns2.gt.maxele) then
        npt2 = 2
      else
        npt2 = 1
      endif
      ns3 = ntspec3(i)
      nts3 = ntptyp3(i)
      call label(ns3,nts3,lab3)
      if (ns3.gt.maxele) then
        npt3 = 2
      else
        npt3 = 1
      endif
      write(iout,'(''ANGA '',i2,1x,3(a5,1x,a4,1x))')i,lab1,stype(npt1),lab2,stype(npt2),lab3,stype(npt3)
      write(iout,'(3(1x,f7.4))')thr1(i),thr2(i),thr3(i)
    enddo
    deallocate(npcosh,stat=status)
    if (status/=0) call deallocate_error('outthb','npcosh')
  endif
!************************
!  Four-body potentials *
!************************
  if (nfor.gt.0) then
    do i = 1,nfor
      if (npfor(i).ge.0) then
        write(iout,'(''TOHA '',i2,1x,f8.5,1x,''-1 '',i2)')i,fork(i),abs(npfor(i))
      else
        write(iout,'(''TOHA '',i2,1x,f8.5,1x,''+1 '',i2)')i,fork(i),abs(npfor(i))
      endif
    enddo
    write(iout,'(''ENDS'')')
    write(iout,'(''TORS'')')
    do i = 1,nfor
      if (nforty(i).eq.1) then
        ns1 = nfspec2(i)
        nts1 = nfptyp2(i)
        call label(ns1,nts1,lab1)
        if (ns1.gt.maxele) then
          npt1 = 2
        else
          npt1 = 1
        endif
        ns2 = nfspec3(i)
        nts2 = nfptyp3(i)
        call label(ns2,nts2,lab2)
        if (ns2.gt.maxele) then
          npt2 = 2
        else
          npt2 = 1
        endif
        ns3 = nfspec1(i)
        nts3 = nfptyp1(i)
        call label(ns3,nts3,lab3)
        if (ns3.gt.maxele) then
          npt3 = 2
        else
          npt3 = 1
        endif
        ns4 = nfspec4(i)
        nts4 = nfptyp4(i)
        call label(ns4,nts4,lab4)
        if (ns4.gt.maxele) then
          npt4 = 2
        else
          npt4 = 1
        endif
        write(iout,'(''TORA '',i2,1x,4(a5,1x,a4,1x))')  &
          i,lab1,stype(npt1),lab2,stype(npt2),lab3,stype(npt3),lab4,stype(npt4)
        write(iout,'(''RIJ MAX '',f7.4)') for2(i)
        write(iout,'(''RIK MAX '',f7.4)') for1(i)
        write(iout,'(''RJL MAX '',f7.4)') for3(i)
        if (for4(i).gt.0.0_dp) then
          write(iout,'(''RKL MAX '',f7.4)') for4(i)
        endif
      endif
    enddo
  endif
  write(iout,'(''ENDS'')')
!******************************
!  General control parameters *
!******************************
  if (lphon.and..not.lopt) then
    write(iout,'(''START PHON'')')
    do i = 1,nkpt
      write(iout,'(''WAVE '',3f8.4)')xkpt(i),ykpt(i),zkpt(i)
    enddo
    write(iout,'(''ENDS'')')
  else
    write(iout,'(''START PLUTO'')')
    if (index(keyword,'conp').ne.0) then
      write(iout,'(''CONP'')')
    elseif (index(keyword,'conv').ne.0) then
      write(iout,'(''CONV'')')
    endif
    if (lopt) write(iout,'(''MAXI 1000'')')
    write(iout,'(''START'')')
    write(iout,'(''STOP'')')
  endif
  close(iout)
!
  return
  end
