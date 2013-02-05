  subroutine dumpbo(iout)
!
!  Dump out bond-body potentials for restart file
!
!   7/03 Created from dump34
!   6/04 M value for attractive and repulsive potentials added
!   7/04 Output of bond order charge potential added
!   2/05 Rho added to boselfenergy
!   7/05 Charge coupled potential output added
!   7/07 ReaxFF parameter output added
!   8/07 ReaxFF parameter output modified for new version of ReaxFF
!   9/07 ReaxFF cutoff added
!  11/07 ReaxFF VDW cutoff added
!  11/07 Species independent ReaxFF parameters added
!  11/07 Pairwise radii added to reaxff2_morse input
!  11/07 Storage in reaxFFtor4 modified to accommodate wildcard end atoms
!  11/07 ReaxFF Coulomb cutoff added
!  12/07 ReaxFF 3-body conjugation term added
!  12/07 lreaxFFbocorrect handling added
!   3/08 Handling of libnodump option corrected
!   3/08 Threshold for ReaxFF bond orders added
!   3/08 Fitting flags for ReaxFF added
!   4/08 Modified for multiple angle reaxFFval3 potentials
!   4/08 Bug in writing borepulsive and boattractive for non-fitting case
!       corrected.
!   6/08 ReaxFF Q shell structure arrays added
!   7/08 pval6 added to reaxFFval3 array
!  12/08 Handling of lreaxFFqfix, reaxFFqfix and reaxFFgamma modified to 
!        be element rather than species focussed. 
!   1/09 lreaxFFpboOK added 
!  10/09 ReaxFF qr12 term added
!  10/09 Modified so that nodump sub-option works for libraries
!  11/09 Further modified so that itmp arrays are not accessed out of bounds
!        and so that the nodump sub-option truly works for ReaxFF.
!   6/10 Modified to handle optional second atom type for bond-order
!        attractive and repulsive terms.
!   9/10 EDIP potentials added
!  10/10 EDIP linear threebody modifications added
!  10/10 Maximum Z value for EDIP cutoff added
!  10/10 EDIP accuracy parameters added
!   8/11 Modified to allow for fixed charged species to be output to restart
!        file even when there are no reaxff terms that are not in the library.
!  11/11 Fixed charged information for reaxFF now points to species rather than element
!   4/12 lreaxFFunder added
!   8/12 Check on nfpot during setting of flags for reaxff2 potentials removed
!   8/12 Dumping of reaxff_gamma corrected since elements were coming out wrong
!   9/12 Dumping of reaxff2_bo corrected for non-over & non-bo13 case
!  12/12 reaxff_q0 option added 
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
!
  use bondorderdata
  use chargecoupled
  use constants
  use control
  use current
  use EDIPdata
  use element
  use fitting
  use library
  use reaxFFdata
  implicit none
!
!  Passed variables
!
  integer(i4),      intent(in)                   :: iout
!
!  Local variables
!
  character(len=4)                               :: ctmp(4)
  character(len=5)                               :: lab1
  character(len=5)                               :: lab2
  character(len=5)                               :: lab3
  character(len=5)                               :: lab4
  integer(i4)                                    :: i
  integer(i4)                                    :: ii
  integer(i4)                                    :: ind
  integer(i4)                                    :: ind1
  integer(i4)                                    :: ind2
  integer(i4), dimension(:),         allocatable :: itmp
  integer(i4), dimension(:,:),       allocatable :: itmp2
  integer(i4), dimension(:,:,:),     allocatable :: itmp3
  integer(i4), dimension(:,:,:,:),   allocatable :: itmp4
  integer(i4), dimension(:,:,:,:,:), allocatable :: itmp5
  integer(i4)                                    :: j
  integer(i4)                                    :: k
  integer(i4)                                    :: l
  integer(i4)                                    :: m
  integer(i4)                                    :: n
  integer(i4)                                    :: nat1
  integer(i4)                                    :: nat2
  integer(i4)                                    :: nat3
  integer(i4)                                    :: nat4
  integer(i4)                                    :: nn
  integer(i4)                                    :: nboAloc
  integer(i4)                                    :: nboQloc
  integer(i4)                                    :: nboQ0loc
  integer(i4)                                    :: nboRloc
  integer(i4)                                    :: nbopotloc
  integer(i4)                                    :: nEDIPout
  integer(i4)                                    :: nEDIPout2
  integer(i4)                                    :: nEDIPspecloc
  integer(i4)                                    :: nEDIPspec2
  integer(i4)                                    :: nreaxFFout
  integer(i4)                                    :: nreaxFFspecloc
  integer(i4)                                    :: nreaxFFfixQspecloc
  integer(i4)                                    :: nreaxFFspec2
  integer(i4)                                    :: nreaxFFspec21
  integer(i4)                                    :: ntype1
  integer(i4)                                    :: ntype2
  integer(i4)                                    :: ntype3
  integer(i4)                                    :: ntype4
  integer(i4)                                    :: status
  logical                                        :: first
  real(dp)                                       :: sum
!
  if (nlib.gt.0) then
    if (.not.llibdump) then
      nbopotloc = nlibnbopot
      nboAloc = nlibnboA
      nboQloc = nlibnboQ
      nboQ0loc = nlibnboQ0
      nboRloc = nlibnboR
      nreaxFFspecloc = nlibreaxFFspec
      nreaxFFfixQspecloc = nlibreaxFFfixQspec
      nEDIPspecloc = nlibEDIPspec
    else
      nbopotloc = nbopot 
      nboAloc = nboA
      nboQloc = nboQ
      nboQ0loc = nboQ0
      nboRloc = nboR
      nreaxFFspecloc = nreaxFFspec
      nreaxFFfixQspecloc = nreaxFFfixQspec
      nEDIPspecloc = nEDIPspec
    endif
  else
    nbopotloc = nbopot
    nboAloc = nboA
    nboQloc = nboQ
    nboQ0loc = nboQ0
    nboRloc = nboR
    nreaxFFspecloc = nreaxFFspec
    nreaxFFfixQspecloc = nreaxFFfixQspec
    nEDIPspecloc = nEDIPspec
  endif
  nreaxFFout = nreaxFFspecloc
  nEDIPout = nEDIPspecloc
!**********************************
!  Bond order two-body potentials *
!**********************************
  if (nbopotloc.gt.0) then
    if (lfit) then
      allocate(itmp(4*nbopot),stat=status)
      if (status/=0) call outofmemory('dumpbo','itmp')
      do i = 1,4*nbopot
        itmp(i) = 0
      enddo
      do i = 1,nfit
        if (nftyp(i).eq.6) then
          if (nfvar(i).le.4) then
            itmp(4*(nfpot(i)-1)+nfvar(i)) = 1
          elseif (nfvar(i).eq.17.or.nfvar(i).eq.18) then
            itmp(4*(nfpot(i)-1)+nfvar(i)-16) = 1
          endif
        endif
      enddo
    endif
    do i = 1,nbopotloc
!
!  Generate species labels
!
      nat1 = nBOspec1(i)
      ntype1 = nBOtyp1(i)
      call label(nat1,ntype1,lab1)
      if (nat1.gt.maxele) then
        ctmp(1) = 'shel'
      else
        ctmp(1) = 'core'
      endif
      nat2 = nBOspec2(i)
      ntype2 = nBOtyp2(i)
      call label(nat2,ntype2,lab2)
      if (nat2.gt.maxele) then
        ctmp(2) = 'shel'
      else
        ctmp(2) = 'core'
      endif
!
      if (BOcombi(i)) then
        write(iout,'(''botwobody combine'')')
        if (lfit) then
          write(iout,'(2(a5,1x,a4,1x),2f12.6,2i2)') lab1,ctmp(1),lab2,ctmp(2),BOchiR(i),BOchiA(i), &
            itmp(4*(i-1)+1),itmp(4*(i-1)+2)
        else
          write(iout,'(2(a5,1x,a4,1x),2f12.6)') lab1,ctmp(1),lab2,ctmp(2),BOchiR(i),BOchiA(i)
        endif
      else
        write(iout,'(''botwobody '')')
        if (lfit) then
          write(iout,'(2(a5,1x,a4,1x),4(g12.6,1x),''&'')') lab1,ctmp(1),lab2,ctmp(2),BOacoeff(i), &
            BObcoeff(i),BOzacoeff(i),BOzbcoeff(i)
          write(iout,'(2x,2(f8.4,1x),4i2)') rBOmin(i),rBOmax(i),(itmp(4*(i-1)+j),j=1,4)
        else
          write(iout,'(2(a5,1x,a4,1x),4(g12.6,1x),''&'')') lab1,ctmp(1),lab2,ctmp(2),BOacoeff(i), &
            BObcoeff(i),BOzacoeff(i),BOzbcoeff(i)
          write(iout,'(2x,2(f8.4,1x))') rBOmin(i),rBOmax(i)
        endif
      endif
    enddo
    if (lfit) then
      deallocate(itmp,stat=status)
      if (status/=0) call deallocate_error('dumpbo','itmp')
    endif
  endif
!***************************
!  Bond order - repulsive  *
!***************************
  if (nboRloc.gt.0) then
    if (lfit) then
      allocate(itmp(6*nboR),stat=status)
      if (status/=0) call outofmemory('dumpbo','itmp')
      do i = 1,6*nboR
        itmp(i) = 0
      enddo
      do i = 1,nfit
        if (nftyp(i).eq.6) then
          if (nfvar(i).ge.5.and.nfvar(i).le.10) then
            itmp(6*(nfpot(i)-1)+nfvar(i)-4) = 1
          endif
        endif
      enddo
    endif
    do i = 1,nboRloc
!
!  Generate species label
!
      nat1 = nBOspecR1(i)
      ntype1 = nBOtypR1(i)
      call label(nat1,ntype1,lab1)
      if (nat1.gt.maxele) then
        ctmp(1) = 'shel'
      else
        ctmp(1) = 'core'
      endif
      nat2 = nBOspecR2(i)
      ntype2 = nBOtypR2(i)
      call label(nat2,ntype2,lab2)
      if (nat2.gt.maxele) then
        ctmp(1) = 'shel'
      else
        ctmp(1) = 'core'
      endif
!
      if (nBOtypeR(i).eq.1) then
        write(iout,'(''borepulsive '')')
        if (lfit) then
          write(iout,'(a5,1x,a4,1x,a5,1x,a4,1x,g12.6,1x,i3,1x,2(g12.6,1x),3i2)')  &
            lab1,ctmp(1),lab2,ctmp(2),BOecoeffR(i),nint(BOmcoeffR(i)),BOncoeffR(i),BOlcoeffR(i),(itmp(6*(i-1)+j),j=1,3)
        else
          write(iout,'(a5,1x,a4,1x,a5,1x,a4,1x,g12.6,1x,i3,1x,2(g12.6,1x))')  &
            lab1,ctmp(1),lab2,ctmp(2),BOecoeffR(i),nint(BOmcoeffR(i)),BOncoeffR(i),BOlcoeffR(i)
        endif
      elseif (nBOtypeR(i).eq.2) then
        write(iout,'(''borepulsive theta'')')
        write(iout,'(a5,1x,a4,1x,a5,1x,a4,1x,g12.6,1x,i3,1x,2(g12.6,1x),''&'')')  &
          lab1,ctmp(1),lab2,ctmp(2),BOecoeffR(i),nint(BOmcoeffR(i)),BOncoeffR(i),BOlcoeffR(i)
        if (lfit) then
          write(iout,'(2x,3(g12.6,1x),6i2)') BOccoeffA(i),BOdcoeffA(i),BOhcoeffA(i),(itmp(6*(i-1)+j),j=1,6)
        else
          write(iout,'(2x,3(g12.6,1x))') BOccoeffA(i),BOdcoeffA(i),BOhcoeffA(i)
        endif
      endif
    enddo
    if (lfit) then
      deallocate(itmp,stat=status)
      if (status/=0) call deallocate_error('dumpbo','itmp')
    endif
  endif
!****************************
!  Bond order - attractive  *
!****************************
  if (nboAloc.gt.0) then
    if (lfit) then
      allocate(itmp(6*nboA),stat=status)
      if (status/=0) call outofmemory('dumpbo','itmp')
      do i = 1,6*nboA
        itmp(i) = 0
      enddo
      do i = 1,nfit
        if (nftyp(i).eq.6) then
          if (nfvar(i).ge.11.and.nfvar(i).le.16) then
            itmp(6*(nfpot(i)-1)+nfvar(i)-10) = 1
          endif
        endif
      enddo
    endif
    do i = 1,nboAloc
!
!  Generate species label
!
      nat1 = nBOspecA1(i)
      ntype1 = nBOtypA1(i)
      call label(nat1,ntype1,lab1)
      if (nat1.gt.maxele) then
        ctmp(1) = 'shel'
      else
        ctmp(1) = 'core'
      endif
      nat2 = nBOspecA2(i)
      ntype2 = nBOtypA2(i)
      call label(nat2,ntype2,lab2)
      if (nat2.gt.maxele) then
        ctmp(1) = 'shel'
      else
        ctmp(1) = 'core'
      endif
!
      if (nBOtypeA(i).eq.1) then
        write(iout,'(''boattractive '')')
        if (lfit) then
          write(iout,'(a5,1x,a4,1x,a5,1x,a4,1x,g12.6,1x,i3,1x,2(g12.6,1x),3i2)')  &
            lab1,ctmp(1),lab2,ctmp(2),BOecoeffA(i),nint(BOmcoeffR(i)),BOncoeffA(i),BOlcoeffA(i),(itmp(6*(i-1)+j),j=1,3)
        else
          write(iout,'(a5,1x,a4,1x,a5,1x,a4,1x,g12.6,1x,i3,1x,2(g12.6,1x))')  &
            lab1,ctmp(1),lab2,ctmp(2),BOecoeffA(i),nint(BOmcoeffR(i)),BOncoeffA(i),BOlcoeffA(i)
        endif
      elseif (nBOtypeA(i).eq.2) then
        write(iout,'(''boattractive theta'')')
        write(iout,'(a5,1x,a4,1x,a5,1x,a4,1x,g12.6,1x,i3,1x,2(g12.6,1x),''&'')')  &
          lab1,ctmp(1),lab2,ctmp(2),BOecoeffA(i),nint(BOmcoeffR(i)),BOncoeffA(i),BOlcoeffA(i)
        if (lfit) then
          write(iout,'(2x,3(g12.6,1x),6i2)') BOccoeffA(i),BOdcoeffA(i),BOhcoeffA(i),(itmp(6*(i-1)+j),j=1,6)
        else
          write(iout,'(2x,3(g12.6,1x))') BOccoeffA(i),BOdcoeffA(i),BOhcoeffA(i)
        endif
      endif
    enddo
    if (lfit) then
      deallocate(itmp,stat=status)
      if (status/=0) call deallocate_error('dumpbo','itmp')
    endif
  endif
!*********************************
!  Bond order charge potentials  *
!*********************************
  if (nboQloc.gt.0) then
    do i = 1,nboQloc
!
!  Generate species labels
!
      nat1 = nBOspecQ1(i)
      ntype1 = nBOtypQ1(i)
      call label(nat1,ntype1,lab1)
      if (nat1.gt.maxele) then
        ctmp(1) = 'shel'
      else
        ctmp(1) = 'core'
      endif
      nat2 = nBOspecQ2(i)
      ntype2 = nBOtypQ2(i)
      call label(nat2,ntype2,lab2)
      if (nat2.gt.maxele) then
        ctmp(2) = 'shel'
      else
        ctmp(2) = 'core'
      endif
!
      if (nBOtaperQ(i).eq.2) then
        write(iout,'(''bocharge staper'')')
      else
        write(iout,'(''bocharge '')')
      endif
      write(iout,'(2(a5,1x,a4,1x),3(f12.6,1x))') lab1,ctmp(1),lab2,ctmp(2),BOq0(i),rBOminQ(i),rBOmaxQ(i)
    enddo
  endif
!*************************************************
!  Bond order self-energy for charge potentials  *
!*************************************************
  if (nboQ0loc.gt.0) then
    do i = 1,nboQ0loc
!
!  Generate species labels
!
      nat1 = nBOspecQ0(i)
      ntype1 = nBOtypQ0(i)
      call label(nat1,ntype1,lab1)
      if (nat1.gt.maxele) then
        ctmp(1) = 'shel'
      else
        ctmp(1) = 'core'
      endif
!
      write(iout,'(''boselfenergy '')')
      write(iout,'(a5,1x,a4,1x,3(f12.6,1x))') lab1,ctmp(1),BOq0pot(i),BOq0rho(i),BOq0ref(i)
    enddo
  endif
!*************************************
!  Charge-coupled potential species  *
!*************************************
  if (nCCspec.gt.0) then
    do i = 1,nCCspec
!
!  Generate species labels
!
      nat1 = nBOspecQ0(i)
      ntype1 = nBOtypQ0(i)
      call label(nat1,ntype1,lab1)
      if (nat1.gt.maxele) then
        ctmp(1) = 'shel'
      else
        ctmp(1) = 'core'
      endif
      write(iout,'(''cc_potential'',1x,i4)') nCCparNb(i)
      write(iout,'(a5,1x,a4,2(1x,f12.6),1x,f8.4,2(1x,f8.5),1x,f10.8)')  &
        lab1,ctmp(1),CCparA(i),CCparB(i),CCvdwC(i),CClambda(i),CCmu(i),CCbeta(i)
      write(iout,'(10x,6(1x,f10.5))')  &
        CCparN(i),CCparM(i),CCparC(i),CCparD(i),CCparH(i),CCeta(i)
      write(iout,'(10x,6(1x,f10.5))')  &
        CCparIE(i),CCparAE(i),CCparQL(i),CCparQU(i),CCparDL(i),CCparDU(i)
      write(iout,'(10x,4(1x,f10.5))')  &
        rCCminS(i),rCCmaxS(i),rCCminL(i),rCCmaxL(i)
    enddo
  endif
  if (nreaxFFspecloc.gt.0) then
!******************************
!  ReaxFF general parameters  *
!******************************
    if (reaxFFcutoff.ne.10.0_dp) then
      write(iout,'(''reaxFFcutoff '',2(f12.6,1x))') reaxFFcutoff,reaxFFtapermin
    endif
    if (reaxFFcutoffVDW.ne.10.0_dp) then
      write(iout,'(''reaxFFvdwcutoff '',f12.6)') reaxFFcutoffVDW
    endif
    if (reaxFFcutoffQ.ne.10.0_dp) then
      write(iout,'(''reaxFFqcutoff '',f12.6)') reaxFFcutoffQ
    endif
    if (reaxFFtol.ne.1.0d-3.or.reaxFFatol.ne.1.0d-3.or.reaxFFatol2.ne.1.0d-6.or. &
        reaxFFhtol.ne.0.01_dp.or.reaxFFrhtol.ne.7.5_dp.or.reaxFFatol3.ne.1.0d-9) then
      write(iout,'(''reaxFFtol '',4(g11.5,1x),f7.3,1x,g12.6)') &
        reaxFFtol,reaxFFatol,reaxFFatol2,reaxFFhtol,reaxFFrhtol,reaxFFatol3
    endif
!************************************
!  ReaxFF species-independent data  *
!************************************
    if (nreaxFFspec.gt.0) then
      if (lfit) then
        allocate(itmp2(5,7),stat=status)
        if (status/=0) call outofmemory('dumpbo','itmp2')
        itmp2(1:5,1:7) = 0
        do i = 1,nfit
          if (nftyp(i).eq.8) then
            if (nfvar(i).ge.9.and.nfvar(i).le.15) then
              itmp2(nfvar2(i),nfvar(i)-8) = 1
            endif
          endif
        enddo
      endif
!
      if (reaxFFlam(1).ne.50.0_dp.or.reaxFFlam(2).ne.4.3822_dp) then
!
!  Bond
!
        if (lfit) then
          write(iout,'(''reaxff0_bond '',2(1x,f12.6),2i2)') reaxFFlam(1),reaxFFlam(2),(itmp2(j,1),j=1,2)
        else
          write(iout,'(''reaxff0_bond '',2(1x,f12.6))') reaxFFlam(1),reaxFFlam(2)
        endif
      endif
      if (reaxFFlam(6).ne.5.6937_dp.or.reaxFFlam(31).ne.1.6356_dp.or.reaxFFlam(7).ne.1.0053_dp &
          .or.reaxFFlam(8).ne.7.6280_dp.or.reaxFFlam(9).ne.14.5067) then
!
!  Over/undercoordination
!
        write(iout,'(''reaxff0_over '',3(1x,f12.6),'' &'')') reaxFFlam(6),reaxFFlam(31),reaxFFlam(7)
        if (lfit) then
          write(iout,'(13x,2(1x,f12.6),5i2)') reaxFFlam(8),reaxFFlam(9),(itmp2(j,2),j=1,5)
        else
          write(iout,'(13x,2(1x,f12.6))') reaxFFlam(8),reaxFFlam(9)
        endif
      endif
      if (reaxFFlam(15).ne.33.8667_dp.or.reaxFFlam(16).ne.2.5067_dp.or.reaxFFlam(17).ne.1.1177_dp &
          .or.reaxFFlam(18).ne.1.9645_dp) then
!
!  Valence
!
        if (lfit) then
          write(iout,'(''reaxff0_valence '',4(1x,f12.6),4i2)') &
            reaxFFlam(15),reaxFFlam(16),reaxFFlam(17),reaxFFlam(18),(itmp2(j,3),j=1,4)
        else
          write(iout,'(''reaxff0_valence '',4(1x,f12.6))') reaxFFlam(15),reaxFFlam(16),reaxFFlam(17),reaxFFlam(18)
        endif
      endif
      if (reaxFFlam(20).ne.6.6623_dp.or.reaxFFlam(21).ne.0.1809_dp.or.reaxFFlam(22).ne.3.9954_dp) then
!
!  Penalty
!
        if (lfit) then
          write(iout,'(''reaxff0_penalty '',3(1x,f12.6),3i2)') &
            reaxFFlam(20),reaxFFlam(21),reaxFFlam(22),(itmp2(j,4),j=1,3)
        else
          write(iout,'(''reaxff0_penalty '',3(1x,f12.6))') reaxFFlam(20),reaxFFlam(21),reaxFFlam(22)
        endif
      endif
      if (reaxFFlam(24).ne.4.8815_dp.or.reaxFFlam(25).ne.10.0_dp.or.reaxFFlam(26).ne.2.3276_dp &
          .or.reaxFFlam(27).ne.1.7905_dp) then
!
!  Torsion
!
        if (lfit) then
          write(iout,'(''reaxff0_torsion '',4(1x,f12.6),4i2)') &
            reaxFFlam(24),reaxFFlam(25),reaxFFlam(26),reaxFFlam(27),(itmp2(j,5),j=1,4)
        else
          write(iout,'(''reaxff0_torsion '',4(1x,f12.6))') reaxFFlam(24),reaxFFlam(25),reaxFFlam(26),reaxFFlam(27)
        endif
      endif
      if (reaxFFlam(28).ne.1.5591_dp) then
!
!  VDW
!
        if (lfit) then
          write(iout,'(''reaxff0_vdw '',1x,f12.6,i2)') reaxFFlam(28),itmp2(1,6)
        else
          write(iout,'(''reaxff0_vdw '',1x,f12.6)') reaxFFlam(28)
        endif
      endif
      if (reaxFFlam(29).ne.25.6125_dp) then
!
!  Lone pair
!
        if (lfit) then
          write(iout,'(''reaxff0_lonepair '',1x,f12.6,i2)') reaxFFlam(29),itmp2(1,7)
        else
          write(iout,'(''reaxff0_lonepair '',1x,f12.6)') reaxFFlam(29)
        endif
      endif
      if (lfit) then
        deallocate(itmp2,stat=status)
        if (status/=0) call deallocate_error('dumpbo','itmp2')
      endif
    endif
!************************
!  ReaxFF species data  *
!************************
    if (nreaxFFspecloc.gt.0.or.nreaxFFfixQspecloc.gt.0) then
!---------------------------
!  Start of reaxff1 terms  |
!---------------------------
!
!  Set fitting flags
!
      if (lfit) then
        allocate(itmp3(4,8,nreaxFFout),stat=status)
        if (status/=0) call outofmemory('dumpbo','itmp3')
        itmp3(1:4,1:8,1:nreaxFFout) = 0
        do i = 1,nfit
          if (nftyp(i).eq.8) then
            if (nfvar(i).le.7) then
              itmp3(nfvar2(i),nfvar(i),nfpot(i)) = 1
            endif
          endif
        enddo
      endif
!   
!  Radii
!       
      write(iout,'(''reaxff1_radii '')')
      do i = 1,nreaxFFspecloc
        nat1 = natreaxFFspec(i)
        ntype1 = ntypreaxFFspec(i)
        call label(nat1,ntype1,lab1)
        if (nat1.gt.maxele) then
          ctmp(1) = 'shel'
        else
          ctmp(1) = 'core'
        endif
        if (lfit) then
          write(iout,'(a5,1x,a4,1x,3(f12.6,1x),3(i1,1x))')  &
            lab1,ctmp(1),reaxFFr(1,i),reaxFFr(2,i),reaxFFr(3,i),(itmp3(j,1,i),j=1,3)
        else
          write(iout,'(a5,1x,a4,1x,3(f12.6,1x))') lab1,ctmp(1),reaxFFr(1,i),reaxFFr(2,i),reaxFFr(3,i)
        endif
      enddo
!   
!  Valence
!       
      write(iout,'(''reaxff1_valence '')')
      do i = 1,nreaxFFspecloc
        nat1 = natreaxFFspec(i)
        ntype1 = ntypreaxFFspec(i)
        call label(nat1,ntype1,lab1)
        if (nat1.gt.maxele) then
          ctmp(1) = 'shel'
        else
          ctmp(1) = 'core'
        endif
        if (lfit) then
          write(iout,'(a5,1x,a4,1x,4(f12.6,1x),4(i1,1x))') &
            lab1,ctmp(1),(reaxFFval(j,i),j=1,4),(itmp3(j,2,i),j=1,4)
        else
          write(iout,'(a5,1x,a4,1x,4(f12.6,1x))') lab1,ctmp(1),(reaxFFval(j,i),j=1,4)
        endif
      enddo
!   
!  Overcoordination
!       
      write(iout,'(''reaxff1_over'')')
      do i = 1,nreaxFFspecloc
        nat1 = natreaxFFspec(i)
        ntype1 = ntypreaxFFspec(i)
        call label(nat1,ntype1,lab1)
        if (nat1.gt.maxele) then
          ctmp(1) = 'shel'
        else
          ctmp(1) = 'core'
        endif
        if (lfit) then
          write(iout,'(a5,1x,a4,1x,4(f12.6,1x),4(i1,1x))') &
            lab1,ctmp(1),(reaxFFpboc(j,i),j=1,3),reaxFFoc1(i),(itmp3(j,3,i),j=1,4)
        else
          write(iout,'(a5,1x,a4,1x,4(f12.6,1x))') lab1,ctmp(1),(reaxFFpboc(j,i),j=1,3),reaxFFoc1(i)
        endif
      enddo
!   
!  Undercoordination
!       
      write(iout,'(''reaxff1_under'')')
      do i = 1,nreaxFFspecloc
        nat1 = natreaxFFspec(i)
        ntype1 = ntypreaxFFspec(i)
        call label(nat1,ntype1,lab1)
        if (nat1.gt.maxele) then
          ctmp(1) = 'shel'
        else
          ctmp(1) = 'core'
        endif
        if (lfit) then
          write(iout,'(a5,1x,a4,1x,f12.6,1x,i1)') &
            lab1,ctmp(1),reaxFFuc1(i),itmp3(1,4,i)
        else
          write(iout,'(a5,1x,a4,1x,f12.6)') lab1,ctmp(1),reaxFFuc1(i)
        endif
      enddo
!   
!  Lone pairs
!       
      write(iout,'(''reaxff1_lonepair'')')
      do i = 1,nreaxFFspecloc
        nat1 = natreaxFFspec(i)
        ntype1 = ntypreaxFFspec(i)
        call label(nat1,ntype1,lab1)
        if (nat1.gt.maxele) then
          ctmp(1) = 'shel'
        else
          ctmp(1) = 'core'
        endif
        if (lfit) then
          write(iout,'(a5,1x,a4,1x,2(f12.6,1x),2(i1,1x))') &
            lab1,ctmp(1),(reaxFFlp(j,i),j=1,2),(itmp3(j,5,i),j=1,2)
        else
          write(iout,'(a5,1x,a4,1x,2(f12.6,1x))') lab1,ctmp(1),(reaxFFlp(j,i),j=1,2)
        endif
      enddo
!     
!  Valence angle per species parameters
!
      write(iout,'(''reaxff1_angle'')')
      do i = 1,nreaxFFspecloc
        nat1 = natreaxFFspec(i)
        ntype1 = ntypreaxFFspec(i)
        call label(nat1,ntype1,lab1)
        if (nat1.gt.maxele) then
          ctmp(1) = 'shel'
        else
          ctmp(1) = 'core'
        endif
        if (lfit) then
          write(iout,'(a5,1x,a4,1x,2(f12.6,1x),2(i1,1x))') &
            lab1,ctmp(1),(reaxFFval1(j,i),j=1,2),(itmp3(j,6,i),j=1,2)
        else
          write(iout,'(a5,1x,a4,1x,2(f12.6,1x))') lab1,ctmp(1),(reaxFFval1(j,i),j=1,2)
        endif
      enddo
!    
!  Species-wise screened VDW - Morse parameters
!
      write(iout,'(''reaxff1_morse'')')
      do i = 1,nreaxFFspecloc
        nat1 = natreaxFFspec(i)
        ntype1 = ntypreaxFFspec(i)
        call label(nat1,ntype1,lab1)
        if (nat1.gt.maxele) then
          ctmp(1) = 'shel'
        else 
          ctmp(1) = 'core'
        endif
        if (lfit) then
          write(iout,'(a5,1x,a4,1x,f12.6,1x,f12.8,1x,2(f12.6,1x),4(i1,1x))') &
            lab1,ctmp(1),reaxFFalpha(i),reaxFFeps(i),reaxFFrvdw(i),reaxFFgammaw(i),(itmp3(j,7,i),j=1,4)
        else
          write(iout,'(a5,1x,a4,1x,f12.6,1x,f12.8,1x,2(f12.6,1x))') &
            lab1,ctmp(1),reaxFFalpha(i),reaxFFeps(i),reaxFFrvdw(i),reaxFFgammaw(i)
        endif
      enddo
      if (lfit) then
        deallocate(itmp3,stat=status)
        if (status/=0) call deallocate_error('dumpbo','itmp3')
      endif
!
!  Element based parameters
!
!  Set fitting flags
!
      if (lfit) then
        allocate(itmp2(10,maxele),stat=status)
        if (status/=0) call outofmemory('dumpbo','itmp2')
        itmp2(1:8,1:maxele) = 0
        do i = 1,nfit
          if (nftyp(i).eq.8) then
            if (nfvar(i).eq.8) then
              itmp2(nfvar2(i),nfpot(i)) = 1
            endif
          endif
        enddo
      endif
!   
!  ReaxFF EEM chi parameters for species present
!           
      write(iout,'(''reaxff_chi'')')
      do i = 1,nreaxFFspecloc
        nat1 = natreaxFFspec(i)
        call label(nat1,0_i4,lab1)
        if (nat1.gt.maxele) then
          ctmp(1) = 'shel'
        else 
          ctmp(1) = 'core'
        endif
        if (lfit) then
          write(iout,'(a5,1x,a4,1x,f12.6,1x,i1)') lab1,ctmp(1),reaxFFchi(nat1),itmp2(1,nat1)
        else
          write(iout,'(a5,1x,a4,1x,f12.6)') lab1,ctmp(1),reaxFFchi(nat1)
        endif
      enddo
!
!  ReaxFF EEM mu parameters for species present
!
      write(iout,'(''reaxff_mu'')')
      do i = 1,nreaxFFspecloc
        nat1 = natreaxFFspec(i)
        call label(nat1,0_i4,lab1)
        if (nat1.gt.maxele) then
          ctmp(1) = 'shel'
        else
          ctmp(1) = 'core'
        endif
        if (lfit) then
          write(iout,'(a5,1x,a4,1x,f12.6,1x,i1)') lab1,ctmp(1),reaxFFmu(nat1),itmp2(2,nat1)
        else
          write(iout,'(a5,1x,a4,1x,f12.6)') lab1,ctmp(1),reaxFFmu(nat1)
        endif
      enddo
!
!  ReaxFF EEM q0 parameters for species present
!
      first = .true.
      do i = 1,nreaxFFspecloc
        nat1 = natreaxFFspec(i)
        if (abs(reaxFFq0(nat1)).gt.1.0d-12) then
          if (first) then
            write(iout,'(''reaxff_q0'')')
            first = .false.
          endif
          call label(nat1,0_i4,lab1)
          if (nat1.gt.maxele) then
            ctmp(1) = 'shel'
          else
            ctmp(1) = 'core'
          endif
          write(iout,'(a5,1x,a4,1x,f12.6)') lab1,ctmp(1),reaxFFq0(nat1)
        endif
      enddo
!   
!  ReaxFF EEM qshell parameters for species present
!           
      first = .true.
      do i = 1,nreaxFFspecloc
        nat1 = natreaxFFspec(i)
        if (abs(reaxFFshell(1,nat1)).gt.1.0d-12) then
          call label(nat1,0_i4,lab1)
          if (nat1.gt.maxele) then
            ctmp(1) = 'shel'
          else 
            ctmp(1) = 'core'
          endif
          if (first) then
            first = .false.
            write(iout,'(''reaxff_qshell'')')
          endif
          if (lfit) then
            write(iout,'(a5,1x,a4,3(1x,f12.6),1x,3i2)') lab1,ctmp(1),(reaxFFshell(j,nat1),j=1,3),(itmp2(k,nat1),k=4,6)
          else
            write(iout,'(a5,1x,a4,3(1x,f12.6))') lab1,ctmp(1),(reaxFFshell(j,nat1),j=1,3)
          endif
        endif
      enddo
      if (nreaxFFspecloc.eq.0.and.nreaxFFfixQspecloc.gt.0) then
!   
!  ReaxFF EEM gamma parameters for species present
!           
        first = .true.
        ctmp(1) = 'core'
        do i = 1,nreaxFFfixQspecloc
          ii = nreaxFFfixQspecptr(i)
          if (reaxFFgamma(ii).ne.0.0_dp) then
            nat1 = natreaxFFspec(ii)
            ntype1 = ntypreaxFFspec(ii)
            call label(nat1,ntype1,lab1)
            if (first) then
              first = .false.
              write(iout,'(''reaxff_gamma'')')
            endif
            if (lfit) then
              write(iout,'(a5,1x,a4,1x,f12.6,1x,i1)') lab1,ctmp(1),reaxFFgamma(ii),itmp2(3,ii)
            else
              write(iout,'(a5,1x,a4,1x,f12.6)') lab1,ctmp(1),reaxFFgamma(ii)
            endif
          endif
        enddo
      else
!   
!  ReaxFF EEM gamma parameters for species present
!           
        first = .true.
        do i = 1,nreaxFFspecloc
          nat1 = natreaxFFspec(i)
          call label(nat1,0_i4,lab1)
          if (nat1.gt.maxele) then
            ctmp(1) = 'shel'
          else
            ctmp(1) = 'core'
          endif
          if (first) then
            first = .false.
            write(iout,'(''reaxff_gamma'')')
          endif
          if (lfit) then
            write(iout,'(a5,1x,a4,1x,f12.6,1x,i1)') lab1,ctmp(1),reaxFFgamma(i),itmp2(2,nat1)
          else
            write(iout,'(a5,1x,a4,1x,f12.6)') lab1,ctmp(1),reaxFFgamma(i)
          endif
        enddo
      endif
!
!  ReaxFF EEM fixed Q
!
      first = .true.
      ctmp(1) = 'core'
      do i = 1,nreaxFFfixQspecloc
        ii = nreaxFFfixQspecptr(i)
        if (lreaxFFqfix(ii)) then
          nat1 = natreaxFFspec(ii)
          ntype1 = ntypreaxFFspec(ii)
          call label(nat1,ntype1,lab1)
          if (first) then
            first = .false.
            write(iout,'(''reaxff_fixq'')')
          endif
          if (lfit) then
            write(iout,'(a5,1x,a4,1x,f12.6,1x,i1)') lab1,ctmp(1),reaxFFqfix(ii),itmp2(7,ii)
          else
            write(iout,'(a5,1x,a4,1x,f12.6)') lab1,ctmp(1),reaxFFqfix(ii)
          endif
        endif
      enddo
!
!  ReaxFF include undercoordination flag
!
      first = .true.
      ctmp(1) = 'core'
      do i = 1,maxele
        if (i.le.10.and..not.lreaxFFunder(i)) then
          if (first) then
            first = .false.
            write(iout,'(''reaxff1_include_under'')')
          endif
          write(iout,'(a2,1x,''0'')') atsym(i)
        elseif (i.gt.10.and.lreaxFFunder(i)) then
          if (first) then
            first = .false.
            write(iout,'(''reaxff1_include_under'')')
          endif
          write(iout,'(a2,1x,''1'')') atsym(i)
        endif
      enddo
      if (lfit) then
        deallocate(itmp2,stat=status)
        if (status/=0) call deallocate_error('dumpbo','itmp2')
      endif
!-------------------------
!  End of reaxff1 terms  |
!-------------------------
!---------------------------
!  Start of reaxff2 terms  |
!---------------------------
!     
!  Set fitting flags
!       
      nreaxFFspec2 = nreaxFFspec*(nreaxFFspec+1)/2
      if (lfit) then
        allocate(itmp3(6,5,nreaxFFspec2),stat=status)
        if (status/=0) call outofmemory('dumpbo','itmp3')
        itmp3(1:6,1:5,1:nreaxFFspec2) = 0
        do i = 1,nfit
          if (nftyp(i).eq.8) then
            if (nfvar(i).ge.16.and.nfvar(i).le.20) then
              itmp3(nfvar2(i),nfvar(i)-15,nfpot(i)) = 1
            endif
          endif
        enddo
      endif
!
!  ReaxFF bond order
!
      first = .true.
      do i = 1,nreaxFFspecloc
!   
!  Generate species label for first species
!       
        nat1 = natreaxFFspec(i)
        ntype1 = ntypreaxFFspec(i)
        call label(nat1,ntype1,lab1)
        if (nat1.gt.maxele) then
          ctmp(1) = 'shel'
        else
          ctmp(1) = 'core'
        endif
        do j = 1,i
          ind = i*(i-1)/2 + j
          if (lreaxFFbocorrect(1,ind).and.lreaxFFbocorrect(2,ind).and.lreaxFFpboOK(ind)) then
            if (first) then
              write(iout,'(''reaxff2_bo over bo13'')')
              first = .false.
            endif
!   
!  Generate species label for second species
!       
            nat2 = natreaxFFspec(j)
            ntype2 = ntypreaxFFspec(j)
            call label(nat2,ntype2,lab2)
            if (nat2.gt.maxele) then
              ctmp(2) = 'shel'
            else
              ctmp(2) = 'core'
            endif
            write(iout,'(2(a5,1x,a4,1x),3(f12.6,1x),''&'')') &
              lab1,ctmp(1),lab2,ctmp(2),(reaxFFpbo(k,ind),k=1,3)
            if (lfit) then
              write(iout,'(22x,3(f12.6,1x),6i2)') (reaxFFpbo(k,ind),k=4,6),(itmp3(l,3,ind),l=1,6)
            else
              write(iout,'(22x,3(f12.6,1x))') (reaxFFpbo(k,ind),k=4,6)
            endif
          endif
        enddo
      enddo
      first = .true.
      do i = 1,nreaxFFspecloc
!   
!  Generate species label for first species
!       
        nat1 = natreaxFFspec(i)
        ntype1 = ntypreaxFFspec(i)
        call label(nat1,ntype1,lab1)
        if (nat1.gt.maxele) then
          ctmp(1) = 'shel'
        else
          ctmp(1) = 'core'
        endif
        do j = 1,i
          ind = i*(i-1)/2 + j
          if (lreaxFFbocorrect(1,ind).and..not.lreaxFFbocorrect(2,ind).and.lreaxFFpboOK(ind)) then
            if (first) then
              write(iout,'(''reaxff2_bo over'')')
              first = .false.
            endif
!   
!  Generate species label for second species
!       
            nat2 = natreaxFFspec(j)
            ntype2 = ntypreaxFFspec(j)
            call label(nat2,ntype2,lab2)
            if (nat2.gt.maxele) then
              ctmp(2) = 'shel'
            else
              ctmp(2) = 'core'
            endif
            write(iout,'(2(a5,1x,a4,1x),3(f12.6,1x),''&'')') &
              lab1,ctmp(1),lab2,ctmp(2),(reaxFFpbo(k,ind),k=1,3)
            if (lfit) then
              write(iout,'(22x,3(f12.6,1x),6i2)') (reaxFFpbo(k,ind),k=4,6),(itmp3(l,3,ind),l=1,6)
            else
              write(iout,'(22x,3(f12.6,1x))') (reaxFFpbo(k,ind),k=4,6)
            endif
          endif
        enddo
      enddo
      first = .true.
      do i = 1,nreaxFFspecloc
!   
!  Generate species label for first species
!       
        nat1 = natreaxFFspec(i)
        ntype1 = ntypreaxFFspec(i)
        call label(nat1,ntype1,lab1)
        if (nat1.gt.maxele) then
          ctmp(1) = 'shel'
        else
          ctmp(1) = 'core'
        endif
        do j = 1,i
          ind = i*(i-1)/2 + j
          if (.not.lreaxFFbocorrect(1,ind).and.lreaxFFbocorrect(2,ind).and.lreaxFFpboOK(ind)) then
            if (first) then
              write(iout,'(''reaxff2_bo bo13'')')
              first = .false.
            endif
!   
!  Generate species label for second species
!       
            nat2 = natreaxFFspec(j)
            ntype2 = ntypreaxFFspec(j)
            call label(nat2,ntype2,lab2)
            if (nat2.gt.maxele) then
              ctmp(2) = 'shel'
            else
              ctmp(2) = 'core'
            endif
            write(iout,'(2(a5,1x,a4,1x),3(f12.6,1x),''&'')') &
              lab1,ctmp(1),lab2,ctmp(2),(reaxFFpbo(k,ind),k=1,3)
            if (lfit) then
              write(iout,'(22x,3(f12.6,1x),6i2)') (reaxFFpbo(k,ind),k=4,6),(itmp3(l,3,ind),l=1,6)
            else
              write(iout,'(22x,3(f12.6,1x))') (reaxFFpbo(k,ind),k=4,6)
            endif
          endif
        enddo
      enddo
      first = .true.
      do i = 1,nreaxFFspecloc
!   
!  Generate species label for first species
!       
        nat1 = natreaxFFspec(i)
        ntype1 = ntypreaxFFspec(i)
        call label(nat1,ntype1,lab1)
        if (nat1.gt.maxele) then
          ctmp(1) = 'shel'
        else
          ctmp(1) = 'core'
        endif
        do j = 1,i
          ind = i*(i-1)/2 + j
          if (.not.lreaxFFbocorrect(1,ind).and..not.lreaxFFbocorrect(2,ind).and.lreaxFFpboOK(ind)) then
            if (first) then
              write(iout,'(''reaxff2_bo '')')
              first = .false.
            endif
!   
!  Generate species label for second species
!       
            nat2 = natreaxFFspec(j)
            ntype2 = ntypreaxFFspec(j)
            call label(nat2,ntype2,lab2)
            if (nat2.gt.maxele) then
              ctmp(2) = 'shel'
            else
              ctmp(2) = 'core'
            endif
            write(iout,'(2(a5,1x,a4,1x),3(f12.6,1x),''&'')') &
              lab1,ctmp(1),lab2,ctmp(2),(reaxFFpbo(k,ind),k=1,3)
            if (lfit) then
              write(iout,'(22x,3(f12.6,1x),6i2)') (reaxFFpbo(k,ind),k=4,6),(itmp3(l,3,ind),l=1,6)
            else
              write(iout,'(22x,3(f12.6,1x))') (reaxFFpbo(k,ind),k=4,6)
            endif
          endif
        enddo
      enddo
!
!  ReaxFF bond energy
!
      write(iout,'(''reaxff2_bond '')')
      do i = 1,nreaxFFspecloc
!   
!  Generate species label for first species
!       
        nat1 = natreaxFFspec(i)
        ntype1 = ntypreaxFFspec(i)
        call label(nat1,ntype1,lab1)
        if (nat1.gt.maxele) then
          ctmp(1) = 'shel'
        else
          ctmp(1) = 'core'
        endif
        do j = 1,i
          ind = i*(i-1)/2 + j
!   
!  Generate species label for second species
!         
          nat2 = natreaxFFspec(j)
          ntype2 = ntypreaxFFspec(j)
          call label(nat2,ntype2,lab2)
          if (nat2.gt.maxele) then
            ctmp(2) = 'shel'
          else
            ctmp(2) = 'core'
          endif
          write(iout,'(2(a5,1x,a4,1x),3(f12.6,1x),''&'')') &
            lab1,ctmp(1),lab2,ctmp(2),(reaxFFDe(l,ind),l=1,3)
          if (lfit) then
            write(iout,'(22x,2(f12.6,1x),5i2)') (reaxFFpbe(k,ind),k=1,2),(itmp3(l,1,ind),l=1,5)
          else
            write(iout,'(22x,2(f12.6,1x))') (reaxFFpbe(k,ind),k=1,2)
          endif
        enddo
      enddo
!
!  ReaxFF bond overcoordination parameter
!
      write(iout,'(''reaxff2_over '')')
      do i = 1,nreaxFFspecloc
!
!  Generate species label for first species
!
        nat1 = natreaxFFspec(i)
        ntype1 = ntypreaxFFspec(i)
        call label(nat1,ntype1,lab1)
        if (nat1.gt.maxele) then
          ctmp(1) = 'shel'
        else
          ctmp(1) = 'core'
        endif
        do j = 1,i
          ind = i*(i-1)/2 + j
!
!  Generate species label for second species
!
          nat2 = natreaxFFspec(j)
          ntype2 = ntypreaxFFspec(j)
          call label(nat2,ntype2,lab2)
          if (nat2.gt.maxele) then
            ctmp(2) = 'shel'
          else
            ctmp(2) = 'core'
          endif
          if (lfit) then
            write(iout,'(2(a5,1x,a4,1x),f12.6,i2)') lab1,ctmp(1),lab2,ctmp(2),reaxFFoc2(ind),itmp3(1,2,ind)
          else
            write(iout,'(2(a5,1x,a4,1x),f12.6)') lab1,ctmp(1),lab2,ctmp(2),reaxFFoc2(ind)
          endif
        enddo
      enddo
!
!  ReaxFF screened-Morse for VDW pairwise parameters - only output specifically input values
!
      write(iout,'(''reaxff2_morse'')')
      do i = 1,nreaxFFspecloc
!  
!  Generate species label for first species
!         
        nat1 = natreaxFFspec(i)
        ntype1 = ntypreaxFFspec(i)
        call label(nat1,ntype1,lab1)
        if (nat1.gt.maxele) then
          ctmp(1) = 'shel'
        else
          ctmp(1) = 'core'
        endif
        do j = 1,i
          ind = i*(i-1)/2 + j
          if (lreaxFFmorseinput(ind)) then
!
!  Generate species label for second species
!
            nat2 = natreaxFFspec(j) 
            ntype2 = ntypreaxFFspec(j)
            call label(nat2,ntype2,lab2)
            if (nat2.gt.maxele) then
              ctmp(2) = 'shel'
            else 
              ctmp(2) = 'core'
            endif
            write(iout,'(2(a5,1x,a4,1x),3(f12.6,1x),'' &'')') lab1,ctmp(1),lab2,ctmp(2),(reaxFFmorse(l,ind),l=1,3)
            if (lfit) then
              write(iout,'(22x,3(f12.6,1x),6i2)') (reaxFFmorse(l,ind),l=4,6),(itmp3(l,4,ind),l=1,6)
            else
              if (reaxFFmorse(6,ind).gt.0.0_dp) then
                write(iout,'(22x,3(f12.6,1x))') (reaxFFmorse(l,ind),l=4,6)
              elseif (reaxFFmorse(5,ind).gt.0.0_dp) then
                write(iout,'(22x,2(f12.6,1x))') (reaxFFmorse(l,ind),l=4,5)
              elseif (reaxFFmorse(4,ind).gt.0.0_dp) then
                write(iout,'(22x,f12.6,1x)') reaxFFmorse(4,ind)
              endif
            endif
          endif
        enddo
      enddo
!
!  ReaxFF bond penalty
!
      first = .true.
      do i = 1,nreaxFFspecloc
!   
!  Generate species label for first species
!       
        nat1 = natreaxFFspec(i)
        ntype1 = ntypreaxFFspec(i)
        call label(nat1,ntype1,lab1)
        if (nat1.gt.maxele) then
          ctmp(1) = 'shel'
        else
          ctmp(1) = 'core'
        endif
        do j = 1,i
          ind = i*(i-1)/2 + j
!   
!  Generate species label for second species
!       
          nat2 = natreaxFFspec(j)
          ntype2 = ntypreaxFFspec(j)
          call label(nat2,ntype2,lab2)
          if (nat2.gt.maxele) then
            ctmp(2) = 'shel'
          else
            ctmp(2) = 'core'
          endif
          sum = 0.0_dp
          do k = 1,3
            sum = sum + abs(reaxFFpen2(k,ind))
          enddo
          if (sum.gt.0.0_dp) then
            if (first) then
              write(iout,'(''reaxff2_pen2 '')')
              first = .false.
            endif
            if (lfit) then
              write(iout,'(2(a5,1x,a4,1x),3(f12.6,1x),3i2)') &
                lab1,ctmp(1),lab2,ctmp(2),(reaxFFpen2(k,ind),k=1,3),(itmp3(l,5,ind),l=1,3)
            else
              write(iout,'(2(a5,1x,a4,1x),3(f12.6,1x))') lab1,ctmp(1),lab2,ctmp(2),(reaxFFpen2(k,ind),k=1,3)
            endif
          endif
        enddo
      enddo
      if (lfit) then
        deallocate(itmp3,stat=status)
        if (status/=0) call deallocate_error('dumpbo','itmp3')
      endif
!-------------------------
!  End of reaxff2 terms  |
!-------------------------
!---------------------------
!  Start of reaxff3 terms  |
!---------------------------
      if (lfit) then
        allocate(itmp5(6,maxreaxFFval3,nreaxFFspec2,nreaxFFout,3),stat=status)
        if (status/=0) call outofmemory('dumpbo','itmp5')
        itmp5(1:6,1:maxreaxFFval3,1:nreaxFFspec2,1:nreaxFFout,1:3) = 0
        do i = 1,nfit
          if (nftyp(i).eq.8) then
            if (nfvar(i).ge.21.and.nfvar(i).le.23) then
              if (nfvar(i).eq.21) then
                itmp5(nfvar2(i),nfpot3(i),nfpot2(i),nfpot(i),nfvar(i)-20) = 1
              else
                itmp5(nfvar2(i),1,nfpot2(i),nfpot(i),nfvar(i)-20) = 1
              endif
            endif
          endif
        enddo
      endif
!         
!  ReaxFF valence angle triad parameters
!       
      first = .true.
      do i = 1,nreaxFFspecloc
!  
!  Generate species label for first species
!         
        nat1 = natreaxFFspec(i)
        ntype1 = ntypreaxFFspec(i)
        call label(nat1,ntype1,lab1)
        if (nat1.gt.maxele) then
          ctmp(1) = 'shel'
        else
          ctmp(1) = 'core'
        endif
        do j = 1,nreaxFFspecloc
!
!  Generate species label for second species
!
          nat2 = natreaxFFspec(j) 
          ntype2 = ntypreaxFFspec(j)
          call label(nat2,ntype2,lab2)
          if (nat2.gt.maxele) then
            ctmp(2) = 'shel'
          else 
            ctmp(2) = 'core'
          endif
          do k = 1,j
            ind = j*(j-1)/2 + k
!
!  Generate species label for third species
!
            nat3 = natreaxFFspec(k) 
            ntype3 = ntypreaxFFspec(k)
            call label(nat3,ntype3,lab3)
            if (nat3.gt.maxele) then
              ctmp(3) = 'shel'
            else 
              ctmp(3) = 'core'
            endif
            do nn = 1,nreaxFFval3(ind,i)
              if (first) then
                write(iout,'(''reaxff3_angle '')')
                first = .false.
              endif
              write(iout,'(3(a5,1x,a4,1x),2(f10.5,1x),''&'')') &
                lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),(reaxFFval3(l,nn,ind,i),l=1,2)
              if (lfit) then
                write(iout,'(22x,4(f10.5,1x),6i2)') (reaxFFval3(l,nn,ind,i),l=3,6), &
                (itmp5(m,nn,ind,i,1),m=1,6)
              else
                write(iout,'(22x,4(f10.5,1x))') (reaxFFval3(l,nn,ind,i),l=3,6)
              endif
            enddo
          enddo
        enddo
      enddo
!
!  ReaxFF valence angle triad parameters
!
      first = .true.
      do i = 1,nreaxFFspecloc
!
!  Generate species label for first species
!
        nat1 = natreaxFFspec(i)
        ntype1 = ntypreaxFFspec(i)
        call label(nat1,ntype1,lab1)
        if (nat1.gt.maxele) then
          ctmp(1) = 'shel'
        else
          ctmp(1) = 'core'
        endif
        do j = 1,nreaxFFspecloc
!
!  Generate species label for second species
!
          nat2 = natreaxFFspec(j)
          ntype2 = ntypreaxFFspec(j)
          call label(nat2,ntype2,lab2)
          if (nat2.gt.maxele) then
            ctmp(2) = 'shel'
          else
            ctmp(2) = 'core'
          endif
          do k = 1,j
            ind = j*(j-1)/2 + k
!
!  Generate species label for third species
!
            nat3 = natreaxFFspec(k)
            ntype3 = ntypreaxFFspec(k)
            call label(nat3,ntype3,lab3)
            if (nat3.gt.maxele) then
              ctmp(3) = 'shel'
            else
              ctmp(3) = 'core'
            endif
            if (reaxFFpen3(ind,i).ne.0.0_dp) then
              if (first) then
                write(iout,'(''reaxff3_penalty '')')
                first = .false.
              endif
              if (lfit) then
                write(iout,'(3(a5,1x,a4,1x),(f10.5,1x),i2)') &
                  lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),reaxFFpen3(ind,i),itmp5(1,1,ind,i,2)
              else
                write(iout,'(3(a5,1x,a4,1x),(f10.5,1x))') lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),reaxFFpen3(ind,i)
              endif
            endif
          enddo
        enddo
      enddo
!
!  ReaxFF 3-body conjugation triad parameters
!      
      first = .true.
      do i = 1,nreaxFFspecloc
! 
!  Generate species label for first species
!
        nat1 = natreaxFFspec(i)
        ntype1 = ntypreaxFFspec(i)
        call label(nat1,ntype1,lab1)
        if (nat1.gt.maxele) then
          ctmp(1) = 'shel'
        else
          ctmp(1) = 'core'
        endif
        do j = 1,nreaxFFspecloc
!
!  Generate species label for second species
!
          nat2 = natreaxFFspec(j) 
          ntype2 = ntypreaxFFspec(j)
          call label(nat2,ntype2,lab2)
          if (nat2.gt.maxele) then
            ctmp(2) = 'shel'
          else 
            ctmp(2) = 'core'
          endif
          do k = 1,j
            ind = j*(j-1)/2 + k
!  
!  Generate species label for third species
!
            nat3 = natreaxFFspec(k)
            ntype3 = ntypreaxFFspec(k)
            call label(nat3,ntype3,lab3)
            if (nat3.gt.maxele) then
              ctmp(3) = 'shel'
            else
              ctmp(3) = 'core'
            endif
            sum = 0.0_dp
            do l = 1,4
              sum = sum + abs(reaxFFconj3(l,ind,i))
            enddo
            if (sum.ne.0.0_dp) then
              if (first) then
                write(iout,'(''reaxff3_conjugation'')')
                first = .false.
              endif
              write(iout,'(3(a5,1x,a4,1x),3(f10.5,1x),''&'')') &
                lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),(reaxFFconj3(l,ind,i),l=1,3)
              if (lfit) then
                write(iout,'(33x,f10.5,4i2)') reaxFFconj3(4,ind,i),(itmp5(m,1,ind,i,3),m=1,4)
              else
                write(iout,'(33x,f10.5)') reaxFFconj3(4,ind,i)
              endif
            endif
          enddo
        enddo
      enddo
      if (lfit) then
        deallocate(itmp5,stat=status)
        if (status/=0) call deallocate_error('dumpbo','itmp5')
        allocate(itmp4(4,nreaxFFout,nreaxFFout,nreaxFFout),stat=status)
        if (status/=0) call outofmemory('dumpbo','itmp4')
        itmp4(1:4,1:nreaxFFout,1:nreaxFFout,1:nreaxFFout) = 0
        do i = 1,nfit
          if (nftyp(i).eq.8) then
            if (nfvar(i).eq.24) then
              itmp4(nfvar2(i),nfpot3(i),nfpot2(i),nfpot(i)) = 1
            endif
          endif
        enddo
      endif
!         
!  ReaxFF hydrogen bond triad parameters
!       
      first = .true.
      do i = 1,nreaxFFspecloc
!  
!  Generate species label for first species
!         
        nat1 = natreaxFFspec(i)
        ntype1 = ntypreaxFFspec(i)
        call label(nat1,ntype1,lab1)
        if (nat1.gt.maxele) then
          ctmp(1) = 'shel'
        else
          ctmp(1) = 'core'
        endif
        do j = 1,nreaxFFspecloc
!
!  Generate species label for second species
!
          nat2 = natreaxFFspec(j) 
          ntype2 = ntypreaxFFspec(j)
          call label(nat2,ntype2,lab2)
          if (nat2.gt.maxele) then
            ctmp(2) = 'shel'
          else 
            ctmp(2) = 'core'
          endif
          do k = 1,j
!
!  Generate species label for third species
!
            nat3 = natreaxFFspec(k) 
            ntype3 = ntypreaxFFspec(k)
            call label(nat3,ntype3,lab3)
            if (nat3.gt.maxele) then
              ctmp(3) = 'shel'
            else 
              ctmp(3) = 'core'
            endif
            sum = 0.0_dp
            do l = 1,4
              sum = sum + abs(reaxFFhb3(l,k,j,i))
            enddo
            if (sum.ne.0.0_dp) then
              if (first) then
                write(iout,'(''reaxff3_hbond '')')
                first = .false.
              endif
              write(iout,'(3(a5,1x,a4,1x),3(f10.5,1x),''&'')') &
                lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),(reaxFFhb3(l,k,j,i),l=1,3)
              if (lfit) then
                write(iout,'(33x,f10.5,4i2)') reaxFFhb3(4,k,j,i), &
                  (itmp4(l,k,j,i),l=1,4)
              else
                write(iout,'(33x,f10.5)') reaxFFhb3(4,k,j,i)
              endif
            endif
          enddo
        enddo
      enddo
      if (lfit) then
        deallocate(itmp4,stat=status)
        if (status/=0) call deallocate_error('dumpbo','itmp4')
      endif
!-------------------------
!  End of reaxff3 terms  |
!-------------------------
      if (lfit) then
        nreaxFFspec21 = (nreaxFFspec + 1)*(nreaxFFspec + 2)/2
        allocate(itmp3(5,nreaxFFspec21,nreaxFFspec21),stat=status)
        if (status/=0) call outofmemory('dumpbo','itmp3')
        itmp3(1:5,1:nreaxFFspec21,1:nreaxFFspec21) = 0
        do i = 1,nfit
          if (nftyp(i).eq.8) then
            if (nfvar(i).eq.25) then
              itmp3(nfvar2(i),nfpot2(i),nfpot(i)) = 1
            endif
          endif
        enddo
      endif
!
!  ReaxFF torsional parameters
!
      first = .true.
      do i = 1,nreaxFFspecloc
!
!  Generate species label for first species
!
        nat2 = natreaxFFspec(i)
        ntype2 = ntypreaxFFspec(i)
        call label(nat2,ntype2,lab2)
        if (nat2.gt.maxele) then
          ctmp(2) = 'shel'
        else
          ctmp(2) = 'core'
        endif
        do j = 1,i
!
!  Generate species label for second species
!
          nat3 = natreaxFFspec(j)
          ntype3 = ntypreaxFFspec(j)
          call label(nat3,ntype3,lab3)
          if (nat3.gt.maxele) then
            ctmp(3) = 'shel'
          else
            ctmp(3) = 'core'
          endif
          ind1 = i*(i-1)/2 + j
          do k = 1,nreaxFFspecloc
!
!  Generate species label for third species
!
            nat1 = natreaxFFspec(k)
            ntype1 = ntypreaxFFspec(k)
            call label(nat1,ntype1,lab1)
            if (nat1.gt.maxele) then
              ctmp(1) = 'shel'
            else
              ctmp(1) = 'core'
            endif
            do l = 1,k
!
!  Generate species label for third species
!
              nat4 = natreaxFFspec(l)
              ntype4 = ntypreaxFFspec(l)
              call label(nat4,ntype4,lab4)
              if (nat4.gt.maxele) then
                ctmp(4) = 'shel'
              else
                ctmp(4) = 'core'
              endif
              ind2 = k*(k-1)/2 + l + 1
              sum = 0.0_dp
              do m = 1,5
                sum = sum + abs(reaxFFtor4(m,ind2,ind1))
              enddo
              if (sum.ne.0.0_dp) then
                if (first) then
                  write(iout,'(''reaxff4_torsion '')')
                  first = .false.
                endif
                write(iout,'(4(a5,1x,a4,1x),2(f10.5,1x),''&'')') &
                  lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),lab4,ctmp(4),(reaxFFtor4(m,ind2,ind1),m=1,2)
                if (lfit) then
                  write(iout,'(33x,3(f10.5,1x),5i2)') (reaxFFtor4(m,ind2,ind1),m=3,5),(itmp3(n,ind2,ind1),n=1,5)
                else
                  write(iout,'(33x,3(f10.5,1x))') (reaxFFtor4(m,ind2,ind1),m=3,5)
                endif
              endif
            enddo
          enddo
!
!  Wildcard output if needed
!
          if (abs(reaxFFtor4(1,1,ind1)).gt.0.0_dp.or.abs(reaxFFtor4(2,1,ind1)).gt.0.0_dp.or. &
              abs(reaxFFtor4(3,1,ind1)).gt.0.0_dp) then
            lab1 = 'X'
            ctmp(1) = 'core'
            lab4 = 'X'
            ctmp(4) = 'core'
            if (first) then
              write(iout,'(''reaxff4_torsion '')')
              first = .false.
            endif
            write(iout,'(4(a5,1x,a4,1x),2(f10.5,1x),''&'')') &
              lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),lab4,ctmp(4),(reaxFFtor4(m,1,ind1),m=1,2)
            if (lfit) then
              write(iout,'(33x,3(f10.5,1x),5i2)') (reaxFFtor4(m,1,ind1),m=3,5),(itmp3(n,1,ind1),n=1,5)
            else
              write(iout,'(33x,3(f10.5,1x))') (reaxFFtor4(m,1,ind1),m=3,5)
            endif
          endif
        enddo
      enddo
      if (lfit) then
        deallocate(itmp3,stat=status)
        if (status/=0) call deallocate_error('dumpbo','itmp3')
      endif
    endif
  elseif (nreaxFFfixQspecloc.gt.0) then
!
!  ReaxFF fixed charge only information was in input file & rest was in library
!
!  Set fitting flags
!
    if (lfit) then
      allocate(itmp2(10,maxele),stat=status)
      if (status/=0) call outofmemory('dumpbo','itmp2')
      itmp2(1:8,1:maxele) = 0
      do i = 1,nfit
        if (nftyp(i).eq.8) then
          if (nfvar(i).eq.8) then
            itmp2(nfvar2(i),nfpot(i)) = 1
          endif
        endif
      enddo
    endif
!
!  ReaxFF EEM gamma parameters for species present
!
    first = .true.
    ctmp(1) = 'core'
    do i = 1,nreaxFFfixQspecloc
      ii = nreaxFFfixQspecptr(i)
      if (reaxFFgamma(ii).ne.0.0_dp) then
        call label(ii,0_i4,lab1)
        if (first) then
          first = .false.
          write(iout,'(''reaxff_gamma'')')
        endif
        if (lfit) then
          write(iout,'(a5,1x,a4,1x,f12.6,1x,i1)') lab1,ctmp(1),reaxFFgamma(ii),itmp2(3,ii)
        else
          write(iout,'(a5,1x,a4,1x,f12.6)') lab1,ctmp(1),reaxFFgamma(ii)
        endif
      endif
    enddo
!
!  ReaxFF EEM fixed Q
!
    first = .true.
    ctmp(1) = 'core'
    do i = 1,nreaxFFfixQspecloc
      ii = nreaxFFfixQspecptr(i)
      if (lreaxFFqfix(ii)) then
        nat1 = natreaxFFspec(ii)
        ntype1 = ntypreaxFFspec(ii)
        call label(nat1,ntype1,lab1)
        if (first) then
          first = .false.
          write(iout,'(''reaxff_fixq'')')
        endif
        if (lfit) then
          write(iout,'(a5,1x,a4,1x,f12.6,1x,i1)') lab1,ctmp(1),reaxFFqfix(ii),itmp2(7,ii)
        else
          write(iout,'(a5,1x,a4,1x,f12.6)') lab1,ctmp(1),reaxFFqfix(ii)
        endif
      endif
    enddo
    if (lfit) then
      deallocate(itmp2,stat=status)
      if (status/=0) call deallocate_error('dumpbo','itmp2')
    endif
  endif
!********************
!  EDIP potentials  *
!********************
  if (nEDIPspecloc.gt.0) then
    if (EDIPaccuracy1.ne.1.0d-6.or.EDIPaccuracy2.ne.1.0d-10) then
      write(iout,'(''edip_accuracy '',f16.14,1x,f16.14)') EDIPaccuracy1,EDIPaccuracy2
    endif
    if (EDIPmaxZcutoff.ne.6.0_dp) then
      write(iout,'(''edip_zmax '',f12.6)') EDIPmaxZcutoff
    endif
!-------------------------
!  Start of EDIP2 terms  |
!-------------------------
!     
!  Set fitting flags
!       
    nEDIPspec2 = nEDIPspec*(nEDIPspec+1)/2
    if (lfit) then
      allocate(itmp3(6,2,nEDIPspec2),stat=status)
      if (status/=0) call outofmemory('dumpbo','itmp3')
      itmp3(1:6,1:2,1:nEDIPspec2) = 0
      do i = 1,nfit
        if (nftyp(i).eq.9) then
          if (nfvar(i).le.2) then
            itmp3(nfvar2(i),nfvar(i),nfpot(i)) = 1
          endif
        endif
      enddo
    endif
!
!  EDIP coordination number
!
    first = .true.
    do i = 1,nEDIPspecloc
!   
!  Generate species label for first species
!       
      nat1 = natEDIPspec(i)
      ntype1 = ntypEDIPspec(i)
      call label(nat1,ntype1,lab1)
      if (nat1.gt.maxele) then
        ctmp(1) = 'shel'
      else
        ctmp(1) = 'core'
      endif
      do j = 1,i
        ind = i*(i-1)/2 + j
        if (lEDIPpairOK(ind)) then
          if (first) then
            write(iout,'(''edip_coordination '')')
            first = .false.
          endif
!   
!  Generate species label for second species
!       
          nat2 = natEDIPspec(j)
          ntype2 = ntypEDIPspec(j)
          call label(nat2,ntype2,lab2)
          if (nat2.gt.maxele) then
            ctmp(2) = 'shel'
          else
            ctmp(2) = 'core'
          endif
          write(iout,'(2(a5,1x,a4,1x),3(f12.6,1x),''&'')') &
            lab1,ctmp(1),lab2,ctmp(2),EDIPalpha(ind),EDIPflow(ind),EDIPfhigh(ind)
          if (lfit) then
            write(iout,'(12x,3(f12.6,1x),2(f8.5,1x),4i2)') EDIPZdih(ind),EDIPZrep(ind),EDIPc0(ind), &
              EDIPplow(ind),EDIPphigh(ind),(itmp3(l,1,ind),l=1,4)
          else
            write(iout,'(12x,3(f12.6,1x),2(f8.5,1x))') EDIPZdih(ind),EDIPZrep(ind),EDIPc0(ind), &
              EDIPplow(ind),EDIPphigh(ind)
          endif
        endif
      enddo
    enddo
!
!  EDIP twobody terms
!
    first = .true.
    do i = 1,nEDIPspecloc
!
!  Generate species label for first species
!
      nat1 = natEDIPspec(i)
      ntype1 = ntypEDIPspec(i)
      call label(nat1,ntype1,lab1)
      if (nat1.gt.maxele) then
        ctmp(1) = 'shel'
      else
        ctmp(1) = 'core'
      endif
      do j = 1,i
        ind = i*(i-1)/2 + j
        if (lEDIPpairOK(ind)) then
          if (first) then
            write(iout,'(''edip_twobody '')')
            first = .false.
          endif
!
!  Generate species label for second species
!
          nat2 = natEDIPspec(j)
          ntype2 = ntypEDIPspec(j)
          call label(nat2,ntype2,lab2)
          if (nat2.gt.maxele) then
            ctmp(2) = 'shel'
          else
            ctmp(2) = 'core'
          endif
          write(iout,'(2(a5,1x,a4,1x),3(f12.6,1x),''&'')') &
            lab1,ctmp(1),lab2,ctmp(2),EDIP2epsilon(ind),EDIP2B(ind),EDIP2beta(ind)
          if (lfit) then
            write(iout,'(12x,3(f12.6,1x),6i2)') EDIP2sigma(ind),EDIP2a(ind),EDIP2aprime(ind),(itmp3(l,2,ind),l=1,6)
          else
            write(iout,'(12x,3(f12.6,1x))') EDIP2sigma(ind),EDIP2a(ind),EDIP2aprime(ind)
          endif
        endif
      enddo
    enddo
    if (lfit) then
      deallocate(itmp3,stat=status)
      if (status/=0) call deallocate_error('dumpbo','itmp3')
    endif
!-----------------------
!  End of EDIP2 terms  |
!-----------------------
!-------------------------
!  Start of EDIP3 terms  |
!-------------------------
    if (lfit) then
      nEDIPout2 = nEDIPout*(nEDIPout+1)/2
      allocate(itmp3(7,nEDIPout2,nEDIPout),stat=status)
      if (status/=0) call outofmemory('dumpbo','itmp3')
      itmp3(1:7,1:nEDIPout2,1:nEDIPout) = 0
      do i = 1,nfit
        if (nftyp(i).eq.9) then
          if (nfvar(i).eq.3) then
            itmp3(nfvar2(i),nfpot2(i),nfpot(i)) = 1
          endif
        endif
      enddo
    endif
    first = .true.
    do i = 1,nEDIPspecloc
!  
!  Generate species label for first species
!         
      nat1 = natEDIPspec(i)
      ntype1 = ntypEDIPspec(i)
      call label(nat1,ntype1,lab1)
      if (nat1.gt.maxele) then
        ctmp(1) = 'shel'
      else
        ctmp(1) = 'core'
      endif
      do j = 1,nEDIPspecloc
!
!  Generate species label for second species
!
        nat2 = natEDIPspec(j) 
        ntype2 = ntypEDIPspec(j)
        call label(nat2,ntype2,lab2)
        if (nat2.gt.maxele) then
          ctmp(2) = 'shel'
        else 
          ctmp(2) = 'core'
        endif
        do k = 1,j
          ind = j*(j-1)/2 + k
          if (lEDIPtriadOK(ind,i)) then
!
!  Generate species label for third species
!
            nat3 = natEDIPspec(k) 
            ntype3 = ntypEDIPspec(k)
            call label(nat3,ntype3,lab3)
            if (nat3.gt.maxele) then
              ctmp(3) = 'shel'
            else 
              ctmp(3) = 'core'
            endif
            if (first) then
              write(iout,'(''edip_threebody '')')
              first = .false.
            endif
            write(iout,'(3(a5,1x,a4,1x),3(f10.5,1x),''&'')') &
              lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),EDIP3lambda0(ind,i),EDIP3lambdap(ind,i),EDIP3Z0(ind,i)
              write(iout,'(22x,3(f10.5,1x),''&'')') EDIP3gamma0(ind,i),EDIP3gammap(ind,i),EDIP3q(ind,i)
            if (lfit) then
              write(iout,'(22x,f10.5,1x,7i2)') EDIP3kq2(ind,i),(itmp3(m,ind,i),m=1,7)
            else
              write(iout,'(22x,f10.5)') EDIP3kq2(ind,i)
            endif
          endif
        enddo
      enddo
    enddo
    if (lfit) then
      deallocate(itmp3,stat=status)
      if (status/=0) call deallocate_error('dumpbo','itmp3')
    endif
!-----------------------
!  End of EDIP3 terms  |
!-----------------------
  endif
!
  return
  end
