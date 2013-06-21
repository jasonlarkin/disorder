  subroutine dump34(iout)
!
!  Dump out three- and four-body potentials for restart file
!
!   2/95 Exponential and SW 3-body potentials added
!   3/95 Bcross potential added
!   3/96 Urey-Bradley potential added
!   4/96 Vessal modification of exponential potential added
!   6/96 General theta0 added to SW3
!   3/97 Out of plane potential added
!   3/98 Cosine form of three-body added
!   6/98 Murrell-Mottram three-body potential added
!   8/98 ESFF torsion potential added
!  10/98 Codes for fitting variables simplified
!   8/99 Linear-threebody potential added
!   5/01 Cut-offs now output, even when bond sub-option is
!        used for SW3 potential since they are parameters
!   5/01 Minimum three-body cut-offs added
!   7/02 K4 added for outofplane potential
!  10/02 Bcoscross potential added
!  10/02 Torharm potential added
!   4/03 Exponentially decaying torsion potential added
!   4/03 Tapered torsional potential added
!   9/04 Jiang & Brown form of SW added
!  11/04 torangle potential added
!  11/04 Six-body potential added
!   6/05 Format of ryckaert-bellemanns cleaned
!   9/05 Correction to lin3 output
!  10/05 Hydrogen-bond threebody potential added
!  10/05 Inversion outofplane potential added
!  12/05 Equatorial ESFF three-body potential added
!   6/06 Squared inversion added
!   7/06 Six-body potentials added
!   8/06 Bond type words added to output
!   9/06 Dreiding sub-option added to torsion potentials
!   9/06 Taper option added to hydrogen bond potential
!   9/06 Use of literal symbols from input added
!   1/07 UFF3 potential added
!   1/07 UFF4 potential added
!   1/07 Amide bond type added
!   5/07 ltdreiding added for hydrogen bonding potential
!   5/07 Format of torsion option lines modified to allow for more botywords
!   7/07 Bond types for three-body terms modified to reflect extra dimension of n3botype
!  10/07 Angle-angle cross potential added
!   4/08 Minimum cutoffs added for out of plane
!   5/08 UFF oop potential added
!   5/08 only3 sub-option added to out of plane potentials
!   6/08 Sub-option for bond number checking added to three-body potentials
!   6/08 Sub-option string handling modified for 3-body terms to allow for bonding checks
!  11/08 bacoscross form added
!  11/08 xcosangleangle potential added
!  11/08 torcosangle potential added
!   3/08 3coulomb potential added
!   6/09 Module name changed from three to m_three
!   7/09 exp2 potential added
!   3/10 Rule based potentials excluded from dump to avoid duplication
!   5/10 g3coulomb potential added
!   3/11 Missing dump of bond sub-option added for bacross potential
!  11/12 Format of some torsion force constants changed
!   1/13 bagcross potential added
!   5/13 Custom bond type added
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
  use constants
  use control
  use current
  use element
  use fitting
  use four
  use library
  use six
  use m_three
  implicit none
!
!  Passed variables
!
  integer(i4),      intent(in)                 :: iout
!
!  Local variables
!
  character(len=4)                             :: ctmp(6)
  character(len=5)                             :: lab1
  character(len=5)                             :: lab2
  character(len=5)                             :: lab3
  character(len=5)                             :: lab4
  character(len=5)                             :: lab5
  character(len=5)                             :: lab6
  character(len=5)                             :: mword(3)
  character(len=9)                             :: botyword(15)
  character(len=9)                             :: wboty(10)
  integer(i4)                                  :: i
  integer(i4)                                  :: ii
  integer(i4)                                  :: in3
  integer(i4), dimension(:), allocatable       :: itmp
  integer(i4)                                  :: itmp1(11)
  integer(i4)                                  :: j
  integer(i4)                                  :: k
  integer(i4)                                  :: nat1
  integer(i4)                                  :: nat2
  integer(i4)                                  :: nat3
  integer(i4)                                  :: nat4
  integer(i4)                                  :: nat5
  integer(i4)                                  :: nat6
  integer(i4)                                  :: nboty
  integer(i4)                                  :: nbotyptr(5)
  integer(i4)                                  :: nforo
  integer(i4)                                  :: nmfptr
  integer(i4)                                  :: nmsptr
  integer(i4)                                  :: nmtptr
  integer(i4)                                  :: nsixo
  integer(i4)                                  :: nthbo
  integer(i4)                                  :: ntype1
  integer(i4)                                  :: ntype2
  integer(i4)                                  :: ntype3
  integer(i4)                                  :: ntype4
  integer(i4)                                  :: ntype5
  integer(i4)                                  :: ntype6
  integer(i4)                                  :: status
  real(dp)                                     :: phi0
  real(dp)                                     :: rho1
  real(dp)                                     :: rho2
  real(dp)                                     :: the
!
  data mword/'intra','inter','     '/
  data botyword/'single   ','double   ','triple   ','quadruple','resonant',  &
                'amide    ','custom   ','regular  ','cyclic   ','exocyclic', &
                'dreiding ','taper    ','only3    ','nbeq     ','nbne     '/
!
  if (nlib.gt.0.and..not.llibdump) then
    nthbo = nlib3s
    nforo = nlib4s
    nsixo = nlib6s
  else
    nthbo = nthb
    nforo = nfor
    nsixo = nsix
  endif
!*************************
!  Three-body potentials *
!*************************
  if (nthbo.gt.0) then
    allocate(itmp(5*nthbo),stat=status)
    if (status/=0) call outofmemory('dump34','itmp')
    if (lfit) then
      do i = 1,5*nthbo
        itmp(i) = 0
      enddo
      do i = 1,nfit
        if (nftyp(i).eq.3) then
          if (nfvar(i).le.5) then
            itmp(5*(nfpot(i)-1)+nfvar(i)) = 1
          endif
        endif
      enddo
    endif
!
!  Loop over three-body potentials
!
    loop3body: do i = 1,nthbo
!
!  Skip three-body potentials that were generated by rules
!
      if (lgenerated3(i)) cycle loop3body
!
      the = theta(i)
!
!  Generate species labels
!
      nat1 = ntspec1(i)
      ntype1 = ntptyp1(i)
      if (llibsymdump) then
        lab1 = symbol3(1,i)
      else
        call label(nat1,ntype1,lab1)
      endif
      if (nat1.gt.maxele) then
        ctmp(1) = 'shel'
      else
        ctmp(1) = 'core'
      endif
      nat2 = ntspec2(i)
      ntype2 = ntptyp2(i)
      if (llibsymdump) then
        lab2 = symbol3(2,i)
      else
        call label(nat2,ntype2,lab2)
      endif
      if (nat2.gt.maxele) then
        ctmp(2) = 'shel'
      else
        ctmp(2) = 'core'
      endif
      nat3 = ntspec3(i)
      ntype3 = ntptyp3(i)
      if (llibsymdump) then
        lab3 = symbol3(3,i)
      else
        call label(nat3,ntype3,lab3)
      endif
      if (nat3.gt.maxele) then
        ctmp(3) = 'shel'
      else
        ctmp(3) = 'core'
      endif
!
!  Set intra/inter/both pointer
!
      if (ltintra(i).and..not.ltinter(i)) then
        nmtptr = 1
      elseif (ltinter(i).and..not.ltintra(i)) then
        nmtptr = 2
      else
        nmtptr = 3
      endif
!
!  Set bond type words
!
      nboty = 0
      if (n3botype(1,1,i).gt.0) then
        nboty = nboty + 1
        nbotyptr(nboty) = n3botype(1,1,i)
        wboty(nboty) = botyword(nbotyptr(nboty))
      elseif (n3botype(1,2,i).gt.0) then
        nboty = nboty + 1
        nbotyptr(nboty) = 1
        wboty(nboty) = botyword(nbotyptr(nboty))
      endif
      if (n3botype(2,1,i).gt.1) then
        nboty = nboty + 1
        nbotyptr(nboty) = 7 + n3botype(2,1,i)
        wboty(nboty) = botyword(nbotyptr(nboty))
      elseif (n3botype(2,2,i).gt.0) then
        nboty = nboty + 1
        nbotyptr(nboty) = 8
        wboty(nboty) = botyword(nbotyptr(nboty))
      endif
      if (n3botype(1,2,i).gt.0) then
        nboty = nboty + 1
        nbotyptr(nboty) = n3botype(1,2,i)
        wboty(nboty) = botyword(nbotyptr(nboty))
      endif
      if (n3botype(2,2,i).gt.1) then
        nboty = nboty + 1
        nbotyptr(nboty) = 7 + n3botype(2,2,i)
        wboty(nboty) = botyword(nbotyptr(nboty))
      endif
      if (lthetataper(i)) then
        nboty = nboty + 1
        nbotyptr(nboty) = 12
        wboty(nboty) = botyword(nbotyptr(nboty))
      endif
      if (n3bondnono(1,i).gt.0) then
        do in3 = 1,n3bondnono(1,i)
          nboty = nboty + 1
          nbotyptr(nboty) = 14
          wboty(nboty) = botyword(nbotyptr(nboty))
          write(wboty(nboty)(6:7),'(i2)') n3bondno(in3,1,i)
        enddo
      endif
      if (n3bondnono(2,i).gt.0) then
        do in3 = 1,n3bondnono(2,i)
          nboty = nboty + 1
          nbotyptr(nboty) = 15
          wboty(nboty) = botyword(nbotyptr(nboty))
          write(wboty(nboty)(6:7),'(i2)') n3bondno(in3,2,i)
        enddo
      endif
!
      if (nthrty(i).eq.1) then
        if (mmtexc(i).eq.1) then
          if (thrho1(i).ne.0.0_dp.and.thrho2(i).ne.0.0_dp) then
            if (lfit) then
              write(iout,'(''three k3 k4 bond '',a5,5(1x,a9))') mword(nmtptr),(wboty(j),j=1,nboty)
              write(iout,'(3(a5,1x,a4,1x),3(g11.4,1x),''&'')')lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),thbk(i), &
                thrho2(i),thrho1(i)
              write(iout,'(g11.4,1x,4i2)')the,itmp(5*(i-1)+1),itmp(5*(i-1)+3),itmp(5*(i-1)+4),itmp(5*(i-1)+2)
            else
              write(iout,'(''three k3 k4 bond '',a5,5(1x,a9))') mword(nmtptr),(wboty(j),j=1,nboty)
              write(iout,'(3(a5,1x,a4,1x),3(g11.4,1x),''&'')')lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),thbk(i), &
                thrho2(i),thrho1(i)
              write(iout,'(g11.4)')the
            endif
          elseif (thrho2(i).ne.0.0_dp) then
            if (lfit) then
              write(iout,'(''three k3 bond '',a5,5(1x,a9))') mword(nmtptr),(wboty(j),j=1,nboty)
              write(iout,'(3(a5,1x,a4,1x),3(g11.4,1x),3i2)') lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),thbk(i), &
                thrho2(i),the,itmp(5*(i-1)+1),itmp(5*(i-1)+3),itmp(5*(i-1)+2)
            else
              write(iout,'(''three k3 bond '',a5,5(1x,a9))') mword(nmtptr),(wboty(j),j=1,nboty)
              write(iout,'(3(a5,1x,a4,1x),3(g11.4,1x))') lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),thbk(i),thrho2(i),the
            endif
          elseif (thrho1(i).ne.0.0_dp) then
            if (lfit) then
              write(iout,'(''three k4 bond '',a5,5(1x,a9))') mword(nmtptr),(wboty(j),j=1,nboty)
              write(iout,'(3(a5,1x,a4,1x),3(g11.4,1x),3i2)') lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),thbk(i), &
                thrho1(i),the,itmp(5*(i-1)+1),itmp(5*(i-1)+4),itmp(5*(i-1)+2)
            else
              write(iout,'(''three k4 bond '',a5,5(1x,a9))') mword(nmtptr),(wboty(j),j=1,nboty)
              write(iout,'(3(a5,1x,a4,1x),3(g11.4,1x))') lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),thbk(i),thrho1(i),the
            endif
          else
            if (lfit) then
              write(iout,'(''three bond '',a5,5(1x,a9))') mword(nmtptr),(wboty(j),j=1,nboty)
              write(iout,'(3(a5,1x,a4,1x),g10.5,1x,g10.5,2i2)') &
                lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),thbk(i),the,itmp(5*(i-1)+1),itmp(5*(i-1)+2)
            else
              write(iout,'(''three bond '',a5,5(1x,a9))') mword(nmtptr),(wboty(j),j=1,nboty)
              write(iout,'(3(a5,1x,a4,1x),g10.5,1x,g10.5)') lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),thbk(i),the
            endif
          endif
        else
          if (thrho1(i).ne.0.0_dp.and.thrho2(i).ne.0.0_dp) then
            if (lfit) then
              write(iout,'(''three k3 k4 '',a5,5(1x,a9))') mword(nmtptr),(wboty(j),j=1,nboty)
              write(iout,'(3(a5,1x,a4,1x),3(g11.4,1x),''&'')') &
                lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),thbk(i),thrho2(i),thrho1(i)
              write(iout,'(g11.4,1x,6(1x,f8.4),4i2)') the,thr1min(i),thr1(i),thr2min(i),thr2(i),thr3min(i), &
                thr3(i),itmp(5*(i-1)+1),itmp(5*(i-1)+3),itmp(5*(i-1)+4),itmp(5*(i-1)+2)
            else
              write(iout,'(''three k3 k4 '',a5,5(1x,a9))') mword(nmtptr),(wboty(j),j=1,nboty)
              write(iout,'(3(a5,1x,a4,1x),3(g11.4,1x),''&'')') &
                  lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),thbk(i),thrho2(i),thrho1(i)
              write(iout,'(g11.4,6(1x,f6.3))') the,thr1min(i),thr1(i),thr2min(i),thr2(i),thr3min(i),thr3(i)
            endif
          elseif (thrho2(i).ne.0.0_dp) then
            if (lfit) then
              write(iout,'(''three k3 '',a5,5(1x,a9))') mword(nmtptr),(wboty(j),j=1,nboty)
              write(iout,'(3(a5,1x,a4,1x),3(g11.4,1x),''&'')') &
                lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),thbk(i),thrho2(i),the
              write(iout,'(6(1x,f6.3),3i2)') &
                thr1min(i),thr1(i),thr2min(i),thr2(i),thr3min(i), &
                thr3(i),itmp(5*(i-1)+1),itmp(5*(i-1)+3),itmp(5*(i-1)+2)
            else
              write(iout,'(''three k3 '',a5,5(1x,a9))') mword(nmtptr),(wboty(j),j=1,nboty)
              write(iout,'(3(a5,1x,a4,1x),3(g11.4,1x),''&'')') &
                lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),thbk(i),thrho2(i),the
              write(iout,'(6(1x,f6.3))') &
                thr1min(i),thr1(i),thr2min(i),thr2(i),thr3min(i),thr3(i)
            endif
          elseif (thrho1(i).ne.0.0_dp) then
            if (lfit) then
              write(iout,'(''three k4 '',a5,5(1x,a9))') mword(nmtptr),(wboty(j),j=1,nboty)
              write(iout,'(3(a5,1x,a4,1x),3(g11.4,1x),''&'')') &
                lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),thbk(i),thrho1(i),the
              write(iout,'(6(1x,f6.3),3i2)') &
                thr1min(i),thr1(i),thr2min(i),thr2(i),thr3min(i), &
                thr3(i),itmp(5*(i-1)+1),itmp(5*(i-1)+4),itmp(5*(i-1)+2)
            else
              write(iout,'(''three k4 '',a5,5(1x,a9))') mword(nmtptr),(wboty(j),j=1,nboty)
              write(iout,'(3(a5,1x,a4,1x),3(g11.4,1x),'' &'')') &
                lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),thbk(i),thrho1(i),the
              write(iout,'(6(1x,f6.3))') &
                thr1min(i),thr1(i),thr2min(i),thr2(i),thr3min(i),thr3(i)
            endif
          else
            if (lfit) then
              write(iout,'(''three '',a5,5(1x,a9))') mword(nmtptr),(wboty(j),j=1,nboty)
              write(iout,'(3(a5,1x,a4,1x),g10.5,1x,f10.6,'' &'')') &
                lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),thbk(i),the
              write(iout,'(6(1x,f6.3),2i2)')thr1min(i),thr1(i), &
                thr2min(i),thr2(i),thr3min(i),thr3(i),itmp(5*(i-1)+1),itmp(5*(i-1)+2)
            else
              write(iout,'(''three '',a5,5(1x,a9))') mword(nmtptr),(wboty(j),j=1,nboty)
              write(iout,'(3(a5,1x,a4,1x),g10.5,1x,f10.5,'' &'')') &
                lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),thbk(i),the
              write(iout,'(6(1x,f6.3))')thr1min(i),thr1(i),thr2min(i),thr2(i),thr3min(i),thr3(i)
            endif
          endif
        endif
      elseif (nthrty(i).eq.2.or.nthrty(i).eq.8) then
        rho1 = thrho1(i)
        rho2 = thrho2(i)
        if (mmtexc(i).eq.1) then
          if (lfit) then
            if (nthrty(i).eq.2) then
              write(iout,'(''three exponential bond '',a5,5(1x,a9))') mword(nmtptr),(wboty(j),j=1,nboty)
            else
              write(iout,'(''three vessal bond '',a5,5(1x,a9))') mword(nmtptr),(wboty(j),j=1,nboty)
            endif
            write(iout,'(3(a5,1x,a3,1x),g17.6,1x,f8.4,2(1x,f8.6),'' &'')') &
              lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),thbk(i),the,rho1,rho2
            write(iout,'(4i2)') itmp(5*(i-1)+1),itmp(5*(i-1)+2),itmp(5*(i-1)+3),itmp(5*(i-1)+4)
          else
            if (nthrty(i).eq.2) then
              write(iout,'(''three exponential bond '',a5,5(1x,a9))') mword(nmtptr),(wboty(j),j=1,nboty)
            else
              write(iout,'(''three vessal bond '',a5,5(1x,a9))') mword(nmtptr),(wboty(j),j=1,nboty)
            endif
            write(iout,'(3(a5,1x,a3,1x),g17.6,1x,f8.4,2(1x,f8.6))') &
              lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),thbk(i),the,rho1,rho2
          endif
        else
          if (lfit) then
            if (nthrty(i).eq.2) then
              write(iout,'(''three exponential '',a5,5(1x,a9))') mword(nmtptr),(wboty(j),j=1,nboty)
            else
              write(iout,'(''three vessal '',a5,5(1x,a9))') mword(nmtptr),(wboty(j),j=1,nboty)
            endif
            write(iout,'(3(a5,1x,a3,1x),g17.6,1x,f8.4,2(1x,f8.6),'' &'')') &
              lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),thbk(i),the,rho1,rho2
            write(iout,'(6(1x,f8.3),4i2)')thr1min(i),thr1(i), &
              thr2min(i),thr2(i),thr3min(i),thr3(i),(itmp(5*(i-1)+j),j=1,4)
          else
            if (nthrty(i).eq.2) then
              write(iout,'(''three exponential '',a5,5(1x,a9))') mword(nmtptr),(wboty(j),j=1,nboty)
            else
              write(iout,'(''three vessal '',a5,5(1x,a9))') mword(nmtptr),(wboty(j),j=1,nboty)
            endif
            write(iout,'(3(a5,1x,a3,1x),g17.6,1x,f8.4,2(1x,f8.6),'' &'')') &
              lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),thbk(i),the,rho1,rho2
            write(iout,'(6(1x,f8.3))')thr1min(i),thr1(i),thr2min(i),thr2(i),thr3min(i),thr3(i)
          endif
        endif
      elseif (nthrty(i).eq.3) then
        if (mmtexc(i).eq.1) then
          write(iout,'(''axilrod-teller bond '',a5,5(1x,a9))') mword(nmtptr),(wboty(j),j=1,nboty)
          if (lfit) then
            write(iout,'(3(a5,1x,a4,1x),g14.6,i2)') &
              lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),thbk(i),itmp(5*(i-1)+1)
          else
            write(iout,'(3(a5,1x,a4,1x),g14.6)')lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),thbk(i)
          endif
        else
          write(iout,'(''axilrod-teller '',a5,5(1x,a9))') mword(nmtptr),(wboty(j),j=1,nboty)
          write(iout,'(3(a5,1x,a4,1x),g14.6,2(1x,f7.3),'' &'')') &
            lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),thbk(i),thr1min(i),thr1(i)
          if (lfit) then
            write(iout,'(4(1x,f8.3),i2)') thr2min(i),thr2(i),thr3min(i),thr3(i),itmp(5*(i-1)+1)
          else
            write(iout,'(4(1x,f8.3))') thr2min(i),thr2(i),thr3min(i),thr3(i)
          endif
        endif
      elseif (nthrty(i).eq.4) then
        if (mmtexc(i).eq.1) then
          write(iout,'(''exponential bond '',a5,5(1x,a9))') mword(nmtptr),(wboty(j),j=1,nboty)
          if (lfit) then
            write(iout,'(3(a5,1x,a4,1x),g14.6,3(1x,f8.6),4i2)') &
              lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),thbk(i), &
              ctmp(3),thbk(i),theta(i),thrho1(i),thrho2(i),(itmp(5*(i-1)+j),j=1,4)
          else
            write(iout,'(3(a5,1x,a4,1x),g14.6,3(1x,f7.5))') &
              lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),thbk(i),theta(i),thrho1(i),thrho2(i)
          endif
        else
          write(iout,'(''exponential '',a5,5(1x,a9))') mword(nmtptr),(wboty(j),j=1,nboty)
          write(iout,'(3(a5,1x,a4,1x),g14.6,3(1x,f8.6),'' &'')') &
            lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),thbk(i),theta(i),thrho1(i),thrho2(i)
          if (lfit) then
            write(iout,'(6(1x,f8.3),4i2)') &
              thr1min(i),thr1(i),thr2min(i),thr2(i),thr3min(i),thr3(i),(itmp(5*(i-1)+j),j=1,4)
          else
            write(iout,'(6(1x,f8.3))') &
              thr1min(i),thr1(i),thr2min(i),thr2(i),thr3min(i),thr3(i)
          endif
        endif
      elseif (nthrty(i).eq.5) then
        write(iout,'(''sw3 '',a5,5(1x,a9))') mword(nmtptr),(wboty(j),j=1,nboty)
        write(iout,'(3(a5,1x,a4,1x),g14.6,1x,f8.4,2(1x,f8.6),'' &'')') &
          lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),thbk(i),the,thrho1(i),thrho2(i)
        if (lfit) then
          write(iout,'(6(1x,f8.3),4i2)') &
            thr1min(i),thr1(i),thr2min(i),thr2(i),thr3min(i),thr3(i),(itmp(5*(i-1)+j),j=1,4)
        else
          write(iout,'(6(1x,f8.3))')thr1min(i),thr1(i),thr2min(i),thr2(i),thr3min(i),thr3(i)
        endif
      elseif (nthrty(i).eq.6) then
        rho1 = thrho1(i)
        rho2 = thrho2(i)
        if (mmtexc(i).eq.1) then
          write(iout,'(''bcross bond '',a5,5(1x,a9))') mword(nmtptr),(wboty(j),j=1,nboty)
          if (lfit) then
            write(iout,'(3(a5,1x,a3,1x),g11.5,1x,2(1x,f8.6),3i2)') &
              lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),thbk(i),rho1,rho2,itmp(5*(i-1)+1), &
              itmp(5*(i-1)+3),itmp(5*(i-1)+4)
          else
            write(iout,'(3(a5,1x,a3,1x),g11.5,1x,2(1x,f8.6))') &
              lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),thbk(i),rho1,rho2
          endif
        else
          write(iout,'(''bcross '',a5,5(1x,a9))') mword(nmtptr),(wboty(j),j=1,nboty)
          write(iout,'(3(a5,1x,a3,1x),g11.5,1x,2(1x,f8.6),'' &'')') &
            lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),thbk(i),rho1,rho2
          if (lfit) then
            write(iout,'(6(1x,f8.3),3i2)') thr1min(i),thr1(i), &
              thr2min(i),thr2(i),thr3min(i),thr3(i),itmp(5*(i-1)+1),itmp(5*(i-1)+3),itmp(5*(i-1)+4)
          else
            write(iout,'(6(1x,f8.3))') thr1min(i),thr1(i),thr2min(i),thr2(i),thr3min(i),thr3(i)
          endif
        endif
      elseif (nthrty(i).eq.7) then
        if (mmtexc(i).eq.1) then
          write(iout,'(''urey-bradley bond '',a5,5(1x,a9))') mword(nmtptr),(wboty(j),j=1,nboty)
          if (lfit) then
            write(iout,'(3(a5,1x,a3,1x),g11.5,1x,f8.6,2i2)') lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3), &
              thbk(i),theta(i),itmp(5*(i-1)+1),itmp(5*(i-1)+2)
          else
            write(iout,'(3(a5,1x,a3,1x),g11.5,1x,f8.6)') lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),thbk(i),theta(i)
          endif
        else
          write(iout,'(''urey-bradley '',a5,5(1x,a9))') mword(nmtptr),(wboty(j),j=1,nboty)
          write(iout,'(3(a5,1x,a3,1x),g11.5,1x,f8.6,'' &'')') &
            lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),thbk(i),theta(i)
          if (lfit) then
            write(iout,'(6(1x,f8.3),2i2)')thr1min(i),thr1(i), &
              thr2min(i),thr2(i),thr3min(i),thr3(i),itmp(5*(i-1)+1),itmp(5*(i-1)+2)
          else
            write(iout,'(6(1x,f8.3))')thr1min(i),thr1(i),thr2min(i),thr2(i),thr3min(i),thr3(i)
          endif
        endif
      elseif (nthrty(i).eq.9) then
        if (mmtexc(i).eq.1) then
          write(iout,'(''three cosine bond '',a5,5(1x,a9))') mword(nmtptr),(wboty(j),j=1,nboty)
          if (lfit) then
            write(iout,'(3(a5,1x,a4,1x),2(g14.6,1x),2i2)') &
              lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),thbk(i),the,itmp(5*(i-1)+1),itmp(5*(i-1)+2)
          else
            write(iout,'(3(a5,1x,a4,1x),2(g14.6,1x))') &
              lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),thbk(i),the
          endif
        else
          write(iout,'(''three cosine '',a5,5(1x,a9))') mword(nmtptr),(wboty(j),j=1,nboty)
          write(iout,'(3(a5,1x,a4,1x),2(g10.5,1x),'' &'')') &
            lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),thbk(i),the
          if (lfit) then
            write(iout,'(6(1x,f8.3),2i2)') &
              thr1min(i),thr1(i),thr2min(i),thr2(i),thr3min(i),thr3(i),itmp(5*(i-1)+1),itmp(5*(i-1)+2)
          else
            write(iout,'(6(1x,f8.3))') &
              thr1min(i),thr1(i),thr2min(i),thr2(i),thr3min(i),thr3(i)
          endif
        endif
      elseif (nthrty(i).eq.10) then
        if (mmtexc(i).eq.1) then
          write(iout,'(''murrel-mottram bond '',a5,5(1x,a9))') mword(nmtptr),(wboty(j),j=1,nboty)
          write(iout,'(3(a5,1x,a4,1x),g14.6,1x,f8.5,'' &'')') &
            lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),thbk(i),theta(i)
          if (lfit) then
            write(iout,'(3(1x,f8.6),5i2)')thrho1(i),thrho2(i),thrho3(i),(itmp(5*(i-1)+j),j=1,5)
          else
            write(iout,'(3(1x,f8.6))')thrho1(i),thrho2(i),thrho3(i)
          endif
        else
          write(iout,'(''murrel-mottram '',a5,5(1x,a9))') mword(nmtptr),(wboty(j),j=1,nboty)
          write(iout,'(3(a5,1x,a4,1x),g14.6,1x,f8.5,'' &'')') &
            lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),thbk(i),theta(i)
          if (lfit) then
            write(iout,'(3(1x,f8.6),6(1x,f6.3),5i2)')thrho1(i), &
              thrho2(i),thrho3(i),thr1min(i),thr1(i),thr2min(i), &
              thr2(i),thr3min(i),thr3(i),(itmp(5*(i-1)+j),j=1,5)
          else
            write(iout,'(3(1x,f8.6),6(1x,f6.3))') &
              thrho1(i),thrho2(i),thrho3(i),thr1min(i),thr1(i),thr2min(i),thr2(i),thr3min(i),thr3(i)
          endif
        endif
        if (lfit) then
          do ii = 1,11
            itmp1(ii) = 0
          enddo
          do ii = 1,nfit
            if (nftyp(ii).eq.3) then
              if (nfvar(ii).gt.5) then
                itmp1(nfvar(ii)-5) = 1
              endif
            endif
          enddo
          write(iout,'(1x,6(1x,f10.5),'' &'')')(threepoly(j,i),j=1,6)
          write(iout,'(1x,5(1x,f10.5),'' &'')')(threepoly(j,i),j=7,11)
          write(iout,'(11i2)')(itmp1(j),j=1,11)
        else
          write(iout,'(1x,6(1x,f10.5),'' &'')')(threepoly(j,i),j=1,6)
          write(iout,'(1x,5(1x,f10.5))')(threepoly(j,i),j=7,11)
        endif
      elseif (nthrty(i).eq.11) then
        if (mmtexc(i).eq.1) then
          write(iout,'(''bacross bond '',a5,5(1x,a9))') mword(nmtptr),(wboty(j),j=1,nboty)
          write(iout,'(3(a5,1x,a4,1x),g14.6,1x,f8.5,'' &'')') &
            lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),thbk(i),thrho3(i)
          if (lfit) then
            write(iout,'(2(1x,f8.6),1x,f8.4,5i2)')thrho1(i),thrho2(i),the,(itmp(5*(i-1)+j),j=1,5)
          else
            write(iout,'(2(1x,f8.6),1x,f8.4)')thrho1(i),thrho2(i),the
          endif
        else
          write(iout,'(''bacross '',a5,5(1x,a9))') mword(nmtptr),(wboty(j),j=1,nboty)
          write(iout,'(3(a5,1x,a4,1x),g14.6,1x,f8.5,'' &'')') &
            lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),thbk(i),thrho3(i)
          if (lfit) then
            write(iout,'(2(1x,f8.6),1x,f8.4,6(1x,f6.3),5i2)') &
              thrho1(i),thrho2(i),the,thr1min(i),thr1(i),thr2min(i),thr2(i),thr3min(i),thr3(i), &
              (itmp(5*(i-1)+j),j=1,5)
          else
            write(iout,'(2(1x,f8.6),1x,f8.4,6(1x,f6.3))') &
              thrho1(i),thrho2(i),the,thr1min(i),thr1(i),thr2min(i),thr2(i),thr3min(i),thr3(i)
          endif
        endif
      elseif (nthrty(i).eq.12) then
        if (mmtexc(i).eq.1) then
          write(iout,'(''lin3 bond '',a5,5(1x,a9))') mword(nmtptr),(wboty(j),j=1,nboty)
          if (lfit) then
            write(iout,'(3(a5,1x,a4,1x),g14.6,1x,f8.5,2(1x,i2))') &
              lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),thbk(i), &
              theta(i),nint(thrho1(i)),itmp(5*(i-1)+1)
          else
            write(iout,'(3(a5,1x,a4,1x),g14.6,1x,f7.4,1x,i2)') &
              lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),thbk(i),theta(i),nint(thrho1(i))
          endif
        else
          write(iout,'(''lin3 '',a5,5(1x,a9))') mword(nmtptr),(wboty(j),j=1,nboty)
          write(iout,'(3(a5,1x,a4,1x),g14.6,1x,f8.5,1x,i2,'' &'')') &
            lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),thbk(i),theta(i),nint(thrho1(i))
          if (lfit) then
            write(iout,'(6(1x,f8.3),i2)')thr1min(i),thr1(i),thr2min(i),thr2(i),thr3min(i),thr3(i),itmp(5*(i-1)+1)
          else
            write(iout,'(6(1x,f8.3))')thr1min(i),thr1(i),thr2min(i),thr2(i),thr3min(i),thr3(i)
          endif
        endif
      elseif (nthrty(i).eq.13) then
        rho1 = thrho1(i)
        rho2 = thrho2(i)
        if (mmtexc(i).eq.1) then
          write(iout,'(''bcoscross bond '',a5,5(1x,a9))') mword(nmtptr),(wboty(j),j=1,nboty)
          if (lfit) then
            write(iout,'(3(a5,1x,a3,1x),g11.5,1x,f8.5,1x,i1,1x,i2,2(1x,f8.6),4i2)') &
              lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),thbk(i),theta(i),nint(threepoly(1,i)),nint(thrho3(i)), &
              rho1,rho2,itmp(5*(i-1)+1),itmp(5*(i-1)+2),itmp(5*(i-1)+3),itmp(5*(i-1)+4)
          else
            write(iout,'(3(a5,1x,a3,1x),g11.5,1x,f8.5,1x,i1,1x,i2,2(1x,f8.6))') &
              lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),thbk(i),theta(i),nint(threepoly(1,i)),nint(thrho3(i)),rho1,rho2
          endif
        else
          write(iout,'(''bcoscross '',a5,5(1x,a9))') mword(nmtptr),(wboty(j),j=1,nboty)
          write(iout,'(3(a5,1x,a3,1x),g11.5,1x,f8.5,1x,i1,1x,i2,2(1x,f8.6),'' &'')')lab1,ctmp(1), &
            lab2,ctmp(2),lab3,ctmp(3),thbk(i),theta(i),nint(threepoly(1,i)),nint(thrho3(i)),rho1,rho2
          if (lfit) then
            write(iout,'(6(1x,f8.3),3i2)')thr1min(i),thr1(i),thr2min(i),thr2(i),thr3min(i),thr3(i), &
              itmp(5*(i-1)+1),itmp(5*(i-1)+2),itmp(5*(i-1)+3),itmp(5*(i-1)+4)
          else
            write(iout,'(6(1x,f8.3))')thr1min(i),thr1(i),thr2min(i),thr2(i),thr3min(i),thr3(i)
          endif
        endif
      elseif (nthrty(i).eq.14) then
        write(iout,'(''sw3jb '',a5,5(1x,a9))') mword(nmtptr),(wboty(j),j=1,nboty)
        write(iout,'(3(a5,1x,a4,1x),g14.6,1x,f8.4,2(1x,f8.6),'' &'')') &
          lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),thbk(i),the,thrho1(i),thrho2(i)
        if (lfit) then
          write(iout,'(1x,f8.6,6(1x,f8.3),5i2)')  &
            thrho3(i),thr1min(i),thr1(i),thr2min(i),thr2(i),thr3min(i),thr3(i),(itmp(5*(i-1)+j),j=1,5)
        else
          write(iout,'(1x,f8.6,6(1x,f8.3))') thrho3(i),thr1min(i),thr1(i),thr2min(i),thr2(i), &
            thr3min(i),thr3(i)
        endif
      elseif (nthrty(i).eq.15) then
        if (ltdreiding(i)) then
          write(iout,'(''hydrogen-bond dreiding '',3i3,1x,a5,5(1x,a9))') nint(thrho1(i)),nint(thrho2(i)), &
            nint(thrho3(i)),mword(nmtptr),(wboty(j),j=1,nboty)
        else
          write(iout,'(''hydrogen-bond '',3i3,1x,a5,5(1x,a9))') nint(thrho1(i)),nint(thrho2(i)),nint(thrho3(i)), &
            mword(nmtptr),(wboty(j),j=1,nboty)
        endif
        if (lthetataper(i)) then
          write(iout,'(3(a5,1x,a4,1x),g14.6,1x,g14.6,2(1x,f7.3),'' &'')') &
            lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),thbk(i),the, &
            thetatapermin(i)*radtodeg,thetatapermax(i)*radtodeg
        else
          write(iout,'(3(a5,1x,a4,1x),g14.6,1x,g14.6,'' &'')') &
            lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),thbk(i),the
        endif
        if (lfit) then
          write(iout,'(6(1x,f8.3),2i2)')  &
            thr1min(i),thr1(i),thr2min(i),thr2(i),thr3min(i),thr3(i),(itmp(5*(i-1)+j),j=1,2)
        else
          write(iout,'(6(1x,f8.3))') thr1min(i),thr1(i),thr2min(i),thr2(i), &
            thr3min(i),thr3(i)
        endif
      elseif (nthrty(i).eq.16) then
        rho1 = thrho1(i)
        rho2 = thrho2(i)
        if (mmtexc(i).eq.1) then
          if (lfit) then
            write(iout,'(''equatorial bond '',a5,5(1x,a9))') mword(nmtptr),(wboty(j),j=1,nboty)
            write(iout,'(3(a5,1x,a3,1x),g17.6,1x,f8.4,2(1x,f8.6),'' &'')') &
              lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),thbk(i),the,rho1,rho2
            write(iout,'(3i2)') itmp(5*(i-1)+1),itmp(5*(i-1)+3),itmp(5*(i-1)+4)
          else
            write(iout,'(''equatorial bond '',a5,5(1x,a9))') mword(nmtptr),(wboty(j),j=1,nboty)
            write(iout,'(3(a5,1x,a3,1x),g17.6,1x,f8.4,2(1x,f8.6))') &
              lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),thbk(i),the,rho1,rho2
          endif
        else
          if (lfit) then
            write(iout,'(''equatorial '',a5,5(1x,a9))') mword(nmtptr),(wboty(j),j=1,nboty)
            write(iout,'(3(a5,1x,a3,1x),g17.6,1x,f8.4,2(1x,f8.6),'' &'')') &
              lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),thbk(i),the,rho1,rho2
            write(iout,'(6(1x,f8.3),3i2)') thr1min(i),thr1(i),thr2min(i),thr2(i),thr3min(i),thr3(i), &
              itmp(5*(i-1)+1),itmp(5*(i-1)+3),itmp(5*(i-1)+4)
          else
            write(iout,'(''equatorial '',a5,5(1x,a9))') mword(nmtptr),(wboty(j),j=1,nboty)
            write(iout,'(3(a5,1x,a3,1x),g17.6,1x,f8.4,2(1x,f8.6),'' &'')') &
              lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),thbk(i),the,rho1,rho2
            write(iout,'(6(1x,f8.3))') thr1min(i),thr1(i),thr2min(i),thr2(i),thr3min(i),thr3(i)
          endif
        endif
      elseif (nthrty(i).eq.17) then
        if (mmtexc(i).eq.1) then
          if (lfit) then
            write(iout,'(''uff3 bond '',a5,5(1x,a9))') mword(nmtptr),(wboty(j),j=1,nboty)
            write(iout,'(3(a5,1x,a4,1x),g10.5,1x,g10.5,2i2)') &
              lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),thbk(i),the,itmp(5*(i-1)+1),itmp(5*(i-1)+2)
          else
            write(iout,'(''uff3 bond '',a5,5(1x,a9))') mword(nmtptr),(wboty(j),j=1,nboty)
            write(iout,'(3(a5,1x,a4,1x),g10.5,1x,g10.5)') lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),thbk(i),the
          endif
        else
          if (lfit) then
            write(iout,'(''uff3 '',a5,5(1x,a9))') mword(nmtptr),(wboty(j),j=1,nboty)
            write(iout,'(3(a5,1x,a4,1x),g10.5,1x,f10.6,'' &'')') &
              lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),thbk(i),the
            write(iout,'(6(1x,f6.3),2i2)')thr1min(i),thr1(i), &
              thr2min(i),thr2(i),thr3min(i),thr3(i),itmp(5*(i-1)+1),itmp(5*(i-1)+2)
          else
            write(iout,'(''uff3 '',a5,5(1x,a9))') mword(nmtptr),(wboty(j),j=1,nboty)
            write(iout,'(3(a5,1x,a4,1x),g10.5,1x,f10.5,'' &'')') &
              lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),thbk(i),the
            write(iout,'(6(1x,f6.3))')thr1min(i),thr1(i),thr2min(i),thr2(i),thr3min(i),thr3(i)
          endif
        endif
      elseif (nthrty(i).eq.18) then
        if (mmtexc(i).eq.1) then
          write(iout,'(''bacoscross '',a5,5(1x,a9))') mword(nmtptr),(wboty(j),j=1,nboty)
          write(iout,'(3(a5,1x,a4,1x),g14.6,1x,f8.5,'' &'')') &
            lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),thbk(i),thrho3(i)
          if (lfit) then
            write(iout,'(2(1x,f8.6),1x,f8.4,5i2)')thrho1(i),thrho2(i),the,(itmp(5*(i-1)+j),j=1,5)
          else
            write(iout,'(2(1x,f8.6),1x,f8.4)')thrho1(i),thrho2(i),the
          endif
        else
          write(iout,'(''bacoscross '',a5,5(1x,a9))') mword(nmtptr),(wboty(j),j=1,nboty)
          write(iout,'(3(a5,1x,a4,1x),g14.6,1x,f8.5,'' &'')') &
            lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),thbk(i),thrho3(i)
          if (lfit) then
            write(iout,'(2(1x,f8.6),1x,f8.4,6(1x,f6.3),5i2)') &
              thrho1(i),thrho2(i),the,thr1min(i),thr1(i),thr2min(i),thr2(i),thr3min(i),thr3(i), &
              (itmp(5*(i-1)+j),j=1,5)
          else
            write(iout,'(2(1x,f8.6),1x,f8.4,6(1x,f6.3))') &
              thrho1(i),thrho2(i),the,thr1min(i),thr1(i),thr2min(i),thr2(i),thr3min(i),thr3(i)
          endif
        endif
      elseif (nthrty(i).eq.19) then
        if (mmtexc(i).eq.1) then
          write(iout,'(''3coulomb bond '',a5,5(1x,a9))') mword(nmtptr),(wboty(j),j=1,nboty)
          write(iout,'(3(a5,1x,a3,1x),g11.5)') lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),thbk(i)
        else
          write(iout,'(''3coulomb '',a5,5(1x,a9))') mword(nmtptr),(wboty(j),j=1,nboty)
          write(iout,'(3(a5,1x,a3,1x),g11.5,'' &'')') &
            lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),thbk(i)
          write(iout,'(6(1x,f8.3))') thr1min(i),thr1(i),thr2min(i),thr2(i),thr3min(i),thr3(i)
        endif
      elseif (nthrty(i).eq.20) then
        if (mmtexc(i).eq.1) then
          write(iout,'(''exp2 '',a5,5(1x,a9))') mword(nmtptr),(wboty(j),j=1,nboty)
          write(iout,'(3(a5,1x,a4,1x),g14.6,2(1x,f8.5),'' &'')') &
            lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),thbk(i),theta(i),thrho1(i)
          if (lfit) then
            write(iout,'(2(1x,f8.6),1x,5i2)') thrho2(i),thrho3(i),(itmp(5*(i-1)+j),j=1,5)
          else
            write(iout,'(2(1x,f8.6))') thrho2(i),thrho3(i)
          endif
        else
          write(iout,'(''exp2 '',a5,5(1x,a9))') mword(nmtptr),(wboty(j),j=1,nboty)
          write(iout,'(3(a5,1x,a4,1x),g14.6,2(1x,f8.5),'' &'')') &
            lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),thbk(i),theta(i),thrho1(i)
          if (lfit) then
            write(iout,'(2(1x,f8.6),1x,6(1x,f6.3),5i2)') &
              thrho2(i),thrho3(i),thr1min(i),thr1(i),thr2min(i),thr2(i),thr3min(i),thr3(i), &
              (itmp(5*(i-1)+j),j=1,5)
          else
            write(iout,'(2(1x,f8.6),1x,6(1x,f6.3))') &
              thrho2(i),thrho3(i),thr1min(i),thr1(i),thr2min(i),thr2(i),thr3min(i),thr3(i)
          endif
        endif
      elseif (nthrty(i).eq.21) then
        if (mmtexc(i).eq.1) then
          if (lfit) then
            write(iout,'(''g3coulomb bond '',a5,5(1x,a9))') mword(nmtptr),(wboty(j),j=1,nboty)
            write(iout,'(3(a5,1x,a4,1x),g10.5,1x,g10.5,2i2)') &
              lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),thbk(i),the,itmp(5*(i-1)+1),itmp(5*(i-1)+2)
          else
            write(iout,'(''g3coulomb bond '',a5,5(1x,a9))') mword(nmtptr),(wboty(j),j=1,nboty)
            write(iout,'(3(a5,1x,a4,1x),g10.5,1x,g10.5)') lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),thbk(i),the
          endif
        else
          if (lfit) then
            write(iout,'(''g3coulomb '',a5,5(1x,a9))') mword(nmtptr),(wboty(j),j=1,nboty)
            write(iout,'(3(a5,1x,a4,1x),g10.5,1x,f10.6,'' &'')') &
              lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),thbk(i),the
            write(iout,'(6(1x,f6.3),2i2)')thr1min(i),thr1(i), &
              thr2min(i),thr2(i),thr3min(i),thr3(i),itmp(5*(i-1)+1),itmp(5*(i-1)+2)
          else
            write(iout,'(''g3coulomb '',a5,5(1x,a9))') mword(nmtptr),(wboty(j),j=1,nboty)
            write(iout,'(3(a5,1x,a4,1x),g10.5,1x,f10.5,'' &'')') &
              lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),thbk(i),the
            write(iout,'(6(1x,f6.3))')thr1min(i),thr1(i),thr2min(i),thr2(i),thr3min(i),thr3(i)
          endif
        endif
      elseif (nthrty(i).eq.22) then
        if (mmtexc(i).eq.1) then
          write(iout,'(''bagcross bond '',a5,5(1x,a9))') mword(nmtptr),(wboty(j),j=1,nboty)
          write(iout,'(3(a5,1x,a4,1x),g14.6,2(1x,f8.5),'' &'')') &
            lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),thbk(i),thrho1(i),thrho2(i)
          if (lfit) then
            write(iout,'(1x,f8.4,1x,3i3,1x,4i2)') the,(nint(threepoly(k,i)),k=1,3),(itmp(5*(i-1)+j),j=1,4)
          else
            write(iout,'(1x,f8.4,1x,3i3)') the,(nint(threepoly(k,i)),k=1,3)
          endif
        else
          write(iout,'(''bagcross '',a5,5(1x,a9))') mword(nmtptr),(wboty(j),j=1,nboty)
          write(iout,'(3(a5,1x,a4,1x),g14.6,2(1x,f8.5),'' &'')') &
            lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),thbk(i),thrho1(i),thrho2(i)
          if (lfit) then
            write(iout,'(x,f8.4,1x,3i3,6(1x,f6.3),4i2)') &
              the,(nint(threepoly(k,i)),k=1,3),thr1min(i),thr1(i),thr2min(i),thr2(i),thr3min(i),thr3(i), &
              (itmp(5*(i-1)+j),j=1,4)
          else
            write(iout,'(x,f8.4,1x,3i3,6(1x,f6.3))') &
              the,(nint(threepoly(k,i)),k=1,3),thr1min(i),thr1(i),thr2min(i),thr2(i),thr3min(i),thr3(i)
          endif
        endif
      endif
    enddo loop3body
    deallocate(itmp,stat=status)
    if (status/=0) call deallocate_error('dump34','itmp')
  endif
!************************
!  Four-body potentials *
!************************
  if (nforo.gt.0) then
    allocate(itmp(6*nforo),stat=status)
    if (status/=0) call outofmemory('dump34','itmp')
    if (lfit) then
      do i = 1,6*nforo
        itmp(i) = 0
      enddo
      do i = 1,nfit
        if (nftyp(i).eq.4) then
          itmp(6*(nfpot(i)-1)+nfvar(i)) = 1
        endif
      enddo
    endif
!
!  Loop over four-body potentials
!
    loop4body: do i = 1,nforo
!
!  Skip four-body potentials that were generated by rules
!
      if (lgenerated4(i)) cycle loop4body
!
!  Generate species labels
!
      nat1 = nfspec1(i)
      nat2 = nfspec2(i)
      nat3 = nfspec3(i)
      nat4 = nfspec4(i)
      ntype1 = nfptyp1(i)
      ntype2 = nfptyp2(i)
      ntype3 = nfptyp3(i)
      ntype4 = nfptyp4(i)
      if (llibsymdump) then
        lab1 = symbol4(1,i)
        lab2 = symbol4(2,i)
        lab3 = symbol4(3,i)
        lab4 = symbol4(4,i)
      else
        call label(nat1,ntype1,lab1)
        call label(nat2,ntype2,lab2)
        call label(nat3,ntype3,lab3)
        call label(nat4,ntype4,lab4)
      endif
      if (nat1.gt.maxele) then
        ctmp(1) = 'shel'
      else
        ctmp(1) = 'core'
      endif
      if (nat2.gt.maxele) then
        ctmp(2) = 'shel'
      else
        ctmp(2) = 'core'
      endif
      if (nat3.gt.maxele) then
        ctmp(3) = 'shel'
      else
        ctmp(3) = 'core'
      endif
      if (nat4.gt.maxele) then
        ctmp(4) = 'shel'
      else
        ctmp(4) = 'core'
      endif
!
!  Set intra/inter/both pointer
!
      if (lfintra(i).and..not.lfinter(i)) then
        nmfptr = 1
      elseif (lfinter(i).and..not.lfintra(i)) then
        nmfptr = 2
      else
        nmfptr = 3
      endif
!
!  Set bond type words
!
      nboty = 0
      if (n4botype(1,i).gt.0) then
        nboty = nboty + 1
        nbotyptr(nboty) = n4botype(1,i)
      endif
      if (n4botype(2,i).gt.1) then
        nboty = nboty + 1
        nbotyptr(nboty) = 7 + n4botype(2,i)
      endif
      if (lfdreiding(i)) then
        nboty = nboty + 1
        nbotyptr(nboty) = 11
      endif
      if (lonly3oop(i)) then
        nboty = nboty + 1
        nbotyptr(nboty) = 13
      endif
!
      if (nforty(i).eq.1) then
        phi0 = forpoly(1,i)
        if (mmfexc(i).eq.1) then
          if (lfit) then
            write(iout,'(''torsion bond '',a5,3(1x,a9))') mword(nmfptr),(botyword(nbotyptr(j)),j=1,nboty)
            write(iout,'(4(a5,1x,a3,1x),g12.5,1x,i3,1x,g10.5,i2)') &
              lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),lab4,ctmp(4),fork(i),npfor(i),phi0,itmp(6*(i-1)+1)
          else
            write(iout,'(''torsion bond '',a5,3(1x,a9))') mword(nmfptr),(botyword(nbotyptr(j)),j=1,nboty)
            write(iout,'(4(a5,1x,a3,1x),g12.5,1x,i3,1x,g10.5)') &
              lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),lab4,ctmp(4),fork(i),npfor(i),phi0
          endif
        else
          if (lfit) then
            write(iout,'(''torsion '',a5,3(1x,a9))') mword(nmfptr),(botyword(nbotyptr(j)),j=1,nboty)
            write(iout,'(4(a5,1x,a3,1x),g12.5,1x,i3,1x,g10.5,'' &'')') &
              lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),lab4,ctmp(4),fork(i),npfor(i),phi0
            write(iout,'(4f5.2,i2)') for1(i),for2(i),for3(i),for4(i),itmp(6*(i-1)+1)
          else
            write(iout,'(''torsion '',a5,3(1x,a9))') mword(nmfptr),(botyword(nbotyptr(j)),j=1,nboty)
            write(iout,'(4(a5,1x,a3,1x),g12.5,1x,i3,1x,g10.5,'' &'')') &
              lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),lab4,ctmp(4),fork(i),npfor(i),phi0
            write(iout,'(4f5.2)') for1(i),for2(i),for3(i),for4(i)
          endif
        endif
      elseif (nforty(i).eq.2) then
        if (mmfexc(i).eq.1) then
          if (lfit) then
            write(iout,'(''torsion ryckaert '',i1,'' bond '',a5,3(1x,a9))') npfor(i),mword(nmfptr), &
              (botyword(nbotyptr(j)),j=1,nboty)
            write(iout,'(4(a5,1x,a3,1x),g12.5,1x,6i2)') &
              lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),lab4,ctmp(4),fork(i),(itmp(6*(i-1)+j),j=1,npfor(i)+1)
            write(iout,'(5(f12.6,1x))')(forpoly(j,i),j=1,npfor(i))
          else
            write(iout,'(''torsion ryckaert '',i1,'' bond '',a5,3(1x,a9))') npfor(i),mword(nmfptr), &
              (botyword(nbotyptr(j)),j=1,nboty)
            write(iout,'(4(a5,1x,a3,1x),g12.5)') &
              lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),lab4,ctmp(4),fork(i)
            write(iout,'(5(f12.6,1x))')(forpoly(j,i),j=1,npfor(i))
          endif
        else
          if (lfit) then
            write(iout,'(''torsion ryckaert '',i1,1x,a5,3(1x,a9))') npfor(i),mword(nmfptr),(botyword(nbotyptr(j)),j=1,nboty)
            write(iout,'(4(a5,1x,a3,1x),g12.5,1x,3(f8.4,1x),''&'')') &
              lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),lab4,ctmp(4),fork(i),for1(i),for2(i),for3(i)
            write(iout,'(f9.4,1x,6i2)') for4(i),(itmp(6*(i-1)+j),j=1,npfor(i)+1)
            write(iout,'(5(f12.6,1x))') (forpoly(j,i),j=1,npfor(i))
          else
            write(iout,'(''torsion ryckaert '',i1,1x,a5,3(1x,a9))') npfor(i),mword(nmfptr),(botyword(nbotyptr(j)),j=1,nboty)
            write(iout,'(4(a5,1x,a3,1x),g12.5,1x,3(f8.4,1x),''&'')') &
              lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),lab4,ctmp(4),fork(i),for1(i),for2(i),for3(i)
            write(iout,'(f9.4,1x)') for4(i)
            write(iout,'(5(f12.6,1x))') (forpoly(j,i),j=1,npfor(i))
          endif
        endif
      elseif (nforty(i).eq.3) then
        if (mmfexc(i).eq.1) then
          if (lfit) then
            write(iout,'(''outofplane bond '',a5,3(1x,a9))') mword(nmfptr),(botyword(nbotyptr(j)),j=1,nboty)
            write(iout,'(4(a5,1x,a3,1x),2(g12.5,1x),2i2)') &
              lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),lab4,ctmp(4),fork(i),forpoly(1,i),(itmp(6*(i-1)+j),j=1,2)
          else
            write(iout,'(''outofplane bond '',a5,3(1x,a9))') mword(nmfptr),(botyword(nbotyptr(j)),j=1,nboty)
            write(iout,'(4(a5,1x,a3,1x),2(g12.5,1x))') &
              lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),lab4,ctmp(4),fork(i),forpoly(1,i)
          endif
        else
          if (lfit) then
            write(iout,'(''outofplane '',a5,3(1x,a9))') mword(nmfptr),(botyword(nbotyptr(j)),j=1,nboty)
            write(iout,'(4(a5,1x,a3,1x),2(g12.5,1x),'' &'')') &
              lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),lab4,ctmp(4),fork(i),forpoly(1,i)
            write(iout,'(20x,6(f6.3,1x),2i2)') &
              for1min(i),for1(i),for2min(i),for2(i),for3min(i),for3(i),(itmp(6*(i-1)+j),j=1,2)
          else
            write(iout,'(''outofplane '',a5,3(1x,a9))') mword(nmfptr),(botyword(nbotyptr(j)),j=1,nboty)
            write(iout,'(4(a5,1x,a3,1x),2(g12.5,1x),'' &'')') &
              lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),lab4,ctmp(4),fork(i),forpoly(1,i)
            write(iout,'(20x,6(f6.3,1x))') &
              for1min(i),for1(i),for2min(i),for2(i),for3min(i),for3(i)
          endif
        endif
      elseif (nforty(i).eq.4) then
        phi0 = forpoly(1,i)
        if (mmfexc(i).eq.1) then
          if (lfit) then
            write(iout,'(''torsion esff bond '',a5,3(1x,a9))') mword(nmfptr),(botyword(nbotyptr(j)),j=1,nboty)
            write(iout,'(4(a5,1x,a3,1x),2(g12.5,1x),i3,1x,2i2)') &
              lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),lab4,ctmp(4),fork(i),phi0,npfor(i),(itmp(6*(i-1)+j),j=1,2)
          else
            write(iout,'(''torsion esff bond '',a5,3(1x,a9))') mword(nmfptr),(botyword(nbotyptr(j)),j=1,nboty)
            write(iout,'(4(a5,1x,a3,1x),2(g12.5,1x),i3,1x)') &
              lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),lab4,ctmp(4),fork(i),phi0,npfor(i)
          endif
        else
          if (lfit) then
            write(iout,'(''torsion esff '',a5,3(1x,a9))') mword(nmfptr),(botyword(nbotyptr(j)),j=1,nboty)
            write(iout,'(4(a5,1x,a3,1x),2(g12.5,1x),i3,'' &'')') &
              lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),lab4,ctmp(4),fork(i),phi0,npfor(i)
            write(iout,'(4f5.2,i2)') for1(i),for2(i),for3(i),for4(i),(itmp(6*(i-1)+j),j=1,2)
          else
            write(iout,'(''torsion esff '',a5,3(1x,a9))') mword(nmfptr),(botyword(nbotyptr(j)),j=1,nboty)
            write(iout,'(4(a5,1x,a3,1x),2(g12.5,1x),i3,'' &'')') &
              lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),lab4,ctmp(4),fork(i),phi0,npfor(i)
            write(iout,'(4f5.2)') for1(i),for2(i),for3(i),for4(i)
          endif
        endif
      elseif (nforty(i).eq.5) then
        phi0 = forpoly(1,i)
        if (mmfexc(i).eq.1) then
          if (lfit) then
            write(iout,'(''torharm bond '',a5,3(1x,a9))') mword(nmfptr),(botyword(nbotyptr(j)),j=1,nboty)
            write(iout,'(4(a5,1x,a3,1x),2(g12.5,1x),1x,2i2)') &
              lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),lab4,ctmp(4),fork(i),phi0,(itmp(6*(i-1)+j),j=1,2)
          else
            write(iout,'(''torharm bond '',a5,3(1x,a9))') mword(nmfptr),(botyword(nbotyptr(j)),j=1,nboty)
            write(iout,'(4(a5,1x,a3,1x),2(g12.5,1x))') &
              lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),lab4,ctmp(4),fork(i),phi0
          endif
        else
          if (lfit) then
            write(iout,'(''torharm '',a5,3(1x,a9))') mword(nmfptr),(botyword(nbotyptr(j)),j=1,nboty)
            write(iout,'(4(a5,1x,a3,1x),2(g12.5,1x),'' &'')') &
              lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),lab4,ctmp(4),fork(i),phi0
            write(iout,'(4f5.2,2i2)')for1(i),for2(i),for3(i),for4(i),(itmp(6*(i-1)+j),j=1,2)
          else
            write(iout,'(''torharm '',a5,3(1x,a9))') mword(nmfptr),(botyword(nbotyptr(j)),j=1,nboty)
            write(iout,'(4(a5,1x,a3,1x),2(g12.5,1x),'' &'')') &
              lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),lab4,ctmp(4),fork(i),phi0
            write(iout,'(4f5.2)')for1(i),for2(i),for3(i),for4(i)
          endif
        endif
      elseif (nforty(i).eq.6) then
        phi0 = forpoly(1,i)
        if (mmfexc(i).eq.1) then
          if (lfit) then
            write(iout,'(''torexp bond '',a5,3(1x,a9))') mword(nmfptr),(botyword(nbotyptr(j)),j=1,nboty)
            write(iout,'(4(a5,1x,a3,1x),g12.5,1x,i3,1x,g10.5,'' &'')') &
              lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),lab4,ctmp(4),fork(i),npfor(i),phi0
            write(iout,'(2x,3(g10.6,1x),1x,4i2)') forpoly(2,i),forpoly(3,i),forpoly(4,i),itmp(6*(i-1)+1),(itmp(6*(i-1)+j),j=3,5)
          else
            write(iout,'(''torexp bond '',a5,3(1x,a9))') mword(nmfptr),(botyword(nbotyptr(j)),j=1,nboty)
            write(iout,'(4(a5,1x,a3,1x),g12.5,1x,i3,1x,g10.5,'' &'')') &
              lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),lab4,ctmp(4),fork(i),npfor(i),phi0
            write(iout,'(2x,3(f10.6,1x))') forpoly(2,i),forpoly(3,i),forpoly(4,i)
          endif
        else
          if (lfit) then
            write(iout,'(''torexp '',a5,3(1x,a9))') mword(nmfptr),(botyword(nbotyptr(j)),j=1,nboty)
            write(iout,'(4(a5,1x,a3,1x),g12.5,1x,i3,1x,g10.5,'' &'')') &
              lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),lab4,ctmp(4),fork(i),npfor(i),phi0
            write(iout,'(2x,3(f10.6,1x),4f5.2,1x,4i2)') forpoly(2,i),forpoly(3,i),forpoly(4,i), &
              for1(i),for2(i),for3(i),for4(i),itmp(6*(i-1)+1),(itmp(6*(i-1)+j),j=3,5)
          else
            write(iout,'(''torexp '',a5,3(1x,a9))') mword(nmfptr),(botyword(nbotyptr(j)),j=1,nboty)
            write(iout,'(4(a5,1x,a3,1x),g12.5,1x,i3,1x,g10.5,'' &'')') &
              lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),lab4,ctmp(4),fork(i),npfor(i),phi0
            write(iout,'(2x,3(f10.6,1x),4f5.2)') forpoly(2,i),forpoly(3,i),forpoly(4,i),for1(i),for2(i),for3(i),for4(i)
          endif
        endif
      elseif (nforty(i).eq.7) then
        if (mmfexc(i).eq.1) then
          if (lfit) then
            write(iout,'(''torexp bond esff '',a5,3(1x,a9))') mword(nmfptr),(botyword(nbotyptr(j)),j=1,nboty)
            write(iout,'(4(a5,1x,a3,1x),g12.5,1x,g12.5,i3,1x,'' &'')') &
              lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),lab4,ctmp(4),fork(i),forpoly(1,i),npfor(i)
            write(iout,'(2x,3(g10.6,1x),1x,5i2)') forpoly(2,i),forpoly(3,i),forpoly(4,i),(itmp(6*(i-1)+j),j=1,5)
          else
            write(iout,'(''torexp bond esff '',a5,3(1x,a9))') mword(nmfptr),(botyword(nbotyptr(j)),j=1,nboty)
            write(iout,'(4(a5,1x,a3,1x),g12.5,1x,g12.5,i3,1x,'' &'')') &
              lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),lab4,ctmp(4),fork(i),forpoly(1,i),npfor(i)
            write(iout,'(2x,3(f10.6,1x))') forpoly(2,i),forpoly(3,i),forpoly(4,i)
          endif
        else
          if (lfit) then
            write(iout,'(''torexp esff '',a5,3(1x,a9))') mword(nmfptr),(botyword(nbotyptr(j)),j=1,nboty)
            write(iout,'(4(a5,1x,a3,1x),g12.5,1x,g12.5,i3,1x,'' &'')') &
              lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),lab4,ctmp(4),fork(i),forpoly(1,i),npfor(i)
            write(iout,'(2x,3(f10.6,1x),4f5.2,1x,5i2)') forpoly(2,i),forpoly(3,i),forpoly(4,i), &
              for1(i),for2(i),for3(i),for4(i),(itmp(6*(i-1)+j),j=1,5)
          else
            write(iout,'(''torexp esff '',a5,3(1x,a9))') mword(nmfptr),(botyword(nbotyptr(j)),j=1,nboty)
            write(iout,'(4(a5,1x,a3,1x),g12.5,1x,g12.5,i3,1x,'' &'')') &
              lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),lab4,ctmp(4),fork(i),forpoly(1,i),npfor(i)
            write(iout,'(2x,3(f10.6,1x),4f5.2)') forpoly(2,i),forpoly(3,i),forpoly(4,i),for1(i),for2(i),for3(i),for4(i)
          endif
        endif
      elseif (nforty(i).eq.8) then
        phi0 = forpoly(1,i)
        if (mmfexc(i).eq.1) then
          if (lfit) then
            write(iout,'(''tortaper bond '',a5,3(1x,a9))') mword(nmfptr),(botyword(nbotyptr(j)),j=1,nboty)
            write(iout,'(4(a5,1x,a3,1x),g12.5,1x,i3,1x,g10.5,'' &'')') &
              lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),lab4,ctmp(4),fork(i),npfor(i),phi0
            write(iout,'(2x,g10.6,1x,4i2)') forpoly(2,i),itmp(6*(i-1)+1),(itmp(6*(i-1)+j),j=3,5)
          else
            write(iout,'(''tortaper bond '',a5,3(1x,a9))') mword(nmfptr),(botyword(nbotyptr(j)),j=1,nboty)
            write(iout,'(4(a5,1x,a3,1x),g12.5,1x,i3,1x,g10.5,'' &'')') &
              lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),lab4,ctmp(4),fork(i),npfor(i),phi0
            write(iout,'(2x,f10.6)') forpoly(2,i)
          endif
        else
          if (lfit) then
            write(iout,'(''tortaper '',a5,3(1x,a9))') mword(nmfptr),(botyword(nbotyptr(j)),j=1,nboty)
            write(iout,'(4(a5,1x,a3,1x),g12.5,1x,i3,1x,g10.5,'' &'')') &
              lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),lab4,ctmp(4),fork(i),npfor(i),phi0
            write(iout,'(2x,f10.6,1x,4f5.2,1x,4i2)') forpoly(2,i), &
              for1(i),for2(i),for3(i),for4(i),itmp(6*(i-1)+1),(itmp(6*(i-1)+j),j=3,5)
          else
            write(iout,'(''tortaper '',a5,3(1x,a9))') mword(nmfptr),(botyword(nbotyptr(j)),j=1,nboty)
            write(iout,'(4(a5,1x,a3,1x),g12.5,1x,i3,1x,g10.5,'' &'')') &
              lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),lab4,ctmp(4),fork(i),npfor(i),phi0
            write(iout,'(2x,f10.6,1x,4f5.2)') forpoly(2,i),for1(i),for2(i),for3(i),for4(i)
          endif
        endif
      elseif (nforty(i).eq.9) then
        if (mmfexc(i).eq.1) then
          if (lfit) then
            write(iout,'(''tortaper bond esff '',a5,3(1x,a9))') mword(nmfptr),(botyword(nbotyptr(j)),j=1,nboty)
            write(iout,'(4(a5,1x,a3,1x),g12.5,1x,g12.5,i3,1x,'' &'')') &
              lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),lab4,ctmp(4),fork(i),forpoly(1,i),npfor(i)
            write(iout,'(2x,g10.6,1x,1x,5i2)') forpoly(2,i),(itmp(6*(i-1)+j),j=1,5)
          else
            write(iout,'(''tortaper bond esff '',a5,3(1x,a9))') mword(nmfptr),(botyword(nbotyptr(j)),j=1,nboty)
            write(iout,'(4(a5,1x,a3,1x),g12.5,1x,g12.5,i3,1x,'' &'')') &
              lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),lab4,ctmp(4),fork(i),forpoly(1,i),npfor(i)
            write(iout,'(2x,f10.6,1x)') forpoly(2,i)
          endif
        else
          if (lfit) then
            write(iout,'(''tortaper esff '',a5,3(1x,a9))') mword(nmfptr),(botyword(nbotyptr(j)),j=1,nboty)
            write(iout,'(4(a5,1x,a3,1x),g12.5,1x,g12.5,i3,1x,'' &'')') &
              lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),lab4,ctmp(4),fork(i),forpoly(1,i),npfor(i)
            write(iout,'(2x,f10.6,1x,4f5.2,1x,5i2)') forpoly(2,i), &
              for1(i),for2(i),for3(i),for4(i),(itmp(6*(i-1)+j),j=1,5)
          else
            write(iout,'(''tortaper esff '',a5,3(1x,a9))') mword(nmfptr),(botyword(nbotyptr(j)),j=1,nboty)
            write(iout,'(4(a5,1x,a3,1x),g12.5,1x,g12.5,i3,1x,'' &'')') &
              lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),lab4,ctmp(4),fork(i),forpoly(1,i),npfor(i)
            write(iout,'(2x,f10.6,1x,4f5.2)') forpoly(2,i),for1(i),for2(i),for3(i),for4(i)
          endif
        endif
      elseif (nforty(i).eq.10) then
        if (mmfexc(i).eq.1) then
          if (lfit) then
            write(iout,'(''torangle bond '',a5,3(1x,a9))') mword(nmfptr),(botyword(nbotyptr(j)),j=1,nboty)
            write(iout,'(4(a5,1x,a3,1x),3(g10.5,1x),1x,3i2)') &
              lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),lab4,ctmp(4),fork(i),forpoly(1,i),forpoly(2,i), &
              (itmp(6*(i-1)+j),j=1,3)
          else
            write(iout,'(''torangle bond '',a5,3(1x,a9))') mword(nmfptr),(botyword(nbotyptr(j)),j=1,nboty)
            write(iout,'(4(a5,1x,a3,1x),3(g10.5,1x))') &
              lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),lab4,ctmp(4),fork(i),forpoly(1,i),forpoly(2,i)
          endif
        else
          if (lfit) then
            write(iout,'(''torangle '',a5,3(1x,a9))') mword(nmfptr),(botyword(nbotyptr(j)),j=1,nboty)
            write(iout,'(4(a5,1x,a3,1x),3(g10.5,1x),'' &'')') &
              lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),lab4,ctmp(4),fork(i),forpoly(1,i),forpoly(2,i)
            write(iout,'(4f5.2,3i2)') for1(i),for2(i),for3(i),for4(i),(itmp(6*(i-1)+j),j=1,3)
          else
            write(iout,'(''torangle '',a5,3(1x,a9))') mword(nmfptr),(botyword(nbotyptr(j)),j=1,nboty)
            write(iout,'(4(a5,1x,a3,1x),3(g10.5,1x),'' &'')') &
              lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),lab4,ctmp(4),fork(i),forpoly(1,i),forpoly(2,i)
            write(iout,'(4f5.2)') for1(i),for2(i),for3(i),for4(i)
          endif
        endif
      elseif (nforty(i).eq.11) then
        if (mmfexc(i).eq.1) then
          if (lfit) then
            write(iout,'(''inversion bond '',a5,3(1x,a9))') mword(nmfptr),(botyword(nbotyptr(j)),j=1,nboty)
            write(iout,'(4(a5,1x,a3,1x),g10.5,1x,i2)') &
              lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),lab4,ctmp(4),fork(i),itmp(6*(i-1)+1)
          else
            write(iout,'(''inversion bond '',a5,3(1x,a9))') mword(nmfptr),(botyword(nbotyptr(j)),j=1,nboty)
            write(iout,'(4(a5,1x,a3,1x),g10.5)') &
              lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),lab4,ctmp(4),fork(i)
          endif
        else
          if (lfit) then
            write(iout,'(''inversion '',a5,3(1x,a9))') mword(nmfptr),(botyword(nbotyptr(j)),j=1,nboty)
            write(iout,'(4(a5,1x,a3,1x),g10.5,1x,3f6.3,i2)') &
              lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),lab4, &
              ctmp(4),fork(i),for1(i),for2(i),for3(i),itmp(6*(i-1)+1)
          else
            write(iout,'(''inversion '',a5,3(1x,a9))') mword(nmfptr),(botyword(nbotyptr(j)),j=1,nboty)
            write(iout,'(4(a5,1x,a3,1x),g10.5,1x,3(f6.3,1x))') &
              lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),lab4,ctmp(4),fork(i),for1(i),for2(i),for3(i)
          endif
        endif
      elseif (nforty(i).eq.12) then
        if (mmfexc(i).eq.1) then
          if (lfit) then
            write(iout,'(''inversion squared bond '',a5,3(1x,a9))') mword(nmfptr),(botyword(nbotyptr(j)),j=1,nboty)
            write(iout,'(4(a5,1x,a3,1x),2(g10.5,1x),i2)') &
              lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),lab4,ctmp(4),fork(i),forpoly(1,i)*radtodeg,itmp(6*(i-1)+1)
          else
            write(iout,'(''inversion squared bond '',a5,3(1x,a9))') mword(nmfptr),(botyword(nbotyptr(j)),j=1,nboty)
            write(iout,'(4(a5,1x,a3,1x),2(g10.5,1x))') &
              lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),lab4,ctmp(4),fork(i),forpoly(1,i)*radtodeg
          endif
        else 
          if (lfit) then
            write(iout,'(''inversion squared '',a5,3(1x,a9))') mword(nmfptr),(botyword(nbotyptr(j)),j=1,nboty)
            write(iout,'(4(a5,1x,a3,1x),2(g10.5,1x),3f6.3,i2)') &
              lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),lab4, &
              ctmp(4),fork(i),forpoly(1,i)*radtodeg,for1(i),for2(i),for3(i),itmp(6*(i-1)+1)
          else
            write(iout,'(''inversion squared '',a5,3(1x,a9))') mword(nmfptr),(botyword(nbotyptr(j)),j=1,nboty)
            write(iout,'(4(a5,1x,a3,1x),2(g10.5,1x),3(f6.3,1x))') &
              lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),lab4,ctmp(4),fork(i),forpoly(1,i)*radtodeg,for1(i),for2(i),for3(i)
          endif
        endif
      elseif (nforty(i).eq.13) then
        phi0 = forpoly(1,i)
        if (mmfexc(i).eq.1) then
          if (lfit) then
            write(iout,'(''uff4 bond '',a5,3(1x,a9))') mword(nmfptr),(botyword(nbotyptr(j)),j=1,nboty)
            write(iout,'(4(a5,1x,a3,1x),g10.5,1x,i3,1x,g10.5,i2)') &
              lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),lab4,ctmp(4),fork(i),npfor(i),phi0,itmp(6*(i-1)+1)
          else
            write(iout,'(''uff4 bond '',a5,3(1x,a9))') mword(nmfptr),(botyword(nbotyptr(j)),j=1,nboty)
            write(iout,'(4(a5,1x,a3,1x),g10.5,1x,i3,1x,g10.5)') &
              lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),lab4,ctmp(4),fork(i),npfor(i),phi0
          endif
        else
          if (lfit) then
            write(iout,'(''uff4 '',a5,3(1x,a9))') mword(nmfptr),(botyword(nbotyptr(j)),j=1,nboty)
            write(iout,'(4(a5,1x,a3,1x),g10.5,1x,i3,1x,g10.5,'' &'')') &
              lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),lab4,ctmp(4),fork(i),npfor(i),phi0
            write(iout,'(4f5.2,i2)') for1(i),for2(i),for3(i),for4(i),itmp(6*(i-1)+1)
          else
            write(iout,'(''uff4 '',a5,3(1x,a9))') mword(nmfptr),(botyword(nbotyptr(j)),j=1,nboty)
            write(iout,'(4(a5,1x,a3,1x),g10.5,1x,i3,1x,g10.5,'' &'')') &
              lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),lab4,ctmp(4),fork(i),npfor(i),phi0
            write(iout,'(4f5.2)') for1(i),for2(i),for3(i),for4(i)
          endif
        endif
      elseif (nforty(i).eq.14) then
        if (mmfexc(i).eq.1) then
          if (lfit) then
            write(iout,'(''xangleangle bond '',a5,3(1x,a9))') mword(nmfptr),(botyword(nbotyptr(j)),j=1,nboty)
            write(iout,'(4(a5,1x,a3,1x),3(g10.5,1x),'' &'')') &
              lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),lab4,ctmp(4),fork(i),forpoly(1,i),forpoly(2,i)
            write(iout,'(4x,3(f8.4,1x),6i2)') &
              forpoly(3,i)*radtodeg,forpoly(4,i)*radtodeg,forpoly(5,i)*radtodeg,(itmp(6*(i-1)+j),j=1,6)
          else
            write(iout,'(''xangleangle bond '',a5,3(1x,a9))') mword(nmfptr),(botyword(nbotyptr(j)),j=1,nboty)
            write(iout,'(4(a5,1x,a3,1x),3(g10.5,1x),'' &'')') &
              lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),lab4,ctmp(4),fork(i),forpoly(1,i),forpoly(2,i)
            write(iout,'(4x,3(f8.4,1x))') &
              forpoly(3,i)*radtodeg,forpoly(4,i)*radtodeg,forpoly(5,i)*radtodeg
          endif
        else
          if (lfit) then
            write(iout,'(''xangleangle '',a5,3(1x,a9))') mword(nmfptr),(botyword(nbotyptr(j)),j=1,nboty)
            write(iout,'(4(a5,1x,a3,1x),3(g10.5,1x),'' &'')') &
              lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),lab4,ctmp(4),fork(i),forpoly(1,i),forpoly(2,i)
            write(iout,'(4x,3(f8.4,1x),3(f6.3,1x),6i2)') &
              forpoly(3,i)*radtodeg,forpoly(4,i)*radtodeg,forpoly(5,i)*radtodeg,for1(i),for2(i),for3(i), &
              (itmp(6*(i-1)+j),j=1,6)
          else
            write(iout,'(''xangleangle '',a5,3(1x,a9))') mword(nmfptr),(botyword(nbotyptr(j)),j=1,nboty)
            write(iout,'(4(a5,1x,a3,1x),3(g10.5,1x),'' &'')') &
              lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),lab4,ctmp(4),fork(i),forpoly(1,i),forpoly(2,i)
            write(iout,'(4x,3(f8.4,1x),3(f6.3,1x))') &
              forpoly(3,i)*radtodeg,forpoly(4,i)*radtodeg,forpoly(5,i)*radtodeg,for1(i),for2(i),for3(i)
          endif
        endif
      elseif (nforty(i).eq.15) then
        if (mmfexc(i).eq.1) then
          if (lfit) then
            write(iout,'(''uffoop bond '',a5,3(1x,a9))') mword(nmfptr),(botyword(nbotyptr(j)),j=1,nboty)
            write(iout,'(4(a5,1x,a3,1x),g10.5,1x,3(f6.3,1x),4i2)') &
              lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),lab4,ctmp(4),fork(i),(forpoly(k,i),k=1,3), &
              (itmp(6*(i-1)+j),j=1,4)
          else
            write(iout,'(''uffoop bond '',a5,3(1x,a9))') mword(nmfptr),(botyword(nbotyptr(j)),j=1,nboty)
            write(iout,'(4(a5,1x,a3,1x),g10.5,1x,3(f6.3,1x))') &
              lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),lab4,ctmp(4),fork(i),(forpoly(k,i),k=1,3)
          endif
        else
          write(iout,'(''uffoop '',a5,3(1x,a9))') mword(nmfptr),(botyword(nbotyptr(j)),j=1,nboty)
          write(iout,'(4(a5,1x,a3,1x),g10.5,1x,3(f6.3,1x),'' &'')') &
            lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),lab4,ctmp(4),fork(i),(forpoly(k,i),k=1,3)
          if (lfit) then
            write(iout,'(40x,3f6.3,4i2)') for1(i),for2(i),for3(i),(itmp(6*(i-1)+j),j=1,4)
          else
            write(iout,'(40x,3f6.3)') for1(i),for2(i),for3(i)
          endif
        endif
      elseif (nforty(i).eq.16) then
        if (mmfexc(i).eq.1) then
          if (lfit) then
            write(iout,'(''xcosangleangle bond '',a5,3(1x,a9))') mword(nmfptr),(botyword(nbotyptr(j)),j=1,nboty)
            write(iout,'(4(a5,1x,a3,1x),3(g10.5,1x),'' &'')') &
              lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),lab4,ctmp(4),fork(i),forpoly(1,i),forpoly(2,i)
            write(iout,'(4x,3(f8.4,1x),6i2)') &
              forpoly(3,i)*radtodeg,forpoly(4,i)*radtodeg,forpoly(5,i)*radtodeg,(itmp(6*(i-1)+j),j=1,6)
          else
            write(iout,'(''xcosangleangle bond '',a5,3(1x,a9))') mword(nmfptr),(botyword(nbotyptr(j)),j=1,nboty)
            write(iout,'(4(a5,1x,a3,1x),3(g10.5,1x),'' &'')') &
              lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),lab4,ctmp(4),fork(i),forpoly(1,i),forpoly(2,i)
            write(iout,'(4x,3(f8.4,1x))') &
              forpoly(3,i)*radtodeg,forpoly(4,i)*radtodeg,forpoly(5,i)*radtodeg
          endif
        else
          if (lfit) then
            write(iout,'(''xcosangleangle '',a5,3(1x,a9))') mword(nmfptr),(botyword(nbotyptr(j)),j=1,nboty)
            write(iout,'(4(a5,1x,a3,1x),3(g10.5,1x),'' &'')') &
              lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),lab4,ctmp(4),fork(i),forpoly(1,i),forpoly(2,i)
            write(iout,'(4x,3(f8.4,1x),3(f6.3,1x),6i2)') &
              forpoly(3,i)*radtodeg,forpoly(4,i)*radtodeg,forpoly(5,i)*radtodeg,for1(i),for2(i),for3(i), &
              (itmp(6*(i-1)+j),j=1,6)
          else
            write(iout,'(''xcosangleangle '',a5,3(1x,a9))') mword(nmfptr),(botyword(nbotyptr(j)),j=1,nboty)
            write(iout,'(4(a5,1x,a3,1x),3(g10.5,1x),'' &'')') &
              lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),lab4,ctmp(4),fork(i),forpoly(1,i),forpoly(2,i)
            write(iout,'(4x,3(f8.4,1x),3(f6.3,1x))') &
              forpoly(3,i)*radtodeg,forpoly(4,i)*radtodeg,forpoly(5,i)*radtodeg,for1(i),for2(i),for3(i)
          endif
        endif
      elseif (nforty(i).eq.17) then
        if (mmfexc(i).eq.1) then
          if (lfit) then
            write(iout,'(''torcosangle bond '',a5,3(1x,a9))') mword(nmfptr),(botyword(nbotyptr(j)),j=1,nboty)
            write(iout,'(4(a5,1x,a3,1x),3(g10.5,1x),1x,3i2)') &
              lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),lab4,ctmp(4),fork(i),forpoly(1,i),forpoly(2,i), &
              (itmp(6*(i-1)+j),j=1,3)
          else
            write(iout,'(''torcosangle bond '',a5,3(1x,a9))') mword(nmfptr),(botyword(nbotyptr(j)),j=1,nboty)
            write(iout,'(4(a5,1x,a3,1x),3(g10.5,1x))') &
              lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),lab4,ctmp(4),fork(i),forpoly(1,i),forpoly(2,i)
          endif
        else
          if (lfit) then
            write(iout,'(''torcosangle '',a5,3(1x,a9))') mword(nmfptr),(botyword(nbotyptr(j)),j=1,nboty)
            write(iout,'(4(a5,1x,a3,1x),3(g10.5,1x),'' &'')') &
              lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),lab4,ctmp(4),fork(i),forpoly(1,i),forpoly(2,i)
            write(iout,'(4f5.2,3i2)') for1(i),for2(i),for3(i),for4(i),(itmp(6*(i-1)+j),j=1,3)
          else
            write(iout,'(''torcosangle '',a5,3(1x,a9))') mword(nmfptr),(botyword(nbotyptr(j)),j=1,nboty)
            write(iout,'(4(a5,1x,a3,1x),3(g10.5,1x),'' &'')') &
              lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),lab4,ctmp(4),fork(i),forpoly(1,i),forpoly(2,i)
            write(iout,'(4f5.2)') for1(i),for2(i),for3(i),for4(i)
          endif
        endif
      endif
    enddo loop4body
    deallocate(itmp,stat=status)
    if (status/=0) call deallocate_error('dump34','itmp')
  endif
!***********************
!  Six-body potentials *
!***********************
  if (nsixo.gt.0) then
    allocate(itmp(4*nsixo),stat=status)
    if (status/=0) call outofmemory('dump34','itmp')
    if (lfit) then
      do i = 1,4*nsixo
        itmp(i) = 0
      enddo
      do i = 1,nfit
        if (nftyp(i).eq.7) then
          itmp(4*(nfpot(i)-1)+nfvar(i)) = 1
        endif
      enddo
    endif
    do i = 1,nsixo
!         
!  Generate species labels
!           
      nat1 = nsspec1(i)
      nat2 = nsspec2(i)
      nat3 = nsspec3(i)
      nat4 = nsspec4(i)
      nat5 = nsspec5(i)
      nat6 = nsspec6(i)
      ntype1 = nsptyp1(i)
      ntype2 = nsptyp2(i)
      ntype3 = nsptyp3(i)
      ntype4 = nsptyp4(i)
      ntype5 = nsptyp5(i)
      ntype6 = nsptyp6(i)
      if (llibsymdump) then
        lab1 = symbol6(1,i)
        lab2 = symbol6(2,i)
        lab3 = symbol6(3,i)
        lab4 = symbol6(4,i)
        lab5 = symbol6(5,i)
        lab6 = symbol6(6,i)
      else
        call label(nat1,ntype1,lab1)
        call label(nat2,ntype2,lab2)
        call label(nat3,ntype3,lab3)
        call label(nat4,ntype4,lab4) 
        call label(nat5,ntype5,lab5)
        call label(nat6,ntype6,lab6) 
      endif
      if (nat1.gt.maxele) then
        ctmp(1) = 'shel'
      else  
        ctmp(1) = 'core'
      endif 
      if (nat2.gt.maxele) then
        ctmp(2) = 'shel'
      else  
        ctmp(2) = 'core'
      endif
      if (nat3.gt.maxele) then
        ctmp(3) = 'shel'
      else
        ctmp(3) = 'core'
      endif 
      if (nat4.gt.maxele) then
        ctmp(4) = 'shel'
      else
        ctmp(4) = 'core'
      endif
      if (nat5.gt.maxele) then
        ctmp(5) = 'shel'
      else
        ctmp(5) = 'core'
      endif
      if (nat6.gt.maxele) then
        ctmp(6) = 'shel'
      else
        ctmp(6) = 'core'
      endif
!
!  Set intra/inter/both pointer
!
      if (lsintra(i).and..not.lsinter(i)) then
        nmsptr = 1
      elseif (lsinter(i).and..not.lsintra(i)) then
        nmsptr = 2
      else
        nmsptr = 3
      endif
!
!  Set bond type words
!
      nboty = 0
      if (n6botype(1,i).gt.0) then
        nboty = nboty + 1
        nbotyptr(nboty) = n6botype(1,i)
      endif
      if (n6botype(2,i).gt.1) then
        nboty = nboty + 1
        nbotyptr(nboty) = 7 + n6botype(2,i)
      endif
!
      if (nsixty(i).eq.1) then
        if (mmsexc(i).eq.1) then
          if (lfit) then
            write(iout,'(''xoutofplane bond '',a5,2(1x,a9))') mword(nmsptr),(botyword(nbotyptr(j)),j=1,nboty)
            write(iout,'(6(a5,1x,a3,1x),g15.6,1x,i2)') &
              lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),lab4,ctmp(4),lab5,ctmp(5),lab6,ctmp(6),sixk(i),itmp(6*(i-1)+1)
          else
            write(iout,'(''xoutofplane bond '',a5,2(1x,a9))') mword(nmsptr),(botyword(nbotyptr(j)),j=1,nboty)
            write(iout,'(6(a5,1x,a3,1x),g15.6,1x)') &
              lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),lab4,ctmp(4),lab5,ctmp(5),lab6,ctmp(6),sixk(i)
          endif
        else
          if (lfit) then
            write(iout,'(''xoutofplane '',a5,2(1x,a9))') mword(nmsptr),(botyword(nbotyptr(j)),j=1,nboty)
            write(iout,'(6(a5,1x,a3,1x),g15.6,1x,''&'')') &
              lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),lab4,ctmp(4),lab5,ctmp(5),lab6,ctmp(6),sixk(i)
            write(iout,'(5(f8.4,1x),i2)') &
              six1(i),six2(i),six3(i),six4(i),six5(i),itmp(6*(i-1)+1)
          else
            write(iout,'(''xoutofplane '',a5,2(1x,a9))') mword(nmsptr),(botyword(nbotyptr(j)),j=1,nboty)
            write(iout,'(6(a5,1x,a3,1x),g15.6,1x,''&'')') &
              lab1,ctmp(1),lab2,ctmp(2),lab3,ctmp(3),lab4,ctmp(4),lab5,ctmp(5),lab6,ctmp(6),sixk(i)
            write(iout,'(5(f8.4,1x))') &
              six1(i),six2(i),six3(i),six4(i),six5(i)
          endif
        endif
      endif
    enddo
    deallocate(itmp,stat=status)
    if (status/=0) call deallocate_error('dump34','itmp')
  endif
!
  return
  end
