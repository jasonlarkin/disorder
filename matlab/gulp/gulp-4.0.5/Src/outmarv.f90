  subroutine outmarv(iout)
!
!  Write out marvin input file
!
!   3/98 Element mass and covalent radius are now passed to Marvin
!   5/99 Outputing of Lennard-Jones/ESFF enabled
!   4/01 Modified to allow for surface output
!  10/01 icentfct moved to module and no longer needed here since energycfg
!        is now already scaled
!   2/06 Rmin added for Morse
!   5/06 Mass for individual species substituted for atmass
!  11/06 Format statement cleaned up
!   6/07 lall set to false in calls to setup to avoid
!        potential recursive call issue
!   1/09 Arrays apot, bpot, cpot & dpot replaced by new 2-D array twopot
!   6/09 Module name changed from three to m_three
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
  use current
  use element
  use files
  use four
  use general
  use m_three
  use molecule
  use shell
  use species
  use symmetry
  use two
  implicit none
!
!  Passed variables
!
  integer(i4)        :: iout
!
!  Local variables
!
  character(len=5)   :: lab1
  character(len=5)   :: lab2
  character(len=5)   :: lab3
  character(len=5)   :: lab4
  character(len=80)  :: line
  character(len=6)   :: mmword
  character(len=14)  :: mword(2)
  character(len=4)   :: stype(2)
  integer(i4)        :: i
  integer(i4)        :: ifail
  integer(i4)        :: ii
  integer(i4)        :: isign
  integer(i4)        :: j
  integer(i4)        :: jj
  integer(i4)        :: k
  integer(i4)        :: kk
  integer(i4)        :: mpt
  integer(i4)        :: nat1
  integer(i4)        :: nat2
  integer(i4)        :: nat3
  integer(i4)        :: nat4
  integer(i4)        :: nc
  integer(i4)        :: ni
  integer(i4)        :: nmptr
  integer(i4)        :: npl
  integer(i4)        :: npt
  integer(i4)        :: npt1
  integer(i4)        :: npt2
  integer(i4)        :: npt3
  integer(i4)        :: nptr1
  integer(i4)        :: nptr2
  integer(i4)        :: nptr3
  integer(i4)        :: nptr4
  integer(i4)        :: nr
  integer(i4)        :: ns1
  integer(i4)        :: ns2
  integer(i4)        :: ns3
  integer(i4)        :: ntp
  integer(i4)        :: nts1
  integer(i4)        :: nts2
  integer(i4)        :: nts3
  integer(i4)        :: ntype1
  integer(i4)        :: ntype2
  integer(i4)        :: ntype3
  integer(i4)        :: ntype4
  real(dp)           :: diff
  real(dp)           :: rrl(6)
  real(dp)           :: rvf(3,3)
  real(dp)           :: rvi(3,3)
  real(dp)           :: rvt(3,3)
  real(dp)           :: the
  real(dp)           :: xt(3)
  real(dp)           :: xcl
  real(dp)           :: ycl
  real(dp)           :: zcl
  real(dp)           :: xfi
  real(dp)           :: yfi
  real(dp)           :: zfi
  real(dp)           :: zero
!
  data stype/'core','shel'/
  data mword/'intramolecular','intermolecular'/
!
!  If marvfile name has been given then open file
!
  if (marvfile(1:1).ne.' ') then
    open(iout,file=marvfile,status='unknown')
  endif
!**********************************************************************
!  Write out control line
!**********************************************************************
  write(iout,'(''#! marvin'')')
!
!  Write out title line
!
  if (ntitle.gt.0) then
    write(iout,'(/,''title'')')
    write(iout,'(a80)')titleword(1)
  endif
  write(iout,'(/,''surface'')')
!
!  Molecule option
!
  if (lmol) then
    write(iout,'(/,''build'')')
    if (.not.lmolq) write(iout,'(''exclude'')')
  endif
!*****************************
!  Loop over configurations  *
!*****************************
!
!  For MARVIN return the full cell info
!
  do nc = 1,ncfg
    if (ndimen(nc).eq.3) then
!
!  Bulk output to input
!
      ncf = nc
      call setup(.false.)
      do i = 1,3
        rvf(1,i) = rvcfg(1,i,ncf)
        rvf(2,i) = rvcfg(2,i,ncf)
        rvf(3,i) = rvcfg(3,i,ncf)
        rv(1,i) = rvf(1,i)
        rv(2,i) = rvf(2,i)
        rv(3,i) = rvf(3,i)
      enddo
      if (ncbl.gt.1) then
        call uncentre(rv)
      endif
!
!  Crystal structure info first
!
      write(iout,'(/,''latvec a'')')
      do j = 1,3
        write(iout,'(3f15.10)')(rv(k,j),k=1,3)
      enddo
      write(iout,'(''basis a'')')
      if (ncbl.eq.1) then
!************************
!  Primitive cell case  *
!************************
        do i = 1,numat
          ni = nat(i)
          ntp = nftype(i)
          call label(ni,ntp,lab1)
          if (ni.gt.maxele) then
            npt = 2
          else
            npt = 1
          endif
          xfi = xfrac(i)
          yfi = yfrac(i)
          zfi = zfrac(i)
          xfi = mod(xfi+1.0_dp,1.0_dp)
          yfi = mod(yfi+1.0_dp,1.0_dp)
          zfi = mod(zfi+1.0_dp,1.0_dp)
          xcl = xfi*rv(1,1) + yfi*rv(1,2) + zfi*rv(1,3)
          ycl = xfi*rv(2,1) + yfi*rv(2,2) + zfi*rv(2,3)
          zcl = xfi*rv(3,1) + yfi*rv(3,2) + zfi*rv(3,3)
          write(iout,'(a5,1x,a4,2x,3(f17.10,2x))') lab1,stype(npt),xcl,ycl,zcl
        enddo
      else
!****************************
!  Non-primitive cell case  *
!****************************
!
!  Create transformation matrix
!
        do i = 1,3
          rvi(1,i) = rv(1,i)
          rvi(2,i) = rv(2,i)
          rvi(3,i) = rv(3,i)
        enddo
        ifail = 0
        call matinv(rvi,3_i4,3_i4,rrl,ifail)
        do i = 1,3
          do j = 1,3
            rvt(j,i) = rvi(j,1)*rvf(1,i) + rvi(j,2)*rvf(2,i) + rvi(j,3)*rvf(3,i)
          enddo
        enddo
!
!  Transform primitive fractional coordinates to full set
!
        do i = 1,numat
          xt(1) = xfrac(i)
          xt(2) = yfrac(i)
          xt(3) = zfrac(i)
          xfrac(i) = rvt(1,1)*xt(1) + rvt(1,2)*xt(2) + rvt(1,3)*xt(3)
          yfrac(i) = rvt(2,1)*xt(1) + rvt(2,2)*xt(2) + rvt(2,3)*xt(3)
          zfrac(i) = rvt(3,1)*xt(1) + rvt(3,2)*xt(2) + rvt(3,3)*xt(3)
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
          xfi = xfrac(i)
          yfi = yfrac(i)
          zfi = zfrac(i)
          xfi = mod(xfi+1.0_dp,1.0_dp)
          yfi = mod(yfi+1.0_dp,1.0_dp)
          zfi = mod(zfi+1.0_dp,1.0_dp)
          xcl = xfi*rv(1,1) + yfi*rv(1,2) + zfi*rv(1,3)
          ycl = xfi*rv(2,1) + yfi*rv(2,2) + zfi*rv(2,3)
          zcl = xfi*rv(3,1) + yfi*rv(3,2) + zfi*rv(3,3)
          write(iout,'(a5,1x,a4,2x,3(f17.10,2x))') lab1,stype(npt),xcl,ycl,zcl
!
!  Expand by centring operators
!
          if (ncbl.eq.2) then
            xt(1) = 0.0_dp
            xt(2) = 0.5_dp
            xt(3) = 0.5_dp
          elseif (ncbl.eq.3) then
            xt(1) = 0.5_dp
            xt(2) = 0.0_dp
            xt(3) = 0.5_dp
          elseif (ncbl.eq.4) then
            xt(1) = 0.5_dp
            xt(2) = 0.5_dp
            xt(3) = 0.0_dp
          elseif (ncbl.eq.5) then
            xt(1) = 0.0_dp
            xt(2) = 0.5_dp
            xt(3) = 0.5_dp
          elseif (ncbl.eq.6) then
            xt(1) = 0.5_dp
            xt(2) = 0.5_dp
            xt(3) = 0.5_dp
          else
            xt(1) = 2.0_dp/3.0_dp
            xt(2) = 1.0_dp/3.0_dp
            xt(3) = 1.0_dp/3.0_dp
          endif
          xfi = xfrac(i)+xt(1)
          yfi = yfrac(i)+xt(2)
          zfi = zfrac(i)+xt(3)
          xfi = mod(xfi+1.0_dp,1.0_dp)
          yfi = mod(yfi+1.0_dp,1.0_dp)
          zfi = mod(zfi+1.0_dp,1.0_dp)
          xcl = xfi*rv(1,1) + yfi*rv(1,2) + zfi*rv(1,3)
          ycl = xfi*rv(2,1) + yfi*rv(2,2) + zfi*rv(2,3)
          zcl = xfi*rv(3,1) + yfi*rv(3,2) + zfi*rv(3,3)
          write(iout,'(a5,1x,a4,2x,3(f17.10,2x))') lab1,stype(npt),xcl,ycl,zcl
          if (ncbl.eq.7) then
            xt(1) = 1.0_dp/3.0_dp
            xt(2) = 2.0_dp/3.0_dp
            xt(3) = 2.0_dp/3.0_dp
            xfi = xfrac(i) + xt(1)
            yfi = yfrac(i) + xt(2)
            zfi = zfrac(i) + xt(3)
            xfi = mod(xfi+1.0_dp,1.0_dp)
            yfi = mod(yfi+1.0_dp,1.0_dp)
            zfi = mod(zfi+1.0_dp,1.0_dp)
            xcl = xfi*rv(1,1) + yfi*rv(1,2) + zfi*rv(1,3)
            ycl = xfi*rv(2,1) + yfi*rv(2,2) + zfi*rv(2,3)
            zcl = xfi*rv(3,1) + yfi*rv(3,2) + zfi*rv(3,3)
            write(iout,'(a5,1x,a4,2x,3(f17.10,2x))') lab1,stype(npt),xcl,ycl,zcl
          elseif (ncbl.eq.5) then
            xt(1) = 0.5_dp
            xt(2) = 0.0_dp
            xt(3) = 0.5_dp
            xfi = xfrac(i)+xt(1)
            yfi = yfrac(i)+xt(2)
            zfi = zfrac(i)+xt(3)
            xfi = mod(xfi+1.0_dp,1.0_dp)
            yfi = mod(yfi+1.0_dp,1.0_dp)
            zfi = mod(zfi+1.0_dp,1.0_dp)
            xcl = xfi*rv(1,1) + yfi*rv(1,2) + zfi*rv(1,3)
            ycl = xfi*rv(2,1) + yfi*rv(2,2) + zfi*rv(2,3)
            zcl = xfi*rv(3,1) + yfi*rv(3,2) + zfi*rv(3,3)
            write(iout,'(a5,1x,a4,2x,3(f17.10,2x))') lab1,stype(npt),xcl,ycl,zcl
            xt(1) = 0.5_dp
            xt(2) = 0.5_dp
            xt(3) = 0.0_dp
            xfi = xfrac(i) + xt(1)
            yfi = yfrac(i) + xt(2)
            zfi = zfrac(i) + xt(3)
            xfi = mod(xfi+1.0_dp,1.0_dp)
            yfi = mod(yfi+1.0_dp,1.0_dp)
            zfi = mod(zfi+1.0_dp,1.0_dp)
            xcl = xfi*rv(1,1) + yfi*rv(1,2) + zfi*rv(1,3)
            ycl = xfi*rv(2,1) + yfi*rv(2,2) + zfi*rv(2,3)
            zcl = xfi*rv(3,1) + yfi*rv(3,2) + zfi*rv(3,3)
            write(iout,'(a5,1x,a4,2x,3(f17.10,2x))') lab1,stype(npt),xcl,ycl,zcl
          endif
        enddo
!
      endif
      write(iout,'(''end'')')
      write(iout,'(''bulk-energy '',f18.8)') energycfg(ncf)
      write(iout,'(/)')
    elseif (ndimen(nc).eq.2) then
!
!  Surface output to input
!
      ncf = nc
      call setup(.false.)
      write(iout,'(/,''surfvec a'')')
      do j = 1,2
        write(iout,'(2f15.10)') (rv(k,j),k = 1,2)
      enddo
      do nr = 1,nregions(ncf)
        write(iout,'(/,''coord '',i1,'' a'')')nr
        do i = 1,numat
          if (nregionno(nsft+i).eq.nr) then
            ni = nat(i)
            ntp = nftype(i)
            call label(ni,ntp,lab1)
            if (ni.gt.maxele) then
              npt = 2
            else
              npt = 1
            endif
            xfi = xfrac(i)
            yfi = yfrac(i)
            zcl = zfrac(i)
            xfi = mod(xfi+1.0_dp,1.0_dp)
            yfi = mod(yfi+1.0_dp,1.0_dp)
!
!  If part of a molecule, take image that preserves connectivity
!
            if (natmol(i).gt.0) then
              call mindtoijk(nmolind(i),ii,jj,kk)
              xfi = xfi + dble(ii)
              yfi = yfi + dble(jj)
            endif
            xcl = xfi*rv(1,1) + yfi*rv(1,2)
            ycl = xfi*rv(2,1) + yfi*rv(2,2)
            write(iout,'(a5,1x,a4,2x,3(f17.10,2x))') lab1,stype(npt),xcl,ycl,zcl
          endif
        enddo
      enddo
      write(iout,'(''end'')')
    endif
!******************************
!  End of configuration loop  *
!******************************
  enddo
!**************************
!  Interatomic potentials *
!**************************
  if (npote.gt.0) then
    do i = 1,npote
!
!  Molecule options
!
      if (lintra(i).and..not.linter(i)) then
        nmptr = 1
      elseif (linter(i).and..not.lintra(i)) then
        nmptr = 2
      else
        nmptr = 0
      endif
      ns1 = nspec1(i)
      nts1 = nptyp1(i)
      call label(ns1,nts1,lab1)
      if (ns1.gt.maxele) then
        npt1 = 2
      else
        npt1 = 1
      endif
      ns2 = nspec2(i)
      nts2 = nptyp2(i)
      call label(ns2,nts2,lab2)
      if (ns2.gt.maxele) then
        npt2 = 2
      else
        npt2 = 1
      endif
      if (mmexc(i).gt.0) then
        mmword = 'molmec'
      else
        mmword = '      '
      endif
      if (nptype(i).eq.1) then
!
!  Buckingham
!
        if (nmptr.gt.0) then
          write(iout,'(''buckingham '',a14,1x,a6)') mword(nmptr),mmword
        else
          write(iout,'(''buckingham'',1x,a6)') mmword
        endif
        if (mmword.eq.'molmec') then
          write(iout,'(2(a5,1x,a4,1x),3(1x,f13.6))')  &
            lab1,stype(npt1),lab2,stype(npt2),twopot(1,i),twopot(2,i),twopot(3,i)
        else
          write(iout,'(2(a5,1x,a4,1x),3(1x,f13.6),2(1x,f6.3))') &
            lab1,stype(npt1),lab2,stype(npt2),twopot(1,i),twopot(2,i),twopot(3,i),rpot2(i),rpot(i)
        endif
      elseif (nptype(i).eq.2.or.nptype(i).eq.21) then
!
!  Lennard-Jones
!
        mpt = int(tpot(1,i))
        npt = int(tpot(2,i))
        if (nmptr.gt.0) then
          if (mpt.eq.9.and.npt.eq.6) then
            write(iout,'(''nine_six '',a14,1x,a6)') mword(nmptr),mmword
          else
            write(iout,'(''lennard '',a14,1x,a6)') mword(nmptr),mmword
          endif
        else
          if (mpt.eq.9.and.npt.eq.6) then
            write(iout,'(''nine_six'',1x,a6)') mmword
          else
            write(iout,'(''lennard'',1x,a6)') mmword
          endif
        endif
        if (mmword.eq.'molmec') then
          write(iout,'(2(a5,1x,a4,1x),2(1x,f13.6))') &
            lab1,stype(npt1),lab2,stype(npt2),twopot(1,i),twopot(2,i)
        else
          write(iout,'(2(a5,1x,a4,1x),2(1x,f13.6),2(1x,f6.3))') &
            lab1,stype(npt1),lab2,stype(npt2),twopot(1,i),twopot(2,i),rpot2(i),rpot(i)
        endif
      elseif (nptype(i).eq.3) then
!
!  Morse - no coulomb offset
!
        if (mmword.ne.' ') then
          write(iout,'(''morse '',a6)') mmword
        else
          write(iout,'(''morse '',a14)') mword(nmptr)
        endif
        if (mmexc(i).gt.0) then
          write(iout,'(2(a5,1x,a4,1x),3(1x,f12.6))') &
            lab1,stype(npt1),lab2,stype(npt2),twopot(1,i),twopot(2,i),twopot(3,i)
        else
          write(iout,'(2(a5,1x,a4,1x),3(1x,f12.6),2(1x,f6.3))') &
            lab1,stype(npt1),lab2,stype(npt2),twopot(1,i),twopot(2,i),twopot(3,i),rpot2(i),rpot(i)
        endif
      elseif (nptype(i).eq.4) then
!
!  Morse - with coulomb offset
!
        if (nmptr.gt.0) then
          if (mmword.ne.' ') then
            write(iout,'(''morse '',a6)') mmword
          else
            write(iout,'(''morse '',a14)') mword(nmptr)
          endif
        else
          write(iout,'(''morse'',1x,a6)') mmword
        endif
        if (mmexc(i).gt.0) then
          write(iout,'(2(a5,1x,a4,1x),3(1x,f12.6))') &
            lab1,stype(npt1),lab2,stype(npt2),twopot(1,i),twopot(2,i),twopot(3,i)
        else
          write(iout,'(2(a5,1x,a4,1x),3(1x,f12.6),2(1x,f6.3))') &
            lab1,stype(npt1),lab2,stype(npt2),twopot(1,i),twopot(2,i),twopot(3,i),rpot2(i),rpot(i)
        endif
!
!  Coulomb subtract
!
        if (nmptr.gt.0) then
          write(iout,'(''coulomb '',a14)') mword(nmptr)
        else
          write(iout,'(''coulomb'')')
        endif
        if (mmexc(i).gt.0) then
          write(iout,'(2(a5,1x,a4,1x))') lab1,stype(npt1),lab2,stype(npt2)
        else
          write(iout,'(2(a5,1x,a4,1x),2(1x,f6.3))') lab1,stype(npt1),lab2,stype(npt2),rpot2(i),rpot(i)
        endif
      elseif (nptype(i).eq.5) then
!
!  Harmonic - no coulomb offset
!  If core - shell harmonic use spring
!
        if (ns1.eq.ns2.and.npt1.ne.npt2) then
          write(iout,'(''spring'')')
          write(iout,'(2(a5,1x,a4,1x),2(1x,f12.6),1x,f6.3)') &
            lab1,stype(npt1),lab2,stype(npt2),twopot(1,i),twopot(2,i),rpot(i)
        else
          if (index(mmword,'mol').eq.1) then
!
!  Andrew's requested fix to leave out intra for molmec harmonic
!
            write(iout,'(''harmonic'',1x,a6)') mmword
            write(iout,'(2(a5,1x,a4,1x),2(1x,f12.6))') &
              lab1,stype(npt1),lab2,stype(npt2),twopot(1,i),twopot(2,i)
          else
            if (nmptr.gt.0) then
              write(iout,'(''harmonic '',a14,1x,a6)') mword(nmptr),mmword
            else
              write(iout,'(''harmonic'',1x,a6)') mmword
            endif
            write(iout,'(2(a5,1x,a4,1x),2(1x,f12.6),2(1x,f6.3))') &
              lab1,stype(npt1),lab2,stype(npt2),twopot(1,i),twopot(2,i),rpot2(i),rpot(i)
          endif
        endif
      elseif (nptype(i).eq.6) then
!
!  Harmonic - with coulomb offset
!
        if (index(mmword,'mol').eq.1) then
!
!  Andrew's requested fix to leave out intra for molmec harmonic
!
          write(iout,'(''harmonic'',1x,a6)') mmword
          write(iout,'(2(a5,1x,a4,1x),2(1x,f12.6))') &
            lab1,stype(npt1),lab2,stype(npt2),twopot(1,i),twopot(2,i)
        else
          if (nmptr.gt.0) then
            write(iout,'(''harmonic '',a14,1x,a6)') mword(nmptr),mmword
          else
            write(iout,'(''harmonic'',1x,a6)') mmword
          endif
          write(iout,'(2(a5,1x,a4,1x),2(1x,f12.6),2(1x,f6.3))') &
            lab1,stype(npt1),lab2,stype(npt2),twopot(1,i),twopot(2,i),rpot2(i),rpot(i)
        endif
!
!  Coulomb subtract
!
        if (nmptr.gt.0) then
          write(iout,'(''coulomb '',a14)') mword(nmptr)
        else
          write(iout,'(''coulomb'')')
        endif
        if (mmexc(i).gt.0) then
          write(iout,'(2(a5,1x,a4,1x))') lab1,stype(npt1),lab2,stype(npt2)
        else
          write(iout,'(2(a5,1x,a4,1x),2(1x,f6.3))') lab1,stype(npt1),lab2,stype(npt2),rpot2(i),rpot(i)
        endif
      elseif (nptype(i).eq.8) then
!
!  Spring potential
!
        write(iout,'(''spring'')')
        if (twopot(2,i).ne.0.0_dp) then
          write(iout,'(2(a5,1x,a4,1x),2(1x,f12.6),1x,f6.3)') &
            lab1,stype(npt1),lab2,stype(npt2),twopot(1,i),twopot(2,i),rpot(i)
        else
          write(iout,'(2(a5,1x,a4,1x),1x,f12.6,1x,f6.3)') &
            lab1,stype(npt1),lab2,stype(npt2),twopot(1,i),rpot(i)
        endif
      elseif (nptype(i).eq.9) then
!
!  Coulomb subtraction potential
!
        if (nmptr.gt.0) then
          write(iout,'(''coulomb '',a14)') mword(nmptr)
        else
          write(iout,'(''coulomb'')')
        endif
        if (mmexc(i).gt.0) then
          write(iout,'(2(a5,1x,a4,1x))') lab1,stype(npt1),lab2,stype(npt2)
        else
          write(iout,'(2(a5,1x,a4,1x),2(1x,f6.3))') &
            lab1,stype(npt1),lab2,stype(npt2),rpot2(i),rpot(i)
        endif
      endif
    enddo
  endif
!*************************
!  Three-body potentials *
!*************************
  if (nthb.gt.0) then
    do i = 1,nthb
      the = theta(i)
      ns1 = ntspec1(i)
      ns2 = ntspec2(i)
      ns3 = ntspec3(i)
      nts1 = ntptyp1(i)
      nts2 = ntptyp2(i)
      nts3 = ntptyp3(i)
      call label(ns1,nts1,lab1)
      call label(ns2,nts2,lab2)
      call label(ns3,nts3,lab3)
      if (ns1.gt.maxele) then
        npt1 = 2
      else
        npt1 = 1
      endif
      ns2 = ntspec2(i)
      if (ns2.gt.maxele) then
        npt2 = 2
      else
        npt2 = 1
      endif
      ns3 = ntspec3(i)
      if (ns3.gt.maxele) then
        npt3 = 2
      else
        npt3 = 1
      endif
      if (mmtexc(i).gt.0) then
        mmword = 'molmec'
      else
        mmword = '      '
      endif
      if (nthrty(i).eq.1) then
        if (mmtexc(i).gt.0) then
          write(iout,'(''three-body '',a6)') mmword
          write(iout,'(3(a5,1x,a4,1x),f12.6,1x,f12.6)') &
            lab1,stype(npt1),lab2,stype(npt2),lab3,stype(npt3),thbk(i),the
        else
          write(iout,'(''three-body '',a6)') mmword
          write(iout,'(3(a5,1x,a4,1x),f12.6,1x,f12.6,3(1x,f7.4))') &
            lab1,stype(npt1),lab2,stype(npt2),lab3,stype(npt3),thbk(i),the,thr1(i),thr2(i),thr3(i)
        endif
      else
        nwarn = nwarn + 1
        call outwarning('potential type not available in Marvin',0_i4)
      endif
    enddo
  endif
!************************
!  Four-body potentials *
!************************
  if (nfor.gt.0) then
    do i = 1,nfor
      nat1 = nfspec1(i)
      ntype1 = nfptyp1(i)
      call label(nat1,ntype1,lab1)
      if (nat1.gt.maxele) then
        nptr1 = 2
      else
        nptr1 = 1
      endif
      nat2 = nfspec2(i)
      ntype2 = nfptyp2(i)
      call label(nat2,ntype2,lab2)
      if (nat2.gt.maxele) then
        nptr2 = 2
      else
        nptr2 = 1
      endif
      nat3 = nfspec3(i)
      ntype3 = nfptyp3(i)
      call label(nat3,ntype3,lab3)
      if (nat3.gt.maxele) then
        nptr3 = 2
      else
        nptr3 = 1
      endif
      nat4 = nfspec4(i)
      ntype4 = nfptyp4(i)
      call label(nat4,ntype4,lab4)
      if (nat4.gt.maxele) then
        nptr4 = 2
      else
        nptr4 = 1
      endif
      npl = npfor(i)
      if (npl.ge.0) then
        isign = 1
      else
        isign = -1
      endif
      diff = abs(forpoly(1,i)-180.0)
!
!  No phi0 in Marvin - switch sign instead
!
      if (diff.lt.1.0d-3) isign = - isign
!
      if (mmfexc(i).gt.0) then
        mmword = 'molmec'
      else
        mmword = '      '
      endif
      npl = abs(npl)
      if (nforty(i).eq.1) then
        write(iout,'(''four '',a6)') mmword
        if (mmfexc(i).gt.0) then
          write(iout,'(4(a5,1x,a4,1x),f10.6,1x,i2,1x,i3)') &
            lab1,stype(nptr1),lab2,stype(nptr2),lab3,stype(nptr3),lab4,stype(nptr4),fork(i),isign,npl
        else
          if (for4(i).eq.0) then
            write(iout,'(4(a5,1x,a4,1x),f10.6,1x,i2,1x,i3,3(1x,f6.3))') &
              lab1,stype(nptr1),lab2,stype(nptr2),lab3,stype(nptr3),lab4,stype(nptr4),fork(i),isign,npl, &
              for1(i),for2(i),for3(i)
          else
            write(iout,'(4(a5,1x,a4,1x),f10.6,1x,i2,1x,i3,4(1x,f5.2))') &
              lab1,stype(nptr1),lab2,stype(nptr2),lab3,stype(nptr3),lab4,stype(nptr4),fork(i),isign,npl, &
              for1(i),for2(i),for3(i),for4(i)
          endif
        endif
      endif
    enddo
  endif
!**********************
!  Species parameters *
!**********************
  write(iout,'(/,''element'')')
  do i = 1,nspec
    ni = natspec(i)
    ntp = ntypspec(i)
    call label(ni,ntp,lab1)
    if (ni.gt.maxele) then
      zero = 0.0_dp
      write(iout,'(a5,1x,a4,1x,f10.6,'' mass '',f10.6,'' cov '',f10.6)') lab1,stype(2),qlspec(i),zero,rcov(ni-maxele)
    else
      write(iout,'(a5,1x,a4,1x,f10.6,'' mass '',f10.6,'' cov '',f10.6)') lab1,stype(1),qlspec(i),massspec(i),rcov(ni)
    endif
  enddo
  write(iout,'(''end'',/)')
!******************
!  Marvin insert  *
!******************
  if (marvtemp(1:1).ne.' ') then
    open(iout+10,file=marvtemp,status='old')
90   read(iout+10,'(a)',err=100,end=100) line
    write(iout,'(a)') line
    goto 90
100   close(iout+10)
  endif
!******************************
!  General control parameters *
!******************************
  if (accuracy.ne.6.0) then
    write(iout,'(''accuracy '',f6.3)') accuracy
  endif
  close(iout)
!
  return
  end
