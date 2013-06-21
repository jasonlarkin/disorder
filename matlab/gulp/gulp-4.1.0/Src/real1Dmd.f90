  subroutine real1Dmd(erealin,esregion12,esregion2,lgrad1)
!
!  This subroutine calculates the electrostatic energy of a 1-D system 
!  in real space based on the algorithm implemented in CRYSTAL. A 
!  neutralising uniform background charge density is applied and then 
!  subtracted again. Because a sum over neutral unit cells is required, 
!  this code is kept separate from the other real space routines. The 
!  conventional real space routines are used to handle the Coulomb
!  subtraction issues in order to keep this routine simple.
!
!   5/06 Created from real1D for first derivative/parallel case
!   5/06 Parallelised using Brode-Ahlrichs algorithm
!   5/07 QM/MM scheme added
!  12/07 Unused variables removed
!   3/09 Explicit 1.0d-15 replaced by global value smallself from general module
!   6/09 Site energy and virials added
!  11/09 Region derivatives added
!   2/11 Integer types for local variables changed to i4
!  11/11 Region-region energy contributions stored
!   4/12 xvir, yvir and zvir removed
!   5/12 Atomic stresses added
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
!  No claim is made that this program is free from errors and
!  no liability will be accepted for any loss or damage that
!  may result. The user is responsible for checking the validity
!  of their results.
!
!  Copyright Curtin University 2012
!
!  Julian Gale, NRI, Curtin University, May 2012
!
  use constants,      only : angstoev
  use configurations, only : nregionno, nregions, nregiontype, QMMMmode
  use control,        only : lnoreal, lseok, keyword, lDoQDeriv1, latomicstress
  use current
  use derivatives
  use element,        only : maxele
  use energies,       only : siteenergy, eregion2region
  use general,        only : accuracy, nmaxcells, nemorder, smallself
  use iochannels,     only : ioout
  use optimisation,   only : lfreeze, lopf
  use parallel,       only : nprocs, procid
  use qmedata,        only : maxloop
  use shell,          only : cuts
  use symmetry,       only : lstr
  use times
  implicit none
!
!  Passed arguments
!
  real(dp), intent(inout) :: erealin
  real(dp), intent(inout) :: esregion12
  real(dp), intent(inout) :: esregion2
  logical,  intent(in)    :: lgrad1
!
!  Local variables
!
  integer(i4)             :: i
  integer(i4)             :: j 
  integer(i4)             :: m
  integer(i4)             :: mj
  integer(i4)             :: nati
  integer(i4)             :: natj
  integer(i4)             :: noff
  integer(i4)             :: noffm1
  integer(i4)             :: noffset
  integer(i4)             :: nregioni
  integer(i4)             :: nregionj
  integer(i4)             :: nregiontypi
  integer(i4)             :: nregiontypj
  logical                 :: lconverged
  logical                 :: lcspair
  logical                 :: lopi
  logical                 :: lopj
  logical                 :: lreg2one
  logical                 :: lreg2pair
  real(dp)                :: accf
  real(dp)                :: acell
  real(dp)                :: cputime
  real(dp)                :: cut2s
  real(dp)                :: d0
  real(dp)                :: d0i
  real(dp)                :: d0j
  real(dp)                :: d0term
  real(dp)                :: d1
  real(dp)                :: d1s
  real(dp)                :: dh1(3)
  real(dp)                :: dh2(3)
  real(dp)                :: dh1s
  real(dp)                :: dh2s
  real(dp)                :: d2h1(6)
  real(dp)                :: d2h2(6)
  real(dp)                :: d2h1m(3)
  real(dp)                :: d2h2m(3)
  real(dp)                :: d2h1s
  real(dp)                :: d2h2s
  real(dp)                :: d3h1(10)
  real(dp)                :: d3h1m(6)
  real(dp)                :: ediff
  real(dp)                :: elast
  real(dp)                :: ereal
  real(dp)                :: esum
  real(dp)                :: esumem
  real(dp)                :: esumh
  real(dp)                :: esum12
  real(dp)                :: esumem12
  real(dp)                :: esumh12
  real(dp)                :: esum2
  real(dp)                :: esumem2
  real(dp)                :: esumh2
  real(dp)                :: etrm
  real(dp)                :: e1
  real(dp)                :: e2
  real(dp)                :: h1
  real(dp)                :: h2
  real(dp)                :: lna
  real(dp)                :: oci
  real(dp)                :: ocj
  real(dp)                :: qi
  real(dp)                :: qj
  real(dp)                :: qii
  real(dp)                :: qij
  real(dp)                :: r
  real(dp)                :: rcut
  real(dp)                :: rr
  real(dp)                :: t1, t2
  real(dp)                :: u
  real(dp)                :: x
  real(dp)                :: y
  real(dp)                :: z
!
  t1 = cputime()
!
!  If noreal specified, return
!
  if (lnoreal) then
    erealin = 0.0_dp
    return
  endif
!********************************************************
!  Calculate Coulomb sum converged to desired accuracy  *
!********************************************************
!
!  Use the Brode-Ahlrichs Algorithm in parallel
!
  noff = numat/2
  noffset = noff
  if (mod(numat,2_i4).eq.0) then
    noffm1 = noff - 1
  else
    noffm1 = noff
  endif
!
!  Loop over number of cells in sum
!
  accf = 10.0**(-accuracy)
  lna = log(a)
  cut2s = cuts*cuts
  m = - 1
  lconverged = .false.
  elast = 0.0_dp
  esum = 0.0_dp
  esum12 = 0.0_dp
  esum2 = 0.0_dp
  if (procid.eq.0.and.index(keyword,'verb').ne.0) then
    write(ioout,'(/,'' Convergence of 1-D electrostatic summation:'',/)')
  endif
  do while (m.lt.nmaxcells.and..not.lconverged) 
    m = m + 1
!********************************************
!  Direct sum component over neutral cells  *
!********************************************
    acell = dble(m)*a
    do i = procid+1,numat,nprocs
!orig     do i = 2,numat
      if (i.gt.noff) then
        noffset = noffm1
      else
        noffset = noff
      endif
      nati = nat(i)
      oci = occuf(i)
      qi = qf(i)*oci
      nregioni = nregionno(nsft+i)
      nregiontypi = nregiontype(nregioni,ncf)
      lopi = (.not.lfreeze.or.lopf(nrelat(i)))
      jloop: do mj = 1,noffset
        j = mod(i+mj-1_i4,numat) + 1
!orig     do j = 1,i-1
        natj = nat(j)
        ocj = occuf(j)
        qj = qf(j)*ocj
        nregionj = nregionno(nsft+j)
        nregiontypj = nregiontype(nregionj,ncf)
        lopj = (.not.lfreeze.or.lopf(nrelat(j)))
        lcspair = (abs(nati-natj).eq.maxele.or.(oci+ocj).lt.1.0001_dp)
        if (lcspair) then
          rcut = cut2s
        else
          rcut = smallself
        endif
        if (lopi.or.lopj) then
!
!  Set region 2 pair flags
!        
          lreg2one  = .false.
          lreg2pair = .false.
          if (lseok.and.nregions(ncf).ge.2) then
            lreg2pair = (nregioni.eq.2.and.nregionj.eq.2)
            if (.not.lreg2pair) lreg2one = (nregioni.eq.2.or.nregionj.eq.2)
          endif
!  
!  QM/MM handling : i & j are both QM atoms => exclude
!
          if (QMMMmode(ncf).gt.0) then
            if (nregiontypi.eq.1.and.nregiontypj.eq.1) cycle jloop
!
!  QM/MM : Electrostatic embedding : If either i or j are QM atoms => exclude 
!
            if (QMMMmode(ncf).eq.2.and.(nregiontypi.eq.1.or.nregiontypj.eq.1)) cycle jloop
          endif
!
          x = acell + xclat(j) - xclat(i)
          y = yclat(j) - yclat(i)
          z = zclat(j) - zclat(i)
          r = x*x + y*y + z*z
          if (r.gt.rcut) then
            r = sqrt(r)
            rr = 1.0_dp/r
            d0 = qi*qj*rr
            if (lreg2one) then
              esum12 = esum12 + d0
            elseif (lreg2pair) then
              esum2 = esum2 + d0
            else
              esum = esum + d0
            endif
!
            eregion2region(nregionj,nregioni) = eregion2region(nregionj,nregioni) + d0*angstoev
!
            siteenergy(i) = siteenergy(i) + 0.5_dp*d0*angstoev
            siteenergy(j) = siteenergy(j) + 0.5_dp*d0*angstoev
            if (lgrad1) then
              d1 = d0*rr*rr*angstoev
              if (lopi) then
                xdrv(i) = xdrv(i) + d1*x
                ydrv(i) = ydrv(i) + d1*y
                zdrv(i) = zdrv(i) + d1*z
              endif
              if (lopj) then
                xdrv(j) = xdrv(j) - d1*x
                ydrv(j) = ydrv(j) - d1*y
                zdrv(j) = zdrv(j) - d1*z
              endif
              if (nregioni.ne.nregionj) then
                xregdrv(nregioni) = xregdrv(nregioni) + d1*x
                yregdrv(nregioni) = yregdrv(nregioni) + d1*y
                zregdrv(nregioni) = zregdrv(nregioni) + d1*z
                xregdrv(nregionj) = xregdrv(nregionj) - d1*x
                yregdrv(nregionj) = yregdrv(nregionj) - d1*y
                zregdrv(nregionj) = zregdrv(nregionj) - d1*z
              endif
              if (lstr) then
                rstrd(1) = rstrd(1) - d1*x*x
                if (latomicstress) then
                  atomicstress(1,i) = atomicstress(1,i) - 0.5_dp*d1*x*x
                  atomicstress(1,j) = atomicstress(1,j) - 0.5_dp*d1*x*x
                endif
              endif
              if (lDoQDeriv1) then
                d0i = qj*rr*angstoev
                d0j = qi*rr*angstoev
                call d1charge(i,j,lopi,lopj,1_i4,d0i,d0j)
              endif
            endif
          endif
          if (m.gt.0) then
            x = - acell + xclat(j) - xclat(i)
            r = x*x + y*y + z*z
            if (r.gt.rcut) then
              r = sqrt(r)
              rr = 1.0_dp/r
              d0 = qi*qj*rr
              if (lreg2one) then
                esum12 = esum12 + d0
              elseif (lreg2pair) then
                esum2 = esum2 + d0
              else
                esum = esum + d0
              endif
!
              eregion2region(nregionj,nregioni) = eregion2region(nregionj,nregioni) + d0*angstoev
!
              siteenergy(i) = siteenergy(i) + 0.5_dp*d0*angstoev
              siteenergy(j) = siteenergy(j) + 0.5_dp*d0*angstoev
              if (lgrad1) then
                d1 = d0*rr*rr*angstoev
                if (lopi) then
                  xdrv(i) = xdrv(i) + d1*x
                  ydrv(i) = ydrv(i) + d1*y
                  zdrv(i) = zdrv(i) + d1*z
                endif
                if (lopj) then
                  xdrv(j) = xdrv(j) - d1*x
                  ydrv(j) = ydrv(j) - d1*y
                  zdrv(j) = zdrv(j) - d1*z
                endif
                if (nregioni.ne.nregionj) then
                  xregdrv(nregioni) = xregdrv(nregioni) + d1*x
                  yregdrv(nregioni) = yregdrv(nregioni) + d1*y
                  zregdrv(nregioni) = zregdrv(nregioni) + d1*z
                  xregdrv(nregionj) = xregdrv(nregionj) - d1*x
                  yregdrv(nregionj) = yregdrv(nregionj) - d1*y
                  zregdrv(nregionj) = zregdrv(nregionj) - d1*z
                endif
                if (lstr) then
                  rstrd(1) = rstrd(1) - d1*x*x
                  if (latomicstress) then
                    atomicstress(1,i) = atomicstress(1,i) - 0.5_dp*d1*x*x
                    atomicstress(1,j) = atomicstress(1,j) - 0.5_dp*d1*x*x
                  endif
                endif
                if (lDoQDeriv1) then
                  d0i = qj*rr*angstoev
                  d0j = qi*rr*angstoev
                  call d1charge(i,j,lopi,lopj,1_i4,d0i,d0j)
                endif
              endif
            endif
          endif
        endif
      enddo jloop
    enddo
!**********************
!  Self interactions  *
!**********************
    iloop: do i = procid+1,numat,nprocs
      oci = occuf(i)
      qi = qf(i)*oci
      nregioni = nregionno(nsft+i)
      nregiontypi = nregiontype(nregioni,ncf)
!  
!  QM/MM handling : i is a QM atom => exclude
!
      if (QMMMmode(ncf).gt.0) then
        if (nregiontypi.eq.1) cycle iloop
      endif
      lopi = (.not.lfreeze.or.lopf(nrelat(i)))
      if (lopi) then
        r = abs(acell)
        if (r.gt.smallself) then
          rr = 1.0_dp/r
          d0 = qi*qi*rr
!
!  Set region 2 pair flags
!        
          lreg2pair = .false.
          if (lseok.and.nregions(ncf).ge.2) then
            lreg2pair = (nregioni.eq.2)
          endif
          if (lreg2pair) then
            esum2 = esum2 + d0
          else
            esum = esum + d0
          endif
!
          eregion2region(nregioni,nregioni) = eregion2region(nregioni,nregioni) + d0*angstoev
!
          siteenergy(i) = siteenergy(i) + d0*angstoev
          if (lgrad1.and.lstr) then
            d1 = d0*angstoev
            rstrd(1) = rstrd(1) - d1
            if (latomicstress) then
              atomicstress(1,i) = atomicstress(1,i) - d1
            endif
          endif
          if (lgrad1.and.lDoQDeriv1) then
            d0i = qj*rr*angstoev
            call d1charge(i,i,lopi,lopi,1_i4,d0i,d0i)
          endif
        endif
      endif
    enddo iloop
!
!  Neutralising terms
!
!  Background
!
!  and
!
!  Euler-MacLaurin component
!
    esumh = 0.0_dp
    esumh12 = 0.0_dp
    esumh2 = 0.0_dp
    esumem = 0.0_dp
    esumem12 = 0.0_dp
    esumem2 = 0.0_dp
    if (m.gt.0) then
      u = (dble(m)+0.5_dp)*a
      do i = procid+1,numat,nprocs
!orig     do i = 2,numat
        if (i.gt.noff) then
          noffset = noffm1
        else
          noffset = noff
        endif
        oci = occuf(i)
        qi = qf(i)*oci
        nregioni = nregionno(nsft+i)
        nregiontypi = nregiontype(nregioni,ncf)
        lopi = (.not.lfreeze.or.lopf(nrelat(i)))
        jloop2: do mj = 1,noffset
          j = mod(i+mj-1_i4,numat) + 1
!orig       do j = 1,i-1
          ocj = occuf(j)
          qj = qf(j)*ocj
          nregionj = nregionno(nsft+j)
          nregiontypj = nregiontype(nregionj,ncf)
          lopj = (.not.lfreeze.or.lopf(nrelat(j)))
          if (lopi.or.lopj) then
!     
!  Set region 2 pair flags
!     
            lreg2one  = .false.
            lreg2pair = .false.
            if (lseok.and.nregions(ncf).ge.2) then
              lreg2pair = (nregioni.eq.2.and.nregionj.eq.2)
              if (.not.lreg2pair) lreg2one = (nregioni.eq.2.or.nregionj.eq.2)
            endif
!  
!  QM/MM handling : i & j are both QM atoms => exclude
!
            if (QMMMmode(ncf).gt.0) then
              if (nregiontypi.eq.1.and.nregiontypj.eq.1) cycle jloop2
!
!  QM/MM : Electrostatic embedding : If either i or j are QM atoms => exclude 
!
              if (QMMMmode(ncf).eq.2.and.(nregiontypi.eq.1.or.nregiontypj.eq.1)) cycle jloop2
            endif
!
            qij = qi*qj
            x = xclat(j) - xclat(i)
            y = yclat(j) - yclat(i)
            z = zclat(j) - zclat(i)
            call hfunc(u,+x,1.0_dp,y,z,h1,dh1,d2h1,d3h1,.false.,.false.,.false.)
            call hfunc(u,-x,-1.0_dp,y,z,h2,dh2,d2h2,d3h1,.false.,.false.,.false.)
            if (lreg2one) then
              esumh12 = esumh12 - qij*(h1 + h2 - 2.0_dp*lna)/a
            elseif (lreg2pair) then
              esumh2 = esumh2 - qij*(h1 + h2 - 2.0_dp*lna)/a
            else
              esumh = esumh - qij*(h1 + h2 - 2.0_dp*lna)/a
            endif
!
            etrm = qij*angstoev*(h1 + h2 - 2.0_dp*lna)/a
            eregion2region(nregionj,nregioni) = eregion2region(nregionj,nregioni) - etrm
!
            siteenergy(i) = siteenergy(i) - 0.5_dp*etrm
            siteenergy(j) = siteenergy(j) - 0.5_dp*etrm
!
            call emfunc(nemorder,u,+x,1.0_dp,y,z,a,e1,dh1,d2h1,dh1s,d2h1s,d2h1m,d3h1,d3h1m,.false.,.false.,.false.)
            call emfunc(nemorder,u,-x,-1.0_dp,y,z,a,e2,dh2,d2h2,dh2s,d2h2s,d2h2m,d3h1,d3h1m,.false.,.false.,.false.)
            if (lreg2one) then
              esumem12 = esumem12 + qij*(e1 + e2)
            elseif (lreg2pair) then
              esumem2 = esumem2 + qij*(e1 + e2)
            else
              esumem = esumem + qij*(e1 + e2)
            endif
!
            etrm = qij*angstoev*(e1 + e2)
            eregion2region(nregionj,nregioni) = eregion2region(nregionj,nregioni) + etrm
!
            siteenergy(i) = siteenergy(i) + 0.5_dp*etrm
            siteenergy(j) = siteenergy(j) + 0.5_dp*etrm
          endif
        enddo jloop2
      enddo
      iloop2: do i = procid+1,numat,nprocs
        oci = occuf(i)
        qi = qf(i)*oci
        nregioni = nregionno(nsft+i)
        nregiontypi = nregiontype(nregioni,ncf)
!
!  QM/MM handling : i is a QM atom => exclude
!
        if (QMMMmode(ncf).gt.0) then
          if (nregiontypi.eq.1) cycle iloop2
        endif
        lopi = (.not.lfreeze.or.lopf(nrelat(i)))
        if (lopi) then
!
!  Set region 2 pair flags
!
          lreg2pair = .false.
          if (lseok.and.nregions(ncf).ge.2) then
            lreg2pair = (nregioni.eq.2)
          endif
!
          qii = qi*qi
          call hfunc(u,0.0_dp,1.0_dp,0.0_dp,0.0_dp,h1,dh1,d2h1,d3h1,.false.,.false.,.false.)
          if (lreg2pair) then
            esumh2 = esumh2 - qii*(h1 - lna)/a
          else
            esumh = esumh - qii*(h1 - lna)/a
          endif
!
          etrm = qii*angstoev*(h1 - lna)/a
          eregion2region(nregioni,nregioni) = eregion2region(nregioni,nregioni) + etrm
!
          siteenergy(i) = siteenergy(i) + etrm
!
          call emfunc(nemorder,u,0.0_dp,1.0_dp,0.0_dp,0.0_dp,a,e1,dh1,d2h1,dh1s,d2h1s,d2h1m, &
            d3h1,d3h1m,.false.,.false.,.false.)
          if (lreg2pair) then
            esumem2 = esumem2 + qii*e1
          else
            esumem = esumem + qii*e1
          endif
!
          etrm = qii*angstoev*e1
          eregion2region(nregioni,nregioni) = eregion2region(nregioni,nregioni) + etrm
!
          siteenergy(i) = siteenergy(i) + etrm
        endif
      enddo iloop2
    endif
!
!  Sum up terms
!
    ereal = esum + esumh + esumem
!
!  Compare to energy with previous number of cells
!  and check for convergence to the required
!  accuracy.
!
    if (abs(ereal).lt.accf) lconverged = .true.
    if (.not.lconverged) then
      ediff = abs((ereal - elast)/ereal)
      lconverged = (ediff.lt.accf)
    endif
    if (procid.eq.0.and.index(keyword,'verb').ne.0) then
      write(ioout,'('' Mcell = '',i4,''  Ereal(1-D) = '',f15.6,'' eV'')') m,ereal*angstoev
    endif
    elast = ereal
  enddo
  if (procid.eq.0.and.index(keyword,'verb').ne.0) write(ioout,'(/)')
  ereal = ereal*angstoev
  erealin = erealin + ereal
  esregion12 = esregion12 + (esum12 + esumh12 + esumem12)*angstoev
  esregion2 = esregion2 + (esum2 + esumh2 + esumem2)*angstoev
!
!  Save number of cells needed
!
  maxloop(1) = m
!
!  Derivatives of Euler-MacLaurin terms since these are not cumulative
!
  if (lgrad1.and.m.gt.0) then
    u = (dble(m)+0.5_dp)*a
    do i = procid+1,numat,nprocs
      if (i.gt.noff) then
        noffset = noffm1
      else
        noffset = noff
      endif
      oci = occuf(i)
      qi = qf(i)*oci
      nregioni = nregionno(nsft+i)
      nregiontypi = nregiontype(nregioni,ncf)
      lopi = (.not.lfreeze.or.lopf(nrelat(i)))
      jloop3: do mj = 1,noffset
        j = mod(i+mj-1_i4,numat) + 1
        ocj = occuf(j)
        qj = qf(j)*ocj
        nregionj = nregionno(nsft+j)
        nregiontypj = nregiontype(nregionj,ncf)
!  
!  QM/MM handling : i & j are both QM atoms => exclude
!
        if (QMMMmode(ncf).gt.0) then
          if (nregiontypi.eq.1.and.nregiontypj.eq.1) cycle jloop3
!
!  QM/MM : Electrostatic embedding : If either i or j are QM atoms => exclude 
!
          if (QMMMmode(ncf).eq.2.and.(nregiontypi.eq.1.or.nregiontypj.eq.1)) cycle jloop3
        endif
        lopj = (.not.lfreeze.or.lopf(nrelat(j)))
        if (lopi.or.lopj) then
          qij = qi*qj
          x = xclat(j) - xclat(i)
          y = yclat(j) - yclat(i)
          z = zclat(j) - zclat(i)
          r = sqrt(x*x + y*y + z*z)
          call hfunc(u,+x,1.0_dp,y,z,h1,dh1,d2h1,d3h1,lgrad1,.false.,.false.)
          call hfunc(u,-x,-1.0_dp,y,z,h2,dh2,d2h2,d3h1,lgrad1,.false.,.false.)
          d1 = qij*angstoev/a
          if (lopi) then
            xdrv(i) = xdrv(i) + d1*(dh1(1) + dh2(1))
            ydrv(i) = ydrv(i) + d1*(dh1(2) + dh2(2))
            zdrv(i) = zdrv(i) + d1*(dh1(3) + dh2(3))
          endif
          if (lopj) then
            xdrv(j) = xdrv(j) - d1*(dh1(1) + dh2(1))
            ydrv(j) = ydrv(j) - d1*(dh1(2) + dh2(2))
            zdrv(j) = zdrv(j) - d1*(dh1(3) + dh2(3))
          endif
          if (nregioni.ne.nregionj) then
            xregdrv(nregioni) = xregdrv(nregioni) + d1*(dh1(1) + dh2(1))
            yregdrv(nregioni) = yregdrv(nregioni) + d1*(dh1(2) + dh2(2))
            zregdrv(nregioni) = zregdrv(nregioni) + d1*(dh1(3) + dh2(3))
            xregdrv(nregionj) = xregdrv(nregionj) - d1*(dh1(1) + dh2(1))
            yregdrv(nregionj) = yregdrv(nregionj) - d1*(dh1(2) + dh2(2))
            zregdrv(nregionj) = zregdrv(nregionj) - d1*(dh1(3) + dh2(3))
          endif
          if (lstr) then
            d1s = - (dh1(1)*(u+x)-dh2(1)*(u-x))
            d1s = d1s + (h1+h2)
            d1s = d1s + 2.0_dp*(1.0d0-lna)
            d1s = d1s*angstoev/a
            d1s = d1s*qij
            rstrd(1) = rstrd(1) + d1s
            if (latomicstress) then
              atomicstress(1,i) = atomicstress(1,i) + 0.5_dp*d1s
              atomicstress(1,j) = atomicstress(1,j) + 0.5_dp*d1s
            endif
          endif
!
!  Compute variable charge term = E without charges
!
          d0term = - angstoev*(h1 + h2 - 2.0_dp*lna)/a
!
          call emfunc(nemorder,u,+x,1.0_dp,y,z,a,e1,dh1,d2h1,dh1s, &
                      d2h1s,d2h1m,d3h1,d3h1m,lgrad1,.false.,.false.)
          call emfunc(nemorder,u,-x,-1.0_dp,y,z,a,e2,dh2,d2h2,dh2s, &
                      d2h2s,d2h2m,d3h1,d3h1m,lgrad1,.false.,.false.)
          d1 = qij*angstoev
          if (lopi) then
            xdrv(i) = xdrv(i) - d1*(dh1(1) + dh2(1))
            ydrv(i) = ydrv(i) - d1*(dh1(2) + dh2(2))
            zdrv(i) = zdrv(i) - d1*(dh1(3) + dh2(3))
          endif
          if (lopj) then
            xdrv(j) = xdrv(j) + d1*(dh1(1) + dh2(1))
            ydrv(j) = ydrv(j) + d1*(dh1(2) + dh2(2))
            zdrv(j) = zdrv(j) + d1*(dh1(3) + dh2(3))
          endif
          if (nregioni.ne.nregionj) then
            xregdrv(nregioni) = xregdrv(nregioni) - d1*(dh1(1) + dh2(1))
            yregdrv(nregioni) = yregdrv(nregioni) - d1*(dh1(2) + dh2(2))
            zregdrv(nregioni) = zregdrv(nregioni) - d1*(dh1(3) + dh2(3))
            xregdrv(nregionj) = xregdrv(nregionj) + d1*(dh1(1) + dh2(1))
            yregdrv(nregionj) = yregdrv(nregionj) + d1*(dh1(2) + dh2(2))
            zregdrv(nregionj) = zregdrv(nregionj) + d1*(dh1(3) + dh2(3))
          endif
          if (lstr) then
            rstrd(1) = rstrd(1) + d1*(dh1s + dh2s)
            if (latomicstress) then
              atomicstress(1,i) = atomicstress(1,i) + 0.5_dp*d1*(dh1s + dh2s)
              atomicstress(1,j) = atomicstress(1,j) + 0.5_dp*d1*(dh1s + dh2s)
            endif
          endif
          d0term = d0term + (e1 + e2)*angstoev 
          if (lDoQDeriv1) then
            d0i = qj*d0term
            d0j = qi*d0term
            call d1charge(i,j,lopi,lopj,1_i4,d0i,d0j)
          endif
        endif
      enddo jloop3
    enddo
    if (lstr.or.lDoQDeriv1) then
!***************************
!  Self-interaction terms  *
!***************************
      iloop3: do i = procid+1,numat,nprocs
        oci = occuf(i)
        qi = qf(i)*oci
        lopi = (.not.lfreeze.or.lopf(nrelat(i)))
        nregioni = nregionno(nsft+nrelat(i))
        nregiontypi = nregiontype(nregioni,ncf)
!
!  QM/MM handling : i is a QM atom => exclude
!
        if (QMMMmode(ncf).gt.0) then
          if (nregiontypi.eq.1) cycle iloop3
        endif
        qii = qi*qi
        call hfunc(u,0.0_dp,1.0_dp,0.0_dp,0.0_dp,h1,dh1,d2h1,d3h1,lgrad1,.false.,.false.)
        if (lstr) then
          d1 = angstoev/a
          d1s = - d1*dh1(1)*u
          d1s = d1s + d1*h1
          d1s = d1s + d1*(1.0d0-lna)
          d1 = d1*qii
          d1s = d1s*qii
          rstrd(1) = rstrd(1) + d1s
          if (latomicstress) then
            atomicstress(1,i) = atomicstress(1,i) + d1s
          endif
        endif
!
!  Compute variable charge term = E without charges
!
        d0term = - angstoev*(h1 - lna)/a
        call emfunc(nemorder,u,0.0_dp,1.0_dp,0.0_dp,0.0_dp,a,e1,dh1,d2h1, &
                    dh1s,d2h1s,d2h1m,d3h1,d3h1m,lgrad1,.false.,.false.)
        if (lstr) then
          rstrd(1) = rstrd(1) + qii*dh1s*angstoev
          if (latomicstress) then
            atomicstress(1,i) = atomicstress(1,i) + qii*dh1s*angstoev
          endif
        endif
        d0term = d0term + e1*angstoev
        if (lDoQDeriv1) then
          d0i = qj*d0term
          call d1charge(i,i,lopi,lopi,1_i4,d0i,d0i)
        endif
      enddo iloop3
    endif
  endif
!
  t2 = cputime()
  tatom = tatom + t2 - t1
!
  return
  end
