  subroutine setlistoop
!
!  Generate list of valid out of plane potentials for MD
!
!  nlist4md = number of valid four body terms
!  nforptr  = pointer to potential number
!  ilind    = pointer to terminal atoms
!  jkind    = pointer to middle atoms
!  icell41  = pointer to cell vector
!  icell42  = pointer to cell vector
!  icell43  = pointer to cell vector
!
!  Strategy - sift by potential first, then cutoffs
!
!   4/97 Created from setlist4md
!   2/01 Call to rlist removed as this routine will have
!        already been called in setlist4 before entering
!   2/01 lintoijk calls now use imaxl,jmaxl and kmaxl
!   4/01 Checking altered so that bonding takes precedence
!        over distances
!   6/01 Setting of lsamemol altered
!   9/01 lmolq calculations accelerated using lneedmol 
!  11/02 Wildcard atoms added
!   1/04 Logic for out of plane atom type matching corrected
!  10/05 Inversion potential added
!   6/06 Inversion squared potential added
!   1/07 Wildcard handling in lmatch calls corrected
!   2/07 Bonding types added
!   5/07 QM/MM schemes added
!  10/07 Angle-angle cross potential added
!  12/07 Unused variables removed
!   4/08 Minimum cutoff added for out of plane potentials
!   5/08 only3 check added as an option
!   7/08 New type checking algorithm introduced
!   7/08 Modified to include atom number pointers in lmatchanyof calls
!  11/08 loutofplane array introduced to indicate whether a potential is out-of-plane 
!        type or not to replace potential number checking
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
!  Julian Gale, NRI, Curtin University, November 2008
!
  use configurations, only : nregionno, nregiontype, QMMMmode
  use current
  use four
  use molecule
  use parallel
  implicit none
!
!  Local variables
!
  integer(i4)                                  :: i
  integer(i4)                                  :: ii
  integer(i4)                                  :: ind
  integer(i4)                                  :: indm
  integer(i4)                                  :: indmj
  integer(i4)                                  :: indmk
  integer(i4)                                  :: indml
  integer(i4)                                  :: ix
  integer(i4)                                  :: iy
  integer(i4)                                  :: iz
  integer(i4)                                  :: ixi
  integer(i4)                                  :: iyi
  integer(i4)                                  :: izi
  integer(i4)                                  :: ixj
  integer(i4)                                  :: iyj
  integer(i4)                                  :: izj
  integer(i4)                                  :: ixk
  integer(i4)                                  :: iyk
  integer(i4)                                  :: izk
  integer(i4)                                  :: ixl
  integer(i4)                                  :: iyl
  integer(i4)                                  :: izl
  integer(i4)                                  :: ixx
  integer(i4)                                  :: iyy
  integer(i4)                                  :: izz
  integer(i4)                                  :: j
  integer(i4)                                  :: jj
  integer(i4)                                  :: jjmax
  integer(i4)                                  :: jxx
  integer(i4)                                  :: jyy
  integer(i4)                                  :: jzz
  integer(i4)                                  :: k
  integer(i4)                                  :: kmax
  integer(i4)                                  :: kxx
  integer(i4)                                  :: kyy
  integer(i4)                                  :: kzz
  integer(i4)                                  :: l
  integer(i4)                                  :: ll
  integer(i4)                                  :: llmax
  integer(i4)                                  :: lmax
  integer(i4)                                  :: n
  integer(i4)                                  :: nbtypeij
  integer(i4)                                  :: nbtypeik
  integer(i4)                                  :: nbtypeil
  integer(i4)                                  :: nbtypeij2
  integer(i4)                                  :: nbtypeik2
  integer(i4)                                  :: nbtypeil2
  integer(i4)                                  :: ni
  integer(i4)                                  :: nj
  integer(i4)                                  :: nk
  integer(i4)                                  :: nl
  integer(i4)                                  :: nmi
  integer(i4)                                  :: nmj
  integer(i4)                                  :: nmk
  integer(i4)                                  :: nml
  integer(i4)                                  :: nregioni
  integer(i4)                                  :: nregionj
  integer(i4)                                  :: nregionk
  integer(i4)                                  :: nregionl
  integer(i4)                                  :: nregiontypi
  integer(i4)                                  :: nregiontypj
  integer(i4)                                  :: nregiontypk
  integer(i4)                                  :: nregiontypl
  integer(i4)                                  :: nt1
  integer(i4)                                  :: nt2
  integer(i4)                                  :: nt3
  integer(i4)                                  :: nt4
  integer(i4)                                  :: ntp2
  integer(i4)                                  :: ntp3
  integer(i4)                                  :: ntp4
  integer(i4)                                  :: ntyp1
  integer(i4)                                  :: ntyp2
  integer(i4)                                  :: ntyp3
  integer(i4)                                  :: ntyp4
  integer(i4)                                  :: ntypi
  integer(i4)                                  :: ntypj
  integer(i4)                                  :: ntypk
  integer(i4)                                  :: ntypl
  logical                                      :: l2bonds
  logical                                      :: lbonded
  logical                                      :: lbtyp
  logical                                      :: linter_only
  logical                                      :: lintra_only
  logical                                      :: lmatch
  logical                                      :: lmatch2
  logical                                      :: lmatch3
  logical                                      :: lmatchanyof2
  logical                                      :: lmatchanyof3
  logical                                      :: lmolok
  logical                                      :: lsamemol
  logical                                      :: lneedmol 
  real(dp)                                     :: tr1
  real(dp)                                     :: tr2
  real(dp)                                     :: tr3
  real(dp)                                     :: tr1min
  real(dp)                                     :: tr2min
  real(dp)                                     :: tr3min
  real(dp)                                     :: r21
  real(dp)                                     :: r31
  real(dp)                                     :: r41
  real(dp)                                     :: x21t
  real(dp)                                     :: y21t
  real(dp)                                     :: z21t
  real(dp)                                     :: x31t
  real(dp)                                     :: y31t
  real(dp)                                     :: z31t
  real(dp)                                     :: x41t
  real(dp)                                     :: y41t
  real(dp)                                     :: z41t
  real(dp)                                     :: xc1
  real(dp)                                     :: yc1
  real(dp)                                     :: zc1
  real(dp)                                     :: xc2t
  real(dp)                                     :: yc2t
  real(dp)                                     :: zc2t
  real(dp)                                     :: xc3t
  real(dp)                                     :: yc3t
  real(dp)                                     :: zc3t
  real(dp)                                     :: xc4t
  real(dp)                                     :: yc4t
  real(dp)                                     :: zc4t
!*************************
!  Loop over potentials  *
!*************************
  pots: do n = 1,nfor
    if (.not.loutofplane(n)) cycle pots
    ntp2 = 2
    ntp3 = 3
    ntp4 = 4
    nt1 = nfspec1(n)
    nt2 = nfspec2(n)
    nt3 = nfspec3(n)
    nt4 = nfspec4(n)
    ntyp1 = nfptyp1(n)
    ntyp2 = nfptyp2(n)
    ntyp3 = nfptyp3(n)
    ntyp4 = nfptyp4(n)
    tr1 = for1(n)**2
    tr2 = for2(n)**2
    tr3 = for3(n)**2
    tr1min = for1min(n)**2
    tr2min = for2min(n)**2
    tr3min = for3min(n)**2
    lintra_only = (lfintra(n).and..not.lfinter(n))
    linter_only = (lfinter(n).and..not.lfintra(n))
    lbtyp = (mmfexc(n).eq.1)
    lneedmol  =  (lintra_only.or.linter_only.or.lbtyp)
!*****************************
!  Loop over end site 1 / i  *
!*****************************
    iloop: do i = 1,numat
      ni = nat(i)
      ntypi = nftype(i)
      nregioni = nregionno(nsft+nrelat(i))
      nregiontypi = nregiontype(nregioni,ncf)
!
!  Check i is allowed for n
!
      if (.not.lmatch(ni,ntypi,nt1,ntyp1,.true.)) cycle iloop
!
!  QM/MM handling : i is a QM atom and potential is of bonded type => exclude
!
      if (QMMMmode(ncf).gt.0) then
        if (nregiontypi.eq.1.and.lbtyp) cycle iloop
      endif
!
!  Only 3 bonds check
!
      if (lbtyp.and.lonly3oop(n).and.nbonds(i).ne.3) cycle iloop
!
!  i has been accepted
!
      xc1 = xclat(i)
      yc1 = yclat(i)
      zc1 = zclat(i)
!
!  Molecule handling
!
      if (lmol.and.lneedmol) then
        nmi = natmol(i)
        if (ndim.gt.0) then
          indm = nmolind(i)
          call mindtoijk(indm,ixi,iyi,izi)
        endif
      endif
!***********************************
!  Loop over first end site 2 / j  *
!***********************************
      jloop: do j = 1,numat
        nj = nat(j)
        ntypj = nftype(j)
        nregionj = nregionno(nsft+nrelat(j))
        nregiontypj = nregiontype(nregionj,ncf)
!
!  Check j is allowed for n
!
        lmatch3 = lmatchanyof3(nj,ntypj,ntp2,nt2,ntyp2,tr1,tr1min,ntp3,nt3,ntyp3,tr2,tr2min,ntp4,nt4,ntyp4,tr3,tr3min)
        if (.not.lmatch3) cycle jloop
!
        if (lmol.and.lneedmol) then
!
!  Molecule handling
!
          nmj = natmol(j)
          if (ndim.gt.0) then
            indmj = nmolind(j)
            call mindtoijk(indmj,ixj,iyj,izj)
            ixj = ixj - ixi
            iyj = iyj - iyi
            izj = izj - izi
          endif
          lmolok = (nmi.eq.nmj.and.nmi.gt.0)
        else
          lmolok = .false.
        endif
!
!  Check for intra and but not in same molecule
!
        if (lintra_only.and..not.lmolok) cycle jloop
        if (lbtyp.and..not.lmolok) cycle jloop
        xc2t = xclat(j)
        yc2t = yclat(j)
        zc2t = zclat(j)
        x21t = xc2t - xc1
        y21t = yc2t - yc1
        z21t = zc2t - zc1
!
!  Check r21 is OK
!  Loop over cell vectors
!
        iiloop: do ii = 1,iimax
          r21 = (xvec1cell(ii)+x21t)**2+ (yvec1cell(ii)+y21t)**2+ (zvec1cell(ii)+z21t)**2
          if (r21.lt.1d-12) cycle iiloop
!
!  Molecule checking
!
          lbonded = .false.
          if (lmolok) then
            if (ndim.eq.0) then
              if (linter_only) cycle iiloop
              if (lbtyp) then
                call bonded(lbonded,l2bonds,nbtypeij,nbtypeij2,i,j,0_i4,0_i4,0_i4)
                if (.not.lbonded) cycle iiloop
              endif
            else
              call lintoijk(ixx,iyy,izz,ii,imaxl,jmaxl,kmaxl)
              if (lbtyp) then
                call bonded(lbonded,l2bonds,nbtypeij,nbtypeij2,i,j,ixx,iyy,izz)
                if (.not.lbonded) cycle iiloop
                lsamemol = (lbonded.or.l2bonds)
              else
                lsamemol = .false.
              endif
              if (.not.lsamemol) then
                call samemol(lsamemol,nmi,ixx,iyy,izz,ixj,iyj,izj)
              endif
              if (lintra_only.and..not.lsamemol) cycle iiloop
              if (linter_only.and.lsamemol) cycle iiloop
            endif
          endif
!
!  Distance checking
!
          if ((r21.gt.tr1.or.r21.lt.tr1min).and.(.not.lbtyp.or..not.lbonded)) cycle iiloop
          if (ndim.eq.0) then
            kmax = j - 1
          else
            kmax = j
          endif
!************************************
!  Loop over second end site 3 / k  *
!************************************
          kloop: do k = 1,kmax
            nk = nat(k)
            ntypk = nftype(k)
            nregionk = nregionno(nsft+nrelat(k))
            nregiontypk = nregiontype(nregionk,ncf)
!
!  Check k is allowed for n
!
            lmatch2 = lmatchanyof2(nk,ntypk,ntp3,nt3,ntyp3,tr2,tr2min,ntp4,nt4,ntyp4,tr3,tr3min)
            if (.not.lmatch2) cycle kloop
!
            if (lmol.and.lneedmol) then
!
!  Molecule handling
!
              nmk = natmol(k)
              if (ndim.gt.0) then
                indmk = nmolind(k)
                call mindtoijk(indmk,ixk,iyk,izk)
                ixk = ixk - ixi
                iyk = iyk - iyi
                izk = izk - izi
              endif
              lmolok = (nmi.eq.nmk.and.nmi.gt.0)
            else
              lmolok = .false.
            endif
!
!  Check for intra and but not in same molecule
!
            if (lintra_only.and..not.lmolok) cycle kloop
            if (lbtyp.and..not.lmolok) cycle kloop
            xc3t = xclat(k)
            yc3t = yclat(k)
            zc3t = zclat(k)
            x31t = xc3t - xc1
            y31t = yc3t - yc1
            z31t = zc3t - zc1
            if (j.eq.k) then
              jjmax = ii-1
            else
              jjmax = iimax
            endif
!
!  Check r31 is OK
!  Loop over cell vectors
!
            jjloop: do jj = 1,jjmax
              r31 = (xvec1cell(jj)+x31t)**2+ (yvec1cell(jj)+y31t)**2+ (zvec1cell(jj)+z31t)**2
              if (r31.lt.1d-12) cycle jjloop
!
!  Prevent atoms i and k being the same atom
!
              if (k.eq.i.and.jj.eq.iimid) cycle jjloop
!
!  Prevent atoms j and k being the same atom
!
              if (k.eq.j.and.jj.eq.ii) cycle jjloop
!
!  Molecule checking
!
              lbonded = .false.
              if (lmolok) then
                if (ndim.eq.0) then
                  if (linter_only) cycle jjloop
                  if (lbtyp) then
                    call bonded(lbonded,l2bonds,nbtypeik,nbtypeik2,i,k,0_i4,0_i4,0_i4)
                    if (.not.lbonded) cycle jjloop
                  endif
                else
                  call lintoijk(jxx,jyy,jzz,jj,imaxl,jmaxl,kmaxl)
                  if (lbtyp) then
                    call bonded(lbonded,l2bonds,nbtypeik,nbtypeik2,i,k,jxx,jyy,jzz)
                    if (.not.lbonded) cycle jjloop
                    lsamemol = (lbonded.or.l2bonds)
                  else
                    lsamemol = .false.
                  endif
                  if (.not.lsamemol) then
                    call samemol(lsamemol,nmi,jxx,jyy,jzz,ixk,iyk,izk)
                  endif
                  if (lintra_only.and..not.lsamemol) cycle jjloop
                  if (linter_only.and.lsamemol) cycle jjloop
                endif
              endif
!
!  Distance checking
!
              if ((r31.gt.tr2.or.r31.lt.tr2min).and.(.not.lbtyp.or..not.lbonded)) cycle jjloop
!
              if (ndim.eq.0) then
                lmax = k - 1
              else
                lmax = k
              endif
!**********************************
!  Loop over last end site 4 / l  *
!**********************************
              lloop: do l = 1,lmax
                nl = nat(l)
                ntypl = nftype(l)
                nregionl = nregionno(nsft+nrelat(l))
                nregiontypl = nregiontype(nregionl,ncf)
!
!  Check l is allowed for n
!
                if (.not.lmatch(nl,ntypl,nt4,ntyp4,.true.)) cycle lloop
!
!  QM/MM handling : i, j, k & l are all QM atoms => exclude
!
                if (QMMMmode(ncf).gt.0) then
                  if (nregiontypi.eq.1.and.nregiontypj.eq.1.and.nregiontypk.eq.1.and.nregiontypl.eq.1) cycle lloop
                endif
!
                if (lmol.and.lneedmol) then
!                 
!  Molecule handling
!
                  nml = natmol(l)
                  if (ndim.gt.0) then
                    indml = nmolind(l)
                    call mindtoijk(indml,ixl,iyl,izl)
                    ixl = ixl - ixi
                    iyl = iyl - iyi
                    izl = izl - izi
                  endif
                  lmolok = (nmi.eq.nml.and.nmi.gt.0)
                else
                  lmolok = .false.
                endif
!
!  Check for intra and but not in same molecule
!
                if (lintra_only.and..not.lmolok) cycle lloop
                if (lbtyp.and..not.lmolok) cycle lloop
                xc4t = xclat(l)
                yc4t = yclat(l)
                zc4t = zclat(l)
                x41t = xc4t - xc1
                y41t = yc4t - yc1
                z41t = zc4t - zc1
!
                if (k.eq.l) then
                  llmax = jj - 1
                else
                  llmax = iimax
                endif
!
!  Check r41 is OK
!  Loop over cell vectors
!
                llloop: do ll = 1,llmax
                  r41 = (xvec1cell(ll)+x41t)**2 + (yvec1cell(ll)+y41t)**2 + (zvec1cell(ll)+z41t)**2
                  if (r41.lt.1d-12) cycle llloop
!             
!  Prevent atoms i and l being the same atom
!             
                  if (l.eq.i.and.ll.eq.iimid) cycle llloop
!  
!  Prevent atoms j and l being the same atom
!               
                  if (l.eq.j.and.ll.eq.ii) cycle llloop
!                 
!  Prevent atoms k and l being the same atom
!                 
                  if (l.eq.k.and.ll.eq.jj) cycle llloop
!
!  Molecule checking
!
                  lbonded = .false.
                  if (lmolok) then
                    if (ndim.eq.0) then
                      if (linter_only) cycle llloop
                      if (lbtyp) then
                        call bonded(lbonded,l2bonds,nbtypeil,nbtypeil2,i,l,0_i4,0_i4,0_i4)
                        if (.not.lbonded) cycle llloop
                      endif
                    else
                      call lintoijk(kxx,kyy,kzz,ll,imaxl,jmaxl,kmaxl)
                      if (lbtyp) then
                        call bonded(lbonded,l2bonds,nbtypeil,nbtypeil2,i,l,kxx,kyy,kzz)
                        if (.not.lbonded) cycle llloop
                        lsamemol = (lbonded.or.l2bonds)
                      else
                        lsamemol = .false.
                      endif
                      if (.not.lsamemol) then
                        call samemol(lsamemol,nmi,kxx,kyy,kzz,ixl,iyl,izl)
                      endif
                      if (lintra_only.and..not.lsamemol) cycle llloop
                      if (linter_only.and.lsamemol) cycle llloop
                    endif
                  endif
!
!  Distance checking
!
                  if ((r41.gt.tr3.or.r41.lt.tr3min).and.(.not.lbtyp.or..not.lbonded)) cycle llloop
!*********************************
!  Valid four-body term located  *
!*********************************
                  nlist4md = nlist4md + 1
                  if (nlist4md.gt.maxlist4) then
                    maxlist4 = nlist4md + 100
                    call changemaxlist4
                  endif
                  ilind(nlist4md) = i + l*(numat+1)
                  jkind(nlist4md) = j + k*(numat+1)
                  nforptr(nlist4md) = n
                  if (ndim.gt.0) then
                    ind = ii - 1
                    ix = (ind/9)
                    ind = ind - ix*9
                    iy = (ind/3)
                    ind = ind - iy*3
                    iz = ind - 1
                    iy = iy - 1
                    ix = ix - 1
                    icell41(nlist4md) = ix + 5 + 10*(iy+5) + 100*(iz+5)
                    ind = jj - 1
                    ix = (ind/9)
                    ind = ind - ix*9
                    iy = (ind/3)
                    ind = ind - iy*3
                    iz = ind - 1
                    iy = iy - 1
                    ix = ix - 1
                    icell42(nlist4md) = ix + 5 + 10*(iy+5) + 100*(iz+5)
                    ind = ll - 1
                    ix = (ind/9)
                    ind = ind - ix*9
                    iy = (ind/3)
                    ind = ind - iy*3
                    iz = ind - 1
                    iy = iy - 1
                    ix = ix - 1
                    icell43(nlist4md) = ix + 5 + 10*(iy+5) + 100*(iz+5)
                  else
                    icell41(nlist4md) = 0
                    icell42(nlist4md) = 0
                    icell43(nlist4md) = 0
                  endif
!
!  End of loops
!
                enddo llloop
              enddo lloop
            enddo jjloop
          enddo kloop
        enddo iiloop
      enddo jloop
    enddo iloop
  enddo pots
!
  return
  end
