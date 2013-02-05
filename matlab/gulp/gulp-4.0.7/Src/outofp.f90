  subroutine outofp
!
!  Calculates out of plane distances and prints them out for valid four-body terms
!
!   3/97 First created from torsion.f
!  12/00 Modifications for general periodicity handled - iimid
!       used to replace reference to "14" and call to rlist
!       before hand is assumed.
!   2/01 lintoijk calls now use imaxl,jmaxl and kmaxl
!   4/01 Checking altered so that bonding takes precedence
!        over distances
!   6/01 Setting of lsamemol altered
!   9/01 lmolq calculations accelerated using lneedmol 
!  11/02 Wildcard atom types added
!   6/06 Inversion squared potential added
!   2/07 Bonding types added
!   5/07 QM/MM schemes added
!  12/07 Unused variables removed
!   7/08 New type checking algorithm introduced & logic updated to match calculations
!   8/08 Error in calls to lmatchany corrected
!  11/08 xcosangleangle potential added
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
  use element
  use four
  use iochannels
  use molecule
  use symmetry
  implicit none
!
!  Local variables
!
  character(len=5)                             :: lab1
  character(len=5)                             :: lab2
  character(len=5)                             :: lab3
  character(len=5)                             :: lab4
  integer(i4), dimension(:), allocatable       :: ind1
  integer(i4), dimension(:), allocatable       :: ind2
  integer(i4), dimension(:), allocatable       :: ind3
  integer(i4), dimension(:), allocatable       :: ind4
  integer(i4),                            save :: maxphi = 400
  integer(i4)                                  :: i
  integer(i4)                                  :: ii
  integer(i4)                                  :: ind
  integer(i4)                                  :: indm
  integer(i4)                                  :: indmj
  integer(i4)                                  :: indmk
  integer(i4)                                  :: indml
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
  integer(i4), dimension(:), allocatable       :: nat1
  integer(i4), dimension(:), allocatable       :: nat2
  integer(i4), dimension(:), allocatable       :: nat3
  integer(i4), dimension(:), allocatable       :: nat4
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
  integer(i4)                                  :: nphi
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
  integer(i4)                                  :: ntt2
  integer(i4)                                  :: ntt3
  integer(i4)                                  :: ntt4
  integer(i4), dimension(:), allocatable       :: ntp1
  integer(i4), dimension(:), allocatable       :: ntp2
  integer(i4), dimension(:), allocatable       :: ntp3
  integer(i4), dimension(:), allocatable       :: ntp4
  integer(i4)                                  :: ntyp1
  integer(i4)                                  :: ntyp2
  integer(i4)                                  :: ntyp3
  integer(i4)                                  :: ntyp4
  integer(i4)                                  :: ntypi
  integer(i4)                                  :: ntypj
  integer(i4)                                  :: ntypk
  integer(i4)                                  :: ntypl
  integer(i4)                                  :: status
  logical                                      :: l2bonds
  logical                                      :: lbonded
  logical                                      :: lbtyp
  logical                                      :: lintra_only
  logical                                      :: linter_only
  logical                                      :: lmatch
  logical                                      :: lmatch2
  logical                                      :: lmatch3
  logical                                      :: lmatchanyof2
  logical                                      :: lmatchanyof3
  logical                                      :: lmolok
  logical                                      :: lneedmol 
  logical                                      :: lsamemol
  real(dp)                                     :: cos123
  real(dp)                                     :: cosphi
  real(dp),    dimension(:), allocatable       :: phi
  real(dp)                                     :: r21
  real(dp)                                     :: r213
  real(dp)                                     :: r234
  real(dp)                                     :: r31
  real(dp)                                     :: r32
  real(dp)                                     :: r41
  real(dp)                                     :: r43
  real(dp)                                     :: sin1232
  real(dp)                                     :: sinphi2
  real(dp)                                     :: tr1
  real(dp)                                     :: tr2
  real(dp)                                     :: tr3
  real(dp)                                     :: tr1min
  real(dp)                                     :: tr2min
  real(dp)                                     :: tr3min
  real(dp)                                     :: v213x
  real(dp)                                     :: v213y
  real(dp)                                     :: v213z
  real(dp)                                     :: v234x
  real(dp)                                     :: v234y
  real(dp)                                     :: v234z
  real(dp)                                     :: x21
  real(dp)                                     :: y21
  real(dp)                                     :: z21
  real(dp)                                     :: x21t
  real(dp)                                     :: y21t
  real(dp)                                     :: z21t
  real(dp)                                     :: x31
  real(dp)                                     :: y31
  real(dp)                                     :: z31
  real(dp)                                     :: x31t
  real(dp)                                     :: y31t
  real(dp)                                     :: z31t
  real(dp)                                     :: x32
  real(dp)                                     :: y32
  real(dp)                                     :: z32
  real(dp)                                     :: x41
  real(dp)                                     :: y41
  real(dp)                                     :: z41
  real(dp)                                     :: x41t
  real(dp)                                     :: y41t
  real(dp)                                     :: z41t
  real(dp)                                     :: x43
  real(dp)                                     :: y43
  real(dp)                                     :: z43
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
!
  write(ioout,'(''  Analysis of out of plane distances :'',/)')
  write(ioout,'(''--------------------------------------------------------------------------------'')')
  write(ioout,'(''  Potential    Atom 1     Atom 2     Atom 3     Atom 4        Distance (Angs)'')')
  write(ioout,'(''--------------------------------------------------------------------------------'')')
!*************************
!  Loop over potentials  *
!*************************
  pots: do n = 1,nfor
!
!  If potential type is not out of plane then skip
!
    if (.not.loutofplane(n)) cycle pots
!
!  Re-enter here if arrays have to re-allocated due to maxphi too small
!
5   nphi = 0
!
!  Allocate local memory
!
    allocate(phi(maxphi),stat=status)
    if (status/=0) call outofmemory('outofp','phi')
    allocate(nat1(maxphi),stat=status)
    if (status/=0) call outofmemory('outofp','nat1')
    allocate(nat2(maxphi),stat=status)
    if (status/=0) call outofmemory('outofp','nat2')
    allocate(nat3(maxphi),stat=status)
    if (status/=0) call outofmemory('outofp','nat3')
    allocate(nat4(maxphi),stat=status)
    if (status/=0) call outofmemory('outofp','nat4')
    allocate(ntp1(maxphi),stat=status)
    if (status/=0) call outofmemory('outofp','ntp1')
    allocate(ntp2(maxphi),stat=status)
    if (status/=0) call outofmemory('outofp','ntp2')
    allocate(ntp3(maxphi),stat=status)
    if (status/=0) call outofmemory('outofp','ntp3')
    allocate(ntp4(maxphi),stat=status)
    if (status/=0) call outofmemory('outofp','ntp4')
    allocate(ind1(maxphi),stat=status)
    if (status/=0) call outofmemory('outofp','ind1')
    allocate(ind2(maxphi),stat=status)
    if (status/=0) call outofmemory('outofp','ind2')
    allocate(ind3(maxphi),stat=status)
    if (status/=0) call outofmemory('outofp','ind3')
    allocate(ind4(maxphi),stat=status)
    if (status/=0) call outofmemory('outofp','ind4')
!
    ntt2 = 2
    ntt3 = 3
    ntt4 = 4
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
!********************************
!  Loop over middle site 1 / i  *
!********************************
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
!  Only 3 bonds check
!
      if (lbtyp.and.lonly3oop(n).and.nbonds(i).ne.3) cycle iloop
!
!  QM/MM handling : i is a QM atom and potential is of bonded type => exclude
!
      if (QMMMmode(ncf).gt.0) then
        if (nregiontypi.eq.1.and.lbtyp) cycle iloop
      endif
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
          izi = (indm/100) - 5
          ind = indm-100*(izi + 5)
          iyi = (ind/10) - 5
          ind = ind - 10*(iyi + 5)
          ixi = ind - 5
        endif
      endif
!*****************************
!  Loop over end site 2 / j  *
!*****************************
      jloop: do j = 1,numat
        nj = nat(j)
        ntypj = nftype(j)
        nregionj = nregionno(nsft+nrelat(j))
        nregiontypj = nregiontype(nregionj,ncf)
!
!  Check j is allowed for n
!
        lmatch3 = lmatchanyof3(nj,ntypj,ntt2,nt2,ntyp2,tr1,tr1min,ntt3,nt3,ntyp3,tr2,tr2min,ntt4,nt4,ntyp4,tr3,tr3min)
        if (.not.lmatch3) cycle jloop
!
        if (lmol.and.lneedmol) then
!
!  Molecule handling
!
          nmj = natmol(j)
          if (ndim.gt.0) then
            indmj = nmolind(j)
            izj = (indmj/100) - 5
            ind = indmj - 100*(izj+5)
            iyj = (ind/10) - 5
            ind = ind - 10*(iyj + 5)
            ixj = ind - 5
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
!
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
          r21 = (xvec1cell(ii)+x21t)**2 + (yvec1cell(ii)+y21t)**2 + (zvec1cell(ii)+z21t)**2
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
          r21 = sqrt(r21)
          x21 = x21t + xvec1cell(ii)
          y21 = y21t + yvec1cell(ii)
          z21 = z21t + zvec1cell(ii)
!
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
            lmatch2 = lmatchanyof2(nk,ntypk,ntt3,nt3,ntyp3,tr2,tr2min,ntt4,nt4,ntyp4,tr3,tr3min)
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
!
            xc3t = xclat(k)
            yc3t = yclat(k)
            zc3t = zclat(k)
            x31t = xc3t - xc1
            y31t = yc3t - yc1
            z31t = zc3t - zc1
!
            if (j.eq.k) then
              jjmax = ii - 1
            else
              jjmax = iimax
            endif
!
!  Check r31 is OK
!  Loop over cell vectors
!
            jjloop: do jj = 1,jjmax
              r31 = (xvec1cell(jj)+x31t)**2 + (yvec1cell(jj)+y31t)**2 + (zvec1cell(jj)+z31t)**2
              if (r31.lt.1d-12) cycle jjloop
!
!  Prevent atoms i and k being the same atom
!
              if (k.eq.i.and.(jj.eq.iimid)) cycle jjloop
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
              r31 = sqrt(r31)
              x31 = x31t + xvec1cell(jj)
              y31 = y31t + yvec1cell(jj)
              z31 = z31t + zvec1cell(jj)
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
!************************************
!  Valid out of plane term located  *
!************************************
                  r41 = sqrt(r41)
                  x41 = x41t + xvec1cell(ll)
                  y41 = y41t + yvec1cell(ll)
                  z41 = z41t + zvec1cell(ll)
!
!  Calculate other vectors and distances needed
!
                  x43 = x41-x31
                  y43 = y41-y31
                  z43 = z41-z31
                  r43 = x43*x43 + y43*y43 + z43*z43
                  r43 = sqrt(r43)
                  x32 = x31-x21
                  y32 = y31-y21
                  z32 = z31-z21
                  r32 = x32*x32 + y32*y32 + z32*z32
                  r32 = sqrt(r32)
!
!  Generate perpendicular end vectors -> sin phi
!
!  Cross products
!
                  v213x = y21*z31 - y31*z21
                  v213y = x31*z21 - x21*z31
                  v213z = x21*y31 - x31*y21
                  r213 = v213x*v213x + v213y*v213y + v213z*v213z
                  r213 = sqrt(r213)
                  v234x = y43*z32 - y32*z43
                  v234y = x32*z43 - x43*z32
                  v234z = x43*y32 - x32*y43
                  r234 = v234x*v234x + v234y*v234y + v234z*v234z
                  r234 = sqrt(r234)
                  cosphi = - (v213x*v234x + v213y*v234y + v213z*v234z)
                  cosphi = cosphi/(r213*r234)
                  if (cosphi.gt.1.0_dp) cosphi = 1.0_dp
                  if (cosphi.lt.-1.0_dp) cosphi = - 1.0_dp
                  sinphi2 = 1.0_dp - cosphi*cosphi
!
!  Angle 123
!
                  cos123 = x21*x32 + y21*y32 + z21*z32
                  cos123 = - cos123/(r21*r32)
                  sin1232 = 1.0_dp - cos123*cos123
                  nphi = nphi + 1
                  if (nphi.le.maxphi) then
                    phi(nphi) = sqrt(sin1232*sinphi2)*r21
!
                    nat1(nphi) = ni
                    nat2(nphi) = nj
                    nat3(nphi) = nk
                    nat4(nphi) = nl
                    ntp1(nphi) = ntypi
                    ntp2(nphi) = ntypj
                    ntp3(nphi) = ntypk
                    ntp4(nphi) = ntypl
                    ind1(nphi) = i
                    ind2(nphi) = j
                    ind3(nphi) = k
                    ind4(nphi) = l
                  endif
!
!  End of inner loops over atoms and cell vectors
!
                enddo llloop
              enddo lloop
            enddo jjloop
          enddo kloop
        enddo iiloop
      enddo jloop
    enddo iloop
!
!  Write out valid out of plane distances for potential number n
!
    if (nphi.gt.maxphi) then
!
!  Too many torsional angles - deallocate arrays, increase size and start again
!
      maxphi = nint(1.1*nphi)
      deallocate(ind4,stat=status)
      if (status/=0) call deallocate_error('outofp','ind4')
      deallocate(ind3,stat=status)
      if (status/=0) call deallocate_error('outofp','ind3')
      deallocate(ind2,stat=status)
      if (status/=0) call deallocate_error('outofp','ind2')
      deallocate(ind1,stat=status)
      if (status/=0) call deallocate_error('outofp','ind1')
      deallocate(ntp4,stat=status)
      if (status/=0) call deallocate_error('outofp','ntp4')
      deallocate(ntp3,stat=status)
      if (status/=0) call deallocate_error('outofp','ntp3')
      deallocate(ntp2,stat=status)
      if (status/=0) call deallocate_error('outofp','ntp2')
      deallocate(ntp1,stat=status)
      if (status/=0) call deallocate_error('outofp','ntp1')
      deallocate(nat4,stat=status)
      if (status/=0) call deallocate_error('outofp','nat4')
      deallocate(nat3,stat=status)
      if (status/=0) call deallocate_error('outofp','nat3')
      deallocate(nat2,stat=status)
      if (status/=0) call deallocate_error('outofp','nat2')
      deallocate(nat1,stat=status)
      if (status/=0) call deallocate_error('outofp','nat1')
      deallocate(phi,stat=status)
      if (status/=0) call deallocate_error('outofp','phi')
      goto 5
    endif
    if (nphi.eq.0) then
      write(ioout,'(5x,i3,9x,''No distances found'')')n
    else
      call label(nat1(1),ntp1(1),lab1)
      call label(nat2(1),ntp2(1),lab2)
      call label(nat3(1),ntp3(1),lab3)
      call label(nat4(1),ntp4(1),lab4)
      write(ioout,'(5x,i3,4x,4(2x,a5,1x,i3),7x,f10.6)') &
        n,lab1,ind1(1),lab2,ind2(1),lab3,ind3(1),lab4,ind4(1),phi(1)
      if (nphi.gt.1) then
        do ii = 2,nphi
          call label(nat1(ii),ntp1(ii),lab1)
          call label(nat2(ii),ntp2(ii),lab2)
          call label(nat3(ii),ntp3(ii),lab3)
          call label(nat4(ii),ntp4(ii),lab4)
          write(ioout,'(12x,4(2x,a5,1x,i3),7x,f10.6)') &
            lab1,ind1(ii),lab2,ind2(ii),lab3,ind3(ii),lab4,ind4(ii),phi(ii)
        enddo
      endif
    endif
    write(ioout,'(''--------------------------------------------------------------------------------'')')
!
!  Free local memory
!
    deallocate(ind4,stat=status)
    if (status/=0) call deallocate_error('outofp','ind4')
    deallocate(ind3,stat=status)
    if (status/=0) call deallocate_error('outofp','ind3')
    deallocate(ind2,stat=status)
    if (status/=0) call deallocate_error('outofp','ind2')
    deallocate(ind1,stat=status)
    if (status/=0) call deallocate_error('outofp','ind1')
    deallocate(ntp4,stat=status)
    if (status/=0) call deallocate_error('outofp','ntp4')
    deallocate(ntp3,stat=status)
    if (status/=0) call deallocate_error('outofp','ntp3')
    deallocate(ntp2,stat=status)
    if (status/=0) call deallocate_error('outofp','ntp2')
    deallocate(ntp1,stat=status)
    if (status/=0) call deallocate_error('outofp','ntp1')
    deallocate(nat4,stat=status)
    if (status/=0) call deallocate_error('outofp','nat4')
    deallocate(nat3,stat=status)
    if (status/=0) call deallocate_error('outofp','nat3')
    deallocate(nat2,stat=status)
    if (status/=0) call deallocate_error('outofp','nat2')
    deallocate(nat1,stat=status)
    if (status/=0) call deallocate_error('outofp','nat1')
    deallocate(phi,stat=status)
    if (status/=0) call deallocate_error('outofp','phi')
!
!  End of outer loops
!
  enddo pots
  write(ioout,'(/)')
!
  return
  end
