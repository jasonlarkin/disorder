  subroutine setlist3
!
!  Subroutine for three-body energy in MD
!  Modified for distance dependent three body terms
!
!  nlist3md = number of valid three body terms
!  i3ind    = pointer to i atom involved
!  j3ind    = pointer to j atom involved
!  k3ind    = pointer to k atom involved
!  icell31  = cell index for first end atom
!  icell32  = cell index for second end atom
!  nthbptr  = pointer to three-body type
!
!  Strategy - sift by potential first, then cutoffs
!
!   1/95 Intra/intermolecular specification added
!   3/95 Bcross potential added
!   3/95 Periodic molecule corrections added
!   6/95 Correction added for asymmetric cutoffs with symmetric potential
!   4/01 Checking altered so that bonding takes precedence over distances
!   5/01 Minimum cut-offs added
!   6/01 Setting of lsamemol altered
!   9/01 lmolq calculations accelerated using lneedmol 
!  10/01 ijkind switched for 3 separate pointers since
!        numbers were exceeding the integer precision
!   2/02 Error in bond checking fixed - j/k swap
!   6/05 Order of deallocations reversed
!   2/07 Bonding types added
!   5/07 QMMM schemes added
!   5/07 Dreiding option added
!   6/07 Checking of species now uses lmatch
!   6/07 Dreiding option bonded2donorJK check added
!   7/07 Checking of bond orders added 
!  10/07 Error in checking of exocyclic attribute for bonds corrected
!  12/07 Unused variables removed
!   5/08 Handling of asymmetric bond orders corrected
!   6/08 Checking of bond numbers added
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
  use configurations, only : nregionno, nregiontype, QMMMmode
  use current
  use m_three
  use molecule
  use parallel
  use times
  implicit none
!
!  Local variables
!
  integer(i4)                                  :: i
  integer(i4)                                  :: ii
  integer(i4)                                  :: imax
  integer(i4)                                  :: in3
  integer(i4)                                  :: ind
  integer(i4)                                  :: indm
  integer(i4)                                  :: indmj
  integer(i4)                                  :: indmk
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
  integer(i4)                                  :: ixx
  integer(i4)                                  :: iyy
  integer(i4)                                  :: izz
  integer(i4)                                  :: j
  integer(i4)                                  :: jj
  integer(i4)                                  :: jjmin
  integer(i4)                                  :: jmax
  integer(i4)                                  :: jmin
  integer(i4)                                  :: jxx
  integer(i4)                                  :: jyy
  integer(i4)                                  :: jzz
  integer(i4)                                  :: k
  integer(i4)                                  :: kmax
  integer(i4),                            save :: maxvector = 100
  integer(i4)                                  :: n
  integer(i4)                                  :: nbotyp11
  integer(i4)                                  :: nbotyp12
  integer(i4)                                  :: nbotyp21
  integer(i4)                                  :: nbotyp22
  integer(i4)                                  :: nbtyp11
  integer(i4)                                  :: nbtyp12
  integer(i4)                                  :: nbtyp21
  integer(i4)                                  :: nbtyp22
  integer(i4)                                  :: nbtypeij
  integer(i4)                                  :: nbtypeik
  integer(i4)                                  :: nbtypeij2
  integer(i4)                                  :: nbtypeik2
  integer(i4)                                  :: ni
  integer(i4)                                  :: nj
  integer(i4)                                  :: nk
  integer(i4)                                  :: nmi
  integer(i4)                                  :: nmid
  integer(i4)                                  :: nmj
  integer(i4)                                  :: nmk
  integer(i4)                                  :: nregioni
  integer(i4)                                  :: nregionj
  integer(i4)                                  :: nregionk
  integer(i4)                                  :: nregiontypi
  integer(i4)                                  :: nregiontypj
  integer(i4)                                  :: nregiontypk
  integer(i4)                                  :: nt1
  integer(i4)                                  :: nt2
  integer(i4)                                  :: nt3
  integer(i4)                                  :: nto
  integer(i4)                                  :: ntyp1
  integer(i4)                                  :: ntyp2
  integer(i4)                                  :: ntyp3
  integer(i4)                                  :: ntypi
  integer(i4)                                  :: ntypj
  integer(i4)                                  :: ntypk
  integer(i4)                                  :: ntypo
  integer(i4)                                  :: nvector
  integer(i4)                                  :: status
  logical                                      :: bonded2donor
  logical                                      :: bonded2donorJK
  logical                                      :: l2bonds
  logical                                      :: lbonded
  logical                                      :: lbondnoOK
  logical                                      :: lbondtypeOK
  logical                                      :: lbtyp
  logical                                      :: ldiff23typ
  logical                                      :: ldiff23cut
  logical                                      :: ldiff23bo
  logical                                      :: lmatch
  logical                                      :: lintra_only
  logical                                      :: linter_only
  logical                                      :: lmolok
  logical                                      :: lmolok2
  logical                                      :: lneedmol 
  logical                                      :: lsamemol
  real(dp)                                     :: cputime
  real(dp)                                     :: cut
  real(dp)                                     :: r12
  real(dp)                                     :: r13
  real(dp)                                     :: r23
  real(dp)                                     :: time1
  real(dp)                                     :: time2
  real(dp)                                     :: tr1
  real(dp)                                     :: tr2
  real(dp)                                     :: tr3
  real(dp)                                     :: tr1m
  real(dp)                                     :: tr2m
  real(dp)                                     :: tr3m
  real(dp)                                     :: ttmp
  real(dp)                                     :: ttr1
  real(dp)                                     :: ttr1m
  real(dp)                                     :: ttr2
  real(dp)                                     :: ttr2m
  real(dp)                                     :: x21
  real(dp)                                     :: y21
  real(dp)                                     :: z21
  real(dp)                                     :: x23
  real(dp)                                     :: y23
  real(dp)                                     :: z23
  real(dp)                                     :: x31
  real(dp)                                     :: y31
  real(dp)                                     :: z31
  real(dp)                                     :: xc1
  real(dp)                                     :: yc1
  real(dp)                                     :: zc1
  real(dp)                                     :: xt21
  real(dp)                                     :: yt21
  real(dp)                                     :: zt21
  real(dp)                                     :: xt31
  real(dp)                                     :: yt31
  real(dp)                                     :: zt31
  real(dp),    dimension(:), allocatable       :: xvec
  real(dp),    dimension(:), allocatable       :: yvec
  real(dp),    dimension(:), allocatable       :: zvec
!
  time1 = cputime()
  nlist3md = 0
!
!  Allocate local memory
!
  if (ndim.gt.0) then
    allocate(xvec(maxvector),stat=status)
    if (status/=0) call outofmemory('setlist3','xvec')
    allocate(yvec(maxvector),stat=status)
    if (status/=0) call outofmemory('setlist3','yvec')
    allocate(zvec(maxvector),stat=status)
    if (status/=0) call outofmemory('setlist3','zvec')
  else
    allocate(xvec(1),stat=status)
    if (status/=0) call outofmemory('setlist3','xvec')
    allocate(yvec(1),stat=status)
    if (status/=0) call outofmemory('setlist3','yvec')
    allocate(zvec(1),stat=status)
    if (status/=0) call outofmemory('setlist3','zvec')
  endif
!
!  Loop over potentials
!
  do n = 1,nthb
    nt1 = ntspec1(n)
    nt2 = ntspec2(n)
    nt3 = ntspec3(n)
    ntyp1 = ntptyp1(n)
    ntyp2 = ntptyp2(n)
    ntyp3 = ntptyp3(n)
    nbtyp11 = n3botype(1,1,n)
    nbtyp12 = n3botype(2,1,n)
    nbtyp21 = n3botype(1,2,n)
    nbtyp22 = n3botype(2,2,n)
    tr1m = thr1min(n)**2
    tr2m = thr2min(n)**2
    tr3m = thr3min(n)**2
    tr1 = thr1(n)**2
    tr2 = thr2(n)**2
    tr3 = thr3(n)**2
!
!  Check that atomic numbers match
!
    ldiff23typ = (ntyp2.eq.ntyp3.and.nt2.eq.nt3)
!
!  ..and the cutoffs..
!
    ldiff23cut = (tr1.eq.tr2.and.tr1m.eq.tr2m)
!
!  ..and the bond orders
!
    ldiff23bo = (nbtyp11.ne.nbtyp21.or.nbtyp12.ne.nbtyp22)
!
    lintra_only = (ltintra(n).and..not.ltinter(n))
    linter_only = (ltinter(n).and..not.ltintra(n))
    lbtyp = (mmtexc(n).eq.1)
    lneedmol = (lintra_only.or.linter_only.or.lbtyp)
!
!  Create lattice vectors
!
    if (ndim.gt.0) then
      cut = tr1
      if (tr2.gt.cut) cut = tr2
      cut = sqrt(cut)
      call rtlist(nvector,cut,xvec,yvec,zvec,imax,jmax,kmax,nmid,maxvector)
      if (nvector.gt.maxvector) then
!
!  Too many vectors
!
        deallocate(zvec,stat=status)
        if (status/=0) call deallocate_error('setlist3','zvec')
        deallocate(yvec,stat=status)
        if (status/=0) call deallocate_error('setlist3','yvec')
        deallocate(xvec,stat=status)
        if (status/=0) call deallocate_error('setlist3','xvec')
        maxvector = nint(1.1*nvector)
        allocate(xvec(maxvector),stat=status)
        if (status/=0) call outofmemory('setlist3','xvec')
        allocate(yvec(maxvector),stat=status)
        if (status/=0) call outofmemory('setlist3','yvec')
        allocate(zvec(maxvector),stat=status)
        if (status/=0) call outofmemory('setlist3','zvec')
        call rtlist(nvector,cut,xvec,yvec,zvec,imax,jmax,kmax,nmid,maxvector)
      endif
    else
      nvector = 1
      nmid = 1
      xvec(1) = 0.0_dp
      yvec(1) = 0.0_dp
      zvec(1) = 0.0_dp
    endif
!
!  Outer loop over sites
!
    iloop: do i = 1,numat
      ni = nat(i)
      ntypi = nftype(i)
!
!  Check i is allowed for n
!
      if (.not.lmatch(ni,ntypi,nt1,ntyp1,.true.)) cycle iloop
      nregioni = nregionno(nsft+nrelat(i))
      nregiontypi = nregiontype(nregioni,ncf)
!
!  QM/MM handling
!
      if (QMMMmode(ncf).gt.0) then
        if (nregiontypi.eq.1.and.lbtyp) cycle iloop
      endif
! 
!  Dreiding option handling                 
! 
      if (ltdreiding(n)) then               
        if (.not.bonded2donor(i)) cycle iloop
      endif
!
      jmin = 1
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
!     
!  Check number of bonds if necessary
!  
      if (n3bondnono(1,n).gt.0) then
        lbondnoOK = .false.
        do in3 = 1,n3bondnono(1,n)
          if (nbonds(i).eq.n3bondno(in3,1,n)) lbondnoOK = .true.
        enddo
        if (.not.lbondnoOK) cycle iloop
      endif
      if (n3bondnono(2,n).gt.0) then
        lbondnoOK = .true.
        do in3 = 1,n3bondnono(2,n)
          if (nbonds(i).eq.n3bondno(in3,2,n)) lbondnoOK = .false.
        enddo
        if (.not.lbondnoOK) cycle iloop
      endif
!
!  Inner loop over first site
!
      jloop: do j = jmin,numat
        nj = nat(j)
        ntypj = nftype(j)
        nregionj = nregionno(nsft+nrelat(j))
        nregiontypj = nregiontype(nregionj,ncf)
!
!  Check j is allowed for n
!
        if (lmatch(nj,ntypj,nt2,ntyp2,.false.)) then
          nto = nt3
          ntypo = ntyp3
          nbotyp11 = nbtyp11
          nbotyp12 = nbtyp12
          nbotyp21 = nbtyp21
          nbotyp22 = nbtyp22
          ttr1m = tr1m
          ttr2m = tr2m
          ttr1 = tr1
          ttr2 = tr2
        elseif (lmatch(nj,ntypj,nt3,ntyp3,.true.)) then
          nto = nt2
          ntypo = ntyp2
          nbotyp11 = nbtyp21
          nbotyp12 = nbtyp22
          nbotyp21 = nbtyp11
          nbotyp22 = nbtyp12
          ttr1m = tr2m
          ttr2m = tr1m
          ttr1 = tr2
          ttr2 = tr1
        else
          cycle jloop
        endif
        x21 = xclat(j) - xc1
        y21 = yclat(j) - yc1
        z21 = zclat(j) - zc1
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
!
!  Check r12 is OK
!  Loop over cell vectors
!
        iiloop: do ii = 1,nvector
          r12 = (xvec(ii) + x21)**2 + (yvec(ii) + y21)**2 + (zvec(ii) + z21)**2
          if (r12.lt.1.0d-10) cycle iiloop
!
!  Molecule checking
!
          lbonded  =  .false.
          if (lmolok) then
            if (ndim.eq.0) then
              if (linter_only) cycle iiloop
              if (lbtyp) then
                call bonded(lbonded,l2bonds,nbtypeij,nbtypeij2,i,j,0_i4,0_i4,0_i4)
                if (.not.lbonded) cycle iiloop
              endif
            else
              call lintoijk(ixx,iyy,izz,ii,imax,jmax,kmax)
              if (lbtyp) then
                call bonded(lbonded,l2bonds,nbtypeij,nbtypeij2,i,j,ixx,iyy,izz)
                if (.not.lbonded) cycle iiloop
                lsamemol  =  (lbonded.or.l2bonds)
              else
                lsamemol  =  .false.
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
          if ((r12.gt.ttr1.or.r12.lt.ttr1m).and.(.not.lbtyp.or..not.lbonded)) then
            if (ldiff23typ.or..not.ldiff23cut) cycle iiloop
            if (r12.lt.ttr2.and.r12.gt.ttr2m) then
              ttmp = ttr2m
              ttr2m = ttr1m
              ttr1m = ttmp
              ttmp = ttr2
              ttr2 = ttr1
              ttr1 = ttmp
            else
              cycle iiloop
            endif
          endif
!
!  Inner loop over second site
!
          kloop: do k = j,numat
            nk = nat(k)
            ntypk = nftype(k)
            nregionk = nregionno(nsft+nrelat(k))
            nregiontypk = nregiontype(nregionk,ncf)
!
!  Check k is allowed for n, and not equivalent to j
!
            if (.not.lmatch(nk,ntypk,nto,ntypo,.true.)) cycle kloop
!  
!  QM/MM handling
!           
            if (QMMMmode(ncf).gt.0) then
              if (nregiontypi.eq.1.and.nregiontypj.eq.1.and.nregiontypk.eq.1) cycle kloop
            endif
!
!  Dreiding option handling
!
            if (ltdreiding(n)) then
              if (.not.bonded2donorJK(i,j,k)) cycle kloop
            endif
!
            x31 = xclat(k) - xc1
            y31 = yclat(k) - yc1
            z31 = zclat(k) - zc1
            if (j.eq.k) then
              jjmin = ii + 1
            else
              jjmin = 1
            endif
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
              lmolok2 = (nmi.eq.nmk.and.nmi.gt.0)
            else
              lmolok2 = .false.
            endif
!
!  Check for intra and but not in same molecule
!
            if (lintra_only.and..not.lmolok2) cycle kloop
            if (lbtyp.and..not.lmolok2) cycle kloop
!
!  Check r13 is OK
!  Loop over cell vectors
!
            jjloop: do jj = jjmin,nvector
              r13 = (xvec(jj) + x31)**2 + (yvec(jj) + y31)**2 + (zvec(jj) + z31)**2
              if (r13.lt.1.0d-10) cycle jjloop
!
!  Molecule checking
!
              lbonded  =  .false.
              if (lmolok2) then
                if (ndim.eq.0) then
                  if (linter_only) cycle jjloop
                  if (lbtyp) then
                    call bonded(lbonded,l2bonds,nbtypeik,nbtypeik2,i,k,0_i4,0_i4,0_i4)
                    if (.not.lbonded) cycle jjloop
                  endif
                else
                  call lintoijk(jxx,jyy,jzz,jj,imax,jmax,kmax)
                  if (lbtyp) then
                    call bonded(lbonded,l2bonds,nbtypeik,nbtypeik2,i,k,jxx,jyy,jzz)
                    if (.not.lbonded) cycle jjloop
                    lsamemol  =  (lbonded.or.l2bonds)
                  else
                    lsamemol  =  .false.
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
!  Modification to handle case where species for 2 and 3 are the same
!  but cutoffs are different
!
              if ((r13.gt.ttr2.or.r13.lt.ttr2m).and.(.not.lbtyp.or..not.lbonded)) then
                if (ldiff23typ.or..not.ldiff23cut) cycle jjloop
                if (r12.gt.ttr2.or.r13.gt.ttr1) cycle jjloop
                if (r12.lt.ttr2m.or.r13.lt.ttr1m) cycle jjloop
              endif
              if (lbtyp) then
!
!  Bond type checking
!  
!  If we've made it this far then the atoms must be bonded. We just have
!  to check if the bond orders match.
!               
                lbondtypeOK = .true.
!               
!  Check i-j bond for correct order
!               
                if (nbotyp11.gt.0.and.nbotyp11.ne.nbtypeij) lbondtypeOK = .false.
                if (nbotyp12.gt.1.and.nbotyp12.ne.nbtypeij2) lbondtypeOK = .false.
!               
!  Check i-k bond for correct order
!               
                if (nbotyp21.gt.0.and.nbotyp21.ne.nbtypeik) lbondtypeOK = .false.
                if (nbotyp22.gt.1.and.nbotyp22.ne.nbtypeik2) lbondtypeOK = .false.
!               
                if (.not.lbondtypeOK) then
!  
!  If bond types don't match, but atom types are symmetric then try other permutation
!                 
                  if (.not.ldiff23typ.or..not.ldiff23bo) cycle jjloop
!  
!  Check i-j bond for correct order
!                 
                  if (nbotyp21.gt.0.and.nbotyp21.ne.nbtypeij) cycle jjloop
                  if (nbotyp22.gt.1.and.nbotyp22.ne.nbtypeij2) cycle jjloop
!               
!  Check i-k bond for correct order
!                 
                  if (nbotyp11.gt.0.and.nbotyp11.ne.nbtypeik) cycle jjloop
                  if (nbotyp12.gt.1.and.nbotyp12.ne.nbtypeik2) cycle jjloop
                endif
              endif
!
!  Check r23 is OK
!
              xt31 = x31 + xvec(jj)
              yt31 = y31 + yvec(jj)
              zt31 = z31 + zvec(jj)
              xt21 = x21 + xvec(ii)
              yt21 = y21 + yvec(ii)
              zt21 = z21 + zvec(ii)
              x23 = xt31 - xt21
              y23 = yt31 - yt21
              z23 = zt31 - zt21
              r23 = x23**2 + y23**2 + z23**2
              if (r23.gt.tr3.and..not.lbtyp) cycle jjloop
              if (r23.lt.tr3m.or.r23.lt.1.0d-10) cycle jjloop
!
!  Valid three - body term  = > store pointer
!
              nlist3md = nlist3md + 1
              if (nlist3md.gt.maxlist3) then
                maxlist3  =  nlist3md  +  100
                call changemaxlist3
              endif
              i3ind(nlist3md)  =  i
              j3ind(nlist3md)  =  j
              k3ind(nlist3md)  =  k
              nthbptr(nlist3md) = n
              if (ndim.gt.0) then
                ind = ii - 1
                ix = (ind/((2*jmax + 1)*(2*kmax + 1)))
                ind = ind - ix*(2*jmax + 1)*(2*kmax + 1)
                iy = (ind/(2*kmax + 1))
                ind = ind - iy*(2*kmax + 1)
                iz = ind - kmax
                iy = iy - jmax
                ix = ix - imax
                icell31(nlist3md) = ix + 5 + 10*(iy + 5) + 100*(iz + 5)
                ind = jj - 1
                ix = (ind/((2*jmax + 1)*(2*kmax + 1)))
                ind = ind - ix*(2*jmax + 1)*(2*kmax + 1)
                iy = (ind/(2*kmax + 1))
                ind = ind - iy*(2*kmax + 1)
                iz = ind - kmax
                iy = iy - jmax
                ix = ix - imax
                icell32(nlist3md) = ix + 5 + 10*(iy + 5) + 100*(iz + 5)
              else
                icell31(nlist3md) = 0
                icell32(nlist3md) = 0
              endif
            enddo jjloop
          enddo kloop
        enddo iiloop
      enddo jloop
!
!  End of outer loops
!
    enddo iloop
  enddo
!
!  Free local memory
!
  deallocate(zvec,stat=status)
  if (status/=0) call deallocate_error('setlist3','zvec')
  deallocate(yvec,stat=status)
  if (status/=0) call deallocate_error('setlist3','yvec')
  deallocate(xvec,stat=status)
  if (status/=0) call deallocate_error('setlist3','xvec')
!
!  Timing
!
  time2 = cputime()
  tthree = tthree + time2 - time1
!
  return
  end
