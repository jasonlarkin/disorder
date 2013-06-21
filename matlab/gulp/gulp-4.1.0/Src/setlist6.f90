  subroutine setlist6
!
!  Subroutine for setting up the lists for six-body potentials
!
!   7/06 Created from sixnos.f
!   2/07 Bonding types and test added
!   5/07 QM/MM schemes added
!  12/07 Unused variables removed
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
!  Copyright Curtin University 2007
!
!  Julian Gale, NRI, Curtin University, December 2007
!
  use configurations, only : nregionno, nregiontype, QMMMmode
  use current
  use molecule
  use six
  use times,         only : tsix
  implicit none
!
!  Local variables
!
  integer(i4)                                  :: i
  integer(i4)                                  :: ii
  integer(i4)                                  :: iiimax
  integer(i4)                                  :: imax
  integer(i4)                                  :: ind
  integer(i4)                                  :: indm
  integer(i4)                                  :: indmj
  integer(i4)                                  :: indmk
  integer(i4)                                  :: indml
  integer(i4)                                  :: indmm
  integer(i4)                                  :: indmn
  integer(i4)                                  :: ix
  integer(i4)                                  :: iy
  integer(i4)                                  :: iz
  integer(i4)                                  :: ixm
  integer(i4)                                  :: ixn
  integer(i4)                                  :: iym
  integer(i4)                                  :: iyn
  integer(i4)                                  :: izm
  integer(i4)                                  :: izn
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
  integer(i4)                                  :: jmax
  integer(i4)                                  :: jmin
  integer(i4)                                  :: jxx
  integer(i4)                                  :: jyy
  integer(i4)                                  :: jzz
  integer(i4)                                  :: k
  integer(i4)                                  :: kmax
  integer(i4)                                  :: kxx
  integer(i4)                                  :: kyy
  integer(i4)                                  :: kzz
  integer(i4)                                  :: l
  integer(i4)                                  :: liimax
  integer(i4)                                  :: ll
  integer(i4)                                  :: lmax
  integer(i4)                                  :: lmin
  integer(i4)                                  :: m
  integer(i4),                            save :: maxvector = 27
  integer(i4)                                  :: mm
  integer(i4)                                  :: mn
  integer(i4)                                  :: mxx
  integer(i4)                                  :: myy
  integer(i4)                                  :: mzz
  integer(i4)                                  :: n
  integer(i4)                                  :: nbtypeij
  integer(i4)                                  :: nbtypeik
  integer(i4)                                  :: nbtypeil
  integer(i4)                                  :: nbtypejm
  integer(i4)                                  :: nbtypejn
  integer(i4)                                  :: nbtypeij2
  integer(i4)                                  :: nbtypeik2
  integer(i4)                                  :: nbtypeil2
  integer(i4)                                  :: nbtypejm2
  integer(i4)                                  :: nbtypejn2
  integer(i4)                                  :: ni
  integer(i4)                                  :: niimax
  integer(i4)                                  :: nj
  integer(i4)                                  :: nk
  integer(i4)                                  :: nl 
  integer(i4)                                  :: nm 
  integer(i4)                                  :: nn 
  integer(i4)                                  :: nmi 
  integer(i4)                                  :: nmj 
  integer(i4)                                  :: nmk 
  integer(i4)                                  :: nml 
  integer(i4)                                  :: nmm 
  integer(i4)                                  :: nmn 
  integer(i4)                                  :: nmax
  integer(i4)                                  :: nmid
  integer(i4)                                  :: nmin
  integer(i4)                                  :: np
  integer(i4)                                  :: nregioni
  integer(i4)                                  :: nregionj
  integer(i4)                                  :: nregionk
  integer(i4)                                  :: nregionl
  integer(i4)                                  :: nregionm
  integer(i4)                                  :: nregionn
  integer(i4)                                  :: nregiontypi
  integer(i4)                                  :: nregiontypj
  integer(i4)                                  :: nregiontypk
  integer(i4)                                  :: nregiontypl
  integer(i4)                                  :: nregiontypm
  integer(i4)                                  :: nregiontypn
  integer(i4)                                  :: nt2
  integer(i4)                                  :: nt3
  integer(i4)                                  :: nt4
  integer(i4)                                  :: nt5
  integer(i4)                                  :: nt6
  integer(i4)                                  :: ntyp2
  integer(i4)                                  :: ntyp3
  integer(i4)                                  :: ntyp4
  integer(i4)                                  :: ntyp5
  integer(i4)                                  :: ntyp6
  integer(i4)                                  :: ntypi
  integer(i4)                                  :: ntypj
  integer(i4)                                  :: ntypk
  integer(i4)                                  :: ntypl
  integer(i4)                                  :: ntypm
  integer(i4)                                  :: ntypn
  integer(i4)                                  :: nvector
  integer(i4)                                  :: nxx
  integer(i4)                                  :: nyy
  integer(i4)                                  :: nzz
  integer(i4)                                  :: status
  logical                                      :: l2bonds
  logical                                      :: lbonded
  logical                                      :: lbtyp
  logical                                      :: linter_only
  logical                                      :: lintra_only
  logical                                      :: lmatch
  logical                                      :: lmolok
  logical                                      :: lneedmol
  logical                                      :: lsamemol
  logical                                      :: ltsym12
  logical                                      :: ltsym34
  logical                                      :: ltsym56
  real(dp)                                     :: cputime
  real(dp)                                     :: cut
  real(dp)                                     :: r212
  real(dp)                                     :: r312
  real(dp)                                     :: r412
  real(dp)                                     :: r522
  real(dp)                                     :: r622
  real(dp)                                     :: time1
  real(dp)                                     :: time2
  real(dp)                                     :: tr1
  real(dp)                                     :: tr2
  real(dp)                                     :: tr3
  real(dp)                                     :: tr4
  real(dp)                                     :: tr5
  real(dp)                                     :: ttr2
  real(dp)                                     :: ttr3
  real(dp)                                     :: ttr4
  real(dp)                                     :: ttr5
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
  real(dp)                                     :: x41
  real(dp)                                     :: y41
  real(dp)                                     :: z41
  real(dp)                                     :: x41t
  real(dp)                                     :: y41t
  real(dp)                                     :: z41t
  real(dp)                                     :: x52
  real(dp)                                     :: y52
  real(dp)                                     :: z52
  real(dp)                                     :: x52t
  real(dp)                                     :: y52t
  real(dp)                                     :: z52t
  real(dp)                                     :: x62
  real(dp)                                     :: y62
  real(dp)                                     :: z62
  real(dp)                                     :: x62t
  real(dp)                                     :: y62t
  real(dp)                                     :: z62t
  real(dp)                                     :: xc2t
  real(dp)                                     :: yc2t
  real(dp)                                     :: zc2t
  real(dp)                                     :: xc3t
  real(dp)                                     :: yc3t
  real(dp)                                     :: zc3t
  real(dp)                                     :: xc4t
  real(dp)                                     :: yc4t
  real(dp)                                     :: zc4t
  real(dp)                                     :: xc5t
  real(dp)                                     :: yc5t
  real(dp)                                     :: zc5t
  real(dp)                                     :: xc6t
  real(dp)                                     :: yc6t
  real(dp)                                     :: zc6t
  real(dp)                                     :: sxyz(3,6)
  real(dp),    dimension(:), allocatable       :: xvec
  real(dp),    dimension(:), allocatable       :: yvec
  real(dp),    dimension(:), allocatable       :: zvec
!
  time1 = cputime()
  nlist6md = 0
!
!  Allocate local memory
!         
  if (ndim.gt.0) then 
    allocate(xvec(maxvector),stat=status)
    if (status/=0) call outofmemory('setlist6','xvec')
    allocate(yvec(maxvector),stat=status)
    if (status/=0) call outofmemory('setlist6','yvec')
    allocate(zvec(maxvector),stat=status)
    if (status/=0) call outofmemory('setlist6','zvec')
  else
    allocate(xvec(1),stat=status)
    if (status/=0) call outofmemory('setlist6','xvec')
    allocate(yvec(1),stat=status)
    if (status/=0) call outofmemory('setlist6','yvec')
    allocate(zvec(1),stat=status)
    if (status/=0) call outofmemory('setlist6','zvec')
  endif
!*************************
!  Loop over potentials  *
!*************************
  do np = 1,nsix
    tr1 = six1(np)**2
    ttr2 = six2(np)**2
    ttr3 = six3(np)**2
    ttr4 = six4(np)**2
    ttr5 = six5(np)**2
    ltsym12 = (lmatch(nsspec1(np),nsptyp1(np),nsspec2(np),nsptyp2(np),.true.).or. &
               lmatch(nsspec2(np),nsptyp2(np),nsspec1(np),nsptyp1(np),.true.))
    lbtyp = (mmsexc(np).eq.1)
    lintra_only = (lsintra(np).and..not.lsinter(np))
    linter_only = (lsinter(np).and..not.lsintra(np))
    lneedmol = (lintra_only.or.linter_only.or.lbtyp)
!
!  Create lattice vectors
!
    if (ndim.gt.0) then
      cut = six1(np) + max(six2(np),six3(np)) + max(six4(np),six5(np))
      call rtlist(nvector,cut,xvec,yvec,zvec,imax,jmax,kmax,nmid,maxvector)
      if (nvector.gt.maxvector) then
!
!  Too many vectors
!
        deallocate(zvec,stat=status)
        if (status/=0) call deallocate_error('setlist6','zvec')
        deallocate(yvec,stat=status)
        if (status/=0) call deallocate_error('setlist6','yvec')
        deallocate(xvec,stat=status)
        if (status/=0) call deallocate_error('setlist6','xvec')
        maxvector = nint(1.1*nvector)
        allocate(xvec(maxvector),stat=status)
        if (status/=0) call outofmemory('setlist6','xvec')
        allocate(yvec(maxvector),stat=status)
        if (status/=0) call outofmemory('setlist6','yvec')
        allocate(zvec(maxvector),stat=status)
        if (status/=0) call outofmemory('setlist6','zvec')
        call rtlist(nvector,cut,xvec,yvec,zvec,imax,jmax,kmax,nmid,maxvector)
      endif
    else
      nvector  =  1
      nmid  =  1
      xvec(1)  =  0.0_dp
      yvec(1)  =  0.0_dp
      zvec(1)  =  0.0_dp
    endif
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
      if (lmatch(ni,ntypi,nsspec1(np),nsptyp1(np),.true.)) then
        nt2 = nsspec2(np)
        ntyp2 = nsptyp2(np)
        nt3 = nsspec3(np)
        nt4 = nsspec4(np)
        nt5 = nsspec5(np)
        nt6 = nsspec6(np)
        ntyp3 = nsptyp3(np)
        ntyp4 = nsptyp4(np)
        ntyp5 = nsptyp5(np)
        ntyp6 = nsptyp6(np)
        tr2 = ttr2
        tr3 = ttr3
        tr4 = ttr4
        tr5 = ttr5
      elseif (lmatch(ni,ntypi,nsspec2(np),nsptyp2(np),.true.)) then
        nt2 = nsspec1(np)
        ntyp2 = nsptyp1(np)
        nt3 = nsspec5(np)
        nt4 = nsspec6(np)
        nt5 = nsspec3(np)
        nt6 = nsspec4(np)
        ntyp3 = nsptyp5(np)
        ntyp4 = nsptyp6(np)
        ntyp5 = nsptyp3(np)
        ntyp6 = nsptyp4(np)
        tr2 = ttr4
        tr3 = ttr5
        tr4 = ttr2
        tr5 = ttr3
      else
        cycle iloop
      endif
!
!  Set flags as to whether potentials are symmetric at the two ends
!
      ltsym34 = (lmatch(nt3,ntyp3,nt4,ntyp4,.true.).or.lmatch(nt4,ntyp4,nt3,ntyp3,.true.))
      ltsym56 = (lmatch(nt5,ntyp5,nt6,ntyp6,.true.).or.lmatch(nt6,ntyp6,nt5,ntyp5,.true.))
!
!  i has been accepted
!
      sxyz(1,1) = xclat(i)
      sxyz(2,1) = yclat(i)
      sxyz(3,1) = zclat(i)
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
!  Set j looping indices
!
      if (ltsym12) then
        jmin = 1
        jmax = i
      else
        jmin = 1
        jmax = numat
      endif
!********************************
!  Loop over middle site 2 / j  *
!********************************
      jloop: do j = jmin,jmax
        nj = nat(j)
        ntypj = nftype(j)
        nregionj = nregionno(nsft+nrelat(j))     
        nregiontypj = nregiontype(nregionj,ncf)
!
!  Check j is allowed for n
!
        if (.not.lmatch(nj,ntypj,nt2,ntyp2,.true.)) cycle jloop
!
!  QM/MM handling : i & j are both QM atoms and potential is of bonded type => exclude
!
        if (QMMMmode(ncf).gt.0) then
          if (nregiontypi.eq.1.and.nregiontypj.eq.1.and.lbtyp) cycle jloop
        endif
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
        x21t = xc2t - sxyz(1,1)
        y21t = yc2t - sxyz(2,1)
        z21t = zc2t - sxyz(3,1)
        if (ltsym12.and.i.eq.j) then
          iiimax = nmid - 1
        else
          iiimax = nvector
        endif
!
!  Check r21 is OK
!  Loop over cell vectors
!
        iiloop: do ii = 1,iiimax
          x21 = x21t + xvec(ii)
          y21 = y21t + yvec(ii)
          z21 = z21t + zvec(ii)
          r212 = x21*x21 + y21*y21 + z21*z21
          if (r212.lt.1d-12) cycle iiloop
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
!
!  Check central bond type for correct order
!
                if (n6botype(1,np).gt.0) then
                  if (n6botype(1,np).ne.nbtypeij) cycle iiloop
                endif
                if (n6botype(2,np).gt.0) then
                  if (n6botype(2,np).ne.nbtypeij2) cycle iiloop
                endif
              endif
            else
              call lintoijk(ixx,iyy,izz,ii,imaxl,jmaxl,kmaxl)
              if (lbtyp) then
                call bonded(lbonded,l2bonds,nbtypeij,nbtypeij2,i,j,ixx,iyy,izz)
                if (.not.lbonded) cycle iiloop
!
!  Check central bond type for correct order
!
                if (n6botype(1,np).gt.0) then
                  if (n6botype(1,np).ne.nbtypeij) cycle iiloop
                endif
                if (n6botype(2,np).gt.0) then
                  if (n6botype(2,np).ne.nbtypeij2) cycle iiloop
                endif
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
          if (r212.gt.tr1.and.(.not.lbtyp.or..not.lbonded)) cycle iiloop
!
!  j has been accepted
!
          sxyz(1,2) = x21 + sxyz(1,1)
          sxyz(2,2) = y21 + sxyz(2,1)
          sxyz(3,2) = z21 + sxyz(3,1)
!*****************************************
!  Loop over first end site bonded to i  *
!*****************************************
          kloop: do k = 1,numat
            nk = nat(k)
            ntypk = nftype(k)
            nregionk = nregionno(nsft+nrelat(k))     
            nregiontypk = nregiontype(nregionk,ncf)
!
!  Check k is allowed for n
!
            if (.not.lmatch(nk,ntypk,nt3,ntyp3,.true.)) cycle kloop
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
            x31t = xc3t - sxyz(1,1)
            y31t = yc3t - sxyz(2,1)
            z31t = zc3t - sxyz(3,1)
!
!  Check r31 is OK
!  Loop over cell vectors
!
            jjloop: do jj = 1,nvector
              x31 = x31t + xvec(jj)
              y31 = y31t + yvec(jj)
              z31 = z31t + zvec(jj)
              r312 = x31*x31 + y31*y31 + z31*z31
              if (r312.lt.1d-12) cycle jjloop
!
!  Prevent atoms i and k being the same atom
!
              if (k.eq.i.and.jj.eq.nmid) cycle jjloop
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
              if (r312.gt.tr2.and.(.not.lbtyp.or..not.lbonded)) cycle jjloop
!
!  k has been accepted
!
              sxyz(1,3) = x31 + sxyz(1,1)
              sxyz(2,3) = y31 + sxyz(2,1)
              sxyz(3,3) = z31 + sxyz(3,1)
!************************************
!  Loop over second end site for i  *
!************************************
!
!  Set l looping indices
!
              if (ltsym34) then
                lmin = 1
                lmax = k
              else
                lmin = 1
                lmax = numat
              endif
              lloop: do l = lmin,lmax
                nl = nat(l)
                ntypl = nftype(l)
                nregionl = nregionno(nsft+nrelat(l))     
                nregiontypl = nregiontype(nregionl,ncf)
!
!  Check l is allowed for n
!
                if (.not.lmatch(nl,ntypl,nt4,ntyp4,.true.)) cycle lloop
!
!  Molecule handling
!
                if (lmol.and.lneedmol) then
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
                x41t = xc4t - sxyz(1,1)
                y41t = yc4t - sxyz(2,1)
                z41t = zc4t - sxyz(3,1)
                if (ltsym34.and.k.eq.l) then
                  liimax = nmid - 1
                else
                  liimax = nvector
                endif
!
!  Check r41 is OK
!  Loop over cell vectors
!
                llloop: do ll = 1,liimax
                  x41 = x41t + xvec(ll)
                  y41 = y41t + yvec(ll)
                  z41 = z41t + zvec(ll)
                  r412 = x41*x41 + y41*y41 + z41*z41
                  if (r412.lt.1d-12) cycle llloop
!
!  Prevent atoms i and l being the same atom
!
                  if (l.eq.i.and.ll.eq.nmid) cycle llloop
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
                  if (r412.gt.tr3.and.(.not.lbtyp.or..not.lbonded)) cycle llloop
!
!  l has been accepted
!
                  sxyz(1,4) = x41 + sxyz(1,1)
                  sxyz(2,4) = y41 + sxyz(2,1)
                  sxyz(3,4) = z41 + sxyz(3,1)
!*****************************************
!  Loop over first end site bonded to j  *
!*****************************************
                  mloop: do m = 1,numat
                    nm = nat(m)
                    ntypm = nftype(m)
                    nregionm = nregionno(nsft+nrelat(m))     
                    nregiontypm = nregiontype(nregionm,ncf)
!
!  Check m is allowed for n
!
                    if (.not.lmatch(nm,ntypm,nt5,ntyp5,.true.)) cycle mloop
                    if (lmol.and.lneedmol) then
!
!  Molecule handling
!
                      nmm = natmol(m)
                      if (ndim.gt.0) then
                        indmm = nmolind(m)
                        call mindtoijk(indmm,ixm,iym,izm)
                        ixm = ixm - ixj
                        iym = iym - iyj
                        izm = izm - izj
                      endif
                      lmolok = (nmi.eq.nmm.and.nmi.gt.0)
                    else
                      lmolok = .false.
                    endif
!
!  Check for intra and but not in same molecule
!
                    if (lintra_only.and..not.lmolok) cycle mloop
                    if (lbtyp.and..not.lmolok) cycle mloop
                    xc5t = xclat(m)
                    yc5t = yclat(m)
                    zc5t = zclat(m)
                    x52t = xc5t - sxyz(1,2)
                    y52t = yc5t - sxyz(2,2)
                    z52t = zc5t - sxyz(3,2)
!
!  Check r52 is OK
!  Loop over cell vectors
!
                    mmloop: do mm = 1,nvector
                      x52 = x52t + xvec(mm)
                      y52 = y52t + yvec(mm)
                      z52 = z52t + zvec(mm)
                      r522 = x52*x52 + y52*y52 + z52*z52
                      if (r522.lt.1d-12) cycle mmloop
!
!  Prevent atoms i and m being the same atom
!
                      if (m.eq.i.and.mm.eq.nmid) cycle mmloop
!
!  Prevent atoms j and m being the same atom
!
                      if (m.eq.j.and.mm.eq.ii) cycle mmloop
!
!  Molecule checking
!
                      lbonded = .false.
                      if (lmolok) then
                        if (ndim.eq.0) then
                          if (linter_only) cycle mmloop
                          if (lbtyp) then
                            call bonded(lbonded,l2bonds,nbtypejm,nbtypejm2,j,m,0_i4,0_i4,0_i4)
                            if (.not.lbonded) cycle mmloop
                          endif
                        else
                          call lintoijk(mxx,myy,mzz,mm,imaxl,jmaxl,kmaxl)
                          if (lbtyp) then
                            call bonded(lbonded,l2bonds,nbtypejm,nbtypejm2,j,m,mxx-ixx,myy-iyy,mzz-izz)
                            if (.not.lbonded) cycle mmloop
                            lsamemol = (lbonded.or.l2bonds)
                          else
                            lsamemol = .false.
                          endif
                          if (.not.lsamemol) then
                            call samemol(lsamemol,nmi,jxx,jyy,jzz,ixm,iym,izm)
                          endif
                          if (lintra_only.and..not.lsamemol) cycle mmloop
                          if (linter_only.and.lsamemol) cycle mmloop
                        endif
                      endif
!
!  Distance checking
!
                      if (r522.gt.tr4.and.(.not.lbtyp.or..not.lbonded)) cycle mmloop
!
!  m has been accepted
!
                      sxyz(1,5) = x52 + sxyz(1,2)
                      sxyz(2,5) = y52 + sxyz(2,2)
                      sxyz(3,5) = z52 + sxyz(3,2)
!************************************
!  Loop over second end site for j  *
!************************************
!
!  Set n looping indices
!
                      if (ltsym56) then
                        nmin = 1
                        nmax = m
                      else
                        nmin = 1
                        nmax = numat
                      endif
                      nloop: do n = nmin,nmax
                        nn = nat(n)
                        ntypn = nftype(n)
                        nregionn = nregionno(nsft+nrelat(n))     
                        nregiontypn = nregiontype(nregionn,ncf)
!
!  Check n is allowed for n
!
                        if (.not.lmatch(nn,ntypn,nt6,ntyp6,.true.)) cycle nloop
!
!  QM/MM handling : i, j, k, l, m and n are all QM atoms => exclude
!
                        if (QMMMmode(ncf).gt.0) then
                          if (nregiontypi.eq.1.and.nregiontypj.eq.1.and.nregiontypk.eq.1.and.nregiontypl.eq.1.and. &
                              nregiontypm.eq.1.and.nregiontypn.eq.1) cycle nloop
                        endif
!
!  Molecule handling
!
                        if (lmol.and.lneedmol) then
                          nmn = natmol(n)
                          if (ndim.gt.0) then
                            indmn = nmolind(n)
                            call mindtoijk(indmn,ixn,iyn,izn)
                            ixn = ixn - ixj
                            iyn = iyn - iyj
                            izn = izn - izj
                          endif
                          lmolok = (nmi.eq.nmn.and.nmi.gt.0)
                        else
                          lmolok = .false.
                        endif
!
!  Check for intra and but not in same molecule
!
                        if (lintra_only.and..not.lmolok) cycle nloop
                        if (lbtyp.and..not.lmolok) cycle nloop
                        xc6t = xclat(n)
                        yc6t = yclat(n)
                        zc6t = zclat(n)
                        x62t = xc6t - sxyz(1,2)
                        y62t = yc6t - sxyz(2,2)
                        z62t = zc6t - sxyz(3,2)
                        if (ltsym56.and.m.eq.n) then
                          niimax = nmid - 1
                        else
                          niimax = nvector
                        endif
!
!  Check r62 is OK
!  Loop over cell vectors
!
                        mnloop: do mn = 1,niimax
                          x62 = x62t + xvec(mn)
                          y62 = y62t + yvec(mn)
                          z62 = z62t + zvec(mn)
                          r622 = x62*x62 + y62*y62 + z62*z62
                          if (r622.lt.1d-12) cycle mnloop
!
!  Prevent atoms i and n being the same atom
!
                          if (n.eq.i.and.mn.eq.nmid) cycle mnloop
!
!  Prevent atoms j and n being the same atom
!
                          if (n.eq.j.and.mn.eq.ii) cycle mnloop
!
!  Prevent atoms m and n being the same atom
!
                          if (n.eq.m.and.mn.eq.mm) cycle mnloop
!
!  Molecule checking
!
                          lbonded = .false.
                          if (lmolok) then
                            if (ndim.eq.0) then
                              if (linter_only) cycle mnloop
                              if (lbtyp) then
                                call bonded(lbonded,l2bonds,nbtypejn,nbtypejn2,j,n,0_i4,0_i4,0_i4)
                                if (.not.lbonded) cycle mnloop
                              endif
                            else
                              call lintoijk(nxx,nyy,nzz,mn,imaxl,jmaxl,kmaxl)
                              if (lbtyp) then
                                call bonded(lbonded,l2bonds,nbtypejn,nbtypejn2,j,n,nxx-ixx,nyy-iyy,nzz-izz)
                                if (.not.lbonded) cycle mnloop
                                lsamemol = (lbonded.or.l2bonds)
                              else
                                lsamemol = .false.
                              endif
                              if (.not.lsamemol) then
                                call samemol(lsamemol,nmi,jxx,jyy,jzz,ixn,iyn,izn)
                              endif
                              if (lintra_only.and..not.lsamemol) cycle mnloop
                              if (linter_only.and.lsamemol) cycle mnloop
                            endif
                          endif
!
!  Distance checking
!
                          if (r622.gt.tr5.and.(.not.lbtyp.or..not.lbonded)) cycle mnloop
!
!  n has been accepted
!
                          sxyz(1,6) = x62 + sxyz(1,2)
                          sxyz(2,6) = y62 + sxyz(2,2)
                          sxyz(3,6) = z62 + sxyz(3,2)
!********************************
!  Valid six-body term located  *
!********************************
                          nlist6md = nlist6md + 1
                          if (nlist6md.gt.maxlist6) then
                            maxlist6 = nlist6md + 100
                            call changemaxlist6
                          endif
                          ijind(nlist6md) = i + j*(numat+1)
                          klind(nlist6md) = k + l*(numat+1)
                          mnind(nlist6md) = m + n*(numat+1)
                          nsixptr(nlist6md) = np
                          if (ndim.eq.3) then
                            ind = ii - 1
                            ix = (ind/9)
                            ind = ind - ix*9
                            iy = (ind/3)
                            ind = ind - iy*3
                            iz = ind - 1
                            iy = iy - 1
                            ix = ix - 1
                            icell61(nlist6md) = ix + 5 + 10*(iy+5) + 100*(iz+5)
                            ind = jj - 1
                            ix = (ind/9)
                            ind = ind - ix*9
                            iy = (ind/3)
                            ind = ind - iy*3
                            iz = ind - 1
                            iy = iy - 1
                            ix = ix - 1
                            icell62(nlist6md) = ix + 5 + 10*(iy+5) + 100*(iz+5)
                            ind = ll - 1
                            ix = (ind/9)
                            ind = ind - ix*9
                            iy = (ind/3)
                            ind = ind - iy*3
                            iz = ind - 1
                            iy = iy - 1
                            ix = ix - 1
                            icell63(nlist6md) = ix + 5 + 10*(iy+5) + 100*(iz+5)
                            ind = mm - 1
                            ix = (ind/9)
                            ind = ind - ix*9
                            iy = (ind/3)
                            ind = ind - iy*3
                            iz = ind - 1
                            iy = iy - 1
                            ix = ix - 1
                            icell64(nlist6md) = ix + 5 + 10*(iy+5) + 100*(iz+5)
                            ind = mn - 1
                            ix = (ind/9)
                            ind = ind - ix*9
                            iy = (ind/3)
                            ind = ind - iy*3
                            iz = ind - 1
                            iy = iy - 1
                            ix = ix - 1
                            icell65(nlist6md) = ix + 5 + 10*(iy+5) + 100*(iz+5)
                          elseif (ndim.eq.2) then
                            ind = ii - 1
                            ix = (ind/3)
                            ind = ind - ix*3
                            iy = ind - 1
                            ix = ix - 1
                            iz = 0
                            icell61(nlist6md) = ix + 5 + 10*(iy+5) + 100*(iz+5)
                            ind = jj - 1
                            ix = (ind/3)
                            ind = ind - ix*3
                            iy = ind - 1
                            ix = ix - 1
                            iz = 0
                            icell62(nlist6md) = ix + 5 + 10*(iy+5) + 100*(iz+5)
                            ind = ll - 1
                            ix = (ind/3)
                            ind = ind - ix*3
                            iy = ind - 1
                            ix = ix - 1
                            iz = 0
                            icell63(nlist6md) = ix + 5 + 10*(iy+5) + 100*(iz+5)
                            ind = mm - 1
                            ix = (ind/3)
                            ind = ind - ix*3
                            iy = ind - 1
                            ix = ix - 1
                            iz = 0
                            icell64(nlist6md) = ix + 5 + 10*(iy+5) + 100*(iz+5)
                            ind = mn - 1
                            ix = (ind/3)
                            ind = ind - ix*3
                            iy = ind - 1
                            ix = ix - 1
                            iz = 0
                            icell65(nlist6md) = ix + 5 + 10*(iy+5) + 100*(iz+5)
                          elseif (ndim.eq.1) then
                            ind = ii - 1
                            ix = ind - 1
                            iy = 0
                            iz = 0
                            icell61(nlist6md) = ix + 5 + 10*(iy+5) + 100*(iz+5)
                            ind = jj - 1
                            ix = ind - 1
                            iy = 0
                            iz = 0
                            icell62(nlist6md) = ix + 5 + 10*(iy+5) + 100*(iz+5)
                            ind = ll - 1
                            ix = ind - 1
                            iy = 0
                            iz = 0
                            icell63(nlist6md) = ix + 5 + 10*(iy+5) + 100*(iz+5)
                            ind = mm - 1
                            ix = ind - 1
                            iy = 0
                            iz = 0
                            icell64(nlist6md) = ix + 5 + 10*(iy+5) + 100*(iz+5)
                            ind = mn - 1
                            ix = ind - 1
                            iy = 0
                            iz = 0
                            icell65(nlist6md) = ix + 5 + 10*(iy+5) + 100*(iz+5)
                          else
                            icell61(nlist6md) = 0
                            icell62(nlist6md) = 0
                            icell63(nlist6md) = 0
                            icell64(nlist6md) = 0
                            icell65(nlist6md) = 0
                          endif
!
!  End of inner loops over atoms and cell vectors
!
                        enddo mnloop
                      enddo nloop
                    enddo mmloop
                  enddo mloop
                enddo llloop
              enddo lloop
            enddo jjloop
          enddo kloop
        enddo iiloop
      enddo jloop
    enddo iloop
!
!  End of outer loops
!
  enddo
!
!  Free local memory
!
  deallocate(zvec,stat=status)
  if (status/=0) call deallocate_error('setlist6','zvec')
  deallocate(yvec,stat=status)
  if (status/=0) call deallocate_error('setlist6','yvec')
  deallocate(xvec,stat=status)
  if (status/=0) call deallocate_error('setlist6','xvec')
!
!  Timing
!
  time2 = cputime()
  tsix = tsix + time2 - time1
!
  return
  end
