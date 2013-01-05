  subroutine BOself(eboQself,lgrad1,lgrad2,lPhonon)
!
!  Calculates the self energy for the bond order charges.
!
!  On entry :
!
!  lgrad1       = if .true. calculate first derivatives
!  lgrad2       = if .true. calculate second derivatives
!  lPhonon      = if .true. then exclude strain derivatives
!
!  On exit :
!
!  eboQself     = the self energy of the bond order charges
!
!   8/04 Created from bondorder.f
!   9/04 Derivatives added
!   9/04 Strain derivatives turned off for phonon case
!   9/04 Partial occupancies added
!  10/04 Modified so that only upper half triangular derv2 used
!  10/04 Error in logic for when to calculate term corrected
!  10/04 Error in sign for d2edq2 for anion case corrected
!  10/04 Strain terms moved outside atom loop
!   2/05 Rho value added
!  11/07 Unused variables cleaned up
!   1/08 Parallelisation added
!  11/09 Region derivatives added
!  11/11 Region-region energy contributions stored
!   5/12 Atomic stress added
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
!  Julian Gale, NRI, Curtin University, May 2012
!
  use datatypes
  use bondorderdata
  use configurations, only : nregions, nregionno
  use control,        only : lDoQDeriv1, lDoQDeriv2, latomicstress
  use current
  use derivatives,    only : dqdxyz, d2qdxyz2, d2qdxyzs, dqds, d2qds2, nqatoms, nqatomptr
  use derivatives,    only : xdrv, ydrv, zdrv, rstrd, derv2, derv3, sderv2
  use derivatives,    only : xregdrv, yregdrv, zregdrv, atomicstress
  use energies,       only : esregion2, eregion2region
  use optimisation,   only : lfreeze, lopf
  use parallel,       only : procid, nprocs
  use symmetry,       only : lstr, lsymopt, lsymderv
  use times
  implicit none
!
!  Passed variables
!
  real(dp),    intent(out)                       :: eboQself
  logical,     intent(in)                        :: lgrad1
  logical,     intent(in)                        :: lgrad2
  logical,     intent(in)                        :: lPhonon
!
!  Local variables
!
  integer(i4)                                    :: i
  integer(i4)                                    :: ii
  integer(i4)                                    :: ind
  integer(i4)                                    :: ix
  integer(i4)                                    :: iy
  integer(i4)                                    :: iz
  integer(i4)                                    :: j
  integer(i4)                                    :: jx
  integer(i4)                                    :: jy
  integer(i4)                                    :: jz
  integer(i4)                                    :: k
  integer(i4)                                    :: kk
  integer(i4)                                    :: kl
  integer(i4)                                    :: kx
  integer(i4)                                    :: ky
  integer(i4)                                    :: kz
  integer(i4)                                    :: m
  integer(i4)                                    :: mm
  integer(i4)                                    :: mn
  integer(i4)                                    :: mx
  integer(i4)                                    :: my
  integer(i4)                                    :: mz
  integer(i4)                                    :: mmxx
  integer(i4)                                    :: mmxy
  integer(i4)                                    :: mmxz
  integer(i4)                                    :: mmyy
  integer(i4)                                    :: mmyz
  integer(i4)                                    :: mmzz
  integer(i4)                                    :: n
  integer(i4)                                    :: nati
  integer(i4)                                    :: nloop
  integer(i4)                                    :: nregioni
  integer(i4)                                    :: nregionj
  integer(i4)                                    :: ntypi
  logical                                        :: lDoTerm
  logical                                        :: lopi
  logical                                        :: lopj
  logical                                        :: lreg2pair
  logical                                        :: lsym1
  real(dp)                                       :: cputime
  real(dp)                                       :: dedq
  real(dp)                                       :: d2edq2
  real(dp)                                       :: dneqv
  real(dp)                                       :: esum
  real(dp)                                       :: oci
  real(dp)                                       :: qi
  real(dp)                                       :: rho
  real(dp)                                       :: t1
  real(dp)                                       :: t2
  real(dp)                                       :: xdrvk
  real(dp)                                       :: ydrvk
  real(dp)                                       :: zdrvk
!
  t1 = cputime()
  lsym1 = (lsymopt.and.lsymderv)
!
!  Initialise Bond Order charge self energy
!
  eboQself = 0.0_dp
!*************************************
!  Calculate self-energy of charges  *
!*************************************
  if (lsym1) then
    nloop = nasym
  else
    nloop = numat
  endif
  do i = 1+procid,nloop,nprocs
    if (lsym1) then
      nati = iatn(i)
      ntypi = natype(i)
      nregioni = nregionno(nsft+i)
      oci = occua(i)
      lopi = (.not.lfreeze.or.lopf(i))
      ii = nrel2(i)
      dneqv = neqv(i)
    else
      nati = nat(i)
      ntypi = nftype(i)
      nregioni = nregionno(nsft+nrelat(i))
      oci = occuf(i)
      lopi = (.not.lfreeze.or.lopf(nrelat(i)))
      ii = i
      dneqv = 1.0_dp
    endif
    dneqv = dneqv*oci*oci
    qi = qf(ii)
    ix = 3*(ii - 1) + 1
    iy = ix + 1
    iz = iy + 1
!       
!  Set region 2 pair flag
!       
    lreg2pair = .false.
    if (nregions(ncf).ge.2) then
      lreg2pair = (nregionno(nsft+nrelat(ii)).gt.1)
    endif
    do m = 1,nboQ0
      esum = 0.0_dp
      if (nati.eq.nBOspecQ0(m).and.(ntypi.eq.nBOtypQ0(m).or.nBOtypQ0(m).eq.0)) then
        lDoTerm = .false.
        rho = BOq0rho(m)
        if (BOq0ref(m).gt.0.0d0.and.qi.gt.BOq0ref(m)) then
!
!  Cation case
!
          lDoTerm = .true.
          esum = dneqv*BOq0pot(m)*exp(-rho/(qi - BOq0ref(m)))
          if (lgrad1) then
            dedq = esum*rho/(qi - BOq0ref(m))**2
            if (lgrad2) then
              d2edq2 = dedq*(rho/(qi - BOq0ref(m)) - 2.0_dp)/(qi - BOq0ref(m))
            endif
          endif
        elseif (BOq0ref(m).lt.0.0d0.and.qi.lt.BOq0ref(m)) then
!
!  Anion case
!
          lDoTerm = .true.
          esum = dneqv*BOq0pot(m)*exp(rho/(qi - BOq0ref(m)))
          if (lgrad1.and.lDoQDeriv1) then
            dedq = - esum*rho/(qi - BOq0ref(m))**2
            if (lgrad2) then
              d2edq2 = - dedq*(rho/(qi - BOq0ref(m)) + 2.0_dp)/(qi - BOq0ref(m))
            endif
          endif
        endif
!
!  If no derivatives are needed don't do terms
!
        if (.not.lgrad1) lDoTerm = .false.
!
        if (lDoTerm) then
          if (lstr.and..not.lPhonon) then
!
!  Strain first derivatives
!
            do kl = 1,nstrains
              rstrd(kl) = rstrd(kl) + dedq*dqds(kl,ii)
            enddo
            if (latomicstress) then
              do kl = 1,nstrains
                atomicstress(kl,i) = atomicstress(kl,i) + dedq*dqds(kl,ii)
              enddo
            endif
            if (lgrad2) then
!
!  Strain-strain second derivatives : d2E/dQ2 x dQ/ds1 x dQ/ds2
!
              do kk = 1,nstrains
                do kl = 1,nstrains
                  sderv2(kl,kk) = sderv2(kl,kk) + d2edq2*dqds(kk,ii)*dqds(kl,ii)
                enddo
              enddo
!
!  Strain-strain second derivatives : dE/dQ x d2Q/ds1.ds2
!
              ind = 0
              do kk = 1,nstrains
                do kl = 1,kk-1
                  ind = ind + 1
                  sderv2(kl,kk) = sderv2(kl,kk) + dedq*d2qds2(ind,ii)
                  sderv2(kk,kl) = sderv2(kk,kl) + dedq*d2qds2(ind,ii)
                enddo
                ind = ind + 1
                sderv2(kk,kk) = sderv2(kk,kk) + dedq*d2qds2(ind,ii)
              enddo
            endif
          endif
          do n = 1,nqatoms(ii)
            j = nqatomptr(n,ii)
            jx = 3*(j - 1) + 1
            jy = jx + 1
            jz = jy + 1
            nregionj = nregionno(nsft+nrelat(j))
            lopj = (.not.lfreeze.or.lopf(nrelat(j)))
!
!  Internal first derivatives
!
            if (lopi) then
              xdrv(i) = xdrv(i) - dedq*dqdxyz(jx,ii)
              ydrv(i) = ydrv(i) - dedq*dqdxyz(jy,ii)
              zdrv(i) = zdrv(i) - dedq*dqdxyz(jz,ii)
            endif
            xregdrv(nregioni) = xregdrv(nregioni) - dedq*dqdxyz(jx,ii)
            yregdrv(nregioni) = yregdrv(nregioni) - dedq*dqdxyz(jy,ii)
            zregdrv(nregioni) = zregdrv(nregioni) - dedq*dqdxyz(jz,ii)
            if (nrel2(nrelat(j)).eq.j) then
              if (lopj) then
                xdrv(j) = xdrv(j) + dedq*dqdxyz(jx,ii)
                ydrv(j) = ydrv(j) + dedq*dqdxyz(jy,ii)
                zdrv(j) = zdrv(j) + dedq*dqdxyz(jz,ii)
              endif
              xregdrv(nregionj) = xregdrv(nregionj) + dedq*dqdxyz(jx,ii)
              yregdrv(nregionj) = yregdrv(nregionj) + dedq*dqdxyz(jy,ii)
              zregdrv(nregionj) = zregdrv(nregionj) + dedq*dqdxyz(jz,ii)
            endif
          enddo
          if (lgrad2.and.lDoQDeriv2) then
            do mm = 1,nqatoms(ii)
              j = nqatomptr(mm,ii)
              jx = 3*(j - 1) + 1
              jy = jx + 1
              jz = jy + 1
!
              mx = 3*(mm - 1) + 1
              my = mx + 1 
              mz = my + 1
!   
              mmxx = mx*(mx + 1)/2 
              mmyy = my*(my + 1)/2 
              mmzz = mz*(mz + 1)/2 
              mmxy = mmyy - 1
              mmxz = mmzz - 2
              mmyz = mmzz - 1
!
!  Second internal derivatives : dE/dQ x d2Q/da.db
!
              if (i.gt.j) then
                derv2(jx,ix) = derv2(jx,ix) + dedq*d2qdxyz2(mmxx,ii)
                derv2(jy,ix) = derv2(jy,ix) + dedq*d2qdxyz2(mmxy,ii)
                derv2(jz,ix) = derv2(jz,ix) + dedq*d2qdxyz2(mmxz,ii)
                derv2(jx,iy) = derv2(jx,iy) + dedq*d2qdxyz2(mmxy,ii)
                derv2(jy,iy) = derv2(jy,iy) + dedq*d2qdxyz2(mmyy,ii)
                derv2(jz,iy) = derv2(jz,iy) + dedq*d2qdxyz2(mmyz,ii)
                derv2(jx,iz) = derv2(jx,iz) + dedq*d2qdxyz2(mmxz,ii)
                derv2(jy,iz) = derv2(jy,iz) + dedq*d2qdxyz2(mmyz,ii)
                derv2(jz,iz) = derv2(jz,iz) + dedq*d2qdxyz2(mmzz,ii)
              elseif (i.lt.j) then
                derv2(ix,jx) = derv2(ix,jx) + dedq*d2qdxyz2(mmxx,ii)
                derv2(iy,jx) = derv2(iy,jx) + dedq*d2qdxyz2(mmxy,ii)
                derv2(iz,jx) = derv2(iz,jx) + dedq*d2qdxyz2(mmxz,ii)
                derv2(ix,jy) = derv2(ix,jy) + dedq*d2qdxyz2(mmxy,ii)
                derv2(iy,jy) = derv2(iy,jy) + dedq*d2qdxyz2(mmyy,ii)
                derv2(iz,jy) = derv2(iz,jy) + dedq*d2qdxyz2(mmyz,ii)
                derv2(ix,jz) = derv2(ix,jz) + dedq*d2qdxyz2(mmxz,ii)
                derv2(iy,jz) = derv2(iy,jz) + dedq*d2qdxyz2(mmyz,ii)
                derv2(iz,jz) = derv2(iz,jz) + dedq*d2qdxyz2(mmzz,ii)
              endif
!
!  Second internal derivatives : d2E/dQ2 x dQ/da x dQ/db : i - j
!
              if (i.gt.j) then
                derv2(jx,ix) = derv2(jx,ix) + d2edq2*dqdxyz(ix,ii)*dqdxyz(jx,ii)
                derv2(jy,ix) = derv2(jy,ix) + d2edq2*dqdxyz(ix,ii)*dqdxyz(jy,ii)
                derv2(jz,ix) = derv2(jz,ix) + d2edq2*dqdxyz(ix,ii)*dqdxyz(jz,ii)
                derv2(jx,iy) = derv2(jx,iy) + d2edq2*dqdxyz(iy,ii)*dqdxyz(jx,ii)
                derv2(jy,iy) = derv2(jy,iy) + d2edq2*dqdxyz(iy,ii)*dqdxyz(jy,ii)
                derv2(jz,iy) = derv2(jz,iy) + d2edq2*dqdxyz(iy,ii)*dqdxyz(jz,ii)
                derv2(jx,iz) = derv2(jx,iz) + d2edq2*dqdxyz(iz,ii)*dqdxyz(jx,ii)
                derv2(jy,iz) = derv2(jy,iz) + d2edq2*dqdxyz(iz,ii)*dqdxyz(jy,ii)
                derv2(jz,iz) = derv2(jz,iz) + d2edq2*dqdxyz(iz,ii)*dqdxyz(jz,ii)
              elseif (i.lt.j) then
                derv2(ix,jx) = derv2(ix,jx) + d2edq2*dqdxyz(jx,ii)*dqdxyz(ix,ii)
                derv2(iy,jx) = derv2(iy,jx) + d2edq2*dqdxyz(jx,ii)*dqdxyz(iy,ii)
                derv2(iz,jx) = derv2(iz,jx) + d2edq2*dqdxyz(jx,ii)*dqdxyz(iz,ii)
                derv2(ix,jy) = derv2(ix,jy) + d2edq2*dqdxyz(jy,ii)*dqdxyz(ix,ii)
                derv2(iy,jy) = derv2(iy,jy) + d2edq2*dqdxyz(jy,ii)*dqdxyz(iy,ii)
                derv2(iz,jy) = derv2(iz,jy) + d2edq2*dqdxyz(jy,ii)*dqdxyz(iz,ii)
                derv2(ix,jz) = derv2(ix,jz) + d2edq2*dqdxyz(jz,ii)*dqdxyz(ix,ii)
                derv2(iy,jz) = derv2(iy,jz) + d2edq2*dqdxyz(jz,ii)*dqdxyz(iy,ii)
                derv2(iz,jz) = derv2(iz,jz) + d2edq2*dqdxyz(jz,ii)*dqdxyz(iz,ii)
              endif
!
              do kk = 1,nstrains
!                   
!  Strain-internal second derivatives : dE/dQ x d2Q/ds.da
!               
                derv3(ix,kk) = derv3(ix,kk) - dedq*d2qdxyzs(kk,mx,ii)
                derv3(iy,kk) = derv3(iy,kk) - dedq*d2qdxyzs(kk,my,ii)
                derv3(iz,kk) = derv3(iz,kk) - dedq*d2qdxyzs(kk,mz,ii)
                derv3(jx,kk) = derv3(jx,kk) + dedq*d2qdxyzs(kk,mx,ii)
                derv3(jy,kk) = derv3(jy,kk) + dedq*d2qdxyzs(kk,my,ii)
                derv3(jz,kk) = derv3(jz,kk) + dedq*d2qdxyzs(kk,mz,ii)
!               
!  Strain-internal second derivatives : d2E/dQ2 x dQ/ds x dQ/da
!  
                derv3(ix,kk) = derv3(ix,kk) - d2edq2*dqdxyz(jx,ii)*dqds(kk,ii)
                derv3(iy,kk) = derv3(iy,kk) - d2edq2*dqdxyz(jy,ii)*dqds(kk,ii)
                derv3(iz,kk) = derv3(iz,kk) - d2edq2*dqdxyz(jz,ii)*dqds(kk,ii)
                derv3(jx,kk) = derv3(jx,kk) + d2edq2*dqdxyz(jx,ii)*dqds(kk,ii)
                derv3(jy,kk) = derv3(jy,kk) + d2edq2*dqdxyz(jy,ii)*dqds(kk,ii)
                derv3(jz,kk) = derv3(jz,kk) + d2edq2*dqdxyz(jz,ii)*dqds(kk,ii)
              enddo
              if (lstr.and..not.lPhonon) then
!
!  Subtract terms that get added in strfin but are not needed
!
                if (ndim.eq.3) then
                  xdrvk = dedq*dqdxyz(jx,ii) 
                  ydrvk = dedq*dqdxyz(jy,ii) 
                  zdrvk = dedq*dqdxyz(jz,ii) 
                  derv3(ix,5) = derv3(ix,5) + zdrvk
                  derv3(ix,6) = derv3(ix,6) + ydrvk
                  derv3(iy,4) = derv3(iy,4) + zdrvk
                  derv3(iy,6) = derv3(iy,6) + xdrvk
                  derv3(iz,4) = derv3(iz,4) + ydrvk
                  derv3(iz,5) = derv3(iz,5) + xdrvk
                  derv3(ix,1) = derv3(ix,1) + 2.0_dp*xdrvk
                  derv3(iy,2) = derv3(iy,2) + 2.0_dp*ydrvk
                  derv3(iz,3) = derv3(iz,3) + 2.0_dp*zdrvk
                  derv3(jx,5) = derv3(jx,5) - zdrvk
                  derv3(jx,6) = derv3(jx,6) - ydrvk
                  derv3(jy,4) = derv3(jy,4) - zdrvk
                  derv3(jy,6) = derv3(jy,6) - xdrvk
                  derv3(jz,4) = derv3(jz,4) - ydrvk
                  derv3(jz,5) = derv3(jz,5) - xdrvk
                  derv3(jx,1) = derv3(jx,1) - 2.0_dp*xdrvk
                  derv3(jy,2) = derv3(jy,2) - 2.0_dp*ydrvk
                  derv3(jz,3) = derv3(jz,3) - 2.0_dp*zdrvk
                elseif (ndim.eq.2) then
                  xdrvk = dedq*dqdxyz(jx,ii) 
                  ydrvk = dedq*dqdxyz(jy,ii) 
                  derv3(ix,1) = derv3(ix,1) + 2.0_dp*xdrvk
                  derv3(iy,2) = derv3(iy,2) + 2.0_dp*ydrvk
                  derv3(ix,3) = derv3(ix,3) + ydrvk
                  derv3(iy,3) = derv3(iy,3) + xdrvk
                  derv3(jx,1) = derv3(jx,1) - 2.0_dp*xdrvk
                  derv3(jy,2) = derv3(jy,2) - 2.0_dp*ydrvk
                  derv3(jx,3) = derv3(jx,3) - ydrvk
                  derv3(jy,3) = derv3(jy,3) - xdrvk
                elseif (ndim.eq.1) then
                  xdrvk = dedq*dqdxyz(jx,ii) 
                  derv3(ix,1) = derv3(ix,1) + 2.0_dp*xdrvk
                  derv3(jx,1) = derv3(jx,1) - 2.0_dp*xdrvk
                endif
              endif
!
              do mn = 1,mm-1
                k = nqatomptr(mn,ii)
                kx = 3*(k - 1) + 1
                ky = kx + 1
                kz = ky + 1
!
!  Second internal derivatives : d2E/dQ2 x dQ/da x dQ/db : k - j
!
                if (j.gt.k) then
                  derv2(kx,jx) = derv2(kx,jx) + d2edq2*dqdxyz(jx,ii)*dqdxyz(kx,ii)
                  derv2(ky,jx) = derv2(ky,jx) + d2edq2*dqdxyz(jx,ii)*dqdxyz(ky,ii)
                  derv2(kz,jx) = derv2(kz,jx) + d2edq2*dqdxyz(jx,ii)*dqdxyz(kz,ii)
                  derv2(kx,jy) = derv2(kx,jy) + d2edq2*dqdxyz(jy,ii)*dqdxyz(kx,ii)
                  derv2(ky,jy) = derv2(ky,jy) + d2edq2*dqdxyz(jy,ii)*dqdxyz(ky,ii)
                  derv2(kz,jy) = derv2(kz,jy) + d2edq2*dqdxyz(jy,ii)*dqdxyz(kz,ii)
                  derv2(kx,jz) = derv2(kx,jz) + d2edq2*dqdxyz(jz,ii)*dqdxyz(kx,ii)
                  derv2(ky,jz) = derv2(ky,jz) + d2edq2*dqdxyz(jz,ii)*dqdxyz(ky,ii)
                  derv2(kz,jz) = derv2(kz,jz) + d2edq2*dqdxyz(jz,ii)*dqdxyz(kz,ii)
                elseif (j.lt.k) then
                  derv2(jx,kx) = derv2(jx,kx) + d2edq2*dqdxyz(kx,ii)*dqdxyz(jx,ii)
                  derv2(jy,kx) = derv2(jy,kx) + d2edq2*dqdxyz(kx,ii)*dqdxyz(jy,ii)
                  derv2(jz,kx) = derv2(jz,kx) + d2edq2*dqdxyz(kx,ii)*dqdxyz(jz,ii)
                  derv2(jx,ky) = derv2(jx,ky) + d2edq2*dqdxyz(ky,ii)*dqdxyz(jx,ii)
                  derv2(jy,ky) = derv2(jy,ky) + d2edq2*dqdxyz(ky,ii)*dqdxyz(jy,ii)
                  derv2(jz,ky) = derv2(jz,ky) + d2edq2*dqdxyz(ky,ii)*dqdxyz(jz,ii)
                  derv2(jx,kz) = derv2(jx,kz) + d2edq2*dqdxyz(kz,ii)*dqdxyz(jx,ii)
                  derv2(jy,kz) = derv2(jy,kz) + d2edq2*dqdxyz(kz,ii)*dqdxyz(jy,ii)
                  derv2(jz,kz) = derv2(jz,kz) + d2edq2*dqdxyz(kz,ii)*dqdxyz(jz,ii)
                endif
              enddo
            enddo
          endif
        endif
      endif
      if (lreg2pair) then
        esregion2 = esregion2 + esum
      else
        eboQself = eboQself + esum
      endif
      eregion2region(nregionj,nregioni) = eregion2region(nregionj,nregioni) + esum
    enddo
  enddo
!
  t2 = cputime()
  tbondorder = tbondorder + t2 - t1
!
  return
  end
