  subroutine reaxFFpot(vsite)
!
!  Calculates the electrostatic potential for ReaxFF
!
!  vsite = calculated potential on output
!
!   5/12 Created from reaxffmd.f90
!
!  NB: The derivatives of the potential cannot yet be
!      calculated because this would require the charge
!      derivatives, which are not yet implemented. 
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
  use constants,      only : angstoev
  use current
  use element,        only : reaxFFgamma, lreaxFFqfix
  use numbers,        only : third
  use parallel,       only : nprocs, procid
  use reaxFFdata
  use realvectors,    only : dist, xtmp, ytmp, ztmp
  use spatialbo
  use times,          only : tsum
  implicit none
!
!  Passed variables
!
  real(dp),                      intent(out)     :: vsite(*)
!
!  Local variables
!
  integer(i4)                                    :: i
  integer(i4)                                    :: ic
  integer(i4)                                    :: ii
  integer(i4)                                    :: imin
  integer(i4)                                    :: imx
  integer(i4)                                    :: imy
  integer(i4)                                    :: imz
  integer(i4)                                    :: ind
  integer(i4)                                    :: indn
  integer(i4)                                    :: ind1
  integer(i4)                                    :: ind2
  integer(i4)                                    :: ix
  integer(i4)                                    :: ixyz
  integer(i4)                                    :: iy
  integer(i4)                                    :: iz
  integer(i4)                                    :: j
  integer(i4)                                    :: jc
  integer(i4)                                    :: jj
  integer(i4)                                    :: jmax
  integer(i4)                                    :: k
  integer(i4)                                    :: kc
  integer(i4)                                    :: kk
  integer(i4)                                    :: kl
  integer(i4)                                    :: l
  integer(i4)                                    :: m
  integer(i4)                                    :: maxx
  integer(i4)                                    :: maxxy
  integer(i4)                                    :: n
  integer(i4)                                    :: n1i
  integer(i4)                                    :: n1j
  integer(i4)                                    :: nati
  integer(i4), dimension(:),   allocatable, save :: nbos
  integer(i4), dimension(:),   allocatable, save :: nbosptr
  integer(i4)                                    :: nboatom
  integer(i4), dimension(:),   allocatable, save :: nboatomRptr
  integer(i4)                                    :: ni
  integer(i4)                                    :: nj
  integer(i4)                                    :: nmolonly
  integer(i4)                                    :: nor
  integer(i4)                                    :: nspeci
  integer(i4)                                    :: nspecj
  integer(i4)                                    :: nsplower(3)
  integer(i4)                                    :: nspupper(3)
  integer(i4)                                    :: ntypi
  integer(i4)                                    :: status
  logical                                        :: lfixQi
  logical                                        :: lfixQj
  logical                                        :: lfound
  logical                                        :: lreactivei
  logical                                        :: lreactivej
  logical                                        :: lself
  real(dp)                                       :: cputime
  real(dp)                                       :: cutq2
  real(dp)                                       :: gam
  real(dp)                                       :: gammai
  real(dp)                                       :: gammaj
  real(dp)                                       :: gammaij
  real(dp)                                       :: qij
  real(dp)                                       :: rij
  real(dp)                                       :: rij2
  real(dp)                                       :: scale
  real(dp),    dimension(:),   allocatable, save :: sum
  real(dp)                                       :: t1
  real(dp)                                       :: t2
  real(dp)                                       :: tpQ
  real(dp)                                       :: dtpQdr
  real(dp)                                       :: d2tpQdr2
  real(dp)                                       :: xi
  real(dp)                                       :: yi
  real(dp)                                       :: zi
  real(dp)                                       :: xji
  real(dp)                                       :: yji
  real(dp)                                       :: zji
!
  do i = 1,numat
    vsite(i) = 0.0_dp
  enddo
!
  allocate(nboatomRptr(numat),stat=status)
  if (status/=0) call outofmemory('reaxFFpot','nboatomRptr')
! 
!  Set up a dummy pointer for derivative array calls
! 
  nboatom = 0
  do i = 1,numat
    nboatom = nboatom + 1
    nboatomRptr(i) = nboatom
  enddo
!
!  Allocate memory that does not depend on maxneigh
!
  allocate(nbos(numat),stat=status)
  if (status/=0) call outofmemory('reaxFFpot','nbos')
  allocate(nbosptr(numat),stat=status)
  if (status/=0) call outofmemory('reaxFFpot','nbosptr')
!***************************
!  Find species for atoms  *
!***************************
  nbosptr(1:numat) = 0
  do i = 1,numat
    nati = nat(i)
    ntypi = nftype(i)
!
!  Check repulsive terms and build atom pointers to species
!
    nbos(i) = 0
    do j = 1,nreaxFFspec
      if (nati.eq.natreaxFFspec(j).and.(ntypi.eq.ntypreaxFFspec(j).or.ntypreaxFFspec(j).eq.0)) then
        nbos(i) = nbos(i) + 1
        nbosptr(i) = j
      endif
    enddo
!
!  Check number of species for now
!
    if (nbos(i).gt.1) then
      call outerror('Multiple species per atom not yet allowed for in reaxFF',0_i4)
      call stopnow('reaxFFpot')
    endif
  enddo
!*************************
!  Coulomb contribution  *
!*************************
!
!  Set cutoffs
!
  cutq2   = reaxFFcutoffQ**2
!
  if (lspatialboOK) then
!************************************
!  Spatial decomposition algorithm  *
!************************************
    maxxy = nspcellbo(1)*nspcellbo(2)
    maxx  = nspcellbo(1)
!
!  Loop over all local spatial cells except buffer regions
!
    do ixyz = 1,ncellpernodebo
      ind1 = ncellnodeptrbo(ixyz)
      ind2 = ind1 - 1
      iz = ind2/maxxy
      ind2 = ind2 - maxxy*iz
      iy = ind2/maxx
      ix = ind2 - maxx*iy + 1
      iy = iy + 1
      iz = iz + 1
      if (.not.lbuffercellbo(ixyz)) then
!
!  Set cell search bounds
!
        nspupper(1) = min(ix+ncellsearchbo(1),nspcellbo(1))
        nspupper(2) = min(iy+ncellsearchbo(2),nspcellbo(2))
        nspupper(3) = min(iz+ncellsearchbo(3),nspcellbo(3))
        nsplower(1) = max(ix-ncellsearchbo(1),1)
        nsplower(2) = max(iy-ncellsearchbo(2),1)
        nsplower(3) = max(iz-ncellsearchbo(3),1)
!
!  Get number of atoms in this cell
!
        ni = nspcellatbo(ind1)
        n1i = nspcellat1ptrbo(ind1)
!
!  Outer loop over atoms within this cell
!
        do ii = 1,ni
          i = nspcellatptrbo(n1i+ii)
!
!  Does i have a reaxFF species or a fixed charge?
!
          nspeci = nbosptr(i)
          lfixQi = lreaxFFqfix(nspeci)
          lreactivei = (nbos(i).gt.0.and..not.lfixQi)
!
          if (lreactivei.or.lfixQi) then
            gammai = reaxFFgamma(nspeci)
            ic = nspcellatptrcellbo(n1i+ii)
!
!  Set coordinates of atom i
!
            xi = xinboxbo(i) + xvec2cell(ic)
            yi = yinboxbo(i) + yvec2cell(ic)
            zi = zinboxbo(i) + zvec2cell(ic)
!
!  Loop over neighbouring cells
!         
            do imz = nsplower(3),nspupper(3)
              do imy = nsplower(2),nspupper(2)
                do imx = nsplower(1),nspupper(1)
                  indn = (imz-1)*maxxy + (imy-1)*maxx + imx
!  
!  Loop over atoms within neighbouring cells
!               
                  nj = nspcellatbo(indn)
                  n1j = nspcellat1ptrbo(indn)
                  do jj = 1,nj
                    j = nspcellatptrbo(n1j+jj)
!
!  Does j have a reaxFF species or a fixed charge?
!
                    nspecj = nbosptr(j)
                    lfixQj = lreaxFFqfix(nspecj)
                    lreactivej = (nbos(j).gt.0.and..not.lfixQj)
!
                    if (lreactivej.or.lfixQj) then
                      gammaj = reaxFFgamma(nspecj)
                      jc = nspcellatptrcellbo(n1j+jj)
!  
!  Only calculate lower-half triangular interactions
!                 
                      if (j.lt.i.or.(j.eq.i.and.ind1.ne.indn)) then
!  
!  Set coordinate differences and calculate square of distance
!                   
                        xji = xvec2cell(jc) + xinboxbo(j) - xi
                        yji = yvec2cell(jc) + yinboxbo(j) - yi
                        zji = zvec2cell(jc) + zinboxbo(j) - zi
                        rij2 = xji*xji + yji*yji + zji*zji
                        if (rij2.lt.cutq2) then
!
                          if (i.eq.j) then
                            scale = 0.5_dp
                          else
                            scale = 1.0_dp
                          endif
!
!  Compute Morse parameters for pair if not explicitly input
!
                          if (nspeci.ge.nspecj) then
                            ind = nspeci*(nspeci - 1)/2 + nspecj
                          else
                            ind = nspecj*(nspecj - 1)/2 + nspeci
                          endif
                          qij = scale*angstoev
                          rij = sqrt(rij2)
!
!  Compute taper function(s)
!
                          call p7reaxFFqtaper(rij,reaxFFcutoffQ,tpQ,dtpQdr,d2tpQdr2,.false.,.false.)
!
!  Compute Coulomb shielding parameters
!
                          gammaij  = sqrt(gammai*gammaj)
                          gammaij  = 1.0_dp/(gammaij**3)
!
!  Pure real space tapered lreaxFF case
!
                          gam = qij/(rij**3 + gammaij)**third
                          vsite(i) = vsite(i) + tpQ*gam*qreaxFF(j)
                          vsite(j) = vsite(j) + tpQ*gam*qreaxFF(i)
                        endif
                      endif
                    endif
!
!  End loop over j
!
                  enddo 
!               
!  End loops over neighbouring cells
!  
                enddo
              enddo
            enddo
!
!  End check on whether i has a reaxFF species
!
          endif
!
!  End loop over atom i
!
        enddo
!
!  End checks on whether cell is required
!
      endif
!
!  End loop over cells on node
!
    enddo
  else
!***************************
!  Standard N^2 algorithm  *
!***************************
!
!  Set lower bound for i loop
!
    if (ndim.gt.0) then
      imin = 1
    else
      imin = 2
    endif
!
!  Loop over pairs
!
    do i = 1+procid,numat,nprocs
!
!  Does i have a reaxFF species or a fixed charge?
!
      nspeci = nbosptr(i)
      lfixQi = lreaxFFqfix(nspeci)
      lreactivei = (nbos(i).gt.0.and..not.lfixQi)
!
      if (lreactivei.or.lfixQi) then
        gammai = reaxFFgamma(nspeci)
!
!  Set upper bound for j loop
!
        if (ndim.gt.0) then
          jmax = i
        else
          jmax = i - 1
        endif
!
!  Loop over second atom
!
        jloop: do j = 1,jmax
!
!  Does j have a reaxFF species or a fixed charge?
!
          nspecj = nbosptr(j)
          lfixQj = lreaxFFqfix(nspecj)
          lreactivej = (nbos(j).gt.0.and..not.lfixQj)
!
          if (lreactivej.or.lfixQj) then
            gammaj = reaxFFgamma(nspecj)
!
!  Compute basic interatomic vector
!
            xji = xclat(j) - xclat(i)
            yji = yclat(j) - yclat(i)
            zji = zclat(j) - zclat(i)
!
!  Find valid vectors
!
            nor = 0
            if (ndim.eq.3) then
              call rsearch3D(xji,yji,zji,.false.,.false.,i,j,0_i4,0_i4,0_i4,0_i4,nor,nmolonly,lself,cutq2)
            elseif (ndim.eq.2) then
              call rsearch2D(xji,yji,zji,.false.,.false.,i,j,0_i4,0_i4,0_i4,0_i4,nor,nmolonly,lself,cutq2)
            elseif (ndim.eq.1) then
              call rsearch1D(xji,yji,zji,.false.,.false.,i,j,0_i4,0_i4,0_i4,0_i4,nor,nmolonly,lself,cutq2)
            elseif (ndim.eq.0) then
              rij2 = xji*xji + yji*yji + zji*zji
              if (rij2.lt.cutq2) then
                nor = nor + 1
              endif
            endif
!
!  If no distances then cycle
!
            if (nor.eq.0) cycle jloop
!
            if (i.eq.j) then
              scale = 0.5_dp
            else
              scale = 1.0_dp
            endif
!
!  Set index for pairwise parameters
!
            if (nspeci.ge.nspecj) then
              ind = nspeci*(nspeci - 1)/2 + nspecj
            else
              ind = nspecj*(nspecj - 1)/2 + nspeci
            endif
!
            qij = scale*angstoev
!
!  Loop over valid distances and calculate contributions
!
            do n = 1,nor
              if (ndim.gt.0) rij2 = dist(n)
              rij = sqrt(rij2)
!
!  Compute taper function
!
              call p7reaxFFqtaper(rij,reaxFFcutoffQ,tpQ,dtpQdr,d2tpQdr2,.false.,.false.)
!
!  Compute Coulomb shielding parameters
!
              gammaij  = sqrt(gammai*gammaj)
              gammaij  = 1.0_dp/(gammaij**3)
!                     
!  Pure real space tapered lreaxFF case
!                     
              gam = qij/(rij*rij2 + gammaij)**third
              vsite(i) = vsite(i) + tpQ*gam*qreaxFF(j)
              vsite(j) = vsite(j) + tpQ*gam*qreaxFF(i)
!
!  End loop over distances
!
            enddo
          endif
!
!  End of loop over j
!
        enddo jloop
      endif
!
!  End of loop over i
!
    enddo
  endif
!
!  Global sums
!
  if (nprocs.gt.1) then
    t1 = cputime()
    allocate(sum(numat),stat=status)
    if (status/=0) call outofmemory('reaxffpot','sum')
    call sumall(vsite,sum,numat,"reaxffpot","vsite")
    do i = 1,numat
      vsite(i) = sum(i)
    enddo
    deallocate(sum,stat=status)
    if (status/=0) call deallocate_error('epot','sum')
    tsum = tsum + cputime() - t1
  endif
!
!  Free local memory
!
  deallocate(nbosptr,stat=status)
  if (status/=0) call deallocate_error('reaxFFpot','nbosptr')
  deallocate(nbos,stat=status)
  if (status/=0) call deallocate_error('reaxFFpot','nbos')
  deallocate(nboatomRptr,stat=status)
  if (status/=0) call deallocate_error('reaxFFpot','nboatomRptr')
!
  return
  end
