  subroutine setreaxffQ(nboatom,nboatomRptr,nbos,nbosptr,qreaxFF,eself)
!
!  Subroutine for performing electronegativity equilisation calcns
!  in order to determine reaxFF charges.
!
!   1/08 Created based on eem & reaxff
!   4/08 Qtot now properly initialised
!   4/08 qreaxFF returned to full atom array at the end
!   6/08 Shell structure correction added along with iterative solution
!   6/08 Option to fix charges in ReaxFF added
!  10/09 qr12 term added
!  11/09 qr12 term removed
!  12/09 Solution for charges modified to use lapack solution rather 
!        than matrix inversion.
!   4/10 Setting of nfi corrected
!   9/11 Use of ni/nj for nat of i and j replaced by nati/natj for
!        consistency with setreaxffQiter.
!  10/11 Site energy terms added
!  12/11 nbosptr added to arguments of setreaxff
!  12/11 Dimension of nbos corrected to numat from nboatom
!  12/11 lreaxFFqfix and reaxFFgamma referenced by species
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
!  Copyright Curtin University 2011
!
!  Julian Gale, NRI, Curtin University, December 2011
!
  use constants,      only : angstoev
  use control
  use current
  use element
  use energies,       only : siteenergy
  use iochannels
  use numbers,        only : third
  use parallel
  use realvectors,    only : dist
  use reaxFFdata,     only : reaxFFcutoffQ
  use reaxFFdata,     only : reaxFFqdamp, nreaxFFqiter, reaxFFqconverged, reaxFFqconverged1
  use times
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)                      :: nboatom
  integer(i4), intent(in)                      :: nboatomRptr(nboatom)
  integer(i4), intent(in)                      :: nbos(numat)
  integer(i4), intent(in)                      :: nbosptr(numat)
  real(dp),    intent(out)                     :: qreaxFF(numat+1)
  real(dp),    intent(out)                     :: eself
!
!  Local variables
!
  integer(i4)                                  :: i
  integer(i4)                                  :: ifail
  integer(i4)                                  :: ii
  integer(i4)                                  :: imin
  integer(i4)                                  :: ind
  integer(i4), dimension(:), allocatable       :: ipivot
  integer(i4)                                  :: j
  integer(i4)                                  :: jj
  integer(i4)                                  :: jmax
  integer(i4)                                  :: n
  integer(i4)                                  :: nfree
  integer(i4), dimension(:), allocatable       :: nfreeptr
  integer(i4)                                  :: nfi
  integer(i4)                                  :: nfj
  integer(i4)                                  :: nati
  integer(i4)                                  :: niter
  integer(i4)                                  :: nitermax
  integer(i4)                                  :: nmolonly
  integer(i4)                                  :: nor
  integer(i4)                                  :: nspeci
  integer(i4)                                  :: nspecj
  integer(i4)                                  :: status
  logical                                      :: lfixQi
  logical                                      :: lfixQj
  logical                                      :: literative
  logical                                      :: lmixed
  logical                                      :: lself
  real(dp),    dimension(:), allocatable       :: dpacked
  real(dp),    dimension(:), allocatable       :: dpackedsave
  real(dp)                                     :: cut2
  real(dp)                                     :: damp
  real(dp)                                     :: esite
  real(dp)                                     :: gam
  real(dp)                                     :: gammai
  real(dp)                                     :: gammaj
  real(dp)                                     :: gammaij
  real(dp)                                     :: qdiff
  real(dp)                                     :: qdiff1
  real(dp)                                     :: qi
  real(dp)                                     :: qsum
  real(dp)                                     :: qtot
  real(dp),    dimension(:), allocatable       :: qreaxFFfree
  real(dp),    dimension(:), allocatable       :: qreaxFFsave
  real(dp)                                     :: rij
  real(dp)                                     :: rij2
  real(dp)                                     :: tp
  real(dp)                                     :: dtpdr
  real(dp)                                     :: d2tpdr2
  real(dp)                                     :: xji
  real(dp)                                     :: yji
  real(dp)                                     :: zji
  real(dp),    dimension(:), allocatable       :: z
!
!  Allocate pointer array for atoms to act on
!
  allocate(nfreeptr(nboatom),stat=status)
  if (status/=0) call outofmemory('setreaxffQ','nfreeptr')
!
!  Find out whether an iterative solution is needed due to shell structure or whether there are any fixed charges
!
  literative = .false.
  qsum = 0.0_dp
  nfree = 0
  do i = 1,nboatom
    ii = nboatomRptr(i)
    if (ii.gt.0) then
      nspeci = nbosptr(ii)
      nati = nat(ii)
      if (abs(reaxFFshell(1,nati)).gt.1.0d-12) then
        literative = .true.
      endif
      if (lreaxFFqfix(nspeci)) then
        qsum = qsum - reaxFFqfix(nspeci)*occuf(ii)
      else
        nfree = nfree + 1
        nfreeptr(nfree) = ii
      endif
    endif
  enddo
!
!  Decide on total EEM fragment charge - for cluster
!  there is no constraint so use sum of initial charges.
!  For periodic system, charge must be equal to the
!  negative sum of the non-EEM ion charges.
!
  qtot = totalcharge + qsum
!
!  Allocate local memory that depends on nboatom
!
  n = nfree + 1
  allocate(z(n),stat=status)
  if (status/=0) call outofmemory('setreaxffQ','z')
  allocate(qreaxFFfree(nfree+1),stat=status)
  if (status/=0) call outofmemory('setreaxffQ','qreaxFFfree')
  allocate(qreaxFFsave(nboatom+1),stat=status)
  if (status/=0) call outofmemory('setreaxffQ','qreaxFFsave')
  allocate(dpacked(n*(n+1)/2),stat=status)
  if (status/=0) call outofmemory('setreaxffQ','dpacked')
  if (literative) then
    allocate(dpackedsave(n*(n+1)/2),stat=status)
    if (status/=0) call outofmemory('setreaxffQ','dpackedsave')
  endif
!
!  Form right hand vector
!
  z(1:n) = 0.0_dp
  do i = 1,nfree
    ii = nfreeptr(i)
    nati = nat(ii)
    z(i) = z(i) - reaxFFchi(nati)
  enddo
  z(nfree+1) = qtot
!*******************************************
!  Generate electrostatic potential terms  *
!*******************************************
!
!  Initialise i-j matrix elements
!
  dpacked(1:n*(n+1)/2) = 0.0_dp
!
!  Set cutoffs
!
  cut2 = reaxFFcutoffQ**2
!
!  Set lower bound for i loop
!
  if (ndim.gt.0) then
    imin = 1
    ind = 0
    nfi = 0
  else
    imin = 2
    if (nboatomRptr(1).gt.0) then
      if (lreaxFFqfix(nbosptr(nboatomRptr(1)))) then
        ind = 0
      else
        ind = 1
      endif
    else
      ind = 0
    endif
!
!  Set nfi allowing for whether skipped first atom is fixed or not
!
    lfixQi = lreaxFFqfix(nbosptr(nboatomRptr(1)))
    if (lfixQi) then
      nfi = 0
    else
      nfi = 1
    endif
  endif
!
!  Loop over first atom
!
  do ii = imin,nboatom
    i = nboatomRptr(ii)
    if (i.gt.0) then
      nspeci = nbosptr(i)
      lfixQi = lreaxFFqfix(nspeci)
      if (.not.lfixQi) nfi = nfi + 1
!
!  Does i have a reaxFF species?
!
      if (nbos(ii).gt.0.or.lfixQi) then
        gammai = reaxFFgamma(nspeci)
!
!  Set upper bound for j loop
!
        if (ndim.gt.0) then
          jmax = ii
        else
          jmax = ii - 1
        endif
!
!  Loop over second atom
!
        nfj = 0
        jloop: do jj = 1,jmax
          j = nboatomRptr(jj)
          if (j.gt.0) then
            nspecj = nbosptr(j)
            lfixQj = lreaxFFqfix(nspecj)
            if (.not.lfixQj) nfj = nfj + 1
!
!  Does j have a reaxFF species?
!
            if (nbos(jj).gt.0.or.lfixQj) then
!
!  Skip loop for pairs of fixed atoms
!
              if (lfixQi.and.lfixQj) then
                cycle jloop
              elseif (.not.lfixQi.and..not.lfixQj) then
!
!  Only increment matrix element for pairs of free atoms
!
                ind = ind + 1
                lmixed = .false.
              else
                lmixed = .true.
              endif
!
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
                call rsearch3D(xji,yji,zji,.false.,.false.,i,j,0_i4,0_i4,0_i4,0_i4,nor,nmolonly,lself,cut2)
              elseif (ndim.eq.2) then
                call rsearch2D(xji,yji,zji,.false.,.false.,i,j,0_i4,0_i4,0_i4,0_i4,nor,nmolonly,lself,cut2)
              elseif (ndim.eq.1) then
                call rsearch1D(xji,yji,zji,.false.,.false.,i,j,0_i4,0_i4,0_i4,0_i4,nor,nmolonly,lself,cut2)
              elseif (ndim.eq.0) then
                rij2 = xji*xji + yji*yji + zji*zji
                if (rij2.lt.cut2) then
                  nor = nor + 1
                endif
              endif
!
!  If no distances then cycle
!
              if (nor.eq.0) cycle jloop
!
!  Compute Coulomb shielding parameters 
!
              gammaij = sqrt(gammai*gammaj)
              gammaij = 1.0_dp/(gammaij**3)
!
!  Loop over valid distances and calculate contributions
!
              if (ndim.gt.0) then
                do n = 1,nor
                  rij2 = dist(n)
                  rij = sqrt(rij2)
!
!  Compute taper function
!
                  call p7reaxFFqtaper(rij,reaxFFcutoffQ,tp,dtpdr,d2tpdr2,.false.,.false.)
!                     
!  Pure real space tapered lreaxFF case
!                     
                  gam = tp/(rij2*rij + gammaij)**third
                  if (lmixed) then
                    if (lfixQi) then
                      z(nfj) = z(nfj) - gam*reaxFFqfix(nspeci)*angstoev
                    else
                      z(nfi) = z(nfi) - gam*reaxFFqfix(nspecj)*angstoev
                    endif
                  else
                    dpacked(ind) = dpacked(ind) + gam
                  endif
                enddo
              else
                rij = sqrt(rij2)
!
!  Compute taper function
!
                call p7reaxFFqtaper(rij,reaxFFcutoffQ,tp,dtpdr,d2tpdr2,.false.,.false.)
!                     
!  Pure real space tapered lreaxFF case
!                     
                gam = tp/(rij2*rij + gammaij)**third
                if (lmixed) then
                  if (lfixQi) then
                    z(nfj) = z(nfj) - gam*reaxFFqfix(nspeci)*angstoev
                  else
                    z(nfi) = z(nfi) - gam*reaxFFqfix(nspecj)*angstoev
                  endif
                else
                  dpacked(ind) = dpacked(ind) + gam
                endif
              endif
            endif
          endif
!
!  End of loop over j
!
        enddo jloop
!
!  For 0-D case we need to increment ind by 1 to allow for diagonal element that is skipped
!
        if (ndim.eq.0.and..not.lfixQi) then
          ind = ind + 1
        endif
      endif
    endif
!
!  End of loop over i
!
  enddo
!
!  Scale matrix elements by conversion factor
!
  n = nfree + 1
  do i = 1,n*(n+1)/2
    dpacked(i) = dpacked(i)*angstoev
  enddo
!     
!  Allocate workspace for inversion
!     
  allocate(ipivot(n),stat=status)
  if (status/=0) call outofmemory('setreaxffQ','ipivot')
!
!  If this an iterative run then save the original dpacked matrix to avoid recomputing it
!
  if (literative) then
    do i = 1,n*(n+1)/2
      dpackedsave(i) = dpacked(i)
    enddo
    nitermax = nreaxFFqiter
  else
    nitermax = 1
  endif
!****************************
!  Start of iterative loop  *
!****************************
  do niter = 1,nitermax
!
!  If this is not the first time then copy saved dpacked back
!
    if (niter.gt.1) then
      n = nfree + 1
      do i = 1,n*(n+1)/2
        dpacked(i) = dpackedsave(i)
      enddo
    endif
!
!  Save qreaxFF for later comparison
!
    qreaxFFsave(1:nboatom) = qreaxFF(1:nboatom) 
!********************************
!  Form matrix of coefficients  *
!********************************
    do i = 1,nfree
      ii = nfreeptr(i)
      nati = nat(ii)
      ind = i*(i+1)/2
      dpacked(ind) = dpacked(ind) + 2.0_dp*reaxFFmu(nati)*occuf(ii)
!
!  Shell structure option
!
      if (abs(reaxFFshell(1,nati)).gt.1.0d-12) then
        if (qreaxFF(ii).gt.reaxFFshell(2,nati)) then
          dpacked(ind) = dpacked(ind) + 4.0_dp*reaxFFshell(1,nati)*occuf(ii)*(qreaxFF(ii) - reaxFFshell(2,nati))**3
        endif
      endif
    enddo
    ind = nfree*(nfree + 1)/2 
    do i = 1,nfree
      dpacked(ind+i) = 1.0_dp
    enddo
!
!  Debugging output
!
    if (index(keyword,'debu').ne.0.and.ioproc) then
      write(ioout,'(/,''  ReaxFF Charge Matrix :'',/)')
      ind = 0
      do i = 1,nfree + 1
        write(ioout,'(1x,f9.5,4x,9(1x,f9.5))') z(i),(dpacked(j),j=ind+1,ind+i)
        ind = ind + i
      enddo
    endif
    if (nfree.gt.0) then
!******************
!  Invert matrix  *
!******************
      ifail = 0
!     
!  Factorise matrix
!     
      n = nfree + 1
      call dsptrf('U',n,dpacked,ipivot,ifail)
      if (ifail.eq.0) then
!
!  Copy z to qreaxFFfree
!
        qreaxFFfree(1:n) = z(1:n)
!     
!  Solve for new charges
!     
        call dsptrs('U',n,1_i4,dpacked,ipivot,qreaxFFfree,n,ifail)
      endif
!
!  Was inversion successful?
!
      if (ifail.ne.0) then
        call outerror('charge solution failed in setreaxffQ',0_i4)
        call stopnow('setreaxffQ')
      endif
    endif
!
!  Expand back to full array
!
    nfi = 0
    do i = 1,nboatom
      ii = nboatomRptr(i)
      if (ii.gt.0) then
        nspeci = nbosptr(ii)
        if (lreaxFFqfix(nspeci)) then
          qreaxFF(ii) = reaxFFqfix(nspeci)
        else
          nfi = nfi + 1
          qreaxFF(ii) = qreaxFFfree(nfi)
        endif
      endif
    enddo
!
    if (literative) then
!
!  Check for convergence
!
      qdiff  = 0.0_dp
      qdiff1 = 0.0_dp
      do i = 1,nboatom
        qdiff  = qdiff + abs(qreaxFF(i) - qreaxFFsave(i))
        qdiff1 = max(qdiff1,abs(qreaxFF(i) - qreaxFFsave(i)))
      enddo
      qdiff = qdiff/dble(nboatom)
      if (index(keyword,'debu').ne.0.and.ioproc) then
        write(ioout,'(''  ** Cycle : '',i4,'' Qdiffs : '',2f10.8)') niter,qdiff,qdiff1
      endif
      if (qdiff.lt.reaxFFqconverged.and.qdiff1.lt.reaxFFqconverged1) exit 
!
!  If not converged then damp charges for next iteration except for first iteration
!
      if (niter.gt.1) then
        damp = reaxFFqdamp
        do i = 1,nboatom
          qreaxFF(i) = damp*qreaxFF(i) + (1.0_dp - damp)*qreaxFFsave(i)
        enddo
      endif
      if (index(keyword,'debu').ne.0.and.ioproc) then
        write(ioout,'(//,''  Charges for ReaxFF during iteration :'',/)')
        write(ioout,'(''--------------------------------------------------------------------------------'')')
        write(ioout,'(''    Atom no.            Atomic No.             Charge'')')
        write(ioout,'(''--------------------------------------------------------------------------------'')')
        do i = 1,nboatom
          ii = nboatomRptr(i)
          if (ii.gt.0) then
            write(ioout,'(6x,i4,18x,i2,16x,f10.7)') i,nat(ii),qreaxFF(i)
          endif
        enddo
        write(ioout,'(''--------------------------------------------------------------------------------'',/)')
      endif
    endif
!**************************
!  End of iterative loop  *
!**************************
  enddo
!     
!  Free workspace for inversion
!     
  deallocate(ipivot,stat=status)  
  if (status/=0) call deallocate_error('setreaxffQ','ipivot')
!**************************
!  Calculate self energy  *
!**************************
  eself = 0.0_dp
  do i = 1,nfree
    ii = nfreeptr(i)
    qi = qreaxFF(ii)
    nati = nat(ii)
    esite = qi*occuf(ii)*(reaxFFchi(nati)+qi*reaxFFmu(nati))
    eself = eself + esite
    siteenergy(ii) = siteenergy(ii) + esite
!
!  Shell structure correction
!
    if (abs(reaxFFshell(1,nati)).gt.1.0d-12) then
      if (qi.gt.reaxFFshell(2,nati)) then
        esite = occuf(ii)*reaxFFshell(1,nati)*(qi - reaxFFshell(2,nati))**4
        eself = eself + esite
        siteenergy(ii) = siteenergy(ii) + esite
      endif
    endif
  enddo
!*******************
!  Output results  *
!*******************
  if (index(keyword,'debu').ne.0.and.ioproc) then
    write(ioout,'(//,''  Final charges from ReaxFF :'',/)')
    write(ioout,'(''--------------------------------------------------------------------------------'')')
    write(ioout,'(''    Atom no.            Atomic No.             Charge'')')
    write(ioout,'(''--------------------------------------------------------------------------------'')')
    do i = 1,nboatom
      ii = nboatomRptr(i)
      if (ii.gt.0) then
        write(ioout,'(6x,i4,18x,i2,16x,f10.7)') i,nat(ii),qreaxFF(i)
      endif
    enddo
    write(ioout,'(''--------------------------------------------------------------------------------'',/)')
  endif
!
!  Free local memory 
!
  if (literative) then
    deallocate(dpackedsave,stat=status)
    if (status/=0) call deallocate_error('setreaxffQ','dpackedsave')
  endif
  deallocate(dpacked,stat=status)
  if (status/=0) call deallocate_error('setreaxffQ','dpacked')
  deallocate(qreaxFFsave,stat=status)
  if (status/=0) call deallocate_error('setreaxffQ','qreaxFFsave')
  deallocate(qreaxFFfree,stat=status)
  if (status/=0) call deallocate_error('setreaxffQ','qreaxFFfree')
  deallocate(z,stat=status)
  if (status/=0) call deallocate_error('setreaxffQ','z')
  deallocate(nfreeptr,stat=status)
  if (status/=0) call deallocate_error('setreaxffQ','nfreeptr')
!
  return
  end
