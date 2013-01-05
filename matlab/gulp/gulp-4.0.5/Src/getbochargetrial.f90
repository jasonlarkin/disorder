  subroutine getBOchargetrial(ntrialatom,nptrtrialatom)
!
!  Calculates the charges according to the bond order formalism for a subset of atoms.
!
!   1/08 Created from getBOcharge
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
!  Julian Gale, NRI, Curtin University, April 2008
!
  use datatypes
  use bondorderdata
  use current
  use iochannels
  use neighbours
  use spatial
  use times
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)                        :: ntrialatom
  integer(i4), intent(in)                        :: nptrtrialatom(ntrialatom)
!
!  Local variables
!
  integer(i4)                                    :: i
  integer(i4)                                    :: ii
  integer(i4)                                    :: j
  integer(i4)                                    :: m
  integer(i4)                                    :: nati
  integer(i4)                                    :: natj
  integer(i4)                                    :: nboij
  integer(i4)                                    :: ni
  integer(i4), dimension(:,:), allocatable, save :: neighno
  integer(i4), dimension(:),   allocatable, save :: nneigh
  integer(i4)                                    :: nt
  integer(i4)                                    :: ntypi
  integer(i4)                                    :: ntypj
  integer(i4)                                    :: status
  logical                                        :: lfound
  logical                                        :: lfound1
  logical                                        :: lmaxneighok
  logical                                        :: lok
  real(dp)                                       :: bR22
  real(dp)                                       :: cputime
  real(dp)                                       :: dfdr
  real(dp)                                       :: d2fdr2
  real(dp)                                       :: d3fdr3
  real(dp)                                       :: f
  real(dp),    dimension(:),   allocatable, save :: rBOcutmax
  real(dp)                                       :: rij
  real(dp)                                       :: r2
  real(dp)                                       :: t1
  real(dp)                                       :: t2
  real(dp),    dimension(:,:), allocatable, save :: rneigh
  real(dp),    dimension(:,:), allocatable, save :: xneigh
  real(dp),    dimension(:,:), allocatable, save :: yneigh
  real(dp),    dimension(:,:), allocatable, save :: zneigh
  real(dp)                                       :: xji
  real(dp)                                       :: yji
  real(dp)                                       :: zji
  real(dp)                                       :: xji0
  real(dp)                                       :: yji0
  real(dp)                                       :: zji0
!
  t1 = cputime()
!
!  Allocate memory that does not depend on maxneigh
!
  allocate(nneigh(ntrialatom),stat=status)
  if (status/=0) call outofmemory('getBOchargetrial','nneigh')
  allocate(rBOcutmax(ntrialatom),stat=status)
  if (status/=0) call outofmemory('getBOchargetrial','rBOcutmax')
!
!  Reinitialisation point should maxneigh be increased
!
100 continue
  lmaxneighok = .true.
  if (allocated(rneigh)) then
    deallocate(zneigh,stat=status)
    if (status/=0) call deallocate_error('getBOchargetrial','zneigh')
    deallocate(yneigh,stat=status)
    if (status/=0) call deallocate_error('getBOchargetrial','yneigh')
    deallocate(xneigh,stat=status)
    if (status/=0) call deallocate_error('getBOchargetrial','xneigh')
    deallocate(rneigh,stat=status)
    if (status/=0) call deallocate_error('getBOchargetrial','rneigh')
    deallocate(neighno,stat=status)
    if (status/=0) call deallocate_error('getBOchargetrial','neighno')
  endif
!
!  Initialise charges
!
  do nt = 1,ntrialatom
    i = nptrtrialatom(nt)
    qf(i) = 0.0_dp
  enddo
!
!  Allocate local memory
!
  allocate(neighno(maxneigh,ntrialatom),stat=status)
  if (status/=0) call outofmemory('getBOchargetrial','neighno')
  allocate(rneigh(maxneigh,ntrialatom),stat=status)
  if (status/=0) call outofmemory('getBOchargetrial','rneigh')
  allocate(xneigh(maxneigh,ntrialatom),stat=status)
  if (status/=0) call outofmemory('getBOchargetrial','xneigh')
  allocate(yneigh(maxneigh,ntrialatom),stat=status)
  if (status/=0) call outofmemory('getBOchargetrial','yneigh')
  allocate(zneigh(maxneigh,ntrialatom),stat=status)
  if (status/=0) call outofmemory('getBOchargetrial','zneigh')
!*************************************
!  Find cut-off radii for all atoms  *
!*************************************
  do nt = 1,ntrialatom
    i = nptrtrialatom(nt)
    nati = nat(i)
    ntypi = nftype(i)
    rBOcutmax(i) = 0.0_dp
!
!  Check twobody potentials
!
    do j = 1,nboQ
      if (nati.eq.nBOspecQ1(j).and.(ntypi.eq.nBOtypQ1(j).or.nBOtypQ1(j).eq.0)) then
        rBOcutmax(i) = max(rBOcutmax(i),rBOmaxQ(j))
      endif
      if (nati.eq.nBOspecQ2(j).and.(ntypi.eq.nBOtypQ2(j).or.nBOtypQ2(j).eq.0)) then
        rBOcutmax(i) = max(rBOcutmax(i),rBOmaxQ(j))
      endif
    enddo
  enddo
!****************************************
!  Calculate neighbour lists for atoms  *
!****************************************
  do nt = 1,ntrialatom
    i = nptrtrialatom(nt)
    nneigh(nt) = 0
    nati = nat(i)
    ntypi = nftype(i)
!     
!  Compute square of cut-off for distance checking   
!     
    bR22 = rBOcutmax(i)**2
!
!  Loop over atoms
!
    do j = 1,numat
      natj = nat(j)
      ntypj = nftype(j)
!
!  Set centre cell coordinate differences
!
      xji0 = xclat(j) - xclat(i)
      yji0 = yclat(j) - yclat(i)
      zji0 = zclat(j) - zclat(i)
!
!  Loop over unit cells
!
      do ii = 1,iimax
!
!  Exclude self term
!
        if (i.ne.j.or.ii.ne.iimid) then
          xji = xji0 + xvec1cell(ii)
          yji = yji0 + yvec1cell(ii)
          zji = zji0 + zvec1cell(ii)
          r2 = xji*xji + yji*yji + zji*zji
          if (r2 .lt. bR22) then
            m = 0
            lok = .false.   
            do while (m.lt.nboQ.and..not.lok)
              m = m + 1   
              if (nati.eq.nBOspecQ1(m).and.(ntypi.eq.nBOtypQ1(m).or.nBOtypQ1(m).eq.0).and. &
                  natj.eq.nBOspecQ2(m).and.(ntypj.eq.nBOtypQ2(m).or.nBOtypQ2(m).eq.0)) then
                lok = (r2.lt.rBOmaxQ(m)**2)
              elseif (nati.eq.nBOspecQ2(m).and.(ntypi.eq.nBOtypQ2(m).or.nBOtypQ2(m).eq.0).and. &
                  natj.eq.nBOspecQ1(m).and.(ntypj.eq.nBOtypQ1(m).or.nBOtypQ1(m).eq.0)) then
                lok = (r2.lt.rBOmaxQ(m)**2)
              endif
            enddo
            if (lok) then
              if (nneigh(nt).ge.maxneigh.or..not.lmaxneighok) then
                lmaxneighok = .false.
                nneigh(nt) = nneigh(nt) + 1
              else
                rij = sqrt(r2)
                nneigh(nt) = nneigh(nt) + 1
                neighno(nneigh(nt),nt) = j
                rneigh(nneigh(nt),nt) = rij
                xneigh(nneigh(nt),nt) = xji
                yneigh(nneigh(nt),nt) = yji
                zneigh(nneigh(nt),nt) = zji
              endif
            endif
          endif
        endif
      enddo
    enddo
  enddo
!**********************************************************************
!  If maxneigh has been exceeded, increase value and return to start  *
!**********************************************************************
  if (.not.lmaxneighok) then
    do nt = 1,ntrialatom
      if (nneigh(nt).gt.maxneigh) maxneigh = nneigh(nt)
    enddo
    goto 100
  endif
!*****************************
!  Loop over pairs of atoms  *
!*****************************
  do nt = 1,ntrialatom
    i = nptrtrialatom(nt)
!
!  Set variables relating to i
!
    nati = nat(i)
    ntypi = nftype(i)
!
!  Loop over neighbours of i (=> j)
!
    ni = 1
    do while (ni.le.nneigh(nt))
!
      j = neighno(ni,nt)
!
!  Set variables relating to j
!
      natj = nat(j)
      ntypj = nftype(j)
!
!  Set up i-j quantities
!
      rij = rneigh(ni,nt)
      xji = xneigh(ni,nt)
      yji = yneigh(ni,nt)
      zji = zneigh(ni,nt)
!
!  Find two-body bond order potential between i and j
!
      lfound = .false.
      lfound1 = .false.
      nboij = 0
      do while (.not.lfound.and.nboij.lt.nboQ) 
        nboij = nboij + 1
        if (nBOspecQ1(nboij).eq.nati.and.nBOspecQ2(nboij).eq.natj) then
          if ((nBOtypQ1(nboij).eq.ntypi.or.nBOtypQ1(nboij).eq.0).and. &
              (nBOtypQ2(nboij).eq.ntypj.or.nBOtypQ2(nboij).eq.0)) then
            lfound = .true.
            lfound1 = .true.
          endif
        elseif (nBOspecQ1(nboij).eq.natj.and.nBOspecQ2(nboij).eq.nati) then
          if ((nBOtypQ1(nboij).eq.ntypj.or.nBOtypQ1(nboij).eq.0).and. &
              (nBOtypQ2(nboij).eq.ntypi.or.nBOtypQ2(nboij).eq.0)) then
            lfound = .true.
          endif
        endif
      enddo
      if (lfound) then
!***********************************************
!  Valid two-body bond order charge potential  *
!***********************************************
!
!  Calculate fij
!
        if (nBOtaperQ(nboij).eq.2) then
          call staper(rij,rBOminQ(nboij),rBOmaxQ(nboij),f,dfdr,d2fdr2,d3fdr3,.false.,.false.,.false.)
        else
          call ctaper(rij,rBOminQ(nboij),rBOmaxQ(nboij),f,dfdr,d2fdr2,d3fdr3,.false.,.false.,.false.)
        endif
!
!  Add contribution to charge
!
        if (lfound1) then
          qf(i) = qf(i) + BOq0(nboij)*f
        else
          qf(i) = qf(i) - BOq0(nboij)*f
        endif
      endif
!
!  End of loop over neighbours of i
!
      ni = ni + 1
    enddo
!****************************
!  Propagate charges to qa  *
!****************************
    qa(i) = qf(i)
  enddo
!
!  Free local memory
!
  deallocate(zneigh,stat=status)
  if (status/=0) call deallocate_error('getBOchargetrial','zneigh')
  deallocate(yneigh,stat=status)
  if (status/=0) call deallocate_error('getBOchargetrial','yneigh')
  deallocate(xneigh,stat=status)
  if (status/=0) call deallocate_error('getBOchargetrial','xneigh')
  deallocate(rneigh,stat=status)
  if (status/=0) call deallocate_error('getBOchargetrial','rneigh')
  deallocate(neighno,stat=status)
  if (status/=0) call deallocate_error('getBOchargetrial','neighno')
  deallocate(rBOcutmax,stat=status)
  if (status/=0) call deallocate_error('getBOchargetrial','rBOcutmax')
  deallocate(nneigh,stat=status)
  if (status/=0) call deallocate_error('getBOchargetrial','nneigh')
!
  t2 = cputime()
  tbondorder = tbondorder + t2 - t1
!
  return
  end
