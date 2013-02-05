  subroutine setreaxFF
!
!  Subroutine sets the parameters for the ReaxFF potential
!  model of van Duin and co-workers.
!
!  10/02 Created
!   7/07 Setting of cutoffs based on drop tolerance added
!   8/07 Initialisation and setting of reaxFF chi, mu & gamma removed
!        since these values can now be input
!   9/07 Modified so that a cutoff can be input. If this is done then
!        the tolerance is ignored.
!  11/07 Coulomb taper arrays set up
!   4/08 Cutoff now set based on bond order tolerance
!   4/08 Precomputation of VDW & Q terms added to avoid multiple sqrts later
!   4/08 Pi and pi-pi bond order tolerance cutoffs removed since these should
!        always be less than sigma value.
!   4/08 Check for presence of shells added
!   4/08 Second derivatives turned off for now
!   1/09 Check to pick up pairs with no bond order parameters added
!   6/09 Only print out warning once trap added
!   5/10 Maximum cutoff for all species set to be the same to avoid potential
!        for inconsistency in neighbour lists
!  12/11 reaxFFgamma now referenced by species number
!  12/11 Check on bond orders for pairs modified to allow for the fact that
!        fixed charge atoms are now in nreaxFFspec list.
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
  use datatypes
  use control,       only : keyword, lnoanald2
  use element
  use iochannels,    only : ioout
  use parallel,      only : ioproc
  use reaxFFdata
  use shell,         only : nshell
  use two,           only : pts0_7, pts1_7, pts2_7, pts3_7
  use two,           only : pts4_7, pts5_7, pts6_7, pts7_7
  implicit none
!
!  Local variables
!
  integer(i4)            :: i
  integer(i4)            :: ind
  integer(i4)            :: j
  logical                :: lpairsOK
  logical,          save :: lprintwarning = .true.
  real(dp)               :: delta
  real(dp)               :: rdelta7
  real(dp)               :: reaxFFcutoff2
  real(dp)               :: reaxFFcutoff3
  real(dp)               :: reaxFFormax
  real(dp)               :: rsigma
!
!  Prevent runs with shells for now
!
  if (nshell.gt.0.and.nreaxffspec.gt.0) then
    call outerror('ReaxFF is current incompatible with the shell model',0_i4)
    call stopnow('setreaxFF')
  endif
!
!  Turn off second derivatives
!
  lnoanald2 = .true.
!
!  Check that there are bond order parameters for all pairs of species
!
  ind = 0
  lpairsOK = .true.
  do i = 1,nreaxFFspec
    do j = 1,i
      ind = ind + 1
      if (.not.lreaxFFpboOK(ind).and.(.not.lreaxFFqfix(i).and..not.lreaxFFqfix(j))) lpairsOK = .false.
    enddo
  enddo
  if (.not.lpairsOK) then
!
!  Only print this warning once
!
    if (lprintwarning) then
      call outwarning('bond order parameters are not specified for all pairs in ReaxFF',0_i4)
      lprintwarning = .false.
    endif
  endif
!
!  If the cutoff is already set then apply to all species
!
  if (reaxFFcutoff.gt.1.0d-12) then
    ind = 0
    reaxFFrmax(1:nreaxFFspec) = 0.0_dp
    reaxFFormax = 0.0_dp
    do i = 1,nreaxFFspec
      do j = 1,i
        ind = ind + 1
!
!  Find cutoffs at which bond order is below tolerance
!
        call reaxFFbocut_sigma(ind,i,j,reaxFFtol,rsigma)
!
!  Set overall cutoff to be that from the bond order tolerance or the cutoff, which ever is lower
!
        reaxFFrmaxpair(ind) = min(reaxFFcutoff,rsigma)
        reaxFFormax   = max(reaxFFormax,reaxFFrmaxpair(ind))
        reaxFFrmax(i) = max(reaxFFrmax(i),reaxFFrmaxpair(ind))
        reaxFFrmax(j) = max(reaxFFrmax(j),reaxFFrmaxpair(ind))
      enddo
    enddo
!
!  Set maximum cutoff for all species to be the same to avoid inconsistency in neighbour lists
!
    do i = 1,nreaxFFspec
      reaxFFrmax(i) = reaxFFormax
    enddo
!
!  Set taper parameters
!
    delta  = reaxFFcutoff
    rdelta7 = 1.0_dp/delta**7.0_dp
!
    reaxFFcutoff2 = reaxFFcutoff*reaxFFcutoff
    reaxFFcutoff3 = reaxFFcutoff2*reaxFFcutoff
!
    pts7_7 =  20.0_dp*rdelta7
    pts6_7 = -70.0_dp*reaxFFcutoff*rdelta7
    pts5_7 =  84.0_dp*reaxFFcutoff2*rdelta7
    pts4_7 = -35.0_dp*reaxFFcutoff3*rdelta7
    pts3_7 =   0.0_dp
    pts2_7 =   0.0_dp
    pts1_7 =   0.0_dp
    pts0_7 =   1.0_dp
  else
!
!  Set cutoff radii based on drop tolerance so that all bond order terms are less than this tolerance
!
    ind = 0
    reaxFFrmax(1:nreaxFFspec) = 0.0_dp
    do i = 1,nreaxFFspec
      do j = 1,i
        ind = ind + 1
        call reaxFFbocut_sigma(ind,i,j,reaxFFtol,rsigma)
!
        reaxFFrmaxpair(ind) = rsigma
        reaxFFrmax(i) = max(reaxFFrmax(i),reaxFFrmaxpair(ind))
        reaxFFrmax(j) = max(reaxFFrmax(j),reaxFFrmaxpair(ind))
      enddo
    enddo
  endif
  if (index(keyword,'verb').ne.0.and.ioproc) then
    write(ioout,'(/,''  ReaxFF: Bond order cutoffs: '',/)')
    do i = 1,nreaxFFspec
      write(ioout,'(''  Species number = '',i4,f10.5)') i,reaxFFrmax(i)
    enddo
    write(ioout,'(/)')
  endif
!
!  Set VDW cutoff and taper parameters
!
  if (reaxFFcutoffVDW.gt.1.0d-12) then
    rdelta7 = 1.0_dp/reaxFFcutoffVDW**7.0_dp
    reaxFFtaperVDW(8) =  20.0_dp*rdelta7
    rdelta7 = rdelta7*reaxFFcutoffVDW
    reaxFFtaperVDW(7) = -70.0_dp*rdelta7
    rdelta7 = rdelta7*reaxFFcutoffVDW
    reaxFFtaperVDW(6) =  84.0_dp*rdelta7
    rdelta7 = rdelta7*reaxFFcutoffVDW
    reaxFFtaperVDW(5) = -35.0_dp*rdelta7
    reaxFFtaperVDW(4) =   0.0_dp
    reaxFFtaperVDW(3) =   0.0_dp
    reaxFFtaperVDW(2) =   0.0_dp
    reaxFFtaperVDW(1) =   1.0_dp
  else
    reaxFFtaperVDW(1:8) = 0.0_dp
  endif
!
!  Set Coulomb cutoff and taper parameters
!
  if (reaxFFcutoffQ.gt.1.0d-12) then
    rdelta7 = 1.0_dp/reaxFFcutoffQ**7.0_dp
    reaxFFtaperQ(8) =  20.0_dp*rdelta7
    rdelta7 = rdelta7*reaxFFcutoffQ
    reaxFFtaperQ(7) = -70.0_dp*rdelta7
    rdelta7 = rdelta7*reaxFFcutoffQ
    reaxFFtaperQ(6) =  84.0_dp*rdelta7
    rdelta7 = rdelta7*reaxFFcutoffQ
    reaxFFtaperQ(5) = -35.0_dp*rdelta7
    reaxFFtaperQ(4) =   0.0_dp
    reaxFFtaperQ(3) =   0.0_dp
    reaxFFtaperQ(2) =   0.0_dp
    reaxFFtaperQ(1) =   1.0_dp
  else
    reaxFFtaperQ(1:8) = 0.0_dp
  endif
!
!  Set pairwise VDW and Coulomb parameters needed later
!
  ind = 0
  do i = 1,nreaxFFspec
    do j = 1,i
      ind = ind + 1
      if (lreaxFFmorseinput(ind)) then
        reaxFFDeVDW(ind)    = reaxFFmorse(1,ind)
        reaxFFalphaVDW(ind) = reaxFFmorse(2,ind)
        reaxFFr0VDW(ind)    = 2.0_dp*reaxFFmorse(3,ind)
      else
        reaxFFDeVDW(ind)    = sqrt(reaxFFeps(i)*reaxFFeps(j))
        reaxFFalphaVDW(ind) = sqrt(reaxFFalpha(i)*reaxFFalpha(j))
        reaxFFr0VDW(ind)    = 2.0_dp*sqrt(reaxFFrvdw(i)*reaxFFrvdw(j))
      endif
      reaxFFgammaVDW(ind) = sqrt(reaxFFgammaw(i)*reaxFFgammaw(j))
      reaxFFgammaQ(ind)   = sqrt(reaxFFgamma(i)*reaxFFgamma(j))
      reaxFFgammaQ(ind)   = 1.0_dp/(reaxFFgammaQ(ind)**3)
    enddo
  enddo
!
  return
  end
