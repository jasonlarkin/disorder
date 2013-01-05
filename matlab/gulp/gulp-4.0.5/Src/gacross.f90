  subroutine gacross(ngacfg,mcfg,nvar,ndiscret,nbsl,pcross,xc,xmin,xmax,iseed,l2pxo,nspar)
!
!  Perform cross over step in genetic algorithm with probability pcross.
!  Called from gafit and gaopt.
!
!   1/08 random -> GULP_random
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
!  Julian Gale, NRI, Curtin University, January 2008
!
!
!  Passed arrays and variables
!
  use datatypes
  use gaconf, only : xconf
  implicit none
!
!  Passed variables
!
  integer(i4)                              :: iseed
  integer(i4)                              :: mcfg
  integer(i4)                              :: nbsl
  integer(i4)                              :: ndiscret(*)
  integer(i4)                              :: ngacfg
  integer(i4)                              :: nspar
  integer(i4)                              :: nvar
  logical                                  :: l2pxo
  real(dp)                                 :: pcross
  real(dp)                                 :: xc(*)
  real(dp)                                 :: xmax(*)
  real(dp)                                 :: xmin(*)
!
!  Local variables
!
  integer(i4)                              :: gridpt1
  integer(i4)                              :: gridpt2
  integer(i4)                              :: ibin1(30)
  integer(i4)                              :: ibin2(30)
  integer(i4)                              :: itemp(30)
  integer(i4)                              :: i
  integer(i4)                              :: ileft
  integer(i4)                              :: ind
  integer(i4)                              :: inds
  integer(i4)                              :: j
  integer(i4)                              :: mx
  integer(i4)                              :: nsel1
  integer(i4)                              :: nsel2
  integer(i4)                              :: ncp
  integer(i4)                              :: ncv
  integer(i4)                              :: nd
  integer(i4)                              :: nx
  logical                                  :: lfound
  real(dp)                                 :: rnc
  real(dp)                                 :: GULP_random
  real(dp)                                 :: rncp
  real(dp)                                 :: ngridpts
  real(dp)                                 :: xmi
  real(dp)                                 :: xlength
  real(dp)                                 :: xint
  real(dp)                                 :: xc1
  real(dp)                                 :: xc2
!
!  Set up one or two point cross-over of DNA
!
  if (l2pxo) then
    mx=2
  else
    mx=1
  endif
  do nx=1,mx
!********************
!  Loop over pairs  *
!********************
    do i = 1+nspar,ngacfg,2
!
!  Select two configurations, nsel1 and nsel2, to pair
!
      nsel1 = i + mcfg
      nsel2 = i + 1 + mcfg
!
!  Decide whether to crossover
!
      rnc = GULP_random(iseed,1_i4)
      if (rnc.le.pcross) then
!
!  Select crossover point
!
        rncp = GULP_random(iseed,1_i4)
        ncp = rncp*dble(nbsl) + 1
!
!  Make crossover
!
        ind = 0
        lfound = .false.
        j = 0
!
!  Find variable which is split by crossover
!
        do while (j.lt.nvar.and..not.lfound)
          j = j + 1
          ind = ind + ndiscret(j)
          if (ind.gt.ncp) lfound = .true.
        enddo
        ncv = j
        if (ncv.gt.1) then
!
!  Exchange crossed parts
!
          do j = 1,ncv-1
            xc(j) = xconf(j,nsel1)
            xconf(j,nsel1) = xconf(j,nsel2)
            xconf(j,nsel2) = xc(j)
          enddo
        endif
!
!  Split variable crossover
!
        nd = ndiscret(ncv)
        ngridpts = 2.0_dp**nd
        inds = ind - nd
        ileft = ncp - inds
        xmi = xmin(j)
        xlength = xmax(j) - xmi
        xint = xlength/ngridpts
        xc1 = xconf(ncv,nsel1)
        xc2 = xconf(ncv,nsel2)
        gridpt1 = nint((xc1-xmi)/xint)
        gridpt2 = nint((xc2-xmi)/xint)
        call inttobin(gridpt1,ibin1,nd)
        call inttobin(gridpt2,ibin2,nd)
        do j = nd,nd-ileft+1,-1
          itemp(j) = ibin1(j)
          ibin1(j) = ibin2(j)
          ibin2(j) = itemp(j)
        enddo
        call bintoint(gridpt1,ibin1,nd)
        call bintoint(gridpt2,ibin2,nd)
        xconf(ncv,nsel1) = xmi + xint*gridpt1
        xconf(ncv,nsel2) = xmi + xint*gridpt2
      endif
    enddo
!************************
!  End loop over pairs  *
!************************
  enddo
!
  return
  end
