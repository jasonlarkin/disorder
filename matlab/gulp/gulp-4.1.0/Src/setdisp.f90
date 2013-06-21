  subroutine setdisp
!
!  Setups up k points required for phonon dispersion curves
!
!  nkpt = total number of k points across all structures
!  xkpt = fractional x component of k point
!  ykpt = fractional y component of k point
!  zkpt = fractional z component of k point
!  wkpt = weight of each k point
!  nkptcfg = configuration pointer for each k point
!  ndline  = no. of continuous lines through k space
!  ndpoint = no. of points specified on the lines
!  ndispcfg = configuration pointer for each line
!  ndispres = total no of points in output width
!  xdisp   = x component of reciprocal space point
!  ydisp   = y component of reciprocal space point
!  zdisp   = z component of reciprocal space point
!
!  On entry :
!
!  ndstart = starting dpoint for a line
!  ndend   = finishing dpoint for a line
!
!  On exit :
!
!  ndstart = pointer to first k point for dispersion curve
!  ndend   = pointer to last k point for dispersion curve
!
!   6/95 Insertion of new k points bug fixed
!  12/07 Unused variables removed
!   3/09 lkptdispersion added
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
!  Julian Gale, NRI, Curtin University, March 2009
!
  use current
  use dispersion
  use ksample
  use parallel
  implicit none
!
!  Local variables
!
  integer(i4)                                  :: i
  integer(i4)                                  :: j
  integer(i4)                                  :: k
  integer(i4)                                  :: l
  integer(i4)                                  :: nd
  integer(i4)                                  :: nde
  integer(i4)                                  :: nds
  integer(i4)                                  :: ninsert
  integer(i4)                                  :: nldpt
  integer(i4)                                  :: nperl
  integer(i4)                                  :: nudpt
  integer(i4)                                  :: status
  logical                                      :: lfound
  real(dp)                                     :: rklen
  real(dp),    dimension(:), allocatable       :: rtmp
  real(dp)                                     :: xd
  real(dp)                                     :: yd
  real(dp)                                     :: zd
  real(dp)                                     :: xstep
  real(dp)                                     :: ystep
  real(dp)                                     :: zstep
!
!  Work out lower and upper d lines for this configuration
!
  nldpt = 0
  do i = 1,ndline
    nd = ndispcfg(i)
    if (nldpt.eq.0.and.nd.eq.ncf) nldpt = i
    if (nd.eq.ncf) nudpt = i
  enddo
  if (nldpt.eq.0) return
!
!  Find point at which to insert any new k points such that
!  they remain in order of configuration
!
  if (nkpt.gt.0) then
    i = 1
    lfound = .false.
    do while (i.le.nkpt.and..not.lfound)
      if (nkptcfg(i).gt.ncf) then
        lfound = .true.
        ninsert = i
      endif
      i = i + 1
    enddo
    if (.not.lfound) ninsert = nkpt + 1
  else
    ninsert = 1
  endif
!*************************************************
!  Generate k points along the dispersion lines  *
!*************************************************
!
!  Loop over number of lines
!
  do i = nldpt,nudpt
    nds = ndstart(i)
    nde = ndend(i)
    allocate(rtmp(nde),stat=status)
    if (status/=0) call outofmemory('setdisp','rtmp')
!
!  First work out length of line in k space to generate 
!  most even split of points per section of line
!
    rklen = 0.0_dp
    do j = nds+1,nde
      xd = xdisp(j) - xdisp(j-1)
      yd = ydisp(j) - ydisp(j-1)
      zd = zdisp(j) - zdisp(j-1)
      rtmp(j-nds) = xd*xd + yd*yd + zd*zd
      rklen = rklen + rtmp(j-nds)
    enddo
!
!  Check there is enough space to insert new K point
!
    if (nkpt.ge.maxkpt) then
      maxkpt = nkpt + 10
      call changemaxkpt
    endif
!
!  Split points based on magnitude of fractional vector in kspace.
!  Aim for a total of about thirty points over all sections on line.
!
    if (nkpt.gt.0) then
      do k = nkpt,ninsert,-1
        xkpt(k+1) = xkpt(k)
        ykpt(k+1) = ykpt(k)
        zkpt(k+1) = zkpt(k)
        wkpt(k+1) = wkpt(k)
        nkptcfg(k+1) = nkptcfg(k)
        lkptdispersion(k+1) = lkptdispersion(k)
      enddo
    endif
    nkpt = nkpt + 1
!
!  Add in starting point of line
!
    ndds(i) = ninsert
    xkpt(ninsert) = xdisp(nds)
    if (ndim.ge.2) then
      ykpt(ninsert) = ydisp(nds)
    else
      ykpt(ninsert) = 0.0_dp
    endif
    if (ndim.eq.3) then
      zkpt(ninsert) = zdisp(nds)
    else
      zkpt(ninsert) = 0.0_dp
    endif
    wkpt(ninsert) = 1.0_dp
    nkptcfg(ninsert) = ncf
    lkptdispersion(ninsert) = .true.
    ninsert = ninsert + 1
!
!  Loop over sections
!
    do j = nds+1,nde
      nperl = ndispres*(rtmp(j-nds)/rklen) - 1
      xd = xdisp(j) - xdisp(j-1)
      yd = ydisp(j) - ydisp(j-1)
      zd = zdisp(j) - zdisp(j-1)
      xstep = xd/(float(nperl))
      ystep = yd/(float(nperl))
      zstep = zd/(float(nperl))
!
!  Loop over points along each section
!
      do l = 1,nperl
!
!  Check there is enough space to insert new K point
!
        if (nkpt.ge.maxkpt) then
          maxkpt = nkpt + 10
          call changemaxkpt
        endif
!
!  Make space for new point
!
        do k = nkpt,ninsert,-1
          xkpt(k+1) = xkpt(k)
          ykpt(k+1) = ykpt(k)
          zkpt(k+1) = zkpt(k)
          wkpt(k+1) = wkpt(k)
          lkptdispersion(k+1) = lkptdispersion(k)
        enddo
        nkpt = nkpt + 1
!
!  Add in new point
!
        xkpt(ninsert) = xkpt(ninsert-1) + xstep
        if (ndim.ge.2) then
          ykpt(ninsert) = ykpt(ninsert-1) + ystep
        else
          ykpt(ninsert) = 0.0_dp
        endif
        if (ndim.eq.3) then
          zkpt(ninsert) = zkpt(ninsert-1) + zstep
        else
          zkpt(ninsert) = 0.0_dp
        endif
        wkpt(ninsert) = 1.0_dp
        nkptcfg(ninsert) = ncf
        lkptdispersion(ninsert) = .true.
        ninsert = ninsert + 1
      enddo
    enddo
    ndde(i) = ninsert - 1
    deallocate(rtmp,stat=status)
    if (status/=0) call deallocate_error('setdisp','rtmp')
  enddo
!
  return
  end
