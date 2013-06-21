  subroutine setregiontrans(ltmp)
!
!  This routine creates constraints that are needed to perform
!  the rigid translation of regions.
!
!  ltmp = array of logical flags according to whether a
!         parameter can be varied or not. Passed from
!         setcfg.
!
!   5/03 Created
!   9/03 Modified to use logical pointer as to whether region
!        is to be rigid or not.
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
!  Copyright Curtin University 2005
!
!  Julian Gale, NRI, Curtin University, June 2005
!
  use configurations
  use control
  use current
  use element, only : maxele
  implicit none
!
!  Passed variables
!
  logical          :: ltmp(*)
!
!  Local variables
!
  integer(i4)      :: i
  integer(i4)      :: ifirst
  integer(i4)      :: indfx
  integer(i4)      :: indfy
  integer(i4)      :: indfz
  integer(i4)      :: indx
  integer(i4)      :: indy
  integer(i4)      :: indz
  integer(i4)      :: k
  integer(i4)      :: natomthisregion
  integer(i4)      :: ndirection
  integer(i4)      :: nnewcon
  integer(i4)      :: nr
  logical          :: lfirstatom
!**********************
!  Loop over regions  *
!**********************
  do nr = 1,nregions(ncf)
    if (lregionrigid(nr,ncf)) then
!
!  Count number of directions of motion
!
      ndirection = 0
      if (lopfreg(3*(nr-1)+1,ncf)) ndirection = ndirection + 1
      if (lopfreg(3*(nr-1)+2,ncf)) ndirection = ndirection + 2
      if (lopfreg(3*(nr-1)+3,ncf)) ndirection = ndirection + 3
      if (ndirection.gt.0) then
!
!  Find atoms for this region and count
!
        natomthisregion = 0
        do i = 1,nasym
          if (nregionno(nsft+i).eq.nr) then
            natomthisregion = natomthisregion + 1
          endif
        enddo
!
!  Check that there is space in constraint arrays
!
        nnewcon = ndirection*(natomthisregion - 1)
        if (ncontot+nnewcon.ge.maxcontot) then
          maxcontot = ncontot + nnewcon
          call changemaxcontot
        endif
!********************************************
!  Move data to make space for constraints  *
!********************************************
        if (ncf.lt.ncfg) then
          do k = ncontot,n1con(ncf+1),-1
            ncvarcfg(k+nnewcon) = ncvarcfg(k)
            ncfixcfg(k+nnewcon) = ncfixcfg(k)
            concocfg(k+nnewcon) = concocfg(k)
            nconcfg(k+nnewcon) = nconcfg(k)
            conaddcfg(k+nnewcon) = conaddcfg(k)
          enddo
          do k = ncf+1,ncfg
            n1con(k) = n1con(k) + nnewcon
          enddo
        endif
!
!  Find atoms for this region
!
        lfirstatom = .true.
        do i = 1,nasym
          if (nregionno(nsft+i).eq.nr) then
            indx = 3*(i-1) + nstrains + 1
            indy = indx + 1
            indz = indy + 1
!
!  Is this the first atom for the region?
!
            if (lfirstatom) then
              ifirst = i
              indfx = indx
              indfy = indy
              indfz = indz
              lfirstatom = .false.
            endif
            if (i.ne.ifirst) then
!*********************************************
!  Add constraint to atom if not first atom  *
!*********************************************
!
!  X direction
!
              if (lopfreg(3*(nr-1)+1,ncf)) then
                ncon = ncon + 1
                ncontot = ncontot + 1
                ltmp(indx) = .false.
                ncvarcfg(ncon) = indfx
                ncfixcfg(ncon) = indx
                concocfg(ncon) = 1.0_dp
                conaddcfg(ncon) = xcfg(nsft+i) - xcfg(nsft+ifirst)
                nconcfg(ncon) = ncf
              endif
!
!  Y direction
!
              if (lopfreg(3*(nr-1)+2,ncf)) then
                ncon = ncon + 1
                ncontot = ncontot + 1
                ltmp(indy) = .false.
                ncvarcfg(ncon) = indfy
                ncfixcfg(ncon) = indy
                concocfg(ncon) = 1.0_dp
                conaddcfg(ncon) = ycfg(nsft+i) - ycfg(nsft+ifirst)
                nconcfg(ncon) = ncf
              endif
!
!  Z direction
!
              if (lopfreg(3*(nr-1)+3,ncf)) then
                ncon = ncon + 1
                ncontot = ncontot + 1
                ltmp(indz) = .false.
                ncvarcfg(ncon) = indfz
                ncfixcfg(ncon) = indz
                concocfg(ncon) = 1.0_dp
                conaddcfg(ncon) = zcfg(nsft+i) - zcfg(nsft+ifirst)
                nconcfg(ncon) = ncf
              endif
            endif
          endif
        enddo
      endif
    endif
  enddo
!
  return
  end
