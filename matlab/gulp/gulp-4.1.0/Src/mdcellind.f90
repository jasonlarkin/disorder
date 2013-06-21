  subroutine mdcellind(ni,icos1,icos2,icos3)
!
!  Corrects cell indices for MD based on
!  changes in directional cosines. Applies
!  to molecule indices and list terms.
!
!   3/95 Bonding list indices now corrected
!   5/96 Bug in bonded index changes fixed
!  10/06 Fourlist handling modified in line with changes
!   4/07 Searching for bonded atoms now uses nbonds number rather
!        than checking for a non-zero type
!   6/09 Module name changed from three to m_three
!  10/11 Corrections made to handling of cell indices for fourbody potentials
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
!  Julian Gale, NRI, Curtin University, October 2011
!
  use control
  use current
  use four
  use m_three
  use molecule
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in) :: ni
  integer(i4), intent(in) :: icos1
  integer(i4), intent(in) :: icos2
  integer(i4), intent(in) :: icos3
!
!  Local variables
!
  integer(i4)             :: i
  integer(i4)             :: icm
  integer(i4)             :: ii
  integer(i4)             :: ind
  integer(i4)             :: ixd
  integer(i4)             :: iyd
  integer(i4)             :: izd
  integer(i4)             :: j
  integer(i4)             :: jcm
  integer(i4)             :: k
  integer(i4)             :: l
  integer(i4)             :: n
  integer(i4)             :: numat1
  logical                 :: llist
!
!  See if cosines have changed
!
  ixd = icos1 - icosx(ni)
  iyd = icos2 - icosy(ni)
  izd = icos3 - icosz(ni)
!
!  If no change then return
!
  if ((abs(ixd)+abs(iyd)+abs(izd)).eq.0) return
  llist = (index(keyword,'noli').eq.0)
  if (natmol(ni).gt.0) then
!*************
!  Molecule  *
!*************
    ind = nmolind(ni)
    call indsft(ind,ixd,iyd,izd,1_i4)
    nmolind(ni) = ind
!************
!  Bonding  *
!************
!
!  Change in bonding index for other atoms
!
    do i = 1,numat
      icm = nbonded(1,i)
      jcm = 1
      do while (icm.gt.0.and.jcm.le.nbonds(i))
        if (icm.eq.ni) then
          ind = nbondind(jcm,i)
          call indsft(ind,ixd,iyd,izd,1_i4)
          nbondind(jcm,i) = ind
        endif
        jcm = jcm + 1
        icm = nbonded(jcm,i)
      enddo
    enddo
!
!  Change in bonding indices for ni
!
    icm = nbonded(1,ni)
    jcm = 1
    do while (icm.gt.0.and.jcm.le.nbonds(ni))
      ind = nbondind(jcm,ni)
      call indsft(ind,ixd,iyd,izd,-1_i4)
      nbondind(jcm,ni) = ind
      jcm = jcm + 1
      icm = nbonded(jcm,ni)
    enddo
  endif
  if (llist.and.nlist3md.gt.0) then
!********************
!  Three-body list  *
!********************
    do i = 1,nlist3md
      ii = i3ind(i)
      j = j3ind(i)
      k = k3ind(i)
      if (ni.eq.j.or.ni.eq.k) then
        if (ni.eq.j) then
          ind = icell31(i)
        else
          ind = icell32(i)
        endif
        call indsft(ind,ixd,iyd,izd,1_i4)
        if (ni.eq.j) then
          icell31(i) = ind
        else
          icell32(i) = ind
        endif
      elseif (ni.eq.ii) then
        ind = icell31(i)
        call indsft(ind,ixd,iyd,izd,-1_i4)
        icell31(i) = ind
        ind = icell32(i)
        call indsft(ind,ixd,iyd,izd,-1_i4)
        icell32(i) = ind
      endif
    enddo
  endif
  if (llist.and.nlist4md.gt.0) then
    numat1 = (numat + 1)
!*******************
!  Four-body list  *
!*******************
    do i = 1,nlist4md
      n = nforptr(i)
      ind = ilind(i)
      l = ind/numat1
      ii = ind - l*numat1
      ind = jkind(i)
      k = ind/numat1
      j = ind - k*numat1
      if (loutofplane(n)) then
!
!  Out of plane
!
        if (ni.eq.ii) then
          ind = icell41(i)
          call indsft(ind,ixd,iyd,izd,-1_i4)
          icell41(i) = ind
          ind = icell42(i)
          call indsft(ind,ixd,iyd,izd,-1_i4)
          icell42(i) = ind
          ind = icell43(i)
          call indsft(ind,ixd,iyd,izd,-1_i4)
          icell43(i) = ind
        elseif (ni.eq.j) then
          ind = icell41(i)
          call indsft(ind,ixd,iyd,izd,1_i4)
          icell41(i) = ind
        elseif (ni.eq.k) then
          ind = icell42(i)
          call indsft(ind,ixd,iyd,izd,1_i4)
          icell42(i) = ind
        elseif (ni.eq.l) then
          ind = icell43(i)
          call indsft(ind,ixd,iyd,izd,1_i4)
          icell43(i) = ind
        endif
      else
!
!  Standard torsional potential
!
        if (ni.eq.ii) then
          ind = icell41(i)
          call indsft(ind,ixd,iyd,izd,1_i4)
          icell41(i) = ind
        elseif (ni.eq.j) then
          ind = icell41(i)
          call indsft(ind,ixd,iyd,izd,-1_i4)
          icell41(i) = ind
          ind = icell42(i)
          call indsft(ind,ixd,iyd,izd,-1_i4)
          icell42(i) = ind
          ind = icell43(i)
          call indsft(ind,ixd,iyd,izd,-1_i4)
          icell43(i) = ind
        elseif (ni.eq.k) then
          ind = icell42(i)
          call indsft(ind,ixd,iyd,izd,1_i4)
          icell42(i) = ind
        elseif (ni.eq.l) then
          ind = icell43(i)
          call indsft(ind,ixd,iyd,izd,1_i4)
          icell43(i) = ind
        endif
      endif
    enddo
  endif
!********************************
!  Correct stored cell indices  *
!********************************
  icosx(ni) = icos1
  icosy(ni) = icos2
  icosz(ni) = icos3
!
  return
  end
