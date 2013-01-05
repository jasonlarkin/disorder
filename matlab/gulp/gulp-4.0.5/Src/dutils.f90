!*********************************************
!  Utility routines for derivative handling  *
!*********************************************
!
!  Note structure of d1 is as follows:
!
!  1 -> nneigh(i) => derivatives between i and neighbours of i
!  nneigh(i) + 1 -> nneigh(i) + nneigh(i)*(nneigh(i) + 1)/2 => derivatives between neighbours of i and each other
!  nneigh(i) + nneigh(i)*(nneigh(i) + 1)/2 + 1 -> nneigh(i) + nneigh(i)*(nneigh(i) + 1)/2 + (nj - 1)*(maxneigh+1)*maxneigh => i to neighbours of neighbours of i
!  nneigh(i) + nneigh(i)*(nneigh(i) + 1)/2 + (nj - 1)*(maxneigh+1)*maxneigh + 1 -> end => neighbours of i to neighbours of neighbours.
!
!
  subroutine d1add(i,maxneigh,mneigh,nneigh,neighno,xneigh,yneigh,zneigh,nfreeatom,nREBOatomRptr,d1,ltorderv)
!
!  Utility to add the pairwise derivatives for a group of atoms
!  on to the global Cartesian first derivative arrays.
!
!  On entry : 
!
!  i               = atom number of i to whom others are connected
!  maxneigh        = left-hand dimension of 2-D arrays
!  mneigh          = value of maxneigh actually used for building index numbers
!  nneigh          = no. of neighbours of atoms
!  neighno         = atom number of each neighbour
!  xneigh          = x component of vector to neighbour of atoms
!  yneigh          = y component of vector to neighbour of atoms
!  zneigh          = z component of vector to neighbour of atoms
!  nfreeatom       = pointer from atom to free atom to move
!  d1              = array of first derivatives w.r.t. interatomic
!                    distances
!  ltorderv        = if true then include torsional derivatives
!  nREBOatomRptr   = pointer from atoms to position in REBO set
!
!   5/02 Created
!   6/02 Strains added
!   6/02 Virial added
!   6/02 Modified to allow for torsional terms
!   7/02 Indexing further modified for torsions
!   8/02 Freezing added
!  12/03 Torsional derivatives made optional
!   6/07 nREBOatomRptr added as input
!   4/07 mneigh added as a distinct number from maxneigh
!  11/09 Region derivatives added
!   4/12 Explicit virial calculation removed as no longer needed
!   5/12 Atomic stresses added
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
  use brennerdata
  use configurations, only : nregionno
  use control,        only : latomicstress
  use current,        only : ndim, nrelat, nsft
  use derivatives
  use symmetry,       only : lstr
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)             :: i
  integer(i4), intent(in)             :: maxneigh
  integer(i4), intent(in)             :: mneigh
  integer(i4), intent(in)             :: nneigh(*)
  integer(i4), intent(in)             :: neighno(maxneigh,*)
  real(dp),    intent(in)             :: xneigh(maxneigh,*)
  real(dp),    intent(in)             :: yneigh(maxneigh,*)
  real(dp),    intent(in)             :: zneigh(maxneigh,*)
  integer(i4), intent(in)             :: nfreeatom(*)
  integer(i4), intent(in)             :: nREBOatomRptr(*)
  real(dp),    intent(in)             :: d1(*)
  logical,     intent(in)             :: ltorderv
!
!  Local variables
!
  integer(i4)                         :: ind
  integer(i4)                         :: io
  integer(i4)                         :: j
  integer(i4)                         :: k
  integer(i4)                         :: ko
  integer(i4)                         :: l
  integer(i4)                         :: lo
  integer(i4)                         :: nri
  integer(i4)                         :: nrj
  integer(i4)                         :: nj
  integer(i4)                         :: nk
  integer(i4)                         :: nl
  integer(i4)                         :: nregi
  integer(i4)                         :: nregk
  integer(i4)                         :: nregl
  real(dp)                            :: xd
  real(dp)                            :: yd
  real(dp)                            :: zd
  real(dp)                            :: xdiff
  real(dp)                            :: ydiff
  real(dp)                            :: zdiff
!
!  Loop over i -> neighbours
!
  ind = 0
  nri = nREBOatomRptr(i)
  do nk = 1,nneigh(nri)
    ind = ind + 1
    k = neighno(nk,nri)
    xd = xneigh(nk,nri)*d1(ind)
    yd = yneigh(nk,nri)*d1(ind)
    zd = zneigh(nk,nri)*d1(ind)
    io = nfreeatom(i)
    ko = nfreeatom(k)
    if (io.gt.0) then
      xdrv(i) = xdrv(i) - xd
      ydrv(i) = ydrv(i) - yd
      zdrv(i) = zdrv(i) - zd
    endif
    if (ko.gt.0) then
      xdrv(k) = xdrv(k) + xd
      ydrv(k) = ydrv(k) + yd
      zdrv(k) = zdrv(k) + zd
    endif
    nregi = nregionno(nsft+nrelat(i))
    nregk = nregionno(nsft+nrelat(k))
    if (nregi.ne.nregk) then
      xregdrv(nregi) = xregdrv(nregi) - xd
      yregdrv(nregi) = yregdrv(nregi) - yd
      zregdrv(nregi) = zregdrv(nregi) - zd
      xregdrv(nregk) = xregdrv(nregk) + xd
      yregdrv(nregk) = yregdrv(nregk) + yd
      zregdrv(nregk) = zregdrv(nregk) + zd
    endif
    if (lstr) then
      select case(ndim)
        case(1)
          rstrd(1) = rstrd(1) + xd*xneigh(nk,nri)
        case(2)
          rstrd(1) = rstrd(1) + xd*xneigh(nk,nri)
          rstrd(2) = rstrd(2) + yd*yneigh(nk,nri)
          rstrd(3) = rstrd(3) + xd*yneigh(nk,nri)
        case(3)
          rstrd(1) = rstrd(1) + xd*xneigh(nk,nri)
          rstrd(2) = rstrd(2) + yd*yneigh(nk,nri)
          rstrd(3) = rstrd(3) + zd*zneigh(nk,nri)
          rstrd(4) = rstrd(4) + yd*zneigh(nk,nri)
          rstrd(5) = rstrd(5) + xd*zneigh(nk,nri)
          rstrd(6) = rstrd(6) + xd*yneigh(nk,nri)
      end select
      if (latomicstress) then
        select case(ndim)
          case(1)
            atomicstress(1,i) = atomicstress(1,i) + 0.5_dp*xd*xneigh(nk,nri)
            atomicstress(1,k) = atomicstress(1,k) + 0.5_dp*xd*xneigh(nk,nri)
          case(2)
            atomicstress(1,i) = atomicstress(1,i) + 0.5_dp*xd*xneigh(nk,nri)
            atomicstress(2,i) = atomicstress(2,i) + 0.5_dp*yd*yneigh(nk,nri)
            atomicstress(3,i) = atomicstress(3,i) + 0.5_dp*xd*yneigh(nk,nri)
            atomicstress(1,k) = atomicstress(1,k) + 0.5_dp*xd*xneigh(nk,nri)
            atomicstress(2,k) = atomicstress(2,k) + 0.5_dp*yd*yneigh(nk,nri)
            atomicstress(3,k) = atomicstress(3,k) + 0.5_dp*xd*yneigh(nk,nri)
          case(3)
            atomicstress(1,i) = atomicstress(1,i) + 0.5_dp*xd*xneigh(nk,nri)
            atomicstress(2,i) = atomicstress(2,i) + 0.5_dp*yd*yneigh(nk,nri)
            atomicstress(3,i) = atomicstress(3,i) + 0.5_dp*zd*zneigh(nk,nri)
            atomicstress(4,i) = atomicstress(4,i) + 0.5_dp*yd*zneigh(nk,nri)
            atomicstress(5,i) = atomicstress(5,i) + 0.5_dp*xd*zneigh(nk,nri)
            atomicstress(6,i) = atomicstress(6,i) + 0.5_dp*xd*yneigh(nk,nri)
            atomicstress(1,k) = atomicstress(1,k) + 0.5_dp*xd*xneigh(nk,nri)
            atomicstress(2,k) = atomicstress(2,k) + 0.5_dp*yd*yneigh(nk,nri)
            atomicstress(3,k) = atomicstress(3,k) + 0.5_dp*zd*zneigh(nk,nri)
            atomicstress(4,k) = atomicstress(4,k) + 0.5_dp*yd*zneigh(nk,nri)
            atomicstress(5,k) = atomicstress(5,k) + 0.5_dp*xd*zneigh(nk,nri)
            atomicstress(6,k) = atomicstress(6,k) + 0.5_dp*xd*yneigh(nk,nri)
        end select
      endif
    endif
  enddo
!
!  Loop over neighbours -> neighbours
!
  do nk = 2,nneigh(nri)
    k = neighno(nk,nri)
    do nl = 1,nk - 1
      ind = nneigh(nri) + nk*(nk-1)/2 + nl
      if (d1(ind).ne.0.0_dp) then
        l = neighno(nl,nri)
        xdiff = (xneigh(nk,nri) - xneigh(nl,nri))
        ydiff = (yneigh(nk,nri) - yneigh(nl,nri))
        zdiff = (zneigh(nk,nri) - zneigh(nl,nri))
        xd = xdiff*d1(ind)
        yd = ydiff*d1(ind)
        zd = zdiff*d1(ind)
        lo = nfreeatom(l)
        ko = nfreeatom(k)
        if (lo.gt.0) then
          xdrv(l) = xdrv(l) - xd
          ydrv(l) = ydrv(l) - yd
          zdrv(l) = zdrv(l) - zd
        endif
        if (ko.gt.0) then
          xdrv(k) = xdrv(k) + xd
          ydrv(k) = ydrv(k) + yd
          zdrv(k) = zdrv(k) + zd
        endif
!
        nregl = nregionno(nsft+nrelat(l))
        nregk = nregionno(nsft+nrelat(k))
        if (nregl.ne.nregk) then
          xregdrv(nregl) = xregdrv(nregl) - xd
          yregdrv(nregl) = yregdrv(nregl) - yd
          zregdrv(nregl) = zregdrv(nregl) - zd
          xregdrv(nregk) = xregdrv(nregk) + xd
          yregdrv(nregk) = yregdrv(nregk) + yd
          zregdrv(nregk) = zregdrv(nregk) + zd
        endif
        if (lstr) then
          select case(ndim)
            case(1)
              rstrd(1) = rstrd(1) + xd*xdiff
            case(2)
              rstrd(1) = rstrd(1) + xd*xdiff
              rstrd(2) = rstrd(2) + yd*ydiff
              rstrd(3) = rstrd(3) + xd*ydiff
            case(3)
              rstrd(1) = rstrd(1) + xd*xdiff
              rstrd(2) = rstrd(2) + yd*ydiff
              rstrd(3) = rstrd(3) + zd*zdiff
              rstrd(4) = rstrd(4) + yd*zdiff
              rstrd(5) = rstrd(5) + xd*zdiff
              rstrd(6) = rstrd(6) + xd*ydiff
          end select
          if (latomicstress) then
            select case(ndim)
              case(1)
                atomicstress(1,l) = atomicstress(1,l) + 0.5_dp*xd*xdiff
                atomicstress(1,k) = atomicstress(1,k) + 0.5_dp*xd*xdiff
              case(2)
                atomicstress(1,l) = atomicstress(1,l) + 0.5_dp*xd*xdiff
                atomicstress(2,l) = atomicstress(2,l) + 0.5_dp*yd*ydiff
                atomicstress(3,l) = atomicstress(3,l) + 0.5_dp*xd*ydiff
                atomicstress(1,k) = atomicstress(1,k) + 0.5_dp*xd*xdiff
                atomicstress(2,k) = atomicstress(2,k) + 0.5_dp*yd*ydiff
                atomicstress(3,k) = atomicstress(3,k) + 0.5_dp*xd*ydiff
              case(3)
                atomicstress(1,l) = atomicstress(1,l) + 0.5_dp*xd*xdiff
                atomicstress(2,l) = atomicstress(2,l) + 0.5_dp*yd*ydiff
                atomicstress(3,l) = atomicstress(3,l) + 0.5_dp*zd*zdiff
                atomicstress(4,l) = atomicstress(4,l) + 0.5_dp*yd*zdiff
                atomicstress(5,l) = atomicstress(5,l) + 0.5_dp*xd*zdiff
                atomicstress(6,l) = atomicstress(6,l) + 0.5_dp*xd*ydiff
                atomicstress(1,k) = atomicstress(1,k) + 0.5_dp*xd*xdiff
                atomicstress(2,k) = atomicstress(2,k) + 0.5_dp*yd*ydiff
                atomicstress(3,k) = atomicstress(3,k) + 0.5_dp*zd*zdiff
                atomicstress(4,k) = atomicstress(4,k) + 0.5_dp*yd*zdiff
                atomicstress(5,k) = atomicstress(5,k) + 0.5_dp*xd*zdiff
                atomicstress(6,k) = atomicstress(6,k) + 0.5_dp*xd*ydiff
            end select
          endif
        endif
      endif
    enddo
  enddo
!
!  Loop over i -> neighbours of neighbours
!
  if (ltorderv) then
    do nj = 1,nneigh(nri)
      j = neighno(nj,nri)
      nrj = nREBOatomRptr(j)
      ind = nneigh(nri) + nneigh(nri)*(nneigh(nri) + 1)/2 + (nj - 1)*(mneigh+1)*mneigh
      do nl = 1,nneigh(nrj)
        ind = ind + 1
        if (d1(ind).ne.0.0_dp) then
          l = neighno(nl,nrj)
          xdiff = xneigh(nl,nrj) + xneigh(nj,nri)
          ydiff = yneigh(nl,nrj) + yneigh(nj,nri)
          zdiff = zneigh(nl,nrj) + zneigh(nj,nri)
          xd = xdiff*d1(ind)
          yd = ydiff*d1(ind)
          zd = zdiff*d1(ind)
          io = nfreeatom(i)
          lo = nfreeatom(l)
          if (io.gt.0) then
            xdrv(i) = xdrv(i) - xd
            ydrv(i) = ydrv(i) - yd
            zdrv(i) = zdrv(i) - zd
          endif
          if (lo.gt.0) then
            xdrv(l) = xdrv(l) + xd
            ydrv(l) = ydrv(l) + yd
            zdrv(l) = zdrv(l) + zd
          endif
!
          nregi = nregionno(nsft+nrelat(i))
          nregl = nregionno(nsft+nrelat(l))
          if (nregi.ne.nregl) then
            xregdrv(nregi) = xregdrv(nregi) - xd
            yregdrv(nregi) = yregdrv(nregi) - yd
            zregdrv(nregi) = zregdrv(nregi) - zd
            xregdrv(nregl) = xregdrv(nregl) + xd
            yregdrv(nregl) = yregdrv(nregl) + yd
            zregdrv(nregl) = zregdrv(nregl) + zd
          endif
!
          if (lstr) then
            select case(ndim)
              case(1)
                rstrd(1) = rstrd(1) + xd*xdiff
              case(2)
                rstrd(1) = rstrd(1) + xd*xdiff
                rstrd(2) = rstrd(2) + yd*ydiff
                rstrd(3) = rstrd(3) + xd*ydiff
              case(3)
                rstrd(1) = rstrd(1) + xd*xdiff
                rstrd(2) = rstrd(2) + yd*ydiff
                rstrd(3) = rstrd(3) + zd*zdiff
                rstrd(4) = rstrd(4) + yd*zdiff
                rstrd(5) = rstrd(5) + xd*zdiff
                rstrd(6) = rstrd(6) + xd*ydiff
            end select
            if (latomicstress) then
              select case(ndim)
                case(1)
                  atomicstress(1,i) = atomicstress(1,i) + 0.5_dp*xd*xdiff
                  atomicstress(1,l) = atomicstress(1,l) + 0.5_dp*xd*xdiff
                case(2)
                  atomicstress(1,i) = atomicstress(1,i) + 0.5_dp*xd*xdiff
                  atomicstress(2,i) = atomicstress(2,i) + 0.5_dp*yd*ydiff
                  atomicstress(3,i) = atomicstress(3,i) + 0.5_dp*xd*ydiff
                  atomicstress(1,l) = atomicstress(1,l) + 0.5_dp*xd*xdiff
                  atomicstress(2,l) = atomicstress(2,l) + 0.5_dp*yd*ydiff
                  atomicstress(3,l) = atomicstress(3,l) + 0.5_dp*xd*ydiff
                case(3)
                  atomicstress(1,i) = atomicstress(1,i) + 0.5_dp*xd*xdiff
                  atomicstress(2,i) = atomicstress(2,i) + 0.5_dp*yd*ydiff
                  atomicstress(3,i) = atomicstress(3,i) + 0.5_dp*zd*zdiff
                  atomicstress(4,i) = atomicstress(4,i) + 0.5_dp*yd*zdiff
                  atomicstress(5,i) = atomicstress(5,i) + 0.5_dp*xd*zdiff
                  atomicstress(6,i) = atomicstress(6,i) + 0.5_dp*xd*ydiff
                  atomicstress(1,l) = atomicstress(1,l) + 0.5_dp*xd*xdiff
                  atomicstress(2,l) = atomicstress(2,l) + 0.5_dp*yd*ydiff
                  atomicstress(3,l) = atomicstress(3,l) + 0.5_dp*zd*zdiff
                  atomicstress(4,l) = atomicstress(4,l) + 0.5_dp*yd*zdiff
                  atomicstress(5,l) = atomicstress(5,l) + 0.5_dp*xd*zdiff
                  atomicstress(6,l) = atomicstress(6,l) + 0.5_dp*xd*ydiff
              end select
            endif
          endif
        endif
      enddo
    enddo
!
!  Loop over neighbours -> neighbours of neighbours
!
    do nj = 1,nneigh(nri)
      j = neighno(nj,nri)
      nrj = nREBOatomRptr(j)
      do nk = 1,nneigh(nri)
        k = neighno(nk,nri)
        ind = nneigh(nri) + nneigh(nri)*(nneigh(nri) + 1)/2 + (nj - 1)*(mneigh+1)*mneigh + nk*mneigh
        do nl = 1,nneigh(nrj)
          ind = ind + 1
          if (d1(ind).ne.0.0_dp) then
            l = neighno(nl,nrj)
            xdiff = xneigh(nl,nrj) + xneigh(nj,nri) - xneigh(nk,nri)
            ydiff = yneigh(nl,nrj) + yneigh(nj,nri) - yneigh(nk,nri)
            zdiff = zneigh(nl,nrj) + zneigh(nj,nri) - zneigh(nk,nri)
            xd = xdiff*d1(ind)
            yd = ydiff*d1(ind)
            zd = zdiff*d1(ind)
            ko = nfreeatom(k)
            lo = nfreeatom(l)
            if (ko.gt.0) then
              xdrv(k) = xdrv(k) - xd
              ydrv(k) = ydrv(k) - yd
              zdrv(k) = zdrv(k) - zd
            endif
            if (lo.gt.0) then
              xdrv(l) = xdrv(l) + xd
              ydrv(l) = ydrv(l) + yd
              zdrv(l) = zdrv(l) + zd
            endif
!
            nregk = nregionno(nsft+nrelat(k))
            nregl = nregionno(nsft+nrelat(l))
            if (nregk.ne.nregl) then
              xregdrv(nregk) = xregdrv(nregk) - xd
              yregdrv(nregk) = yregdrv(nregk) - yd
              zregdrv(nregk) = zregdrv(nregk) - zd
              xregdrv(nregl) = xregdrv(nregl) + xd
              yregdrv(nregl) = yregdrv(nregl) + yd
              zregdrv(nregl) = zregdrv(nregl) + zd
            endif
!
            if (lstr) then
              select case(ndim)
                case(1)
                  rstrd(1) = rstrd(1) + xd*xdiff
                case(2)
                  rstrd(1) = rstrd(1) + xd*xdiff
                  rstrd(2) = rstrd(2) + yd*ydiff
                  rstrd(3) = rstrd(3) + xd*ydiff
                case(3)
                  rstrd(1) = rstrd(1) + xd*xdiff
                  rstrd(2) = rstrd(2) + yd*ydiff
                  rstrd(3) = rstrd(3) + zd*zdiff
                  rstrd(4) = rstrd(4) + yd*zdiff
                  rstrd(5) = rstrd(5) + xd*zdiff
                  rstrd(6) = rstrd(6) + xd*ydiff
              end select
              if (latomicstress) then
                select case(ndim)
                  case(1)
                    atomicstress(1,k) = atomicstress(1,k) + 0.5_dp*xd*xdiff
                    atomicstress(1,l) = atomicstress(1,l) + 0.5_dp*xd*xdiff
                  case(2)
                    atomicstress(1,k) = atomicstress(1,k) + 0.5_dp*xd*xdiff
                    atomicstress(2,k) = atomicstress(2,k) + 0.5_dp*yd*ydiff
                    atomicstress(3,k) = atomicstress(3,k) + 0.5_dp*xd*ydiff
                    atomicstress(1,l) = atomicstress(1,l) + 0.5_dp*xd*xdiff
                    atomicstress(2,l) = atomicstress(2,l) + 0.5_dp*yd*ydiff
                    atomicstress(3,l) = atomicstress(3,l) + 0.5_dp*xd*ydiff
                  case(3)
                    atomicstress(1,k) = atomicstress(1,k) + 0.5_dp*xd*xdiff
                    atomicstress(2,k) = atomicstress(2,k) + 0.5_dp*yd*ydiff
                    atomicstress(3,k) = atomicstress(3,k) + 0.5_dp*zd*zdiff
                    atomicstress(4,k) = atomicstress(4,k) + 0.5_dp*yd*zdiff
                    atomicstress(5,k) = atomicstress(5,k) + 0.5_dp*xd*zdiff
                    atomicstress(6,k) = atomicstress(6,k) + 0.5_dp*xd*ydiff
                    atomicstress(1,l) = atomicstress(1,l) + 0.5_dp*xd*xdiff
                    atomicstress(2,l) = atomicstress(2,l) + 0.5_dp*yd*ydiff
                    atomicstress(3,l) = atomicstress(3,l) + 0.5_dp*zd*zdiff
                    atomicstress(4,l) = atomicstress(4,l) + 0.5_dp*yd*zdiff
                    atomicstress(5,l) = atomicstress(5,l) + 0.5_dp*xd*zdiff
                    atomicstress(6,l) = atomicstress(6,l) + 0.5_dp*xd*ydiff
                end select
              endif
            endif
          endif
        enddo
      enddo
    enddo
  endif
!
  return
  end
!
  subroutine d1addc(i,maxneigh,mneigh,nneigh,neighno,nfreeatom,nREBOatomRptr,d1,d1s,ltorderv)
!
!  Utility to add the pairwise derivatives for a group of atoms
!  on to the global Cartesian first derivative arrays. This version
!  is as per d1add except that Cartesian derivative components are
!  passed in rather than computed here.
!
!  On entry : 
!
!  i               = atom number of i to whom others are connected
!  maxneigh        = left-hand dimension of 2-D arrays
!  mneigh          = value of maxneigh actually used for building index numbers
!  nneigh          = no. of neighbours of atoms
!  neighno         = atom number of each neighbour
!  nfreeatom       = pointer from atom to free atom to move
!  d1              = array of first derivatives w.r.t. Cartesian components
!  d1s             = array of first derivatives w.r.t. strains
!  ltorderv        = if true then include torsional derivatives
!  nREBOatomRptr   = pointer from atoms to position in REBO set
!
!  10/10 Created from d1add
!   5/11 Check on d1 being > 0 reinstated using sum of components of d1
!   4/12 Explicit virial calculation removed as no longer needed
!   5/12 Atomic stresses added
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
  use brennerdata
  use configurations, only : nregionno
  use control,        only : latomicstress
  use current,        only : ndim, nrelat, nsft
  use derivatives
  use symmetry,       only : lstr
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)             :: i
  integer(i4), intent(in)             :: maxneigh
  integer(i4), intent(in)             :: mneigh
  integer(i4), intent(in)             :: nneigh(*)
  integer(i4), intent(in)             :: neighno(maxneigh,*)
  integer(i4), intent(in)             :: nfreeatom(*)
  integer(i4), intent(in)             :: nREBOatomRptr(*)
  real(dp),    intent(in)             :: d1(3,*)
  real(dp),    intent(in)             :: d1s(6,*)
  logical,     intent(in)             :: ltorderv
!
!  Local variables
!
  integer(i4)                         :: ind
  integer(i4)                         :: io
  integer(i4)                         :: j
  integer(i4)                         :: k
  integer(i4)                         :: ko
  integer(i4)                         :: l
  integer(i4)                         :: lo
  integer(i4)                         :: nri
  integer(i4)                         :: nrj
  integer(i4)                         :: nj
  integer(i4)                         :: nk
  integer(i4)                         :: nl
  integer(i4)                         :: nregi
  integer(i4)                         :: nregk
  integer(i4)                         :: nregl
  real(dp)                            :: d1sum
  real(dp)                            :: xd
  real(dp)                            :: yd
  real(dp)                            :: zd
!
!  Loop over i -> neighbours
!
  ind = 0
  nri = nREBOatomRptr(i)
  do nk = 1,nneigh(nri)
    ind = ind + 1
    k = neighno(nk,nri)
    xd = d1(1,ind)
    yd = d1(2,ind)
    zd = d1(3,ind)
    io = nfreeatom(i)
    ko = nfreeatom(k)
    if (io.gt.0) then
      xdrv(i) = xdrv(i) - xd
      ydrv(i) = ydrv(i) - yd
      zdrv(i) = zdrv(i) - zd
    endif
    if (ko.gt.0) then
      xdrv(k) = xdrv(k) + xd
      ydrv(k) = ydrv(k) + yd
      zdrv(k) = zdrv(k) + zd
    endif
    nregi = nregionno(nsft+nrelat(i))
    nregk = nregionno(nsft+nrelat(k))
    if (nregi.ne.nregk) then
      xregdrv(nregi) = xregdrv(nregi) - xd
      yregdrv(nregi) = yregdrv(nregi) - yd
      zregdrv(nregi) = zregdrv(nregi) - zd
      xregdrv(nregk) = xregdrv(nregk) + xd
      yregdrv(nregk) = yregdrv(nregk) + yd
      zregdrv(nregk) = zregdrv(nregk) + zd
    endif
    if (lstr) then
      select case(ndim)
        case(1)
          rstrd(1) = rstrd(1) + d1s(1,ind)
        case(2)
          rstrd(1) = rstrd(1) + d1s(1,ind)
          rstrd(2) = rstrd(2) + d1s(2,ind)
          rstrd(3) = rstrd(3) + d1s(3,ind)
        case(3)
          rstrd(1) = rstrd(1) + d1s(1,ind)
          rstrd(2) = rstrd(2) + d1s(2,ind)
          rstrd(3) = rstrd(3) + d1s(3,ind)
          rstrd(4) = rstrd(4) + d1s(4,ind)
          rstrd(5) = rstrd(5) + d1s(5,ind)
          rstrd(6) = rstrd(6) + d1s(6,ind)
      end select
      if (latomicstress) then
        select case(ndim)
          case(1)
            atomicstress(1,i) = atomicstress(1,i) + 0.5_dp*d1s(1,ind)
            atomicstress(1,k) = atomicstress(1,k) + 0.5_dp*d1s(1,ind)
          case(2)
            atomicstress(1,i) = atomicstress(1,i) + 0.5_dp*d1s(1,ind)
            atomicstress(2,i) = atomicstress(2,i) + 0.5_dp*d1s(2,ind)
            atomicstress(3,i) = atomicstress(3,i) + 0.5_dp*d1s(3,ind)
            atomicstress(1,k) = atomicstress(1,k) + 0.5_dp*d1s(1,ind)
            atomicstress(2,k) = atomicstress(2,k) + 0.5_dp*d1s(2,ind)
            atomicstress(3,k) = atomicstress(3,k) + 0.5_dp*d1s(3,ind)
          case(3)
            atomicstress(1,i) = atomicstress(1,i) + 0.5_dp*d1s(1,ind)
            atomicstress(2,i) = atomicstress(2,i) + 0.5_dp*d1s(2,ind)
            atomicstress(3,i) = atomicstress(3,i) + 0.5_dp*d1s(3,ind)
            atomicstress(4,i) = atomicstress(4,i) + 0.5_dp*d1s(4,ind)
            atomicstress(5,i) = atomicstress(5,i) + 0.5_dp*d1s(5,ind)
            atomicstress(6,i) = atomicstress(6,i) + 0.5_dp*d1s(6,ind)
            atomicstress(1,k) = atomicstress(1,k) + 0.5_dp*d1s(1,ind)
            atomicstress(2,k) = atomicstress(2,k) + 0.5_dp*d1s(2,ind)
            atomicstress(3,k) = atomicstress(3,k) + 0.5_dp*d1s(3,ind)
            atomicstress(4,k) = atomicstress(4,k) + 0.5_dp*d1s(4,ind)
            atomicstress(5,k) = atomicstress(5,k) + 0.5_dp*d1s(5,ind)
            atomicstress(6,k) = atomicstress(6,k) + 0.5_dp*d1s(6,ind)
        end select
      endif
    endif
  enddo
!
!  Loop over neighbours -> neighbours
!
  do nk = 2,nneigh(nri)
    k = neighno(nk,nri)
    do nl = 1,nk - 1
      ind = nneigh(nri) + nk*(nk-1)/2 + nl
      d1sum = abs(d1(1,ind)) + abs(d1(2,ind)) + abs(d1(3,ind))
      if (d1sum.ne.0.0_dp) then
        l = neighno(nl,nri)
        xd = d1(1,ind)
        yd = d1(2,ind)
        zd = d1(3,ind)
        lo = nfreeatom(l)
        ko = nfreeatom(k)
        if (lo.gt.0) then
          xdrv(l) = xdrv(l) - xd
          ydrv(l) = ydrv(l) - yd
          zdrv(l) = zdrv(l) - zd
        endif
        if (ko.gt.0) then
          xdrv(k) = xdrv(k) + xd
          ydrv(k) = ydrv(k) + yd
          zdrv(k) = zdrv(k) + zd
        endif
!
        nregl = nregionno(nsft+nrelat(l))
        nregk = nregionno(nsft+nrelat(k))
        if (nregl.ne.nregk) then
          xregdrv(nregl) = xregdrv(nregl) - xd
          yregdrv(nregl) = yregdrv(nregl) - yd
          zregdrv(nregl) = zregdrv(nregl) - zd
          xregdrv(nregk) = xregdrv(nregk) + xd
          yregdrv(nregk) = yregdrv(nregk) + yd
          zregdrv(nregk) = zregdrv(nregk) + zd
        endif
        if (lstr) then
          select case(ndim)
            case(1)
              rstrd(1) = rstrd(1) + d1s(1,ind)
            case(2)
              rstrd(1) = rstrd(1) + d1s(1,ind)
              rstrd(2) = rstrd(2) + d1s(2,ind)
              rstrd(3) = rstrd(3) + d1s(3,ind)
            case(3)
              rstrd(1) = rstrd(1) + d1s(1,ind)
              rstrd(2) = rstrd(2) + d1s(2,ind)
              rstrd(3) = rstrd(3) + d1s(3,ind)
              rstrd(4) = rstrd(4) + d1s(4,ind)
              rstrd(5) = rstrd(5) + d1s(5,ind)
              rstrd(6) = rstrd(6) + d1s(6,ind)
          end select
          if (latomicstress) then
            select case(ndim)
              case(1)
                atomicstress(1,l) = atomicstress(1,l) + 0.5_dp*d1s(1,ind)
                atomicstress(1,k) = atomicstress(1,k) + 0.5_dp*d1s(1,ind)
              case(2)
                atomicstress(1,l) = atomicstress(1,l) + 0.5_dp*d1s(1,ind)
                atomicstress(2,l) = atomicstress(2,l) + 0.5_dp*d1s(2,ind)
                atomicstress(3,l) = atomicstress(3,l) + 0.5_dp*d1s(3,ind)
                atomicstress(1,k) = atomicstress(1,k) + 0.5_dp*d1s(1,ind)
                atomicstress(2,k) = atomicstress(2,k) + 0.5_dp*d1s(2,ind)
                atomicstress(3,k) = atomicstress(3,k) + 0.5_dp*d1s(3,ind)
              case(3)
                atomicstress(1,l) = atomicstress(1,l) + 0.5_dp*d1s(1,ind)
                atomicstress(2,l) = atomicstress(2,l) + 0.5_dp*d1s(2,ind)
                atomicstress(3,l) = atomicstress(3,l) + 0.5_dp*d1s(3,ind)
                atomicstress(4,l) = atomicstress(4,l) + 0.5_dp*d1s(4,ind)
                atomicstress(5,l) = atomicstress(5,l) + 0.5_dp*d1s(5,ind)
                atomicstress(6,l) = atomicstress(6,l) + 0.5_dp*d1s(6,ind)
                atomicstress(1,k) = atomicstress(1,k) + 0.5_dp*d1s(1,ind)
                atomicstress(2,k) = atomicstress(2,k) + 0.5_dp*d1s(2,ind)
                atomicstress(3,k) = atomicstress(3,k) + 0.5_dp*d1s(3,ind)
                atomicstress(4,k) = atomicstress(4,k) + 0.5_dp*d1s(4,ind)
                atomicstress(5,k) = atomicstress(5,k) + 0.5_dp*d1s(5,ind)
                atomicstress(6,k) = atomicstress(6,k) + 0.5_dp*d1s(6,ind)
            end select
          endif
        endif
      endif
    enddo
  enddo
!
!  Loop over i -> neighbours of neighbours
!
  if (ltorderv) then
    do nj = 1,nneigh(nri)
      j = neighno(nj,nri)
      nrj = nREBOatomRptr(j)
      ind = nneigh(nri) + nneigh(nri)*(nneigh(nri) + 1)/2 + (nj - 1)*(mneigh+1)*mneigh
      do nl = 1,nneigh(nrj)
        ind = ind + 1
        d1sum = abs(d1(1,ind)) + abs(d1(2,ind)) + abs(d1(3,ind))
        if (d1sum.ne.0.0_dp) then
          l = neighno(nl,nrj)
          xd = d1(1,ind)
          yd = d1(2,ind)
          zd = d1(3,ind)
          io = nfreeatom(i)
          lo = nfreeatom(l)
          if (io.gt.0) then
            xdrv(i) = xdrv(i) - xd
            ydrv(i) = ydrv(i) - yd
            zdrv(i) = zdrv(i) - zd
          endif
          if (lo.gt.0) then
            xdrv(l) = xdrv(l) + xd
            ydrv(l) = ydrv(l) + yd
            zdrv(l) = zdrv(l) + zd
          endif
!
          nregi = nregionno(nsft+nrelat(i))
          nregl = nregionno(nsft+nrelat(l))
          if (nregi.ne.nregl) then
            xregdrv(nregi) = xregdrv(nregi) - xd
            yregdrv(nregi) = yregdrv(nregi) - yd
            zregdrv(nregi) = zregdrv(nregi) - zd
            xregdrv(nregl) = xregdrv(nregl) + xd
            yregdrv(nregl) = yregdrv(nregl) + yd
            zregdrv(nregl) = zregdrv(nregl) + zd
          endif
!
          if (lstr) then
            select case(ndim)
              case(1)
                rstrd(1) = rstrd(1) + d1s(1,ind)
              case(2)
                rstrd(1) = rstrd(1) + d1s(1,ind)
                rstrd(2) = rstrd(2) + d1s(2,ind)
                rstrd(3) = rstrd(3) + d1s(3,ind)
              case(3)
                rstrd(1) = rstrd(1) + d1s(1,ind)
                rstrd(2) = rstrd(2) + d1s(2,ind)
                rstrd(3) = rstrd(3) + d1s(3,ind)
                rstrd(4) = rstrd(4) + d1s(4,ind)
                rstrd(5) = rstrd(5) + d1s(5,ind)
                rstrd(6) = rstrd(6) + d1s(6,ind)
            end select
            if (latomicstress) then
              select case(ndim)
                case(1)
                  atomicstress(1,i) = atomicstress(1,i) + 0.5_dp*d1s(1,ind)
                  atomicstress(1,l) = atomicstress(1,l) + 0.5_dp*d1s(1,ind)
                case(2)
                  atomicstress(1,i) = atomicstress(1,i) + 0.5_dp*d1s(1,ind)
                  atomicstress(2,i) = atomicstress(2,i) + 0.5_dp*d1s(2,ind)
                  atomicstress(3,i) = atomicstress(3,i) + 0.5_dp*d1s(3,ind)
                  atomicstress(1,l) = atomicstress(1,l) + 0.5_dp*d1s(1,ind)
                  atomicstress(2,l) = atomicstress(2,l) + 0.5_dp*d1s(2,ind)
                  atomicstress(3,l) = atomicstress(3,l) + 0.5_dp*d1s(3,ind)
                case(3)
                  atomicstress(1,i) = atomicstress(1,i) + 0.5_dp*d1s(1,ind)
                  atomicstress(2,i) = atomicstress(2,i) + 0.5_dp*d1s(2,ind)
                  atomicstress(3,i) = atomicstress(3,i) + 0.5_dp*d1s(3,ind)
                  atomicstress(4,i) = atomicstress(4,i) + 0.5_dp*d1s(4,ind)
                  atomicstress(5,i) = atomicstress(5,i) + 0.5_dp*d1s(5,ind)
                  atomicstress(6,i) = atomicstress(6,i) + 0.5_dp*d1s(6,ind)
                  atomicstress(1,l) = atomicstress(1,l) + 0.5_dp*d1s(1,ind)
                  atomicstress(2,l) = atomicstress(2,l) + 0.5_dp*d1s(2,ind)
                  atomicstress(3,l) = atomicstress(3,l) + 0.5_dp*d1s(3,ind)
                  atomicstress(4,l) = atomicstress(4,l) + 0.5_dp*d1s(4,ind)
                  atomicstress(5,l) = atomicstress(5,l) + 0.5_dp*d1s(5,ind)
                  atomicstress(6,l) = atomicstress(6,l) + 0.5_dp*d1s(6,ind)
              end select
            endif
          endif
        endif
      enddo
    enddo
!
!  Loop over neighbours -> neighbours of neighbours
!
    do nj = 1,nneigh(nri)
      j = neighno(nj,nri)
      nrj = nREBOatomRptr(j)
      do nk = 1,nneigh(nri)
        k = neighno(nk,nri)
        ind = nneigh(nri) + nneigh(nri)*(nneigh(nri) + 1)/2 + (nj - 1)*(mneigh+1)*mneigh + nk*mneigh
        do nl = 1,nneigh(nrj)
          ind = ind + 1
          d1sum = abs(d1(1,ind)) + abs(d1(2,ind)) + abs(d1(3,ind))
          if (d1sum.ne.0.0_dp) then
            l = neighno(nl,nrj)
            xd = d1(1,ind)
            yd = d1(2,ind)
            zd = d1(3,ind)
            ko = nfreeatom(k)
            lo = nfreeatom(l)
            if (ko.gt.0) then
              xdrv(k) = xdrv(k) - xd
              ydrv(k) = ydrv(k) - yd
              zdrv(k) = zdrv(k) - zd
            endif
            if (lo.gt.0) then
              xdrv(l) = xdrv(l) + xd
              ydrv(l) = ydrv(l) + yd
              zdrv(l) = zdrv(l) + zd
            endif
!
            nregk = nregionno(nsft+nrelat(k))
            nregl = nregionno(nsft+nrelat(l))
            if (nregk.ne.nregl) then
              xregdrv(nregk) = xregdrv(nregk) - xd
              yregdrv(nregk) = yregdrv(nregk) - yd
              zregdrv(nregk) = zregdrv(nregk) - zd
              xregdrv(nregl) = xregdrv(nregl) + xd
              yregdrv(nregl) = yregdrv(nregl) + yd
              zregdrv(nregl) = zregdrv(nregl) + zd
            endif
!
            if (lstr) then
              select case(ndim)
                case(1)
                  rstrd(1) = rstrd(1) + d1s(1,ind)
                case(2)
                  rstrd(1) = rstrd(1) + d1s(1,ind)
                  rstrd(2) = rstrd(2) + d1s(2,ind)
                  rstrd(3) = rstrd(3) + d1s(3,ind)
                case(3)
                  rstrd(1) = rstrd(1) + d1s(1,ind)
                  rstrd(2) = rstrd(2) + d1s(2,ind)
                  rstrd(3) = rstrd(3) + d1s(3,ind)
                  rstrd(4) = rstrd(4) + d1s(4,ind)
                  rstrd(5) = rstrd(5) + d1s(5,ind)
                  rstrd(6) = rstrd(6) + d1s(6,ind)
              end select
              if (latomicstress) then
                select case(ndim)
                  case(1)
                    atomicstress(1,k) = atomicstress(1,k) + 0.5_dp*d1s(1,ind)
                    atomicstress(1,l) = atomicstress(1,l) + 0.5_dp*d1s(1,ind)
                  case(2)
                    atomicstress(1,k) = atomicstress(1,k) + 0.5_dp*d1s(1,ind)
                    atomicstress(2,k) = atomicstress(2,k) + 0.5_dp*d1s(2,ind)
                    atomicstress(3,k) = atomicstress(3,k) + 0.5_dp*d1s(3,ind)
                    atomicstress(1,l) = atomicstress(1,l) + 0.5_dp*d1s(1,ind)
                    atomicstress(2,l) = atomicstress(2,l) + 0.5_dp*d1s(2,ind)
                    atomicstress(3,l) = atomicstress(3,l) + 0.5_dp*d1s(3,ind)
                  case(3)
                    atomicstress(1,k) = atomicstress(1,k) + 0.5_dp*d1s(1,ind)
                    atomicstress(2,k) = atomicstress(2,k) + 0.5_dp*d1s(2,ind)
                    atomicstress(3,k) = atomicstress(3,k) + 0.5_dp*d1s(3,ind)
                    atomicstress(4,k) = atomicstress(4,k) + 0.5_dp*d1s(4,ind)
                    atomicstress(5,k) = atomicstress(5,k) + 0.5_dp*d1s(5,ind)
                    atomicstress(6,k) = atomicstress(6,k) + 0.5_dp*d1s(6,ind)
                    atomicstress(1,l) = atomicstress(1,l) + 0.5_dp*d1s(1,ind)
                    atomicstress(2,l) = atomicstress(2,l) + 0.5_dp*d1s(2,ind)
                    atomicstress(3,l) = atomicstress(3,l) + 0.5_dp*d1s(3,ind)
                    atomicstress(4,l) = atomicstress(4,l) + 0.5_dp*d1s(4,ind)
                    atomicstress(5,l) = atomicstress(5,l) + 0.5_dp*d1s(5,ind)
                    atomicstress(6,l) = atomicstress(6,l) + 0.5_dp*d1s(6,ind)
                end select
              endif
            endif
          endif
        enddo
      enddo
    enddo
  endif
!
  return
  end
!
  subroutine d1adds(i,maxneigh,mneigh,nneigh,neighno,xneigh,yneigh,zneigh,nfreeatom,nREBOatomRptr,nauatom,neqvatom,d1,ltorderv)
!
!  Utility to add the pairwise derivatives for a group of atoms
!  on to the global Cartesian first derivative arrays.
!
!  Symmetry adapted version of d1add
!
!  On entry : 
!
!  i               = atom number of i to whom others are connected
!  maxneigh        = left-hand dimension of 2-D arrays
!  mneigh          = value of maxneigh actually used for building index numbers
!  nneigh          = no. of neighbours of atoms
!  neighno         = atom number of each neighbour
!  xneigh          = x component of vector to neighbour of atoms
!  yneigh          = y component of vector to neighbour of atoms
!  zneigh          = z component of vector to neighbour of atoms
!  nfreeatom       = pointer from atom to free atom to move
!  nREBOatomRptr   = pointer from atoms to position in REBO set
!  nauatom         = pointer from full cell atom to asymmetric unit
!  neqvatom        = number of equivalent positions for asymmetric unit atom
!  d1              = array of first derivatives w.r.t. interatomic
!                    distances
!  ltorderv        = if true then include torsional derivatives
!
!   9/02 Created from d1add
!  12/03 Torsional derivatives made optional
!   6/07 nREBOatomRptr added as input
!   4/08 mneigh added as a distinct number from maxneigh
!  11/09 Region derivatives added
!   3/10 Bug in handling of region derivatives when atom number is 0 fixed
!   4/12 Explicit virial calculation removed as no longer needed
!   5/12 Atomic stresses added
!   5/12 Atomic stresses removed for routines involving symmetry
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
  use brennerdata
  use configurations, only : nregionno
  use control,        only : latomicstress
  use current,        only : ndim, nsft
  use derivatives
  use symmetry,       only : lstr
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)             :: i
  integer(i4), intent(in)             :: maxneigh
  integer(i4), intent(in)             :: mneigh
  integer(i4), intent(in)             :: nneigh(*)
  integer(i4), intent(in)             :: neighno(maxneigh,*)
  real(dp),    intent(in)             :: xneigh(maxneigh,*)
  real(dp),    intent(in)             :: yneigh(maxneigh,*)
  real(dp),    intent(in)             :: zneigh(maxneigh,*)
  integer(i4), intent(in)             :: nfreeatom(*)
  integer(i4), intent(in)             :: nREBOatomRptr(*)
  integer(i4), intent(in)             :: nauatom(*)
  integer(i4), intent(in)             :: neqvatom(*)
  real(dp),    intent(in)             :: d1(*)
  logical,     intent(in)             :: ltorderv
!
!  Local variables
!
  integer(i4)                         :: ind
  integer(i4)                         :: ia
  integer(i4)                         :: io
  integer(i4)                         :: j
  integer(i4)                         :: k
  integer(i4)                         :: ka
  integer(i4)                         :: ko
  integer(i4)                         :: l
  integer(i4)                         :: la
  integer(i4)                         :: lo
  integer(i4)                         :: nj
  integer(i4)                         :: nk
  integer(i4)                         :: nl
  integer(i4)                         :: nri
  integer(i4)                         :: nrj
  integer(i4)                         :: nregi
  integer(i4)                         :: nregk
  integer(i4)                         :: nregl
  real(dp)                            :: xd
  real(dp)                            :: yd
  real(dp)                            :: zd
  real(dp)                            :: xdiff
  real(dp)                            :: ydiff
  real(dp)                            :: zdiff
!
!  Loop over i -> neighbours
!
  ind = 0
  nri = nREBOatomRptr(i)
  do nk = 1,nneigh(nri)
    ind = ind + 1
    k = neighno(nk,nri)
    xd = xneigh(nk,nri)*d1(ind)
    yd = yneigh(nk,nri)*d1(ind)
    zd = zneigh(nk,nri)*d1(ind)
    io = nfreeatom(i)
    ko = nfreeatom(k)
    ia = nauatom(i)
    ka = nauatom(k)
    if (io.gt.0.and.ia.gt.0) then
      xdrv(ia) = xdrv(ia) - xd*dble(neqvatom(ia))
      ydrv(ia) = ydrv(ia) - yd*dble(neqvatom(ia))
      zdrv(ia) = zdrv(ia) - zd*dble(neqvatom(ia))
      nregi = nregionno(nsft+ia)
      xregdrv(nregi) = xregdrv(nregi) - xd*dble(neqvatom(ia))
      yregdrv(nregi) = yregdrv(nregi) - yd*dble(neqvatom(ia))
      zregdrv(nregi) = zregdrv(nregi) - zd*dble(neqvatom(ia))
    endif
    if (ko.gt.0.and.ka.gt.0) then
      xdrv(ka) = xdrv(ka) + xd*dble(neqvatom(ka))
      ydrv(ka) = ydrv(ka) + yd*dble(neqvatom(ka))
      zdrv(ka) = zdrv(ka) + zd*dble(neqvatom(ka))
      nregk = nregionno(nsft+ka)
      xregdrv(nregk) = xregdrv(nregk) + xd*dble(neqvatom(ka))
      yregdrv(nregk) = yregdrv(nregk) + yd*dble(neqvatom(ka))
      zregdrv(nregk) = zregdrv(nregk) + zd*dble(neqvatom(ka))
    endif
!
    if (lstr) then
      select case(ndim)
        case(1)
          rstrd(1) = rstrd(1) + xd*xneigh(nk,nri)
        case(2)
          rstrd(1) = rstrd(1) + xd*xneigh(nk,nri)
          rstrd(2) = rstrd(2) + yd*yneigh(nk,nri)
          rstrd(3) = rstrd(3) + xd*yneigh(nk,nri)
        case(3)
          rstrd(1) = rstrd(1) + xd*xneigh(nk,nri)
          rstrd(2) = rstrd(2) + yd*yneigh(nk,nri)
          rstrd(3) = rstrd(3) + zd*zneigh(nk,nri)
          rstrd(4) = rstrd(4) + yd*zneigh(nk,nri)
          rstrd(5) = rstrd(5) + xd*zneigh(nk,nri)
          rstrd(6) = rstrd(6) + xd*yneigh(nk,nri)
      end select
    endif
  enddo
!
!  Loop over neighbours -> neighbours
!
  do nk = 2,nneigh(nri)
    k = neighno(nk,nri)
    do nl = 1,nk - 1
      ind = nneigh(nri) + nk*(nk-1)/2 + nl
      if (d1(ind).ne.0.0_dp) then
        l = neighno(nl,nri)
        xdiff = (xneigh(nk,nri) - xneigh(nl,nri))
        ydiff = (yneigh(nk,nri) - yneigh(nl,nri))
        zdiff = (zneigh(nk,nri) - zneigh(nl,nri))
        xd = xdiff*d1(ind)
        yd = ydiff*d1(ind)
        zd = zdiff*d1(ind)
        lo = nfreeatom(l)
        ko = nfreeatom(k)
        la = nauatom(l)
        ka = nauatom(k)
        if (lo.gt.0.and.la.gt.0) then
          xdrv(la) = xdrv(la) - xd*dble(neqvatom(la))
          ydrv(la) = ydrv(la) - yd*dble(neqvatom(la))
          zdrv(la) = zdrv(la) - zd*dble(neqvatom(la))
          nregl = nregionno(nsft+la)
          xregdrv(nregl) = xregdrv(nregl) - xd*dble(neqvatom(la))
          yregdrv(nregl) = yregdrv(nregl) - yd*dble(neqvatom(la))
          zregdrv(nregl) = zregdrv(nregl) - zd*dble(neqvatom(la))
        endif
        if (ko.gt.0.and.ka.gt.0) then
          xdrv(ka) = xdrv(ka) + xd*dble(neqvatom(ka))
          ydrv(ka) = ydrv(ka) + yd*dble(neqvatom(ka))
          zdrv(ka) = zdrv(ka) + zd*dble(neqvatom(ka))
          nregk = nregionno(nsft+ka)
          xregdrv(nregk) = xregdrv(nregk) + xd*dble(neqvatom(ka))
          yregdrv(nregk) = yregdrv(nregk) + yd*dble(neqvatom(ka))
          zregdrv(nregk) = zregdrv(nregk) + zd*dble(neqvatom(ka))
        endif
!
        if (lstr) then
          select case(ndim)
            case(1)
              rstrd(1) = rstrd(1) + xd*xdiff
            case(2)
              rstrd(1) = rstrd(1) + xd*xdiff
              rstrd(2) = rstrd(2) + yd*ydiff
              rstrd(3) = rstrd(3) + xd*ydiff
            case(3)
              rstrd(1) = rstrd(1) + xd*xdiff
              rstrd(2) = rstrd(2) + yd*ydiff
              rstrd(3) = rstrd(3) + zd*zdiff
              rstrd(4) = rstrd(4) + yd*zdiff
              rstrd(5) = rstrd(5) + xd*zdiff
              rstrd(6) = rstrd(6) + xd*ydiff
          end select
        endif
      endif
    enddo
  enddo
!
!  Loop over i -> neighbours of neighbours
!
  if (ltorderv) then
    do nj = 1,nneigh(nri)
      j = neighno(nj,nri)
      nrj = nREBOatomRptr(j)
      ind = nneigh(nri) + nneigh(nri)*(nneigh(nri) + 1)/2 + (nj - 1)*(mneigh+1)*mneigh
      do nl = 1,nneigh(nrj)
        ind = ind + 1
        if (d1(ind).ne.0.0_dp) then
          l = neighno(nl,nrj)
          xdiff = xneigh(nl,nrj) + xneigh(nj,nri)
          ydiff = yneigh(nl,nrj) + yneigh(nj,nri)
          zdiff = zneigh(nl,nrj) + zneigh(nj,nri)
          xd = xdiff*d1(ind)
          yd = ydiff*d1(ind)
          zd = zdiff*d1(ind)
          io = nfreeatom(i)
          lo = nfreeatom(l)
          ia = nauatom(i)
          la = nauatom(l)
          if (io.gt.0.and.ia.gt.0) then
            xdrv(ia) = xdrv(ia) - xd*dble(neqvatom(ia))
            ydrv(ia) = ydrv(ia) - yd*dble(neqvatom(ia))
            zdrv(ia) = zdrv(ia) - zd*dble(neqvatom(ia))
            nregi = nregionno(nsft+ia)
            xregdrv(nregi) = xregdrv(nregi) - xd*dble(neqvatom(ia))
            yregdrv(nregi) = yregdrv(nregi) - yd*dble(neqvatom(ia))
            zregdrv(nregi) = zregdrv(nregi) - zd*dble(neqvatom(ia))
          endif
          if (lo.gt.0.and.la.gt.0) then
            xdrv(la) = xdrv(la) + xd*dble(neqvatom(la))
            ydrv(la) = ydrv(la) + yd*dble(neqvatom(la))
            zdrv(la) = zdrv(la) + zd*dble(neqvatom(la))
            nregl = nregionno(nsft+la)
            xregdrv(nregl) = xregdrv(nregl) + xd*dble(neqvatom(la))
            yregdrv(nregl) = yregdrv(nregl) + yd*dble(neqvatom(la))
            zregdrv(nregl) = zregdrv(nregl) + zd*dble(neqvatom(la))
          endif
!
          if (lstr) then
            select case(ndim)
              case(1)
                rstrd(1) = rstrd(1) + xd*xdiff
              case(2)
                rstrd(1) = rstrd(1) + xd*xdiff
                rstrd(2) = rstrd(2) + yd*ydiff
                rstrd(3) = rstrd(3) + xd*ydiff
              case(3)
                rstrd(1) = rstrd(1) + xd*xdiff
                rstrd(2) = rstrd(2) + yd*ydiff
                rstrd(3) = rstrd(3) + zd*zdiff
                rstrd(4) = rstrd(4) + yd*zdiff
                rstrd(5) = rstrd(5) + xd*zdiff
                rstrd(6) = rstrd(6) + xd*ydiff
            end select
          endif
        endif
      enddo
    enddo
!
!  Loop over neighbours -> neighbours of neighbours
!
    do nj = 1,nneigh(nri)
      j = neighno(nj,nri)
      nrj = nREBOatomRptr(j)
      do nk = 1,nneigh(nri)
        k = neighno(nk,nri)
        ind = nneigh(nri) + nneigh(nri)*(nneigh(nri) + 1)/2 + (nj - 1)*(mneigh+1)*mneigh + nk*mneigh
        do nl = 1,nneigh(nrj)
          ind = ind + 1
          if (d1(ind).ne.0.0_dp) then
            l = neighno(nl,nrj)
            xdiff = xneigh(nl,nrj) + xneigh(nj,nri) - xneigh(nk,nri)
            ydiff = yneigh(nl,nrj) + yneigh(nj,nri) - yneigh(nk,nri)
            zdiff = zneigh(nl,nrj) + zneigh(nj,nri) - zneigh(nk,nri)
            xd = xdiff*d1(ind)
            yd = ydiff*d1(ind)
            zd = zdiff*d1(ind)
            ko = nfreeatom(k)
            lo = nfreeatom(l)
            ka = nauatom(k)
            la = nauatom(l)
            if (ko.gt.0.and.ka.gt.0) then
              xdrv(ka) = xdrv(ka) - xd*dble(neqvatom(ka))
              ydrv(ka) = ydrv(ka) - yd*dble(neqvatom(ka))
              zdrv(ka) = zdrv(ka) - zd*dble(neqvatom(ka))
              nregk = nregionno(nsft+ka)
              xregdrv(nregk) = xregdrv(nregk) - xd*dble(neqvatom(ka))
              yregdrv(nregk) = yregdrv(nregk) - yd*dble(neqvatom(ka))
              zregdrv(nregk) = zregdrv(nregk) - zd*dble(neqvatom(ka))
            endif
            if (lo.gt.0.and.la.gt.0) then
              xdrv(la) = xdrv(la) + xd*dble(neqvatom(la))
              ydrv(la) = ydrv(la) + yd*dble(neqvatom(la))
              zdrv(la) = zdrv(la) + zd*dble(neqvatom(la))
              nregl = nregionno(nsft+la)
              xregdrv(nregl) = xregdrv(nregl) + xd*dble(neqvatom(la))
              yregdrv(nregl) = yregdrv(nregl) + yd*dble(neqvatom(la))
              zregdrv(nregl) = zregdrv(nregl) + zd*dble(neqvatom(la))
            endif
!
            if (lstr) then
              select case(ndim)
                case(1)
                  rstrd(1) = rstrd(1) + xd*xdiff
                case(2)
                  rstrd(1) = rstrd(1) + xd*xdiff
                  rstrd(2) = rstrd(2) + yd*ydiff
                  rstrd(3) = rstrd(3) + xd*ydiff
                case(3)
                  rstrd(1) = rstrd(1) + xd*xdiff
                  rstrd(2) = rstrd(2) + yd*ydiff
                  rstrd(3) = rstrd(3) + zd*zdiff
                  rstrd(4) = rstrd(4) + yd*zdiff
                  rstrd(5) = rstrd(5) + xd*zdiff
                  rstrd(6) = rstrd(6) + xd*ydiff
              end select
            endif
          endif
        enddo
      enddo
    enddo
  endif
!
  return
  end
!
  subroutine d2add(i,maxneigh,nneigh,neighno,xneigh,yneigh,zneigh,nfreeatom,nREBOatomRptr,d1,d2,ltorderv)
!
!  Utility to add the pairwise derivatives for a group of atoms
!  on to the global Cartesian second derivative array.
!
!  On entry : 
!
!  i               = atom number of i to whom others are connected
!  maxneigh        = left-hand dimension of 2-D arrays
!  nneigh          = no. of neighbours of atoms
!  neighno         = atom number of each neighbour
!  xneigh          = x component of vector to neighbour of atoms
!  yneigh          = y component of vector to neighbour of atoms
!  zneigh          = z component of vector to neighbour of atoms
!  nfreeatom       = pointer from atom to free atom to move
!  nREBOatomRptr   = pointer from atoms to position in REBO set
!  d1              = array of first derivatives w.r.t. interatomic
!                    distances
!  d2              = array of first derivatives w.r.t. interatomic
!                    distances
!  ltorderv        = if true then include torsional derivatives
!
!   6/02 Created from d1add
!   6/02 Modified to allow for torsional terms
!   7/02 Indexing further modified for torsions
!   8/02 Strain derivatives added
!   8/02 Freezing added
!   8/02 Removal of uninvalid terms added
!   9/02 Correction to lsameijkl added - need to check distances too
!  12/03 Torsional derivatives made optional
!   6/07 nREBOatomRptr added as input
!  10/07 Correction made to setting of n1i/n2k for torsional case
!  11/07 Second criterion for lsameijkl removed as it gives wrong second
!        derivatives for reaxff.
!  11/07 Unused variables removed
!   1/10 Bug fixed for neighbour of i -> neighbour of neighbour of i case
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
!  Copyright Curtin University 2010
!
!  Julian Gale, NRI, Curtin University, January 2010
!
  use brennerdata
  use current,     only : ndim, nstrains
  use derivatives
  use symmetry,    only : lstr
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)             :: i
  integer(i4), intent(in)             :: maxneigh
  integer(i4), intent(in)             :: nneigh(*)
  integer(i4), intent(in)             :: neighno(maxneigh,*)
  real(dp),    intent(in)             :: xneigh(maxneigh,*)
  real(dp),    intent(in)             :: yneigh(maxneigh,*)
  real(dp),    intent(in)             :: zneigh(maxneigh,*)
  integer(i4), intent(in)             :: nfreeatom(*)
  integer(i4), intent(in)             :: nREBOatomRptr(*)
  real(dp),    intent(in)             :: d1(*)
  real(dp),    intent(in)             :: d2(*)
  logical,     intent(in)             :: ltorderv
!
!  Local variables
!
  integer(i4)                         :: ind
  integer(i4)                         :: ix,iy,iz
  integer(i4)                         :: jx,jy,jz
  integer(i4)                         :: kx,ky,kz
  integer(i4)                         :: lx,ly,lz
  integer(i4)                         :: ixc,iyc,izc
  integer(i4)                         :: jxc,jyc,jzc
  integer(i4)                         :: kxc,kyc,kzc
  integer(i4)                         :: lxc,lyc,lzc
  integer(i4)                         :: n1
  integer(i4)                         :: n1i
  integer(i4)                         :: n1j
  integer(i4)                         :: n1j2
  integer(i4)                         :: n2
  integer(i4)                         :: n2k
  integer(i4)                         :: n2l
  integer(i4)                         :: n2l2
  integer(i4)                         :: ndist
  integer(i4)                         :: nji
  integer(i4)                         :: nneigh2
  integer(i4)                         :: nri
  integer(i4)                         :: ns1
  integer(i4)                         :: ns2
  integer(i4)                         :: ntmp
  real(dp)                            :: one1
  real(dp)                            :: one2
  real(dp)                            :: rp1(6)
  real(dp)                            :: rp2(6)
  real(dp)                            :: r2ik
  real(dp)                            :: r2jl
  real(dp)                            :: xd1
  real(dp)                            :: yd1
  real(dp)                            :: zd1
  real(dp)                            :: xd2
  real(dp)                            :: yd2
  real(dp)                            :: zd2
  real(dp)                            :: xi
  real(dp)                            :: yi
  real(dp)                            :: zi
  real(dp)                            :: xik
  real(dp)                            :: yik
  real(dp)                            :: zik
  real(dp)                            :: xjk
  real(dp)                            :: yjk
  real(dp)                            :: zjk
  real(dp)                            :: xjl
  real(dp)                            :: yjl
  real(dp)                            :: zjl
  real(dp)                            :: x2i
  real(dp)                            :: y2i  
  real(dp)                            :: z2i
  logical                             :: lsameijkl
  logical                             :: liok
  logical                             :: ljok
  logical                             :: lkok
  logical                             :: llok
  logical                             :: lopi
  logical                             :: lopj
  logical                             :: lopk
  logical                             :: lopl
!
!  Calculate total number of distances
!
  nri = nREBOatomRptr(i)
  nneigh2 = nneigh(nri) + nneigh(nri)*(nneigh(nri) + 1)/2
  if (ltorderv) then
    ndist = nneigh2 + nneigh(nri)*(maxneigh + 1)*maxneigh
  else
    ndist = nneigh2 
  endif
!
!  Loop over pairs of interatomic distances
!
  ind = 0
  do n1 = 1,ndist
!******************************
!  Find pair of atoms for n1  *
!******************************
    if (n1.le.nneigh(nri)) then
!
!  i -> neighbours of i
!
      n1i = i
      n1j = neighno(n1,nri)
      xd1 = xneigh(n1,nri)
      yd1 = yneigh(n1,nri)
      zd1 = zneigh(n1,nri)
      x2i = 0.0_dp
      y2i = 0.0_dp
      z2i = 0.0_dp
      liok = .true.
      ljok = .true.
    elseif (n1.le.nneigh2) then
!
!  Neighbours of i -> neighbours of i
!
      ntmp = n1 - nneigh(nri)
      n1j = 1
      n1j2 = 1
      do while (n1j2.lt.ntmp)
        n1j = n1j + 1
        n1j2 = n1j2 + n1j
      enddo
      n1j2 = n1j2 - n1j
      n1i = ntmp - n1j2
!
!  Check that neighbour numbers are valid
!
      liok = (n1i.le.nneigh(nri))
      ljok = (n1j.le.nneigh(nri))
      if (liok.and.ljok) then
!
!  Get vector between atoms
!
        xd1 = (xneigh(n1j,nri) - xneigh(n1i,nri))
        yd1 = (yneigh(n1j,nri) - yneigh(n1i,nri))
        zd1 = (zneigh(n1j,nri) - zneigh(n1i,nri))
!  
        x2i = xneigh(n1i,nri)
        y2i = yneigh(n1i,nri)
        z2i = zneigh(n1i,nri)
!
!  Convert neighbour numbers to real atoms
!
        n1i = neighno(n1i,nri)
        n1j = neighno(n1j,nri)
      endif
    else
!
!  Neighbours of neighbours of i -> neighbours of i
!
      ntmp = n1 - nneigh2
      nji = (ntmp - 1)/((maxneigh + 1)*maxneigh) + 1
      ntmp = ntmp - (nji - 1)*(maxneigh + 1)*maxneigh
      n1i = (ntmp - 1)/maxneigh
      n1j = ntmp - n1i*maxneigh
!
!  Check that atoms are valid
!
      liok = (n1i.le.nneigh(nri))
      if (nji.le.nneigh(nri)) then
        ljok = (n1j.le.nneigh(nREBOatomRptr(neighno(nji,nri))))
      else
        ljok = .false.
      endif
!
      if (liok.and.ljok) then
        if (n1i.eq.0) then
          xi = 0.0_dp
          yi = 0.0_dp
          zi = 0.0_dp
          n1i = i
        else
          xi = xneigh(n1i,nri)
          yi = yneigh(n1i,nri)
          zi = zneigh(n1i,nri)
          n1i = neighno(n1i,nri)
        endif
!
        xd1 = (xneigh(n1j,nREBOatomRptr(neighno(nji,nri))) + xneigh(nji,nri) - xi)
        yd1 = (yneigh(n1j,nREBOatomRptr(neighno(nji,nri))) + yneigh(nji,nri) - yi)
        zd1 = (zneigh(n1j,nREBOatomRptr(neighno(nji,nri))) + zneigh(nji,nri) - zi)
!         
        x2i = xi
        y2i = yi
        z2i = zi
!
        n1j = neighno(n1j,nREBOatomRptr(neighno(nji,nri)))
      endif
    endif
!
!  Skip the following section as it is irrelevant if i and j are not valid
!
    if (liok.and.ljok) then
!
!  Set up products for strain
!
      if (lstr) then
        select case(ndim)
          case(1)
            rp1(1) = xd1*xd1
          case(2)
            rp1(1) = xd1*xd1
            rp1(2) = yd1*yd1
            rp1(3) = xd1*yd1
          case(3)
            rp1(1) = xd1*xd1
            rp1(2) = yd1*yd1
            rp1(3) = zd1*zd1
            rp1(4) = yd1*zd1
            rp1(5) = xd1*zd1
            rp1(6) = xd1*yd1
        end select
      endif
!
!  Set second derivative location pointers
!
      lopi = (nfreeatom(n1i).gt.0)
      lopj = (nfreeatom(n1j).gt.0)
      ix = 3*(nfreeatom(n1i) - 1) + 1
      iy = ix + 1
      iz = iy + 1
      jx = 3*(nfreeatom(n1j) - 1) + 1
      jy = jx + 1
      jz = jy + 1
    endif
!
!  If neither i nor j are being optimised and strain derivatives are not needed then atoms are not valid
!
    if (.not.lopi.and..not.lopj.and..not.lstr) then
      liok = .false.
      ljok = .false.
    endif
!
!  If i and j are not valid then there is no point continuing for this value of n1
!
    if (liok.and.ljok) then
      do n2 = 1,n1
        ind = ind + 1
        if (d2(ind).ne.0.0_dp.or.(n1.eq.n2.and.d1(n2).ne.0.0_dp)) then
!******************************
!  Find pair of atoms for n2  *
!******************************
          if (n2.le.nneigh(nri)) then
!
!  i -> neighbours of i
!
            n2k = i
            n2l = neighno(n2,nri)
            xd2 = xneigh(n2,nri)
            yd2 = yneigh(n2,nri)
            zd2 = zneigh(n2,nri)
!
            xik = - x2i
            yik = - y2i
            zik = - z2i
!
            lkok = .true.
            llok = .true.
          elseif (n2.le.nneigh2) then
!
!  Neighbours of i -> neighbours of i
!
            ntmp = n2 - nneigh(nri)
            n2l = 1
            n2l2 = 1
            do while (n2l2.lt.ntmp)
              n2l = n2l + 1
              n2l2 = n2l2 + n2l
            enddo
            n2l2 = n2l2 - n2l
            n2k = ntmp - n2l2
!
!  Check that atoms are valid
!
            lkok = (n2k.le.nneigh(nri))
            llok = (n2l.le.nneigh(nri))
            if (lkok.and.llok) then
!
!  Calculate vector between atoms
!
              xd2 = (xneigh(n2l,nri) - xneigh(n2k,nri))
              yd2 = (yneigh(n2l,nri) - yneigh(n2k,nri))
              zd2 = (zneigh(n2l,nri) - zneigh(n2k,nri))
!  
              xik = xneigh(n2k,nri) - x2i
              yik = yneigh(n2k,nri) - y2i
              zik = zneigh(n2k,nri) - z2i
!
!  Convert neighbour numbers to real atoms
!
              n2k = neighno(n2k,nri)
              n2l = neighno(n2l,nri)
            endif
          else
!
!  Neighbours of neighbours of i -> neighbours of i
!
            ntmp = n2 - nneigh2
            nji = (ntmp - 1)/((maxneigh + 1)*maxneigh) + 1
            ntmp = ntmp - (nji - 1)*(maxneigh + 1)*maxneigh
            n2k = (ntmp - 1)/maxneigh
            n2l = ntmp - n2k*maxneigh
!
!  Check that atoms are valid
!
            lkok = (n2k.le.nneigh(nri))
            if (nji.le.nneigh(nri)) then
              llok = (n2l.le.nneigh(nREBOatomRptr(neighno(nji,nri))))
            else
              llok = .false.
            endif
!
            if (lkok.and.llok) then
              if (n2k.eq.0) then
                xi = 0.0_dp
                yi = 0.0_dp
                zi = 0.0_dp
                n2k = i
              else
                xi = xneigh(n2k,nri)
                yi = yneigh(n2k,nri)
                zi = zneigh(n2k,nri)
                n2k = neighno(n2k,nri)
              endif
!         
              xd2 = (xneigh(n2l,nREBOatomRptr(neighno(nji,nri))) + xneigh(nji,nri) - xi)
              yd2 = (yneigh(n2l,nREBOatomRptr(neighno(nji,nri))) + yneigh(nji,nri) - yi)
              zd2 = (zneigh(n2l,nREBOatomRptr(neighno(nji,nri))) + zneigh(nji,nri) - zi)
!               
              xik = xi - x2i
              yik = yi - y2i
              zik = zi - z2i
!       
              n2l = neighno(n2l,nREBOatomRptr(neighno(nji,nri)))
            endif
          endif
!
!  No point continuing beyond here unless k and l are valid
!
          if (lkok.and.llok) then
!
!  Complete remaining vectors
!
            xjk = xik - xd1
            yjk = yik - yd1
            zjk = zik - zd1
            xjl = xjk + xd2
            yjl = yjk + yd2
            zjl = zjk + zd2
!
!  Compute distances squared
!
            r2ik = xik*xik + yik*yik + zik*zik
            r2jl = xjl*xjl + yjl*yjl + zjl*zjl
!
!  Set up products for strain
!
            if (lstr) then
              select case(ndim)
                case(1)
                  rp2(1) = xd2*xd2
                case(2)
                  rp2(1) = xd2*xd2
                  rp2(2) = yd2*yd2
                  rp2(3) = xd2*yd2
                case(3)
                  rp2(1) = xd2*xd2
                  rp2(2) = yd2*yd2
                  rp2(3) = zd2*zd2
                  rp2(4) = yd2*zd2
                  rp2(5) = xd2*zd2
                  rp2(6) = xd2*yd2
              end select
            endif
!
            lopk = (nfreeatom(n2k).gt.0)
            lopl = (nfreeatom(n2l).gt.0)
            kx = 3*(nfreeatom(n2k) - 1) + 1
            ky = kx + 1
            kz = ky + 1
            lx = 3*(nfreeatom(n2l) - 1) + 1
            ly = lx + 1
            lz = ly + 1
!
            lsameijkl = .false.
            if (n1i.eq.n2k.and.n1j.eq.n2l.and.r2ik.lt.1.0d-8.and.r2jl.lt.1.0d-8) lsameijkl = .true.
!
            if ((lopi.or.lopk).and.n1i.ne.n2k) then
!
!  Second derivatives : i - k
!
              if (lopi.and.lopk) then
                ixc = ix
                iyc = iy
                izc = iz
                kxc = kx
                kyc = ky
                kzc = kz
                one1 = 1.0_dp
                one2 = 1.0_dp
              elseif (lopi) then
                ixc = ix
                iyc = iy
                izc = iz
                kxc = ix
                kyc = iy
                kzc = iz
                if (n1i.gt.n2k) then
                  one1 = 1.0_dp
                  one2 = 0.0_dp
                else
                  one1 = 0.0_dp
                  one2 = 1.0_dp
                endif
              elseif (lopk) then
                ixc = kx
                iyc = ky
                izc = kz
                kxc = kx
                kyc = ky
                kzc = kz
                if (n1i.gt.n2k) then
                  one1 = 1.0_dp
                  one2 = 0.0_dp
                else
                  one1 = 0.0_dp
                  one2 = 1.0_dp
                endif
              endif
!
              derv2(kxc,ixc) = derv2(kxc,ixc) + xd1*xd2*d2(ind)*one1
              derv2(kyc,ixc) = derv2(kyc,ixc) + xd1*yd2*d2(ind)*one1
              derv2(kzc,ixc) = derv2(kzc,ixc) + xd1*zd2*d2(ind)*one1
              derv2(kxc,iyc) = derv2(kxc,iyc) + yd1*xd2*d2(ind)*one1
              derv2(kyc,iyc) = derv2(kyc,iyc) + yd1*yd2*d2(ind)*one1
              derv2(kzc,iyc) = derv2(kzc,iyc) + yd1*zd2*d2(ind)*one1
              derv2(kxc,izc) = derv2(kxc,izc) + zd1*xd2*d2(ind)*one1
              derv2(kyc,izc) = derv2(kyc,izc) + zd1*yd2*d2(ind)*one1
              derv2(kzc,izc) = derv2(kzc,izc) + zd1*zd2*d2(ind)*one1
              if (n1.eq.n2) then
                derv2(kxc,ixc) = derv2(kxc,ixc) + d1(n2)*one1
                derv2(kyc,iyc) = derv2(kyc,iyc) + d1(n2)*one1
                derv2(kzc,izc) = derv2(kzc,izc) + d1(n2)*one1
              endif
              if (.not.lsameijkl) then
                derv2(ixc,kxc) = derv2(ixc,kxc) + xd1*xd2*d2(ind)*one2
                derv2(ixc,kyc) = derv2(ixc,kyc) + xd1*yd2*d2(ind)*one2
                derv2(ixc,kzc) = derv2(ixc,kzc) + xd1*zd2*d2(ind)*one2
                derv2(iyc,kxc) = derv2(iyc,kxc) + yd1*xd2*d2(ind)*one2
                derv2(iyc,kyc) = derv2(iyc,kyc) + yd1*yd2*d2(ind)*one2
                derv2(iyc,kzc) = derv2(iyc,kzc) + yd1*zd2*d2(ind)*one2
                derv2(izc,kxc) = derv2(izc,kxc) + zd1*xd2*d2(ind)*one2
                derv2(izc,kyc) = derv2(izc,kyc) + zd1*yd2*d2(ind)*one2
                derv2(izc,kzc) = derv2(izc,kzc) + zd1*zd2*d2(ind)*one2
                if (n1.eq.n2) then
                  derv2(ixc,kxc) = derv2(ixc,kxc) + d1(n2)*one2
                  derv2(iyc,kyc) = derv2(iyc,kyc) + d1(n2)*one2
                  derv2(izc,kzc) = derv2(izc,kzc) + d1(n2)*one2
                endif
              endif
            endif
!
!  Second derivatives : i - l
!
            if ((lopi.or.lopl).and.n1i.ne.n2l) then
              if (lopi.and.lopl) then
                ixc = ix
                iyc = iy
                izc = iz
                lxc = lx
                lyc = ly
                lzc = lz
                one1 = 1.0_dp
                one2 = 1.0_dp
              elseif (lopi) then
                ixc = ix
                iyc = iy
                izc = iz
                lxc = ix
                lyc = iy
                lzc = iz
                if (n1i.gt.n2l) then
                  one1 = 1.0_dp
                  one2 = 0.0_dp
                else
                  one1 = 0.0_dp
                  one2 = 1.0_dp
                endif
              elseif (lopl) then
                ixc = lx
                iyc = ly
                izc = lz
                lxc = lx
                lyc = ly
                lzc = lz
                if (n1i.gt.n2l) then
                  one1 = 1.0_dp
                  one2 = 0.0_dp
                else
                  one1 = 0.0_dp
                  one2 = 1.0_dp
                endif
              endif
!
              derv2(lxc,ixc) = derv2(lxc,ixc) - xd1*xd2*d2(ind)*one1
              derv2(lyc,ixc) = derv2(lyc,ixc) - xd1*yd2*d2(ind)*one1
              derv2(lzc,ixc) = derv2(lzc,ixc) - xd1*zd2*d2(ind)*one1
              derv2(lxc,iyc) = derv2(lxc,iyc) - yd1*xd2*d2(ind)*one1
              derv2(lyc,iyc) = derv2(lyc,iyc) - yd1*yd2*d2(ind)*one1
              derv2(lzc,iyc) = derv2(lzc,iyc) - yd1*zd2*d2(ind)*one1
              derv2(lxc,izc) = derv2(lxc,izc) - zd1*xd2*d2(ind)*one1
              derv2(lyc,izc) = derv2(lyc,izc) - zd1*yd2*d2(ind)*one1
              derv2(lzc,izc) = derv2(lzc,izc) - zd1*zd2*d2(ind)*one1
              if (n1.eq.n2) then
                derv2(lxc,ixc) = derv2(lxc,ixc) - d1(n2)*one1
                derv2(lyc,iyc) = derv2(lyc,iyc) - d1(n2)*one1
                derv2(lzc,izc) = derv2(lzc,izc) - d1(n2)*one1
              endif
              if (.not.lsameijkl) then
                derv2(ixc,lxc) = derv2(ixc,lxc) - xd1*xd2*d2(ind)*one2
                derv2(ixc,lyc) = derv2(ixc,lyc) - xd1*yd2*d2(ind)*one2
                derv2(ixc,lzc) = derv2(ixc,lzc) - xd1*zd2*d2(ind)*one2
                derv2(iyc,lxc) = derv2(iyc,lxc) - yd1*xd2*d2(ind)*one2
                derv2(iyc,lyc) = derv2(iyc,lyc) - yd1*yd2*d2(ind)*one2
                derv2(iyc,lzc) = derv2(iyc,lzc) - yd1*zd2*d2(ind)*one2
                derv2(izc,lxc) = derv2(izc,lxc) - zd1*xd2*d2(ind)*one2
                derv2(izc,lyc) = derv2(izc,lyc) - zd1*yd2*d2(ind)*one2
                derv2(izc,lzc) = derv2(izc,lzc) - zd1*zd2*d2(ind)*one2
                if (n1.eq.n2) then
                  derv2(ixc,lxc) = derv2(ixc,lxc) - d1(n2)*one2
                  derv2(iyc,lyc) = derv2(iyc,lyc) - d1(n2)*one2
                  derv2(izc,lzc) = derv2(izc,lzc) - d1(n2)*one2
                endif
              endif
            endif
!
!  Second derivatives : j - k
!
            if ((lopj.or.lopk).and.n1j.ne.n2k) then
              if (lopj.and.lopk) then
                jxc = jx
                jyc = jy
                jzc = jz
                kxc = kx
                kyc = ky
                kzc = kz
                one1 = 1.0_dp
                one2 = 1.0_dp
              elseif (lopj) then
                jxc = jx
                jyc = jy
                jzc = jz
                kxc = jx
                kyc = jy
                kzc = jz
                if (n1j.gt.n2k) then
                  one1 = 1.0_dp
                  one2 = 0.0_dp
                else
                  one1 = 0.0_dp
                  one2 = 1.0_dp
                endif
              elseif (lopk) then
                jxc = kx
                jyc = ky
                jzc = kz
                kxc = kx
                kyc = ky
                kzc = kz
                if (n1j.gt.n2k) then
                  one1 = 1.0_dp
                  one2 = 0.0_dp
                else
                  one1 = 0.0_dp
                  one2 = 1.0_dp
                endif
              endif
!
              derv2(kxc,jxc) = derv2(kxc,jxc) - xd1*xd2*d2(ind)*one1
              derv2(kyc,jxc) = derv2(kyc,jxc) - xd1*yd2*d2(ind)*one1
              derv2(kzc,jxc) = derv2(kzc,jxc) - xd1*zd2*d2(ind)*one1
              derv2(kxc,jyc) = derv2(kxc,jyc) - yd1*xd2*d2(ind)*one1
              derv2(kyc,jyc) = derv2(kyc,jyc) - yd1*yd2*d2(ind)*one1
              derv2(kzc,jyc) = derv2(kzc,jyc) - yd1*zd2*d2(ind)*one1
              derv2(kxc,jzc) = derv2(kxc,jzc) - zd1*xd2*d2(ind)*one1
              derv2(kyc,jzc) = derv2(kyc,jzc) - zd1*yd2*d2(ind)*one1
              derv2(kzc,jzc) = derv2(kzc,jzc) - zd1*zd2*d2(ind)*one1
              if (n1.eq.n2) then
                derv2(kxc,jxc) = derv2(kxc,jxc) - d1(n2)*one1
                derv2(kyc,jyc) = derv2(kyc,jyc) - d1(n2)*one1
                derv2(kzc,jzc) = derv2(kzc,jzc) - d1(n2)*one1
              endif
              if (.not.lsameijkl) then
                derv2(jxc,kxc) = derv2(jxc,kxc) - xd1*xd2*d2(ind)*one2
                derv2(jxc,kyc) = derv2(jxc,kyc) - xd1*yd2*d2(ind)*one2
                derv2(jxc,kzc) = derv2(jxc,kzc) - xd1*zd2*d2(ind)*one2
                derv2(jyc,kxc) = derv2(jyc,kxc) - yd1*xd2*d2(ind)*one2
                derv2(jyc,kyc) = derv2(jyc,kyc) - yd1*yd2*d2(ind)*one2
                derv2(jyc,kzc) = derv2(jyc,kzc) - yd1*zd2*d2(ind)*one2
                derv2(jzc,kxc) = derv2(jzc,kxc) - zd1*xd2*d2(ind)*one2
                derv2(jzc,kyc) = derv2(jzc,kyc) - zd1*yd2*d2(ind)*one2
                derv2(jzc,kzc) = derv2(jzc,kzc) - zd1*zd2*d2(ind)*one2
                if (n1.eq.n2) then
                  derv2(jxc,kxc) = derv2(jxc,kxc) - d1(n2)*one2
                  derv2(jyc,kyc) = derv2(jyc,kyc) - d1(n2)*one2
                  derv2(jzc,kzc) = derv2(jzc,kzc) - d1(n2)*one2
                endif
              endif
            endif
!
!  Second derivatives : j - l
!
            if ((lopj.or.lopl).and.n1j.ne.n2l) then
              if (lopj.and.lopl) then
                jxc = jx
                jyc = jy
                jzc = jz
                lxc = lx
                lyc = ly
                lzc = lz
                one1 = 1.0_dp
                one2 = 1.0_dp
              elseif (lopj) then
                jxc = jx
                jyc = jy
                jzc = jz
                lxc = jx
                lyc = jy
                lzc = jz
                if (n1j.gt.n2l) then
                  one1 = 1.0_dp
                  one2 = 0.0_dp
                else
                  one1 = 0.0_dp
                  one2 = 1.0_dp
                endif
              elseif (lopl) then
                jxc = lx
                jyc = ly
                jzc = lz
                lxc = lx
                lyc = ly
                lzc = lz
                if (n1j.gt.n2l) then
                  one1 = 1.0_dp
                  one2 = 0.0_dp
                else
                  one1 = 0.0_dp
                  one2 = 1.0_dp
                endif
              endif
!
              derv2(lxc,jxc) = derv2(lxc,jxc) + xd1*xd2*d2(ind)*one1
              derv2(lyc,jxc) = derv2(lyc,jxc) + xd1*yd2*d2(ind)*one1
              derv2(lzc,jxc) = derv2(lzc,jxc) + xd1*zd2*d2(ind)*one1
              derv2(lxc,jyc) = derv2(lxc,jyc) + yd1*xd2*d2(ind)*one1
              derv2(lyc,jyc) = derv2(lyc,jyc) + yd1*yd2*d2(ind)*one1
              derv2(lzc,jyc) = derv2(lzc,jyc) + yd1*zd2*d2(ind)*one1
              derv2(lxc,jzc) = derv2(lxc,jzc) + zd1*xd2*d2(ind)*one1
              derv2(lyc,jzc) = derv2(lyc,jzc) + zd1*yd2*d2(ind)*one1
              derv2(lzc,jzc) = derv2(lzc,jzc) + zd1*zd2*d2(ind)*one1
              if (n1.eq.n2) then
                derv2(lxc,jxc) = derv2(lxc,jxc) + d1(n2)*one1
                derv2(lyc,jyc) = derv2(lyc,jyc) + d1(n2)*one1
                derv2(lzc,jzc) = derv2(lzc,jzc) + d1(n2)*one1
              endif
              if (.not.lsameijkl) then
                derv2(jxc,lxc) = derv2(jxc,lxc) + xd1*xd2*d2(ind)*one2
                derv2(jxc,lyc) = derv2(jxc,lyc) + xd1*yd2*d2(ind)*one2
                derv2(jxc,lzc) = derv2(jxc,lzc) + xd1*zd2*d2(ind)*one2
                derv2(jyc,lxc) = derv2(jyc,lxc) + yd1*xd2*d2(ind)*one2
                derv2(jyc,lyc) = derv2(jyc,lyc) + yd1*yd2*d2(ind)*one2
                derv2(jyc,lzc) = derv2(jyc,lzc) + yd1*zd2*d2(ind)*one2
                derv2(jzc,lxc) = derv2(jzc,lxc) + zd1*xd2*d2(ind)*one2
                derv2(jzc,lyc) = derv2(jzc,lyc) + zd1*yd2*d2(ind)*one2
                derv2(jzc,lzc) = derv2(jzc,lzc) + zd1*zd2*d2(ind)*one2
                if (n1.eq.n2) then
                  derv2(jxc,lxc) = derv2(jxc,lxc) + d1(n2)*one2
                  derv2(jyc,lyc) = derv2(jyc,lyc) + d1(n2)*one2
                  derv2(jzc,lzc) = derv2(jzc,lzc) + d1(n2)*one2
                endif
              endif
            endif
            if (lstr) then
!
!  Strain - strain second derivatives
!
              do ns1 = 1,nstrains
                do ns2 = 1,nstrains
                  sderv2(ns2,ns1) = sderv2(ns2,ns1) + d2(ind)*rp2(ns2)*rp1(ns1)
                  if (n1.ne.n2) then
                    sderv2(ns2,ns1) = sderv2(ns2,ns1) + d2(ind)*rp1(ns2)*rp2(ns1)
                  endif
                enddo
              enddo
!
!  Internal - strain second derivatives
!
              do ns1 = 1,nstrains
                derv3(ix,ns1) = derv3(ix,ns1) - xd1*d2(ind)*rp2(ns1)
                derv3(iy,ns1) = derv3(iy,ns1) - yd1*d2(ind)*rp2(ns1)
                derv3(iz,ns1) = derv3(iz,ns1) - zd1*d2(ind)*rp2(ns1)
                derv3(jx,ns1) = derv3(jx,ns1) + xd1*d2(ind)*rp2(ns1)
                derv3(jy,ns1) = derv3(jy,ns1) + yd1*d2(ind)*rp2(ns1)
                derv3(jz,ns1) = derv3(jz,ns1) + zd1*d2(ind)*rp2(ns1)
                if (n1.ne.n2) then
                  derv3(kx,ns1) = derv3(kx,ns1) - xd2*d2(ind)*rp1(ns1)
                  derv3(ky,ns1) = derv3(ky,ns1) - yd2*d2(ind)*rp1(ns1)
                  derv3(kz,ns1) = derv3(kz,ns1) - zd2*d2(ind)*rp1(ns1)
                  derv3(lx,ns1) = derv3(lx,ns1) + xd2*d2(ind)*rp1(ns1)
                  derv3(ly,ns1) = derv3(ly,ns1) + yd2*d2(ind)*rp1(ns1)
                  derv3(lz,ns1) = derv3(lz,ns1) + zd2*d2(ind)*rp1(ns1)
                endif
              enddo
            endif
          endif
        endif
      enddo
    else
!
!  Increment ind pointer by number of missed terms
!
      ind = ind + n1
    endif
  enddo
!
  return
  end
!
  subroutine d2adds(i,maxneigh,nneigh,neighno,xneigh,yneigh,zneigh,nfreeatom,nfreeatomau, &
                    nREBOatomRptr,nauatom,neqvatom,d1,d2,ltorderv)
!
!  Utility to add the pairwise derivatives for a group of atoms
!  on to the global Cartesian second derivative array.
!
!  Symmetry adapted version of d2add
!
!  On entry : 
!
!  i               = atom number of i to whom others are connected
!  maxneigh        = left-hand dimension of 2-D arrays
!  nneigh          = no. of neighbours of atoms
!  neighno         = atom number of each neighbour
!  xneigh          = x component of vector to neighbour of atoms
!  yneigh          = y component of vector to neighbour of atoms
!  zneigh          = z component of vector to neighbour of atoms
!  nfreeatom       = pointer from atom to free atom to move in full cell
!  nfreeatomau     = pointer from atom to free atom to move in asymmetric unit
!  nREBOatomRptr   = pointer from atoms to position in REBO set
!  nauatom         = pointer from atom to asymmetric unit atom
!  neqvatom        = number of equivalent positions for asymmetric unit atom
!  d1              = array of first derivatives w.r.t. interatomic
!                    distances
!  d2              = array of first derivatives w.r.t. interatomic
!                    distances
!  ltorderv        = if true then include torsional derivatives
!
!   9/02 Created from d2add
!  12/03 Torsional derivatives made optional
!   6/07 nREBOatomRptr added as input
!  10/07 Correction made to setting of n1i/n2k for torsional case
!  11/07 Unused variables removed
!   1/10 Bug fixed for neighbour of i -> neighbour of neighbour of i case
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
!  Copyright Curtin University 2010
!
!  Julian Gale, NRI, Curtin University, January 2010
!
  use brennerdata
  use current,     only : ndim, nstrains
  use derivatives
  use symmetry,    only : lstr
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)             :: i
  integer(i4), intent(in)             :: maxneigh
  integer(i4), intent(in)             :: nneigh(*)
  integer(i4), intent(in)             :: neighno(maxneigh,*)
  real(dp),    intent(in)             :: xneigh(maxneigh,*)
  real(dp),    intent(in)             :: yneigh(maxneigh,*)
  real(dp),    intent(in)             :: zneigh(maxneigh,*)
  integer(i4), intent(in)             :: nfreeatom(*)
  integer(i4), intent(in)             :: nfreeatomau(*)
  integer(i4), intent(in)             :: nREBOatomRptr(*)
  integer(i4), intent(in)             :: nauatom(*)
  integer(i4), intent(in)             :: neqvatom(*)
  real(dp),    intent(in)             :: d1(*)
  real(dp),    intent(in)             :: d2(*)
  logical,     intent(in)             :: ltorderv
!
!  Local variables
!
  integer(i4)                         :: ind
  integer(i4)                         :: ix,iy,iz
  integer(i4)                         :: jx,jy,jz
  integer(i4)                         :: kx,ky,kz
  integer(i4)                         :: lx,ly,lz
  integer(i4)                         :: ixa,iya,iza
  integer(i4)                         :: jxa,jya,jza
  integer(i4)                         :: kxa,kya,kza
  integer(i4)                         :: lxa,lya,lza
  integer(i4)                         :: ixc,iyc,izc
  integer(i4)                         :: jxc,jyc,jzc
  integer(i4)                         :: kxc,kyc,kzc
  integer(i4)                         :: lxc,lyc,lzc
  integer(i4)                         :: n1
  integer(i4)                         :: n1i
  integer(i4)                         :: n1j
  integer(i4)                         :: n1j2
  integer(i4)                         :: n2
  integer(i4)                         :: n2k
  integer(i4)                         :: n2l
  integer(i4)                         :: n2l2
  integer(i4)                         :: ndist
  integer(i4)                         :: nji
  integer(i4)                         :: nneigh2
  integer(i4)                         :: nri
  integer(i4)                         :: ns1
  integer(i4)                         :: ns2
  integer(i4)                         :: ntmp
  real(dp)                            :: rp1(6)
  real(dp)                            :: rp2(6)
  real(dp)                            :: r2ik
  real(dp)                            :: r2jl
  real(dp)                            :: rneq
  real(dp)                            :: xd1
  real(dp)                            :: yd1
  real(dp)                            :: zd1
  real(dp)                            :: xd2
  real(dp)                            :: yd2
  real(dp)                            :: zd2
  real(dp)                            :: xi
  real(dp)                            :: yi
  real(dp)                            :: zi
  real(dp)                            :: xik
  real(dp)                            :: yik
  real(dp)                            :: zik
  real(dp)                            :: xjk
  real(dp)                            :: yjk
  real(dp)                            :: zjk
  real(dp)                            :: xjl
  real(dp)                            :: yjl
  real(dp)                            :: zjl
  real(dp)                            :: x2i
  real(dp)                            :: y2i  
  real(dp)                            :: z2i
  logical                             :: lsameijkl
  logical                             :: liok
  logical                             :: ljok
  logical                             :: lkok
  logical                             :: llok
  logical                             :: lopi
  logical                             :: lopj
  logical                             :: lopk
  logical                             :: lopl
!
!  Calculate total number of distances
!
  nri = nREBOatomRptr(i)
  nneigh2 = nneigh(nri) + nneigh(nri)*(nneigh(nri) + 1)/2
  if (ltorderv) then
    ndist = nneigh2 + nneigh(nri)*(maxneigh + 1)*maxneigh
  else
    ndist = nneigh2
  endif
!
!  Loop over pairs of interatomic distances
!
  ind = 0
  do n1 = 1,ndist
!******************************
!  Find pair of atoms for n1  *
!******************************
    if (n1.le.nneigh(nri)) then
!
!  i -> neighbours of i
!
      n1i = i
      n1j = neighno(n1,nri)
      xd1 = xneigh(n1,nri)
      yd1 = yneigh(n1,nri)
      zd1 = zneigh(n1,nri)
      x2i = 0.0_dp
      y2i = 0.0_dp
      z2i = 0.0_dp
      liok = .true.
      ljok = .true.
    elseif (n1.le.nneigh2) then
!
!  Neighbours of i -> neighbours of i
!
      ntmp = n1 - nneigh(nri)
      n1j = 1
      n1j2 = 1
      do while (n1j2.lt.ntmp)
        n1j = n1j + 1
        n1j2 = n1j2 + n1j
      enddo
      n1j2 = n1j2 - n1j
      n1i = ntmp - n1j2
!
!  Check that neighbour numbers are valid
!
      liok = (n1i.le.nneigh(nri))
      ljok = (n1j.le.nneigh(nri))
      if (liok.and.ljok) then
!
!  Get vector between atoms
!
        xd1 = (xneigh(n1j,nri) - xneigh(n1i,nri))
        yd1 = (yneigh(n1j,nri) - yneigh(n1i,nri))
        zd1 = (zneigh(n1j,nri) - zneigh(n1i,nri))
!  
        x2i = xneigh(n1i,nri)
        y2i = yneigh(n1i,nri)
        z2i = zneigh(n1i,nri)
!
!  Convert neighbour numbers to real atoms
!
        n1i = neighno(n1i,nri)
        n1j = neighno(n1j,nri)
      endif
    else
!
!  Neighbours of neighbours of i -> neighbours of i
!
      ntmp = n1 - nneigh2
      nji = (ntmp - 1)/((maxneigh + 1)*maxneigh) + 1
      ntmp = ntmp - (nji - 1)*(maxneigh + 1)*maxneigh
      n1i = (ntmp - 1)/maxneigh
      n1j = ntmp - n1i*maxneigh
!
!  Check that atoms are valid
!
      liok = (n1i.le.nneigh(nri))
      if (nji.le.nneigh(nri)) then
        ljok = (n1j.le.nneigh(nREBOatomRptr(neighno(nji,nri))))
      else
        ljok = .false.
      endif
!
      if (liok.and.ljok) then
        if (n1i.eq.0) then
          xi = 0.0_dp
          yi = 0.0_dp
          zi = 0.0_dp
          n1i = i
        else
          xi = xneigh(n1i,nri)
          yi = yneigh(n1i,nri)
          zi = zneigh(n1i,nri)
          n1i = neighno(n1i,nri)
        endif
!
        xd1 = (xneigh(n1j,nREBOatomRptr(neighno(nji,nri))) + xneigh(nji,nri) - xi)
        yd1 = (yneigh(n1j,nREBOatomRptr(neighno(nji,nri))) + yneigh(nji,nri) - yi)
        zd1 = (zneigh(n1j,nREBOatomRptr(neighno(nji,nri))) + zneigh(nji,nri) - zi)
!         
        x2i = xi
        y2i = yi
        z2i = zi
!
        n1j = neighno(n1j,nREBOatomRptr(neighno(nji,nri)))
      endif
    endif
!
!  Skip the following section as it is irrelevant if i and j are not valid
!
    if (liok.and.ljok) then
!
!  Set up products for strain
!
      if (lstr) then
        select case(ndim)
          case(1)
            rp1(1) = xd1*xd1
          case(2)
            rp1(1) = xd1*xd1
            rp1(2) = yd1*yd1
            rp1(3) = xd1*yd1
          case(3)
            rp1(1) = xd1*xd1
            rp1(2) = yd1*yd1
            rp1(3) = zd1*zd1
            rp1(4) = yd1*zd1
            rp1(5) = xd1*zd1
            rp1(6) = xd1*yd1
        end select
      endif
!
!  Set second derivative location pointers
!
      lopi = (nfreeatom(n1i).gt.0)
      lopj = (nfreeatom(n1j).gt.0)
      ix = 3*(nfreeatom(n1i) - 1) + 1
      iy = ix + 1
      iz = iy + 1
      jx = 3*(nfreeatom(n1j) - 1) + 1
      jy = jx + 1
      jz = jy + 1
      if (nauatom(n1i).gt.0) then
        ixa = 3*(nfreeatomau(nauatom(n1i)) - 1) + 1
        iya = ixa + 1
        iza = iya + 1
      endif
      if (nauatom(n1j).gt.0) then
        jxa = 3*(nfreeatomau(nauatom(n1j)) - 1) + 1
        jya = jxa + 1
        jza = jya + 1
      endif
    endif
!
!  If neither i nor j are being optimised and strain derivatives are not needed then atoms are not valid
!
    if (.not.lopi.and..not.lopj.and..not.lstr) then
      liok = .false.
      ljok = .false.
    endif
!
!  If i and j are not valid then there is no point continuing for this value of n1
!
    if (liok.and.ljok) then
      do n2 = 1,n1
        ind = ind + 1
        if (d2(ind).ne.0.0_dp.or.(n1.eq.n2.and.d1(n2).ne.0.0_dp)) then
!******************************
!  Find pair of atoms for n2  *
!******************************
          if (n2.le.nneigh(nri)) then
!
!  i -> neighbours of i
!
            n2k = i
            n2l = neighno(n2,nri)
            xd2 = xneigh(n2,nri)
            yd2 = yneigh(n2,nri)
            zd2 = zneigh(n2,nri)
!
            xik = - x2i
            yik = - y2i
            zik = - z2i
!
            lkok = .true.
            llok = .true.
          elseif (n2.le.nneigh2) then
!
!  Neighbours of i -> neighbours of i
!
            ntmp = n2 - nneigh(nri)
            n2l = 1
            n2l2 = 1
            do while (n2l2.lt.ntmp)
              n2l = n2l + 1
              n2l2 = n2l2 + n2l
            enddo
            n2l2 = n2l2 - n2l
            n2k = ntmp - n2l2
!
!  Check that atoms are valid
!
            lkok = (n2k.le.nneigh(nri))
            llok = (n2l.le.nneigh(nri))
            if (lkok.and.llok) then
!
!  Calculate vector between atoms
!
              xd2 = (xneigh(n2l,nri) - xneigh(n2k,nri))
              yd2 = (yneigh(n2l,nri) - yneigh(n2k,nri))
              zd2 = (zneigh(n2l,nri) - zneigh(n2k,nri))
!  
              xik = xneigh(n2k,nri) - x2i
              yik = yneigh(n2k,nri) - y2i
              zik = zneigh(n2k,nri) - z2i
!
!  Convert neighbour numbers to real atoms
!
              n2k = neighno(n2k,nri)
              n2l = neighno(n2l,nri)
            endif
          else
!
!  Neighbours of neighbours of i -> neighbours of i
!
            ntmp = n2 - nneigh2
            nji = (ntmp - 1)/((maxneigh + 1)*maxneigh) + 1
            ntmp = ntmp - (nji - 1)*(maxneigh + 1)*maxneigh
            n2k = (ntmp - 1)/maxneigh
            n2l = ntmp - n2k*maxneigh
!
!  Check that atoms are valid
!
            lkok = (n2k.le.nneigh(nri))
            if (nji.le.nneigh(nri)) then
              llok = (n2l.le.nneigh(nREBOatomRptr(neighno(nji,nri))))
            else
              llok = .false.
            endif
!
            if (lkok.and.llok) then
              if (n2k.eq.0) then
                xi = 0.0_dp
                yi = 0.0_dp
                zi = 0.0_dp
                n2k = i
              else
                xi = xneigh(n2k,nri)
                yi = yneigh(n2k,nri)
                zi = zneigh(n2k,nri)
                n2k = neighno(n2k,nri)
              endif
!         
              xd2 = (xneigh(n2l,nREBOatomRptr(neighno(nji,nri))) + xneigh(nji,nri) - xi)
              yd2 = (yneigh(n2l,nREBOatomRptr(neighno(nji,nri))) + yneigh(nji,nri) - yi)
              zd2 = (zneigh(n2l,nREBOatomRptr(neighno(nji,nri))) + zneigh(nji,nri) - zi)
!               
              xik = xi - x2i
              yik = yi - y2i
              zik = zi - z2i
!       
              n2l = neighno(n2l,nREBOatomRptr(neighno(nji,nri)))
            endif
          endif
!
!  No point continuing beyond here unless k and l are valid
!
          if (lkok.and.llok) then
!
!  Complete remaining vectors
!
            xjk = xik - xd1
            yjk = yik - yd1
            zjk = zik - zd1
            xjl = xjk + xd2
            yjl = yjk + yd2
            zjl = zjk + zd2
!
!  Compute distances squared
!
            r2ik = xik*xik + yik*yik + zik*zik
            r2jl = xjl*xjl + yjl*yjl + zjl*zjl
!
!  Set up products for strain
!
            if (lstr) then
              select case(ndim)
                case(1)
                  rp2(1) = xd2*xd2
                case(2)
                  rp2(1) = xd2*xd2
                  rp2(2) = yd2*yd2
                  rp2(3) = xd2*yd2
                case(3)
                  rp2(1) = xd2*xd2
                  rp2(2) = yd2*yd2
                  rp2(3) = zd2*zd2
                  rp2(4) = yd2*zd2
                  rp2(5) = xd2*zd2
                  rp2(6) = xd2*yd2
              end select
            endif
!
            lopk = (nfreeatom(n2k).gt.0)
            lopl = (nfreeatom(n2l).gt.0)
            kx = 3*(nfreeatom(n2k) - 1) + 1
            ky = kx + 1
            kz = ky + 1
            lx = 3*(nfreeatom(n2l) - 1) + 1
            ly = lx + 1
            lz = ly + 1
            if (nauatom(n2k).gt.0) then
              kxa = 3*(nfreeatomau(nauatom(n2k)) - 1) + 1
              kya = kxa + 1
              kza = kya + 1
            endif
            if (nauatom(n2l).gt.0) then
              lxa = 3*(nfreeatomau(nauatom(n2l)) - 1) + 1
              lya = lxa + 1
              lza = lya + 1
            endif
!
            lsameijkl = .false.
            if (n1i.eq.n2k.and.n1j.eq.n2l.and.r2ik.lt.1.0d-8.and.r2jl.lt.1.0d-8) lsameijkl = .true.
!
            if ((lopi.or.lopk).and.n1i.ne.n2k) then
!
!  Second derivatives : i - k
!
              if (lopi.and.nauatom(n1i).gt.0) then
                rneq = dble(neqvatom(nauatom(n1i)))
                if (lopk) then
                  kxc = kx
                  kyc = ky
                  kzc = kz
                else
                  kxc = ix
                  kyc = iy
                  kzc = iz
                endif
                derv2(kxc,ixa) = derv2(kxc,ixa) + xd1*xd2*d2(ind)*rneq
                derv2(kyc,ixa) = derv2(kyc,ixa) + xd1*yd2*d2(ind)*rneq
                derv2(kzc,ixa) = derv2(kzc,ixa) + xd1*zd2*d2(ind)*rneq
                derv2(kxc,iya) = derv2(kxc,iya) + yd1*xd2*d2(ind)*rneq
                derv2(kyc,iya) = derv2(kyc,iya) + yd1*yd2*d2(ind)*rneq
                derv2(kzc,iya) = derv2(kzc,iya) + yd1*zd2*d2(ind)*rneq
                derv2(kxc,iza) = derv2(kxc,iza) + zd1*xd2*d2(ind)*rneq
                derv2(kyc,iza) = derv2(kyc,iza) + zd1*yd2*d2(ind)*rneq
                derv2(kzc,iza) = derv2(kzc,iza) + zd1*zd2*d2(ind)*rneq
                if (n1.eq.n2) then
                  derv2(kxc,ixa) = derv2(kxc,ixa) + d1(n2)*rneq
                  derv2(kyc,iya) = derv2(kyc,iya) + d1(n2)*rneq
                  derv2(kzc,iza) = derv2(kzc,iza) + d1(n2)*rneq
                endif
              endif
              if (lopk.and.nauatom(n2k).gt.0.and..not.lsameijkl) then
                rneq = dble(neqvatom(nauatom(n2k)))
                if (lopi) then
                  ixc = ix
                  iyc = iy
                  izc = iz
                else
                  ixc = kx 
                  iyc = ky 
                  izc = kz
                endif
                derv2(ixc,kxa) = derv2(ixc,kxa) + xd2*xd1*d2(ind)*rneq
                derv2(iyc,kxa) = derv2(iyc,kxa) + xd2*yd1*d2(ind)*rneq
                derv2(izc,kxa) = derv2(izc,kxa) + xd2*zd1*d2(ind)*rneq
                derv2(ixc,kya) = derv2(ixc,kya) + yd2*xd1*d2(ind)*rneq
                derv2(iyc,kya) = derv2(iyc,kya) + yd2*yd1*d2(ind)*rneq
                derv2(izc,kya) = derv2(izc,kya) + yd2*zd1*d2(ind)*rneq
                derv2(ixc,kza) = derv2(ixc,kza) + zd2*xd1*d2(ind)*rneq
                derv2(iyc,kza) = derv2(iyc,kza) + zd2*yd1*d2(ind)*rneq
                derv2(izc,kza) = derv2(izc,kza) + zd2*zd1*d2(ind)*rneq
                if (n1.eq.n2) then
                  derv2(ixc,kxa) = derv2(ixc,kxa) + d1(n2)*rneq
                  derv2(iyc,kya) = derv2(iyc,kya) + d1(n2)*rneq
                  derv2(izc,kza) = derv2(izc,kza) + d1(n2)*rneq
                endif
              endif
            endif
!
!  Second derivatives : i - l
!
            if ((lopi.or.lopl).and.n1i.ne.n2l) then
              if (lopi.and.nauatom(n1i).gt.0) then
                rneq = dble(neqvatom(nauatom(n1i)))
                if (lopl) then
                  lxc = lx
                  lyc = ly
                  lzc = lz
                else
                  lxc = ix
                  lyc = iy
                  lzc = iz
                endif
                derv2(lxc,ixa) = derv2(lxc,ixa) - xd1*xd2*d2(ind)*rneq
                derv2(lyc,ixa) = derv2(lyc,ixa) - xd1*yd2*d2(ind)*rneq
                derv2(lzc,ixa) = derv2(lzc,ixa) - xd1*zd2*d2(ind)*rneq
                derv2(lxc,iya) = derv2(lxc,iya) - yd1*xd2*d2(ind)*rneq
                derv2(lyc,iya) = derv2(lyc,iya) - yd1*yd2*d2(ind)*rneq
                derv2(lzc,iya) = derv2(lzc,iya) - yd1*zd2*d2(ind)*rneq
                derv2(lxc,iza) = derv2(lxc,iza) - zd1*xd2*d2(ind)*rneq
                derv2(lyc,iza) = derv2(lyc,iza) - zd1*yd2*d2(ind)*rneq
                derv2(lzc,iza) = derv2(lzc,iza) - zd1*zd2*d2(ind)*rneq
                if (n1.eq.n2) then
                  derv2(lxc,ixa) = derv2(lxc,ixa) - d1(n2)*rneq
                  derv2(lyc,iya) = derv2(lyc,iya) - d1(n2)*rneq
                  derv2(lzc,iza) = derv2(lzc,iza) - d1(n2)*rneq
                endif
              endif
              if (lopl.and.nauatom(n2l).gt.0.and..not.lsameijkl) then
                rneq = dble(neqvatom(nauatom(n2l)))
                if (lopi) then
                  ixc = ix
                  iyc = iy
                  izc = iz
                else
                  ixc = lx 
                  iyc = ly 
                  izc = lz
                endif
                derv2(ixc,lxa) = derv2(ixc,lxa) - xd2*xd1*d2(ind)*rneq
                derv2(iyc,lxa) = derv2(iyc,lxa) - xd2*yd1*d2(ind)*rneq
                derv2(izc,lxa) = derv2(izc,lxa) - xd2*zd1*d2(ind)*rneq
                derv2(ixc,lya) = derv2(ixc,lya) - yd2*xd1*d2(ind)*rneq
                derv2(iyc,lya) = derv2(iyc,lya) - yd2*yd1*d2(ind)*rneq
                derv2(izc,lya) = derv2(izc,lya) - yd2*zd1*d2(ind)*rneq
                derv2(ixc,lza) = derv2(ixc,lza) - zd2*xd1*d2(ind)*rneq
                derv2(iyc,lza) = derv2(iyc,lza) - zd2*yd1*d2(ind)*rneq
                derv2(izc,lza) = derv2(izc,lza) - zd2*zd1*d2(ind)*rneq
                if (n1.eq.n2) then
                  derv2(ixc,lxa) = derv2(ixc,lxa) - d1(n2)*rneq
                  derv2(iyc,lya) = derv2(iyc,lya) - d1(n2)*rneq
                  derv2(izc,lza) = derv2(izc,lza) - d1(n2)*rneq
                endif
              endif
            endif
!
!  Second derivatives : j - k
!
            if ((lopj.or.lopk).and.n1j.ne.n2k) then
              if (lopj.and.nauatom(n1j).gt.0) then
                rneq = dble(neqvatom(nauatom(n1j)))
                if (lopk) then
                  kxc = kx
                  kyc = ky
                  kzc = kz
                else
                  kxc = jx
                  kyc = jy
                  kzc = jz
                endif
                derv2(kxc,jxa) = derv2(kxc,jxa) - xd1*xd2*d2(ind)*rneq
                derv2(kyc,jxa) = derv2(kyc,jxa) - xd1*yd2*d2(ind)*rneq
                derv2(kzc,jxa) = derv2(kzc,jxa) - xd1*zd2*d2(ind)*rneq
                derv2(kxc,jya) = derv2(kxc,jya) - yd1*xd2*d2(ind)*rneq
                derv2(kyc,jya) = derv2(kyc,jya) - yd1*yd2*d2(ind)*rneq
                derv2(kzc,jya) = derv2(kzc,jya) - yd1*zd2*d2(ind)*rneq
                derv2(kxc,jza) = derv2(kxc,jza) - zd1*xd2*d2(ind)*rneq
                derv2(kyc,jza) = derv2(kyc,jza) - zd1*yd2*d2(ind)*rneq
                derv2(kzc,jza) = derv2(kzc,jza) - zd1*zd2*d2(ind)*rneq
                if (n1.eq.n2) then
                  derv2(kxc,jxa) = derv2(kxc,jxa) - d1(n2)*rneq
                  derv2(kyc,jya) = derv2(kyc,jya) - d1(n2)*rneq
                  derv2(kzc,jza) = derv2(kzc,jza) - d1(n2)*rneq
                endif
              endif
              if (lopk.and.nauatom(n2k).gt.0.and..not.lsameijkl) then
                rneq = dble(neqvatom(nauatom(n2k)))
                if (lopj) then
                  jxc = jx
                  jyc = jy
                  jzc = jz
                else
                  jxc = kx 
                  jyc = ky 
                  jzc = kz
                endif
                derv2(jxc,kxa) = derv2(jxc,kxa) - xd2*xd1*d2(ind)*rneq
                derv2(jyc,kxa) = derv2(jyc,kxa) - xd2*yd1*d2(ind)*rneq
                derv2(jzc,kxa) = derv2(jzc,kxa) - xd2*zd1*d2(ind)*rneq
                derv2(jxc,kya) = derv2(jxc,kya) - yd2*xd1*d2(ind)*rneq
                derv2(jyc,kya) = derv2(jyc,kya) - yd2*yd1*d2(ind)*rneq
                derv2(jzc,kya) = derv2(jzc,kya) - yd2*zd1*d2(ind)*rneq
                derv2(jxc,kza) = derv2(jxc,kza) - zd2*xd1*d2(ind)*rneq
                derv2(jyc,kza) = derv2(jyc,kza) - zd2*yd1*d2(ind)*rneq
                derv2(jzc,kza) = derv2(jzc,kza) - zd2*zd1*d2(ind)*rneq
                if (n1.eq.n2) then
                  derv2(jxc,kxa) = derv2(jxc,kxa) - d1(n2)*rneq
                  derv2(jyc,kya) = derv2(jyc,kya) - d1(n2)*rneq
                  derv2(jzc,kza) = derv2(jzc,kza) - d1(n2)*rneq
                endif
              endif
            endif
!
!  Second derivatives : j - l
!
            if ((lopj.or.lopl).and.n1j.ne.n2l) then
              if (lopj.and.nauatom(n1j).gt.0) then
                rneq = dble(neqvatom(nauatom(n1j)))
                if (lopl) then
                  lxc = lx
                  lyc = ly
                  lzc = lz
                else
                  lxc = jx
                  lyc = jy
                  lzc = jz
                endif
                derv2(lxc,jxa) = derv2(lxc,jxa) + xd1*xd2*d2(ind)*rneq
                derv2(lyc,jxa) = derv2(lyc,jxa) + xd1*yd2*d2(ind)*rneq
                derv2(lzc,jxa) = derv2(lzc,jxa) + xd1*zd2*d2(ind)*rneq
                derv2(lxc,jya) = derv2(lxc,jya) + yd1*xd2*d2(ind)*rneq
                derv2(lyc,jya) = derv2(lyc,jya) + yd1*yd2*d2(ind)*rneq
                derv2(lzc,jya) = derv2(lzc,jya) + yd1*zd2*d2(ind)*rneq
                derv2(lxc,jza) = derv2(lxc,jza) + zd1*xd2*d2(ind)*rneq
                derv2(lyc,jza) = derv2(lyc,jza) + zd1*yd2*d2(ind)*rneq
                derv2(lzc,jza) = derv2(lzc,jza) + zd1*zd2*d2(ind)*rneq
                if (n1.eq.n2) then
                  derv2(lxc,jxa) = derv2(lxc,jxa) + d1(n2)*rneq
                  derv2(lyc,jya) = derv2(lyc,jya) + d1(n2)*rneq
                  derv2(lzc,jza) = derv2(lzc,jza) + d1(n2)*rneq
                endif
              endif
              if (lopl.and.nauatom(n2l).gt.0.and..not.lsameijkl) then
                rneq = dble(neqvatom(nauatom(n2l)))
                if (lopj) then
                  jxc = jx
                  jyc = jy
                  jzc = jz
                else
                  jxc = lx 
                  jyc = ly 
                  jzc = lz
                endif
                derv2(jxc,lxa) = derv2(jxc,lxa) + xd2*xd1*d2(ind)*rneq
                derv2(jyc,lxa) = derv2(jyc,lxa) + xd2*yd1*d2(ind)*rneq
                derv2(jzc,lxa) = derv2(jzc,lxa) + xd2*zd1*d2(ind)*rneq
                derv2(jxc,lya) = derv2(jxc,lya) + yd2*xd1*d2(ind)*rneq
                derv2(jyc,lya) = derv2(jyc,lya) + yd2*yd1*d2(ind)*rneq
                derv2(jzc,lya) = derv2(jzc,lya) + yd2*zd1*d2(ind)*rneq
                derv2(jxc,lza) = derv2(jxc,lza) + zd2*xd1*d2(ind)*rneq
                derv2(jyc,lza) = derv2(jyc,lza) + zd2*yd1*d2(ind)*rneq
                derv2(jzc,lza) = derv2(jzc,lza) + zd2*zd1*d2(ind)*rneq
                if (n1.eq.n2) then
                  derv2(jxc,lxa) = derv2(jxc,lxa) + d1(n2)*rneq
                  derv2(jyc,lya) = derv2(jyc,lya) + d1(n2)*rneq
                  derv2(jzc,lza) = derv2(jzc,lza) + d1(n2)*rneq
                endif
              endif
            endif
            if (lstr) then
!
!  Strain - strain second derivatives
!
              do ns1 = 1,nstrains
                do ns2 = 1,nstrains
                  sderv2(ns2,ns1) = sderv2(ns2,ns1) + d2(ind)*rp2(ns2)*rp1(ns1)
                  if (n1.ne.n2) then
                    sderv2(ns2,ns1) = sderv2(ns2,ns1) + d2(ind)*rp1(ns2)*rp2(ns1)
                  endif
                enddo
              enddo
!
!  Internal - strain second derivatives
!
              if (lopi.and.nauatom(n1i).gt.0) then
                rneq = dble(neqvatom(nauatom(n1i)))
                do ns1 = 1,nstrains
                  derv3(ixa,ns1) = derv3(ixa,ns1) - xd1*d2(ind)*rp2(ns1)*rneq
                  derv3(iya,ns1) = derv3(iya,ns1) - yd1*d2(ind)*rp2(ns1)*rneq
                  derv3(iza,ns1) = derv3(iza,ns1) - zd1*d2(ind)*rp2(ns1)*rneq
                enddo
              endif
              if (lopj.and.nauatom(n1j).gt.0) then
                rneq = dble(neqvatom(nauatom(n1j)))
                do ns1 = 1,nstrains
                  derv3(jxa,ns1) = derv3(jxa,ns1) + xd1*d2(ind)*rp2(ns1)*rneq
                  derv3(jya,ns1) = derv3(jya,ns1) + yd1*d2(ind)*rp2(ns1)*rneq
                  derv3(jza,ns1) = derv3(jza,ns1) + zd1*d2(ind)*rp2(ns1)*rneq
                enddo
              endif
              if (n1.ne.n2) then
                if (lopk.and.nauatom(n2k).gt.0) then
                  rneq = dble(neqvatom(nauatom(n2k)))
                  do ns1 = 1,nstrains
                    derv3(kxa,ns1) = derv3(kxa,ns1) - xd2*d2(ind)*rp1(ns1)*rneq
                    derv3(kya,ns1) = derv3(kya,ns1) - yd2*d2(ind)*rp1(ns1)*rneq
                    derv3(kza,ns1) = derv3(kza,ns1) - zd2*d2(ind)*rp1(ns1)*rneq
                  enddo
                endif
                if (lopl.and.nauatom(n2l).gt.0) then
                  rneq = dble(neqvatom(nauatom(n2l)))
                  do ns1 = 1,nstrains
                    derv3(lxa,ns1) = derv3(lxa,ns1) + xd2*d2(ind)*rp1(ns1)*rneq
                    derv3(lya,ns1) = derv3(lya,ns1) + yd2*d2(ind)*rp1(ns1)*rneq
                    derv3(lza,ns1) = derv3(lza,ns1) + zd2*d2(ind)*rp1(ns1)*rneq
                  enddo
                endif
              endif
            endif
          endif
        endif
      enddo
    else
!
!  Increment ind pointer by number of missed terms
!
      ind = ind + n1
    endif
  enddo
!
  return
  end
!
  subroutine d2addd(i,maxneigh,nneigh,neighno,xneigh,yneigh,zneigh,nfreeatom,nREBOatomRptr, &
                    nregion1,d1,d2,ltorderv)
!
!  Utility to add the pairwise derivatives for a group of atoms
!  on to the global Cartesian second derivative array. Defect
!  calculation version.
!
!  On entry : 
!
!  i               = atom number of i to whom others are connected
!  maxneigh        = left-hand dimension of 2-D arrays
!  nneigh          = no. of neighbours of atoms
!  neighno         = atom number of each neighbour
!  xneigh          = x component of vector to neighbour of atoms
!  yneigh          = y component of vector to neighbour of atoms
!  zneigh          = z component of vector to neighbour of atoms
!  nfreeatom       = pointer from atom to free atom to move
!  nREBOatomRptr   = pointer from atoms to position in REBO set
!  nregion1        = number of ions in region 1
!  d1              = array of first derivatives w.r.t. interatomic
!                    distances
!  d2              = array of first derivatives w.r.t. interatomic
!                    distances
!  ltorderv        = if true then include torsional derivatives
!
!   9/02 Created from d2add
!  12/03 Torsional derivatives made optional
!   6/07 nREBOatomRptr added as input
!  10/07 Correction made to setting of n1i/n2k for torsional case
!  11/07 Unused variables removed
!   1/10 Bug fixed for neighbour of i -> neighbour of neighbour of i case
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
!  Copyright Curtin University 2010
!
!  Julian Gale, NRI, Curtin University, January 2010
!
  use brennerdata
  use derivatives
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)             :: i
  integer(i4), intent(in)             :: maxneigh
  integer(i4), intent(in)             :: nneigh(*)
  integer(i4), intent(in)             :: neighno(maxneigh,*)
  real(dp),    intent(in)             :: xneigh(maxneigh,*)
  real(dp),    intent(in)             :: yneigh(maxneigh,*)
  real(dp),    intent(in)             :: zneigh(maxneigh,*)
  integer(i4), intent(in)             :: nfreeatom(*)
  integer(i4), intent(in)             :: nREBOatomRptr(*)
  integer(i4), intent(in)             :: nregion1
  real(dp),    intent(in)             :: d1(*)
  real(dp),    intent(in)             :: d2(*)
  logical,     intent(in)             :: ltorderv
!
!  Local variables
!
  integer(i4)                         :: ind
  integer(i4)                         :: ix,iy,iz
  integer(i4)                         :: jx,jy,jz
  integer(i4)                         :: kx,ky,kz
  integer(i4)                         :: lx,ly,lz
  integer(i4)                         :: n1
  integer(i4)                         :: n1i
  integer(i4)                         :: n1j
  integer(i4)                         :: n1j2
  integer(i4)                         :: n2
  integer(i4)                         :: n2k
  integer(i4)                         :: n2l
  integer(i4)                         :: n2l2
  integer(i4)                         :: ndist
  integer(i4)                         :: nji
  integer(i4)                         :: nneigh2
  integer(i4)                         :: nri
  integer(i4)                         :: ntmp
  real(dp)                            :: r2ik
  real(dp)                            :: r2jl
  real(dp)                            :: xd1
  real(dp)                            :: yd1
  real(dp)                            :: zd1
  real(dp)                            :: xd2
  real(dp)                            :: yd2
  real(dp)                            :: zd2
  real(dp)                            :: xi
  real(dp)                            :: yi
  real(dp)                            :: zi
  real(dp)                            :: xik
  real(dp)                            :: yik
  real(dp)                            :: zik
  real(dp)                            :: xjk
  real(dp)                            :: yjk
  real(dp)                            :: zjk
  real(dp)                            :: xjl
  real(dp)                            :: yjl
  real(dp)                            :: zjl
  real(dp)                            :: x2i
  real(dp)                            :: y2i  
  real(dp)                            :: z2i
  logical                             :: lsameijkl
  logical                             :: liok
  logical                             :: ljok
  logical                             :: lkok
  logical                             :: llok
  logical                             :: lopi
  logical                             :: lopj
  logical                             :: lopk
  logical                             :: lopl
!
!  Calculate total number of distances
!
  nri = nREBOatomRptr(i)
  nneigh2 = nneigh(nri) + nneigh(nri)*(nneigh(nri) + 1)/2
  if (ltorderv) then
    ndist = nneigh2 + nneigh(nri)*(maxneigh + 1)*maxneigh
  else
    ndist = nneigh2
  endif
!
!  Loop over pairs of interatomic distances
!
  ind = 0
  do n1 = 1,ndist
!******************************
!  Find pair of atoms for n1  *
!******************************
    if (n1.le.nneigh(nri)) then
!
!  i -> neighbours of i
!
      n1i = i
      n1j = neighno(n1,nri)
      xd1 = xneigh(n1,nri)
      yd1 = yneigh(n1,nri)
      zd1 = zneigh(n1,nri)
      x2i = 0.0_dp
      y2i = 0.0_dp
      z2i = 0.0_dp
      liok = .true.
      ljok = .true.
    elseif (n1.le.nneigh2) then
!
!  Neighbours of i -> neighbours of i
!
      ntmp = n1 - nneigh(nri)
      n1j = 1
      n1j2 = 1
      do while (n1j2.lt.ntmp)
        n1j = n1j + 1
        n1j2 = n1j2 + n1j
      enddo
      n1j2 = n1j2 - n1j
      n1i = ntmp - n1j2
!
!  Check that neighbour numbers are valid
!
      liok = (n1i.le.nneigh(nri))
      ljok = (n1j.le.nneigh(nri))
      if (liok.and.ljok) then
!
!  Get vector between atoms
!
        xd1 = (xneigh(n1j,nri) - xneigh(n1i,nri))
        yd1 = (yneigh(n1j,nri) - yneigh(n1i,nri))
        zd1 = (zneigh(n1j,nri) - zneigh(n1i,nri))
!  
        x2i = xneigh(n1i,nri)
        y2i = yneigh(n1i,nri)
        z2i = zneigh(n1i,nri)
!
!  Convert neighbour numbers to real atoms
!
        n1i = neighno(n1i,nri)
        n1j = neighno(n1j,nri)
      endif
    else
!
!  Neighbours of neighbours of i -> neighbours of i
!
      ntmp = n1 - nneigh2
      nji = (ntmp - 1)/((maxneigh + 1)*maxneigh) + 1
      ntmp = ntmp - (nji - 1)*(maxneigh + 1)*maxneigh
      n1i = (ntmp - 1)/maxneigh
      n1j = ntmp - n1i*maxneigh
!
!  Check that atoms are valid
!
      liok = (n1i.le.nneigh(nri))
      if (nji.le.nneigh(nri)) then
        ljok = (n1j.le.nneigh(neighno(nji,nri)))
      else
        ljok = .false.
      endif
!
      if (liok.and.ljok) then
        if (n1i.eq.0) then
          xi = 0.0_dp
          yi = 0.0_dp
          zi = 0.0_dp
          n1i = i
        else
          xi = xneigh(n1i,nri)
          yi = yneigh(n1i,nri)
          zi = zneigh(n1i,nri)
          n1i = neighno(n1i,nri)
        endif
!
        xd1 = (xneigh(n1j,nREBOatomRptr(neighno(nji,nri))) + xneigh(nji,nri) - xi)
        yd1 = (yneigh(n1j,nREBOatomRptr(neighno(nji,nri))) + yneigh(nji,nri) - yi)
        zd1 = (zneigh(n1j,nREBOatomRptr(neighno(nji,nri))) + zneigh(nji,nri) - zi)
!         
        x2i = xi
        y2i = yi
        z2i = zi
!
        n1j = neighno(n1j,nREBOatomRptr(neighno(nji,nri)))
      endif
    endif
!
!  Skip the following section as it is irrelevant if i and j are not valid
!
    if (liok.and.ljok) then
!
!  Set second derivative location pointers
!
      lopi = (nfreeatom(n1i).gt.0)
      lopj = (nfreeatom(n1j).gt.0)
      if (n1i.le.nregion1) then
        ix = 3*(nfreeatom(n1i) - 1) + 1
        iy = ix + 1
        iz = iy + 1
      else
        ix = 3*nregion1 + 1
        iy = ix + 1
        iz = iy + 1
      endif
      if (n1j.le.nregion1) then
        jx = 3*(nfreeatom(n1j) - 1) + 1
        jy = jx + 1
        jz = jy + 1
      else
        jx = 3*nregion1 + 1
        jy = jx + 1
        jz = jy + 1
      endif
    endif
!
!  If neither i nor j are being optimised then atoms are not valid
!
    if (.not.lopi.and..not.lopj) then
      liok = .false.
      ljok = .false.
    endif
!
!  If i and j are not valid then there is no point continuing for this value of n1
!
    if (liok.and.ljok) then
      do n2 = 1,n1
        ind = ind + 1
        if (d2(ind).ne.0.0_dp.or.(n1.eq.n2.and.d1(n2).ne.0.0_dp)) then
!******************************
!  Find pair of atoms for n2  *
!******************************
          if (n2.le.nneigh(nri)) then
!
!  i -> neighbours of i
!
            n2k = i
            n2l = neighno(n2,nri)
            xd2 = xneigh(n2,nri)
            yd2 = yneigh(n2,nri)
            zd2 = zneigh(n2,nri)
!
            xik = - x2i
            yik = - y2i
            zik = - z2i
!
            lkok = .true.
            llok = .true.
          elseif (n2.le.nneigh2) then
!
!  Neighbours of i -> neighbours of i
!
            ntmp = n2 - nneigh(nri)
            n2l = 1
            n2l2 = 1
            do while (n2l2.lt.ntmp)
              n2l = n2l + 1
              n2l2 = n2l2 + n2l
            enddo
            n2l2 = n2l2 - n2l
            n2k = ntmp - n2l2
!
!  Check that atoms are valid
!
            lkok = (n2k.le.nneigh(nri))
            llok = (n2l.le.nneigh(nri))
            if (lkok.and.llok) then
!
!  Calculate vector between atoms
!
              xd2 = (xneigh(n2l,nri) - xneigh(n2k,nri))
              yd2 = (yneigh(n2l,nri) - yneigh(n2k,nri))
              zd2 = (zneigh(n2l,nri) - zneigh(n2k,nri))
!  
              xik = xneigh(n2k,nri) - x2i
              yik = yneigh(n2k,nri) - y2i
              zik = zneigh(n2k,nri) - z2i
!
!  Convert neighbour numbers to real atoms
!
              n2k = neighno(n2k,nri)
              n2l = neighno(n2l,nri)
            endif
          else
!
!  Neighbours of neighbours of i -> neighbours of i
!
            ntmp = n2 - nneigh2
            nji = (ntmp - 1)/((maxneigh + 1)*maxneigh) + 1
            ntmp = ntmp - (nji - 1)*(maxneigh + 1)*maxneigh
            n2k = (ntmp - 1)/maxneigh
            n2l = ntmp - n2k*maxneigh
!
!  Check that atoms are valid
!
            lkok = (n2k.le.nneigh(nri))
            if (nji.le.nneigh(nri)) then
              llok = (n2l.le.nneigh(nREBOatomRptr(neighno(nji,nri))))
            else
              llok = .false.
            endif
!
            if (lkok.and.llok) then
              if (n2k.eq.0) then
                xi = 0.0_dp
                yi = 0.0_dp
                zi = 0.0_dp
                n2k = i
              else
                xi = xneigh(n2k,nri)
                yi = yneigh(n2k,nri)
                zi = zneigh(n2k,nri)
                n2k = neighno(n2k,nri)
              endif
!         
              xd2 = (xneigh(n2l,nREBOatomRptr(neighno(nji,nri))) + xneigh(nji,nri) - xi)
              yd2 = (yneigh(n2l,nREBOatomRptr(neighno(nji,nri))) + yneigh(nji,nri) - yi)
              zd2 = (zneigh(n2l,nREBOatomRptr(neighno(nji,nri))) + zneigh(nji,nri) - zi)
!               
              xik = xi - x2i
              yik = yi - y2i
              zik = zi - z2i
!       
              n2l = neighno(n2l,nREBOatomRptr(neighno(nji,nri)))
            endif
          endif
!
!  No point continuing beyond here unless k and l are valid
!
          if (lkok.and.llok) then
!
!  Complete remaining vectors
!
            xjk = xik - xd1
            yjk = yik - yd1
            zjk = zik - zd1
            xjl = xjk + xd2
            yjl = yjk + yd2
            zjl = zjk + zd2
!
!  Compute distances squared
!
            r2ik = xik*xik + yik*yik + zik*zik
            r2jl = xjl*xjl + yjl*yjl + zjl*zjl
!
            lopk = (nfreeatom(n2k).gt.0)
            lopl = (nfreeatom(n2l).gt.0)
            if (n2k.le.nregion1) then
              kx = 3*(nfreeatom(n2k) - 1) + 1
              ky = kx + 1
              kz = ky + 1
            else
              kx = 3*nregion1 + 1
              ky = kx + 1
              kz = ky + 1
            endif
            if (n2l.le.nregion1) then
              lx = 3*(nfreeatom(n2l) - 1) + 1
              ly = lx + 1
              lz = ly + 1
            else
              lx = 3*nregion1 + 1
              ly = lx + 1
              lz = ly + 1
            endif
!
            lsameijkl = .false.
            if (n1i.eq.n2k.and.n1j.eq.n2l.and.r2ik.lt.1.0d-8.and.r2jl.lt.1.0d-8) lsameijkl = .true.
!
            if ((lopi.or.lopk).and.n1i.ne.n2k) then
!
!  Second derivatives : i - k
!
              derv2(kx,ix) = derv2(kx,ix) + xd1*xd2*d2(ind)
              derv2(ky,ix) = derv2(ky,ix) + xd1*yd2*d2(ind)
              derv2(kz,ix) = derv2(kz,ix) + xd1*zd2*d2(ind)
              derv2(kx,iy) = derv2(kx,iy) + yd1*xd2*d2(ind)
              derv2(ky,iy) = derv2(ky,iy) + yd1*yd2*d2(ind)
              derv2(kz,iy) = derv2(kz,iy) + yd1*zd2*d2(ind)
              derv2(kx,iz) = derv2(kx,iz) + zd1*xd2*d2(ind)
              derv2(ky,iz) = derv2(ky,iz) + zd1*yd2*d2(ind)
              derv2(kz,iz) = derv2(kz,iz) + zd1*zd2*d2(ind)
              if (n1.eq.n2) then
                derv2(kx,ix) = derv2(kx,ix) + d1(n2)
                derv2(ky,iy) = derv2(ky,iy) + d1(n2)
                derv2(kz,iz) = derv2(kz,iz) + d1(n2)
              endif
              if (.not.lsameijkl) then
                derv2(ix,kx) = derv2(ix,kx) + xd1*xd2*d2(ind)
                derv2(ix,ky) = derv2(ix,ky) + xd1*yd2*d2(ind)
                derv2(ix,kz) = derv2(ix,kz) + xd1*zd2*d2(ind)
                derv2(iy,kx) = derv2(iy,kx) + yd1*xd2*d2(ind)
                derv2(iy,ky) = derv2(iy,ky) + yd1*yd2*d2(ind)
                derv2(iy,kz) = derv2(iy,kz) + yd1*zd2*d2(ind)
                derv2(iz,kx) = derv2(iz,kx) + zd1*xd2*d2(ind)
                derv2(iz,ky) = derv2(iz,ky) + zd1*yd2*d2(ind)
                derv2(iz,kz) = derv2(iz,kz) + zd1*zd2*d2(ind)
                if (n1.eq.n2) then
                  derv2(ix,kx) = derv2(ix,kx) + d1(n2)
                  derv2(iy,ky) = derv2(iy,ky) + d1(n2)
                  derv2(iz,kz) = derv2(iz,kz) + d1(n2)
                endif
              endif
            endif
!
!  Second derivatives : i - l
!
            if ((lopi.or.lopl).and.n1i.ne.n2l) then
              derv2(lx,ix) = derv2(lx,ix) - xd1*xd2*d2(ind)
              derv2(ly,ix) = derv2(ly,ix) - xd1*yd2*d2(ind)
              derv2(lz,ix) = derv2(lz,ix) - xd1*zd2*d2(ind)
              derv2(lx,iy) = derv2(lx,iy) - yd1*xd2*d2(ind)
              derv2(ly,iy) = derv2(ly,iy) - yd1*yd2*d2(ind)
              derv2(lz,iy) = derv2(lz,iy) - yd1*zd2*d2(ind)
              derv2(lx,iz) = derv2(lx,iz) - zd1*xd2*d2(ind)
              derv2(ly,iz) = derv2(ly,iz) - zd1*yd2*d2(ind)
              derv2(lz,iz) = derv2(lz,iz) - zd1*zd2*d2(ind)
              if (n1.eq.n2) then
                derv2(lx,ix) = derv2(lx,ix) - d1(n2)
                derv2(ly,iy) = derv2(ly,iy) - d1(n2)
                derv2(lz,iz) = derv2(lz,iz) - d1(n2)
              endif
              if (.not.lsameijkl) then
                derv2(ix,lx) = derv2(ix,lx) - xd1*xd2*d2(ind)
                derv2(ix,ly) = derv2(ix,ly) - xd1*yd2*d2(ind)
                derv2(ix,lz) = derv2(ix,lz) - xd1*zd2*d2(ind)
                derv2(iy,lx) = derv2(iy,lx) - yd1*xd2*d2(ind)
                derv2(iy,ly) = derv2(iy,ly) - yd1*yd2*d2(ind)
                derv2(iy,lz) = derv2(iy,lz) - yd1*zd2*d2(ind)
                derv2(iz,lx) = derv2(iz,lx) - zd1*xd2*d2(ind)
                derv2(iz,ly) = derv2(iz,ly) - zd1*yd2*d2(ind)
                derv2(iz,lz) = derv2(iz,lz) - zd1*zd2*d2(ind)
                if (n1.eq.n2) then
                  derv2(ix,lx) = derv2(ix,lx) - d1(n2)
                  derv2(iy,ly) = derv2(iy,ly) - d1(n2)
                  derv2(iz,lz) = derv2(iz,lz) - d1(n2)
                endif
              endif
            endif
!
!  Second derivatives : j - k
!
            if ((lopj.or.lopk).and.n1j.ne.n2k) then
              derv2(kx,jx) = derv2(kx,jx) - xd1*xd2*d2(ind)
              derv2(ky,jx) = derv2(ky,jx) - xd1*yd2*d2(ind)
              derv2(kz,jx) = derv2(kz,jx) - xd1*zd2*d2(ind)
              derv2(kx,jy) = derv2(kx,jy) - yd1*xd2*d2(ind)
              derv2(ky,jy) = derv2(ky,jy) - yd1*yd2*d2(ind)
              derv2(kz,jy) = derv2(kz,jy) - yd1*zd2*d2(ind)
              derv2(kx,jz) = derv2(kx,jz) - zd1*xd2*d2(ind)
              derv2(ky,jz) = derv2(ky,jz) - zd1*yd2*d2(ind)
              derv2(kz,jz) = derv2(kz,jz) - zd1*zd2*d2(ind)
              if (n1.eq.n2) then
                derv2(kx,jx) = derv2(kx,jx) - d1(n2)
                derv2(ky,jy) = derv2(ky,jy) - d1(n2)
                derv2(kz,jz) = derv2(kz,jz) - d1(n2)
              endif
              if (.not.lsameijkl) then
                derv2(jx,kx) = derv2(jx,kx) - xd1*xd2*d2(ind)
                derv2(jx,ky) = derv2(jx,ky) - xd1*yd2*d2(ind)
                derv2(jx,kz) = derv2(jx,kz) - xd1*zd2*d2(ind)
                derv2(jy,kx) = derv2(jy,kx) - yd1*xd2*d2(ind)
                derv2(jy,ky) = derv2(jy,ky) - yd1*yd2*d2(ind)
                derv2(jy,kz) = derv2(jy,kz) - yd1*zd2*d2(ind)
                derv2(jz,kx) = derv2(jz,kx) - zd1*xd2*d2(ind)
                derv2(jz,ky) = derv2(jz,ky) - zd1*yd2*d2(ind)
                derv2(jz,kz) = derv2(jz,kz) - zd1*zd2*d2(ind)
                if (n1.eq.n2) then
                  derv2(jx,kx) = derv2(jx,kx) - d1(n2)
                  derv2(jy,ky) = derv2(jy,ky) - d1(n2)
                  derv2(jz,kz) = derv2(jz,kz) - d1(n2)
                endif
              endif
            endif
!
!  Second derivatives : j - l
!
            if ((lopj.or.lopl).and.n1j.ne.n2l) then
              derv2(lx,jx) = derv2(lx,jx) + xd1*xd2*d2(ind)
              derv2(ly,jx) = derv2(ly,jx) + xd1*yd2*d2(ind)
              derv2(lz,jx) = derv2(lz,jx) + xd1*zd2*d2(ind)
              derv2(lx,jy) = derv2(lx,jy) + yd1*xd2*d2(ind)
              derv2(ly,jy) = derv2(ly,jy) + yd1*yd2*d2(ind)
              derv2(lz,jy) = derv2(lz,jy) + yd1*zd2*d2(ind)
              derv2(lx,jz) = derv2(lx,jz) + zd1*xd2*d2(ind)
              derv2(ly,jz) = derv2(ly,jz) + zd1*yd2*d2(ind)
              derv2(lz,jz) = derv2(lz,jz) + zd1*zd2*d2(ind)
              if (n1.eq.n2) then
                derv2(lx,jx) = derv2(lx,jx) + d1(n2)
                derv2(ly,jy) = derv2(ly,jy) + d1(n2)
                derv2(lz,jz) = derv2(lz,jz) + d1(n2)
              endif
              if (.not.lsameijkl) then
                derv2(jx,lx) = derv2(jx,lx) + xd1*xd2*d2(ind)
                derv2(jx,ly) = derv2(jx,ly) + xd1*yd2*d2(ind)
                derv2(jx,lz) = derv2(jx,lz) + xd1*zd2*d2(ind)
                derv2(jy,lx) = derv2(jy,lx) + yd1*xd2*d2(ind)
                derv2(jy,ly) = derv2(jy,ly) + yd1*yd2*d2(ind)
                derv2(jy,lz) = derv2(jy,lz) + yd1*zd2*d2(ind)
                derv2(jz,lx) = derv2(jz,lx) + zd1*xd2*d2(ind)
                derv2(jz,ly) = derv2(jz,ly) + zd1*yd2*d2(ind)
                derv2(jz,lz) = derv2(jz,lz) + zd1*zd2*d2(ind)
                if (n1.eq.n2) then
                  derv2(jx,lx) = derv2(jx,lx) + d1(n2)
                  derv2(jy,ly) = derv2(jy,ly) + d1(n2)
                  derv2(jz,lz) = derv2(jz,lz) + d1(n2)
                endif
              endif
            endif
          endif
        endif
      enddo
    else
!
!  Increment ind pointer by number of missed terms
!
      ind = ind + n1
    endif
  enddo
!
  return
  end
!
  subroutine d2addds(i,maxneigh,nneigh,neighno,xneigh,yneigh,zneigh,nfreeatom,nfreeatomau,nREBOatomRptr, &
                     nauatom,neqvatom,nregion1,d1,d2,ltorderv)
!
!  Utility to add the pairwise derivatives for a group of atoms
!  on to the global Cartesian second derivative array. Defect
!  calculation version
!
!  Symmetry adapted version of d2addd
!
!  On entry : 
!
!  i               = atom number of i to whom others are connected
!  maxneigh        = left-hand dimension of 2-D arrays
!  nneigh          = no. of neighbours of atoms
!  neighno         = atom number of each neighbour
!  xneigh          = x component of vector to neighbour of atoms
!  yneigh          = y component of vector to neighbour of atoms
!  zneigh          = z component of vector to neighbour of atoms
!  nfreeatom       = pointer from atom to free atom to move in full cell
!  nfreeatomau     = pointer from atom to free atom to move in asymmetric unit
!  nREBOatomRptr   = pointer from atoms to position in REBO set
!  nauatom         = pointer from atom to asymmetric unit atom
!  neqvatom        = number of equivalent positions for asymmetric unit atom
!  nregion1        = number of atoms in region 1
!  d1              = array of first derivatives w.r.t. interatomic
!                    distances
!  d2              = array of first derivatives w.r.t. interatomic
!                    distances
!  ltorderv        = if true then include torsional derivatives
!
!   9/02 Created from d2adds
!  12/03 Torsional derivatives made optional
!   6/07 nREBOatomRptr added as input
!  10/07 Correction made to setting of n1i/n2k for torsional case
!  11/07 Unused variables removed
!   1/10 Bug fixed for neighbour of i -> neighbour of neighbour of i case
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
!  Copyright Curtin University 2010
!
!  Julian Gale, NRI, Curtin University, January 2010
!
  use brennerdata
  use derivatives
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)             :: i
  integer(i4), intent(in)             :: maxneigh
  integer(i4), intent(in)             :: nneigh(*)
  integer(i4), intent(in)             :: neighno(maxneigh,*)
  real(dp),    intent(in)             :: xneigh(maxneigh,*)
  real(dp),    intent(in)             :: yneigh(maxneigh,*)
  real(dp),    intent(in)             :: zneigh(maxneigh,*)
  integer(i4), intent(in)             :: nfreeatom(*)
  integer(i4), intent(in)             :: nfreeatomau(*)
  integer(i4), intent(in)             :: nREBOatomRptr(*)
  integer(i4), intent(in)             :: nauatom(*)
  integer(i4), intent(in)             :: neqvatom(*)
  integer(i4), intent(in)             :: nregion1
  real(dp),    intent(in)             :: d1(*)
  real(dp),    intent(in)             :: d2(*)
  logical,     intent(in)             :: ltorderv
!
!  Local variables
!
  integer(i4)                         :: ind
  integer(i4)                         :: ix,iy,iz
  integer(i4)                         :: jx,jy,jz
  integer(i4)                         :: kx,ky,kz
  integer(i4)                         :: lx,ly,lz
  integer(i4)                         :: ixa,iya,iza
  integer(i4)                         :: jxa,jya,jza
  integer(i4)                         :: kxa,kya,kza
  integer(i4)                         :: lxa,lya,lza
  integer(i4)                         :: n1
  integer(i4)                         :: n1i
  integer(i4)                         :: n1j
  integer(i4)                         :: n1j2
  integer(i4)                         :: n2
  integer(i4)                         :: n2k
  integer(i4)                         :: n2l
  integer(i4)                         :: n2l2
  integer(i4)                         :: ndist
  integer(i4)                         :: nji
  integer(i4)                         :: nri
  integer(i4)                         :: nneigh2
  integer(i4)                         :: ntmp
  real(dp)                            :: r2ik
  real(dp)                            :: r2jl
  real(dp)                            :: rneq
  real(dp)                            :: xd1
  real(dp)                            :: yd1
  real(dp)                            :: zd1
  real(dp)                            :: xd2
  real(dp)                            :: yd2
  real(dp)                            :: zd2
  real(dp)                            :: xi
  real(dp)                            :: yi
  real(dp)                            :: zi
  real(dp)                            :: xik
  real(dp)                            :: yik
  real(dp)                            :: zik
  real(dp)                            :: xjk
  real(dp)                            :: yjk
  real(dp)                            :: zjk
  real(dp)                            :: xjl
  real(dp)                            :: yjl
  real(dp)                            :: zjl
  real(dp)                            :: x2i
  real(dp)                            :: y2i  
  real(dp)                            :: z2i
  logical                             :: lsameijkl
  logical                             :: liok
  logical                             :: ljok
  logical                             :: lkok
  logical                             :: llok
  logical                             :: lopi
  logical                             :: lopj
  logical                             :: lopk
  logical                             :: lopl
!
!  Calculate total number of distances
!
  nri = nREBOatomRptr(i)
  nneigh2 = nneigh(nri) + nneigh(nri)*(nneigh(nri) + 1)/2
  if (ltorderv) then
    ndist = nneigh2 + nneigh(nri)*(maxneigh + 1)*maxneigh
  else
    ndist = nneigh2
  endif
!
!  Loop over pairs of interatomic distances
!
  ind = 0
  do n1 = 1,ndist
!******************************
!  Find pair of atoms for n1  *
!******************************
    if (n1.le.nneigh(nri)) then
!
!  i -> neighbours of i
!
      n1i = i
      n1j = neighno(n1,nri)
      xd1 = xneigh(n1,nri)
      yd1 = yneigh(n1,nri)
      zd1 = zneigh(n1,nri)
      x2i = 0.0_dp
      y2i = 0.0_dp
      z2i = 0.0_dp
      liok = .true.
      ljok = .true.
    elseif (n1.le.nneigh2) then
!
!  Neighbours of i -> neighbours of i
!
      ntmp = n1 - nneigh(nri)
      n1j = 1
      n1j2 = 1
      do while (n1j2.lt.ntmp)
        n1j = n1j + 1
        n1j2 = n1j2 + n1j
      enddo
      n1j2 = n1j2 - n1j
      n1i = ntmp - n1j2
!
!  Check that neighbour numbers are valid
!
      liok = (n1i.le.nneigh(nri))
      ljok = (n1j.le.nneigh(nri))
      if (liok.and.ljok) then
!
!  Get vector between atoms
!
        xd1 = (xneigh(n1j,nri) - xneigh(n1i,nri))
        yd1 = (yneigh(n1j,nri) - yneigh(n1i,nri))
        zd1 = (zneigh(n1j,nri) - zneigh(n1i,nri))
!  
        x2i = xneigh(n1i,nri)
        y2i = yneigh(n1i,nri)
        z2i = zneigh(n1i,nri)
!
!  Convert neighbour numbers to real atoms
!
        n1i = neighno(n1i,nri)
        n1j = neighno(n1j,nri)
      endif
    else
!
!  Neighbours of neighbours of i -> neighbours of i
!
      ntmp = n1 - nneigh2
      nji = (ntmp - 1)/((maxneigh + 1)*maxneigh) + 1
      ntmp = ntmp - (nji - 1)*(maxneigh + 1)*maxneigh
      n1i = (ntmp - 1)/maxneigh
      n1j = ntmp - n1i*maxneigh
!
!  Check that atoms are valid
!
      liok = (n1i.le.nneigh(nri))
      if (nji.le.nneigh(nri)) then
        ljok = (n1j.le.nneigh(nREBOatomRptr(neighno(nji,nri))))
      else
        ljok = .false.
      endif
!
      if (liok.and.ljok) then
        if (n1i.eq.0) then
          xi = 0.0_dp
          yi = 0.0_dp
          zi = 0.0_dp
          n1i = i
        else
          xi = xneigh(n1i,nri)
          yi = yneigh(n1i,nri)
          zi = zneigh(n1i,nri)
          n1i = neighno(n1i,nri)
        endif
!
        xd1 = (xneigh(n1j,nREBOatomRptr(neighno(nji,nri))) + xneigh(nji,nri) - xi)
        yd1 = (yneigh(n1j,nREBOatomRptr(neighno(nji,nri))) + yneigh(nji,nri) - yi)
        zd1 = (zneigh(n1j,nREBOatomRptr(neighno(nji,nri))) + zneigh(nji,nri) - zi)
!         
        x2i = xi
        y2i = yi
        z2i = zi
!
        n1j = neighno(n1j,nREBOatomRptr(neighno(nji,nri)))
      endif
    endif
!
!  Skip the following section as it is irrelevant if i and j are not valid
!
    if (liok.and.ljok) then
!
!  Set second derivative location pointers
!
      lopi = (nfreeatom(n1i).gt.0)
      lopj = (nfreeatom(n1j).gt.0)
      if (n1i.le.nregion1) then
        ix = 3*(nfreeatom(n1i) - 1) + 1
        iy = ix + 1
        iz = iy + 1
      else
        ix = 3*nregion1 + 1
        iy = ix + 1
        iz = iy + 1
      endif
      if (n1j.le.nregion1) then
        jx = 3*(nfreeatom(n1j) - 1) + 1
        jy = jx + 1
        jz = jy + 1
      else
        jx = 3*nregion1 + 1
        jy = jx + 1
        jz = jy + 1
      endif
      if (nauatom(n1i).gt.0) then
        ixa = 3*(nfreeatomau(nauatom(n1i)) - 1) + 1
        iya = ixa + 1
        iza = iya + 1
      endif
      if (nauatom(n1j).gt.0) then
        jxa = 3*(nfreeatomau(nauatom(n1j)) - 1) + 1
        jya = jxa + 1
        jza = jya + 1
      endif
    endif
!
!  If neither i nor j are being optimised then atoms are not valid
!
    if (.not.lopi.and..not.lopj) then
      liok = .false.
      ljok = .false.
    endif
!
!  If i and j are not valid then there is no point continuing for this value of n1
!
    if (liok.and.ljok) then
      do n2 = 1,n1
        ind = ind + 1
        if (d2(ind).ne.0.0_dp.or.(n1.eq.n2.and.d1(n2).ne.0.0_dp)) then
!******************************
!  Find pair of atoms for n2  *
!******************************
          if (n2.le.nneigh(nri)) then
!
!  i -> neighbours of i
!
            n2k = i
            n2l = neighno(n2,nri)
            xd2 = xneigh(n2,nri)
            yd2 = yneigh(n2,nri)
            zd2 = zneigh(n2,nri)
!
            xik = - x2i
            yik = - y2i
            zik = - z2i
!
            lkok = .true.
            llok = .true.
          elseif (n2.le.nneigh2) then
!
!  Neighbours of i -> neighbours of i
!
            ntmp = n2 - nneigh(nri)
            n2l = 1
            n2l2 = 1
            do while (n2l2.lt.ntmp)
              n2l = n2l + 1
              n2l2 = n2l2 + n2l
            enddo
            n2l2 = n2l2 - n2l
            n2k = ntmp - n2l2
!
!  Check that atoms are valid
!
            lkok = (n2k.le.nneigh(nri))
            llok = (n2l.le.nneigh(nri))
            if (lkok.and.llok) then
!
!  Calculate vector between atoms
!
              xd2 = (xneigh(n2l,nri) - xneigh(n2k,nri))
              yd2 = (yneigh(n2l,nri) - yneigh(n2k,nri))
              zd2 = (zneigh(n2l,nri) - zneigh(n2k,nri))
!  
              xik = xneigh(n2k,nri) - x2i
              yik = yneigh(n2k,nri) - y2i
              zik = zneigh(n2k,nri) - z2i
!
!  Convert neighbour numbers to real atoms
!
              n2k = neighno(n2k,nri)
              n2l = neighno(n2l,nri)
            endif
          else
!
!  Neighbours of neighbours of i -> neighbours of i
!
            ntmp = n2 - nneigh2
            nji = (ntmp - 1)/((maxneigh + 1)*maxneigh) + 1
            ntmp = ntmp - (nji - 1)*(maxneigh + 1)*maxneigh
            n2k = (ntmp - 1)/maxneigh
            n2l = ntmp - n2k*maxneigh
!
!  Check that atoms are valid
!
            lkok = (n2k.le.nneigh(nri))
            if (nji.le.nneigh(nri)) then
              llok = (n2l.le.nneigh(nREBOatomRptr(neighno(nji,nri))))
            else
              llok = .false.
            endif
!
            if (lkok.and.llok) then
              if (n2k.eq.0) then
                xi = 0.0_dp
                yi = 0.0_dp
                zi = 0.0_dp
                n2k = i
              else
                xi = xneigh(n2k,nri)
                yi = yneigh(n2k,nri)
                zi = zneigh(n2k,nri)
                n2k = neighno(n2k,nri)
              endif
!         
              xd2 = (xneigh(n2l,nREBOatomRptr(neighno(nji,nri))) + xneigh(nji,nri) - xi)
              yd2 = (yneigh(n2l,nREBOatomRptr(neighno(nji,nri))) + yneigh(nji,nri) - yi)
              zd2 = (zneigh(n2l,nREBOatomRptr(neighno(nji,nri))) + zneigh(nji,nri) - zi)
!               
              xik = xi - x2i
              yik = yi - y2i
              zik = zi - z2i
!       
              n2l = neighno(n2l,nREBOatomRptr(neighno(nji,nri)))
            endif
          endif
!
!  No point continuing beyond here unless k and l are valid
!
          if (lkok.and.llok) then
!
!  Complete remaining vectors
!
            xjk = xik - xd1
            yjk = yik - yd1
            zjk = zik - zd1
            xjl = xjk + xd2
            yjl = yjk + yd2
            zjl = zjk + zd2
!
!  Compute distances squared
!
            r2ik = xik*xik + yik*yik + zik*zik
            r2jl = xjl*xjl + yjl*yjl + zjl*zjl
!
            lopk = (nfreeatom(n2k).gt.0)
            lopl = (nfreeatom(n2l).gt.0)
            if (n2k.le.nregion1) then
              kx = 3*(nfreeatom(n2k) - 1) + 1
              ky = kx + 1
              kz = ky + 1
            else
              kx = 3*nregion1 + 1
              ky = kx + 1
              kz = ky + 1
            endif
            if (n2l.le.nregion1) then
              lx = 3*(nfreeatom(n2l) - 1) + 1
              ly = lx + 1
              lz = ly + 1
            else
              lx = 3*nregion1 + 1
              ly = lx + 1
              lz = ly + 1
            endif
            if (nauatom(n2k).gt.0) then
              kxa = 3*(nfreeatomau(nauatom(n2k)) - 1) + 1
              kya = kxa + 1
              kza = kya + 1
            endif
            if (nauatom(n2l).gt.0) then
              lxa = 3*(nfreeatomau(nauatom(n2l)) - 1) + 1
              lya = lxa + 1
              lza = lya + 1
            endif
!
            lsameijkl = .false.
            if (n1i.eq.n2k.and.n1j.eq.n2l.and.r2ik.lt.1.0d-8.and.r2jl.lt.1.0d-8) lsameijkl = .true.
!
            if ((lopi.or.lopk).and.n1i.ne.n2k) then
!
!  Second derivatives : i - k
!
              if (lopi.and.nauatom(n1i).gt.0) then
                rneq = dble(neqvatom(nauatom(n1i)))
                derv2(kx,ixa) = derv2(kx,ixa) + xd1*xd2*d2(ind)*rneq
                derv2(ky,ixa) = derv2(ky,ixa) + xd1*yd2*d2(ind)*rneq
                derv2(kz,ixa) = derv2(kz,ixa) + xd1*zd2*d2(ind)*rneq
                derv2(kx,iya) = derv2(kx,iya) + yd1*xd2*d2(ind)*rneq
                derv2(ky,iya) = derv2(ky,iya) + yd1*yd2*d2(ind)*rneq
                derv2(kz,iya) = derv2(kz,iya) + yd1*zd2*d2(ind)*rneq
                derv2(kx,iza) = derv2(kx,iza) + zd1*xd2*d2(ind)*rneq
                derv2(ky,iza) = derv2(ky,iza) + zd1*yd2*d2(ind)*rneq
                derv2(kz,iza) = derv2(kz,iza) + zd1*zd2*d2(ind)*rneq
                if (n1.eq.n2) then
                  derv2(kx,ixa) = derv2(kx,ixa) + d1(n2)*rneq
                  derv2(ky,iya) = derv2(ky,iya) + d1(n2)*rneq
                  derv2(kz,iza) = derv2(kz,iza) + d1(n2)*rneq
                endif
              endif
              if (lopk.and.nauatom(n2k).gt.0.and..not.lsameijkl) then
                rneq = dble(neqvatom(nauatom(n2k)))
                derv2(ix,kxa) = derv2(ix,kxa) + xd2*xd1*d2(ind)*rneq
                derv2(iy,kxa) = derv2(iy,kxa) + xd2*yd1*d2(ind)*rneq
                derv2(iz,kxa) = derv2(iz,kxa) + xd2*zd1*d2(ind)*rneq
                derv2(ix,kya) = derv2(ix,kya) + yd2*xd1*d2(ind)*rneq
                derv2(iy,kya) = derv2(iy,kya) + yd2*yd1*d2(ind)*rneq
                derv2(iz,kya) = derv2(iz,kya) + yd2*zd1*d2(ind)*rneq
                derv2(ix,kza) = derv2(ix,kza) + zd2*xd1*d2(ind)*rneq
                derv2(iy,kza) = derv2(iy,kza) + zd2*yd1*d2(ind)*rneq
                derv2(iz,kza) = derv2(iz,kza) + zd2*zd1*d2(ind)*rneq
                if (n1.eq.n2) then
                  derv2(ix,kxa) = derv2(ix,kxa) + d1(n2)*rneq
                  derv2(iy,kya) = derv2(iy,kya) + d1(n2)*rneq
                  derv2(iz,kza) = derv2(iz,kza) + d1(n2)*rneq
                endif
              endif
            endif
!
!  Second derivatives : i - l
!
            if ((lopi.or.lopl).and.n1i.ne.n2l) then
              if (lopi.and.nauatom(n1i).gt.0) then
                rneq = dble(neqvatom(nauatom(n1i)))
                derv2(lx,ixa) = derv2(lx,ixa) - xd1*xd2*d2(ind)*rneq
                derv2(ly,ixa) = derv2(ly,ixa) - xd1*yd2*d2(ind)*rneq
                derv2(lz,ixa) = derv2(lz,ixa) - xd1*zd2*d2(ind)*rneq
                derv2(lx,iya) = derv2(lx,iya) - yd1*xd2*d2(ind)*rneq
                derv2(ly,iya) = derv2(ly,iya) - yd1*yd2*d2(ind)*rneq
                derv2(lz,iya) = derv2(lz,iya) - yd1*zd2*d2(ind)*rneq
                derv2(lx,iza) = derv2(lx,iza) - zd1*xd2*d2(ind)*rneq
                derv2(ly,iza) = derv2(ly,iza) - zd1*yd2*d2(ind)*rneq
                derv2(lz,iza) = derv2(lz,iza) - zd1*zd2*d2(ind)*rneq
                if (n1.eq.n2) then
                  derv2(lx,ixa) = derv2(lx,ixa) - d1(n2)*rneq
                  derv2(ly,iya) = derv2(ly,iya) - d1(n2)*rneq
                  derv2(lz,iza) = derv2(lz,iza) - d1(n2)*rneq
                endif
              endif
              if (lopl.and.nauatom(n2l).gt.0.and..not.lsameijkl) then
                rneq = dble(neqvatom(nauatom(n2l)))
                derv2(ix,lxa) = derv2(ix,lxa) - xd2*xd1*d2(ind)*rneq
                derv2(iy,lxa) = derv2(iy,lxa) - xd2*yd1*d2(ind)*rneq
                derv2(iz,lxa) = derv2(iz,lxa) - xd2*zd1*d2(ind)*rneq
                derv2(ix,lya) = derv2(ix,lya) - yd2*xd1*d2(ind)*rneq
                derv2(iy,lya) = derv2(iy,lya) - yd2*yd1*d2(ind)*rneq
                derv2(iz,lya) = derv2(iz,lya) - yd2*zd1*d2(ind)*rneq
                derv2(ix,lza) = derv2(ix,lza) - zd2*xd1*d2(ind)*rneq
                derv2(iy,lza) = derv2(iy,lza) - zd2*yd1*d2(ind)*rneq
                derv2(iz,lza) = derv2(iz,lza) - zd2*zd1*d2(ind)*rneq
                if (n1.eq.n2) then
                  derv2(ix,lxa) = derv2(ix,lxa) - d1(n2)*rneq
                  derv2(iy,lya) = derv2(iy,lya) - d1(n2)*rneq
                  derv2(iz,lza) = derv2(iz,lza) - d1(n2)*rneq
                endif
              endif
            endif
!
!  Second derivatives : j - k
!
            if ((lopj.or.lopk).and.n1j.ne.n2k) then
              if (lopj.and.nauatom(n1j).gt.0) then
                rneq = dble(neqvatom(nauatom(n1j)))
                derv2(kx,jxa) = derv2(kx,jxa) - xd1*xd2*d2(ind)*rneq
                derv2(ky,jxa) = derv2(ky,jxa) - xd1*yd2*d2(ind)*rneq
                derv2(kz,jxa) = derv2(kz,jxa) - xd1*zd2*d2(ind)*rneq
                derv2(kx,jya) = derv2(kx,jya) - yd1*xd2*d2(ind)*rneq
                derv2(ky,jya) = derv2(ky,jya) - yd1*yd2*d2(ind)*rneq
                derv2(kz,jya) = derv2(kz,jya) - yd1*zd2*d2(ind)*rneq
                derv2(kx,jza) = derv2(kx,jza) - zd1*xd2*d2(ind)*rneq
                derv2(ky,jza) = derv2(ky,jza) - zd1*yd2*d2(ind)*rneq
                derv2(kz,jza) = derv2(kz,jza) - zd1*zd2*d2(ind)*rneq
                if (n1.eq.n2) then
                  derv2(kx,jxa) = derv2(kx,jxa) - d1(n2)*rneq
                  derv2(ky,jya) = derv2(ky,jya) - d1(n2)*rneq
                  derv2(kz,jza) = derv2(kz,jza) - d1(n2)*rneq
                endif
              endif
              if (lopk.and.nauatom(n2k).gt.0.and..not.lsameijkl) then
                rneq = dble(neqvatom(nauatom(n2k)))
                derv2(jx,kxa) = derv2(jx,kxa) - xd2*xd1*d2(ind)*rneq
                derv2(jy,kxa) = derv2(jy,kxa) - xd2*yd1*d2(ind)*rneq
                derv2(jz,kxa) = derv2(jz,kxa) - xd2*zd1*d2(ind)*rneq
                derv2(jx,kya) = derv2(jx,kya) - yd2*xd1*d2(ind)*rneq
                derv2(jy,kya) = derv2(jy,kya) - yd2*yd1*d2(ind)*rneq
                derv2(jz,kya) = derv2(jz,kya) - yd2*zd1*d2(ind)*rneq
                derv2(jx,kza) = derv2(jx,kza) - zd2*xd1*d2(ind)*rneq
                derv2(jy,kza) = derv2(jy,kza) - zd2*yd1*d2(ind)*rneq
                derv2(jz,kza) = derv2(jz,kza) - zd2*zd1*d2(ind)*rneq
                if (n1.eq.n2) then
                  derv2(jx,kxa) = derv2(jx,kxa) - d1(n2)*rneq
                  derv2(jy,kya) = derv2(jy,kya) - d1(n2)*rneq
                  derv2(jz,kza) = derv2(jz,kza) - d1(n2)*rneq
                endif
              endif
            endif
!
!  Second derivatives : j - l
!
            if ((lopj.or.lopl).and.n1j.ne.n2l) then
              if (lopj.and.nauatom(n1j).gt.0) then
                rneq = dble(neqvatom(nauatom(n1j)))
                derv2(lx,jxa) = derv2(lx,jxa) + xd1*xd2*d2(ind)*rneq
                derv2(ly,jxa) = derv2(ly,jxa) + xd1*yd2*d2(ind)*rneq
                derv2(lz,jxa) = derv2(lz,jxa) + xd1*zd2*d2(ind)*rneq
                derv2(lx,jya) = derv2(lx,jya) + yd1*xd2*d2(ind)*rneq
                derv2(ly,jya) = derv2(ly,jya) + yd1*yd2*d2(ind)*rneq
                derv2(lz,jya) = derv2(lz,jya) + yd1*zd2*d2(ind)*rneq
                derv2(lx,jza) = derv2(lx,jza) + zd1*xd2*d2(ind)*rneq
                derv2(ly,jza) = derv2(ly,jza) + zd1*yd2*d2(ind)*rneq
                derv2(lz,jza) = derv2(lz,jza) + zd1*zd2*d2(ind)*rneq
                if (n1.eq.n2) then
                  derv2(lx,jxa) = derv2(lx,jxa) + d1(n2)*rneq
                  derv2(ly,jya) = derv2(ly,jya) + d1(n2)*rneq
                  derv2(lz,jza) = derv2(lz,jza) + d1(n2)*rneq
                endif
              endif
              if (lopl.and.nauatom(n2l).gt.0.and..not.lsameijkl) then
                rneq = dble(neqvatom(nauatom(n2l)))
                derv2(jx,lxa) = derv2(jx,lxa) + xd2*xd1*d2(ind)*rneq
                derv2(jy,lxa) = derv2(jy,lxa) + xd2*yd1*d2(ind)*rneq
                derv2(jz,lxa) = derv2(jz,lxa) + xd2*zd1*d2(ind)*rneq
                derv2(jx,lya) = derv2(jx,lya) + yd2*xd1*d2(ind)*rneq
                derv2(jy,lya) = derv2(jy,lya) + yd2*yd1*d2(ind)*rneq
                derv2(jz,lya) = derv2(jz,lya) + yd2*zd1*d2(ind)*rneq
                derv2(jx,lza) = derv2(jx,lza) + zd2*xd1*d2(ind)*rneq
                derv2(jy,lza) = derv2(jy,lza) + zd2*yd1*d2(ind)*rneq
                derv2(jz,lza) = derv2(jz,lza) + zd2*zd1*d2(ind)*rneq
                if (n1.eq.n2) then
                  derv2(jx,lxa) = derv2(jx,lxa) + d1(n2)*rneq
                  derv2(jy,lya) = derv2(jy,lya) + d1(n2)*rneq
                  derv2(jz,lza) = derv2(jz,lza) + d1(n2)*rneq
                endif
              endif
            endif
          endif
        endif
      enddo
    else
!
!  Increment ind pointer by number of missed terms
!
      ind = ind + n1
    endif
  enddo
!
  return
  end
!
  subroutine d2addp(i,maxneigh,nneigh,neighno,xneigh,yneigh,zneigh,nREBOatomRptr,d1,d2,xkv,ykv,zkv,ltorderv)
!
!  Utility to add the pairwise derivatives for a group of atoms
!  on to the global Cartesian phased second derivative arrays.
!
!  On entry : 
!
!  i               = atom number of i to whom others are connected
!  maxneigh        = left-hand dimension of 2-D arrays
!  nneigh          = no. of neighbours of atoms
!  neighno         = atom number of each neighbour
!  xneigh          = x component of vector to neighbour of atoms
!  yneigh          = y component of vector to neighbour of atoms
!  zneigh          = z component of vector to neighbour of atoms
!  nREBOatomRptr   = pointer from atoms to position in REBO set
!  d1              = array of first derivatives w.r.t. interatomic
!                    distances
!  d2              = array of first derivatives w.r.t. interatomic
!                    distances
!  xkv             = x component of K vector
!  ykv             = y component of K vector
!  zkv             = z component of K vector
!  ltorderv        = if true then include torsional derivatives
!
!   8/02 Created from d2add
!   8/02 Removal of uninvalid terms added
!   9/02 Correction to lsameijkl added - need to check distances too
!  12/03 Torsional derivatives made optional
!   6/07 nREBOatomRptr added as input
!  10/07 Correction made to setting of n1i/n2k for torsional case
!  11/07 Unused variables removed
!   1/10 Bug fixed for neighbour of i -> neighbour of neighbour of i case
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
!  Copyright Curtin University 2010
!
!  Julian Gale, NRI, Curtin University, January 2010
!
  use brennerdata
  use derivatives
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)             :: i
  integer(i4), intent(in)             :: maxneigh
  integer(i4), intent(in)             :: nneigh(*)
  integer(i4), intent(in)             :: neighno(maxneigh,*)
  integer(i4), intent(in)             :: nREBOatomRptr(*)
  real(dp),    intent(in)             :: xneigh(maxneigh,*)
  real(dp),    intent(in)             :: yneigh(maxneigh,*)
  real(dp),    intent(in)             :: zneigh(maxneigh,*)
  real(dp),    intent(in)             :: d1(*)
  real(dp),    intent(in)             :: d2(*)
  real(dp),    intent(in)             :: xkv
  real(dp),    intent(in)             :: ykv
  real(dp),    intent(in)             :: zkv
  logical,     intent(in)             :: ltorderv
!
!  Local variables
!
  integer(i4)                         :: ind
  integer(i4)                         :: ix,iy,iz
  integer(i4)                         :: jx,jy,jz
  integer(i4)                         :: kx,ky,kz
  integer(i4)                         :: lx,ly,lz
  integer(i4)                         :: n1
  integer(i4)                         :: n1i
  integer(i4)                         :: n1j
  integer(i4)                         :: n1j2
  integer(i4)                         :: n2
  integer(i4)                         :: n2k
  integer(i4)                         :: n2l
  integer(i4)                         :: n2l2
  integer(i4)                         :: ndist
  integer(i4)                         :: nji
  integer(i4)                         :: nneigh2
  integer(i4)                         :: nri
  integer(i4)                         :: ntmp
  real(dp)                            :: cosik
  real(dp)                            :: cosil
  real(dp)                            :: cosjk
  real(dp)                            :: cosjl
  real(dp)                            :: oneik
  real(dp)                            :: oneil
  real(dp)                            :: onejk
  real(dp)                            :: onejl
  real(dp)                            :: r2ik
  real(dp)                            :: r2jl
  real(dp)                            :: sinik
  real(dp)                            :: sinil
  real(dp)                            :: sinjk
  real(dp)                            :: sinjl
  real(dp)                            :: xd1
  real(dp)                            :: yd1
  real(dp)                            :: zd1
  real(dp)                            :: xd2
  real(dp)                            :: yd2
  real(dp)                            :: zd2
  real(dp)                            :: xi
  real(dp)                            :: yi
  real(dp)                            :: zi
  real(dp)                            :: xik
  real(dp)                            :: yik
  real(dp)                            :: zik
  real(dp)                            :: xil
  real(dp)                            :: yil
  real(dp)                            :: zil
  real(dp)                            :: xjk
  real(dp)                            :: yjk
  real(dp)                            :: zjk
  real(dp)                            :: xjl
  real(dp)                            :: yjl
  real(dp)                            :: zjl
  real(dp)                            :: x2i
  real(dp)                            :: y2i
  real(dp)                            :: z2i
  logical                             :: liok
  logical                             :: ljok
  logical                             :: lkok
  logical                             :: llok
  logical                             :: lsameijkl
!
!  Calculate total number of distances
!
  nri = nREBOatomRptr(i)
  nneigh2 = nneigh(nri) + nneigh(nri)*(nneigh(nri) + 1)/2
  if (ltorderv) then
    ndist = nneigh2 + nneigh(nri)*(maxneigh + 1)*maxneigh
  else
    ndist = nneigh2
  endif
!
!  Loop over pairs of interatomic distances
!
  ind = 0
  do n1 = 1,ndist
!******************************
!  Find pair of atoms for n1  *
!******************************
    if (n1.le.nneigh(nri)) then
!
!  i -> neighbours of i
!
      n1i = i
      n1j = neighno(n1,nri)
      xd1 = xneigh(n1,nri)
      yd1 = yneigh(n1,nri)
      zd1 = zneigh(n1,nri)
      x2i = 0.0_dp
      y2i = 0.0_dp
      z2i = 0.0_dp
      liok = .true.
      ljok = .true.
    elseif (n1.le.nneigh2) then
!
!  Neighbours of i -> neighbours of i
!
      ntmp = n1 - nneigh(nri)
      n1j = 1
      n1j2 = 1
      do while (n1j2.lt.ntmp)
        n1j = n1j + 1
        n1j2 = n1j2 + n1j
      enddo
      n1j2 = n1j2 - n1j
      n1i = ntmp - n1j2
!     
!  Check that neighbour numbers are valid
!         
      liok = (n1i.le.nneigh(nri))
      ljok = (n1j.le.nneigh(nri))       
      if (liok.and.ljok) then
!         
!  Get vector between atoms
!
        xd1 = (xneigh(n1j,nri) - xneigh(n1i,nri))
        yd1 = (yneigh(n1j,nri) - yneigh(n1i,nri))
        zd1 = (zneigh(n1j,nri) - zneigh(n1i,nri))
!
        x2i = xneigh(n1i,nri)
        y2i = yneigh(n1i,nri)
        z2i = zneigh(n1i,nri)
!
!  Convert neighbour numbers to real atoms
!
        n1i = neighno(n1i,nri)
        n1j = neighno(n1j,nri)
      endif
    else
!
!  Neighbours of neighbours of i -> neighbours of i
!
      ntmp = n1 - nneigh2
      nji = (ntmp - 1)/((maxneigh + 1)*maxneigh) + 1
      ntmp = ntmp - (nji - 1)*(maxneigh + 1)*maxneigh
      n1i = (ntmp - 1)/maxneigh
      n1j = ntmp - n1i*maxneigh
!         
!  Check that atoms are valid
!       
      liok = (n1i.le.nneigh(nri))
      ljok = (n1j.le.nneigh(nREBOatomRptr(neighno(nji,nri))))
!  
      if (liok.and.ljok) then
        if (n1i.eq.0) then
          xi = 0.0_dp
          yi = 0.0_dp
          zi = 0.0_dp
          n1i = i
        else
          xi = xneigh(n1i,nri)
          yi = yneigh(n1i,nri)
          zi = zneigh(n1i,nri)
          n1i = neighno(n1i,nri)
        endif
!
        xd1 = (xneigh(n1j,nREBOatomRptr(neighno(nji,nri))) + xneigh(nji,nri) - xi)
        yd1 = (yneigh(n1j,nREBOatomRptr(neighno(nji,nri))) + yneigh(nji,nri) - yi)
        zd1 = (zneigh(n1j,nREBOatomRptr(neighno(nji,nri))) + zneigh(nji,nri) - zi)
!
        x2i = xi
        y2i = yi
        z2i = zi
!
        n1j = neighno(n1j,nREBOatomRptr(neighno(nji,nri)))
      endif
    endif
!  
!  Skip the following section as it is irrelevant if i and j are not valid
!           
    if (liok.and.ljok) then
!
!  Set second derivative location pointers
!
      ix = 3*(n1i - 1) + 1
      iy = ix + 1
      iz = iy + 1
      jx = 3*(n1j - 1) + 1
      jy = jx + 1
      jz = jy + 1
    endif
!  
!  If i and j are not valid then there is no point continuing for this value of n1
!           
    if (liok.and.ljok) then
      do n2 = 1,n1
        ind = ind + 1
        if (d2(ind).ne.0.0_dp.or.(n1.eq.n2.and.d1(n2).ne.0.0_dp)) then
!******************************
!  Find pair of atoms for n2  *
!******************************
          if (n2.le.nneigh(nri)) then
!
!  i -> neighbours of i
!
            n2k = i
            n2l = neighno(n2,nri)
            xd2 = xneigh(n2,nri)
            yd2 = yneigh(n2,nri)
            zd2 = zneigh(n2,nri)
!
            xik = - x2i
            yik = - y2i
            zik = - z2i
!
            lkok = .true.
            llok = .true.
          elseif (n2.le.nneigh2) then
!
!  Neighbours of i -> neighbours of i
!
            ntmp = n2 - nneigh(nri)
            n2l = 1
            n2l2 = 1
            do while (n2l2.lt.ntmp)
              n2l = n2l + 1
              n2l2 = n2l2 + n2l
            enddo
            n2l2 = n2l2 - n2l
            n2k = ntmp - n2l2
!
!  Check that atoms are valid
!  
            lkok = (n2k.le.nneigh(nri))
            llok = (n2l.le.nneigh(nri))
            if (lkok.and.llok) then
!               
!  Calculate vector between atoms
!             
              xd2 = (xneigh(n2l,nri) - xneigh(n2k,nri))
              yd2 = (yneigh(n2l,nri) - yneigh(n2k,nri))
              zd2 = (zneigh(n2l,nri) - zneigh(n2k,nri))
!
              xik = xneigh(n2k,nri) - x2i
              yik = yneigh(n2k,nri) - y2i
              zik = zneigh(n2k,nri) - z2i
!
!  Convert neighbour numbers to real atoms
!
              n2k = neighno(n2k,nri)
              n2l = neighno(n2l,nri)
            endif
          else
!
!  Neighbours of neighbours of i -> neighbours of i
!
            ntmp = n2 - nneigh2
            nji = (ntmp - 1)/((maxneigh + 1)*maxneigh) + 1
            ntmp = ntmp - (nji - 1)*(maxneigh + 1)*maxneigh
            n2k = (ntmp - 1)/maxneigh
            n2l = ntmp - n2k*maxneigh
!               
!  Check that atoms are valid
!             
            lkok = (n2k.le.nneigh(nri))
            llok = (n2l.le.nneigh(neighno(nji,nri))) 
!               
            if (lkok.and.llok) then
              if (n2k.eq.0) then
                xi = 0.0_dp
                yi = 0.0_dp
                zi = 0.0_dp
                n2k = i
              else
                xi = xneigh(n2k,nri)
                yi = yneigh(n2k,nri)
                zi = zneigh(n2k,nri)
                n2k = neighno(n2k,nri)
              endif
!         
              xd2 = (xneigh(n2l,nREBOatomRptr(neighno(nji,nri))) + xneigh(nji,nri) - xi)
              yd2 = (yneigh(n2l,nREBOatomRptr(neighno(nji,nri))) + yneigh(nji,nri) - yi)
              zd2 = (zneigh(n2l,nREBOatomRptr(neighno(nji,nri))) + zneigh(nji,nri) - zi)
!
              xik = xi - x2i
              yik = yi - y2i
              zik = zi - z2i
!       
              n2l = neighno(n2l,nREBOatomRptr(neighno(nji,nri)))
            endif
          endif
!               
!  No point continuing beyond here unless k and l are valid
!                 
          if (lkok.and.llok) then
!
!  Complete remaining vectors
!
            xil = xik + xd2
            yil = yik + yd2
            zil = zik + zd2
            xjk = xik - xd1
            yjk = yik - yd1
            zjk = zik - zd1
            xjl = xjk + xd2
            yjl = yjk + yd2
            zjl = zjk + zd2
!
!  Compute distances squared
!
            r2ik = xik*xik + yik*yik + zik*zik
            r2jl = xjl*xjl + yjl*yjl + zjl*zjl
!
            kx = 3*(n2k - 1) + 1
            ky = kx + 1
            kz = ky + 1
            lx = 3*(n2l - 1) + 1
            ly = lx + 1
            lz = ly + 1
!
            lsameijkl = .false.
            if (n1i.eq.n2k.and.n1j.eq.n2l.and.r2ik.lt.1.0d-8.and.r2jl.lt.1.0d-8) lsameijkl = .true.
!
!  Second derivatives : i - k
!
            if (n1i.eq.n2k) then
              oneik = 1.0_dp
            else
              oneik = 0.0_dp
            endif
            cosik = xkv*xik + ykv*yik + zkv*zik
            sinik = sin(cosik)
            cosik = cos(cosik) - oneik
!
            if (lsameijkl.or.n1i.eq.n2k) then
              cosik = 0.5_dp*cosik
              sinik = 0.5_dp*sinik
            endif
            derv2(kx,ix) = derv2(kx,ix) + xd1*xd2*d2(ind)*cosik
            derv2(ky,ix) = derv2(ky,ix) + xd1*yd2*d2(ind)*cosik
            derv2(kz,ix) = derv2(kz,ix) + xd1*zd2*d2(ind)*cosik
            derv2(kx,iy) = derv2(kx,iy) + yd1*xd2*d2(ind)*cosik
            derv2(ky,iy) = derv2(ky,iy) + yd1*yd2*d2(ind)*cosik
            derv2(kz,iy) = derv2(kz,iy) + yd1*zd2*d2(ind)*cosik
            derv2(kx,iz) = derv2(kx,iz) + zd1*xd2*d2(ind)*cosik
            derv2(ky,iz) = derv2(ky,iz) + zd1*yd2*d2(ind)*cosik
            derv2(kz,iz) = derv2(kz,iz) + zd1*zd2*d2(ind)*cosik
!
            dervi(kx,ix) = dervi(kx,ix) + xd1*xd2*d2(ind)*sinik
            dervi(ky,ix) = dervi(ky,ix) + xd1*yd2*d2(ind)*sinik
            dervi(kz,ix) = dervi(kz,ix) + xd1*zd2*d2(ind)*sinik
            dervi(kx,iy) = dervi(kx,iy) + yd1*xd2*d2(ind)*sinik
            dervi(ky,iy) = dervi(ky,iy) + yd1*yd2*d2(ind)*sinik
            dervi(kz,iy) = dervi(kz,iy) + yd1*zd2*d2(ind)*sinik
            dervi(kx,iz) = dervi(kx,iz) + zd1*xd2*d2(ind)*sinik
            dervi(ky,iz) = dervi(ky,iz) + zd1*yd2*d2(ind)*sinik
            dervi(kz,iz) = dervi(kz,iz) + zd1*zd2*d2(ind)*sinik
            if (n1.eq.n2) then
              derv2(kx,ix) = derv2(kx,ix) + d1(n2)*cosik
              derv2(ky,iy) = derv2(ky,iy) + d1(n2)*cosik
              derv2(kz,iz) = derv2(kz,iz) + d1(n2)*cosik
              dervi(kx,ix) = dervi(kx,ix) + d1(n2)*sinik
              dervi(ky,iy) = dervi(ky,iy) + d1(n2)*sinik
              dervi(kz,iz) = dervi(kz,iz) + d1(n2)*sinik
            endif
!
            derv2(ix,kx) = derv2(ix,kx) + xd1*xd2*d2(ind)*cosik
            derv2(ix,ky) = derv2(ix,ky) + xd1*yd2*d2(ind)*cosik
            derv2(ix,kz) = derv2(ix,kz) + xd1*zd2*d2(ind)*cosik
            derv2(iy,kx) = derv2(iy,kx) + yd1*xd2*d2(ind)*cosik
            derv2(iy,ky) = derv2(iy,ky) + yd1*yd2*d2(ind)*cosik
            derv2(iy,kz) = derv2(iy,kz) + yd1*zd2*d2(ind)*cosik
            derv2(iz,kx) = derv2(iz,kx) + zd1*xd2*d2(ind)*cosik
            derv2(iz,ky) = derv2(iz,ky) + zd1*yd2*d2(ind)*cosik
            derv2(iz,kz) = derv2(iz,kz) + zd1*zd2*d2(ind)*cosik
!
            dervi(ix,kx) = dervi(ix,kx) - xd1*xd2*d2(ind)*sinik
            dervi(ix,ky) = dervi(ix,ky) - xd1*yd2*d2(ind)*sinik
            dervi(ix,kz) = dervi(ix,kz) - xd1*zd2*d2(ind)*sinik
            dervi(iy,kx) = dervi(iy,kx) - yd1*xd2*d2(ind)*sinik
            dervi(iy,ky) = dervi(iy,ky) - yd1*yd2*d2(ind)*sinik
            dervi(iy,kz) = dervi(iy,kz) - yd1*zd2*d2(ind)*sinik
            dervi(iz,kx) = dervi(iz,kx) - zd1*xd2*d2(ind)*sinik
            dervi(iz,ky) = dervi(iz,ky) - zd1*yd2*d2(ind)*sinik
            dervi(iz,kz) = dervi(iz,kz) - zd1*zd2*d2(ind)*sinik
            if (n1.eq.n2) then
              derv2(ix,kx) = derv2(ix,kx) + d1(n2)*cosik
              derv2(iy,ky) = derv2(iy,ky) + d1(n2)*cosik
              derv2(iz,kz) = derv2(iz,kz) + d1(n2)*cosik
              dervi(ix,kx) = dervi(ix,kx) - d1(n2)*sinik
              dervi(iy,ky) = dervi(iy,ky) - d1(n2)*sinik
              dervi(iz,kz) = dervi(iz,kz) - d1(n2)*sinik
            endif
!
!  Second derivatives : i - l
!
            if (n1i.eq.n2l) then
              oneil = 1.0_dp
            else
              oneil = 0.0_dp
            endif
            cosil = xkv*xil + ykv*yil + zkv*zil
            sinil = sin(cosil)
            cosil = cos(cosil) - oneil
!
            if (lsameijkl.or.n1i.eq.n2l) then
              cosil = 0.5_dp*cosil
              sinil = 0.5_dp*sinil
            endif
!
            derv2(lx,ix) = derv2(lx,ix) - xd1*xd2*d2(ind)*cosil
            derv2(ly,ix) = derv2(ly,ix) - xd1*yd2*d2(ind)*cosil
            derv2(lz,ix) = derv2(lz,ix) - xd1*zd2*d2(ind)*cosil
            derv2(lx,iy) = derv2(lx,iy) - yd1*xd2*d2(ind)*cosil
            derv2(ly,iy) = derv2(ly,iy) - yd1*yd2*d2(ind)*cosil
            derv2(lz,iy) = derv2(lz,iy) - yd1*zd2*d2(ind)*cosil
            derv2(lx,iz) = derv2(lx,iz) - zd1*xd2*d2(ind)*cosil
            derv2(ly,iz) = derv2(ly,iz) - zd1*yd2*d2(ind)*cosil
            derv2(lz,iz) = derv2(lz,iz) - zd1*zd2*d2(ind)*cosil
!
            dervi(lx,ix) = dervi(lx,ix) - xd1*xd2*d2(ind)*sinil
            dervi(ly,ix) = dervi(ly,ix) - xd1*yd2*d2(ind)*sinil
            dervi(lz,ix) = dervi(lz,ix) - xd1*zd2*d2(ind)*sinil
            dervi(lx,iy) = dervi(lx,iy) - yd1*xd2*d2(ind)*sinil
            dervi(ly,iy) = dervi(ly,iy) - yd1*yd2*d2(ind)*sinil
            dervi(lz,iy) = dervi(lz,iy) - yd1*zd2*d2(ind)*sinil
            dervi(lx,iz) = dervi(lx,iz) - zd1*xd2*d2(ind)*sinil
            dervi(ly,iz) = dervi(ly,iz) - zd1*yd2*d2(ind)*sinil
            dervi(lz,iz) = dervi(lz,iz) - zd1*zd2*d2(ind)*sinil
            if (n1.eq.n2) then
              derv2(lx,ix) = derv2(lx,ix) - d1(n2)*cosil
              derv2(ly,iy) = derv2(ly,iy) - d1(n2)*cosil
              derv2(lz,iz) = derv2(lz,iz) - d1(n2)*cosil
              dervi(lx,ix) = dervi(lx,ix) - d1(n2)*sinil
              dervi(ly,iy) = dervi(ly,iy) - d1(n2)*sinil
              dervi(lz,iz) = dervi(lz,iz) - d1(n2)*sinil
            endif
!
            derv2(ix,lx) = derv2(ix,lx) - xd1*xd2*d2(ind)*cosil
            derv2(ix,ly) = derv2(ix,ly) - xd1*yd2*d2(ind)*cosil
            derv2(ix,lz) = derv2(ix,lz) - xd1*zd2*d2(ind)*cosil
            derv2(iy,lx) = derv2(iy,lx) - yd1*xd2*d2(ind)*cosil
            derv2(iy,ly) = derv2(iy,ly) - yd1*yd2*d2(ind)*cosil
            derv2(iy,lz) = derv2(iy,lz) - yd1*zd2*d2(ind)*cosil
            derv2(iz,lx) = derv2(iz,lx) - zd1*xd2*d2(ind)*cosil
            derv2(iz,ly) = derv2(iz,ly) - zd1*yd2*d2(ind)*cosil
            derv2(iz,lz) = derv2(iz,lz) - zd1*zd2*d2(ind)*cosil
!
            dervi(ix,lx) = dervi(ix,lx) + xd1*xd2*d2(ind)*sinil
            dervi(ix,ly) = dervi(ix,ly) + xd1*yd2*d2(ind)*sinil
            dervi(ix,lz) = dervi(ix,lz) + xd1*zd2*d2(ind)*sinil
            dervi(iy,lx) = dervi(iy,lx) + yd1*xd2*d2(ind)*sinil
            dervi(iy,ly) = dervi(iy,ly) + yd1*yd2*d2(ind)*sinil
            dervi(iy,lz) = dervi(iy,lz) + yd1*zd2*d2(ind)*sinil
            dervi(iz,lx) = dervi(iz,lx) + zd1*xd2*d2(ind)*sinil
            dervi(iz,ly) = dervi(iz,ly) + zd1*yd2*d2(ind)*sinil
            dervi(iz,lz) = dervi(iz,lz) + zd1*zd2*d2(ind)*sinil
            if (n1.eq.n2) then
              derv2(ix,lx) = derv2(ix,lx) - d1(n2)*cosil
              derv2(iy,ly) = derv2(iy,ly) - d1(n2)*cosil
              derv2(iz,lz) = derv2(iz,lz) - d1(n2)*cosil
              dervi(ix,lx) = dervi(ix,lx) + d1(n2)*sinil
              dervi(iy,ly) = dervi(iy,ly) + d1(n2)*sinil
              dervi(iz,lz) = dervi(iz,lz) + d1(n2)*sinil
            endif
!
!  Second derivatives : j - k
!
            if (n1j.eq.n2k) then
              onejk = 1.0_dp
            else
              onejk = 0.0_dp
            endif
            cosjk = xkv*xjk + ykv*yjk + zkv*zjk
            sinjk = sin(cosjk)
            cosjk = cos(cosjk) - onejk
!
            if (lsameijkl.or.n1j.eq.n2k) then
              cosjk = 0.5_dp*cosjk
              sinjk = 0.5_dp*sinjk
            endif
!
            derv2(kx,jx) = derv2(kx,jx) - xd1*xd2*d2(ind)*cosjk
            derv2(ky,jx) = derv2(ky,jx) - xd1*yd2*d2(ind)*cosjk
            derv2(kz,jx) = derv2(kz,jx) - xd1*zd2*d2(ind)*cosjk
            derv2(kx,jy) = derv2(kx,jy) - yd1*xd2*d2(ind)*cosjk
            derv2(ky,jy) = derv2(ky,jy) - yd1*yd2*d2(ind)*cosjk
            derv2(kz,jy) = derv2(kz,jy) - yd1*zd2*d2(ind)*cosjk
            derv2(kx,jz) = derv2(kx,jz) - zd1*xd2*d2(ind)*cosjk
            derv2(ky,jz) = derv2(ky,jz) - zd1*yd2*d2(ind)*cosjk
            derv2(kz,jz) = derv2(kz,jz) - zd1*zd2*d2(ind)*cosjk
!
            dervi(kx,jx) = dervi(kx,jx) - xd1*xd2*d2(ind)*sinjk
            dervi(ky,jx) = dervi(ky,jx) - xd1*yd2*d2(ind)*sinjk
            dervi(kz,jx) = dervi(kz,jx) - xd1*zd2*d2(ind)*sinjk
            dervi(kx,jy) = dervi(kx,jy) - yd1*xd2*d2(ind)*sinjk
            dervi(ky,jy) = dervi(ky,jy) - yd1*yd2*d2(ind)*sinjk
            dervi(kz,jy) = dervi(kz,jy) - yd1*zd2*d2(ind)*sinjk
            dervi(kx,jz) = dervi(kx,jz) - zd1*xd2*d2(ind)*sinjk
            dervi(ky,jz) = dervi(ky,jz) - zd1*yd2*d2(ind)*sinjk
            dervi(kz,jz) = dervi(kz,jz) - zd1*zd2*d2(ind)*sinjk
            if (n1.eq.n2) then
              derv2(kx,jx) = derv2(kx,jx) - d1(n2)*cosjk
              derv2(ky,jy) = derv2(ky,jy) - d1(n2)*cosjk
              derv2(kz,jz) = derv2(kz,jz) - d1(n2)*cosjk
              dervi(kx,jx) = dervi(kx,jx) - d1(n2)*sinjk
              dervi(ky,jy) = dervi(ky,jy) - d1(n2)*sinjk
              dervi(kz,jz) = dervi(kz,jz) - d1(n2)*sinjk
            endif
!
            derv2(jx,kx) = derv2(jx,kx) - xd1*xd2*d2(ind)*cosjk
            derv2(jx,ky) = derv2(jx,ky) - xd1*yd2*d2(ind)*cosjk
            derv2(jx,kz) = derv2(jx,kz) - xd1*zd2*d2(ind)*cosjk
            derv2(jy,kx) = derv2(jy,kx) - yd1*xd2*d2(ind)*cosjk
            derv2(jy,ky) = derv2(jy,ky) - yd1*yd2*d2(ind)*cosjk
            derv2(jy,kz) = derv2(jy,kz) - yd1*zd2*d2(ind)*cosjk
            derv2(jz,kx) = derv2(jz,kx) - zd1*xd2*d2(ind)*cosjk
            derv2(jz,ky) = derv2(jz,ky) - zd1*yd2*d2(ind)*cosjk
            derv2(jz,kz) = derv2(jz,kz) - zd1*zd2*d2(ind)*cosjk
!
            dervi(jx,kx) = dervi(jx,kx) + xd1*xd2*d2(ind)*sinjk
            dervi(jx,ky) = dervi(jx,ky) + xd1*yd2*d2(ind)*sinjk
            dervi(jx,kz) = dervi(jx,kz) + xd1*zd2*d2(ind)*sinjk
            dervi(jy,kx) = dervi(jy,kx) + yd1*xd2*d2(ind)*sinjk
            dervi(jy,ky) = dervi(jy,ky) + yd1*yd2*d2(ind)*sinjk
            dervi(jy,kz) = dervi(jy,kz) + yd1*zd2*d2(ind)*sinjk
            dervi(jz,kx) = dervi(jz,kx) + zd1*xd2*d2(ind)*sinjk
            dervi(jz,ky) = dervi(jz,ky) + zd1*yd2*d2(ind)*sinjk
            dervi(jz,kz) = dervi(jz,kz) + zd1*zd2*d2(ind)*sinjk
            if (n1.eq.n2) then
              derv2(jx,kx) = derv2(jx,kx) - d1(n2)*cosjk
              derv2(jy,ky) = derv2(jy,ky) - d1(n2)*cosjk
              derv2(jz,kz) = derv2(jz,kz) - d1(n2)*cosjk
              dervi(jx,kx) = dervi(jx,kx) + d1(n2)*sinjk
              dervi(jy,ky) = dervi(jy,ky) + d1(n2)*sinjk
              dervi(jz,kz) = dervi(jz,kz) + d1(n2)*sinjk
            endif
!
!  Second derivatives : j - l
!
            if (n1j.eq.n2l) then
              onejl = 1.0_dp
            else
              onejl = 0.0_dp
            endif
            cosjl = xkv*xjl + ykv*yjl + zkv*zjl
            sinjl = sin(cosjl)
            cosjl = cos(cosjl) - onejl
!
            if (lsameijkl.or.n1j.eq.n2l) then
              cosjl = 0.5_dp*cosjl
              sinjl = 0.5_dp*sinjl
            endif
!
            derv2(lx,jx) = derv2(lx,jx) + xd1*xd2*d2(ind)*cosjl
            derv2(ly,jx) = derv2(ly,jx) + xd1*yd2*d2(ind)*cosjl
            derv2(lz,jx) = derv2(lz,jx) + xd1*zd2*d2(ind)*cosjl
            derv2(lx,jy) = derv2(lx,jy) + yd1*xd2*d2(ind)*cosjl
            derv2(ly,jy) = derv2(ly,jy) + yd1*yd2*d2(ind)*cosjl
            derv2(lz,jy) = derv2(lz,jy) + yd1*zd2*d2(ind)*cosjl
            derv2(lx,jz) = derv2(lx,jz) + zd1*xd2*d2(ind)*cosjl
            derv2(ly,jz) = derv2(ly,jz) + zd1*yd2*d2(ind)*cosjl
            derv2(lz,jz) = derv2(lz,jz) + zd1*zd2*d2(ind)*cosjl
!
            dervi(lx,jx) = dervi(lx,jx) + xd1*xd2*d2(ind)*sinjl
            dervi(ly,jx) = dervi(ly,jx) + xd1*yd2*d2(ind)*sinjl
            dervi(lz,jx) = dervi(lz,jx) + xd1*zd2*d2(ind)*sinjl
            dervi(lx,jy) = dervi(lx,jy) + yd1*xd2*d2(ind)*sinjl
            dervi(ly,jy) = dervi(ly,jy) + yd1*yd2*d2(ind)*sinjl
            dervi(lz,jy) = dervi(lz,jy) + yd1*zd2*d2(ind)*sinjl
            dervi(lx,jz) = dervi(lx,jz) + zd1*xd2*d2(ind)*sinjl
            dervi(ly,jz) = dervi(ly,jz) + zd1*yd2*d2(ind)*sinjl
            dervi(lz,jz) = dervi(lz,jz) + zd1*zd2*d2(ind)*sinjl
            if (n1.eq.n2) then
              derv2(lx,jx) = derv2(lx,jx) + d1(n2)*cosjl
              derv2(ly,jy) = derv2(ly,jy) + d1(n2)*cosjl
              derv2(lz,jz) = derv2(lz,jz) + d1(n2)*cosjl
              dervi(lx,jx) = dervi(lx,jx) + d1(n2)*sinjl
              dervi(ly,jy) = dervi(ly,jy) + d1(n2)*sinjl
              dervi(lz,jz) = dervi(lz,jz) + d1(n2)*sinjl
            endif
!
            derv2(jx,lx) = derv2(jx,lx) + xd1*xd2*d2(ind)*cosjl
            derv2(jx,ly) = derv2(jx,ly) + xd1*yd2*d2(ind)*cosjl
            derv2(jx,lz) = derv2(jx,lz) + xd1*zd2*d2(ind)*cosjl
            derv2(jy,lx) = derv2(jy,lx) + yd1*xd2*d2(ind)*cosjl
            derv2(jy,ly) = derv2(jy,ly) + yd1*yd2*d2(ind)*cosjl
            derv2(jy,lz) = derv2(jy,lz) + yd1*zd2*d2(ind)*cosjl
            derv2(jz,lx) = derv2(jz,lx) + zd1*xd2*d2(ind)*cosjl
            derv2(jz,ly) = derv2(jz,ly) + zd1*yd2*d2(ind)*cosjl
            derv2(jz,lz) = derv2(jz,lz) + zd1*zd2*d2(ind)*cosjl
!
            dervi(jx,lx) = dervi(jx,lx) - xd1*xd2*d2(ind)*sinjl
            dervi(jx,ly) = dervi(jx,ly) - xd1*yd2*d2(ind)*sinjl
            dervi(jx,lz) = dervi(jx,lz) - xd1*zd2*d2(ind)*sinjl
            dervi(jy,lx) = dervi(jy,lx) - yd1*xd2*d2(ind)*sinjl
            dervi(jy,ly) = dervi(jy,ly) - yd1*yd2*d2(ind)*sinjl
            dervi(jy,lz) = dervi(jy,lz) - yd1*zd2*d2(ind)*sinjl
            dervi(jz,lx) = dervi(jz,lx) - zd1*xd2*d2(ind)*sinjl
            dervi(jz,ly) = dervi(jz,ly) - zd1*yd2*d2(ind)*sinjl
            dervi(jz,lz) = dervi(jz,lz) - zd1*zd2*d2(ind)*sinjl
            if (n1.eq.n2) then
              derv2(jx,lx) = derv2(jx,lx) + d1(n2)*cosjl
              derv2(jy,ly) = derv2(jy,ly) + d1(n2)*cosjl
              derv2(jz,lz) = derv2(jz,lz) + d1(n2)*cosjl
              dervi(jx,lx) = dervi(jx,lx) - d1(n2)*sinjl
              dervi(jy,ly) = dervi(jy,ly) - d1(n2)*sinjl
              dervi(jz,lz) = dervi(jz,lz) - d1(n2)*sinjl
            endif
          endif
        endif
      enddo
    else    
!               
!  Increment ind pointer by number of missed terms
!  
      ind = ind + n1
    endif
  enddo
!
  return
  end
!
  subroutine d2addij(i,k,xik,yik,zik,maxneigh,nneigh,neighno,xneigh,yneigh,zneigh,nfreeatom,nREBOatomRptr,d2)
!
!  Utility to add the pairwise derivatives for a group of atoms
!  on to the global Cartesian second derivative array. Version to handle i-j block.
!
!  On entry : 
!
!  i               = atom number of i to whom others are connected
!  k               = atom number of j to whom others are connected
!  xik             = x component of vector from i to k
!  yik             = y component of vector from i to k
!  zik             = z component of vector from i to k
!  maxneigh        = left-hand dimension of 2-D arrays
!  nneigh          = no. of neighbours of atoms
!  neighno         = atom number of each neighbour
!  xneigh          = x component of vector to neighbour of atoms
!  yneigh          = y component of vector to neighbour of atoms
!  zneigh          = z component of vector to neighbour of atoms
!  nfreeatom       = pointer from atom to free atom to move
!  nREBOatomRptr   = pointer from atoms to position in REBO set
!  d2              = array of first derivatives w.r.t. interatomic
!                    distances
!
!  10/07 Created from d2add
!  11/07 Unused variables removed
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
!  Julian Gale, NRI, Curtin University, November 2007
!
  use brennerdata
  use current,     only : ndim, nstrains
  use derivatives
  use symmetry,    only : lstr
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)             :: i
  integer(i4), intent(in)             :: k
  integer(i4), intent(in)             :: maxneigh
  integer(i4), intent(in)             :: nneigh(*)
  integer(i4), intent(in)             :: neighno(maxneigh,*)
  real(dp),    intent(in)             :: xik
  real(dp),    intent(in)             :: yik
  real(dp),    intent(in)             :: zik
  real(dp),    intent(in)             :: xneigh(maxneigh,*)
  real(dp),    intent(in)             :: yneigh(maxneigh,*)
  real(dp),    intent(in)             :: zneigh(maxneigh,*)
  integer(i4), intent(in)             :: nfreeatom(*)
  integer(i4), intent(in)             :: nREBOatomRptr(*)
  real(dp),    intent(in)             :: d2(*)
!
!  Local variables
!
  integer(i4)                         :: ind
  integer(i4)                         :: ix,iy,iz
  integer(i4)                         :: jx,jy,jz
  integer(i4)                         :: kx,ky,kz
  integer(i4)                         :: lx,ly,lz
  integer(i4)                         :: ixc,iyc,izc
  integer(i4)                         :: jxc,jyc,jzc
  integer(i4)                         :: kxc,kyc,kzc
  integer(i4)                         :: lxc,lyc,lzc
  integer(i4)                         :: n1
  integer(i4)                         :: n1i
  integer(i4)                         :: n1j
  integer(i4)                         :: n2
  integer(i4)                         :: n2k
  integer(i4)                         :: n2l
  integer(i4)                         :: nri
  integer(i4)                         :: nrk
  integer(i4)                         :: ns1
  integer(i4)                         :: ns2
  integer(i4)                         :: n2max
  real(dp)                            :: one1
  real(dp)                            :: one2
  real(dp)                            :: rp1(6)
  real(dp)                            :: rp2(6)
  real(dp)                            :: r2ik
  real(dp)                            :: r2jl
  real(dp)                            :: xd1
  real(dp)                            :: yd1
  real(dp)                            :: zd1
  real(dp)                            :: xd2
  real(dp)                            :: yd2
  real(dp)                            :: zd2
  real(dp)                            :: xjk
  real(dp)                            :: yjk
  real(dp)                            :: zjk
  real(dp)                            :: xjl
  real(dp)                            :: yjl
  real(dp)                            :: zjl
  logical                             :: lsameijkl
  logical                             :: liok
  logical                             :: ljok
  logical                             :: lkok
  logical                             :: llok
  logical                             :: lopi
  logical                             :: lopj
  logical                             :: lopk
  logical                             :: lopl
!
!  Calculate total number of distances
!
  nri = nREBOatomRptr(i)
  nrk = nREBOatomRptr(k)
!
!  Loop over pairs of interatomic distances
!
  ind = 0
  do n1 = 1,nneigh(nri)
!
!  i -> neighbours of i
!
    n1i = i
    n1j = neighno(n1,nri)
    xd1 = xneigh(n1,nri)
    yd1 = yneigh(n1,nri)
    zd1 = zneigh(n1,nri)
    liok = .true.
    ljok = .true.
!
!  Skip the following section as it is irrelevant if i and j are not valid
!
    if (liok.and.ljok) then
!
!  Set up products for strain
!
      if (lstr) then
        select case(ndim)
          case(1)
            rp1(1) = xd1*xd1
          case(2)
            rp1(1) = xd1*xd1
            rp1(2) = yd1*yd1
            rp1(3) = xd1*yd1
          case(3)
            rp1(1) = xd1*xd1
            rp1(2) = yd1*yd1
            rp1(3) = zd1*zd1
            rp1(4) = yd1*zd1
            rp1(5) = xd1*zd1
            rp1(6) = xd1*yd1
        end select
      endif
!
!  Set second derivative location pointers
!
      lopi = (nfreeatom(n1i).gt.0)
      lopj = (nfreeatom(n1j).gt.0)
      ix = 3*(nfreeatom(n1i) - 1) + 1
      iy = ix + 1
      iz = iy + 1
      jx = 3*(nfreeatom(n1j) - 1) + 1
      jy = jx + 1
      jz = jy + 1
    endif
!
!  If neither i nor j are being optimised and strain derivatives are not needed then atoms are not valid
!
    if (.not.lopi.and..not.lopj.and..not.lstr) then
      liok = .false.
      ljok = .false.
    endif
!
!  If i and j are not valid then there is no point continuing for this value of n1
!
    if (liok.and.ljok) then
      n2max = nneigh(nrk)
      do n2 = 1,n2max
        ind = ind + 1
        if (d2(ind).ne.0.0_dp) then
!
!  k -> neighbours of k
!
          n2k = k
          n2l = neighno(n2,nrk)
          xd2 = xneigh(n2,nrk)
          yd2 = yneigh(n2,nrk)
          zd2 = zneigh(n2,nrk)
!
          lkok = .true.
          llok = .true.
!
!  No point continuing beyond here unless k and l are valid
!
          if (lkok.and.llok) then
!
!  Complete remaining vectors
!
            xjk = xik - xd1
            yjk = yik - yd1
            zjk = zik - zd1
            xjl = xjk + xd2
            yjl = yjk + yd2
            zjl = zjk + zd2
!
!  Compute distances squared
!
            r2ik = xik*xik + yik*yik + zik*zik
            r2jl = xjl*xjl + yjl*yjl + zjl*zjl
!
!  Set up products for strain
!
            if (lstr) then
              select case(ndim)
                case(1)
                  rp2(1) = xd2*xd2
                case(2)
                  rp2(1) = xd2*xd2
                  rp2(2) = yd2*yd2
                  rp2(3) = xd2*yd2
                case(3)
                  rp2(1) = xd2*xd2
                  rp2(2) = yd2*yd2
                  rp2(3) = zd2*zd2
                  rp2(4) = yd2*zd2
                  rp2(5) = xd2*zd2
                  rp2(6) = xd2*yd2
              end select
            endif
!
            lopk = (nfreeatom(n2k).gt.0)
            lopl = (nfreeatom(n2l).gt.0)
            kx = 3*(nfreeatom(n2k) - 1) + 1
            ky = kx + 1
            kz = ky + 1
            lx = 3*(nfreeatom(n2l) - 1) + 1
            ly = lx + 1
            lz = ly + 1
!
            lsameijkl = .false.
            if (n1i.eq.n2k.and.n1j.eq.n2l.and.r2ik.lt.1.0d-8.and.r2jl.lt.1.0d-8) lsameijkl = .true.
!
            if ((lopi.or.lopk).and.n1i.ne.n2k) then
!
!  Second derivatives : i - k
!
              if (lopi.and.lopk) then
                ixc = ix
                iyc = iy
                izc = iz
                kxc = kx
                kyc = ky
                kzc = kz
                one1 = 1.0_dp
                one2 = 1.0_dp
              elseif (lopi) then
                ixc = ix
                iyc = iy
                izc = iz
                kxc = ix
                kyc = iy
                kzc = iz
                if (n1i.gt.n2k) then
                  one1 = 1.0_dp
                  one2 = 0.0_dp
                else
                  one1 = 0.0_dp
                  one2 = 1.0_dp
                endif
              elseif (lopk) then
                ixc = kx
                iyc = ky
                izc = kz
                kxc = kx
                kyc = ky
                kzc = kz
                if (n1i.gt.n2k) then
                  one1 = 1.0_dp
                  one2 = 0.0_dp
                else
                  one1 = 0.0_dp
                  one2 = 1.0_dp
                endif
              endif
!
              derv2(kxc,ixc) = derv2(kxc,ixc) + xd1*xd2*d2(ind)*one1
              derv2(kyc,ixc) = derv2(kyc,ixc) + xd1*yd2*d2(ind)*one1
              derv2(kzc,ixc) = derv2(kzc,ixc) + xd1*zd2*d2(ind)*one1
              derv2(kxc,iyc) = derv2(kxc,iyc) + yd1*xd2*d2(ind)*one1
              derv2(kyc,iyc) = derv2(kyc,iyc) + yd1*yd2*d2(ind)*one1
              derv2(kzc,iyc) = derv2(kzc,iyc) + yd1*zd2*d2(ind)*one1
              derv2(kxc,izc) = derv2(kxc,izc) + zd1*xd2*d2(ind)*one1
              derv2(kyc,izc) = derv2(kyc,izc) + zd1*yd2*d2(ind)*one1
              derv2(kzc,izc) = derv2(kzc,izc) + zd1*zd2*d2(ind)*one1
              if (.not.lsameijkl) then
                derv2(ixc,kxc) = derv2(ixc,kxc) + xd1*xd2*d2(ind)*one2
                derv2(ixc,kyc) = derv2(ixc,kyc) + xd1*yd2*d2(ind)*one2
                derv2(ixc,kzc) = derv2(ixc,kzc) + xd1*zd2*d2(ind)*one2
                derv2(iyc,kxc) = derv2(iyc,kxc) + yd1*xd2*d2(ind)*one2
                derv2(iyc,kyc) = derv2(iyc,kyc) + yd1*yd2*d2(ind)*one2
                derv2(iyc,kzc) = derv2(iyc,kzc) + yd1*zd2*d2(ind)*one2
                derv2(izc,kxc) = derv2(izc,kxc) + zd1*xd2*d2(ind)*one2
                derv2(izc,kyc) = derv2(izc,kyc) + zd1*yd2*d2(ind)*one2
                derv2(izc,kzc) = derv2(izc,kzc) + zd1*zd2*d2(ind)*one2
              endif
            endif
!
!  Second derivatives : i - l
!
            if ((lopi.or.lopl).and.n1i.ne.n2l) then
              if (lopi.and.lopl) then
                ixc = ix
                iyc = iy
                izc = iz
                lxc = lx
                lyc = ly
                lzc = lz
                one1 = 1.0_dp
                one2 = 1.0_dp
              elseif (lopi) then
                ixc = ix
                iyc = iy
                izc = iz
                lxc = ix
                lyc = iy
                lzc = iz
                if (n1i.gt.n2l) then
                  one1 = 1.0_dp
                  one2 = 0.0_dp
                else
                  one1 = 0.0_dp
                  one2 = 1.0_dp
                endif
              elseif (lopl) then
                ixc = lx
                iyc = ly
                izc = lz
                lxc = lx
                lyc = ly
                lzc = lz
                if (n1i.gt.n2l) then
                  one1 = 1.0_dp
                  one2 = 0.0_dp
                else
                  one1 = 0.0_dp
                  one2 = 1.0_dp
                endif
              endif
!
              derv2(lxc,ixc) = derv2(lxc,ixc) - xd1*xd2*d2(ind)*one1
              derv2(lyc,ixc) = derv2(lyc,ixc) - xd1*yd2*d2(ind)*one1
              derv2(lzc,ixc) = derv2(lzc,ixc) - xd1*zd2*d2(ind)*one1
              derv2(lxc,iyc) = derv2(lxc,iyc) - yd1*xd2*d2(ind)*one1
              derv2(lyc,iyc) = derv2(lyc,iyc) - yd1*yd2*d2(ind)*one1
              derv2(lzc,iyc) = derv2(lzc,iyc) - yd1*zd2*d2(ind)*one1
              derv2(lxc,izc) = derv2(lxc,izc) - zd1*xd2*d2(ind)*one1
              derv2(lyc,izc) = derv2(lyc,izc) - zd1*yd2*d2(ind)*one1
              derv2(lzc,izc) = derv2(lzc,izc) - zd1*zd2*d2(ind)*one1
              if (.not.lsameijkl) then
                derv2(ixc,lxc) = derv2(ixc,lxc) - xd1*xd2*d2(ind)*one2
                derv2(ixc,lyc) = derv2(ixc,lyc) - xd1*yd2*d2(ind)*one2
                derv2(ixc,lzc) = derv2(ixc,lzc) - xd1*zd2*d2(ind)*one2
                derv2(iyc,lxc) = derv2(iyc,lxc) - yd1*xd2*d2(ind)*one2
                derv2(iyc,lyc) = derv2(iyc,lyc) - yd1*yd2*d2(ind)*one2
                derv2(iyc,lzc) = derv2(iyc,lzc) - yd1*zd2*d2(ind)*one2
                derv2(izc,lxc) = derv2(izc,lxc) - zd1*xd2*d2(ind)*one2
                derv2(izc,lyc) = derv2(izc,lyc) - zd1*yd2*d2(ind)*one2
                derv2(izc,lzc) = derv2(izc,lzc) - zd1*zd2*d2(ind)*one2
              endif
            endif
!
!  Second derivatives : j - k
!
            if ((lopj.or.lopk).and.n1j.ne.n2k) then
              if (lopj.and.lopk) then
                jxc = jx
                jyc = jy
                jzc = jz
                kxc = kx
                kyc = ky
                kzc = kz
                one1 = 1.0_dp
                one2 = 1.0_dp
              elseif (lopj) then
                jxc = jx
                jyc = jy
                jzc = jz
                kxc = jx
                kyc = jy
                kzc = jz
                if (n1j.gt.n2k) then
                  one1 = 1.0_dp
                  one2 = 0.0_dp
                else
                  one1 = 0.0_dp
                  one2 = 1.0_dp
                endif
              elseif (lopk) then
                jxc = kx
                jyc = ky
                jzc = kz
                kxc = kx
                kyc = ky
                kzc = kz
                if (n1j.gt.n2k) then
                  one1 = 1.0_dp
                  one2 = 0.0_dp
                else
                  one1 = 0.0_dp
                  one2 = 1.0_dp
                endif
              endif
!
              derv2(kxc,jxc) = derv2(kxc,jxc) - xd1*xd2*d2(ind)*one1
              derv2(kyc,jxc) = derv2(kyc,jxc) - xd1*yd2*d2(ind)*one1
              derv2(kzc,jxc) = derv2(kzc,jxc) - xd1*zd2*d2(ind)*one1
              derv2(kxc,jyc) = derv2(kxc,jyc) - yd1*xd2*d2(ind)*one1
              derv2(kyc,jyc) = derv2(kyc,jyc) - yd1*yd2*d2(ind)*one1
              derv2(kzc,jyc) = derv2(kzc,jyc) - yd1*zd2*d2(ind)*one1
              derv2(kxc,jzc) = derv2(kxc,jzc) - zd1*xd2*d2(ind)*one1
              derv2(kyc,jzc) = derv2(kyc,jzc) - zd1*yd2*d2(ind)*one1
              derv2(kzc,jzc) = derv2(kzc,jzc) - zd1*zd2*d2(ind)*one1
              if (.not.lsameijkl) then
                derv2(jxc,kxc) = derv2(jxc,kxc) - xd1*xd2*d2(ind)*one2
                derv2(jxc,kyc) = derv2(jxc,kyc) - xd1*yd2*d2(ind)*one2
                derv2(jxc,kzc) = derv2(jxc,kzc) - xd1*zd2*d2(ind)*one2
                derv2(jyc,kxc) = derv2(jyc,kxc) - yd1*xd2*d2(ind)*one2
                derv2(jyc,kyc) = derv2(jyc,kyc) - yd1*yd2*d2(ind)*one2
                derv2(jyc,kzc) = derv2(jyc,kzc) - yd1*zd2*d2(ind)*one2
                derv2(jzc,kxc) = derv2(jzc,kxc) - zd1*xd2*d2(ind)*one2
                derv2(jzc,kyc) = derv2(jzc,kyc) - zd1*yd2*d2(ind)*one2
                derv2(jzc,kzc) = derv2(jzc,kzc) - zd1*zd2*d2(ind)*one2
              endif
            endif
!
!  Second derivatives : j - l
!
            if ((lopj.or.lopl).and.n1j.ne.n2l) then
              if (lopj.and.lopl) then
                jxc = jx
                jyc = jy
                jzc = jz
                lxc = lx
                lyc = ly
                lzc = lz
                one1 = 1.0_dp
                one2 = 1.0_dp
              elseif (lopj) then
                jxc = jx
                jyc = jy
                jzc = jz
                lxc = jx
                lyc = jy
                lzc = jz
                if (n1j.gt.n2l) then
                  one1 = 1.0_dp
                  one2 = 0.0_dp
                else
                  one1 = 0.0_dp
                  one2 = 1.0_dp
                endif
              elseif (lopl) then
                jxc = lx
                jyc = ly
                jzc = lz
                lxc = lx
                lyc = ly
                lzc = lz
                if (n1j.gt.n2l) then
                  one1 = 1.0_dp
                  one2 = 0.0_dp
                else
                  one1 = 0.0_dp
                  one2 = 1.0_dp
                endif
              endif
!
              derv2(lxc,jxc) = derv2(lxc,jxc) + xd1*xd2*d2(ind)*one1
              derv2(lyc,jxc) = derv2(lyc,jxc) + xd1*yd2*d2(ind)*one1
              derv2(lzc,jxc) = derv2(lzc,jxc) + xd1*zd2*d2(ind)*one1
              derv2(lxc,jyc) = derv2(lxc,jyc) + yd1*xd2*d2(ind)*one1
              derv2(lyc,jyc) = derv2(lyc,jyc) + yd1*yd2*d2(ind)*one1
              derv2(lzc,jyc) = derv2(lzc,jyc) + yd1*zd2*d2(ind)*one1
              derv2(lxc,jzc) = derv2(lxc,jzc) + zd1*xd2*d2(ind)*one1
              derv2(lyc,jzc) = derv2(lyc,jzc) + zd1*yd2*d2(ind)*one1
              derv2(lzc,jzc) = derv2(lzc,jzc) + zd1*zd2*d2(ind)*one1
              if (.not.lsameijkl) then
                derv2(jxc,lxc) = derv2(jxc,lxc) + xd1*xd2*d2(ind)*one2
                derv2(jxc,lyc) = derv2(jxc,lyc) + xd1*yd2*d2(ind)*one2
                derv2(jxc,lzc) = derv2(jxc,lzc) + xd1*zd2*d2(ind)*one2
                derv2(jyc,lxc) = derv2(jyc,lxc) + yd1*xd2*d2(ind)*one2
                derv2(jyc,lyc) = derv2(jyc,lyc) + yd1*yd2*d2(ind)*one2
                derv2(jyc,lzc) = derv2(jyc,lzc) + yd1*zd2*d2(ind)*one2
                derv2(jzc,lxc) = derv2(jzc,lxc) + zd1*xd2*d2(ind)*one2
                derv2(jzc,lyc) = derv2(jzc,lyc) + zd1*yd2*d2(ind)*one2
                derv2(jzc,lzc) = derv2(jzc,lzc) + zd1*zd2*d2(ind)*one2
              endif
            endif
            if (lstr) then
!
!  Strain - strain second derivatives
!
              do ns1 = 1,nstrains
                do ns2 = 1,nstrains
                  sderv2(ns2,ns1) = sderv2(ns2,ns1) + d2(ind)*rp2(ns2)*rp1(ns1)
                  sderv2(ns2,ns1) = sderv2(ns2,ns1) + d2(ind)*rp1(ns2)*rp2(ns1)
                enddo
              enddo
!
!  Internal - strain second derivatives
!
              do ns1 = 1,nstrains
                derv3(ix,ns1) = derv3(ix,ns1) - xd1*d2(ind)*rp2(ns1)
                derv3(iy,ns1) = derv3(iy,ns1) - yd1*d2(ind)*rp2(ns1)
                derv3(iz,ns1) = derv3(iz,ns1) - zd1*d2(ind)*rp2(ns1)
                derv3(jx,ns1) = derv3(jx,ns1) + xd1*d2(ind)*rp2(ns1)
                derv3(jy,ns1) = derv3(jy,ns1) + yd1*d2(ind)*rp2(ns1)
                derv3(jz,ns1) = derv3(jz,ns1) + zd1*d2(ind)*rp2(ns1)
                derv3(kx,ns1) = derv3(kx,ns1) - xd2*d2(ind)*rp1(ns1)
                derv3(ky,ns1) = derv3(ky,ns1) - yd2*d2(ind)*rp1(ns1)
                derv3(kz,ns1) = derv3(kz,ns1) - zd2*d2(ind)*rp1(ns1)
                derv3(lx,ns1) = derv3(lx,ns1) + xd2*d2(ind)*rp1(ns1)
                derv3(ly,ns1) = derv3(ly,ns1) + yd2*d2(ind)*rp1(ns1)
                derv3(lz,ns1) = derv3(lz,ns1) + zd2*d2(ind)*rp1(ns1)
              enddo
            endif
          endif
        endif
      enddo
    else
!
!  Increment ind pointer by number of missed terms
!
      ind = ind + nneigh(nrk)
    endif
  enddo
!
  return
  end
