  subroutine sixlist(esix,lgrad1)
!
!  Subroutine for six-body potentials using list method
!
!   7/06 Created from sixmd.f
!  12/07 Unused variables removed
!   6/09 Site energy and virials added
!  11/11 Region-region energy contributions stored
!   4/12 Explicit virial calculation removed as no longer needed
!   4/12 xvir, yvir and zvir removed
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
  use configurations, only : nregionno
  use control,        only : latomicstress
  use current
  use derivatives
  use energies,       only : siteenergy, eregion2region
  use mdlogic
  use molecule
  use numbers,        only : sixth, third
  use optimisation
  use parallel
  use six
  use symmetry
  use times,          only : tsix
  implicit none
!
!  Passed variables
!
  real(dp), intent(inout)                      :: esix
  logical,  intent(in)                         :: lgrad1
!
!  Local variables
!
  integer(i4)                                  :: i
  integer(i4)                                  :: icell(3,5)
  integer(i4)                                  :: ii
  integer(i4)                                  :: ind
  integer(i4)                                  :: indvec
  integer(i4)                                  :: isatom(6)
  integer(i4)                                  :: ivec
  integer(i4)                                  :: j
  integer(i4)                                  :: jvec
  integer(i4)                                  :: k
  integer(i4)                                  :: kl
  integer(i4)                                  :: l
  integer(i4)                                  :: m
  integer(i4)                                  :: n
  integer(i4)                                  :: nlist
  integer(i4)                                  :: nregioni
  integer(i4)                                  :: nregionj
  integer(i4)                                  :: nsixtype
  integer(i4)                                  :: np
  logical                                      :: lsg1
  real(dp)                                     :: cputime
  real(dp)                                     :: e1d(15)
  real(dp)                                     :: e2d(1)
  real(dp)                                     :: e3d(1)
  real(dp)                                     :: eterm
  real(dp)                                     :: oci
  real(dp)                                     :: ocj
  real(dp)                                     :: ock
  real(dp)                                     :: ocl 
  real(dp)                                     :: ocm 
  real(dp)                                     :: ocn 
  real(dp)                                     :: ofct 
  real(dp)                                     :: rksix
  real(dp)                                     :: rko
  real(dp)                                     :: rprod(6,15)
  real(dp)                                     :: rstrdloc(6)
  real(dp)                                     :: time1
  real(dp)                                     :: time2
  real(dp)                                     :: sdist(15)
  real(dp)                                     :: svec(3,6,6)
  real(dp)                                     :: sxyz(3,6)
!
  time1 = cputime()
  lsg1 = (lstr.and.lgrad1)
!****************************
!  Loop over six-body list  *
!****************************
  do nlist = procid+1,nlist6md,nprocs
    np = nsixptr(nlist)
    nsixtype = nsixty(np)
    rksix = sixk(np)
    ind = ijind(nlist)
    j = ind/(numat+1)
    i = ind - j*(numat+1)
    ind = klind(nlist)
    l = ind/(numat+1)
    k = ind - l*(numat+1)
    ind = mnind(nlist)
    n = ind/(numat+1)
    m = ind - n*(numat+1)
!
    nregioni = nregionno(nsft+i)
    nregionj = nregionno(nsft+j)
!
    ii = icell61(nlist)
    if (ii.eq.0) then
      icell(1,1) = 0
      icell(2,1) = 0
      icell(3,1) = 0
    else
      icell(3,1) = (ii/100)-5
      ii = ii - 100*(icell(3,1)+5)
      icell(2,1) = (ii/10) - 5
      ii = ii - 10*(icell(2,1)+5)
      icell(1,1) = ii - 5
    endif
!
    ii = icell62(nlist)
    if (ii.eq.0) then
      icell(1,2) = 0
      icell(2,2) = 0
      icell(3,2) = 0
    else
      icell(3,2) = (ii/100)-5
      ii = ii - 100*(icell(3,2)+5)
      icell(2,2) = (ii/10) - 5
      ii = ii - 10*(icell(2,2)+5)
      icell(1,2) = ii - 5
    endif
!
    ii = icell63(nlist)
    if (ii.eq.0) then
      icell(1,3) = 0
      icell(2,3) = 0
      icell(3,3) = 0
    else
      icell(3,3) = (ii/100)-5
      ii = ii - 100*(icell(3,3)+5)
      icell(2,3) = (ii/10) - 5
      ii = ii - 10*(icell(2,3)+5)
      icell(1,3) = ii - 5
    endif
!
    ii = icell64(nlist)
    if (ii.eq.0) then
      icell(1,4) = 0
      icell(2,4) = 0
      icell(3,4) = 0
    else
      icell(3,4) = (ii/100)-5
      ii = ii - 100*(icell(3,4)+5)
      icell(2,4) = (ii/10) - 5
      ii = ii - 10*(icell(2,4)+5)
      icell(1,4) = ii - 5
    endif
!
    ii = icell65(nlist)
    if (ii.eq.0) then
      icell(1,5) = 0
      icell(2,5) = 0
      icell(3,5) = 0
    else
      icell(3,5) = (ii/100)-5
      ii = ii - 100*(icell(3,5)+5)
      icell(2,5) = (ii/10) - 5
      ii = ii - 10*(icell(2,5)+5)
      icell(1,5) = ii - 5
    endif
!**********************
!  Middle site 1 / i  *
!**********************
    oci = occuf(i)
    isatom(1) = i
    sxyz(1,1) = xclat(i)
    sxyz(2,1) = yclat(i)
    sxyz(3,1) = zclat(i)
!**********************
!  Middle site 2 / j  *
!**********************
    ocj = occuf(j)
    isatom(2) = j
    sxyz(1,2) = xclat(j) + icell(1,1)*r1x + icell(2,1)*r2x + icell(3,1)*r3x
    sxyz(2,2) = yclat(j) + icell(1,1)*r1y + icell(2,1)*r2y + icell(3,1)*r3y
    sxyz(3,2) = zclat(j) + icell(1,1)*r1z + icell(2,1)*r2z + icell(3,1)*r3z
!*******************************
!  First end site bonded to i  *
!*******************************
    ock = occuf(k)
    isatom(3) = k
    sxyz(1,3) = xclat(k) + icell(1,2)*r1x + icell(2,2)*r2x + icell(3,2)*r3x
    sxyz(2,3) = yclat(k) + icell(1,2)*r1y + icell(2,2)*r2y + icell(3,2)*r3y
    sxyz(3,3) = zclat(k) + icell(1,2)*r1z + icell(2,2)*r2z + icell(3,2)*r3z
!**************************
!  Second end site for i  *
!**************************
    ocl = occuf(l)
    isatom(4) = l
    sxyz(1,4) = xclat(l) + icell(1,3)*r1x + icell(2,3)*r2x + icell(3,3)*r3x
    sxyz(2,4) = yclat(l) + icell(1,3)*r1y + icell(2,3)*r2y + icell(3,3)*r3y
    sxyz(3,4) = zclat(l) + icell(1,3)*r1z + icell(2,3)*r2z + icell(3,3)*r3z
!*******************************
!  First end site bonded to j  *
!*******************************
    ocm = occuf(m)
    isatom(5) = m
    sxyz(1,5) = xclat(m) + icell(1,4)*r1x + icell(2,4)*r2x + icell(3,4)*r3x
    sxyz(2,5) = yclat(m) + icell(1,4)*r1y + icell(2,4)*r2y + icell(3,4)*r3y
    sxyz(3,5) = zclat(m) + icell(1,4)*r1z + icell(2,4)*r2z + icell(3,4)*r3z
!**************************
!  Second end site for j  *
!**************************
    ocn = occuf(n)
    isatom(6) = n
    sxyz(1,6) = xclat(n) + icell(1,5)*r1x + icell(2,5)*r2x + icell(3,5)*r3x
    sxyz(2,6) = yclat(n) + icell(1,5)*r1y + icell(2,5)*r2y + icell(3,5)*r3y
    sxyz(3,6) = zclat(n) + icell(1,5)*r1z + icell(2,5)*r2z + icell(3,5)*r3z
!********************************
!  Valid six-body term located  *
!********************************
!
!  Calculate vectors and remaining distances
!
    do ivec = 1,6
      do jvec = 1,6
        svec(1,jvec,ivec) = sxyz(1,ivec) - sxyz(1,jvec)
        svec(2,jvec,ivec) = sxyz(2,ivec) - sxyz(2,jvec)
        svec(3,jvec,ivec) = sxyz(3,ivec) - sxyz(3,jvec)
      enddo
    enddo
    indvec = 0
    do ivec = 1,5
      do jvec = ivec+1,6
        indvec = indvec + 1
        sdist(indvec) = svec(1,jvec,ivec)**2 + svec(2,jvec,ivec)**2 + svec(3,jvec,ivec)**2
        sdist(indvec) = sqrt(sdist(indvec))
      enddo
    enddo
!
    ofct = oci*ocj*ock*ocl*ocm*ocn
    rko = rksix*ofct
    call sixbody(np,nsixtype,sdist,eterm,e1d,e2d,e3d,rko,lgrad1,.false.,.false.)
    esix = esix + eterm
!
!  Assign inter-region energy to i-j pair only - sixbody terms shouldn't cross regions anyway
!
    eregion2region(nregionj,nregioni) = eregion2region(nregionj,nregioni) + eterm
!
    do ivec = 1,6
      siteenergy(isatom(ivec)) = siteenergy(isatom(ivec)) + 0.5_dp*third*eterm
    enddo
!***********************************
!  Cross out of plane derivatives  *
!***********************************
!
!  Set up strain products
!
    if (lsg1) then
      call sixstrterms(ndim,rprod,svec)
    endif
!***********************
!  Strain derivatives  *
!***********************
    if (lsg1) then
!
!  First strain derivatives
!
      rstrdloc(1:nstrains) = 0.0_dp
      do ivec = 1,15
        do kl = 1,nstrains
          rstrdloc(kl) = rstrdloc(kl) + e1d(ivec)*rprod(kl,ivec)
        enddo
      enddo
      do kl = 1,nstrains
        rstrd(kl) = rstrd(kl) + rstrdloc(kl)
      enddo
      if (latomicstress) then
        do ivec = 1,6
          do kl = 1,nstrains
            atomicstress(kl,isatom(ivec)) = atomicstress(kl,isatom(ivec)) + sixth*rstrdloc(kl)
          enddo
        enddo
      endif
    endif
!*************************
!  Internal derivatives  *
!*************************
    if (lgrad1) then
      indvec = 0
      do ivec = 1,5
        do jvec = ivec+1,6
          indvec = indvec + 1
          xdrv(isatom(ivec)) = xdrv(isatom(ivec)) + svec(1,jvec,ivec)*e1d(indvec)
          ydrv(isatom(ivec)) = ydrv(isatom(ivec)) + svec(2,jvec,ivec)*e1d(indvec)
          zdrv(isatom(ivec)) = zdrv(isatom(ivec)) + svec(3,jvec,ivec)*e1d(indvec)
          xdrv(isatom(jvec)) = xdrv(isatom(jvec)) + svec(1,ivec,jvec)*e1d(indvec)
          ydrv(isatom(jvec)) = ydrv(isatom(jvec)) + svec(2,ivec,jvec)*e1d(indvec)
          zdrv(isatom(jvec)) = zdrv(isatom(jvec)) + svec(3,ivec,jvec)*e1d(indvec)
        enddo
      enddo
    endif
!
!  End of outer loop
!
  enddo
!
!  Timing
!
  time2 = cputime()
  tsix = tsix + time2 - time1
!
  return
  end
