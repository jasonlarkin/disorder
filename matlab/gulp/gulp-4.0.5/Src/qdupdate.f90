  subroutine qdupdate(lflags,ler1)
!
!  Updates the charges for explicit defect region 1 based
!  on the charges stored in the species arrays
!  Called only from setspec - because of 41/42 channel
!  handling problems will happen if called elsewhere!
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
!  Julian Gale, NRI, Curtin University, July 2005
!
  use configurations
  use defects
  use parallel
  use species
  implicit none
!
!  Passed variables
!
  logical            :: ler1(*)
  logical            :: lflags
!
!  Local variables
!
  integer(i4)        :: i
  integer(i4)        :: ix
  integer(i4)        :: iy
  integer(i4)        :: iz
  integer(i4)        :: j
  integer(i4)        :: k
  integer(i4)        :: n
  integer(i4)        :: nati
  integer(i4)        :: ncurr
  integer(i4)        :: nmi
  integer(i4)        :: nmindi
  integer(i4)        :: ns
  integer(i4)        :: nt
  integer(i4)        :: ntyp
  integer(i4)        :: ntypi
  logical            :: ldbi
  logical            :: ldqi
  real(dp)           :: oci
  real(dp)           :: qi
  real(dp)           :: qs
  real(dp)           :: radi
  real(dp)           :: xci
  real(dp)           :: yci
  real(dp)           :: zci
!
  rewind(41)
  rewind(42)
  do n = 1,ncfg
    if (ler1(n)) then
      read(42) ncurr,nreg1
      if (ioproc) write(41) ncurr,nreg1
      do i = 1,nreg1
        read(42) nati,ntypi,xci,yci,zci,qi,radi,oci,nmi,nmindi,ldbi,ldqi
        if (lflags) then
          read(42) ix,iy,iz
        endif
        do j = 1,nspec
          if (lmask(j)) then
            qs = qlspec(j)
            ns = natspec(j)
            ntyp = ntypspec(j)
            if (nati.eq.ns.and.(ntypi.eq.ntyp.or.ntyp.eq.0)) qi = qs
          endif
        enddo
        if (ioproc) write(41) nati,ntypi,xci,yci,zci,qi,radi,oci,nmi,nmindi,ldbi,ldqi
        if (lflags) then
          if (ioproc) write(41) ix,iy,iz
        endif
      enddo
    endif
    if (ldeflin(n)) then
      read(42) nvaca,ninte
      if (ioproc) write(41) nvaca,ninte
      nt = nvaca + ninte
      read(42) (ndptr(k),k=1,nt)
      if (ioproc) write(41) (ndptr(k),k=1,nt)
    endif
    if (lreldin(n)) then
      read(42) (ndptr(k),k=1,nreg1)
      if (ioproc) write(41) (ndptr(k),k=1,nreg1)
    endif
  enddo
!
  return
  end
