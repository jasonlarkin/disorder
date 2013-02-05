  subroutine BOselftrial(eboQself,ntrialatom,nptrtrialatom)
!
!  Calculates the self energy for the bond order charges.
!  Subset of atoms version.
!
!  On exit :
!
!  eboQself     = the self energy of the bond order charges
!
!   1/08 Created from boself.f
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
  use datatypes
  use bondorderdata
  use current
  use parallel,        only : procid, nprocs
  use times
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)                      :: ntrialatom
  integer(i4), intent(in)                      :: nptrtrialatom(ntrialatom)
  real(dp),    intent(out)                     :: eboQself
!
!  Local variables
!
  integer(i4)                                  :: i
  integer(i4)                                  :: ii
  integer(i4)                                  :: m
  integer(i4)                                  :: nt
  integer(i4)                                  :: nati
  integer(i4)                                  :: ntypi
  real(dp)                                     :: cputime
  real(dp)                                     :: dneqv
  real(dp)                                     :: esum
  real(dp)                                     :: oci
  real(dp)                                     :: qi
  real(dp)                                     :: rho
  real(dp)                                     :: t1
  real(dp)                                     :: t2
!
  t1 = cputime()
!
!  Initialise Bond Order charge self energy
!
  eboQself = 0.0_dp
!*************************************
!  Calculate self-energy of charges  *
!*************************************
  do nt = 1+procid,ntrialatom,nprocs
    i = nptrtrialatom(nt)
    nati = nat(i)
    ntypi = nftype(i)
    oci = occuf(i)
    ii = i
    dneqv = oci*oci
    qi = qf(ii)
    do m = 1,nboQ0
      esum = 0.0_dp
      if (nati.eq.nBOspecQ0(m).and.(ntypi.eq.nBOtypQ0(m).or.nBOtypQ0(m).eq.0)) then
        rho = BOq0rho(m)
        if (BOq0ref(m).gt.0.0d0.and.qi.gt.BOq0ref(m)) then
!
!  Cation case
!
          esum = dneqv*BOq0pot(m)*exp(-rho/(qi - BOq0ref(m)))
        elseif (BOq0ref(m).lt.0.0d0.and.qi.lt.BOq0ref(m)) then
!
!  Anion case
!
          esum = dneqv*BOq0pot(m)*exp(rho/(qi - BOq0ref(m)))
        endif
!
      endif
      eboQself = eboQself + esum
    enddo
  enddo
!
  t2 = cputime()
  tbondorder = tbondorder + t2 - t1
!
  return
  end
