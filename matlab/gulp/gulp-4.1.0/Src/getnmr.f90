  subroutine getnmr(nasym,iatn,qa,qnmr)
!
!  Compute NMR chemical shifts using the charge state of the atoms.
!  Intended to be used in conjunction with the Pacha charges.
!
!  12/12 NMR f value now an input quantity
!  12/12 Sign of f*q changed to match Pacha code rather than paper.
!  12/12 Modified to include use of averages for gap
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
!  Julian Gale, Curtin University, December 2012
!
  use datatypes
  use constants,    only : autoev
  use current,      only : nspecptr, neqv
  use reallocate
  use species,      only : lnmrinspec, nmrspec, nspec
  implicit none
!
!  Passed variables
!
  integer(i4),      intent(in)   :: nasym           ! Number of atoms in asymmetric unit
  integer(i4),      intent(in)   :: iatn(nasym)     ! Atomic numbers of atoms
  real(dp),         intent(in)   :: qa(nasym)       ! Charges of atoms
  real(dp),         intent(out)  :: qnmr(nasym)     ! On return, this is the chemical shift
!
!  Local variables
!
  integer(i4)                    :: i
  integer(i4)                    :: j
  integer(i4)                    :: noftype
  integer(i4)                    :: noftypewtd
  integer(i4)                    :: ns
  integer(i4), allocatable, save :: nsptr(:)
  integer(i4)                    :: status
  real(dp)                       :: average_dia
  real(dp)                       :: average_pu
  real(dp)                       :: average_q
  real(dp),                 save :: dia_coeff(3,5)
  real(dp)                       :: deltaE
  real(dp)                       :: cs_dia
  real(dp)                       :: cs_para
  real(dp)                       :: erg
  real(dp)                       :: f
  real(dp)                       :: fq
  real(dp)                       :: Pu
  real(dp)                       :: q
  real(dp)                       :: R0
  real(dp)                       :: shiftNMR
  real(dp),    allocatable, save :: site_dia(:)
  real(dp),    allocatable, save :: site_pu(:)
  real(dp)                       :: z
  real(dp)                       :: zj
!
  data (dia_coeff(j,1),j=1,3)/-2.92634d0,2.56859d0,5.25192d0/
  data (dia_coeff(j,2),j=1,3)/4.07000d0,0.551632d0,0.182718d0/
  data (dia_coeff(j,3),j=1,3)/0.204929d0,-0.137827d-1,0.217841d-2/
  data (dia_coeff(j,4),j=1,3)/-0.212362d-2,0.182593d-3,-0.803085d-4/
  data (dia_coeff(j,5),j=1,3)/ 0.103515d-4,-0.878065d-6,0.554069d-6/
!
!  Allocate local memory
!
  allocate(nsptr(nasym),stat=status)
  if (status/=0) call outofmemory('getnmr','nsptr')
  allocate(site_dia(nasym),stat=status)
  if (status/=0) call outofmemory('getnmr','site_dia')
  allocate(site_pu(nasym),stat=status)
  if (status/=0) call outofmemory('getnmr','site_pu')
!
!  Set constants
!
  erg = 1.0d-7
  R0 = 1.23_dp
  f = 966.0_dp
!
!  Initialise NMR shifts to zero
!
  qnmr(1:nasym) = 0.0_dp
!
!  Loop over atoms to compute terms on sites and accumulate averages
!
  specloop: do ns = 1,nspec
    if (lnmrinspec(ns)) then
!
!  Initialise averages for this species
!
      average_dia = 0.0_dp
      average_pu  = 0.0_dp
      average_q   = 0.0_dp
!
!  Loop over atoms looking for species match
!
      noftype = 0
      noftypewtd = 0
      do i = 1,nasym
        if (nspecptr(i).eq.ns) then
!
!  Store site number
!
          noftype = noftype + 1
          nsptr(noftype) = i
          q = qa(i)
!
!  Compute the effective charge
!
          z = dble(iatn(i)) - q
!
!  Increment number of atoms of this species type counter
!
          noftypewtd = noftypewtd + neqv(i)
!
!  Compute diamagnetic contribution using parameters of Saxena and Narasimhan, IJQC, 1, 731-749 (1967).
!
          cs_dia = 0.0_dp
          zj = 1.0_dp
          if (q.lt.0.0_dp) then
            do j = 1,5
              cs_dia = cs_dia + (dia_coeff(1,j) + q*dia_coeff(3,j))*zj
              zj = zj*z
            enddo
          else
            do j = 1,5
              cs_dia = cs_dia + (dia_coeff(1,j) + q*dia_coeff(2,j))*zj
              zj = zj*z
            enddo
          endif
          site_dia(i) = 10_dp*cs_dia   ! cs_dia is x 10-5, but qnmr is expressed in ppm
!
!  Compute paramagnetic contribution using formula from Comptes Rendus Chimie, 13, 69-96 (2010)
!
          Pu = 1.5_dp*(1.0_dp - q*q/9.0_dp)
          site_pu(i) = Pu
!
!  Add to averages
!
          average_dia = average_dia + site_dia(i)*dble(neqv(i))
          average_pu  = average_pu  + site_pu(i)*dble(neqv(i))
          average_q   = average_q   + q*dble(neqv(i))
        endif
!
!  End of loop over asymmetric unit
!
      enddo
!
!  If number of species is zero then skip the rest of the steps
!
      if (noftype.eq.0) cycle specloop
!
!  Divide averages by number of species in total
!
      average_dia = average_dia/dble(noftypewtd)
      average_pu  = average_pu/dble(noftypewtd)
      average_q   = average_q/dble(noftypewtd)
!
!  Compute deltaE
!
      shiftNMR = (nmrspec(1,ns) - nmrspec(2,ns)) 
      fq = nmrspec(3,ns)
      deltaE = f*average_pu*R0*(1 + fq*average_q)/(average_dia - shiftNMR + erg) 
!
!  Now go back and compute NMR for sites
!
      do j = 1,noftype
!
!  Set pointer back to asymmetric unit number
!
        i = nsptr(j)
!
!  Set the reference chemical shift
!
        qnmr(i) = nmrspec(1,ns)
!
!  Subtract diamagnetic contribution
!
        qnmr(i) = qnmr(i) - site_dia(i)
!
!  Compute paramagnetic contribution using formula from Comptes Rendus Chimie, 13, 69-96 (2010)
!
        q = qa(i)
        Pu = site_pu(i)
!
!  Sign changed for f.q term relative to paper as Marc's code has positive sign
!
        cs_para = f*R0*(1.0_dp + fq*q)*Pu/(deltaE + erg)
        qnmr(i) = qnmr(i) + cs_para
      enddo
    endif
  enddo specloop
!
!  Free local memory
!
  deallocate(site_pu,stat=status)
  if (status/=0) call deallocate_error('getnmr','site_pu')
  deallocate(site_dia,stat=status)
  if (status/=0) call deallocate_error('getnmr','site_dia')
  deallocate(nsptr,stat=status)
  if (status/=0) call deallocate_error('getnmr','nsptr')
!
  return
  end
