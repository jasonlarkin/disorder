  subroutine bondq(lmain)
!
!  Subroutine for calculating charges according bond increments
!
!  lmain = .true.  => call from gulp, one off calc with output
!        = .false. => call from energy, needed for charges
!
!   5/07 Created based on gasteiger.f90
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
!  Julian Gale, NRI, Curtin University, May 2007
!
  use bondcharge
  use control
  use configurations
  use current
  use element
  use energies
  use iochannels
  use parallel
  use symmetry
  use times
  implicit none
!
!  Passed variables
!
  logical, intent(in)                          :: lmain
!
!  Local variables
!
  integer(i4)                                  :: i
  integer(i4)                                  :: ifull
  integer(i4)                                  :: j
  integer(i4)                                  :: nbond
  integer(i4)                                  :: nbq
  integer(i4)                                  :: ni
  integer(i4)                                  :: nj
  integer(i4)                                  :: nti
  integer(i4)                                  :: ntj
  logical                                      :: lfound
  real(dp)                                     :: cputime
  real(dp)                                     :: deltaQ
  real(dp)                                     :: time1
  real(dp)                                     :: time2
!
  time1 = cputime()
!
!  Initialise charges and store for convergence check
!
  do i = 1,nasym
    qa(i) = 0.0_dp
  enddo
!*********************************************************
!  Compute charges based on the bonds within the system  *
!*********************************************************
!
!  Loop over atoms
!
  do i = 1,nasym
    ni = iatn(i)
    nti = natype(i)
    ifull = nrel2(i)
!
!  Loop over bonds to atom i
!
    do nbond = 1,nbonds(ifull)
      j = nbonded(nbond,ifull)
      nj = nat(j)
      ntj = nftype(j)
!
!  If i and j are the same atom type then don't bother looking for a bond increment
!
      if (ni.ne.nj.or.nti.ne.ntj) then
        nbq = 0
        lfound = .false.
        do while (.not.lfound.and.nbq.lt.nbondQ)
          nbq = nbq + 1
          if (nbondQspec1(nbq).eq.ni.and.(nbondQtyp1(nbq).eq.nti.or.nbondQtyp1(nbq).eq.0)) then
            if (nbondQspec2(nbq).eq.nj.and.(nbondQtyp2(nbq).eq.ntj.or.nbondQtyp2(nbq).eq.0)) then
              lfound = .true.
              deltaQ = bondQincrement(nbq)
            endif
          elseif (nbondQspec1(nbq).eq.nj.and.(nbondQtyp1(nbq).eq.ntj.or.nbondQtyp1(nbq).eq.0)) then
            if (nbondQspec2(nbq).eq.ni.and.(nbondQtyp2(nbq).eq.nti.or.nbondQtyp2(nbq).eq.0)) then
              lfound = .true.
              deltaQ = - bondQincrement(nbq)
            endif
          endif
        enddo
        if (lfound) then
          qa(i) = qa(i) + deltaQ
        endif
      endif
    enddo
  enddo
!
!  Store charges in configurational array
!
  do i = 1,nasym
    qlcfg(nsft+i) = qa(i)
  enddo
!*******************
!  Output results  *
!*******************
  if (lmain.and.ioproc) then
    write(ioout,'(//,''  Final Bond Increment charges:'',/)')
    write(ioout,'(''--------------------------------------------------------------------------------'')')
    write(ioout,'(''    Atom no.            Atomic No.             Charge'')')
    write(ioout,'(''--------------------------------------------------------------------------------'')')
    do i = 1,nasym
      write(ioout,'(6x,i4,18x,i2,16x,f10.7)') i,iatn(i),qa(i)
    enddo
    write(ioout,'(''--------------------------------------------------------------------------------'')')
  endif
!
!  Timing
!
  time2 = cputime()
  teem = teem + time2 - time1
!
  return
  end
