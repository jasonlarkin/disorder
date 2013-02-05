  subroutine symprod
!
!  Creates a table of bulk rotation operator products
!  and operator inverses for use in symmetrising the
!  second derivative matrix.
!
!   2/98 Now stores alternative possible inverse matrices
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
!  Julian Gale, Curtin University, June 2005
!
  use iochannels
  use parallel
  use symmetry
  implicit none
!
!  Local variables
!
  integer(i4)      :: i
  integer(i4)      :: j
  integer(i4)      :: k
  integer(i4)      :: l
  integer(i4)      :: m
  logical          :: lfound
  real(dp)         :: diff
  real(dp)         :: rm(3,3)
  real(dp)         :: tm(3,3)
!****************************
!  Generate product tables  *
!****************************
  do i = 1,ngo
    do j = 1,3
      rm(1,j) = rop(1,j,i)
      rm(2,j) = rop(2,j,i)
      rm(3,j) = rop(3,j,i)
    enddo
    do j = 1,ngo
      do k = 1,3
        do l = 1,3
          tm(l,k) = rm(l,1)*rop(1,k,j) + rm(l,2)*rop(2,k,j) + rm(l,3)*rop(3,k,j)
        enddo
      enddo
      lfound = .false.
      k = 0
      do while (.not.lfound.and.k.lt.ngo)
        k = k + 1
        diff = 0.0_dp
        do l = 1,3
          do m = 1,3
            diff = diff + abs(tm(m,l) - rop(m,l,k))
          enddo
        enddo
        if (diff.lt.1.0d-6) then
          iptab(i,j) = k
          lfound = .true.
        endif
      enddo
      if (.not.lfound) then
        if (ioproc) then
          write(ioout,'(/,''  **** Operator is missing ****'',/)')
        endif
        call stopnow('symprod')
      endif
    enddo
!
!  Find inverse in product table
!
    j = 0
    inverse(i) = 0
    do while (j.lt.ngo.and.inverse(i).eq.0)
      j = j + 1
      if (iptab(i,j).eq.1) inverse(i) = j
    enddo
  enddo
!
  return
  end
