  subroutine lower(mcv,freq,nphonatc,nphonatptr,ncfoc,iocptr,maxd2,eigr)
!
!  Lowers the symmetry of a system according to the imaginary eigenvalue
!  eigenvectors.
!
!   3/02 Created from peigen/peigeng
!   7/02 Modified to allow for region 1 phonons only
!   7/02 Probable bug in assignment of shift atoms in xcfg corrected -
!        displacements were being applied to i not j
!   4/04 Lowering of shell position added
!   3/07 Gauss renamed to GULP_gauss
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
!  Julian Gale, NRI, Curtin University, March 2007
!
  use configurations, only : xcfg, ycfg, zcfg
  use control
  use current
  use general, only : lowerscale
  use iochannels
  use parallel
  use shell,   only : ncsptr
  implicit none
!
!  Passed variables
!
  integer(i4) :: iocptr(*)
  integer(i4) :: maxd2
  integer(i4) :: mcv
  integer(i4) :: ncfoc
  integer(i4) :: nphonatc
  integer(i4) :: nphonatptr(*)
  real(dp)    :: freq(*)
  real(dp)    :: eigr(maxd2,*)
!
!  Local variables
!
  integer(i4) :: i
  integer(i4) :: j
  integer(i4) :: jj
  integer(i4) :: jjs
  integer(i4) :: ind
  integer(i4) :: nimag
  real(dp)    :: rmat(3,4)
  real(dp)    :: scale
!******************************************************
!  Lowering of symmetry according to imaginary modes  *
!******************************************************
  if (index(keyword,'lowe').ne.0) then
    nimag=1
    do while (freq(nimag).lt.-0.5_dp.and.nimag.le.mcv)
      ind = 0
      scale = lowerscale*abs(freq(nimag))
      do i = 1,ncfoc
        rmat(1,4) = scale*eigr(ind+1,nimag)
        rmat(2,4) = scale*eigr(ind+2,nimag)
        rmat(3,4) = scale*eigr(ind+3,nimag)
        if (ndim.eq.3) then
          do j = 1,3
            rmat(1,j) = rv(1,j)
            rmat(2,j) = rv(2,j)
            rmat(3,j) = rv(3,j)
          enddo
          call GULP_gauss(3_i4,3_i4,1_i4,rmat)
        elseif (ndim.eq.2) then
          do j = 1,2
            rmat(1,j) = rv(1,j)
            rmat(2,j) = rv(2,j)
          enddo
          call GULP_gauss(2_i4,3_i4,1_i4,rmat)
        elseif (ndim.eq.1) then
          rmat(1,4) = rv(1,1)
        endif
        do j = 1,nphonatc
          if (iocptr(j).eq.i) then
            jj = nphonatptr(j)
            xcfg(nsft+jj) = xcfg(nsft+jj) + rmat(1,4)
            ycfg(nsft+jj) = ycfg(nsft+jj) + rmat(2,4)
            zcfg(nsft+jj) = zcfg(nsft+jj) + rmat(3,4)
!
!  Move associated shells too
!
            jjs = ncsptr(jj)
            if (jjs.gt.0) then
              xcfg(nsft+jjs) = xcfg(nsft+jjs) + rmat(1,4)
              ycfg(nsft+jjs) = ycfg(nsft+jjs) + rmat(2,4)
              zcfg(nsft+jjs) = zcfg(nsft+jjs) + rmat(3,4)
            endif
!
          endif
        enddo
        ind = ind + 3
      enddo
      nimag = nimag + 1
    enddo
    nimag = nimag - 1
    if (ioproc) then
      if (nimag.gt.1) then
        write(ioout,'(''  Symmetry lowered according to '',i3,'' imaginary mode eigenvectors'',/)') nimag
      elseif (nimag.eq.1) then
        write(ioout,'(''  Symmetry lowered according to one imaginary mode eigenvector'',/)')
      else
        write(ioout,'(''  No imaginary modes present - current symmetry is correct'',/)')
      endif
    endif
  endif
!
  return
  end
