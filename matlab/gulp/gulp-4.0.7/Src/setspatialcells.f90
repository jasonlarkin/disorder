  subroutine setspatialcells(cutoff2,ncellsearch,rnearestx,rnearesty,rnearestz,ncells,ncellix,ncelliy,ncelliz)
!
!  Compute the cells that are actually needed as a linear list
!
!   4/08 Created
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
!  Julian Gale, NRI, Curtin University, April 2008
!
  use datatypes
  implicit none
!
!  Passed variables
!
  integer(i4), intent(in)                      :: ncellsearch(3)
  integer(i4), intent(out)                     :: ncells
  integer(i4), intent(out)                     :: ncellix(*)
  integer(i4), intent(out)                     :: ncelliy(*)
  integer(i4), intent(out)                     :: ncelliz(*)
  real(dp),    intent(in)                      :: cutoff2
  real(dp),    intent(in)                      :: rnearestx
  real(dp),    intent(in)                      :: rnearesty
  real(dp),    intent(in)                      :: rnearestz
!
!  Local variables
!
  integer(i4)                                  :: imx
  integer(i4)                                  :: imy
  integer(i4)                                  :: imz
  integer(i4)                                  :: inx
  integer(i4)                                  :: iny
  integer(i4)                                  :: inz
  real(dp)                                     :: rx
  real(dp)                                     :: rx2
  real(dp)                                     :: ry
  real(dp)                                     :: ry2
  real(dp)                                     :: rz
  real(dp)                                     :: rz2
  real(dp)                                     :: r2
!
!  Initialise number of cells
!
  ncells = 0
!
!  Loop over neighbouring cells
!         
  do imz = -ncellsearch(3),ncellsearch(3)
    if (imz.ne.0) then
      inz = abs(imz) - 1
    else
      inz = 0
    endif
    rz = dble(inz)*rnearestz
    rz2 = rz*rz
    do imy = -ncellsearch(2),ncellsearch(2)
      if (imy.ne.0) then
        iny = abs(imy) - 1
      else
        iny = 0
      endif
      ry = dble(iny)*rnearesty
      ry2 = ry*ry
      do imx = -ncellsearch(1),ncellsearch(1)
        if (imx.ne.0) then
          inx = abs(imx) - 1
        else
          inx = 0
        endif
        rx = dble(inx)*rnearestx
        rx2 = rx*rx
!
!  Compute squared distance from middle cell
!
        r2 = rx2 + ry2 + rz2
!
!  If cell is within this distance then include in the list
!
        if (r2.lt.cutoff2) then
          ncells = ncells + 1
          ncellix(ncells) = imx
          ncelliy(ncells) = imy
          ncelliz(ncells) = imz
        endif
!               
!  End loops over neighbouring cells
!  
      enddo
    enddo
  enddo
!
  return
  end
