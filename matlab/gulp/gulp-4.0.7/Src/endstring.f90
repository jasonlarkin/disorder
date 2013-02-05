  subroutine endstring(string,length_string,end_string)
!
!  Finds the end of a string by search backwards for a non-space.
!  This is done because spaces can appear in file names.
!
!  On input:
!
!  string        = character string
!  length_string = integer length of string
!
!  On exit:
!
!  end_string    = integer first blank position in string after all non-blank characters
!
!   6/12 Created
!   8/12 Logic of search loop corrected to use end_string instead of length_string
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
!  Julian Gale, NRI, Curtin University, August 2012
!
  use datatypes
  implicit none
!
!  Passed variables
!
  integer(i4),                  intent(in)  :: length_string
  character(len=length_string), intent(in)  :: string
  integer(i4),                  intent(out) :: end_string
!
!  Local variables
!
  logical                      :: lfound
!
  lfound = .false.
  end_string = length_string + 1
  do while (.not.lfound.and.end_string.gt.1) 
    end_string = end_string - 1
    lfound = (string(end_string:end_string).ne.' ') 
  enddo
  end_string = end_string + 1
!
  end
