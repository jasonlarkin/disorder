!*****************************************************************************!
!                                                                            !
!       ******                 MODULE pdfvariables                ******     !
!                              ================                              !
!                                                                            !
! This module stores some PDF variables for use with CML                     !
!                                                                            !
! If you beleve you have found a bug in this module contact the author.      !
!                                                                            !
!                                               Beth Cope [ers29] (c) 2007   !
!                                                          ers29@cam.ac.uk   !
!                                                                            !
!****************************************************************************!
!
!   8/10 Length of iounits increased to 6.
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
!  Julian Gale, NRI, Curtin University, 2010
!
  module m_pdfvariables
    use datatypes
    implicit none
    save
!
!  PDF statistics, stored for CML output
!
    real(kind=dp)      :: widmax       ! maximum peak width squared
    integer(kind=i4)   :: np           ! number of pairs counted
    real(kind=dp)      :: wmin_inunits ! min w in i/o units
    real(kind=dp)      :: wmax_inunits ! max w in i/o units
    character(len=6)   :: iounits      ! string name i/o unit
    real(kind=dp)      :: numdensity   ! numberdensity(1/A^3)
    real(kind=dp)      :: sum_cbbar_sq ! (Sum{c_i bbar_i} )^2 
  end module m_pdfvariables
