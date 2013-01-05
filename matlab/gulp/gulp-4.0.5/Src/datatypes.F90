  module datatypes

!
!  i4 and i2 define the size of integer*4 and integer*2 in the code
!
!  i4_limit and i2_limit define a limit on the size of an integer
!  that can be squared in the code without causing integer overflow.
!
!  Largest integers are:
!
!  integer*4   2147483647 => i4_limit ~ 46300
!  integer*2        32768 => i2_limit ~   181
!
#ifdef CRAY
    integer, parameter :: i4  = selected_int_kind(5)
    integer, parameter :: i2  = selected_int_kind(3)
    integer, parameter :: i4_limit = 46300
    integer, parameter :: i2_limit = 46300
#else
    integer, parameter :: i4  = selected_int_kind(5)
    integer, parameter :: i2  = selected_int_kind(3)
    integer, parameter :: i4_limit = 46300
    integer, parameter :: i2_limit = 181
#endif
    integer, parameter :: dp  = kind(1.0d0)
    integer, parameter :: sp  = kind(1.0)
    integer, parameter :: dpc = kind((1.0d0,1.0d0))
    integer, parameter :: spc = kind((1.0,1.0))


  end module datatypes
