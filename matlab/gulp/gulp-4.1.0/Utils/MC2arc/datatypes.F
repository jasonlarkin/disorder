      module datatypes

#ifdef CRAY
        integer, parameter :: i4  = selected_int_kind(9)
        integer, parameter :: i2  = selected_int_kind(9)
#else
        integer, parameter :: i4  = selected_int_kind(9)
        integer, parameter :: i2  = selected_int_kind(4)
#endif
        integer, parameter :: dp  = kind(1.0d0)
        integer, parameter :: sp  = kind(1.0)
        integer, parameter :: dpc = kind((1.0d0,1.0d0))
        integer, parameter :: spc = kind((1.0,1.0))

      end module datatypes
