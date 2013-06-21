  function cdabs(a)
  use datatypes
!
  complex(dpc) :: a
  real(dp)     :: cdabs
!
  cdabs = sqrt(real(a)*conjg(a))
!
  return
  end
