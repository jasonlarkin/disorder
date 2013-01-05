  module itcom1
    use datatypes, only : i4
    integer(i4) :: in
    integer(i4) :: is
    integer(i4) :: isym
    integer(i4) :: itmax
    integer(i4) :: level
    integer(i4) :: nout
    integer(i4) :: numwav
  end module itcom1

  module itcom2
    logical :: adapt
    logical :: betadt
    logical :: caseii
    logical :: halt
    logical :: partad
  end module itcom2

  module itcom3
    use datatypes, only : dp
    real(dp)  :: bdelnm
    real(dp)  :: betab
    real(dp)  :: cme
    real(dp)  :: delnnm
    real(dp)  :: delsnm
    real(dp)  :: ff
    real(dp)  :: gamma
    real(dp)  :: omega
    real(dp)  :: qa
    real(dp)  :: qt
    real(dp)  :: rho
    real(dp)  :: rrr
    real(dp)  :: sige
    real(dp)  :: sme
    real(dp)  :: specr
    real(dp)  :: spr
    real(dp)  :: srelpr
    real(dp)  :: stptst
    real(dp)  :: udnm
    real(dp)  :: zeta
  end module itcom3
