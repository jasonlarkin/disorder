  module m_pr
    use datatypes
    integer(i4),                          save :: ndof_atm
    integer(i4),                          save :: ndof_baro
    logical,                              save :: lpr_baro_flx = .false.
    logical,                              save :: lpr_baro_iso = .false.
    logical,                              save :: lpr_baro_ort = .false.
    logical,                              save :: lpr_globalt = .true.
    logical,                              save :: lpr_stress = .false.
    logical,                              save :: lpr_thermo = .false.
    logical,                              save :: lpflxinput = .false.
    logical,                              save :: lpisoinput = .false.
    logical,                              save :: lpflxoutput = .false.
    logical,                              save :: lpisooutput = .false.
    real(dp),                             save :: bmass_flx
    real(dp),                             save :: bmass_iso
    real(dp),                             save :: dt
    real(dp),                             save :: dth
    real(dp),                             save :: pr_ekin
    real(dp),                             save :: pr_ekinbaro
    real(dp),                             save :: pr_ekintarget_atm
    real(dp),                             save :: pr_ekintarget_baro
    real(dp),                             save :: hinv(3,3)
    real(dp),                             save :: p_flx(3,3)
    real(dp),                             save :: p_iso
    real(dp),                             save :: pr_boltz
    real(dp),                             save :: pr_cons
    real(dp),                             save :: pr_press
    real(dp),                             save :: pr_temp
    real(dp),                             save :: pr_kinstress(3,3)
    real(dp),                             save :: pr_stress(3,3)
    real(dp),                             save :: pr_target_press
    real(dp),                             save :: pr_target_press_tens(3,3)
    real(dp),                             save :: pr_target_temp
    real(dp),                             save :: taub
    real(dp),                             save :: taut
    real(dp),                             save :: virial_m(3,3)
    real(dp),    dimension(:),   pointer, save :: pekin 
    real(dp),    dimension(:),   pointer, save :: taubcfg
    real(dp),    dimension(:),   pointer, save :: tautcfg
    real(dp),    dimension(:),   pointer, save :: pr_conscfg
  end module m_pr
