function x0 = m_af_create_structure( AF )


if isfield(AF.x0,'luc')
    x0 = m_lj_luc_create( AF.x0 )
elseif isfield(AF.x0,'alloy')
    
end

%set to gamma
AF.x0.kptlist = [0 0 0];

x0_dummy = AF.x0;

%output x0 structure
save(strcat(AF.str.main,'/x0.mat'), '-struct', 'x0_dummy');

end