function vel =...
    m_gulp_vel_si(kpt,NUM_ATOMS_UCELL,MASS,str_main,str_matlab,name)
%--------------------------------------------------------------------------
%freq = gulp_lj_vel(kpt,NUM_ATOMS_UCELL)
%--------------------------------------------------------------------------

%--------------------------------------------------------------------------
%This is required to get the kpt + dk to properly input to GULP
    format long
%--------------------------------------------------------------------------

constant = m_constant;
si = m_si;

gulp.dk = 1E-5;

%2) Input dk kpts to measure group velocities
	vel = zeros(3*NUM_ATOMS_UCELL,3);		

for idim = 1:3
    if kpt(idim)==0.5
        freq = gulp_lj_freq(kpt,NUM_ATOMS_UCELL,MASS,...
            str_main,str_matlab,name);
        kpt(idim) = kpt(idim) - gulp.dk;
        freq_mdk = gulp_lj_freq(kpt,NUM_ATOMS_UCELL,MASS,...
            str_main,str_matlab,name);
        vel(:,idim) = (( freq - freq_mdk )/ gulp.dk / 1 )*constant.ang2m;
    %Put kpt back to orig
        kpt(idim) = kpt(idim) + gulp.dk;

    elseif kpt(idim)==-0.5
        freq = gulp_lj_freq(kpt,NUM_ATOMS_UCELL,MASS,...
            str_main,str_matlab,name);
        kpt(idim) = kpt(idim) + gulp.dk;
        freq_pdk = gulp_lj_freq(kpt,NUM_ATOMS_UCELL,MASS,...
            str_main,str_matlab,name);
        vel(:,idim) = (( freq_pdk - freq )/ gulp.dk / 1 )*constant.ang2m;
    %Put kpt back to orig
        kpt(idim) = kpt(idim) - gulp.dk;

    elseif kpt(idim)==0.0
        freq = gulp_lj_freq(kpt,NUM_ATOMS_UCELL,MASS,...
            str_main,str_matlab,name);
        kpt(idim) = kpt(idim) + gulp.dk;
        freq_pdk = gulp_lj_freq(kpt,NUM_ATOMS_UCELL,MASS,...
            str_main,str_matlab,name);
        vel(:,idim) = (( freq_pdk - freq )/ gulp.dk / 1 )*constant.ang2m;
    %Put kpt back to orig
        kpt(idim) = kpt(idim) - gulp.dk;

    else
        freq = gulp_lj_freq(kpt,NUM_ATOMS_UCELL,MASS,...
            str_main,str_matlab,name);
        kpt(idim) = kpt(idim) + gulp.dk;
        freq_pdk = gulp_lj_freq(kpt,NUM_ATOMS_UCELL,MASS,...
            str_main,str_matlab,name);
        kpt(idim) = kpt(idim) - 2*gulp.dk;
        freq_mdk = gulp_lj_freq(kpt,NUM_ATOMS_UCELL,MASS,...
            str_main,str_matlab,name);
        vel(:,idim) = (( freq_pdk - freq_mdk )/ gulp.dk / 2 )*constant.ang2m;
    %Put kpt back to orig
        kpt(idim) = kpt(idim) + gulp.dk;
    end
end



end

