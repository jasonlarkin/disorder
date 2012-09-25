function vel = gulp_lj_vel(kpt,NUM_ATOMS_UCELL)
%--------------------------------------------------------------------------
%freq = gulp_lj_vel(kpt,NUM_ATOMS_UCELL)
%--------------------------------------------------------------------------

%--------------------------------------------------------------------------
%This is required to get the kpt + dk to properly input to GULP
    format long
%--------------------------------------------------------------------------

constant = m_constant;
lj = m_lj;


gulp.dk = 10E-5;

%2) Input dk kpts to measure group velocities
	vel = zeros(3*NUM_ATOMS_UCELL,3);		

for idim = 1:3
    if kpt(idim)==0.5
        freq = gulp_lj_freq(kpt,NUM_ATOMS_UCELL);
        kpt(idim) = kpt(idim) - gulp.dk;
        freq_mdk = gulp_lj_freq(kpt,NUM_ATOMS_UCELL);
        vel(:,idim) = (( freq - freq_mdk )/ gulp.dk / 4 );
    %Put kpt back to orig
        kpt(idim) = kpt(idim) + gulp.dk;

    elseif kpt(idim)==-0.5
        freq = gulp_lj_freq(kpt,NUM_ATOMS_UCELL);
        kpt(idim) = kpt(idim) + gulp.dk;
        freq_pdk = gulp_lj_freq(kpt,NUM_ATOMS_UCELL);
        vel(:,idim) = (( freq_pdk - freq )/ gulp.dk / 4 );
    %Put kpt back to orig
        kpt(idim) = kpt(idim) - gulp.dk;

    elseif kpt(idim)==0.0
        freq = gulp_lj_freq(kpt,NUM_ATOMS_UCELL);
        kpt(idim) = kpt(idim) + gulp.dk;
        freq_pdk = gulp_lj_freq(kpt,NUM_ATOMS_UCELL);
        vel(:,idim) = (( freq_pdk - freq )/ gulp.dk / 4 );
    %Put kpt back to orig
        kpt(idim) = kpt(idim) - gulp.dk;

    else
        freq = gulp_lj_freq(kpt,NUM_ATOMS_UCELL);
        kpt(idim) = kpt(idim) + gulp.dk;
        freq_pdk = gulp_lj_freq(kpt,NUM_ATOMS_UCELL);
        kpt(idim) = kpt(idim) - 2*gulp.dk;
        freq_mdk = gulp_lj_freq(kpt,NUM_ATOMS_UCELL);
        vel(:,idim) = (( freq_pdk - freq_mdk )/ gulp.dk / 8 );
    %Put kpt back to orig
        kpt(idim) = kpt(idim) + gulp.dk;
    end
end

end

