function freq = gulp_grep_vel( gulp , x0)

gulp.dk = 10E-5;

%2) Input dk kpts to measure group velocities
	vel = zeros(3*size(x0.ucell.cart,1),3);		

for idim = 1:3
    if gulp.kpt(idim)==0.5
        freq = gulp_grep_freq( gulp , x0 );
        gulp.kpt(idim) = gulp.kpt(idim) - gulp.dk;
        freq_mdk = gulp_grep_freq( gulp , x0 );
        vel(:,idim) = (( freq - freq_mdk )/ gulp.dk / 4 );
    %Put kpt back to orig
        gulp.kpt(idim) = gulp.kpt(idim) + gulp.dk;

    elseif gulp.kpt(idim)==-0.5
        freq = gulp_grep_freq( gulp , x0 );
        gulp.kpt(idim) = gulp.kpt(idim) + gulp.dk;
        freq_pdk = gulp_grep_freq( gulp , x0 );
        vel(:,idim) = (( freq_pdk - freq )/ gulp.dk / 4 );
    %Put kpt back to orig
        gulp.kpt(idim) = gulp.kpt(idim) - gulp.dk;

    elseif gulp.kpt(idim)==0.0
        freq = gulp_grep_freq( gulp , x0 );
        gulp.kpt(idim) = gulp.kpt(idim) + gulp.dk;
        freq_pdk = gulp_grep_freq( gulp , x0 );
        vel(:,idim) = (( freq_pdk - freq )/ gulp.dk / 4 );
    %Put kpt back to orig
        gulp.kpt(idim) = gulp.kpt(idim) - gulp.dk;

    else
        freq = gulp_grep_freq( gulp , x0 );
        gulp.kpt(idim) = gulp.kpt(idim) + gulp.dk;
        freq_pdk = gulp_grep_freq( gulp , x0 );
        gulp.kpt(idim) = gulp.kpt(idim) - 2*gulp.dk;
        freq_mdk = gulp_grep_freq( gulp , x0 );
        vel(:,idim) = (( freq_pdk - freq_mdk )/ gulp.dk / 8 );
    %Put kpt back to orig
        gulp.kpt(idim) = gulp.kpt(idim) + gulp.dk;
    end
end

end

