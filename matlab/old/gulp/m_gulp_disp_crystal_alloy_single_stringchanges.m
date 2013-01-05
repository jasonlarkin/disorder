function gulp = m_gulp_disp_crystal_alloy_single_stringchanges( gulp, x0 )

gulp.filename = [gulp.matlab.lib '/gin.gulp_lj_disp_conv.gin'];

gulp.orig(1).str = 'EPSILON';
gulp.change(1).str = num2str(x0.LJ.eps);
gulp.orig(2).str = 'SIGMA';
gulp.change(2).str = num2str(x0.LJ.sigma);
gulp.orig(3).str = 'ATOM_MASS_1';
gulp.change(3).str = num2str(x0.alloy.vm);
gulp.orig(4).str = 'ALATX';
gulp.change(4).str = num2str(x0.alat(1));
gulp.orig(5).str = 'ALATY';
gulp.change(5).str = num2str(x0.alat(2));
gulp.orig(6).str = 'ALATZ';
gulp.change(6).str = num2str(x0.alat(3));
gulp.orig(7).str = 'KPT';
gulp.change(7).str = num2str(gulp.kpt);

end
