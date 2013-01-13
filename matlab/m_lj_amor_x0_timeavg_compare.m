clear

lj = m_lj; constant = m_constant;

x0(1).x0 =...
    m_x0_read(...
    '/home/jason/disorder2/lj/amor/4x/XCORR_AF/5K/2^19_2^16/x0.data');
x0(1).freq =...
    load(...
    '/home/jason/disorder2/lj/amor/4x/XCORR_AF/5K/2^19_2^16/AF_freq.dat');
x0(1).eigvec =...
    load(...
    '/home/jason/disorder2/lj/amor/4x/XCORR_AF/5K/2^19_2^16/AF_eigvec.dat');

icnt = 1;
str.main = '/home/jason/disorder2/lj/amor/4x/XCORR_AF/0.1K/';
data(icnt).data =...
    m_lmp_readdump_all([str.main 'lmp.x.dump']);
log(icnt).log =...
    m_lmp_readlog([str.main 'log.lammps']);
array(icnt).array = str2num(log(icnt).log.data{3});

icnt = 2;
str.main = '/home/jason/disorder2/lj/amor/4x/XCORR_AF/1K/';
data(icnt).data =...
    m_lmp_readdump_all([str.main 'lmp.x.dump']);
log(icnt).log =...
    m_lmp_readlog([str.main 'log.lammps']);
array(icnt).array = str2num(log(icnt).log.data{3});

icnt = 3;
str.main = '/home/jason/disorder2/lj/amor/4x/XCORR_AF/5K/';
data(icnt).data =...
    m_lmp_readdump_all([str.main 'lmp.x.dump']);
log(icnt).log =...
    m_lmp_readlog([str.main 'log.lammps']);
array(icnt).array = str2num(log(icnt).log.data{3});

icnt = 4;
str.main = '/home/jason/disorder2/lj/amor/4x/XCORR_AF/10K/';
data(icnt).data =...
    m_lmp_readdump_all([str.main 'lmp.x.dump']);
log(icnt).log =...
    m_lmp_readlog([str.main 'log.lammps']);
array(icnt).array = str2num(log(icnt).log.data{3});

%--------------------------------------------------------------------------
%perfect
%--------------------------------------------------------------------------

x0(2).x0 =...
    m_x0_read(...
    '/home/jason/disorder2/lj/amor/4x/XCORR_AF/10K_alat/4x_2^19_2^15/x0.data');
x0(2).freq =...
    load(...
    '/home/jason/disorder2/lj/amor/4x/XCORR_AF/10K_alat/4x_2^19_2^15/AF_freq.dat');
x0(2).eigvec =...
    load(...
    '/home/jason/disorder2/lj/amor/4x/XCORR_AF/10K_alat/4x_2^19_2^15/AF_eigvec.dat');

icnt = 5;
str.main = '/home/jason/disorder2/lj/amor/4x/XCORR_AF/perfect_1K/';
data(icnt).data =...
    m_lmp_readdump_all([str.main 'lmp.x.dump']);
log(icnt).log =...
    m_lmp_readlog([str.main 'log.lammps']);
array(icnt).array = str2num(log(icnt).log.data{3});

icnt = 6;
str.main = '/home/jason/disorder2/lj/amor/4x/XCORR_AF/perfect_2.5K/';
data(icnt).data =...
    m_lmp_readdump_all([str.main 'lmp.x.dump']);
log(icnt).log =...
    m_lmp_readlog([str.main 'log.lammps']);
array(icnt).array = str2num(log(icnt).log.data{3});

icnt = 7;
str.main = '/home/jason/disorder2/lj/amor/4x/XCORR_AF/perfect_10K/';
data(icnt).data =...
    m_lmp_readdump_all([str.main 'lmp.x.dump']);
log(icnt).log =...
    m_lmp_readlog([str.main 'log.lammps']);
array(icnt).array = str2num(log(icnt).log.data{3});


icnt=6;
for idump = 2:size(data(icnt).data.atom_data,3)
    idump
plot3(...
    data(icnt).data.atom_data(:,1,idump),...
    data(icnt).data.atom_data(:,2,idump),...
    data(icnt).data.atom_data(:,3,idump),'.'...
    )
plot(...
    data(icnt).data.atom_data(:,1,idump),...
    data(icnt).data.atom_data(:,3,idump),'.'...
    )
axis([0 7 0 7])
% print('-dpng','-r300',['frame' int2str(idump)]);
%--------------------------------------------------------------------------
pause
%--------------------------------------------------------------------------
end

T = 0.1;
urms(1).urms = m_lj_rms(T , x0(1).x0.m , x0(1).freq , x0(1).eigvec , 1 , size(x0(1).x0.x,1) );
urms(1).urms_md = mean(sqrt(array(1).array(:,8)));
T = 1;
urms(2).urms = m_lj_rms(T , x0(1).x0.m , x0(1).freq , x0(1).eigvec , 1 , size(x0(1).x0.x,1) );
urms(2).urms_md = mean(sqrt(array(2).array(:,8)));
T = 5;
urms(3).urms = m_lj_rms(T , x0(1).x0.m , x0(1).freq , x0(1).eigvec , 1 , size(x0(1).x0.x,1) );
urms(3).urms_md = mean(sqrt(array(3).array(:,8)));
T = 10;
urms(4).urms = m_lj_rms(T , x0(1).x0.m , x0(1).freq , x0(1).eigvec , 1 , size(x0(1).x0.x,1) );
urms(4).urms_md = mean(sqrt(array(4).array(1:1000,8)));

T = 1;
urms(5).urms = m_lj_rms(T , x0(2).x0.m , x0(2).freq , x0(2).eigvec , 1 , size(x0(2).x0.x,1) );
urms(5).urms_md = mean(sqrt(array(5).array(:,8)));
T = 2.5;
urms(6).urms = m_lj_rms(T , x0(2).x0.m , x0(2).freq , x0(2).eigvec , 1 , size(x0(2).x0.x,1) );
urms(6).urms_md = mean(sqrt(array(6).array(:,8)));
T = 10;
urms(7).urms = m_lj_rms(T , x0(2).x0.m , x0(2).freq , x0(2).eigvec , 1 , size(x0(2).x0.x,1) );
urms(7).urms_md = mean(sqrt(array(7).array(:,8)));

urms(7).urms_alan = 0.117/3.4;

semilogy(...
    array(1).array(:,1),sqrt(array(1).array(:,8)),'.',...
    array(1).array(:,1),ones(length(array(1).array(:,1)),1)*urms(1).urms/lj.sigma,...
    array(2).array(:,1),sqrt(array(2).array(:,8)),'.',...
    array(2).array(:,1),ones(length(array(2).array(:,1)),1)*urms(2).urms/lj.sigma,...
    array(3).array(:,1),sqrt(array(3).array(:,8)),'.',...
    array(3).array(:,1),ones(length(array(3).array(:,1)),1)*urms(3).urms/lj.sigma,...
    array(4).array(:,1),sqrt(array(4).array(:,8)),'.',...
    array(4).array(:,1),ones(length(array(4).array(:,1)),1)*urms(4).urms/lj.sigma,...
    array(5).array(:,1),sqrt(array(5).array(:,8)),'.',...
    array(5).array(:,1),ones(length(array(5).array(:,1)),1)*urms(5).urms/lj.sigma,...
    array(6).array(:,1),sqrt(array(6).array(:,8)),'.',...
    array(6).array(:,1),ones(length(array(6).array(:,1)),1)*urms(6).urms/lj.sigma,...
    array(7).array(:,1),sqrt(array(7).array(:,8)),'.',...
    array(7).array(:,1),ones(length(array(7).array(:,1)),1)*urms(7).urms/lj.sigma...
    )

semilogy(...
    (array(2).array(:,1)-array(2).array(1,1))*0.002,...
    sqrt(array(2).array(:,8)),'.',...
    (array(2).array(:,1)-array(2).array(1,1))*0.002,...
    ones(length(array(2).array(:,1)),1)*urms(2).urms/lj.sigma,...
    (array(4).array(:,1)-array(4).array(1,1))*0.002,...
    sqrt(array(4).array(:,8)),'.',...
    (array(4).array(:,1)-array(4).array(1,1))*0.002,...
    ones(length(array(4).array(:,1)),1)*urms(4).urms/lj.sigma,...
    (array(7).array(:,1)-array(7).array(1,1))*0.002,...
    0.72*sqrt(array(7).array(:,8)),'.',...
    (array(7).array(:,1)-array(7).array(1,1))*0.002,...
    ones(length(array(7).array(:,1)),1)*urms(7).urms_alan...
    )

plot(...
    [0.1 1 5 10],...
    [...
    urms(1).urms_md/(urms(1).urms/lj.sigma) ...
    urms(2).urms_md/(urms(2).urms/lj.sigma) ...
    urms(3).urms_md/(urms(3).urms/lj.sigma) ...
    urms(4).urms_md/(urms(4).urms/lj.sigma)...
    ],...
    [1 2.5 10],...
    [...
    urms(5).urms_md/(urms(5).urms/lj.sigma) ...
    urms(6).urms_md/(urms(6).urms/lj.sigma) ...
    urms(7).urms_md/(urms(7).urms/lj.sigma)...
    ]...
    )

% semilogy(...
%     array(1).array(:,1),array(1).array(:,8),'.',...
%     array(2).array(:,1),array(2).array(:,8),'.',...
%     array(3).array(:,1),array(3).array(:,8),'.',...
%     array(4).array(:,1),array(4).array(:,8),'.',...
%     array(5).array(:,1),array(5).array(:,8),'.',...
%     array(6).array(:,1),array(6).array(:,8),'.',...
%     array(7).array(:,1),array(7).array(:,8),'.'...
%     )

%--------------------------------------------------------------------------
%msd
%--------------------------------------------------------------------------
% semilogy(...
%     array(2).array(:,1),array(2).array(:,8),'.',...
%     array(5).array(:,1),array(5).array(:,8),'.',...
%     array(6).array(:,1),array(6).array(:,8),'.',...
%     array(8).array(:,1),array(8).array(:,8),'.'...
%     )
%--------------------------------------------------------------------------
%pe
%--------------------------------------------------------------------------
% plot(...
%     array(2).array(:,1),array(2).array(:,5)-min(array(6).array(:,5)),'.',...
%     array(5).array(:,1),array(5).array(:,5)-min(array(6).array(:,5)),'.',...
%     array(6).array(:,1),array(6).array(:,5)-min(array(6).array(:,5)),'.',...
%     array(8).array(:,1),array(8).array(:,5)-min(array(6).array(:,5)),'.'...
%     )

%/home/jason/Dropbox/ntpl-paper/internal/turney_prb09_aLD.pdf
%eq. (30)



% urms = m_lj_rms(...
%     T , x0(2).x0.m , x0(2).freq , x0(2).eigvec , 1 , size(x0(2).x0.x,1) )








% x_timeavg =...
%     load(...
%     '/home/jason/disorder2/lj/amor/4x/prepare/tmp/lmp.x.nvt.dump.1.1.matlab');
% 
% x0K =...
%     m_x0_read('/home/jason/disorder2/lj/amor/4x/prepare/tmp/x0K_1.data');
% 
% x0 =...
%     m_x0_read('/home/jason/disorder2/lj/amor/4x/prepare/tmp/x0_1.data');
% 
% plot3(...
%     x_timeavg(:,1),x_timeavg(:,2),x_timeavg(:,3),'.',...
%     x0.x,x0.y,x0.z,'.',...
%     x0K.x,x0K.y,x0K.z,'.'...
%     )
% %--------------------------------------------------------------------------
% pause
% %--------------------------------------------------------------------------
% I = find(...
%     abs(x0.x - x_timeavg(:,1)) < 2.0 & ...
%     abs(x0.y - x_timeavg(:,2)) < 2.0 & ...
%     abs(x0.z - x_timeavg(:,3)) < 2.0 ...
%     );
% plot3(...
%     x0.x(I) - x_timeavg(I,1),x0.y(I) - x_timeavg(I,2),x0.z(I) - x_timeavg(I,3),'.'...
%     )
% %--------------------------------------------------------------------------
% pause
% %--------------------------------------------------------------------------
% I = find(...
%     abs(x0K.x - x_timeavg(:,1)) < 2.0 & ...
%     abs(x0K.y - x_timeavg(:,2)) < 2.0 & ...
%     abs(x0K.z - x_timeavg(:,3)) < 2.0 ...
%     );
% plot3(...
%     x0K.x(I) - x_timeavg(I,1),x0K.y(I) - x_timeavg(I,2),x0K.z(I) - x_timeavg(I,3),'.'...
%     )
% 
% %--------------------------------------------------------------------------
% clear
% %--------------------------------------------------------------------------


