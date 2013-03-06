clear

lj = m_lj; constant = m_constant;

str.main =...
    '/home/jason/disorder2/si/amor/prepare/6x/annealDonadio/';

x0(1).x0 =...
    m_x0_read([str.main 'x0.data']);
% x0(1).freq =...
%     load([str.main 'AF_freq.dat']);
% x0(1).eigvec =...
%     load([str.main 'AF_eigvec.dat']);

icnt = 1;
data(icnt).data =...
    m_lmp_readdump_all([str.main 'lmp.x.nve.dump.1']);

log(icnt).log =...
    m_lmp_readlog([str.main 'log_main_1.lammps']);

array(1).array = str2num(log(icnt).log.data{1});
array(2).array = str2num(log(icnt).log.data{2});
array(3).array = str2num(log(icnt).log.data{3});
array(4).array = str2num(log(icnt).log.data{4});
array(5).array = str2num(log(icnt).log.data{5});
array(6).array = str2num(log(icnt).log.data{6});
array(7).array = str2num(log(icnt).log.data{7});
array(8).array = str2num(log(icnt).log.data{8});

for idump = 2:size(data(1).data.atom_data,3)
    idump
% plot3(...
%     data(1).data.atom_data(:,1,idump),...
%     data(1).data.atom_data(:,2,idump),...
%     data(1).data.atom_data(:,3,idump),'.'...
%     )
plot(...
    data(1).data.atom_data(:,1,idump),...
    data(1).data.atom_data(:,3,idump),'.'...
    )
axis([0 x0(1).x0.Lx 0 x0(1).x0.Lx])
%--------------------------------------------------------------------------
pause
%--------------------------------------------------------------------------
% plot3(...
%     data(1).data.atom_data(:,1,idump-1) - data(1).data.atom_data(:,1,idump),...
%     data(1).data.atom_data(:,2,idump-1) - data(1).data.atom_data(:,2,idump),...
%     data(1).data.atom_data(:,3,idump-1) - data(1).data.atom_data(:,3,idump),'.'...
%     )
%--------------------------------------------------------------------------
%pause
%--------------------------------------------------------------------------
end

subplot(5,1,1),...
plot(...
    array(1).array(:,1),array(1).array(:,8),'.'...
    )
subplot(5,1,2),...
plot(...
    array(3).array(:,1),array(3).array(:,8),'.'...
    )
subplot(5,1,3),...
plot(...
    array(5).array(:,1),array(5).array(:,8),'.'...
    )
subplot(5,1,4),...
plot(...
    array(7).array(:,1),array(7).array(:,8),'.'...
    )
subplot(5,1,5),...
plot(...
    array(8).array(:,1),array(8).array(:,8),'.'...
    )

% T = 0.1;
% urms(1).urms = m_lj_rms(T , x0(1).x0.m , x0(1).freq , x0(1).eigvec , 1 , size(x0(1).x0.x,1) );
% urms(1).urms_md = mean(sqrt(array(1).array(:,8)));

