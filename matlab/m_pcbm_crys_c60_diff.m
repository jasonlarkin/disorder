
con = m_constant;

D(1).D(1).D = load('/home/jason/Dropbox/pcbm_pdb/crys_c60/aligned/Di_b0.01.dat');
D(1).D(2).D = load('/home/jason/Dropbox/pcbm_pdb/crys_c60/aligned/Di_b0.05.dat');
D(1).D(3).D = load('/home/jason/Dropbox/pcbm_pdb/crys_c60/aligned/Di_b0.15.dat');

D(1).D(4).D = load('/home/jason/Dropbox/pcbm_pdb/crys_c60/aligned/2x/Di_b0.05.dat');


D(2).D(1).D = load('/home/jason/Dropbox/pcbm_pdb/crys_c60/unaligned/Di_b0.01.dat');

D(2).D(2).D = load('/home/jason/Dropbox/pcbm_pdb/crys_c60/unaligned/2x/Di_b0.05.dat');





pcbm.Di(1,2).Di =...
    m_gulp_af_si_readDi('/home/jason/disorder/pcbm/pcbm_pdb/p1d1_1a1b1c/highT50000/','Di_b0.01.gout');



loglog(...
    pcbm.Di(1,2).Di(:,2)/(con.c*2*pi),pcbm.Di(1,2).Di(:,3),'.',...
    D(1).D(1).D(:,2),D(1).D(1).D(:,3),'.',...
    D(1).D(4).D(:,2),D(1).D(4).D(:,3),'.',...
    D(1).D(3).D(:,2),D(1).D(3).D(:,3),'.',...
    D(2).D(2).D(:,2),D(2).D(2).D(:,3),'.'...
    )

