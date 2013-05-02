

str = '/home/jason/disorder2/si/amor/normand/perf4096/anneal_1100K/emin/dsf/nmd/';
sed(1).sed = load([str 'SED_1.txt']);
sed(2).sed = load([str 'SED_2.txt']);
sed(3).sed = load([str 'SED_3.txt']);
sed(4).sed = load([str 'SED_4.txt']);
sed(5).sed = load([str 'SED_5.txt']);
sed(6).sed = load([str 'SED_6.txt']);
sed(7).sed = load([str 'SED_7.txt']);
sed(8).sed = load([str 'SED_8.txt']);


semilogy(...
sed(1).sed(:,1),smooth(sed(1).sed(:,2),7),'.',...
sed(2).sed(:,1),smooth(sed(2).sed(:,2),7),'.',...
sed(3).sed(:,1),smooth(sed(3).sed(:,2),7),'.',...
sed(4).sed(:,1),smooth(sed(4).sed(:,2),7),'.',...
sed(5).sed(:,1),smooth(sed(5).sed(:,2),7),'.',...
sed(6).sed(:,1),smooth(sed(6).sed(:,2),7),'.',...
sed(7).sed(:,1),smooth(sed(7).sed(:,2),7),'.',...
sed(8).sed(:,1),smooth(sed(8).sed(:,2),7),'.'...
)

DSF.SL(:,1) = sed(1).sed(:,2);
DSF.SL(:,2) = sed(2).sed(:,2);
DSF.SL(:,3) = sed(3).sed(:,2);
DSF.SL(:,4) = sed(4).sed(:,2);
DSF.SL(:,5) = sed(5).sed(:,2);
DSF.SL(:,6) = sed(6).sed(:,2);
DSF.SL(:,7) = sed(7).sed(:,2);
DSF.SL(:,8) = sed(8).sed(:,2);
DSF.freq_range = sed(1).sed(:,1);
DSF.kpt(:,1) = (1:8)'; DSF.kpt(:,2) = 0; DSF.kpt(:,3) = 0; 
save(strcat('/home/jason/disorder2/si/amor/normand/perf4096/anneal_1100K/emin/dsf/nmd/','DSF.mat'), '-struct', 'DSF');


