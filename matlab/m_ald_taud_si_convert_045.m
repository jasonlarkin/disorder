clear

str_old = '/home/jason/disorder2/si/alloy/0.5/6x/';
alloy = load([str_old 'ALLOY_0.45.mat']);
alloy.life = alloy.life*(1/0.45);
save(strcat(str_old,'ALLOY.mat'), '-struct', 'alloy');