

C=1;

aligned = load('/home/jason/Dropbox/pcbm_pdb/crys_c60/aligned/c60_aligned.matlab');

unaligned = load('/home/jason/Dropbox/pcbm_pdb/crys_c60/unaligned/C60.matlab');

alat = 14.0;

plot3(...
    aligned(:,1),aligned(:,2),aligned(:,3),'.',...
    unaligned(:,1),unaligned(:,2),unaligned(:,3),'.'...
    )

shifted(:,1) = unaligned(:,1) - mean(unaligned(:,1));
shifted(:,2) = unaligned(:,2) - mean(unaligned(:,2));
shifted(:,3) = unaligned(:,3) - mean(unaligned(:,3));

plot3(...
    aligned(:,1),aligned(:,2),aligned(:,3),'.',...
    shifted(:,1),shifted(:,2),shifted(:,3),'.'...
    )



str_write='/home/jason/Dropbox/pcbm_pdb/crys_c60/aligned/2x/anneal/';

%1   1    1   3.0 1.0 0.0 38049  4.3484   -.57058 .72751

str(1).line =...
    [3.0 1.0 0.0 38049  4.3484   -.57058 .72751];

str(2).line =...
    [0.00000015724 2.2119  346.7   1.95   0.15   3.4879  1393.6];

cnt=0;
for i1=1:32
for i2=1:32
for i3=1:32
line = [i1 i2 i3 str(1).line];
dlmwrite(...
    [str_write 'C.temp.tersoff'],...
    line ,'-append','delimiter',' ');
dlmwrite(...
    [str_write 'C.temp.tersoff'],...
    str(2).line ,'-append','delimiter',' ');
cnt=cnt+1
end
end
end


crys(1).x(1:60) = aligned(:,1) + 0.0;
crys(1).y(1:60) = aligned(:,2) + 0.0;
crys(1).z(1:60) = aligned(:,3) + 0.0;

crys(1).x(61:120) = aligned(:,1) + (1/2)*alat;
crys(1).y(61:120) = aligned(:,2) + (1/2)*alat;
crys(1).z(61:120) = aligned(:,3) + 0.0;

crys(1).x(121:180) = aligned(:,1) + (1/2)*alat;
crys(1).y(121:180) = aligned(:,2) + 0.0;
crys(1).z(121:180) = aligned(:,3) + (1/2)*alat;

crys(1).x(181:240) = aligned(:,1) + 0.0;
crys(1).y(181:240) = aligned(:,2) + (1/2)*alat;
crys(1).z(181:240) = aligned(:,3) + (1/2)*alat;

crys(1).type(1:240)= 1;

% %output full
% output = [length(crys(1).type) length(crys(1).type) alat alat alat];
% crys(1).id = 1:length(crys(1).type);
% x =...
%     [ crys(1).id' crys(1).type' crys(1).x' crys(1).y' crys(1).z' ];
% dlmwrite(...
%     '/home/jason/Dropbox/pcbm_pdb/crys_c60/aligned/c60_aligned.data',...
%     output ,'-append','delimiter',' ');
% dlmwrite(...
%     '/home/jason/Dropbox/pcbm_pdb/crys_c60/aligned/c60_aligned.data',...
%     x ,'-append','delimiter',' ');

plot3(crys(1).x,crys(1).y,crys(1).z,'.')

pause



crys(2).x(1:60) = unaligned(:,1) + 0.0;
crys(2).y(1:60) = unaligned(:,2) + 0.0;
crys(2).z(1:60) = unaligned(:,3) + 0.0;

crys(2).x(61:120) = unaligned(:,1) + (1/2)*alat;
crys(2).y(61:120) = unaligned(:,2) + (1/2)*alat;
crys(2).z(61:120) = unaligned(:,3) + 0.0;

crys(2).x(121:180) = unaligned(:,1) + (1/2)*alat;
crys(2).y(121:180) = unaligned(:,2) + 0.0;
crys(2).z(121:180) = unaligned(:,3) + (1/2)*alat;

crys(2).x(181:240) = unaligned(:,1) + 0.0;
crys(2).y(181:240) = unaligned(:,2) + (1/2)*alat;
crys(2).z(181:240) = unaligned(:,3) + (1/2)*alat;

crys(2).type(1:240)= 1;

%output full
% output = [length(crys(2).type) length(crys(2).type) alat alat alat];
% crys(2).id = 1:length(crys(2).type);
% x =...
%     [ crys(2).id' crys(2).type' crys(2).x' crys(2).y' crys(2).z' ];
% dlmwrite(...
%     '/home/jason/Dropbox/pcbm_pdb/crys_c60/unaligned/c60_unaligned.data',...
%     output ,'-append','delimiter',' ');
% dlmwrite(...
%     '/home/jason/Dropbox/pcbm_pdb/crys_c60/unaligned/c60_unaligned.data',...
%     x ,'-append','delimiter',' ');

plot3(crys(2).x,crys(2).y,crys(2).z,'.')




























crys(3).x(1:240) = crys(1).x(1:240) + 0.0;
crys(3).y(1:240) = crys(1).y(1:240) + 0.0;
crys(3).z(1:240) = crys(1).z(1:240) + 0.0;

crys(3).x(241:480) = crys(1).x(1:240) + alat;
crys(3).y(241:480) = crys(1).y(1:240) + 0.0;
crys(3).z(241:480) = crys(1).z(1:240) + 0.0;

crys(3).x(481:720) = crys(1).x(1:240) + 0.0;
crys(3).y(481:720) = crys(1).y(1:240) + alat;
crys(3).z(481:720) = crys(1).z(1:240) + 0.0;

crys(3).x(721:960) = crys(1).x(1:240) + 0.0;
crys(3).y(721:960) = crys(1).y(1:240) + 0.0;
crys(3).z(721:960) = crys(1).z(1:240) + alat;

crys(3).x(961:1200) = crys(1).x(1:240) + alat;
crys(3).y(961:1200) = crys(1).y(1:240) + alat;
crys(3).z(961:1200) = crys(1).z(1:240) + 0.0;

crys(3).x(1201:1440) = crys(1).x(1:240) + 0.0;
crys(3).y(1201:1440) = crys(1).y(1:240) + alat;
crys(3).z(1201:1440) = crys(1).z(1:240) + alat;

crys(3).x(1441:1680) = crys(1).x(1:240) + alat;
crys(3).y(1441:1680) = crys(1).y(1:240) + 0.0;
crys(3).z(1441:1680) = crys(1).z(1:240) + alat;

crys(3).x(1681:1920) = crys(1).x(1:240) + alat;
crys(3).y(1681:1920) = crys(1).y(1:240) + alat;
crys(3).z(1681:1920) = crys(1).z(1:240) + alat;

crys(3).type(1:1920)= 1;

%output full
output = [length(crys(3).type) length(crys(3).type) 2*alat 2*alat 2*alat];
crys(3).id = 1:length(crys(3).type);
x =...
    [ crys(3).id' crys(3).type' crys(3).x' crys(3).y' crys(3).z' ];
dlmwrite(...
    '/home/jason/Dropbox/pcbm_pdb/crys_c60/aligned/2x/c60_aligned.data',...
    output ,'-append','delimiter',' ');
dlmwrite(...
    '/home/jason/Dropbox/pcbm_pdb/crys_c60/aligned/2x/c60_aligned.data',...
    x ,'-append','delimiter',' ');

plot3(crys(3).x,crys(3).y,crys(3).z,'.')

pause


















crys(4).x(1:240) = crys(2).x(1:240) + 0.0;
crys(4).y(1:240) = crys(2).y(1:240) + 0.0;
crys(4).z(1:240) = crys(2).z(1:240) + 0.0;

crys(4).x(241:480) = crys(2).x(1:240) + alat;
crys(4).y(241:480) = crys(2).y(1:240) + 0.0;
crys(4).z(241:480) = crys(2).z(1:240) + 0.0;

crys(4).x(481:720) = crys(2).x(1:240) + 0.0;
crys(4).y(481:720) = crys(2).y(1:240) + alat;
crys(4).z(481:720) = crys(2).z(1:240) + 0.0;

crys(4).x(721:960) = crys(2).x(1:240) + 0.0;
crys(4).y(721:960) = crys(2).y(1:240) + 0.0;
crys(4).z(721:960) = crys(2).z(1:240) + alat;

crys(4).x(961:1200) = crys(2).x(1:240) + alat;
crys(4).y(961:1200) = crys(2).y(1:240) + alat;
crys(4).z(961:1200) = crys(2).z(1:240) + 0.0;

crys(4).x(1201:1440) = crys(2).x(1:240) + 0.0;
crys(4).y(1201:1440) = crys(2).y(1:240) + alat;
crys(4).z(1201:1440) = crys(2).z(1:240) + alat;

crys(4).x(1441:1680) = crys(2).x(1:240) + alat;
crys(4).y(1441:1680) = crys(2).y(1:240) + 0.0;
crys(4).z(1441:1680) = crys(2).z(1:240) + alat;

crys(4).x(1681:1920) = crys(2).x(1:240) + alat;
crys(4).y(1681:1920) = crys(2).y(1:240) + alat;
crys(4).z(1681:1920) = crys(2).z(1:240) + alat;

crys(4).type(1:1920)= 1;

%output full
output = [length(crys(4).type) length(crys(4).type) 2*alat 2*alat 2*alat];
crys(4).id = 1:length(crys(4).type);
x =...
    [ crys(4).id' crys(4).type' crys(4).x' crys(4).y' crys(4).z' ];
dlmwrite(...
    '/home/jason/Dropbox/pcbm_pdb/crys_c60/unaligned/2x/c60_unaligned.data',...
    output ,'-append','delimiter',' ');
dlmwrite(...
    '/home/jason/Dropbox/pcbm_pdb/crys_c60/unaligned/2x/c60_unaligned.data',...
    x ,'-append','delimiter',' ');

plot3(crys(4).x,crys(4).y,crys(4).z,'.')



