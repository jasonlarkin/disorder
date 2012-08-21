

str.NMD = '/home/jason/lammps/LJ/alloy/10K/0.0/12x/NMD/1';

NMD=load(strcat(str.NMD,'/NMDdata.mat'));
SEDper=load(strcat(str.NMD,'/SEDdata.mat'));

str.NMD = '/home/jason/lammps/LJ/alloy/10K/0.5/12x/NMD/1';

ALLOY=load(strcat(str.NMD,'/ALLOY.mat'));



[I,J] = sort(ALLOY.freq);

loglog(ALLOY.freq(J),ALLOY.life(J)*NMD.LJ.tau,...
    ALLOY.freq(J), (1E-10)*ALLOY.freq(J).^(-4),...
ALLOY.freq,ALLOY.life_pp,'.',...
ALLOY.freq,ALLOY.life_d,'.')

%re-sort eigvec and transpose freq
% for ikpt = 1:NMD.NUM_KPTS
%     
%         ALLOY.life(...
%             (ikpt-1)*NMD.NUM_MODES+1:(ikpt)*NMD.NUM_MODES,1)...
%             =...
%         ALLOY
%     
%         ALLOY.freq(...
%             (ikpt-1)*NMD.NUM_MODES+1:(ikpt)*NMD.NUM_MODES,1)...
%             =...
%             NMD.freq(ikpt,:)';
%         ALLOY.life(
%             
% %         NMD.eigvec( (NMD.NUM_ATOMS_UCELL*3)*(ikpt-1)+1 ...
% %             :...
% %             ((NMD.NUM_ATOMS_UCELL*3)*ikpt),   1:NMD.NUM_MODES      )'
% end


