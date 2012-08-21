
str.NMD = '/home/jason/lammps/LJ/alloy/10K/0.0/8x/NMD';

NMD=load(strcat(str.NMD,'/NMDfit.mat'));

for ikpt=1:NMD.NUM_KPTS
freq((ikpt-1)*NMD.NUM_MODES+1:(ikpt)*NMD.NUM_MODES ) =...
    NMD.freq(ikpt,1:NMD.NUM_MODES);
end
DOS.NUM_FFTS = 1; 

DOS.NUM_ATOMS = 2048;

DOS.NUM_SEEDS=1; iseed = 1;

%TIMES---------------------------------------------------------------------
    DOS.t_total = 2^19; DOS.t_fft = 2^19; DOS.t_step = 2^5; DOS.dt = 0.002;
    DOS.NUM_TSTEPS = DOS.t_fft/DOS.t_step; 
%-------------------------------------------------------------------------- 

%IFFT----------------------------------------------------------------------
    DOS.NUM_FFTS = DOS.t_total/DOS.t_fft;
%-------------------------------------------------------------------------- 

%FREQS---------------------------------------------------------------------
    DOS.w_step = 2*pi/(DOS.t_fft*DOS.dt); 
    DOS.w_max = 2*pi/(DOS.t_step*DOS.dt*2);
    DOS.NUM_OMEGAS = DOS.t_fft/(2*DOS.t_step); 
%-------------------------------------------------------------------------- 

[tmp,str.main]=system('pwd');

%---1------------------------------------------------------------------
    iseed = 1;
%--------------------------------------------------------------------------

%---1----------------------------------------------------------------
    ikslice = 1;
%--------------------------------------------------------------------------    

    
for ifft = 1:DOS.NUM_FFTS  
    
%--------------------------------------------------------------------------
tic  
%--------------------------------------------------------------------------
%VElOCITIES
    str_read=strcat( str.main ,'/dump_',int2str(iseed),'_',int2str(ifft),'.vel');
    fid=fopen(str_read);
    dummy = textscan(fid,'%f%f%f','Delimiter',' ','commentStyle', '--'); 
    fclose(fid);
%Store velocity data of all atoms: subtract off the last time step
    velx = zeros(DOS.NUM_ATOMS,DOS.NUM_TSTEPS);
    vely = zeros(DOS.NUM_ATOMS,DOS.NUM_TSTEPS);
    velz = zeros(DOS.NUM_ATOMS,DOS.NUM_TSTEPS);
for iatom = 1:DOS.NUM_ATOMS  
    velx(iatom,1:DOS.NUM_TSTEPS) =...
        dummy{1}(iatom:DOS.NUM_ATOMS:(length(dummy{1}(:))-DOS.NUM_ATOMS));
    vely(iatom,1:DOS.NUM_TSTEPS) =...
        dummy{2}(iatom:DOS.NUM_ATOMS:(length(dummy{1}(:))-DOS.NUM_ATOMS));
    velz(iatom,1:DOS.NUM_TSTEPS) =...
        dummy{3}(iatom:DOS.NUM_ATOMS:(length(dummy{1}(:))-DOS.NUM_ATOMS));
end
%--------------------------------------------------------------------------
toc
%--------------------------------------------------------------------------
%Remove dummy
    clear dummy  
%Set mass array
%    m = repmat(DOS.mass(:,1),1,DOS.NUM_TSTEPS);   
    DOS.DOS(1:DOS.NUM_TSTEPS/2) = 0.0;
%--------------------------------------------------------------------------
tic  
%--------------------------------------------------------------------------
for iatom = 1:DOS.NUM_ATOMS
    XCORR = xcorr(velx(iatom,1:DOS.NUM_TSTEPS),'coeff');
    YCORR = xcorr(vely(iatom,1:DOS.NUM_TSTEPS),'coeff');
    ZCORR = xcorr(velz(iatom,1:DOS.NUM_TSTEPS),'coeff');
    
    XFFT = fft(velx(iatom,1:DOS.NUM_TSTEPS));
    YFFT = fft(vely(iatom,1:DOS.NUM_TSTEPS));
    ZFFT = fft(velz(iatom,1:DOS.NUM_TSTEPS));
    
%     XFFT = fft(XCORR(DOS.NUM_TSTEPS:DOS.NUM_TSTEPS*2-1)); 
%     YFFT = fft(YCORR(DOS.NUM_TSTEPS:DOS.NUM_TSTEPS*2-1)); 
%     ZFFT = fft(ZCORR(DOS.NUM_TSTEPS:DOS.NUM_TSTEPS*2-1));
    
%     XPS = XFFT; YPS  = YFFT; ZPS  = ZFFT; 

% DOS.DOS = DOS.DOS +...
%     XPS(1:(DOS.NUM_TSTEPS/2)) +...
%     YPS(1:(DOS.NUM_TSTEPS/2)) +...
%     ZPS(1:(DOS.NUM_TSTEPS/2));

DOS.DOS = DOS.DOS + ...
    (abs(XFFT(1:(DOS.NUM_TSTEPS/2))).^2)...
    +...
    (abs(YFFT(1:(DOS.NUM_TSTEPS/2))).^2)...
    +...
    (abs(ZFFT(1:(DOS.NUM_TSTEPS/2))).^2) ;

end
%--------------------------------------------------------------------------
toc 
%--------------------------------------------------------------------------

end

omega = (1:DOS.NUM_OMEGAS)*(DOS.w_max/DOS.NUM_OMEGAS);


[y x ] =hist(freq,15);

plot(omega,100*DOS.DOS/sum(DOS.DOS),'.',x,y/sum(y),'.')

numBlock = 2^5;
sizeBlock = (DOS.NUM_TSTEPS/2)/numBlock;

for iblock = 1:numBlock
DOS.block(iblock) =...
    mean(DOS.DOS( (iblock-1)*sizeBlock+1:(iblock)*sizeBlock));
block_omega(iblock) = mean(omega( (iblock-1)*sizeBlock+1:(iblock)*sizeBlock));
end

plot(block_omega,DOS.block/sum(DOS.block),'.',x,y/sum(y))



%     
% %Average over FFTS
%     SED.SED = SED.SED/NMD.NUM_FFTS;
% %Define frequencies
%     omega = (1:NMD.NUM_OMEGAS)*(NMD.w_max/NMD.NUM_OMEGAS);
% %Output SED
%     for ikpt = 1:size(NMD.kptlist(:,1:3,ikslice),1)
%         str_write_single=strcat(NMD.str.main,'/',int2str(NMD.seed.alloy),'/NMD/SED_',...
%             num2str(NMD.kptlist(ikpt,1,ikslice)),...
%             num2str(NMD.kptlist(ikpt,2,ikslice)),...
%             num2str(NMD.kptlist(ikpt,3,ikslice)),...
%             '_',int2str(iseed),'.txt');
%         output(1:length(omega),1) = omega;
%         output(1:length(omega),2:(NMD.NUM_MODES+1)) = SED.SED(ikpt,:,:);
%         dlmwrite(str_write_single,output,'delimiter',' ');
%         clear output
%     end %END ikpt    
% %end %END iseed
% 
