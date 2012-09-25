function m_nmd_cond(str,name_nmd,name_sed)
%--------------------------------------------------------------------------
%m_nmd_cond(str,name_nmd,name_sed)
%assumes quantities in mks units, not lj
%str.NMD = '/home/jason/lammps/LJ/alloy/10K/0.05/10x/NMD/1';
%--------------------------------------------------------------------------

str.NMD = str;

NMD=load(strcat(str.NMD,name_nmd))

SED=load(strcat(str.NMD,name_sed))

%CONVERT TO W/m-K

SED.irrkpt.HLDfreq = SED.irrkpt.HLDfreq;
SED.irrkpt.HLDvel = SED.irrkpt.HLDvel;
SED.irrkpt.life = SED.irrkpt.life;
SED.redkpt.freq = SED.redkpt.freq;
SED.redkpt.groupvel = SED.redkpt.groupvel;

NMD.VOLUME =...
    NMD.Nx*NMD.alat*NMD.Ny*NMD.alat*NMD.Nz*NMD.alat*NMD.LJ.sigma^3;

for ikpt1=1:size(SED.redkpt.kpt,1)
    for ikpt2=1:size(SED.irrkpt.kpt,1)
            if issym(SED.irrkpt.kpt(ikpt2,1:3),SED.redkpt.kpt(ikpt1,1:3)) == 1.0;
            
            SED.redkpt.sedfreq(:,ikpt1) = SED.irrkpt.sedfreq(:,ikpt2);
                
            SED.redkpt.life(:,ikpt1) =...
                SED.irrkpt.life(:,ikpt2);
                
            SED.redkpt.diffx(:,ikpt1) =...
                SED.irrkpt.life(:,ikpt2).*SED.redkpt.groupvel(:,1,ikpt1).^2;
            SED.redkpt.diffy(:,ikpt1) =...
                SED.irrkpt.life(:,ikpt2).*SED.redkpt.groupvel(:,2,ikpt1).^2;
            SED.redkpt.diffz(:,ikpt1) =...
                SED.irrkpt.life(:,ikpt2).*SED.redkpt.groupvel(:,3,ikpt1).^2;
            
            SED.redkpt.condx(ikpt1) = (NMD.constant.kb/NMD.VOLUME)*...
                sum(SED.redkpt.diffx(:,ikpt1),1);
            SED.redkpt.condy(ikpt1) = (NMD.constant.kb/NMD.VOLUME)*...
                sum(SED.redkpt.diffy(:,ikpt1),1);
            SED.redkpt.condz(ikpt1) = (NMD.constant.kb/NMD.VOLUME)*...
                sum(SED.redkpt.diffz(:,ikpt1),1);
    
            SED.irrkpt.numdegen(ikpt2);
            end
    end
end


max(max(max(SED.irrkpt.HLDfreq)))
max(max(max(SED.irrkpt.HLDvel)))
max(max(SED.irrkpt.life))

max(max(max(SED.redkpt.freq)))
max(max(max(SED.redkpt.groupvel)))
max(max(SED.redkpt.life))

%CALC CONDUCTIVITY
SED.conductivityx = sum( SED.redkpt.condx ); SED.conductivityx
SED.conductivityy = sum( SED.redkpt.condy ); SED.conductivityy
SED.conductivityz = sum( SED.redkpt.condz ); SED.conductivityz

loglog( SED.irrkpt.sedfreq,SED.irrkpt.life/NMD.LJ.tau,'.' )

% save(strcat(str.NMD,'NMDdata.mat'), '-struct', 'NMD');
% save(strcat(str.NMD,'SEDdata.mat'), '-struct', 'SED');

end



