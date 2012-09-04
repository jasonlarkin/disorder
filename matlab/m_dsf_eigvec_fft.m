
function AF = m_dsf_eigvec_fft(AF, NMD, SED, imode)

KX = AF.kpt(:,1)./abs(AF.kpt(:,1));
Inan = find(isnan(KX)==1); KX(Inan)=0;
KY = AF.kpt(:,2)./abs(AF.kpt(:,2));
Inan = find(isnan(KY)==1); KY(Inan)=0;
KZ = AF.kpt(:,3)./abs(AF.kpt(:,3));
Inan = find(isnan(KZ)==1); KZ(Inan)=0;


spatialx =...
    bsxfun( @times , AF.x0(:,3) , ...
    AF.kpt(:,1)' / (NMD.alat*NMD.Nx) );
spatialy =...
    bsxfun( @times , AF.x0(:,4) , ...
    AF.kpt(:,2)' / (NMD.alat*NMD.Ny) );
spatialz =...
    bsxfun( @times , AF.x0(:,5) , ...
    AF.kpt(:,3)' / (NMD.alat*NMD.Nz) );

AF.eig_fftx(imode,1:size(AF.kpt,1)) =...
    sum(...
    bsxfun(...
    @times ,...
    bsxfun(@times,exp( 2*pi*1i*(spatialx + spatialy +spatialz)),KX') ,...
    AF.eigvec(1:3:size(AF.eigvec,1),imode) )...
    , 1);

AF.eig_ffty(imode,1:size(AF.kpt,1)) =...
    sum(...
    bsxfun(...
    @times ,...
    bsxfun(@times,exp( 2*pi*1i*(spatialx + spatialy +spatialz)),KY') ,...
    AF.eigvec(2:3:size(AF.eigvec,1),imode) )...
    , 1);

AF.eig_fftz(imode,1:size(AF.kpt,1)) =...
    sum(...
    bsxfun(...
    @times ,...
    bsxfun(@times,exp( 2*pi*1i*(spatialx + spatialy +spatialz)),KZ') ,...
    AF.eigvec(3:3:size(AF.eigvec,1),imode) )...
    , 1);

end


