
function DSF = m_dsf_long( str , kpt , x0 , freq, eigvec )
%M_DSF_LONG
%	m_dsf_long( str , kpt , x0 , freq, eigvec )
%	
%	kpt is expected to have form:
%
%	kpt(:,1) / (Lx)
%	x0(:,1)==atomid, x0(:,2)==typeid, x0(:,3:5)=xyz
%	freq(1:NUM_MODES)
%	eigvec(3*NUM_ATOMS,NUM_MODES)

%--------------------------------------------------------------------------
tic
%--------------------------------------------------------------------------

for imode = 1:size(freq,1)

% KX = kpt(:,1)./abs(kpt(:,1));
% Inan = find(isnan(KX)==1); KX(Inan)=0;
% KY = kpt(:,2)./abs(kpt(:,2));
% Inan = find(isnan(KY)==1); KY(Inan)=0;
% KZ = kpt(:,3)./abs(kpt(:,3));
% Inan = find(isnan(KZ)==1); KZ(Inan)=0;

KX = kpt(:,1);
Inan = find(isnan(KX)==1); KX(Inan)=0;
KY = kpt(:,2);
Inan = find(isnan(KY)==1); KY(Inan)=0;
KZ = kpt(:,3);
Inan = find(isnan(KZ)==1); KZ(Inan)=0;


spatialx =...
    bsxfun( @times , x0(:,3) , ...
    kpt(:,1)' );
spatialy =...
    bsxfun( @times , x0(:,4) , ...
    kpt(:,2)' );
spatialz =...
    bsxfun( @times , x0(:,5) , ...
    kpt(:,3)' );

eig_fftx(imode,1:size(kpt,1)) =...
    sum(...
    bsxfun(...
    @times ,...
    bsxfun(@times,exp( 2*pi*1i*(spatialx + spatialy +spatialz)),KX') ,...
    eigvec(1:3:size(eigvec,1),imode) )...
    , 1);

eig_ffty(imode,1:size(kpt,1)) =...
    sum(...
    bsxfun(...
    @times ,...
    bsxfun(@times,exp( 2*pi*1i*(spatialx + spatialy +spatialz)),KY') ,...
    eigvec(2:3:size(eigvec,1),imode) )...
    , 1);

eig_fftz(imode,1:size(kpt,1)) =...
    sum(...
    bsxfun(...
    @times ,...
    bsxfun(@times,exp( 2*pi*1i*(spatialx + spatialy +spatialz)),KZ') ,...
    eigvec(3:3:size(eigvec,1),imode) )...
    , 1);

dsf(imode,:) =...
    real(eig_fftx(imode,:)).^2 + imag(eig_fftx(imode,:)).^2 +...
    real(eig_ffty(imode,:)).^2 + imag(eig_ffty(imode,:)).^2 +...
    real(eig_fftz(imode,:)).^2 + imag(eig_fftz(imode,:)).^2 ; 


%--------------------------------------------------------------------------
%pause
%--------------------------------------------------------------------------

end

DSF.kpt = kpt;
DSF.dsf = dsf;
DSF.freq = freq;

save(strcat(str,'DSF_long.mat'), '-struct', 'DSF');
%--------------------------------------------------------------------------
toc
%--------------------------------------------------------------------------
end




