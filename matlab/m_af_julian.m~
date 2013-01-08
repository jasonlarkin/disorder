
% I'll reference Phys. Rev. B 48, 12581–12588 (1993) throughout the comments
% by Eq. ().

%--------------------------------------------------------------------------
%BEGIN: AF
%--------------------------------------------------------------------------

% Inputs:
% LD.freq(1:LD.NUM_MODES), LD.Dynam(1:LD.NUM_MODES,1:LD.NUM_MODES)
% LD.eigvec(1:LD.NUM_MODES,1:LD.NUM_MODES)

%--------------------------------------------------------------------------
%Lorentzian broadening: this is necessary for finite systems, Eq. (22)
%--------------------------------------------------------------------------

%average frequency spacing used for broadening

LD.dw_avg = real(mean(LD.freq(2:LD.NUM_MODES)-LD.freq(1:LD.NUM_MODES-1)));

% wi - wj for all modes. This is redundant in i,j j,i, so needs improvement

LD.delwij = ...
    repmat(LD.freq,1,LD.NUM_MODES) - repmat(LD.freq',LD.NUM_MODES,1 ) ;

% lor broadening

LD.lor = (1.0/pi)*(LD.dw_avg./( LD.delwij.^2 + LD.dw_avg^2 ) );

% apply lor cutoff, useful if using for loops. LD.deltaL should be an input 
% parameter. You can basically skip modes which do not interact strongly.

I = find(LD.deltaL>LD.lor); LD.lor(I) = 0;

%--------------------------------------------------------------------------


%REPEAT FOR Y,Z------------------------------------------------------------
%--------------------------------------------------------------------------

% find atom pair separations for use in Eq. (A8). To vectorize, I make 
% these RIJ,X,Y,Z the same size as LD.Dynam.  This is obviously very 
% inefficient and a different method is needed. 
% LD.rijx,y,z(1:LD.NUM_ATOMS,1:LD.NUM_ATOMS)

LD.RIJX = zeros(LD.NUM_MODES,LD.NUM_MODES);

LD.RIJX(1:3:LD.NUM_MODES,1:3:LD.NUM_MODES) = LD.rijx;
LD.RIJX(1:3:LD.NUM_MODES,2:3:LD.NUM_MODES) = LD.rijx;
LD.RIJX(1:3:LD.NUM_MODES,3:3:LD.NUM_MODES) = LD.rijx;

LD.RIJX(2:3:LD.NUM_MODES,1:3:LD.NUM_MODES) = LD.rijx;
LD.RIJX(2:3:LD.NUM_MODES,2:3:LD.NUM_MODES) = LD.rijx;
LD.RIJX(2:3:LD.NUM_MODES,3:3:LD.NUM_MODES) = LD.rijx;

LD.RIJX(3:3:LD.NUM_MODES,1:3:LD.NUM_MODES) = LD.rijx;
LD.RIJX(3:3:LD.NUM_MODES,2:3:LD.NUM_MODES) = LD.rijx;
LD.RIJX(3:3:LD.NUM_MODES,3:3:LD.NUM_MODES) = LD.rijx;

%--------------------------------------------------------------------------

% find x term of Eq. (A8).

LD.vij_x =...
    ((1/2)./sqrt(LD.freq*LD.freq')).*...
    (LD.eigvec'*bsxfun(@times,LD.Dynam,LD.RIJX)*LD.eigvec);

% remove divide by 0 freqs

Inan = isnan(LD.vij_x); I = Inan==1; LD.vij_x(I) = 0.0;
Iinf = isinf(LD.vij_x); I = Iinf==1; LD.vij_x(I) = 0.0;

% find x term Eq. (A7)

LD.Sij_x =...
    1/(2)*LD.vij_x.*...
    (repmat(LD.freq',LD.NUM_MODES,1) + repmat(LD.freq,1,LD.NUM_MODES));

% set diagonal to 0, modes don't interact with themselves

LD.Sij_x(1:LD.NUM_MODES+1:LD.NUM_MODES*LD.NUM_MODES) = 0;

% apply lor broadening

LD.Sij_x = LD.lor.*( abs(LD.Sij_x).^2 );

% add its contribution to Di Eq. (22), average over x,y,z. It should be
% optional to not average over x,y,z

LD.Dix = (pi./(3*LD.freq.^2) ).* ( sum(LD.Sij_x,2) );

% remove divide by 0 freq

Inan = isnan(LD.Dix); I = Inan==1; LD.Dix(I) = 0.0;

%--------------------------------------------------------------------------
%REPEAT FOR Y,Z------------------------------------------------------------


%--------------------------------------------------------------------------

LD.Diy = (pi./(3*LD.freq.^2) ).* ( sum(LD.Sij_y,2) );

% remove divide by 0 freq
Inan = isnan(LD.Diy); I = Inan==1; LD.Diy(I) = 0.0;

LD.Diz = (pi./(3*LD.freq.^2) ).* ( sum(LD.Sij_z,2) );

% remove divide by 0 freq
Inan = isnan(LD.Diz); I = Inan==1; LD.Diz(I) = 0.0;

%--------------------------------------------------------------------------
%END: AF
%--------------------------------------------------------------------------

% optionally, can calculate the thermal conductivity kappa, but this could
% easily be done post-processing.

LD.kappa =...
    sum(LD.Di)*...
    ((x0.LJ.sigma^2)/x0.LJ.tau)*x0.constant.kb/(LD.VOLUME*x0.LJ.sigma^3);


