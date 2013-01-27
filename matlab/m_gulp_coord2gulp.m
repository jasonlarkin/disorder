function coords = m_gulp_coord2gulp( x0 , str_gin )
%-------------------------------------------------------------------------- 

lj = m_lj; constant = m_constant;

formats ='%2.6f';

for l=1:1:length(x0.id(:,1))
    coords1{l}=...
        [x0.atype(x0.m(l)).str '   core ' ...
        num2str((lj.sigma/constant.ang2m)*x0.x(l),formats) ' ' ...
        num2str((lj.sigma/constant.ang2m)*x0.y(l),formats) ' ' ...
        num2str((lj.sigma/constant.ang2m)*x0.z(l),formats) ' ' ...
        '0 1 1 1'];
end
    coords = sprintf('%s \n',coords1{:})
end
