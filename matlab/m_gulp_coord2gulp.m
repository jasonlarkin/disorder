function coords = m_gulp_coord2gulp( x0 , a_type )
%-------------------------------------------------------------------------- 

lj = m_lj; constant = m_constant;

formats ='%2.6f';

for l=1:1:length(x0.x)
    coords1{l}=...
        [a_type(l).str ' core ' ...
        num2str(x0.x(l),formats) ' ' ...
        num2str(x0.y(l),formats) ' ' ...
        num2str(x0.z(l),formats) ' ' ...
        '0 1 1 1'];
end
    coords = sprintf('%s \n',coords1{:})
end
