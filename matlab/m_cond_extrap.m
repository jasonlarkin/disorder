function [p R] = m_cond_extrap(size, cond)
%function m_cond_extrap(size , cond)

p=polyfit(1./size,1./cond,1);
R=corrcoef(1./size,1./cond);

end
