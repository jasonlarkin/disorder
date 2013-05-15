function Di_convert = m_gulp_af_si_readDi(path,name)
con = m_constant; lj = m_lj;
Di = load([path name]);
Di_convert(:,2) = Di(:,2)*con.c*2*pi;
Di_convert(:,3) = Di(:,3)/(100^2); %/(4*pi); %missing factor of ~13

plot(Di_convert(:,2),Di_convert(:,3),'.')
end
