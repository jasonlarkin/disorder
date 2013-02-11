function Di = m_gulp_af_lj_readDi(path,name)
con = m_constant; lj = m_lj;
Di = load([path name]);
%gets to m^2/s
factor.gulp = con.eV2J^2*con.avog^2/con.ang2m^2/con.c
con.eV2J
con.avog
con.ang2m
con.c
%gets to lj units
factor.lj = lj.tau/lj.sigma^2
Di_convert(:,3) = Di(:,3)*factor.gulp*factor.lj;
Di_convert(:,2) = Di(:,2)*con.c*lj.tau;
loglog(Di_convert(:,2),Di_convert(:,3),'.')
end





