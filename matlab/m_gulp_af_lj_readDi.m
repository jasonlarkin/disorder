function Di = m_gulp_af_lj_readDi(path,name)
Di = load([path name]);
loglog(Di(:,2),Di(:,3),'.')
end





