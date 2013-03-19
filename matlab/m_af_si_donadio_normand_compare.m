clear
con = m_constant;

Di(5,10).Di =...
    m_gulp_af_si_readDi(...
    '/home/jason/disorder2/si/amor/prepare/4x/annealDonadio_600K/','Di_b10.gout');
VOLUME = (3*5.43E-10)^3;
cond(3,10) = (con.kb / VOLUME)*sum(Di(3,10).Di(:,3));

Di(3,20).Di =...
    m_gulp_af_si_readDi(...
    '/home/jason/disorder2/si/amor/prepare/4x/annealDonadio_600K/','Di_b20.gout');
VOLUME = (3*5.43E-10)^3;
cond(3,20) = (con.kb / VOLUME)*sum(Di(3,20).Di(:,3));

Di(3,100).Di =...
    m_gulp_af_si_readDi(...
    '/home/jason/disorder2/si/amor/prepare/4x/annealDonadio_600K/','Di_b100.gout');
VOLUME = (5*5.43E-10)^3;
cond(3,100) = (con.kb / VOLUME)*sum(Di(3,100).Di(:,3));

loglog(...
    Di(3,1).Di(:,2),Di(3,1).Di(:,3),...
    Di(3,10).Di(:,2),Di(3,10).Di(:,3),...
    Di(3,20).Di(:,2),Di(3,20).Di(:,3),...
    Di(3,100).Di(:,2),Di(3,100).Di(:,3),...
    linspace(min(Di(3,100).Di(:,2)),max(Di(3,100).Di(:,2)),100),...
    1E21*linspace(min(Di(3,100).Di(:,2)),max(Di(3,100).Di(:,2)),100).^(-2),...
    linspace(min(Di(3,100).Di(:,2)),max(Di(3,100).Di(:,2)),100),...
    1E47*linspace(min(Di(3,100).Di(:,2)),max(Di(3,100).Di(:,2)),100).^(-4)...
    )

cond(3,1)
cond(3,10)
cond(3,20)
cond(3,100)
    