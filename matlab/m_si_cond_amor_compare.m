clear

gk = m_si_cond_amor_gk_results

af = m_si_cond_amor_af_results

plot(...
1./gk.size,gk.cond,'.',...
1./af.size,af.cond,'.'...
)