clear

con = m_constant; lj = m_lj;

P = 10.01E5;
T = 320;
NUM_ATOMS = 256;

V = NUM_ATOMS*con.kb*T / P 

alat = V^(1/3) / lj.sigma



