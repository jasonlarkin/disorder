ald10K = m_joe_read_shift('/home/jason/disorder2/lj/ald/m1/16x/Classical_Shift_Width5.txt');
ald = m_joe_read_shift('/home/jason/disorder2/lj/ald/80K/16x/Classical_Shift_Width5.txt');

loglog(ald.freq,ald.life,'.',ald10K.freq,ald10K.life,'.',ald10K.freq,2*pi./ald10K.freq)