clear


str='/home/jason/disorder/pcbm/pcbm_pdb/isolated/c60/';

c60.old.freq = load([str '/oldtersoff/freq.gout']); 

c60.new.freq = load([str 'freq.gout']);

[c60.old.dosx c60.old.dosy] = m_dos(c60.old.freq,10,1,1); 
[c60.new.dosx c60.new.dosy] = m_dos(c60.new.freq,10,1,1);

subplot(2,1,1),...
    plot(c60.old.freq,c60.old.freq,c60.old.freq,c60.new.freq,'.')
subplot(2,1,2),...
    plot(c60.old.dosx , c60.old.dosy , c60.new.dosx , c60.new.dosy)