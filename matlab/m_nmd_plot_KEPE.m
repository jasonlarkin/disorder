
function m_nmd_plot_KEPE(str)
%m_nmd_plot_KEPE(str) plots the nmd KE then PE 

PE = load(strcat(str,'PE.dat'));
KE = load(strcat(str,'KE.dat'));
figure
plot(1:size(KE,2),KE,1:size(PE,2),PE)
mean(KE)
mean(PE)

end