clear
clf


fH = openfig('/home/jason/disorder/si/amor/m_af_si_normand_4096_D_4_1.fig');
lineH = findobj(fH, 'type', 'line'); % get handles of lines
xData = get(lineH, 'xdata'); % get x-data
yData = get(lineH, 'ydata'); % get y-data

[I(1).I,J(1).J] = find( xData{11} < 4.55E12);
figure
subplot(2,1,1),...
loglog(...
    xData{11},yData{11},'.',...
    xData{12}(J(1).J),yData{12}(J(1).J),'.',...
    xData{9},yData{9},...
    xData{10},yData{10}...
    )
axis([ 1E12 4.0E14 1E-7 1E-4])

fH2 = openfig('/home/jason/disorder/si/amor/m_af_si_normand_4096_D_4_2.fig');
lineH2 = findobj(fH2, 'type', 'line'); % get handles of lines
xData2 = get(lineH2, 'xdata'); % get x-data
yData2 = get(lineH2, 'ydata'); % get y-data
[I(2).I,J(2).J] = find( xData2{14} < 1.16E13);

subplot(2,1,2),...
loglog(...
    xData2{14},yData2{14},'.',...
    xData2{15}(J(2).J),yData2{15}(J(2).J),'.',...
    xData2{11},yData2{11},...
    xData2{12},yData2{12},...
    xData2{13},yData2{13}...
    )
axis([ 1E12 4.0E14 1E-7 1E-4])
    

pause


[I(2).I,J(2).J] = sort(sio2.SED(2).SED.freq(1:end));

[I(2).I,J(2).J] = find(sio2.debye.wcut > sio2.SED(2).SED.freq(1:end));
subplot(2,1,1),...
loglog(...
    sio2.SED(2).SED.freq(1:end),...
    ((1/3))*(0.78*siO2.vs_tran)^2*(sio2.SED(2).SED.life(1:end)),'.',...
    sio2.D(4,3).D(1:end,2),sio2.D(4,3).D(1:end,3),'.',...  %sio2.D(1,3).D(3:end,2),sio2.D(1,3).D(3:end,3),'.',...       %sio2.D(4,3).D(1:end,2),sio2.D(4,3).D(1:end,3),'.',...
    linspace(min(Di(8,10).Di(:,2)),3E14,100),...
    (1/3)*(4.8E-10)*siO2.vs_tran*ones(100,1),...
    linspace( 1E12 , sio2.debye.wcut ,100),...
    sio2.debye.B*linspace( 1E12 , sio2.debye.wcut ,100).^(-2)...
    )

    




