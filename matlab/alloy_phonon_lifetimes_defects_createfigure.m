function alloy_phonon_lifetimes_defects_createfigure(X1, Y1, X2, Y2, X3, YMatrix1)
%CREATEFIGURE(X1,Y1,X2,Y2,X3,YMATRIX1)
%  X1:  vector of x data
%  Y1:  vector of y data
%  X2:  vector of x data
%  Y2:  vector of y data
%  X3:  vector of x data
%  YMATRIX1:  matrix of y data

%  Auto-generated by MATLAB on 08-Aug-2012 08:41:48

% Create figure
figure1 = figure('XVisual',...
    '0x94 (TrueColor, depth 24, RGB mask 0xff0000 0xff00 0x00ff)');

% Create axes
axes1 = axes('Parent',figure1,'YScale','log','YMinorTick','on',...
    'XScale','log',...
    'XMinorTick','on');
box(axes1,'on');
hold(axes1,'all');

% Create loglog
loglog(X1,Y1,'Parent',axes1,'Marker','.','LineStyle','none',...
    'DisplayName','\tau_{p-p}');

% Create loglog
loglog(X2,Y2,'Parent',axes1,'Marker','.','LineStyle','none',...
    'DisplayName','\tau');

% Create multiple lines using matrix input to loglog
loglog1 = loglog(X3,YMatrix1,'Parent',axes1,'Marker','.','LineStyle','none');
set(loglog1(1),'DisplayName','\tau_{d}');
set(loglog1(2),'DisplayName','1/(1/\tau_{d} + 1/\tau_{p-p})');
set(loglog1(3),'DisplayName','\omega^{-4}','Marker','none','LineStyle','-');

% Create xlabel
xlabel({'\omega (LJ)'});

% Create ylabel
ylabel({'\tau (LJ)'});

% Create legend
legend(axes1,'show');

