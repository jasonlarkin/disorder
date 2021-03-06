function m_alloy_tamura_highorder_compare_createfigure(...
    X1, Y1, X2, X3, X4, Y2, X5, Y3, X6, Y4, YMatrix1, YMatrix2, YMatrix3)
%CREATEFIGURE(X1,Y1,X2,X3,X4,Y2,X5,Y3,X6,Y4,YMATRIX1,YMATRIX2,YMATRIX3)
%  X1:  vector of x data
%  Y1:  vector of y data
%  X2:  vector of x data
%  X3:  vector of x data
%  X4:  vector of x data
%  Y2:  vector of y data
%  X5:  vector of x data
%  Y3:  vector of y data
%  X6:  vector of x data
%  Y4:  vector of y data
%  YMATRIX1:  matrix of y data
%  YMATRIX2:  matrix of y data
%  YMATRIX3:  matrix of y data

%  Auto-generated by MATLAB on 22-Jan-2013 00:19:29

% Create figure
figure1 = figure;

% Create axes
axes1 = axes('Parent',figure1,'XScale','log','XMinorTick','on',...
    'Units','inches',...
    'Position',[0.6 4.1 2.5 1.75]);
% Uncomment the following line to preserve the X-limits of the axes
% xlim(axes1,[1.000000001 30]);
% Uncomment the following line to preserve the Y-limits of the axes
% ylim(axes1,[0 0.16]);
box(axes1,'on');
hold(axes1,'all');

% Create semilogx
semilogx(X1,Y1,'Parent',axes1,'DisplayName','c=0.01');

% Create semilogx
semilogx(X2,Y1,'Parent',axes1,'DisplayName','c=0.05');

% Create semilogx
semilogx(X3,Y1,'Parent',axes1,'DisplayName','c=0.5');

% Create ylabel
ylabel({'DOS'});

% Create axes
axes2 = axes('Parent',figure1,'YScale','log','YMinorTick','on',...
    'XScale','log',...
    'XMinorTick','on',...
    'Units','inches',...
    'Position',[0.6 2.35 2.5 1.75]);
% Uncomment the following line to preserve the X-limits of the axes
% xlim(axes2,[1.0000000001 30]);
% Uncomment the following line to preserve the Y-limits of the axes
% ylim(axes2,[1e-06 4]);
box(axes2,'on');
hold(axes2,'all');

% Create loglog
loglog(X4,Y2,'Parent',axes2,'Marker','.','LineStyle','none',...
    'DisplayName','$c=0.01$');

% Create loglog
loglog(X5,Y3,'Parent',axes2,'Marker','.','LineStyle','none',...
    'DisplayName','$c=0.05$');

% Create loglog
loglog(X6,Y4,'Parent',axes2,'Marker','.','LineStyle','none',...
    'DisplayName','$c=0.5$');

% Create ylabel
ylabel({'$abs(d\omega/\omega)$'});

% Create axes
axes3 = axes('Parent',figure1,'YScale','log','YMinorTick','on',...
    'XScale','log',...
    'XMinorTick','on',...
    'Units','inches',...
    'Position',[0.6 0.6 2.5 1.75]);
% Uncomment the following line to preserve the X-limits of the axes
% xlim(axes3,[1 30]);
% Uncomment the following line to preserve the Y-limits of the axes
% ylim(axes3,[0.004 1000]);
box(axes3,'on');
hold(axes3,'all');

% Create multiple lines using matrix input to loglog
loglog1 = loglog(X4,YMatrix1,'Parent',axes3,'Marker','.','LineStyle','none');
set(loglog1(1),'DisplayName','$g_3|I(\omega)|/g_2 c=0.01$');
set(loglog1(2),'DisplayName','$g_4|I(\omega)|^2/g_2 c=0.01$');

% Create multiple lines using matrix input to loglog
loglog2 = loglog(X5,YMatrix2,'Parent',axes3,'Marker','.','LineStyle','none');
set(loglog2(1),'DisplayName','$g_3|I(\omega)|/g_2 c=0.05$');
set(loglog2(2),'DisplayName','$g_4|I(\omega)|^2/g_2 c=0.05$');

% Create multiple lines using matrix input to loglog
loglog3 = loglog(X6,YMatrix3,'Parent',axes3,'Marker','.','LineStyle','none');
set(loglog3(1),'DisplayName','$g_3|I(\omega)|/g_2 c=0.5$');
set(loglog3(2),'DisplayName','$g_4|I(\omega)|^2/g_2 c=0.5$');

% Create xlabel
xlabel({'$\omega$'});

% Create ylabel
ylabel({'$I(\omega)$'});

% Create legend
legend1 = legend(axes1,'show');
set(legend1,'Units','inches',...
    'Position',[0.730555555555558 5.17777777777778 1.00555555555556 0.569444444444444]);

% Create legend
legend2 = legend(axes3,'show');
set(legend2,'Units','inches',...
    'Position',[0.729166666666672 0.955555555555556 0.0222222222222222 1.15763888888889],...
    'FontSize',9);

% Create legend
legend3 = legend(axes2,'show');
set(legend3,'Units','inches',...
    'Position',[0.730555555555556 3.02222222222222 1.13888888888889 0.569444444444444]);

