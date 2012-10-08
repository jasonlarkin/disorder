function m_lj_cond_alloy_results_createfigure(X1, YMatrix1)
%CREATEFIGURE(X1,YMATRIX1)
%  X1:  vector of x data
%  YMATRIX1:  matrix of y data

%  Auto-generated by MATLAB on 08-Oct-2012 18:00:49

% Create figure
figure1 = figure('XVisual',...
    '0x94 (TrueColor, depth 24, RGB mask 0xff0000 0xff00 0x00ff)');

% Create axes
axes1 = axes('Parent',figure1,'YScale','log','YMinorTick','on',...
    'Units','inches',...
    'Position',[0.739555555555556 0.469333333333333 3.5 3.5]);
% Uncomment the following line to preserve the X-limits of the axes
% xlim(axes1,[0 0.5]);
% Uncomment the following line to preserve the Y-limits of the axes
% ylim(axes1,[0 5]);
box(axes1,'on');
hold(axes1,'all');

% Create multiple lines using matrix input to semilogy
semilogy1 = semilogy(X1,YMatrix1,'Parent',axes1,'LineStyle','none');
set(semilogy1(1),'MarkerSize',20,'Marker','+','DisplayName','GK');
set(semilogy1(2),'MarkerSize',20,'Marker','o',...
    'DisplayName','nmd+vc+disorder');
set(semilogy1(3),'MarkerSize',25,'Marker','.','DisplayName','ald+taud');
set(semilogy1(4),'MarkerSize',15,'Marker','.','DisplayName','nmd+vc');

% Create xlabel
xlabel({'c'},'FontSize',15);

% Create ylabel
ylabel({'$k$ (W/m-K)'},'Interpreter','latex','FontSize',15);

% Create legend
legend1 = legend(axes1,'show');
set(legend1,'Interpreter','latex','Units','inches',...
    'Position',[2.30416666666667 1.6 1.75 1.21354166666667]);

