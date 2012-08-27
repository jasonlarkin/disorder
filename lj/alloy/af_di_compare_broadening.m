
%compare all systems biggest size

%af
str.af = '/home/jason/disorder/lj/alloy/10K/0.05/10x/work';
af(1).di=load(strcat(str.af,'/Di(wi)_broaden1_1.dat'));
str.af = '/home/jason/disorder/lj/alloy/10K/0.05/10x/work';
af(2).di=load(strcat(str.af,'/Di(wi)_1.dat'));

loglog(...
    af(1).di(:,1),af(1).di(:,2),'.',...
    af(2).di(:,1),af(2).di(:,2),'.'...
    )

%af
str.af = '/home/jason/disorder/lj/alloy/10K/0.15/10x/work';
af(1).di=load(strcat(str.af,'/Di(wi)_broaden1_1.dat'));
str.af = '/home/jason/disorder/lj/alloy/10K/0.15/10x/work';
af(2).di=load(strcat(str.af,'/Di(wi)_1.dat'));

loglog(...
    af(1).di(:,1),af(1).di(:,2),'.',...
    af(2).di(:,1),af(2).di(:,2),'.'...
    )

%af
str.af = '/home/jason/disorder/lj/alloy/10K/0.5/10x/work';
af(1).di=load(strcat(str.af,'/Di(wi)_broaden1_1.dat'));
str.af = '/home/jason/disorder/lj/alloy/10K/0.5/10x/work';
af(2).di=load(strcat(str.af,'/Di(wi)_1.dat'));

loglog(...
    af(1).di(:,1),af(1).di(:,2),'.',...
    af(2).di(:,1),af(2).di(:,2),'.'...
    )


% plot(af(1).di(:,1),cumtrapz(af(1).di(:,2))/max(cumtrapz(af(1).di(:,2))),...
% af(2).di(:,1),cumtrapz(af(2).di(:,2))/max(cumtrapz(af(2).di(:,2))))
% plot(af(1).di(:,1),cumtrapz(af(1).di(:,2)),...
% af(2).di(:,1),cumtrapz(af(2).di(:,2)))



