
str.af = '/home/jason/disorder/lj/amor/4x/AF/0K/';
af(1).freq=load(strcat(str.af,'AF_freq_1.dat'));
str.af = '/home/jason/disorder/lj/amor/4x/AF/0K/';
af(1).eigvec=load(strcat(str.af,'AF_eigvec_1.dat'));
str.af = '/home/jason/disorder/lj/amor/4x/AF/0K/';
af(1).di=load(strcat(str.af,'AF_Di(wi)_1.dat'));

str.af = '/home/jason/disorder/lj/amor/4x/AF/10K/';
af(2).freq=load(strcat(str.af,'AF_freq_1.dat'));
str.af = '/home/jason/disorder/lj/amor/4x/AF/10K/';
af(2).eigvec=load(strcat(str.af,'AF_eigvec_1.dat'));
str.af = '/home/jason/disorder/lj/amor/4x/AF/10K/';
af(2).di=load(strcat(str.af,'AF_Di(wi)_1.dat'));


[af(1).dos.y af(1).dos.x] =...
    hist(af(1).freq,20);

[af(2).dos.y af(2).dos.x] =...
    hist(af(2).freq,20);

plot(...
    af(1).dos.x,af(1).dos.y,'.',...
    af(2).dos.x,af(2).dos.y,'.'...
    )

plot(...
    af(1).eigvec - af(2).eigvec,'.')


plot(...
    af(1).di(:,1),af(1).di(:,2),'.',...
    af(2).di(:,1),af(2).di(:,2),'.'...
    )

