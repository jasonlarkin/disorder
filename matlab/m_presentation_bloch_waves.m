
x = (0:0.01:3)*pi;

subplot(2,1,1),...
    plot( x/pi, sin(x), x/pi, cos(x))
subplot(2,1,2),...
    plot( x/pi , sin(x).^2, x/pi, cos(x).^2)