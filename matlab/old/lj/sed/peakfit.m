function [FitResults,LowestError,BestStart]=peakfit(signal,center,window,NumPeaks,peakshape,extra,NumTrials,start)
% PEAKFIT(signal);
% Performs an iterative least-squares fit of a single Gaussian  
% peak to the data matrix "signal", which has x values 
% in row 1 and Y values in row 2 (e.g. [x y])
%
% PEAKFIT(signal,center,window);
% Fits a single Gaussian peak to a portion of the 
% matrix "signal". The portion is centered on the 
% x-value "center" and has width "window" (in x units).
% 
% PEAKFIT(signal,center,window,NumPeaks);
% "NumPeaks" = number of peaks in the model (default is 1 if not
% specified). 
% 
% PEAKFIT(signal,center,window,NumPeaks,peakshape);
% Specifies the peak shape of the model: "peakshape" = 1-5.
% (1=Gaussian (default), 2=Lorentzian, 3=logistic, 4=Pearson, and 
% 5=exponentionally broadened Gaussian) 
% 
% PEAKFIT(signal,center,window,NumPeaks,peakshape,extra)
% Specifies the value of 'extra', used in the Pearson and the
% exponentionally broadened Gaussian shapes to fine-tune the peak shape. 
% 
% PEAKFIT(signal,center,window,NumPeaks,peakshape,extra,NumTrials);
% Performs "NumTrials" trial fits and selects the best one (with lowest
% fitting error). NumTrials can be any positive integer (default is 1).
%
% PEAKFIT(signal,center,window,NumPeaks,peakshape,extra,NumTrials,start)
% Specifies the first guesses vector "firstguess" for the peak positions
%  and widths, e.g. start=[position1 width1 position2 width2 ...]
% 
% [FitResults,MeanFitError]=PEAKFIT(signal,center,window...)
% Returns the FitResults vector in the order peak number, peak
% position, peak height, peak width, and peak area), and the MeanFitError
% (the percent RMS difference between the data and the model in the
%  selected segment of that data) of the best fit.
%
% T. C. O'Haver (toh@umd.edu).  Version 1.1: April 14, 2009. 
%
% Example 1: 
% >> x=[0:.005:1];y=humps(x);peakfit([x' y'])
% Fits the humps function with a single Gaussian peak model.
%
% Example 2: 
%   >> x=[0:.005:1];y=humps(x);peakfit([x' y'],.3,.7,1,4,3);
%   Fits a portion of the humps function, 0.7 units wide and centered on 
%   x=0.3, with a single (NumPeaks=1) Pearson function (peakshape=4)
%   with extra=3, using a single trial and default start vector.
%
% Example 3: 
%  >> x=[0:.005:1];y=(humps(x)+humps(x-.13)).^3;smatrix=[x' y'];
%  >> [FitResults,MeanFitError]=peakfit(smatrix,.4,.7,2,1,0,10)
%  Creates a data matrix 'smatrix', fits a portion to a two-peak Gaussian
%  model, takes the best of 10 trials.  Returns FitResults and MeanFitError.
%  FitResults =
%              1       0.4128  3.1114e+008      0.10448  3.4605e+007
%              2       0.3161  2.8671e+008     0.098862  3.0174e+007
%  MeanFitError =
%        0.68048
%
% Example 4:
% >> peakfit([x' y'],.4,.7,2,1,0,10,[.3 .1 .5 .1]);
% As above, but specifies the first-guess position and width of the two
% peaks, in the order [position1 width1 position2 width2]

global PEAKHEIGHTS
format short g
format compact
warning off all
AUTOZERO=1;

% X=[1:length(Y)]; % Use only to create an x vector if signal does not have one
X=signal(:,1);
Y=signal(:,2);
X=reshape(X,1,length(X)); % Adjust X and Y vector shape to 1 x n (rather than n x 1)
Y=reshape(Y,1,length(Y));
Y=Y-min(Y);  % Remove excess offset from data
% Isolate desired segment from data set for curve fitting
if nargin==1,center=(max(X)-min(X))/2;window=max(X)-min(X);,end
xoffset=center-window;
n1=val2ind(X,center-window/2);
n2=val2ind(X,center+window/2);
xx=X(n1:n2)-xoffset;
yy=Y(n1:n2);

% Remove linear baseline from data segment
if AUTOZERO==1,
  X1=min(xx);
  X2=max(xx);
  Y1=mean(yy(1:length(xx)/10));
  Y2=mean(yy((length(xx)-length(xx)/10):length(xx)));
  yy=yy-((Y2-Y1)/(X2-X1)*(xx-X1)+Y1);
end % if

% Define values of any missing arguments
switch nargin
    case 1
        NumPeaks=1;
        peakshape=1;
        extra=0;
        NumTrials=1;
        start=calcstart(xx,NumPeaks,xoffset);
    case 3
        NumPeaks=1;
        peakshape=1;
        extra=0;
        NumTrials=1;
        start=calcstart(xx,NumPeaks,xoffset);
    case 4
        peakshape=1;
        extra=0;
        NumTrials=1;
        start=calcstart(xx,NumPeaks,xoffset);
    case 5
        extra=0;
        NumTrials=1;
        start=calcstart(xx,NumPeaks,xoffset);
    case 6
        NumTrials=1;
        start=calcstart(xx,NumPeaks,xoffset);
    case 7
        start=calcstart(xx,NumPeaks,xoffset);
    otherwise
end % switch nargin

PEAKHEIGHTS=zeros(1,NumPeaks);
n=length(xx);
newstart=start;
switch NumPeaks
    case 1
        newstart(1)=start(1)-xoffset;
    case 2
        newstart(1)=start(1)-xoffset;
        newstart(3)=start(3)-xoffset;
    case 3
        newstart(1)=start(1)-xoffset;
        newstart(3)=start(3)-xoffset;
        newstart(5)=start(5)-xoffset;
    case 4
        newstart(1)=start(1)-xoffset;
        newstart(3)=start(3)-xoffset;
        newstart(5)=start(5)-xoffset;
        newstart(7)=start(7)-xoffset;
     case 5
        newstart(1)=start(1)-xoffset;
        newstart(3)=start(3)-xoffset;
        newstart(5)=start(5)-xoffset;
        newstart(7)=start(7)-xoffset;        
        newstart(9)=start(9)-xoffset;
    otherwise       
end % switch NumPeaks

% Perform peak fitting for selected peak shape using fminsearch function
options = optimset('TolX',.001,'TypicalX',center,'Display','off' );
LowestError=1000;
FitParameters=zeros(NumPeaks.*2); 
BestStart=zeros(NumPeaks.*2); 
height=zeros(NumPeaks); 
bestmodel=zeros(yy);
for k=1:NumTrials, 
    disp(k) % prints the current trial number as progress indicator
    % newstart
  switch peakshape
    case 1
        TrialParameters=fminsearch(@fitgaussian,newstart,options,xx,yy);
        ShapeString='Gaussian';
    case 2
        TrialParameters=fminsearch(@fitlorentzian,newstart,options,xx,yy);
        ShapeString='Lorentzian';    
    case 3
        TrialParameters=fminsearch(@fitlogistic,newstart,options,xx,yy);
        ShapeString='Logistic';
    case 4
        TrialParameters=fminsearch(@fitpearson,newstart,options,xx,yy,extra);
        ShapeString='Pearson';
    case 5
        TrialParameters=fminsearch(@fitexpgaussian,newstart,options,xx,yy,-extra);
        ShapeString='ExpGaussian';
    otherwise
end % switch peakshape

% Construct model from Trial parameters
A=zeros(NumPeaks,n);
for m=1:NumPeaks,
   switch peakshape
    case 1
        A(m,:)=gaussian(xx,TrialParameters(2*m-1),TrialParameters(2*m));
    case 2
        A(m,:)=lorentzian(xx,TrialParameters(2*m-1),TrialParameters(2*m));
    case 3
        A(m,:)=logistic(xx,TrialParameters(2*m-1),TrialParameters(2*m));
    case 4
        A(m,:)=pearson(xx,TrialParameters(2*m-1),TrialParameters(2*m),extra);
    case 5
        A(m,:)=expgaussian(xx,TrialParameters(2*m-1),TrialParameters(2*m),-extra)';
    otherwise
  end % switch
      switch NumPeaks % adds random variation to non-linear parameters
       case 1
          newstart=[newstart(1)*(1+randn/50) newstart(2)*(1+randn/10)]; 
       case 2
          newstart=[newstart(1)*(1+randn/50) newstart(2)*(1+randn/10) newstart(3)*(1+randn/50) newstart(4)*(1+randn/10)]; 
       case 3
          newstart=[newstart(1)*(1+randn/50) newstart(2)*(1+randn/10) newstart(3)*(1+randn/50) newstart(4)*(1+randn/10) newstart(5)*(1+randn/50) newstart(6)*(1+randn/10)]; 
       case 4
          newstart=[newstart(1)*(1+randn/50) newstart(2)*(1+randn/10) newstart(3)*(1+randn/50) newstart(4)*(1+randn/10) newstart(5)*(1+randn/50) newstart(6)*(1+randn/10)  newstart(7)*(1+randn/50) newstart(8)*(1+randn/10)]; 
       case 5
          newstart=[newstart(1)*(1+randn/50) newstart(2)*(1+randn/10) newstart(3)*(1+randn/50) newstart(4)*(1+randn/10) newstart(5)*(1+randn/50) newstart(6)*(1+randn/10)  newstart(7)*(1+randn/50) newstart(8)*(1+randn/10)  newstart(9)*(1+randn/50) newstart(10)*(1+randn/10)]; 
      otherwise       
    end % switch NumPeaks
end % for
% Multiplies each row by the corresponding amplitude and adds them up
model=PEAKHEIGHTS'*A;

% Compare trial model to data segment and computer fit error
  MeanFitError=100*norm(yy-model)./(sqrt(n)*max(yy));
  % Take only the single fit that has the lowest MeanFitError
  if MeanFitError<LowestError, 
      if min(PEAKHEIGHTS)>0,  % Consider only fits with positive peak heights
        LowestError=MeanFitError;  % Assign LowestError to the lowest MeanFitError
        FitParameters=TrialParameters;  % Assign FitParameters to the fit with the lowest MeanFitError
        BestStart=newstart; % Assign BestStart to the start with the lowest MeanFitError
        height=PEAKHEIGHTS; % Assign height to the PEAKHEIGHTS with the lowest MeanFitError
        bestmodel=model; % Assign bestmodel to the model with the lowest MeanFitError
      end % if min
  end % if MeanFitError
end % if k

% Construct model from best-fit parameters
AA=zeros(NumPeaks,100);
xxx=linspace(min(xx),max(xx));
for m=1:NumPeaks,
   switch peakshape
    case 1
        AA(m,:)=gaussian(xxx,FitParameters(2*m-1),FitParameters(2*m));
    case 2
        AA(m,:)=lorentzian(xxx,FitParameters(2*m-1),FitParameters(2*m));
    case 3
        AA(m,:)=logistic(xxx,FitParameters(2*m-1),FitParameters(2*m));
    case 4
        AA(m,:)=pearson(xxx,FitParameters(2*m-1),FitParameters(2*m),extra);
    case 5
        AA(m,:)=expgaussian(xxx,FitParameters(2*m-1),FitParameters(2*m),-extra)';
    otherwise
  end % switch
end % for

% Multiplies each row by the corresponding amplitude and adds them up
mmodel=height'*AA;

% Top half of the figure shows original signal and the fitted model.
subplot(2,1,1);plot(xx+xoffset,yy,'b.'); % Plot the original signal in blue dots
hold on
for m=1:NumPeaks,
    plot(xxx+xoffset,height(m)*AA(m,:),'g')  % Plot the individual component peaks in green lines
    area(m)=trapz(xxx+xoffset,height(m)*AA(m,:)); % Compute the area of each component peak using trapezoidal method
end

% Mark starting peak positions with vertical dashed lines
for marker=1:NumPeaks,
    markx=BestStart((2*marker)-1);
    subplot(2,1,1);plot([markx+xoffset markx+xoffset],[0 max(yy)],'m--')
end % for
plot(xxx+xoffset,mmodel,'r');  % Plot the total model (sum of component peaks) in red lines
hold off;
axis([min(xx)+xoffset max(xx)+xoffset min(yy) max(yy)]);
title('Peak  X position    Peak height    Peak width     Peak area                            ')
xlabel(['Peaks = ' num2str(NumPeaks) '     Shape = ' ShapeString '     Error = ' num2str(LowestError) '     Extra = ' num2str(extra) ] )

% Bottom half of the figure shows the residuals and displays RMS error
% between original signal and model
residual=yy-bestmodel;
subplot(2,1,2);plot(xx+xoffset,residual,'b.')
axis([min(xx)+xoffset max(xx)+xoffset min(residual) max(residual)]);
xlabel('Residual Plot')

% Put results into a matrix, one row for each peak, showing peak index number,
% position, amplitude, and width.
for m=1:NumPeaks,
    if m==1,
       FitResults=[round(m) FitParameters(2*m-1)+xoffset height(m) abs(FitParameters(2*m)) area(m)];
    else
       FitResults=[FitResults ; [round(m) FitParameters(2*m-1)+xoffset height(m) abs(FitParameters(2*m)) area(m)]];
    end % if
end % for

% Display Fit Results on upper graph
subplot(2,1,1);
residualaxis=axis;
text(residualaxis(1), 0.8*residualaxis(4) ,num2str(FitResults));
% ----------------------------------------------------------------------
function start=calcstart(xx,NumPeaks,xoffset)
  n=max(xx)-min(xx);
  start=[];
  startpos=[n/(NumPeaks+1):n/(NumPeaks+1):n-(n/(NumPeaks+1))]+min(xx);
  for marker=1:NumPeaks,
      markx=startpos(marker)+ xoffset;
      start=[start markx n/5];
  end % for marker
% ----------------------------------------------------------------------
function [index,closestval]=val2ind(x,val)
% Returns the index and the value of the element of vector x that is closest to val
% If more than one element is equally close, returns vectors of indicies and values
% Tom O'Haver (toh@umd.edu) October 2006
% Examples: If x=[1 2 4 3 5 9 6 4 5 3 1], then val2ind(x,6)=7 and val2ind(x,5.1)=[5 9]
% [indices values]=val2ind(x,3.3) returns indices = [4 10] and values = [3 3]
dif=abs(x-val);
index=find((dif-min(dif))==0);
closestval=x(index);
% ----------------------------------------------------------------------
function err = fitgaussian(lambda,t,y)
% Fitting function for a Gaussian band signal.
global PEAKHEIGHTS
A = zeros(length(t),round(length(lambda)/2));
for j = 1:length(lambda)/2,
    A(:,j) = gaussian(t,lambda(2*j-1),lambda(2*j))';
end
PEAKHEIGHTS = abs(A\y');
z = A*PEAKHEIGHTS;
err = norm(z-y');
% ----------------------------------------------------------------------
function g = gaussian(x,pos,wid)
%  gaussian(X,pos,wid) = gaussian peak centered on pos, half-width=wid
%  X may be scalar, vector, or matrix, pos and wid both scalar
% Examples: gaussian([0 1 2],1,2) gives result [0.5000    1.0000    0.5000]
% plot(gaussian([1:100],50,20)) displays gaussian band centered at 50 with width 20.
g = exp(-((x-pos)./(0.6006.*wid)) .^2);
% ----------------------------------------------------------------------
function err = fitlorentzian(lambda,t,y)
%	Fitting function for single lorentzian, lambda(1)=position, lambda(2)=width
%	Fitgauss assumes a lorentzian function 
global PEAKHEIGHTS
A = zeros(length(t),round(length(lambda)/2));
for j = 1:length(lambda)/2,
    A(:,j) = lorentzian(t,lambda(2*j-1),lambda(2*j))';
end
PEAKHEIGHTS = A\y';
z = A*PEAKHEIGHTS;
err = norm(z-y');
% ----------------------------------------------------------------------
function g = lorentzian(x,position,width)
% lorentzian(x,position,width) Lorentzian function.
% where x may be scalar, vector, or matrix
% position and width scalar
% T. C. O'Haver, 1988
% Example: lorentzian([1 2 3],2,2) gives result [0.5 1 0.5]
g=ones(size(x))./(1+((x-position)./(0.5.*width)).^2);
% ----------------------------------------------------------------------
function err = fitlogistic(lambda,t,y)
%	Fitting function for logistic, lambda(1)=position, lambda(2)=width
%	between the data and the values computed by the current
%	function of lambda.  Fitlogistic assumes a logistic function 
%  T. C. O'Haver, May 2006
global PEAKHEIGHTS
A = zeros(length(t),round(length(lambda)/2));
for j = 1:length(lambda)/2,
    A(:,j) = logistic(t,lambda(2*j-1),lambda(2*j))';
end
PEAKHEIGHTS = A\y';
z = A*PEAKHEIGHTS;
err = norm(z-y');
% ----------------------------------------------------------------------
function g = logistic(x,pos,wid)
% logistic function.  pos=position; wid=half-width (both scalar)
% logistic(x,pos,wid), where x may be scalar, vector, or matrix
% pos=position; wid=half-width (both scalar)
% T. C. O'Haver, 1991 
n = exp(-((x-pos)/(.477.*wid)) .^2);
g = (2.*n)./(1+n);
% ----------------------------------------------------------------------
function err = fitlognormal(lambda,t,y)
%	Fitting function for lognormal, lambda(1)=position, lambda(2)=width
%	between the data and the values computed by the current
%	function of lambda.  Fitlognormal assumes a lognormal function 
%  T. C. O'Haver, May 2006
global PEAKHEIGHTS
A = zeros(length(t),round(length(lambda)/2));
for j = 1:length(lambda)/2,
    A(:,j) = lognormal(t,lambda(2*j-1),lambda(2*j))';
end
PEAKHEIGHTS = A\y';
z = A*PEAKHEIGHTS;
err = norm(z-y');
% ----------------------------------------------------------------------
function g = lognormal(x,pos,wid)
% lognormal function.  pos=position; wid=half-width (both scalar)
% lognormal(x,pos,wid), where x may be scalar, vector, or matrix
% pos=position; wid=half-width (both scalar)
% T. C. O'Haver, 1991  
g = exp(-(log(x/pos)/(0.01.*wid)) .^2);
% ----------------------------------------------------------------------
function err = fitpearson(lambda,t,y,shapeconstant)
%   Fitting functions for a Pearson 7 band signal.
% T. C. O'Haver (toh@umd.edu),   Version 1.3, October 23, 2006.
global PEAKHEIGHTS
A = zeros(length(t),round(length(lambda)/2));
for j = 1:length(lambda)/2,
    A(:,j) = pearson(t,lambda(2*j-1),lambda(2*j),shapeconstant)';
end
PEAKHEIGHTS = A\y';
z = A*PEAKHEIGHTS;
err = norm(z-y');
% ----------------------------------------------------------------------
function g = pearson(x,pos,wid,m)
% Pearson VII function. 
% g = pearson7(x,pos,wid,m) where x may be scalar, vector, or matrix
% pos=position; wid=half-width (both scalar)
% m=some number
%  T. C. O'Haver, 1990  
g=ones(size(x))./(1+((x-pos)./((0.5.^(2/m)).*wid)).^2).^m;
% ----------------------------------------------------------------------
function err = fitexpgaussian(lambda,t,y,timeconstant)
%   Fitting functions for a exponentially-broadened Gaussian band signal.
%  T. C. O'Haver, October 23, 2006.
global PEAKHEIGHTS
A = zeros(length(t),round(length(lambda)/2));
for j = 1:length(lambda)/2,
    A(:,j) = expgaussian(t,lambda(2*j-1),lambda(2*j),timeconstant);
end
PEAKHEIGHTS = A\y';
z = A*PEAKHEIGHTS;
err = norm(z-y');
% ----------------------------------------------------------------------
function g = expgaussian(x,pos,wid,timeconstant)
%  Exponentially-broadened gaussian(x,pos,wid) = gaussian peak centered on pos, half-width=wid
%  x may be scalar, vector, or matrix, pos and wid both scalar
%  T. C. O'Haver, 2006
g = exp(-((x-pos)./(0.6006.*wid)) .^2);
g = ExpBroaden(g',timeconstant);
% ----------------------------------------------------------------------
function yb = ExpBroaden(y,t)
% ExpBroaden(y,t) convolutes y by an exponential decay of time constant t
% by multiplying Fourier transforms and inverse transforming the result.
fy=fft(y);
a=exp(-[1:length(y)]./t);
fa=fft(a);
fy1=fy.*fa';           
yb=real(ifft(fy1))./sum(a);  
% ----------------------------------------------------------------------
function err = fitNewPeakShape(lambda,x,y)
% Replace "NewPeakShape" with the peak function of your choice.
%  T. C. O'Haver, 2006   
global PEAKHEIGHTS
A = zeros(length(x),round(length(lambda)/2));
for j = 1:length(lambda)/2,
    A(:,j) = NewPeakShape(x,lambda(2*j-1),lambda(2*j))';
end
PEAKHEIGHTS = A\y';
z = A*PEAKHEIGHTS;
err = norm(z-y');
% ----------------------------------------------------------------------
function g = NewPeakShape(x,a,b)
% Replace this with the peak function of your choice.
% a and b are the two non-linear parameters.
g=sin(a.*x+b);