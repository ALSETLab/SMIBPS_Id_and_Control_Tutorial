% This script helps parametrizing the line to be removed
% because the total of the original line_1 is 0.5, we can calculate
% the value of the line to be removed to have a worse damping after it's
% removal
% 
clear all, clc
xt = 0.5; %total line impedance, for reference
% a is the ratio of the line to be removed x2 vs. the line that stays x1
a = 5.5 % this is the case used for the example
% a = 6.06265 % this case is at the critical point when using a tolerance of 1e-6
% a = 6.0627 % is the boundary at which the system cannot keep stability
x2 = (a+1)./(2*a)
x1 = a.*x2

% % To confirm the calculation is correct uncomment this
% xtcalc = 1./(1./x1+1./x2);