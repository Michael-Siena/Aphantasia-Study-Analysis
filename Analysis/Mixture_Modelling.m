% Prepare MATLAB environment
% clear environment, add paths
close all; clearvars; clc;  % closes all other matlab windows, clears all variables in the workspace, and clears the command window.
dirActive = matlab.desktop.editor.getActive; % get dir of open file
cd(fileparts(dirActive.Filename)); % assign active dir to cd
addpath(genpath('./Functions/')); % adds path for Functions directory
addpath(genpath('./Data/')); % adds path for Classes directory

trialwiseData = readtable('TrialwiseData_ObjResps.csv');
data = trialwiseData(trialwiseData.Var13 == 0, :);

data.TargetInRadians = wrap(data.Var15 / 180 * pi);
data.RespInRadians = wrap(data.Var16 / 180 * pi);

sids = unique(data.Var17);
modelData = cell(length(sids), 5);

% Modelling to separate all-or-none accuracy from response precision
for i = 1:length(sids)
    sid = sids(i);
    modelData{i, 5} = sid;

    firstPersonData = data(data.Var17 == sid & (strcmp(data.Var1, 'eStay') | strcmp(data.Var1, 'aSwitch')), :);
    thirdPersonData = data(data.Var17 == sid & (strcmp(data.Var1, 'aStay') | strcmp(data.Var1, 'eSwitch')), :);
    
    fpMod = mixtureFit(firstPersonData.RespInRadians, firstPersonData.TargetInRadians);
    modelData{i, 1} = fpMod(1);
    modelData{i, 2} = fpMod(2);

    tpMod = mixtureFit(thirdPersonData.RespInRadians, thirdPersonData.TargetInRadians);
    modelData{i, 3} = tpMod(1);
    modelData{i, 4} = tpMod(2);    
end

%'eStay' 'aSwitch'
%'eSwitch' 'aStay'

% [B LL W] = mixtureFit(data.RespInRadians, data.TargetInRadians);
% data.pVM = W(:, 1);
% 
% modelBasedGuessTrials = data(data.pVM < 0.05, :);
% 
% angDevGuesses = AbsoluteAngularDifference(modelBasedGuessTrials.Var15, modelBasedGuessTrials.Var16);
% angDevGuessCutOff = min(angDevGuesses);
% 
function absAngDiff = AbsoluteAngularDifference(a, b)
    phi = mod(abs(b - a), 360);
    if (phi > 180)
        absAngDiff = 360 - phi;
    else
        absAngDiff = phi;
    end
end
