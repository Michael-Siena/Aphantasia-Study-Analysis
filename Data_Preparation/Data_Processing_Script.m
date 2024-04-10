% Prepare MATLAB environment
% clear environment, add paths
close all; clearvars; clc;  % closes all other matlab windows, clears all variables in the workspace, and clears the command window.
dirActive = matlab.desktop.editor.getActive; % get dir of open file
cd(fileparts(dirActive.Filename)); % assign active dir to cd
addpath(genpath('./Functions/')); % adds path for Functions directory
addpath(genpath('./Data/')); % adds path for Classes directory

% read chance distribution
ChanceDistribution = readtable('ChanceDistribution/ChanceDistribution10k.csv');
[nChanceCoords, ~] = size(ChanceDistribution);

% read SAM weights
samTotalScoringWeights = readtable('SAM_Total_Scoring_Weights.csv');
samEpisodicScoringWeights = readtable('SAM_Episodic_Scoring_Weights.csv');
samFactualScoringWeights = readtable('SAM_Semantic_Scoring_Weights.csv');
samSpatialScoringWeights = readtable('SAM_Spatial_Scoring_Weights.csv');
samFutureScoringWeights = readtable('SAM_Future_Scoring_Weights.csv');

files = dir(fullfile(cd + "/Data/", "**", "SID*.json"));
mainResults = cell(length(files), 1);
for f = 1:length(files)
    % read raw data
    rawText = fileread(fullfile(files(f).folder, files(f).name));
    lines = splitlines(rawText);
    results = cell(88, 14);
    sid = str2double(extractBetween(lines{1}, '"SID', '" :'));
    results(:, 14) = {sid};
    
    % set counts
    eStayInvalCount = 0;
    eSwitchInvalCount = 0;
    aStayInvalCount = 0;
    aSwitchInvalCount = 0;

    isTestPhase = false;
    blockNum = 0;
    resultsIdx = 1;
    
    for i = 1:length(lines)
        % find blockNum 
        if (contains(lines{i}, "Block"))
            blockNum = str2double(extractBetween(lines(i), '"Block', '" :'));
        end

        % check phase
        if (contains(lines{i}, "Arithmetic"))
            isTestPhase = false;
            continue;
        elseif (contains(lines{i}, "TestPhase"))
            isTestPhase = true;
            trialNum = -1;
        end

        % if test phase, parse trial data
        if (isTestPhase)
            if (contains(lines{i}, 'TrialData'))
                % get trial conditions
                trialDataStrings = split(lines{i}, ',');

                studiedRotation = str2double(trialDataStrings{2});
                shownRotation = str2double(trialDataStrings{3});
                studiedTargetXPos = str2double(extractAfter(trialDataStrings{4}, '('));
                studiedTargetZPos = str2double(extractBetween(trialDataStrings{6}, ' ', ')'));
                memoryTestOrder = str2double(trialDataStrings{7});
                studiedPerspective = str2double(trialDataStrings{8});
                testedPerspective = str2double(trialDataStrings{9});
                switchStatus = str2double(trialDataStrings{10});

                trialData = {studiedRotation,... 
                    shownRotation,... 
                    studiedTargetXPos,... 
                    studiedTargetZPos,...
                    memoryTestOrder,...
                    studiedPerspective,...
                    testedPerspective,...
                    switchStatus};

                % Allocate Responses to Appropriate Conditions
                % Ego-Stay
                if(testedPerspective == 1 && switchStatus == 2)
                    results{resultsIdx, 1} = "eStay";
                % Ego-Switch
                elseif(testedPerspective == 1 && switchStatus == 1)
                    results{resultsIdx, 1} = "eSwitch";
                % Allo-Stay
                elseif(testedPerspective == 2 && switchStatus == 2)
                    results{resultsIdx, 1} = "aStay";
                % Allo-Switch
                elseif(testedPerspective == 2 && switchStatus == 1)
                    results{resultsIdx, 1} = "aSwitch";
                end

                % Assign responses
                if (colorVividnessRT > 0.3 && colorVividnessRT < 10)
                    results{resultsIdx, 9} = colorVividnessRating;
                    results{resultsIdx, 10} = colorVividnessRT;
                else
                    results{resultsIdx, 9} = NaN;
                    results{resultsIdx, 10} = NaN;
                end

                if (exist('locationVividnessRT', 'var')...
                    && (locationVividnessRT > 0.3 && locationVividnessRT < 10)) 
                    results{resultsIdx, 5} = locationVividnessRating;
                    results{resultsIdx, 6} = locationVividnessRT;
                else
                    results{resultsIdx, 5} = NaN;
                    results{resultsIdx, 6} = NaN;
                end

                if (exist('objectMemoryRT', 'var')...
                    && (objectMemoryRT > 0.3 && objectMemoryRT < 15))
                    % Compute abs angular deviation between studied and response
                    % hues
                    results{resultsIdx, 7} = AbsoluteAngularDifference(studiedRotation, objectMemoryResponse);
                    results{resultsIdx, 8} = objectMemoryRT;
                else
                    results{resultsIdx, 7} = NaN;
                    results{resultsIdx, 8} = NaN;
                end

                % check for invalid trials
                if ((spatialMemoryResponseX == 0 && spatialMemoryResponseZ == 0) ...
                    || spatialMemoryRT < 0.5 ...
                    || spatialMemoryRT >= 15 ...
                    || isnan(spatialMemoryRT)) 
                    results{resultsIdx, 2} = NaN;
                    results{resultsIdx, 3} = NaN;
                    results{resultsIdx, 4} = NaN;
                else
                    % compute raw error distance
                    rawErrorDistance = EuclideanDistance(spatialMemoryResponseX, spatialMemoryResponseZ,...
                        studiedTargetXPos, studiedTargetZPos);
                    results{resultsIdx, 2} = rawErrorDistance; 

                    % Compute memory score
                    % Find percentile for placement distance relative to all possible
                    % distances from chance distribution
                    nLessThanPlacementDist = 0;
                    tmpChanceDistances = cell(nChanceCoords);
                    for chanceCoord = 1:nChanceCoords
                        tmpChanceDistances{chanceCoord} = EuclideanDistance(ChanceDistribution.ChanceDistX(chanceCoord), ChanceDistribution.ChanceDistZ(chanceCoord),... 
                                                                            spatialMemoryResponseX, spatialMemoryResponseZ);

                        if (tmpChanceDistances{chanceCoord} < rawErrorDistance)                                                
                            nLessThanPlacementDist = nLessThanPlacementDist + 1;
                        end 
                    end

                    normalisedError = (nLessThanPlacementDist) / nChanceCoords;
                    memoryScore = 1 - normalisedError;

                    results{resultsIdx, 3} = memoryScore;

                    results{resultsIdx, 4} = spatialMemoryRT;
                end

                % find invalid trials
                isInvalidTrial = any(isnan([results{resultsIdx, 2: end - 1}]));
                results{resultsIdx, 13} = isInvalidTrial;

                % count invalid trials per cond
                if (isInvalidTrial)
                    if (results{resultsIdx, 1} == "eStay")
                        eStayInvalCount = eStayInvalCount + 1;
                    elseif (results{resultsIdx, 1} == "eSwitch")
                        eSwitchInvalCount = eSwitchInvalCount + 1;
                    elseif (results{resultsIdx, 1} == "aStay")
                        aStayInvalCount = aStayInvalCount + 1;
                    elseif (results{resultsIdx, 1} == "aSwitch")
                        aSwitchInvalCount = aSwitchInvalCount + 1;
                    end
                end

                resultsIdx = resultsIdx + 1;
            elseif (contains(lines{i}, 'Trial'))
                trialNum = str2double(extractBetween(lines{i}, '"Trial', '"'));
                trialData = "";

                results{resultsIdx, 11} = trialNum + 1;
                results{resultsIdx, 12} = blockNum + 1;
            elseif (contains(lines{i}, 'Block'))
                blockNum = str2double(extractBetween(lines{i}, '"Block', '"'));
            elseif (contains(lines{i}, 'Practice'))
                blockNum = 0;
            elseif (contains(lines{i}, 'ColorVividness'))
                colorVividnessStrings = split(lines{i}, ',');

                colorVividnessRating = str2double(extractAfter(colorVividnessStrings{1}, ': "'));
                colorVividnessRT = str2double(extractBetween(colorVividnessStrings{2}, ' ', '"'));

                colorVividnessData = {colorVividnessRating, colorVividnessRT};
            elseif (contains(lines{i}, 'LocationVividness'))
                locationVividnessStrings = split(lines{i}, ',');

                locationVividnessRating = str2double(extractAfter(locationVividnessStrings{1}, ': "'));
                locationVividnessRT = str2double(extractBetween(locationVividnessStrings{2}, ' ', '"'));

                locationVividnessData = {locationVividnessRating, locationVividnessRT};
            elseif (contains(lines{i}, 'ObjectMemoryPerformance'))
                objectMemoryStrings = split(lines{i}, ',');

                objectMemoryResponse = str2double(extractAfter(objectMemoryStrings{1}, ': "'));
                objectMemoryRT = str2double(extractBetween(objectMemoryStrings{2}, ' ', '"'));

                objectMemoryData = {objectMemoryResponse, objectMemoryRT};
            elseif (contains(lines{i}, 'SpatialMemoryPerformance'))
                spatialMemoryStrings = split(lines{i}, ',');

                spatialMemoryResponseX = str2double(extractAfter(spatialMemoryStrings{1}, ' "(('));
                spatialMemoryResponseZ = str2double(extractBetween(spatialMemoryStrings{3}, ' ', ')'));
                spatialMemoryRT = str2double(extractBetween(spatialMemoryStrings{5}, ' ', '"'));

                spatialMemoryData = {spatialMemoryResponseX, spatialMemoryResponseZ, spatialMemoryRT};
            elseif (contains(lines{i}, 'SAM'))
                samRaw = split(lines{i + 1}, ',');
                samRatings = samRaw(2:end-1);

                samTotalScore = 0;
                samEpisodicScore = 0;
                samFactualScore = 0;
                samSpatialScore = 0;
                samFutureScore = 0;
                for j = 1:length(samRatings)
                    samRating = str2double(samRatings{j});

                    % score SAM component scores based on weights
                    if ismember(j, 1:8)
                        samEpisodicScore = samEpisodicScore + ScoreSAMRating(j, samRating, samEpisodicScoringWeights);
                        if (j == 8)
                            samEpisodicScore = samEpisodicScore + 100;
                        end
                    elseif ismember(j, 9:14)
                        samFactualScore = samFactualScore + ScoreSAMRating(j - 8, samRating, samFactualScoringWeights);
                        if (j == 14)
                            samFactualScore = samFactualScore + 100;
                        end 
                    elseif ismember(j, 15:20)
                        samSpatialScore = samSpatialScore + ScoreSAMRating(j - 14, samRating, samSpatialScoringWeights);
                        if (j == 20)
                            samSpatialScore = samSpatialScore + 100;
                        end
                    elseif ismember(j, 21:26)
                        samFutureScore = samFutureScore + ScoreSAMRating(j - 20, samRating, samFutureScoringWeights);
                        if (j == 26)
                            samFutureScore = samFutureScore + 100;        
                        end
                    end

                    % score total score based on different weights
                    samTotalScore = samTotalScore + ScoreSAMRating(j, samRating, samTotalScoringWeights);
                    if (j == length(samRatings))
                        samTotalScore = samTotalScore + 100;
                    end
                end
            elseif (contains(lines{i}, 'VVIQ'))
                VVIQraw = split(lines{i + 1}, ',');
                VVIQratings = VVIQraw(2:end-1);
                for j = 1:length(VVIQratings)
                    VVIQratings{j} = str2double(VVIQratings{j});
                end
                VVIQtotal = sum([VVIQratings{:}]);
            end
        end    
    end
    mainResults{f} = results(1:end-8, :); % remove practice block (scored at end)
    
    fprintf("PROCESSED file %i / %i\n", f, length(files));
end

writetable(cell2table(vertcat(mainResults{:})), "TrialwiseData.csv");

%% Functions
function absAngDiff = AbsoluteAngularDifference(a, b)
    phi = mod(abs(b - a), 360);
    if (phi > 180)
        absAngDiff = 360 - phi;
    else
        absAngDiff = phi;
    end
end

function distance = EuclideanDistance(x0, y0, x1, y1)
    dX = x1 - x0;
    dY = y1 - y0;
    
    distance = sqrt((dX * dX) + (dY * dY));
end

function score = ScoreSAMRating(itemNum, rating, weights)
    score = weights{itemNum, rating + 1};
end