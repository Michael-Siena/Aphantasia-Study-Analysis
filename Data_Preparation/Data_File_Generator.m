%% Prepare MATLAB environment
close all; clearvars; clc;  % closes all other matlab windows, clears all variables in the workspace, and clears the command window.
dirActive = matlab.desktop.editor.getActive; % get dir of open file
cd(fileparts(dirActive.Filename)); % assign active dir to cd
addpath(genpath('./Functions/')); % adds path for Functions directory

%% Initialise arena vars
arenaCenterX = 0;
arenaCenterY = 0;
arenaDiameter = 40;
arenaCircumfrence = arenaDiameter * pi;
arenaRadius = arenaCircumfrence /(2 * pi);

%% Initialise experiment vars
nSubjects = 120;
nSessions = 1;
nBlocks = 10;
nTrialsPerBlock = 8;
nTrials = nBlocks * nTrialsPerBlock; % total number of targets in a test session
sectors = 1;
nTrialsPerSect = nTrials / sectors;

%% Get stimuli names
source = '\MainTask';
%% FOR LOOP STUFF
tic;
for sub = 1:nSubjects
    targets = dir([cd source '\Targets\*.png']);
    targetNames = {targets(randperm(nTrials)).name}'; % shuffle indices
    targetNames = cellfun(@(t) t(1:end - 4), targetNames, 'UniformOutput', false); % strip file extension from target names
    orientationCues = dir([cd source '\OrientationCues\*.png']);
    orientationCueNames = {orientationCues(randperm(4 * nBlocks)).name}'; % shuffle indices
    orientationCueNames = cellfun(@(oc) oc(1:end - 4), orientationCueNames, 'UniformOutput', false); % strip file extension from names

    for sess = 1:nSessions
        %% Check if data already generated
        subDirectory = ['DataFiles1/sub' num2str(sub) '/'];
        if ~exist(subDirectory, 'dir')
            mkdir(subDirectory);
        else
            disp(['Subject: ' num2str(sub) '| MSG: Folder already exists - checking for conflicts with existing data files']);
            if exist([subDirectory 'sub' num2str(sub) '/*.csv'], 'file') || exist([subDirectory 'sub' num2str(sub) '/*.csv'], 'file')
                disp(['Subject: ' num2str(sub) '| Session: ' num2str(sess) ' | MSG: File already exists - terminating session!']);
                return;
            end
        end

        %% Generate orientation cues
        ocStartingPerspective = [repmat({1}, 6, 1); repmat({2}, 6, 1)];
        ocNReps = 3;
        ocNConds = 1;
        ocCondInds = 1;
        ocStartingPerspective = pseudorand(ocStartingPerspective, ocNReps, ocNConds, ocCondInds);
        ocBlock = 1:nBlocks;
        ocColNames = [{'North'}, {'South'}, {'East'}, {'West'}, {'InitialPerspective'}, {'Block'}, {'Sess'}, {'Sub'}];

        nOrientationCues = length(orientationCueNames);
        ocFirst = 1:4:nOrientationCues;
        ocSecond = 4:4:nOrientationCues;
        orientationCueData = cell(nBlocks, length(ocColNames));
        for b = 1:nBlocks
            tmpOrientationCues = orientationCueNames(ocFirst(b):ocSecond(b))';
            orientationCueData(b, :) = horzcat(tmpOrientationCues, ocStartingPerspective(b), {ocBlock(b)}, {sess}, {sub});
            orientationCueDataTable = cell2table(orientationCueData(b, :), 'VariableNames', ocColNames);
            subFileOrientationCue = ['sub' num2str(sub) '_block' num2str(b) '_orientationCues.csv'];
            writetable(orientationCueDataTable(:, 1:5), [subDirectory subFileOrientationCue]);
        end
        disp('GENERATED : orientation cue data');

        %% Generate study phase data
        % Initialise target coordinates
        targetCoordinates = cell(nTrials, 3);
        targetCoordinates(:, 2) = repmat({2.1}, nTrials, 1);
        
        % Study hue rotations
        studyHueRots = cell(nTrials, 1);
        
        % Test hue rotations
        testHueRots = cell(nTrials, 1);
        
        % Memory test order
        memOrderCond = repmat({1; 1; 1; 1; 2; 2; 2; 2}, nTrials / 8, 1);
        
        % Perspective conds
        perspCond = repmat({1; 1; 2; 2}, nTrials / 4, 1);

        % Switch/stay conditions
        swstCond = repmat({1; 2}, nTrials / 2, 1);

        % Append conditions to coordinate data
        trialDataConds = horzcat(targetNames, studyHueRots, targetCoordinates, memOrderCond, perspCond, swstCond);

        % Pseudorandomise trial order
        nReps = 3; % the same combination of conditions cannot be repeat more than three times
        if (sectors > 1)
            nIgnoredCols = 6; % we ignore first six cols because there is only one sector
        else
            nIgnoredCols = 5; % we subtract 4 from the condition  number because first four columns are the (x,z) coords, names, and sectors for the targets
        end
        nCols = size(trialDataConds, 2);
        nCondsUsedForPseudorand = nCols - nIgnoredCols;
        tmpCondInds = arrayfun(@(c) nIgnoredCols + c, 1:nCondsUsedForPseudorand, 'un', false);
        condInds = [tmpCondInds{:}];
        first = 1:nTrialsPerBlock:nTrials;
        second = nTrialsPerBlock:nTrialsPerBlock:nTrials;
        rng(0, 'twister');
        for b = 1:nBlocks
            % Generate target coordinates
            twodCoords = GenerateCoordinates(arenaCenterX, arenaCenterY, sectors, arenaRadius, nTrialsPerBlock); 
            trialDataConds(first(b):second(b), [3, 5]) = GenerateCoordinates(arenaCenterX, arenaCenterY, sectors, arenaRadius, nTrialsPerBlock); 
            trialDataConds(first(b):second(b), 2) = num2cell((360-0).*rand(8, 1) + 0);
            
            tmpStudyData = trialDataConds(first(b):second(b), :);
            pseudorandStudyData(first(b):second(b), :) = pseudorand(tmpStudyData, nReps, nCondsUsedForPseudorand, condInds);
        end

        % Block conditions
        tmpBlockCond = arrayfun(@(b) repmat(b, nTrials / nBlocks, 1), 1:nBlocks, 'un', false);
        blockCond = num2cell(vertcat(tmpBlockCond{:}));

        % Append block condition to rest of data
        studyData = horzcat(pseudorandStudyData, blockCond);
        finalStudyData = studyData(:, [1:5, 7, 9]);
        
        disp('GENERATED : study data');

        %% Generate test data from study data
        testPerspective = cell(nTrialsPerBlock, 1);
        testData = cell(nTrials, size(studyData, 2) + 2);
        first = 1:nTrialsPerBlock:nTrials;
        second = nTrialsPerBlock:nTrialsPerBlock:nTrials;
        for b = 1:nBlocks
            block = studyData(first(b):second(b), :);
            block = horzcat(block(:, [1:2]), num2cell((360-0).*rand(8, 1) + 0), block(:, [3:end]));

            for r = 1:nTrialsPerBlock
                % switch trial
                if (block{r, 9} == 1)
                    if (block{r, 8} == 1)
                        testPerspective{r} = 2;
                    elseif (block{r, 8} == 2)
                        testPerspective{r} = 1;
                    end
                % stay trial    
                elseif (block{r, 9} == 2)
                    testPerspective{r} = block{r, 8};
                end
            end
            blockAllConds = horzcat(block(:, 1:8), testPerspective, block(:, 9:end));
            nCondsUsedForPseudorand = 4;
            condInds = 7:10;
            testData(first(b):second(b), :) = pseudorand(blockAllConds, nReps, nCondsUsedForPseudorand, condInds);
        end

        disp('GENERATED : test data');

        %% Convert data cell arrays to tables and write to data files
        startIndices = 1:nTrials / nBlocks:nTrials;
        endIndices = nTrials / nBlocks:nTrials / nBlocks:nTrials;

        studyColumnNames = [{'TargetName'}, {'HueRotation'}, {'TargetX'}, {'TargetY'}, {'TargetZ'}, {'Perspective'}, {'Block'}]; 
        testColumnNames = [{'TargetName'}, {'StudiedHueRotation'}, {'ShownHueRotation'}, {'TargetX'}, {'TargetY'}, {'TargetZ'}, {'MemOrder'}, {'StudiedPerspective'}, {'TestPerspective'}, {'SwitchStatus'}, {'Block'}];

        studyDataTable = cell2table(finalStudyData, 'VariableNames', studyColumnNames);
        testDataTable = cell2table(testData, 'VariableNames', testColumnNames);
        
        for b = 1:nBlocks
            subFileStudy = ['sub' num2str(sub) '_block' num2str(b) '_study.csv'];
            subFileTest = ['sub' num2str(sub) '_block' num2str(b) '_test.csv'];
            writetable(studyDataTable(startIndices(b):endIndices(b), 1:end-1), [subDirectory subFileStudy]);
            writetable(testDataTable(startIndices(b):endIndices(b), 1:end - 1), [subDirectory subFileTest]);       
        end

        disp('WROTE : all data');
        disp(['SUBJECT : ' num2str(sub) ' | SESSION : ' num2str(sess) ' | TIME ELAPSED (SECS) : ' num2str(toc)]);
        disp('================================================================================================');
    end
end

disp('FINISHED');
