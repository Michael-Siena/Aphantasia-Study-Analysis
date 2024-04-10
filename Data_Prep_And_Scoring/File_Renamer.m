%% Clear workspace
close all; clearvars; clc; % closes all other matlab windows, clears all variables in the workspace, and clears the command window.
dirActive = matlab.desktop.editor.getActive; % get active dir
cd(fileparts(dirActive.Filename)); % set cd to active dir

%% Input desired file extension and name
fileExt = input('Enter file extension: ', 's' );
newName = input('Enter new name: ', 's');

%% Rename files
filesPath = [cd '\*.' fileExt];

files = dir(filesPath);
files = files(1:end);

for f = 1:numel(files)
    [~, name, ext] = fileparts(files(f).name);    
    replacementName = [newName num2str(f) ext]; 
    movefile(files(f).name, replacementName); 
end