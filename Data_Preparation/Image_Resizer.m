%% PRESERVES AND RESIZES ALPHA CHANNEL FOR TRANSPARENCY
close all; clearvars; clc;  % closes all other matlab windows, clears all variables in the workspace, and clears the command window.
dirActive = matlab.desktop.editor.getActive; % get dir of open file
cd(fileparts(dirActive.Filename)); % assign active dir to cd

imageExt = input('Enter file extension: ', 's' );
newSize = str2num(input('Enter new size (pixels): ', 's'));

imagesPath = [cd '\*.' imageExt];

images = dir(imagesPath);
images = images(1:end);

for i = 1:numel(images)
    [img, ~, alpha] = imread(images(i).name);
    newImgDimensions = [newSize newSize];
    newImg = imresize(img, newImgDimensions);
    newAlpha = imresize(alpha, newImgDimensions);
    imwrite(newImg, images(i).name, 'Alpha', newAlpha);
end