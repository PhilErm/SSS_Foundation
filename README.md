# Scripts for *Meeting wild-caught seafood demand at least cost to biodiversity*

## About
Produces the various analyses and figures associated with "Meeting wild-caught seafood demand at least cost to biodiversity".

## How to use
For the majority of analyses and figures, run 'mainAnalysis.R'. To ensure other scripts (e.g. 'parameters.R' and 'functions.R') can be called from 'mainAnalysis.R', you may need to configure your working directory.

For the analysis of real-world fisheries, run 'fisheriesAnalysis.R' after obtaining the data used with the script ('ProjectionData.csv'; obtainable seperately below) in the working directory.

Both scripts may also require you to download and install several of the packages they load.

## Obtaining the data used with 'fisheriesAnalysis.R'
The version of the fisheries database used can be obtained from https://datadryad.org/stash/dataset/doi:10.25349/D96G6H. The file required from the repository is 'ProjectionData.csv'.

To identify which fisheries meet the criteria outlined in the methods, a seperate set of instructions for acquiring the data used to do so is provided in the script itself.
