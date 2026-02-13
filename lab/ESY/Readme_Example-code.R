### Example code for the Expert system for the assignment of vegetation plots to vegetation types
source('lab/ESY/code/ParsingExpertFile.R')
source('lab/ESY/code/prep.R') #### Loading packages

mc <- getOption("mc.cores", 1) # number of computer cores to use if you are on a non-Windows OS
expertfile <- "EUNIS-ESy-2020-06-08.txt" # latest version of the EUNIS system, used in Chytrý et al. 2020, AVS
source('code/step1and2_load-and-parse-the-expert-file.R')

obs <- fread(file.path('data', 'obs_100716Hoppe2005.csv'), encoding = 'UTF-8')
header <- read.csv(file.path('data', 'header_100716Hoppe2005.csv'))

plot.cond <- array(0, c(length(unique(obs$RELEVE_NR)), length(conditions)), 
dimnames = list(as.character(unique(obs$RELEVE_NR)), conditions))
source('code/step4_aggregate-taxon-levels.R')

source('code/step3and5_extract-and-solve-membership-conditions.R')

table(result.classification)


eval.type('A25c')
eval.EUNIS(which(result.classification == 'A25c')[1], 'A25c')

