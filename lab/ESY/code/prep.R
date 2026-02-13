###   R code of the Czech Expert system - Bruelheide, Jansen et al  2020    ###
###   The result was compared and optimised with the output JUICE program   ###
###   code written by H. Bruelheide and  F. Jansen                          ###
#
# For further explanations see Appendix S1.pdf in Tichy et al.: GRIMP. Journal of Vegetation Science.
# The expert system MUST remain fully functional in case of incomplete definition of formulas.
# In case, you will accept all ##D groups from section 2 and only those groups ###, which appear in the section 3 and they are not defined with the same name as ##D, the section 2 will be correctly translated. 
### ############################################################################ #
# Vocabulary  ####
# "species groups": sociological, ecological, functional groups of species which can replace each other in a vegetation plot
#                 named, usually by a member species of the group; second section of expert file; prefix ### for sociological, 
#                 #TC and #SC for functional species group, ##D for discriminating species group 
# see 'groups' and 'names(groups)'
# formal "vegetation type definitions", irritatingly called "group definitions", i.e the third part of the expert file
# e.g.: ((<#TC +01 A25a-Arctic-coastal-salt-marsh GR #T$> AND <##Q +01 A25a-Arctic-coastal-salt-marsh>) NOT <#TC Trees|#TC Shrubs GR 15>) AND <$$C Coast_EEA EQ ARC_COAST>
# see 'vegtype.formulas'
# they have a "type name", e.g. "A25a Arctic coastal salt marsh" , see 'vegtype.formula.names' and 'vegtaype.formula.names.short'
# and a priority, given as leading number, here "6", see 'vegtype.priority'
# they are composed of membership rules which are given within the symbols < >, see 'membership.expression'
# e.g. ### +05 Eutrophic-deciduous-forest-species GR ### +05 Acidophilous-forest-species"
# these are composed of expressions like "#TC Trees EXCEPT #TC Native-conifer-trees", see 'conditions'
# conditions are evaluated for all plots simultaneously and stored in matrix 'plot.cond'


## Prep ####
# load functions
## source('./code/ParsingExpertFile.R')


# load packages
library(vegdata)
library(stringr)
library(stringi)
library(data.table)
library(fastmatch)
match <- fmatch
# library(dplyr)
# library(readr)

# Speed up the calculation by using parallel computing
library(parallel) 
detectCores()
cl <- makeCluster(detectCores()-1)
library(foreach)
library(doParallel)
registerDoParallel(cl)

# Profiling code
library(profvis)
# library(profmem)

### Functions ####
# returns string w/o trailing whitespace, minus or numerical
trim.trailing <- function (x) sub("\\s+$|\\s+\\d$|\\s+\\-\\s+\\d$", "", x)
# returns string w/o leading whitespace
trim.leading <- function (x)  sub("^\\s+", "", x)
# returns string w/o leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

total.cover <- function(x) round((1 - prod(1 - x/100)) * 100,10)

fw <- function(i, p=1) {
  plot.cond[is.na(plot.cond)] <- 0 # important because NA will ruin the foreach order but appear because of non-solvable conditions
  plot.cond[plot.cond == -Inf] <- 0
  write.csv2(plot.cond[p, plot.cond[p, ] != 0], paste('plot.group', i, 'csv', sep='.'))
}

# Functions to evaluate output
eval.EUNIS <- function(p, t, cond=FALSE) {
  if(is.numeric(p)) {
    n <- p
    p <- as.character(header$RELEVE_NR[n])
  } else  n <- which(header$RELEVE_NR == p)
#  sink('eva.out', split = TRUE)
  cat('Plant observations in', p, ':\n')
  print(obs[obs$RELEVE_NR == p])
  if(exists('logi2')) {
      types <-  which(sapply(logi2, function(x) x[n]))
      cat('Possible types of plot"', p, '" (', n, '):', names(types), '\n')
      cat('Priorities of these types:', vegtype.priority[match(names(types), vegtype.formula.names.short)], '\n')
  }
  if('ESY 2019-10-28' %in% names(header))
    cat('\nOfficially classified as:', vegtype.formula.names[match(header$`ESY 2019-10-28`[n], vegtype.formula.names.short)], '\n')
  if(!missing(t)) {
    if(is.character(t)) t <- match(t, vegtype.formula.names.short)
    if(is.na(t)) cat('Type not defined.') else {
      cat(vegtype.formula.names[t], '\n\n', vegtype.formulas[t], '\n\n', vegtype.formulas.p[t], '\n\n')
      col <- unique(as.numeric(str_extract_all(vegtype.formulas.p[t], "(?<=col\\s{0,1})[-0-9.]+")[[1]]))
      colcond <- unique(as.numeric(unlist(str_extract_all(names(sapply(logi1, '[', n)[sapply(logi1, '[', n) > 0]), "(?<=col\\s{0,1})[-0-9.]+"))))
      print(data.frame(row.names = col, 'expressions for this type' = membership.expressions[col], result=col %in% colcond), right=FALSE)
    }
  }
  if(cond) {
    cat('\nPlot x conditions matrix:\n')
    v <- plot.cond[n, plot.cond[n, ] != 0]
    con <- unique(as.numeric(unlist(str_extract_all(names(plot.cond[n, plot.cond[n, ] != 0]), "(?<=col\\s{0,1})[-0-9.]+"))))
    print(data.frame(row.names = con, value=t(v), conditions=conditions[plot.cond[n, ] != 0]), right = FALSE)
    }
}

eval.type <- function(t) {
    if(is.character(t)) t <- match(t, vegtype.formula.names.short)
    if(is.na(t)) cat('Type not defined.') else {
      cat(vegtype.formula.names[t], '\n\n', vegtype.formulas[t], '\n\n', vegtype.formulas.p[t], '\n\n')
      col <- unique(as.numeric(str_extract_all(vegtype.formulas.p[t], "(?<=col\\s{0,1})[-0-9.]+")[[1]]))
      print(data.frame(row.names = col, expressions = membership.expressions[col]), right=FALSE)
    }
}


##########################
# Snippets
#
# Input
# if(DB == 'sPlot')
# # 1. Reading 1M sPlot data:
#  source('../ReadPlotDataFromJUICE.R') else
# # # 2. Read from Turboveg databases, database name DB given above
#  source('readTV.R')

