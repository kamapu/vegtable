###   Parsing membership formulas    ###

parse.classification.expert.file <- function(expertfile) {
################################### #
### Load the expert system file  ####
################################### #
expert  <- read.csv(expertfile, header = FALSE,
                     encoding="UTF-8", stringsAsFactors = FALSE, sep="\t")[,1]
# str(expert) # chr [1:31518]
expert <- expert[!grepl('---', expert)]
########################### #
### Species aggregation  ####
########################### #
section1 <- grep('SECTION 1', expert)
species.agg <- expert[(section1[1]+1) : (section1[2]-1)]
index.agg.names <- which(substr(species.agg,1,1)!=" ")
number.agg <- length(index.agg.names) # 316
ind.agg.names <- c(index.agg.names, length(species.agg)+1)
aggs <- lapply(X = 1:number.agg, function(x) trim.leading(species.agg[(ind.agg.names[x] + 1) : (ind.agg.names[x+1] - 1)]) )
names(aggs) <- trim.trailing(species.agg[index.agg.names])

# AGG <- vector('list', length(aggs))
# names(AGG) <- names(aggs)
for(i in 1:length(aggs))
  aggs[[i]] <- sapply(aggs[[i]], function(x) trim.trailing(x), USE.NAMES = FALSE)

###################### #
### Species groups  ####
###################### #
section2 <- grep('SECTION 2', expert)
species.groups <- expert[(section2[1]+1) : (section2[2]-1)]
index.group.names <- which(substr(species.groups,1,1)!=" ")
number.groups <- length(index.group.names) # 316
species.groups[index.group.names]

gr <- c(index.group.names, length(species.groups))
groups <- lapply(X = 1:number.groups, function(x) 
  trim.leading(species.groups[(gr[x] + 1) : (gr[x+1]-1)] ) )
names(groups) <- species.groups[index.group.names]

if(!all(substr(names(groups),1,3) %in% c('###', '##D', '$$C', '$$N'))) stop(
  paste('Only "###", "##D", and "$$C" are known as (species) group classifiers, I found ', substr(names(groups),1,3)[!substr(names(groups),1,3) %in% c('###', '##D')]))

discr <- substr(names(groups)[startsWith(names(groups), '+')], 2, 3)
if(any(table(discr) < 2)) stop(paste('Discriminating set', names(table(discr)[table(discr)<2]), 'occur only once!'))
table(discr)

####################################################################### #
###  Step 1: Parse membership formulas into membership expressions   ####
####################################################################### #
# COULD BE LAPPLYfied like the above
section3 <- grep('SECTION 3', expert)
group.definitions <-  expert[(section3[1]+1) : (section3[2]-1)]  # formerly called 'expert3'
# length(group.definitions) # approx. 600

# holds formula names and formulas.
# I refer to the whole line as a formula (=sentence).
# Everything in a bracket (<...>) is an expression, which has to be T or F.
# Every expression can have one to many conditions, which are also T or F. 
# (Actually the conditions are expressions themselves, but inner expressions.)
# Expressions consist of a left- and right-hand condition (NOT), which are compared
# by a logical operator (GR, GE, EQ).
# The membership expressions contain CR (carriage return characters), thus
# the condition may be distributed across more than one line
membership.formula.names <- NULL
# Name of the formula
membership.expressions <- NULL
# inner expressions, between < and >
membership.formulas <- NULL
# whole expressions
i <- 0
# whole expressions
i <- 0
while (i < length(group.definitions)){
  i <- i + 1
  if (substr(group.definitions[i],1,3)!="---"){
    # these conditions are out-commented and not used
    if (!grepl("[^0-9]", substr(group.definitions[i],1,1))){
      # checks whether line is a formula name
      membership.formula.names <- c(membership.formula.names,group.definitions[i])
    } else {
      # then the line is a formula or an out-commented formula
      c <- group.definitions[i]
      while (grepl("[^0-9]", substr(group.definitions[i+1],1,1)) & substr(group.definitions[i+1],1,1)!="-" &
             i < length(group.definitions)){
        # checks whether the next line is still the current formula
        # if not, two lines are pasted together
        # !grepl("[^0-9]", x)  checks whether the first character of the next line is numeric
        # substr(group.definitions[i+1],1,1)!="-" checks whether the first character of the next line is an out-commented line
        i <- i +1
        c <- paste(c, group.definitions[i],sep=" ")
      }
      a <- gregexpr("<",c, fixed=T)[[1]]
      b <- gregexpr(">",c, fixed=T)[[1]]
      if (a[1]>0) {
        membership.formulas <- c(membership.formulas,c)
        membership.expressions2 <- array("",length(a))
        for (j in 1: length(a)){
          membership.expressions2[j] <- substr(c,a[j]+1,b[j]-1)
        }
        membership.expressions <- c(membership.expressions,membership.expressions2)
      }
    }
  }
}
# finished expert system decoding 1 ####

#! We need to check the expert system file
#! xml would be much better fitted for this

if(any(grepl('[', membership.formulas, fixed = TRUE))) stop('Nested bracket "[]" is not implemented in this R code, only "()" is allowed.')
if(any(grepl('{', membership.formulas, fixed = TRUE))) stop('Nested bracket "{}" is not implemented in this R code, only "()" is allowed.')

# Check if all terms are defined
term <- sapply(membership.expressions, function(x) if(startsWith(x, '#') | startsWith(x, '$')) substr(x, 5, nchar(x)) else x, USE.NAMES = FALSE)
term <- unique(sapply(term , function(x) unlist(strsplit(x, ' GR', fixed = TRUE))[1], USE.NAMES = FALSE))
term <- unique(sapply(term , function(x) unlist(strsplit(x, ' GE ', fixed = TRUE))[1], USE.NAMES = FALSE))
term <- unique(sapply(term , function(x) unlist(strsplit(x, ' EQ ', fixed = TRUE))[1], USE.NAMES = FALSE))
term <- unique(sapply(term , function(x) unlist(strsplit(x, '|', fixed = TRUE))[1], USE.NAMES = FALSE))

gr <- c(substr(names(groups), 5, nchar(names(groups))), 'GE 30')
# if(!all(term %in% gr)) {
#   message(paste('The following term is missing in SECTION 2: ', unique(term[!term %in% gr]), '\n'))
# }
if(any(duplicated(gr))) stop(paste('Duplicated group name found', gr[duplicated(gr)]))

# unique(word(membership.expressions))
# unique(word(membership.expressions))[substr(unique(word(membership.expressions)),1,1) == '#']


# There are some formulas that are based on header data.
# There are two group types: "$$C" (character) and "$$N" (numeric). 
# "$$C" is combined with the operator "EQ", 
# "$$N" is combined with all three operators "GR", "GE", "EQ". 

# The name of the group (e.g. "$$C COAST_EEA") is the name of column in the header data table. 
# there are five "##C" fields used in the EUNIS data:  
# $$C COAST_EEA # $$C Country # $$C Dataset # $$C DUNES_BOHN # $$C ECOREG_WWF
# there are three "##N" fields used in the code: 
# $$N Altitude (m) # $$N DEG_LAT # $$N DEG_LON

############################################################################################################ #
### Step 2: Add right-hand sides of membership expressions where there are no right-hand side conditions. ####
############################################################################################################ #
# Before the expressions can be split at the logical operators GR, GE or EQ
# into a left-hand side and right-hand side condition, some right hand-sides
# have to be complemented to ease coding.
# For example, #T$ occurs without group name on the right hand side and means 
# total cover except the species on the left-hand side. 
# Similarly, $05, $25 or $50 on the right-hand side of expressions
# mean that The total cover of a functional species group on the left-hand side is greater/equal 
# than 5%, greater than or equal to 25%, equal to 50% of the total cover of the plot. 
# In these cases, the group name after #T$, $05 etc. on the right-hand side is inserted.

# the only logical operator occurring in #T$ groups in the EUNIS system is GR
# Note, that it has to be checked whether this is also the case in other expert systems

# check whether "#T$" is actually on the right hand side, GR
index3 <- which(regexpr("#T$", membership.expressions, fixed=T)>0 & 
                  trim(tstrsplit(membership.expressions,"GR", fixed=T)[[2]])=="#T$")

# not all of them are unique
b <- unique(membership.expressions[index3])
a <- tstrsplit(b,"GR", fixed=T)
a[[1]] <- trim(a[[1]])

for (i in 1:length(b)){
  index4 <- which(regexpr(b[i], membership.formulas, fixed=T)>0)
  # to handle the left-hand side expression <#TC Dry-and-wet-heath-shrubs|#TC Dry-heath-shrubs GR #T$>
  # "#T$" has also to be inserted inside the string
  a[[1]][i] <- gsub("#TC","#T$",a[[1]][i], fixed=T)
  membership.formulas[index4] <- gsub(b[i],
                paste(b[i], substr(a[[1]][i],4,nchar(a[[1]][i])), sep=""),
                membership.formulas[index4],fixed=T)
}

# now also change this in the membership expressions
a <- tstrsplit(membership.expressions[index3],"GR", fixed=T)
a[[1]] <- trim(a[[1]])
# to handle the left-hand side expression <#TC Dry-and-wet-heath-shrubs|#TC Dry-heath-shrubs GR #T$>
# "#T$" has also to be inserted inside the string
a[[1]] <- gsub("#TC","#T$",a[[1]], fixed=T)

membership.expressions[index3] <- paste(membership.expressions[index3],
                                        substr(a[[1]],4,nchar(a[[1]])),sep="")

# check whether "#T$" is actually on the right hand side, GE
index3 <- which(regexpr("#T$", membership.expressions, fixed=T)>0 & 
                  trim(tstrsplit(membership.expressions,"GE", fixed=T)[[2]])=="#T$")

# not all of them are unique
b <- unique(membership.expressions[index3])
a <- tstrsplit(b,"GE", fixed=T)
a[[1]] <- trim(a[[1]])

for (i in 1:length(b)){
  index4 <- which(regexpr(b[i], membership.formulas, fixed=T)>0)
  # to handle the left-hand side expression <#TC Dry-and-wet-heath-shrubs|#TC Dry-heath-shrubs GR #T$>
  # "#T$" has also to be inserted inside the string
  a[[1]][i] <- gsub("#TC","#T$",a[[1]][i], fixed=T)
  membership.formulas[index4] <- gsub(b[i],
                                      paste(b[i], substr(a[[1]][i],4,nchar(a[[1]][i])), sep=""),
                                      membership.formulas[index4],fixed=T)
}

# now also change this in the membership expressions
a <- tstrsplit(membership.expressions[index3],"GE", fixed=T)
a[[1]] <- trim(a[[1]])
# to handle the left-hand side expression <#TC Dry-and-wet-heath-shrubs|#TC Dry-heath-shrubs GR #T$>
# "#T$" has also to be inserted inside the string
a[[1]] <- gsub("#TC","#T$",a[[1]], fixed=T)

membership.expressions[index3] <- paste(membership.expressions[index3],
                                        substr(a[[1]],4,nchar(a[[1]])),sep="")

# we identify all conditions without a right-hand side
# <##D Diagnostic species group>	
# The number of species of the diagnostic species group is greater than 
# the number of species of any other diagnostic species group defined in section 2. 
# <##C Diagnostic species group>	
# The total cover of the diagnostic species group is greater than the
# total cover of any other diagnostic species group defined in section 2.
# <##Q Diagnostic species group>	
# The sum of the square root cover of the diagnostic species group is greater 
# than the square root cover of any other diagnostic species group defined in section 2.

# these conditions are replaced with "GR NON", meaning that either number, cover or 
# sum of squared cover is greater than on the left-hand side
is.not.right.hand.side <- regexpr("GR",membership.expressions, fixed=T)==-1 &
  regexpr("GE",membership.expressions, fixed=T)==-1 &
  regexpr("EQ",membership.expressions, fixed=T)==-1

# We take only those that do not have numeric condition (i.e. #01, #02 etc.)
index9 <- suppressWarnings(as.numeric(substr(membership.expressions[is.not.right.hand.side],3,3)))

a <- membership.expressions[is.not.right.hand.side][is.na(index9)]
any(duplicated(a)) #T
# duplicated membership expressions result in inserting "GR NON" in the loop below more than once, thus duplicates are removed
a <- unique(a)

for (i in 1:length(a)){
  index4 <- which(regexpr(a[i], membership.formulas, fixed=T)>0)
  membership.formulas[index4] <- gsub(a[i],  paste(a[i], "GR NON",a[i], sep=" "),
  membership.formulas[index4], fixed=T)
}

membership.expressions[is.not.right.hand.side][is.na(index9)] <- paste(membership.expressions[is.not.right.hand.side][is.na(index9)], "GR NON", membership.expressions[is.not.right.hand.side][is.na(index9)], sep=" ")
index9 <- regexpr("GR",membership.expressions, fixed=T)==-1 &
  regexpr("GE",membership.expressions, fixed=T)==-1 &
  regexpr("EQ",membership.expressions, fixed=T)==-1
membership.expressions[index9]
# there are 142 expressions without a right-hand side condition
# but these are all numeric expressions

'########################################################################################## #
### Step 3A: Add left- and right-hand side variables where "|" has been used in an expression    ####
########################################################################################## #
# Before the expressions can be split at the logical operators GR, GE or EQ
# into a left-hand side and right-hand side condition, some left and right hand-sides
# have to be complemented to ease coding.
membership.expressions[grep("\\|",membership.expressions)]
# 15 cases, please note that the "|" can occur on both sides
membership.expressions[grep("\\&",membership.expressions)]
#
# This means that "|" occurs inside expressions on the left hand side 
# "#TC Trees|#TC Shrubs GR 15"      
# and means #TC Trees GR 15 | #TC Shrubs GR 15
# In this case, the expression is split in two parts

membership.expressions2 <- NULL
# collects the new membership expressions

index3 <- grep("\\|",membership.expressions)

# not all of them maybe unique
a <- unique(membership.expressions[index3])

# First, it is identified whether the "|" occurs at the left- or right hand-side
# Thus, we split the membership expressions at GR, GE, EQ
b <- tstrsplit(a,"GR|GE|EQ", fixed=F)
b[[1]] <- trim(b[[1]])
b[[2]] <- trim(b[[2]])

# it may be that some conditions have had a "|" on both sides
# We check this first
# finally the result is assigned to the left hand-side (b[[1]])
index5 <- which(regexpr("|",b[[1]], fixed=T)>0 & regexpr("|",b[[2]], fixed=T)>0)
#b[[1]][index5]
#b[[2]][index5]

for(i in 1:length(index5)){
  c <- unlist(strsplit(b[[1]][index5[i]],"|", fixed=T))
  d <- unlist(strsplit(b[[2]][index5[i]],"|", fixed=T))
  # combine them pairwise
  if(regexpr("GR",a[index5[i]], fixed=T)>0){
    # then GR has to be inserted
    e1 <- paste(c," GR ",d, sep="")
    e2 <- paste(c," GR ",d[c(2,1)], sep="")
  } else {
    if(regexpr("GE",a[index5[i]], fixed=T)>0){
      # then GE has to be inserted
      e1 <- paste(c," GE ",d, sep="")
      e2 <- paste(c," GE ",d[c(2,1)], sep="")
    } else {
      if(regexpr("EQ",a[index5[i]], fixed=T)>0){
        # then EQ has to be inserted
        e1 <- paste(c," GE ",d, sep="")
        e2 <- paste(c," GE ",d[c(2,1)], sep="")
      } 
    }
  }
  membership.expressions2 <- c(membership.expressions2,e1,e2)
  e1 <- paste("<",e1,">", sep="")
  e2 <- paste("<",e2,">", sep="")
  e3 <- paste(c(e1,e2), collapse = " OR ")
  b[[1]][index5[i]] <- e3
  # assign the result to the left hand-side
  b[[2]][index5[i]] <- ""
  # delete right hand side, as this has been solved
}


#b[[1]] is left hand-side
index4 <- which(regexpr("|",b[[1]], fixed=T)>0)
for(i in 1:length(index4)){
  c <- unlist(strsplit(b[[1]][index4[i]],"|", fixed=T))
  if(regexpr("GR",a[index4[i]], fixed=T)>0){
    # then GR has to be inserted
    d <- paste(c," GR ",b[[2]][index4[i]], sep="")
  } else {
    if(regexpr("GE",a[index4[i]], fixed=T)>0){
      # then GE has to be inserted
      d <- paste(c," GE ",b[[2]][index4[i]], sep="")
    } else {
      if(regexpr("EQ",a[index4[i]], fixed=T)>0){
        # then EQ has to be inserted
        d <- paste(c," EQ ",b[[2]][index4[i]], sep="")
      } 
    }
  }
  membership.expressions2 <- c(membership.expressions2,d)
  d <- paste("<",d,">", sep="")
  d <- paste(d, collapse = " OR ")
  d <- paste("(",d,")", sep="")
  b[[1]][index4[i]] <- d  
  # assign the result to the left hand-side
  b[[2]][index4[i]] <- ""
  # delete the right hand-side
}
b[[1]]

index6 <- which(regexpr("|",b[[2]], fixed=T)>0)
#b[[2]] is left hand-side
for(i in 1:length(index6)){
  c <- unlist(strsplit(b[[2]][index6[i]],"|", fixed=T))
  if(regexpr("GR",a[index6[i]], fixed=T)>0){
    # then GR has to be inserted
    d <- paste(b[[1]][index6[i]]," GR ",c, sep="")
  } else {
    if(regexpr("GE",a[index6[i]], fixed=T)>0){
      # then GE has to be inserted
      d <- paste(b[[1]][index6[i]]," GE ",c, sep="")
    } else {
      if(regexpr("EQ",a[index6[i]], fixed=T)>0){
        # then EQ has to be inserted
        d <- paste(b[[1]][index6[i]]," EQ ",c, sep="")
      } 
    }
  }
  membership.expressions2 <- c(membership.expressions2,d)
  d <- paste("<",d,">", sep="")
  d <- paste(d, collapse = " OR ")
  d <- paste("(",d,")", sep="")
  b[[1]][index6[i]] <- d  
  # assign the result to the left hand-side
  b[[2]][index6[i]] <- ""
  # delete the right hand-side
}
#b[[1]]
#b[[2]]

c <- paste("<",a,">", sep="")
for (i in 1:length(a)){
  index4 <- which(regexpr(c[i], membership.formulas, fixed=T)>0)
  membership.formulas[index4] <- gsub(c[i],b[[1]][i],
                membership.formulas[index4],fixed=T)
}

# now also change this in the membership expressions
# c <- strsplit(b[[1]]," OR ", fixed=T)
membership.expressions <- membership.expressions[-index3] 
# remove the old membership expressions

# the new membership expressions have to be added
membership.expressions <- c(membership.expressions,membership.expressions2)
# HB end
'
# HB
########################################################################################## #
### Step 3B: Add EXCEPT on right-hand sides of #SC conditions                           ####
########################################################################################## #

index3 <- grep("#SC",membership.expressions)
a <- unique(membership.expressions[index3])

# First, it is identified whether the "|" occurs at the left- or right hand-side
# Thus, we split the membership expressions at GR, GE, EQ
b <- tstrsplit(a,"GR|GE|EQ", fixed=F)
b[[1]] <- trim(b[[1]])
b[[2]] <- trim(b[[2]])
index4 <- grep("#SC", b[[2]])
# index4 shows which right-hand side conditions have to be complemented by the left-hand side

# add "EXCEPT and the species name from the left-hand side on the right-hand side
for (i in 1:length(index4)){
  index6 <- grep(a[index4[i]], membership.expressions)
  membership.expressions[index6] <- paste(membership.expressions[index6],"EXCEPT",b[[1]][index4[i]],sep=" ")
  index5 <- grep(a[index4[i]], membership.formulas)
  membership.formulas[index5] <- gsub(a[index4[i]],
                                      paste(a[index4[i]],"EXCEPT",b[[1]][index4[i]],sep=" "),
                                      membership.formulas[index5],fixed=T)
}

###################################################################### #
membership.conditions2 <- unlist(strsplit(membership.expressions, "GR"))
membership.conditions2 <- unlist(strsplit(membership.conditions2, "GE"))
membership.conditions2 <- unlist(strsplit(membership.conditions2, "EQ"))
membership.conditions2 <- trim(membership.conditions2)
membership.conditions2 <- sort(unique(membership.conditions2))
#remove numerical elements
membership.conditions2 <- suppressWarnings(membership.conditions2[-which(!is.na(as.numeric(membership.conditions2)))])
length(membership.conditions2)

# finished expert system decoding ####

prio <- substr(membership.formula.names, 1, 1)
p <- factor(prio, ordered = TRUE, levels = c(0:9, LETTERS, letters), exclude = c(0:9, LETTERS, letters)[!c(0:9, LETTERS, letters) %in% prio])

names(membership.formulas) <- membership.formula.names
out <- list(species.aggs = aggs, species.groups = groups, membership.expressions = membership.expressions, group.defs = membership.conditions2, formulas = membership.formulas, membership.priority = p)

return(out)
}



# listToXML <- function(node, sublist){
#   for(i in 1:length(sublist)){
#     child <- newXMLNode(names(sublist)[i], parent=node);
#     if (typeof(sublist[[i]]) == "list"){
#       listToXML(child, sublist[[i]])
#     }
#     else{
#       xmlValue(child) <- sublist[[i]]
#     }
#   } 
# }
# root <- newXMLNode("expertlist")
# li <- list(species.groups = result$species.groups, group.def=result$group.defs, formulas=result$formulas)
# li <- list(species.groups = result$species.groups)
# listToXML(root, li)
# 
# write_xml(root, 'expertlist.xml', options = 'format')
# 
