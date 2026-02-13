# Expert system for the assignment of vegetation plots to vegetation types

This R code applies the EUNIS expert vegetation classification system (Tichy et al. 2019, Chytry et al. 2020) effectively to vegetation data in long table format.

The core of the expert system are logical assignment rules which decide whether a particular plot is assigned to a particular type, in this case a EUNIS habitat type. Each assignment rule is a logical membership formula, for instance the formula for the EUNIS type "B16a Atlantic and Baltic coastal dune scrub":
<pre>
  (<#TC Shrubs GR 25> NOT (<#TC Trees GR 25> OR <#TC Native-light-canopy-trees GR 15>)) AND (<$$C Dunes_Bohn EQ Y> AND (<$$C Coast_EEA EQ ATL> OR <$$C Coast_EEA EQ BAL>))
</pre>

Each membership formula consists of several logical membership expressions (e.g. <#TC Shrubs GR 25>), 
which start and end with angle brackets ("<", ">") and are combined by formal logic.
Each logical membership expression can be evaluated to TRUE or FALSE. In consequence, their 
combination by logical operators "AND", "OR" and "NOT" also result in "TRUE" or "FALSE".
Each logical expression has a left-hand (positiv) and (in most cases) also a right-hand (negative, "and not") condition. They are called membership conditions and can be evaluated resulting in a numeric value (number of species, sum of cover etc). The left-and right-hand membership conditions are compared using the logical operators "GR" (greater), "GE" (greater or equal) and "EQ" (equal).

The following code extracts all membership formulas (Step 1), all membership expressions from membership formulas (Step 2), and all membership conditions from membership expressions (Step 4).
Then these logical comparisons are evaluated in reverse order, i.e. from membership conditions to
expressions to formulas. In other words, the assignment rules are resolved from inside to outside.

Main differences to the JUICE implementation are
... making use of logical matrices, different to the sequential approach,
... allowing vector-oriented evaluation, which is (potentially) much quicker,
... can be generalised for any data and expert system.

# Versions
- Version 1.1 (2023-05-26)
 There was a bug in step4_aggregate-taxon-levels.R by cutting the trailing sign twice.


# Functions
- Read and prepare:
    - prep.R: load packages, define helper functions
    - ReadPlotDataFromJUICE_2020-05-02.R: read data from JUICE, in this case the EVA dataset from Chytry et al. 2020 (not included),
    - ReadTestdata.R read and adapt Turboveg2 data, in this case Tuexen-Archiv from vegetweb.de. Both ways can be adapted to use your own data. 
    For the main part of the code see main.R for the equivalence to Chytry et al 2020, or the following paragraph for a smaller freely available example dataset.
- Parse expert file (step1and2)
- Adapt taxonomy (step4)
- Calculate memberships (step5)
- Evaluate assignments (eval.EUNIS(), eval.type() in prep.R)


# Example

Open the file "Readme_Example-code.R" in R and execute the code lines.
The code applies the EUNIS expert vegetation classification system (Tichy et al. 2019, Chytry et al. 2020) 
to vegetation data in long table format (Releve ID, Taxon name, Cover).

The STEPs mentioned in the following comments refer to the methods in Bruelheide et al. 2020 (Fig. 1)

```{R}
source('code/prep.R') #### Loading packages
mc <- getOption("mc.cores", 1) # number of computer cores to use if you are on a non-Windows OS

### define expert file:
expertfile <- "EUNIS-ESy-2020-06-08.txt" # latest version of the EUNIS system, used in Chytrý et al. 2020, AVS

### Start  ######################
### Read and parse the expert file
#################################################################### #
### Step 1: Parse membership formulas into membership expressions ####
### Step 2: Add right-hand sides of membership expressions        ####
###         where there are no right-hand side conditions
#################################################################### #
source('code/step1and2_load-and-parse-the-expert-file.R')

######################################################## #
### Input 2                                           ####
### To exemplify the usage of the code we use freely available data from vegetweb.de
### The so called Tuexen-Archiv (https://www.vegetweb.de/home#!quellendetails//1933) contains 10717 vegetation plots
### of different types
### Hoppe, A. (2005) Das Reinhold-Tüxen-Archiv am Institut für Geobotanik der Universität Hannover, Tuexenia, 25, 463–474
### Read vegetation data file into a long table with class data.table and with columns named
### "RELEVE_NR", "TaxonName", "Cover_Perc"
### If you want to use your own data, please make sure to have the three columns.
### If you want to import vegetation data from Turboveg databases see next lines and 'ReadTestdata.R'
### # library(vegdata)
### # options(tv.refl='GermanSL 1.5')
### # db <- "100716 Hoppe (2005)"  # you can download the dataset and load the xml file directly in R
### # or after importing it to Turboveg 2.0
### # obs <-tv.coverperc(db, obs)   # Convert cover codes to cover percentages
### # obs$TaxonName <- vegdata::tax(obs$TaxonUsageID)$TaxonName
### # obs <- data.table(obs) # it needs to be a data.table for speed
### # head(obs)  
### # We need three columns: 'RELEVE_NR' for Plot identifier, 'TaxonName' for taxon names, and 'Cover_Perc' for percentage cover
### # str(obs)
### # Classes ‘data.table’ and 'data.frame':    155039 obs. of  3 variables:
### #  $ RELEVE_NR : int  1 1 2 2 3 3 4 4 5 5 ...
### # $ TaxonName : chr  "Lemna gibba" "Lemna minor" "Lemna gibba" "Lemna minor" ...
### # $ Cover_Perc: num  88 13 68 13 38 68 3 88 3 88 ...
### # - attr(*, ".internal.selfref")=<externalptr>

'  RELEVE_NR   TaxonName Cover_Perc  
1:         1 Lemna gibba         88
2:         1 Lemna minor         13
3:         2 Lemna gibba         68
4:         2 Lemna minor         13
5:         3 Lemna gibba         38
6:         3 Lemna minor         68
'

obs <- fread(file.path('data', 'obs_100716Hoppe2005.csv'))

### Additionally we use header data (plot attributes). For the EUNIS classification expert file 
### it should contain the columns
### "RELEVE_NR", "Altitude (m)",  "Latitude", "Longitude", "Country", "Coast_EEA", 'Dunes_Bohn', "Ecoreg", "dataset"
### which are used in the EVA dataset in Chytry et al. 2020
### The EUNIS expert classification expects besides plot locations 4 additional header fields for exact assignment, 
### these are "dataset" "Ecoreg" "Dunes_Bohn" "Coast_EEA"
### Whereas dataset is only used for one EUNIS type (H21), the others can be calculated by pruning plot locations
### with https://storage.googleapis.com/teow2016/Ecoregions2017.zip
### with shapes in data/DUNES_BOHN.zip and a 10km buffer along the coast
### Ecoreg refers to numbers of the 846 WWF ecoregions,
### see e.g. Eco ID when you click at https://ecoregions2017.appspot.com
### Dunes_Bohn can take the values "Y_DUNES" and "N_DUNES" for yes and no
### COAST_EEA can take values ARC_COAST ATL_COAST BAL_COAST BLA_COAST MED_COAST   N_COAST
### see also ReadTestData.r and the Appendix of Chytry et al. (2020)

header <- read.csv(file.path('data', 'header_100716Hoppe2005.csv'))

### # head(header)
### RELEVE_NR Country Altitude..m. DEG_LON DEG_LAT       GESELLSCH            dataset Ecoreg Dunes_Bohn Coast_EEA
###         1 Germany            1    9.26   53.90 Lemnetum gibbae Germany Vegetweb 2    664    N_Dunes   N_COAST
###         2 Germany            1    7.10   53.50 Lemnetum gibbae Germany Vegetweb 2    664    N_Dunes   N_COAST
###         3 Germany            1    9.40   53.84 Lemnetum gibbae Germany Vegetweb 2    664    N_Dunes   N_COAST
###         4 Germany            1    7.34   53.33 Lemnetum gibbae Germany Vegetweb 2    664    N_Dunes   N_COAST
###         5 Germany            1    8.50   53.91 Lemnetum gibbae Germany Vegetweb 2    664    N_Dunes ATL_COAST
###         6 Germany            1   10.51   52.21 Lemnetum gibbae Germany Vegetweb 2    654    N_Dunes   N_COAST

# ################################################################### #
# # Step 3: Create a numerical plot x membership condition matrix  ####
# ################################################################### #
# Create an Array to collect all membership conditions 
# of the left- and right-hand side
plot.cond <- array(0, c(length(unique(obs$RELEVE_NR)), length(conditions)), 
                   dimnames = list(as.character(unique(obs$RELEVE_NR)), conditions))

###################################### #
### Step 4: Aggregate taxon levels  ####
###################################### #
source('code/step4_aggregate-taxon-levels.R')

# ############################################################## #
### Step 5: Solve the membership conditions                   ####
### and fill in the numerical plot x membership condition matrix
# ############################################################## #
source('code/step3and5_extract-and-solve-membership-conditions.R')

# ####################### #
### Evaluate results   ####
table(result.classification)
' result.classification
   ?    + A25c    C C11b C12b  C14  C15 C21a C21b C22a C22b  C23  C24 C35a
 617   14  322  312  129  691   15    5   52    1    6   71  145    1  249
C35b C35c C35e    H  H25 H26a H31c  N11  N13  N15  N18  N19  N1A  N1D  N1H  N21 
 116    3    4   18    6    3    1    7  138   61   14    3  102   27  243    9 
 N31  Q11  Q21  Q22  Q24  Q25  Q41  Q42  Q43  Q44  Q51  Q52  Q53  Q54   Qa   Qb
   9   33   96   97   57  115   10    4    1    9  423  291  100   27  332  293 
   R  R12  R13  R1A  R1B  R1M  R1P  R1Q  R21  R22  R23  R35  R36  R37  R51 
1705    2   17   16    6   25   98  144  206  153   11  136  479   19    1
 R55  R57  R62  R63  S31  S32  S33  S35  S37  S38  S41  S42  S91  S92   Sa   Sb
  45    5    5  182    4   15    3   32    1    6  161   25   12  119   36   55
   T  T11  T12  T13  T15  T16  T17  T18  T1B  T1E  T1F  T32  T35  T3J  T3M 
 191   17    4    9    4   34  479   50  118    7    4    1    1    5    7 
   V  V11  V12  V15  V34  V35  V37  V38  V39 
  46    2    1  367   10  292   37   22    3
' 


### To explore assignments two helper functions are given
### eval.type displays the formal expressions for the given type
eval.type('A25c') # give definition and expressions for the given type
' 
A25c Atlantic coastal salt marsh 

 ((<#TC +01 A25c-Atlantic-coastal-salt-marsh GR #T$ +01 A25c-Atlantic-coastal-salt-marsh> AND <##Q +01 A25c-Atlantic-coastal-salt-marsh GR NON ##Q +01 A25c-Atlantic-coastal-salt-marsh>) NOT <#TC Trees|#TC Shrubs GR 15>) AND <$$C Coast_EEA EQ ATL_COAST> 

 ((col785 & col786) &! col3) & col4 

    expressions                                                                             
785 #TC +01 A25c-Atlantic-coastal-salt-marsh GR #T$ +01 A25c-Atlantic-coastal-salt-marsh    
786 ##Q +01 A25c-Atlantic-coastal-salt-marsh GR NON ##Q +01 A25c-Atlantic-coastal-salt-marsh
3   #TC Trees|#TC Shrubs GR 15                                                              
4   $$C Coast_EEA EQ ATL_COAST   
'

### eval.EUNIS(plot, type) gives
### 1. plant observations of the plot
### 2. possible type assignments for this plot
### 3. the formal definition of the type
### 4. logical rule within the matrix (with column numbers of 3.)
### 5. the expressions and their results for this plot x type



eval.EUNIS(which(result.classification == 'A25c')[1], 'A25c') # display plot and results of relevant expression for the given plot and type
' Plant observations in 2043 :
   RELEVE_NR                TaxonName Cover_Perc
1:      2043      Triglochin maritima          2
2:      2043       Atriplex calotheca         13
3:      2043       Chenopodium rubrum         13
4:      2043     Tripolium pannonicum         38
5:      2043          Suaeda maritima          2
6:      2043      Atriplex littoralis         38
7:      2043 Puccinellia distans agg.          2
Possible types of plot" 2043 " ( 1945 ): A25c R 
Priorities of these types: 6 1 
A25c Atlantic coastal salt marsh 

 ((<#TC +01 A25c-Atlantic-coastal-salt-marsh GR #T$ +01 A25c-Atlantic-coastal-salt-marsh> AND <##Q +01 A25c-Atlantic-coastal-salt-marsh GR NON ##Q +01 A25c-Atlantic-coastal-salt-marsh>) NOT <#TC Trees|#TC Shrubs GR 15>) AND <$$C Coast_EEA EQ ATL_COAST> 

 ((col785 & col786) &! col3) & col4 

    expressions.for.this.type                                                                result
785 #TC +01 A25c-Atlantic-coastal-salt-marsh GR #T$ +01 A25c-Atlantic-coastal-salt-marsh      TRUE 
786 ##Q +01 A25c-Atlantic-coastal-salt-marsh GR NON ##Q +01 A25c-Atlantic-coastal-salt-marsh  TRUE 
3   #TC Trees|#TC Shrubs GR 15                                                               FALSE 
4   $$C Coast_EEA EQ ATL_COAST  
'

```



# Terms

    '###'' Species Group
    '##D' Discriminating Species Group
    '##Q' Sum of Square rooted percentage cover of species in this group

    '#TC'  Total Cover
    '#SC'  Summed Square root of Cover
    '#T$'  Total Cover without specified Group
    '#$$'  cover in the plot of a single species that does not belong to the speciefied group
    '$00'  00 percentage of the Total Cover of all species in the Plot 

    '+01' and similar: group of groups for comparisons

    '$$N'  Numerical header data
    '$$C'  Categorical header data


# Authors
Florian Jansen <florian.jansen at uni-rostock.de>

Helge Bruelheide <helge.bruelheide at botanik.uni-halle.de>

