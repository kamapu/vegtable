########################################################
# Read and convert vegetweb test file
# 1. Download project  "100716 Hoppe (2005)" from vegetweb.de
# 2. Import downloaded XML file into local Turboveg 2 (http://www.synbiosys.alterra.nl/turboveg/) installation
# 3. Run this code

#######################################################
library(vegdata)
library(data.table)
###################################################### #
## Load infos from EUNIS classification 
# EUNIS classification expects several header data, e.g. Coast_EEA, Ecoreg, Dunes_Bohn
# if not present they have to be added
# # "For JUICE output the releve number starts with country code, which is in your case 32.
# DB <- 'EUNIS'
# EUNIS <- environment()
# attach(file.path('data', 'data.EUNIS.RData'), name = 'EUNIS')
# EUNISheader <- header
# table(EUNISheader$Dataset)
# #EUNISheader <- EUNISheader[EUNISheader$Dataset == 'Germany Vegetweb 2', ]
# #EUNISheader$`TV2 relevé number` <- EUNISheader$`TV2 relevé number` - 32000000
# meta <- read.csv2('data/EVA_Metadata_Florian.csv', sep = ';', as.is = TRUE)
# EUNISheader$GUID <- meta$GUID[match(EUNISheader$PlotID, meta$PlotObservationID)] 
# table(meta$Dataset) 
# 
# tuexenarchiv <- tv.readXML('/home/jansen/Projekte/vegetweb2.0/Daten/04_XML/TuexenArchiv/tvexport.xml')
####################################################### #
## Read plant observation data
options(tv_home="~/Projekte/vegetweb2.0/Daten/03_TV_projectDB")
db <- tv.db('vegetweb/TuexenArchiv') # or whatever you called it
obs <- tv.obs(db)
obs <- tv.coverperc(db, obs) # Calculate percentage cover
obs <- obs[, c(1,2,4,7,5)]
names(obs) <- c("RELEVE_NR", "TaxonUsageID", "LAYER", "Cover_Perc", "TaxonName")
obs$OrigName <- obs$TaxonName
sort(unique(obs$TaxonName))
### Adapt Taxonomy
# Replace Synonyms, delete higher taxon observations, aggregate to the highest level occurring in the dataset, see Jansen et al. 2008. 
# using Crosswalk file, see Chytry et al 2020 Electronic appendices:  Nomenclature-translation-from-Turboveg-2-databases.zip at https://zenodo.org/record/3841729#.X13ukXX7RhE
# to adjust nomenclature
taxoncross  <- read.csv('data/GermanSL 1.4_ExpertSystem.txt', header = FALSE,
                    encoding="UTF-8", stringsAsFactors = FALSE, sep="\t")[,1]
section1 <- grep('SECTION 1', taxoncross)
species.agg <- taxoncross[(section1[1]+1) : (section1[2]-1)]
index.agg.names <- which(substr(species.agg,1,1)!=" ")
number.agg <- length(index.agg.names) # 316
ind.agg.names <- c(index.agg.names, length(species.agg)+1)
source('prep.R')
aggs <- lapply(X = 1:number.agg, function(x) trim.leading(species.agg[(ind.agg.names[x] + 1) : (ind.agg.names[x+1] - 1)]) )
names(aggs) <- trim.trailing(species.agg[index.agg.names])
for(i in 1:length(aggs))
  aggs[[i]] <- sapply(aggs[[i]], function(x) trim.trailing(x), USE.NAMES = FALSE)
AGG <- stack(aggs)
AGG$ind <- as.character(AGG$ind)
AGG <- AGG[AGG$values != '',]
index1 <- match(obs$TaxonName, AGG$values)
sum(!is.na(index1))
obs$TaxonName[!is.na(index1)] <- AGG$ind[index1[!is.na(index1)]]

# # For matching with Euro+Med nomenclature see https://euromed.infinitenature.org/
# euromed <- readRDS('~/Projekte/Reflists/EURO+Med/euromed.v5.Germany.Rds')
# obsval$EuroMedConcept <- euromed$TaxonConcept[match(obsval$TaxonName, euromed$TaxonName)]
# obsval$TaxonName[!is.na(obsval$EuroMedConcept)] <- obsval$EuroMedConcept[!is.na(obsval$EuroMedConcept)]
#
# # For Germany taxonomic harmonization can best be done with R package vegdata and https://germansl.infinitenature.org/:
# obsval <- taxval(obs, refl='GermanSL 1.4', maxtaxlevel = 'AGG', interactive = FALSE)
# obsval$TaxonName <- tax(obsval$TaxonUsageID, 'GermanSL 1.5')$TaxonName

# obsval$TaxonName <- sub(' s. str.', '', obsval$TaxonName, fixed = TRUE)
# # obsval$TaxonName <- sub(' s. l.', '', obsval$TaxonName, fixed = TRUE)
# obsval$TaxonName <- sub(' agg.', ' aggr.', obsval$TaxonName, fixed = TRUE)
# obsval$TaxonName[obsval$TaxonName == 'Lychnis flos-cuculi'] <- 'Silene flos-cuculi'
# obsval$TaxonName[obsval$TaxonName == 'Plantago major aggr.'] <- 'Plantago major'
# obsval$TaxonName[obsval$TaxonName == 'Cardamine pratensis s. l.'] <- 'Cardamine pratensis'
# obsval$TaxonName[obsval$TaxonName == 'Potentilla anserina'] <- 'Argentina anserina'
# obsval$TaxonName[obsval$TaxonName == 'Festuca rubra'] <- 'Festuca rubra aggr.'
# obsval$TaxonName[obsval$TaxonName == 'Festuca pratensis s. l.'] <- 'Festuca pratensis aggr.'
# obsval$TaxonName[obsval$TaxonName == 'Anthoxanthum odoratum'] <- 'Anthoxanthum odoratum aggr.'
# obsval$TaxonName[obsval$TaxonName == 'Poa trivialis subsp. trivialis'] <- 'Poa trivialis'
# obsval$TaxonName[obsval$TaxonName == 'Persicaria lapathifolia s. l.'] <- 'Persicaria lapathifolia'
# obsval$TaxonName[obsval$TaxonName == 'Glyceria fluitans'] <- 'Glyceria fluitans aggr.'
# obsval$TaxonName[obsval$TaxonName == 'Poa trivialis subsp. trivialis'] <- 'Poa trivialis'
# obsval$TaxonName[obsval$TaxonName == 'Poa pratensis'] <- 'Poa pratensis aggr.'
# obsval$TaxonName[obsval$TaxonName == 'Elymus repens'] <- 'Elytrigia repens aggr.'

## Now we can use additionally the aggregation from the expert file
expertfile <- "EUNIS-ESy-2020-06-08.txt"
source('prep.R')
source('step1and2.R')
aggs <- parsing.result$species.aggs
AGG <- stack(aggs)
AGG$ind <- as.character(AGG$ind)
#AGG <- AGG[AGG$values != AGG$ind,]
AGG <- AGG[AGG$values != '',]
index1 <- match(obs$TaxonName, AGG$values)
sum(!is.na(index1))
obs$TaxonName[!is.na(index1)] <- AGG$ind[index1[!is.na(index1)]]
# Manual tweeking
obs$TaxonName[obs$TaxonName == "Taraxacum sect. Alpina, Hamata et Ruderalia"] <- 'Taraxacum sect. Taraxacum'
obs <- obs[grep('ZZZ', obs$TaxonName, invert = TRUE), ]
obs[grep('verticillata', obs$TaxonName), ]

# check nomenclature
nomatch <- sort(table(obs$TaxonName[!obs$TaxonName %in% AGG$values & !obs$TaxonName %in% AGG$ind & !obs$TaxonName %in% unlist(groups, use.names = FALSE)]))
tail(nomatch, 25)
library(readxl)
bryo <- read_xlsx('data/Various-names-bryo-lich-algae-fungi.xlsx', col_names = FALSE)
write.csv(sort(names(nomatch)[!names(nomatch) %in% bryo$...1]), 'nomatch.csv')


obs <- data.table(obs) # convert to data.table for speed
setkey(obs, RELEVE_NR)
# tail(sort(table(obs$TaxonName[obs$TaxonName %in% AGG$ind])))

## Header data
header <- tv.site(db)
header <- header[, !names(header) %in% c('AUTOR', 'LOC_KEY', 'LOCATION', 'DATE_MAX', 'DATE_MIN', 'MUNIP', 'COMMENT', "SOIL_TYPE", "SOIL_TEXTU", "WATER_DEPT", "UNCLR_SPEC", "FUNGI", 'NOTE_1', 'NOTE_2', 'CON_SOC', 'CON_BRAC', 'CON_NOTE', 'CON_CF', 'CON_MERGE')]
names(header)[names(header) == 'COUNTRY'] <- "Country"
names(header)[names(header) == 'LONGITUDE'] <- 'DEG_LON'
names(header)[names(header) == 'LATITUDE'] <- 'DEG_LAT'
header$Country[header$Country == 'DE'] <- 'Germany'
header$dataset <- 'Germany Vegetweb 2'

## Adapt or add altitude
names(header)[names(header) == 'ALTITUDE'] <- "Altitude (m)"
header$`Altitude (m)` <- as.numeric(header$`Altitude (m)`)
require(geonames)
options(geonamesUsername="fjansen")
# for(i in 1:nrow(header))
#   if(is.na(header$Altitude[i]) | header$Altitude[i] == 0 )
#       header$Altitude[i] <- unlist(GNsrtm3(header$DEG_LON[i], header$DEG_LAT[i])$srtm3)
header$`Altitude (m)`[header$`Altitude (m)` == -32768] <- 1
header$`Altitude (m)`[header$`Altitude (m)` == 0] <- 1

## Add Coast assignment (better be done by buffering Coastline-Shape)
header$Coast_EEA <- 'N_COAST'
header$Coast_EEA[header$DEG_LON < 8.7 & header$DEG_LAT > 53.5] <- 'ATL_COAST'
header$Coast_EEA[header$DEG_LON > 9.5 & header$DEG_LAT > 54] <- 'BAL_COAST'
table(header$Coast_EEA)
' ATL_COAST BAL_COAST   N_COAST 
     1758       382      8577 
'

## Add Ecoregion
# Download shape file from https://storage.googleapis.com/teow2016/Ecoregions2017.zip
require(rgdal)
library(rgeos)
library(sp)
ecoreg <- readOGR(dsn = "~/Projekte/GIS/Shapes Global/Ecoregions2017", layer = "Ecoregions2017")
plotlocs <- header[, c('DEG_LON', 'DEG_LAT')]
coordinates(plotlocs) <- ~DEG_LON+DEG_LAT
proj4string(plotlocs) <- CRSargs(CRS("+proj=longlat +datum=WGS84 +no_defs "))
ov <- sp::over(plotlocs, ecoreg)
header$Ecoreg <- as.numeric(as.character(ov$ECO_ID))
header$Ecoreg[is.na(header$Ecoreg)] <- 664
table(header$Ecoreg)

names(header)
bohn <- readOGR(dsn = "~/Projekte/GIS/Shapes Global/DUNES_BOHN", layer = "Dunes_BohnMap_buffer500m")
bohn <- spTransform(bohn,  CRS("+proj=longlat +datum=WGS84 +no_defs "))
ov <- sp::over(plotlocs, bohn)
header$Dunes_Bohn <- as.character(ov$DUNE)
header$Dunes_Bohn[is.na(header$Dunes_Bohn)] <- 'N_Dunes'

header <- header[header$RELEVE_NR %in% obs$RELEVE_NR, c('RELEVE_NR', 'Country', "Altitude (m)", 'DEG_LON', 'DEG_LAT', 'GESELLSCH', 'dataset', 'Ecoreg', 'Dunes_Bohn', 'Coast_EEA')] 
      # Code will throw an error for plots without plant observations, 
      # only header information important for EUNIS assignment preserved

obs <- obs[obs$RELEVE_NR %in% header$RELEVE_NR, c('RELEVE_NR','TaxonName', 'Cover_Perc')]
##################################################### #
## Save obs and header                             ####
DB <- '100716 Hoppe (2005)'
write.csv(obs, file = file.path('data', 'obs_100716Hoppe2005.csv'), row.names = FALSE)
write.csv(header, file = file.path('data', 'header_100716Hoppe2005.csv'), row.names = FALSE)
# save(obs, header, file = file.path('data', paste("data", word(DB, sep='/', start=length(strsplit(DB, '/')[[1]])), "RData", sep='.')))


