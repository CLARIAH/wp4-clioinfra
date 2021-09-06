#### bug on reproducing the website: the merge command on row 164:
# GlobalMetadata <- merge(UNregions, Countries, by.x = "numeric", by.y = "ccode", all.x = T, all.y = T)
# introduces one more line on the top compared to the version of 2017. Just in case: Check if the xls that are merged have changed.

# Clio Infra: Heights are missing from IndicatorsPerCountry; missing: data/rankings.csv; 
# data/nations.json;data/DataAtHistoricalBorders.xlsx; graphs/SDN_LabourersRealWage.svg;
# graphs/TUR_HistoricalGenderEqualityIndex.svg; Inflation should be worse as increasing in abs terms; 
#Check what to do with the citation of Historical Gender Inequality Index

options( java.parameters = "-Xmx10g")

library(plyr)
library(readxl)
library(WriteXLS)
library(xlsx)
library(stringr)
library(foreign)
library(countrycode)

rm(list=ls())

nthsplit <- function(lst, n){
  sapply(lst, `[`, n)
}

### "Labourers_wage-historical.xlsx" only had 203 instead of 523 variables, bcs it started from 1820 instead of 1500...
### also two files have more rows, but those are just NA rows read accidently by R, and I remove them
### this happens for: "Book_titles_capita-historical.xlsx-1417" and "Exchange_Rates_UK-historicalV2.xlsx-1414"

DataPath <- paste0(dirname(rstudioapi::getSourceEditorContext()$path),'/historical.all.standardized.2/')
ExportPath <- paste0(dirname(rstudioapi::getSourceEditorContext()$path),'/Data Exports R/')

FilenamePattern <- '.xlsx'
files <- list.files(path=DataPath,pattern = FilenamePattern)
files <- files[!(files=='Labourers_Wage-historical.xlsx')]

ClioData <- suppressMessages(read.xlsx2(paste0(DataPath,files[1]), 
                                        sheetIndex=1, startRow = 3, stringsAsFactors = F, check.names = F, 
                                        colClasses = c("character",rep("numeric",2),"character",rep("numeric",520))))
ClioData[sapply(ClioData, is.nan)] <- NA
ClioData$Filename <- files[1]
IndicatorName <- suppressMessages(read.xlsx2(paste0(DataPath,files[1]), sheetIndex=1, stringsAsFactors = F, check.names = F))
# the warnings above are just for renaming the columns, because of the reading starting at the top row with missing column names in xls

ClioData$Indicator <- names(IndicatorName)[1]

ClioData <- ClioData[rowSums(is.na(ClioData[,7:(ncol(ClioData)-2)]))<(ncol(ClioData)-8),]

print(paste0(files[1],";",nrow(ClioData)))
rm(IndicatorName)

for (j in 2:length(files)){
  #if (paste0(files[j])=='Global_Extreme_Poverty_Cost_of_Basic_Needs.xlsx'){
  #  stop('Global_Extreme_Poverty_Cost_of_Basic_Needs.xlsx')
  #}
  nextContent <- suppressMessages(read.xlsx2(paste0(DataPath,files[j]), 
                                             sheetIndex=1, startRow = 3, stringsAsFactors = F, check.names = F, 
                                             colClasses = c("character",rep("numeric",2),"character",rep("numeric",520))))
  if (ncol(nextContent)>524){
    
    nextContent <- suppressMessages(read.xlsx2(paste0(DataPath,files[j]), 
                                               sheetIndex=1, startRow = 3, stringsAsFactors = F, check.names = F, 
                                               colClasses = c("character",rep("numeric",2),"character",rep("numeric",557))))
  }
  
  nextContent[sapply(nextContent, is.nan)] <- NA
  
  # keep only rows that have not all elements of the specified columns NA
  # nextContent <- nextContent[rowSums(is.na(nextContent[,7:(ncol(nextContent)-2)]))<(ncol(nextContent)-8),]
  
  # does not work but I think it should not work purposefully
  nextContent$Filename <- files[j]
  
  IndicatorName <- suppressMessages(read.xlsx2(paste0(DataPath,files[j]), sheetIndex=1, stringsAsFactors = F, check.names = F))
  nextContent$Indicator <- names(IndicatorName)[1]
  
  if (!(nrow(nextContent)==1412)){
    print(paste0("NO 1412 lines in df:",files[j],", it has: ",nrow(nextContent)," instead"))
  }
  
  if (nrow(subset(nextContent,is.na(nextContent$`country name`)))>0){
    print(paste0("NA country name: ",files[j],"-",nrow(nextContent)))
  }
  
  print(paste0(files[j],";",nrow(nextContent)))
  #stop('Add a control for the additional columns')
  
  if (ncol(nextContent)>ncol(ClioData)){
    w1 = ncol(ClioData)+1
    w2 = ncol(nextContent)
    ClioData[c(w1:w2)] <- as.numeric(NA)
    names(ClioData)[c(w1:w2)] <- c((max(suppressWarnings(as.numeric(names(ClioData))),na.rm = T)+1):max(suppressWarnings(as.numeric(names(nextContent))),na.rm = T))
    ClioData <- ClioData[,names(nextContent)]
    ClioData <- rbind(ClioData,nextContent)
  } else if (ncol(nextContent)<ncol(ClioData)){
    w1 = ncol(nextContent)+1
    w2 = ncol(ClioData)
    nextContent[c(w1:w2)] <- as.numeric(NA)
    names(nextContent)[c(w1:w2)] <- c((max(suppressWarnings(as.numeric(names(nextContent))),na.rm = T)+1):max(suppressWarnings(as.numeric(names(ClioData))),na.rm = T))
    nextContent <- nextContent[,names(ClioData)]
    ClioData <- rbind(ClioData,nextContent)
  } else {
    ClioData <- rbind(ClioData,nextContent)
  }
  rm(nextContent,IndicatorName)
}

# add the "Labourers_wage-historical.xlsx" dataset
# Adding the columns to host the data
MissingFile <- "Labourers_Wage-historical.xlsx"
nextContent <- read.xlsx2(paste0(DataPath,MissingFile), sheetIndex=1, startRow = 3, stringsAsFactors = F, check.names = F,
                          colClasses = c("character",rep("numeric",2),"character",rep("numeric",520)))
nextContent[sapply(nextContent, is.nan)] <- NA
print(paste0(MissingFile,"-",nrow(nextContent)))

# add missing columns
w1 = ncol(nextContent)+1
w2 = w1+length(c(1500:1819))-1
nextContent[c(w1:w2)] <- NA
names(nextContent)[c(w1:w2)] <- c(1500:1819)
nextContent$Filename <- MissingFile
IndicatorName <- read.xlsx2(paste0(DataPath,MissingFile), sheetIndex=1, stringsAsFactors = F, check.names = F)
nextContent$Indicator <- names(IndicatorName)[1]
w1 = ncol(nextContent)+1
w2 = ncol(ClioData)
nextContent[c(w1:w2)] <- as.numeric(NA)
names(nextContent)[c(w1:w2)] <- c((max(suppressWarnings(as.numeric(names(nextContent))),na.rm = T)+1):max(suppressWarnings(as.numeric(names(ClioData))),na.rm = T))
nextContent <- nextContent[,names(ClioData)]
nextContent <- nextContent[rowSums(is.na(nextContent[,7:(ncol(nextContent)-2)]))<(ncol(nextContent)-8),]

ClioData <- rbind(ClioData,nextContent)
rm(nextContent,IndicatorName)

# bring the Filename &Indicator columns up front
ClioData <- ClioData[c(1:6,558,559,7:557)]
# total observations:
sum(rowSums(!is.na(ClioData[,9:(ncol(ClioData))])))

########################### Get the Country Identifiers right #######################
# now I need to add all the info about the country's region, names, ccode, iso3, etc.

UNmembers <- read_excel("/home/michalis/PhD/Clio Infra/Website/OFFICIAL NAMES OF THE UNITED NATIONS MEMBERSHIP.xls",sheet = 1)
ISO3166 <- read_excel("/home/michalis/PhD/Clio Infra/Website/ISO3166.xls",sheet = 1)
ISO3166$`French short name` <- NULL

UNregions <- read_excel("/home/michalis/PhD/Clio Infra/Website/WorldRegiondsUN_categorized.xls",sheet = 1)
#UNregions$numeric <- as.character(as.integer(UNregions$numeric))

# add the other identifying codes of the country
UNregions <- merge(UNregions,ISO3166,by.x = "numeric", by.y = "Numeric", all.x = T, all.y = T)
UNregions$UN_membership_name <- NA

# find the official name of the country:
for (i in 1:nrow(UNregions)){
  if (!is.na(UNregions$country_name[i])){
    if (sum(as.numeric(as.logical(grepl(UNregions$country_name[i], UNmembers$`OFFICIAL NAMES OF THE UNITED NATIONS MEMBERSHIP`))))==1){
      UNregions$UN_membership_name[i] <- UNmembers$`OFFICIAL NAMES OF THE UNITED NATIONS MEMBERSHIP`[which(grepl(UNregions$country_name[i], UNmembers$`OFFICIAL NAMES OF THE UNITED NATIONS MEMBERSHIP`))]
    }
    if (sum(as.numeric(as.logical(grepl(UNregions$country_name[i], UNmembers$`OFFICIAL NAMES OF THE UNITED NATIONS MEMBERSHIP`))))>1){
      print(paste0(UNregions$country_name[i],"-",i))
    }
  }
}
#### XXX check this ?####
# [1] "Congo-51"
# [1] "Dominica-61"
# [1] "Guinea-94"
# [1] "Ireland-107"
# [1] "Republic of Korea-117"
# [1] "Niger-160"
# [1] "Sudan-212"

UNregions$UN_membership_name[which(UNregions$country_name=="Venezuela (Bolivarian Republic of)")] <-
  "Bolivarian Republic of Venezuela"
UNregions$UN_membership_name[which(UNregions$country_name=="Bolivia (Plurinational State of)")] <- 
  "Plurinational State of Bolivia"
UNregions$UN_membership_name[which(UNregions$country_name=="Congo")] <- "Republic of the Congo"
UNregions$UN_membership_name[which(UNregions$country_name=="Dominica")] <- 
  "Commonwealth of Dominica" # XXX double check this
UNregions$UN_membership_name[which(UNregions$country_name=="Guinea")] <- 
  "Republic of Guinea"
UNregions$UN_membership_name[which(UNregions$country_name=="Iran (Islamic Republic of)")] <- "Islamic Republic of Iran"
UNregions$UN_membership_name[which(UNregions$country_name=="Ireland")] <- "Ireland"
UNregions$UN_membership_name[which(UNregions$country_name=="Cote d'Ivoire")] <- "Republic of Côte d’Ivoire"
UNregions$UN_membership_name[which(UNregions$country_name=="Democratic People's Republic of Korea")] <- "Democratic People’s Republic of Korea"
UNregions$UN_membership_name[which(UNregions$country_name=="Republic of Korea")] <- "Republic of Korea"
UNregions$UN_membership_name[which(UNregions$country_name=="Lao People's Democratic Republic")] <- "Lao People’s Democratic Republic"
UNregions$UN_membership_name[which(UNregions$country_name=="Niger")] <- "Republic of the Niger" # XXX double check this and NGA
UNregions$UN_membership_name[which(UNregions$country_name=="Micronesia (Federated States of)")] <- "Federated States of Micronesia"
UNregions$UN_membership_name[which(UNregions$country_name=="Slovakia")] <- "Slovak Republic"
UNregions$UN_membership_name[which(UNregions$country_name=="Sudan")] <- "Republic of the Sudan"
UNregions$UN_membership_name[which(UNregions$country_name=="Switzerland")] <- "Swiss Confederation"

# check if there are some official UN membership names not used
UsedNames <- as.vector(subset(UNregions,!is.na(UNregions$UN_membership_name))[,12])
UNmembers$`OFFICIAL NAMES OF THE UNITED NATIONS MEMBERSHIP`[which(!(UNmembers$`OFFICIAL NAMES OF THE UNITED NATIONS MEMBERSHIP` %in% UsedNames))]
# ok now we got them all; "Slovak Republic"       "Republic of the Sudan" "Swiss Confederation" were missing

# Now we get the list of countries existing in clio infra datasets 
#(but we only care about--and keep only--those with data):

CountriesList <- unique(ClioData$`country name`)

# now match all the countries in the data with naming information from sources above

# then get country codes and country names only
Countries <- ClioData[,c(1,2,3,4)]
# remove the duplicate country names
CountriesA <- Countries[!duplicated(Countries$`country name`),]
# remove the duplicate country codes
CountriesB <- Countries[!duplicated(Countries$ccode),]
# check countries with country codes that have unique country names
cntryB_list <- unique(CountriesB$`country name`)
# keep from the unique country names those that are not in the countryB list
CountriesA <- subset(CountriesA, !(CountriesA$`country name` %in% cntryB_list))
# merge the two lists (one that has unique names of countries with ccode, 
# and the other that has unique names of countries that do not have a ccode)
Countries <- rbind(CountriesA,CountriesB)

# merge the two country lists with UNregions:
# part A are the countries with ccode in both lists, and they are matched like that; 
# keeping the name of the ClioInfra dataset as well
names(Countries)[4] <- "ClioInfraCountryName"

GlobalMetadata <- merge(UNregions, Countries, by.x = "numeric", by.y = "ccode", all.x = T, all.y = T)

# Finally add the OECD regions: NEED TO UPDATE THE LIST WITH THE LATEST ONE BUT I COULD NOT FIND IT SO FAR
OECDregions <- read.csv("/home/michalis/PhD/Clio Infra/Website/oecdregions.csv", stringsAsFactors = F)

GlobalMetadata <- merge(GlobalMetadata, OECDregions, by.x = "numeric", by.y = "ccode", all.x = T, all.y = T)

# add the Webmapper numeric codes that correspond to the same name that correspond to each particular numeric ccode
# also get those along with the time period they correspond to as indicated in the datasets

GlobalMetadata$WebmapperNums <- NA
GlobalMetadata$WebmapperCodes <- NA
GlobalMetadata$WebmapperStartYears <- NA
GlobalMetadata$WebmapperEndYears <- NA

# using a single indicator subset so that there is no duplicate that I would need to remove; if I use the complete dataset and
# then the unique command I am loosing irrecoverably combinations of border year changes that have common number of duplicates
# at both start and end; besides for those that single out I would still have to correct it manually

for (i in 1:nrow(GlobalMetadata)){
  TempClioData <- subset(ClioData,ClioData$`country name` == GlobalMetadata$ClioInfraCountryName[i])
  if (nrow(TempClioData)>0){
    TempClioData$StartEndYears <- paste0(TempClioData$`start year`,'-',TempClioData$`end year`)
    if (!is.na(GlobalMetadata$numeric[i])){
      if (nchar(paste0(unique(TempClioData$`Webmapper numeric code`[which(GlobalMetadata$numeric[i]==TempClioData$ccode)]), collapse = ";"))>0){
        
        GlobalMetadata$WebmapperNums[i] <- paste0(unique(TempClioData$`Webmapper numeric code`[
          which(TempClioData$`country name`==unique(TempClioData$`country name`[which(GlobalMetadata$numeric[i]==TempClioData$ccode)]))]), collapse = ";")
        
        GlobalMetadata$WebmapperCodes[i] <- paste0(unique(TempClioData$`Webmapper code`[
          which(TempClioData$`country name`==unique(TempClioData$`country name`[which(GlobalMetadata$numeric[i]==TempClioData$ccode)]))]), collapse = ";")
        
        GlobalMetadata$WebmapperStartYears[i] <- paste0(nthsplit(strsplit(unique(TempClioData$StartEndYears[
          which(TempClioData$`country name`==unique(TempClioData$`country name`[which(GlobalMetadata$numeric[i]==TempClioData$ccode)]))]),'-'),1), collapse = ";")
        
        GlobalMetadata$WebmapperEndYears[i] <- paste0(nthsplit(strsplit(unique(TempClioData$StartEndYears[
          which(TempClioData$`country name`==unique(TempClioData$`country name`[which(GlobalMetadata$numeric[i]==TempClioData$ccode)]))]),'-'),2), collapse = ";")
      }
    } else {
      if (!is.na(GlobalMetadata$`Webmapper numeric code`[i])){
        
        GlobalMetadata$WebmapperNums[i] <- paste0(unique(TempClioData$`Webmapper numeric code`[which(TempClioData$`country name`==GlobalMetadata$ClioInfraCountryName[i])]), collapse = ";")
        GlobalMetadata$WebmapperCodes[i] <- paste0(unique(TempClioData$`Webmapper code`[which(TempClioData$`country name`==GlobalMetadata$ClioInfraCountryName[i])]), collapse = ";")
        
        GlobalMetadata$WebmapperEndYears[i] <- paste0(nthsplit(strsplit(unique(TempClioData$StartEndYears[
          which(TempClioData$`country name`==GlobalMetadata$ClioInfraCountryName[i])]),'-'),2), collapse = ";")
        
        GlobalMetadata$WebmapperStartYears[i] <- paste0(nthsplit(strsplit(unique(TempClioData$StartEndYears[
          which(TempClioData$`country name`==GlobalMetadata$ClioInfraCountryName[i])]),'-'),1), collapse = ";")
        
      }
    }
  }
}

# also test which other have different number of year ends and beginnings, and correct them all:
for (i in 1:nrow(GlobalMetadata)){
  if (!is.na(GlobalMetadata$WebmapperStartYears[i]) & !is.na(GlobalMetadata$WebmapperEndYears[i])){
    if (nchar(GlobalMetadata$WebmapperStartYears[i])>nchar(GlobalMetadata$WebmapperEndYears[i])){
      print(paste0(i,':::',GlobalMetadata$ClioInfraCountryName[i]))
    }
  }
}

GlobalMetadata$DataPoints <- NA
ClioData[ClioData==""]<-NA
# finally add a column if that country has any data or not:
AllCountryNames <- c()
for (i in 1:nrow(GlobalMetadata)){
  GlobalMetadata$DataPoints[i] <- 
    sum(rowSums(!is.na(ClioData[which(as.numeric(ClioData$`Webmapper numeric code`) %in% 
                                        as.numeric(strsplit(GlobalMetadata$WebmapperNums[i],";")[[1]])),9:ncol(ClioData)])))
  if (GlobalMetadata$DataPoints[i]>0){
    # since it is only Clio that gives data then I only use the name of clio here:
    #    AllCountryNames <- c(AllCountryNames,paste(GlobalMetadata$country_name[i],GlobalMetadata$`English short name`[i],
    #    GlobalMetadata$UN_membership_name[i], GlobalMetadata$ClioInfraCountryName[i],GlobalMetadata$OECD_Country_Name[i],
    #    GlobalMetadata$OECD_simple_name[i], sep=";", collapse = ""))
    AllCountryNames <- c(AllCountryNames,GlobalMetadata$ClioInfraCountryName[i])
  }
}

AllCountryNames <- AllCountryNames[!is.na(AllCountryNames)]
AllCountryNames <- unique(AllCountryNames)
GMWithDataCountries <- AllCountryNames
GMWithDataCountries <- sort(GMWithDataCountries)
length(GMWithDataCountries)
length(which(GlobalMetadata$DataPoints>0))

# the total observations from the main dataset:
sum(rowSums(!is.na(ClioData[,9:(ncol(ClioData))])))
# 872264
# and that of the GlobalMetadata must be the same:
sum(GlobalMetadata$DataPoints)
# 872264

# I get 211 countries with data, and so does the export from the code below:
# [this seems like an old comment, because now I do get 211 countries] I now get 143 countries with data from the above...

# try again:
GlobalMetadata$DataPoints <- NA
ClioData[ClioData==""]<-NA

# finally add a column if that country has any data or not:
AllCountryNames <- c()
for (i in 1:length(CountriesList)){
  CountryData <- subset(ClioData,ClioData$`country name`==CountriesList[i])
  GlobalMetadata$DataPoints[which(GlobalMetadata$ClioInfraCountryName==CountriesList[i])] <- sum(rowSums(!is.na(CountryData[,9:(ncol(CountryData))])))
  if (GlobalMetadata$DataPoints[which(GlobalMetadata$ClioInfraCountryName==CountriesList[i])]>0){
    AllCountryNames <- c(AllCountryNames,CountriesList[i])
  }
}

AllCountryNames <- AllCountryNames[!is.na(AllCountryNames)]
AllCountryNames <- unique(AllCountryNames)
GMWithDataCountries <- AllCountryNames
GMWithDataCountries <- sort(GMWithDataCountries)
length(GMWithDataCountries)
length(which(GlobalMetadata$DataPoints>0))

# the total observations from the main dataset:
sum(rowSums(!is.na(ClioData[,9:(ncol(ClioData))])))
# 872264
# and that of the GlobalMetadata must be the same:
sum(GlobalMetadata$DataPoints, na.rm = T)
# 872264

# the loop below says 211:
NoDataCountries <- c()
CDWithDataCountries <- c()

for (k in 1:length(CountriesList)){
  CountryData <- subset(ClioData,ClioData$`country name`==CountriesList[k])
  # keep only rows that have not all elements of the specified columns NA
  CountryData <- CountryData[rowSums(is.na(CountryData[,9:ncol(ClioData)]))<(ncol(ClioData)-8),]
  if (nrow(CountryData)>0){
    CDWithDataCountries <- c(CDWithDataCountries,CountriesList[k])
    WriteXLS(CountryData, paste0(ExportPath,CountriesList[k],".xlsx"))
  } else {
    # I want to know the names of countries without data
    #print(CountriesList[k])
    NoDataCountries <- c(NoDataCountries,CountriesList[k])
  }
  rm(CountryData)
}

length(NoDataCountries)
# 278
length(CDWithDataCountries)
# 211

CDWithDataCountries[which(!CDWithDataCountries %in% GlobalMetadata$ClioInfraCountryName)]

# OK now lets identify those countries that are missing from the first loop:

# first check which of the 211 countries found with data in the second loop are not found as such by the first that gives 194
MissedCountries <- CDWithDataCountries[which(!(CDWithDataCountries %in% GMWithDataCountries))]
# those below used to be missing, but now they are ok!
#[1] "Ceylon"                       "Czechoslovakia"               "England"                      "Falklands"                   
#[5] "Gambia"                       "German Confederation"         "Hong Kong"                    "Ottoman"                     
#[9] "Persia"                       "Poland-Lithuania"             "Prussia"                      "West Germany"                
#[13] "Yugoslavia"                   "USSR"                         "Germany Federal Republic"     "Egypt (United Arab Republic)"
#[17] "Yemen Arab Republic"

# save.image("~/PhD/Clio Infra/TempDebugging.RData")

# second check which of the countries found with data in the first loop are not found as such by the second
GMWithDataCountries[which(!(GMWithDataCountries %in% CDWithDataCountries))]
# character(0)

WriteXLS(GlobalMetadata,paste0(dirname(rstudioapi::getSourceEditorContext()$path),'/Aggregates/Global_Metadata.xls'))

# duplicates of the same territorial instances and merge their properties (e.g. ccode from Oecd for a country not having ccode before, like USSR)
# I now need to work for cases like USSR and Soviet Union.

# XXX note there are both USSR and Soviet Union on ClioInfra 'country names' 
# which I think I need to substitute and also use the obsolete ISO3 code
# changes can be found here: https://en.wikipedia.org/wiki/ISO_3166-3

# For those countries with ClioInfraCountryName this should be correct already in the metadata file
# for the countries without such a name I need to manually inspect and correct line by line:
# I also need to assign groupings to those countries without any; those should be stated somewhere in a working paper
# on the website

# a great title for a dataframe :)
CountriesWithIssues <- subset(GlobalMetadata,is.na(GlobalMetadata$WebmapperNums))
WriteXLS(CountriesWithIssues,paste0(dirname(rstudioapi::getSourceEditorContext()$path),'/Aggregates/CountriesWithIssues.xls'))

# add a column for tracking numeric codes changes 
GlobalMetadata$PredecessorNumeric <- NA

### is american samoa == east samoa?
### I think so based on the relevant wikipedia pages

# and I need to check with the names of countries in the GlobalMetadata 
# if there are the duplicated as listed below (with different nowname numbers)
# OR check why the 2nd row names are usually not present now...

# for Bermuda merge rows 19 and 297
# now there is no row 297...
GlobalMetadata$`Webmapper code`[which(rownames(GlobalMetadata)=="19")] <- GlobalMetadata$`Webmapper code`[which(rownames(GlobalMetadata)=="297")]
GlobalMetadata$`Webmapper numeric code`[which(rownames(GlobalMetadata)=="19")] <- GlobalMetadata$`Webmapper numeric code`[which(rownames(GlobalMetadata)=="297")]
GlobalMetadata$ClioInfraCountryName[which(rownames(GlobalMetadata)=="19")] <- GlobalMetadata$ClioInfraCountryName[which(rownames(GlobalMetadata)=="297")]
GlobalMetadata$WebmapperNums[which(rownames(GlobalMetadata)=="19")] <- GlobalMetadata$WebmapperNums[which(rownames(GlobalMetadata)=="297")]
GlobalMetadata$WebmapperCodes[which(rownames(GlobalMetadata)=="19")] <- GlobalMetadata$WebmapperCodes[which(rownames(GlobalMetadata)=="297")]
GlobalMetadata$WebmapperStartYears[which(rownames(GlobalMetadata)=="19")] <- GlobalMetadata$WebmapperStartYears[which(rownames(GlobalMetadata)=="297")]
GlobalMetadata$WebmapperEndYears[which(rownames(GlobalMetadata)=="19")] <- GlobalMetadata$WebmapperEndYears[which(rownames(GlobalMetadata)=="297")]
GlobalMetadata <- subset(GlobalMetadata,!rownames(GlobalMetadata)=="297")

### XXX clio infra Virgin Islands correspond to the US or UK version or none?
### XXX are Cooks and Cook islands the same?

# merge 58 and 277 for Czechoslovakia
GlobalMetadata$`Webmapper code`[which(rownames(GlobalMetadata)=="58")] <- GlobalMetadata$`Webmapper code`[which(GlobalMetadata$ClioInfraCountryName=="Czechoslovakia")]
GlobalMetadata$`Webmapper numeric code`[which(rownames(GlobalMetadata)=="58")] <- GlobalMetadata$`Webmapper numeric code`[which(GlobalMetadata$ClioInfraCountryName=="Czechoslovakia")]
GlobalMetadata$WebmapperNums[which(rownames(GlobalMetadata)=="58")] <- GlobalMetadata$WebmapperNums[which(GlobalMetadata$ClioInfraCountryName=="Czechoslovakia")]
GlobalMetadata$WebmapperCodes[which(rownames(GlobalMetadata)=="58")] <- GlobalMetadata$WebmapperCodes[which(GlobalMetadata$ClioInfraCountryName=="Czechoslovakia")]
GlobalMetadata$WebmapperStartYears[which(rownames(GlobalMetadata)=="58")] <- GlobalMetadata$WebmapperStartYears[which(GlobalMetadata$ClioInfraCountryName=="Czechoslovakia")]
GlobalMetadata$WebmapperEndYears[which(rownames(GlobalMetadata)=="58")] <- GlobalMetadata$WebmapperEndYears[which(GlobalMetadata$ClioInfraCountryName=="Czechoslovakia")]
GlobalMetadata$DataPoints[which(rownames(GlobalMetadata)=="58")] <- GlobalMetadata$DataPoints[which(GlobalMetadata$ClioInfraCountryName=="Czechoslovakia")]
GlobalMetadata$ClioInfraCountryName[which(rownames(GlobalMetadata)=="58")] <- GlobalMetadata$ClioInfraCountryName[which(GlobalMetadata$ClioInfraCountryName=="Czechoslovakia")]
GlobalMetadata <- subset(GlobalMetadata,!rownames(GlobalMetadata)=="277")

### XXX how to treat Ethiopia until 1993? this is a fundamental question with the treatment of historical entities

# merge 72 and 331 for Falkland

GlobalMetadata$`Webmapper code`[which(rownames(GlobalMetadata)=="72")] <- GlobalMetadata$`Webmapper code`[which(GlobalMetadata$ClioInfraCountryName=="Falklands")]
GlobalMetadata$`Webmapper numeric code`[which(rownames(GlobalMetadata)=="72")] <- GlobalMetadata$`Webmapper numeric code`[which(GlobalMetadata$ClioInfraCountryName=="Falklands")]
GlobalMetadata$WebmapperNums[which(rownames(GlobalMetadata)=="72")] <- GlobalMetadata$WebmapperNums[which(GlobalMetadata$ClioInfraCountryName=="Falklands")]
GlobalMetadata$WebmapperCodes[which(rownames(GlobalMetadata)=="72")] <- GlobalMetadata$WebmapperCodes[which(GlobalMetadata$ClioInfraCountryName=="Falklands")]
GlobalMetadata$WebmapperStartYears[which(rownames(GlobalMetadata)=="72")] <- GlobalMetadata$WebmapperStartYears[which(GlobalMetadata$ClioInfraCountryName=="Falklands")]
GlobalMetadata$WebmapperEndYears[which(rownames(GlobalMetadata)=="72")] <- GlobalMetadata$WebmapperEndYears[which(GlobalMetadata$ClioInfraCountryName=="Falklands")]
GlobalMetadata$DataPoints[which(rownames(GlobalMetadata)=="72")] <- GlobalMetadata$DataPoints[which(GlobalMetadata$ClioInfraCountryName=="Falklands")]
GlobalMetadata$ClioInfraCountryName[which(rownames(GlobalMetadata)=="72")] <- GlobalMetadata$ClioInfraCountryName[which(GlobalMetadata$ClioInfraCountryName=="Falklands")]
GlobalMetadata <- subset(GlobalMetadata,!rownames(GlobalMetadata)=="331")

# are 73 South Georgia and the South Sandwich Islands and 477 S Georgia the same

# what is the "Polynesia" entity on clioinfradata?

# 85 and 439 for Palestine, 
# XXX but the dates end in 1945 and I need to provide additional ones
# also use PSE or WBG? or both?

GlobalMetadata$`Webmapper code`[which(rownames(GlobalMetadata)=="85")] <- GlobalMetadata$`Webmapper code`[which(rownames(GlobalMetadata)=="439")]
GlobalMetadata$`Webmapper numeric code`[which(rownames(GlobalMetadata)=="85")] <- GlobalMetadata$`Webmapper numeric code`[which(rownames(GlobalMetadata)=="439")]
GlobalMetadata$ClioInfraCountryName[which(rownames(GlobalMetadata)=="85")] <- GlobalMetadata$ClioInfraCountryName[which(rownames(GlobalMetadata)=="439")]
GlobalMetadata$WebmapperNums[which(rownames(GlobalMetadata)=="85")] <- GlobalMetadata$WebmapperNums[which(rownames(GlobalMetadata)=="439")]
GlobalMetadata$WebmapperCodes[which(rownames(GlobalMetadata)=="85")] <- GlobalMetadata$WebmapperCodes[which(rownames(GlobalMetadata)=="439")]
GlobalMetadata$WebmapperStartYears[which(rownames(GlobalMetadata)=="85")] <- GlobalMetadata$WebmapperStartYears[which(rownames(GlobalMetadata)=="439")]
GlobalMetadata$WebmapperEndYears[which(rownames(GlobalMetadata)=="85")] <- GlobalMetadata$WebmapperEndYears[which(rownames(GlobalMetadata)=="439")]
GlobalMetadata <- subset(GlobalMetadata,!rownames(GlobalMetadata)=="439")

# now germany...
# 87 with 544 east germany:
GlobalMetadata$`Webmapper code`[which(rownames(GlobalMetadata)=="87")] <- GlobalMetadata$`Webmapper code`[which(rownames(GlobalMetadata)=="544")]
GlobalMetadata$`Webmapper numeric code`[which(rownames(GlobalMetadata)=="87")] <- GlobalMetadata$`Webmapper numeric code`[which(rownames(GlobalMetadata)=="544")]
GlobalMetadata$ClioInfraCountryName[which(rownames(GlobalMetadata)=="87")] <- GlobalMetadata$ClioInfraCountryName[which(rownames(GlobalMetadata)=="544")]
GlobalMetadata$WebmapperNums[which(rownames(GlobalMetadata)=="87")] <- GlobalMetadata$WebmapperNums[which(rownames(GlobalMetadata)=="544")]
GlobalMetadata$WebmapperCodes[which(rownames(GlobalMetadata)=="87")] <- GlobalMetadata$WebmapperCodes[which(rownames(GlobalMetadata)=="544")]
GlobalMetadata$WebmapperStartYears[which(rownames(GlobalMetadata)=="87")] <- GlobalMetadata$WebmapperStartYears[which(rownames(GlobalMetadata)=="544")]
GlobalMetadata$WebmapperEndYears[which(rownames(GlobalMetadata)=="87")] <- GlobalMetadata$WebmapperEndYears[which(rownames(GlobalMetadata)=="544")]
GlobalMetadata <- subset(GlobalMetadata,!rownames(GlobalMetadata)=="544")

# 88 with 499 east germany:
GlobalMetadata$`Webmapper code`[which(rownames(GlobalMetadata)=="88")] <- GlobalMetadata$`Webmapper code`[which(rownames(GlobalMetadata)=="499")]
GlobalMetadata$`Webmapper numeric code`[which(rownames(GlobalMetadata)=="88")] <- GlobalMetadata$`Webmapper numeric code`[which(rownames(GlobalMetadata)=="499")]
GlobalMetadata$ClioInfraCountryName[which(rownames(GlobalMetadata)=="88")] <- GlobalMetadata$ClioInfraCountryName[which(rownames(GlobalMetadata)=="499")]
GlobalMetadata$WebmapperNums[which(rownames(GlobalMetadata)=="88")] <- GlobalMetadata$WebmapperNums[which(rownames(GlobalMetadata)=="499")]
GlobalMetadata$WebmapperCodes[which(rownames(GlobalMetadata)=="88")] <- GlobalMetadata$WebmapperCodes[which(rownames(GlobalMetadata)=="499")]
GlobalMetadata$WebmapperStartYears[which(rownames(GlobalMetadata)=="88")] <- GlobalMetadata$WebmapperStartYears[which(rownames(GlobalMetadata)=="499")]
GlobalMetadata$WebmapperEndYears[which(rownames(GlobalMetadata)=="88")] <- GlobalMetadata$WebmapperEndYears[which(rownames(GlobalMetadata)=="499")]
GlobalMetadata <- subset(GlobalMetadata,!rownames(GlobalMetadata)=="499")

# XXX check what West Germany corresponds to on "geacron/1139", and "geacron/388" German Confederation

# Gibraltar, XXX needs expansion to present mapping, 90/339
GlobalMetadata$`Webmapper code`[which(rownames(GlobalMetadata)=="90")] <- GlobalMetadata$`Webmapper code`[which(rownames(GlobalMetadata)=="339")]
GlobalMetadata$`Webmapper numeric code`[which(rownames(GlobalMetadata)=="90")] <- GlobalMetadata$`Webmapper numeric code`[which(rownames(GlobalMetadata)=="339")]
GlobalMetadata$ClioInfraCountryName[which(rownames(GlobalMetadata)=="90")] <- GlobalMetadata$ClioInfraCountryName[which(rownames(GlobalMetadata)=="339")]
GlobalMetadata$WebmapperNums[which(rownames(GlobalMetadata)=="90")] <- GlobalMetadata$WebmapperNums[which(rownames(GlobalMetadata)=="339")]
GlobalMetadata$WebmapperCodes[which(rownames(GlobalMetadata)=="90")] <- GlobalMetadata$WebmapperCodes[which(rownames(GlobalMetadata)=="339")]
GlobalMetadata$WebmapperStartYears[which(rownames(GlobalMetadata)=="90")] <- GlobalMetadata$WebmapperStartYears[which(rownames(GlobalMetadata)=="339")]
GlobalMetadata$WebmapperEndYears[which(rownames(GlobalMetadata)=="90")] <- GlobalMetadata$WebmapperEndYears[which(rownames(GlobalMetadata)=="339")]
GlobalMetadata <- subset(GlobalMetadata,!rownames(GlobalMetadata)=="339")

# Greenland, XXX needs expansion to present mapping, 93/345
GlobalMetadata$`Webmapper code`[which(rownames(GlobalMetadata)=="93")] <- GlobalMetadata$`Webmapper code`[which(rownames(GlobalMetadata)=="345")]
GlobalMetadata$`Webmapper numeric code`[which(rownames(GlobalMetadata)=="93")] <- GlobalMetadata$`Webmapper numeric code`[which(rownames(GlobalMetadata)=="345")]
GlobalMetadata$ClioInfraCountryName[which(rownames(GlobalMetadata)=="93")] <- GlobalMetadata$ClioInfraCountryName[which(rownames(GlobalMetadata)=="345")]
GlobalMetadata$WebmapperNums[which(rownames(GlobalMetadata)=="93")] <- GlobalMetadata$WebmapperNums[which(rownames(GlobalMetadata)=="345")]
GlobalMetadata$WebmapperCodes[which(rownames(GlobalMetadata)=="93")] <- GlobalMetadata$WebmapperCodes[which(rownames(GlobalMetadata)=="345")]
GlobalMetadata$WebmapperStartYears[which(rownames(GlobalMetadata)=="93")] <- GlobalMetadata$WebmapperStartYears[which(rownames(GlobalMetadata)=="345")]
GlobalMetadata$WebmapperEndYears[which(rownames(GlobalMetadata)=="93")] <- GlobalMetadata$WebmapperEndYears[which(rownames(GlobalMetadata)=="345")]
GlobalMetadata <- subset(GlobalMetadata,!rownames(GlobalMetadata)=="345")

# Guadeloupe, XXX needs expansion to present mapping, 95/346
GlobalMetadata$`Webmapper code`[which(rownames(GlobalMetadata)=="95")] <- GlobalMetadata$`Webmapper code`[which(rownames(GlobalMetadata)=="346")]
GlobalMetadata$`Webmapper numeric code`[which(rownames(GlobalMetadata)=="95")] <- GlobalMetadata$`Webmapper numeric code`[which(rownames(GlobalMetadata)=="346")]
GlobalMetadata$ClioInfraCountryName[which(rownames(GlobalMetadata)=="95")] <- GlobalMetadata$ClioInfraCountryName[which(rownames(GlobalMetadata)=="346")]
GlobalMetadata$WebmapperNums[which(rownames(GlobalMetadata)=="95")] <- GlobalMetadata$WebmapperNums[which(rownames(GlobalMetadata)=="346")]
GlobalMetadata$WebmapperCodes[which(rownames(GlobalMetadata)=="95")] <- GlobalMetadata$WebmapperCodes[which(rownames(GlobalMetadata)=="346")]
GlobalMetadata$WebmapperStartYears[which(rownames(GlobalMetadata)=="95")] <- GlobalMetadata$WebmapperStartYears[which(rownames(GlobalMetadata)=="346")]
GlobalMetadata$WebmapperEndYears[which(rownames(GlobalMetadata)=="95")] <- GlobalMetadata$WebmapperEndYears[which(rownames(GlobalMetadata)=="346")]
GlobalMetadata <- subset(GlobalMetadata,!rownames(GlobalMetadata)=="346")

# Guam, XXX needs expansion to present mapping, 96/347
GlobalMetadata$`Webmapper code`[which(rownames(GlobalMetadata)=="96")] <- GlobalMetadata$`Webmapper code`[which(rownames(GlobalMetadata)=="347")]
GlobalMetadata$`Webmapper numeric code`[which(rownames(GlobalMetadata)=="96")] <- GlobalMetadata$`Webmapper numeric code`[which(rownames(GlobalMetadata)=="347")]
GlobalMetadata$ClioInfraCountryName[which(rownames(GlobalMetadata)=="96")] <- GlobalMetadata$ClioInfraCountryName[which(rownames(GlobalMetadata)=="347")]
GlobalMetadata$WebmapperNums[which(rownames(GlobalMetadata)=="96")] <- GlobalMetadata$WebmapperNums[which(rownames(GlobalMetadata)=="347")]
GlobalMetadata$WebmapperCodes[which(rownames(GlobalMetadata)=="96")] <- GlobalMetadata$WebmapperCodes[which(rownames(GlobalMetadata)=="347")]
GlobalMetadata$WebmapperStartYears[which(rownames(GlobalMetadata)=="96")] <- GlobalMetadata$WebmapperStartYears[which(rownames(GlobalMetadata)=="347")]
GlobalMetadata$WebmapperEndYears[which(rownames(GlobalMetadata)=="96")] <- GlobalMetadata$WebmapperEndYears[which(rownames(GlobalMetadata)=="347")]
GlobalMetadata <- subset(GlobalMetadata,!rownames(GlobalMetadata)=="347")

# Hong Kong, XXX needs expansion to present mapping, 104/357
GlobalMetadata$`Webmapper code`[which(rownames(GlobalMetadata)=="104")] <- GlobalMetadata$`Webmapper code`[which(rownames(GlobalMetadata)=="357")]
GlobalMetadata$`Webmapper numeric code`[which(rownames(GlobalMetadata)=="104")] <- GlobalMetadata$`Webmapper numeric code`[which(rownames(GlobalMetadata)=="357")]
GlobalMetadata$ClioInfraCountryName[which(rownames(GlobalMetadata)=="104")] <- GlobalMetadata$ClioInfraCountryName[which(rownames(GlobalMetadata)=="357")]
GlobalMetadata$WebmapperNums[which(rownames(GlobalMetadata)=="104")] <- GlobalMetadata$WebmapperNums[which(rownames(GlobalMetadata)=="357")]
GlobalMetadata$WebmapperCodes[which(rownames(GlobalMetadata)=="104")] <- GlobalMetadata$WebmapperCodes[which(rownames(GlobalMetadata)=="357")]
GlobalMetadata$WebmapperStartYears[which(rownames(GlobalMetadata)=="104")] <- GlobalMetadata$WebmapperStartYears[which(rownames(GlobalMetadata)=="357")]
GlobalMetadata$WebmapperEndYears[which(rownames(GlobalMetadata)=="104")] <- GlobalMetadata$WebmapperEndYears[which(rownames(GlobalMetadata)=="357")]
GlobalMetadata$DataPoints[which(rownames(GlobalMetadata)=="104")] <- GlobalMetadata$DataPoints[which(rownames(GlobalMetadata)=="357")]
GlobalMetadata <- subset(GlobalMetadata,!rownames(GlobalMetadata)=="357")

# Martinique, XXX needs expansion to present mapping, 140/404
GlobalMetadata$`Webmapper code`[which(rownames(GlobalMetadata)=="140")] <- GlobalMetadata$`Webmapper code`[which(rownames(GlobalMetadata)=="404")]
GlobalMetadata$`Webmapper numeric code`[which(rownames(GlobalMetadata)=="140")] <- GlobalMetadata$`Webmapper numeric code`[which(rownames(GlobalMetadata)=="404")]
GlobalMetadata$ClioInfraCountryName[which(rownames(GlobalMetadata)=="140")] <- GlobalMetadata$ClioInfraCountryName[which(rownames(GlobalMetadata)=="404")]
GlobalMetadata$WebmapperNums[which(rownames(GlobalMetadata)=="140")] <- GlobalMetadata$WebmapperNums[which(rownames(GlobalMetadata)=="404")]
GlobalMetadata$WebmapperCodes[which(rownames(GlobalMetadata)=="140")] <- GlobalMetadata$WebmapperCodes[which(rownames(GlobalMetadata)=="404")]
GlobalMetadata$WebmapperStartYears[which(rownames(GlobalMetadata)=="140")] <- GlobalMetadata$WebmapperStartYears[which(rownames(GlobalMetadata)=="404")]
GlobalMetadata$WebmapperEndYears[which(rownames(GlobalMetadata)=="140")] <- GlobalMetadata$WebmapperEndYears[which(rownames(GlobalMetadata)=="404")]
GlobalMetadata$DataPoints[which(rownames(GlobalMetadata)=="140")] <- GlobalMetadata$DataPoints[which(rownames(GlobalMetadata)=="404")]
GlobalMetadata <- subset(GlobalMetadata,!rownames(GlobalMetadata)=="404")

# Aruba, XXX needs expansion to present mapping, 158/281
GlobalMetadata$`Webmapper code`[which(rownames(GlobalMetadata)=="158")] <- GlobalMetadata$`Webmapper code`[which(rownames(GlobalMetadata)=="281")]
GlobalMetadata$`Webmapper numeric code`[which(rownames(GlobalMetadata)=="158")] <- GlobalMetadata$`Webmapper numeric code`[which(rownames(GlobalMetadata)=="281")]
GlobalMetadata$ClioInfraCountryName[which(rownames(GlobalMetadata)=="158")] <- GlobalMetadata$ClioInfraCountryName[which(rownames(GlobalMetadata)=="281")]
GlobalMetadata$WebmapperNums[which(rownames(GlobalMetadata)=="158")] <- GlobalMetadata$WebmapperNums[which(rownames(GlobalMetadata)=="281")]
GlobalMetadata$WebmapperCodes[which(rownames(GlobalMetadata)=="158")] <- GlobalMetadata$WebmapperCodes[which(rownames(GlobalMetadata)=="281")]
GlobalMetadata$WebmapperStartYears[which(rownames(GlobalMetadata)=="158")] <- GlobalMetadata$WebmapperStartYears[which(rownames(GlobalMetadata)=="281")]
GlobalMetadata$WebmapperEndYears[which(rownames(GlobalMetadata)=="158")] <- GlobalMetadata$WebmapperEndYears[which(rownames(GlobalMetadata)=="281")]
GlobalMetadata$DataPoints[which(rownames(GlobalMetadata)=="158")] <- GlobalMetadata$DataPoints[which(rownames(GlobalMetadata)=="281")]
GlobalMetadata <- subset(GlobalMetadata,!rownames(GlobalMetadata)=="281")

# New Caledonia, XXX needs expansion to present mapping, 161/425
GlobalMetadata$`Webmapper code`[which(rownames(GlobalMetadata)=="161")] <- GlobalMetadata$`Webmapper code`[which(rownames(GlobalMetadata)=="425")]
GlobalMetadata$`Webmapper numeric code`[which(rownames(GlobalMetadata)=="161")] <- GlobalMetadata$`Webmapper numeric code`[which(rownames(GlobalMetadata)=="425")]
GlobalMetadata$ClioInfraCountryName[which(rownames(GlobalMetadata)=="161")] <- GlobalMetadata$ClioInfraCountryName[which(rownames(GlobalMetadata)=="425")]
GlobalMetadata$WebmapperNums[which(rownames(GlobalMetadata)=="161")] <- GlobalMetadata$WebmapperNums[which(rownames(GlobalMetadata)=="425")]
GlobalMetadata$WebmapperCodes[which(rownames(GlobalMetadata)=="161")] <- GlobalMetadata$WebmapperCodes[which(rownames(GlobalMetadata)=="425")]
GlobalMetadata$WebmapperStartYears[which(rownames(GlobalMetadata)=="161")] <- GlobalMetadata$WebmapperStartYears[which(rownames(GlobalMetadata)=="425")]
GlobalMetadata$WebmapperEndYears[which(rownames(GlobalMetadata)=="161")] <- GlobalMetadata$WebmapperEndYears[which(rownames(GlobalMetadata)=="425")]
GlobalMetadata$DataPoints[which(rownames(GlobalMetadata)=="161")] <- GlobalMetadata$DataPoints[which(rownames(GlobalMetadata)=="425")]
GlobalMetadata <- subset(GlobalMetadata,!rownames(GlobalMetadata)=="425")

# I left Marianas as they where XXX

# Puerto Rico, XXX needs expansion to present mapping, 187/549
GlobalMetadata$`Webmapper code`[which(rownames(GlobalMetadata)=="187")] <- GlobalMetadata$`Webmapper code`[which(rownames(GlobalMetadata)=="549")]
GlobalMetadata$`Webmapper numeric code`[which(rownames(GlobalMetadata)=="187")] <- GlobalMetadata$`Webmapper numeric code`[which(rownames(GlobalMetadata)=="549")]
GlobalMetadata$ClioInfraCountryName[which(rownames(GlobalMetadata)=="187")] <- GlobalMetadata$ClioInfraCountryName[which(rownames(GlobalMetadata)=="549")]
GlobalMetadata$WebmapperNums[which(rownames(GlobalMetadata)=="187")] <- GlobalMetadata$WebmapperNums[which(rownames(GlobalMetadata)=="549")]
GlobalMetadata$WebmapperCodes[which(rownames(GlobalMetadata)=="187")] <- GlobalMetadata$WebmapperCodes[which(rownames(GlobalMetadata)=="549")]
GlobalMetadata$WebmapperStartYears[which(rownames(GlobalMetadata)=="187")] <- GlobalMetadata$WebmapperStartYears[which(rownames(GlobalMetadata)=="549")]
GlobalMetadata$WebmapperEndYears[which(rownames(GlobalMetadata)=="187")] <- GlobalMetadata$WebmapperEndYears[which(rownames(GlobalMetadata)=="549")]
GlobalMetadata$DataPoints[which(rownames(GlobalMetadata)=="187")] <- GlobalMetadata$DataPoints[which(rownames(GlobalMetadata)=="549")]
GlobalMetadata <- subset(GlobalMetadata,!rownames(GlobalMetadata)=="549")

# Yemen: first merge: 216/503 for South Yemen
GlobalMetadata$`Webmapper code`[which(rownames(GlobalMetadata)=="216")] <- GlobalMetadata$`Webmapper code`[which(rownames(GlobalMetadata)=="503")]
GlobalMetadata$`Webmapper numeric code`[which(rownames(GlobalMetadata)=="216")] <- GlobalMetadata$`Webmapper numeric code`[which(rownames(GlobalMetadata)=="503")]
GlobalMetadata$ClioInfraCountryName[which(rownames(GlobalMetadata)=="216")] <- GlobalMetadata$ClioInfraCountryName[which(rownames(GlobalMetadata)=="503")]
GlobalMetadata$WebmapperNums[which(rownames(GlobalMetadata)=="216")] <- GlobalMetadata$WebmapperNums[which(rownames(GlobalMetadata)=="503")]
GlobalMetadata$WebmapperCodes[which(rownames(GlobalMetadata)=="216")] <- GlobalMetadata$WebmapperCodes[which(rownames(GlobalMetadata)=="503")]
GlobalMetadata$WebmapperStartYears[which(rownames(GlobalMetadata)=="216")] <- GlobalMetadata$WebmapperStartYears[which(rownames(GlobalMetadata)=="503")]
GlobalMetadata$WebmapperEndYears[which(rownames(GlobalMetadata)=="216")] <- GlobalMetadata$WebmapperEndYears[which(rownames(GlobalMetadata)=="503")]
GlobalMetadata$DataPoints[which(rownames(GlobalMetadata)=="216")] <- GlobalMetadata$DataPoints[which(rownames(GlobalMetadata)=="503")]
GlobalMetadata <- subset(GlobalMetadata,!rownames(GlobalMetadata)=="503")

# Yemen: second merge: 259/543 for North Yemen
GlobalMetadata$`Webmapper code`[which(rownames(GlobalMetadata)=="259")] <- GlobalMetadata$`Webmapper code`[which(rownames(GlobalMetadata)=="543")]
GlobalMetadata$`Webmapper numeric code`[which(rownames(GlobalMetadata)=="259")] <- GlobalMetadata$`Webmapper numeric code`[which(rownames(GlobalMetadata)=="543")]
GlobalMetadata$ClioInfraCountryName[which(rownames(GlobalMetadata)=="259")] <- GlobalMetadata$ClioInfraCountryName[which(rownames(GlobalMetadata)=="543")]
GlobalMetadata$WebmapperNums[which(rownames(GlobalMetadata)=="259")] <- GlobalMetadata$WebmapperNums[which(rownames(GlobalMetadata)=="543")]
GlobalMetadata$WebmapperCodes[which(rownames(GlobalMetadata)=="259")] <- GlobalMetadata$WebmapperCodes[which(rownames(GlobalMetadata)=="543")]
GlobalMetadata$WebmapperStartYears[which(rownames(GlobalMetadata)=="259")] <- GlobalMetadata$WebmapperStartYears[which(rownames(GlobalMetadata)=="543")]
GlobalMetadata$WebmapperEndYears[which(rownames(GlobalMetadata)=="259")] <- GlobalMetadata$WebmapperEndYears[which(rownames(GlobalMetadata)=="543")]
GlobalMetadata$DataPoints[which(rownames(GlobalMetadata)=="259")] <- GlobalMetadata$DataPoints[which(rownames(GlobalMetadata)=="543")]
GlobalMetadata <- subset(GlobalMetadata,!rownames(GlobalMetadata)=="543")

# and to both 216 and 543 give the geo regions of present day Yemen 260
GlobalMetadata$region[which(rownames(GlobalMetadata)=="216")] <- GlobalMetadata$region[which(rownames(GlobalMetadata)=="260")]
GlobalMetadata$region_numeric[which(rownames(GlobalMetadata)=="216")] <- GlobalMetadata$region_numeric[which(rownames(GlobalMetadata)=="260")]
GlobalMetadata$subregion[which(rownames(GlobalMetadata)=="216")] <- GlobalMetadata$subregion[which(rownames(GlobalMetadata)=="260")]
GlobalMetadata$subregion_numeric[which(rownames(GlobalMetadata)=="216")] <- GlobalMetadata$subregion_numeric[which(rownames(GlobalMetadata)=="260")]

GlobalMetadata$region[which(rownames(GlobalMetadata)=="259")] <- GlobalMetadata$region[which(rownames(GlobalMetadata)=="260")]
GlobalMetadata$region_numeric[which(rownames(GlobalMetadata)=="259")] <- GlobalMetadata$region_numeric[which(rownames(GlobalMetadata)=="260")]
GlobalMetadata$subregion[which(rownames(GlobalMetadata)=="259")] <- GlobalMetadata$subregion[which(rownames(GlobalMetadata)=="260")]
GlobalMetadata$subregion_numeric[which(rownames(GlobalMetadata)=="259")] <- GlobalMetadata$subregion_numeric[which(rownames(GlobalMetadata)=="260")]

# Sudan split:
GlobalMetadata$PredecessorNumeric[which(rownames(GlobalMetadata)=="218")] <- GlobalMetadata$numeric[which(rownames(GlobalMetadata)=="221")]
GlobalMetadata$PredecessorNumeric[which(rownames(GlobalMetadata)=="219")] <- GlobalMetadata$PredecessorNumeric[which(rownames(GlobalMetadata)=="218")]
GlobalMetadata <- subset(GlobalMetadata,!rownames(GlobalMetadata)=="221")

# Western Sahara: 220/527 XXX borders update
GlobalMetadata$`Webmapper code`[which(rownames(GlobalMetadata)=="220")] <- GlobalMetadata$`Webmapper code`[which(rownames(GlobalMetadata)=="527")]
GlobalMetadata$`Webmapper numeric code`[which(rownames(GlobalMetadata)=="220")] <- GlobalMetadata$`Webmapper numeric code`[which(rownames(GlobalMetadata)=="527")]
GlobalMetadata$ClioInfraCountryName[which(rownames(GlobalMetadata)=="220")] <- GlobalMetadata$ClioInfraCountryName[which(rownames(GlobalMetadata)=="527")]
GlobalMetadata$WebmapperNums[which(rownames(GlobalMetadata)=="220")] <- GlobalMetadata$WebmapperNums[which(rownames(GlobalMetadata)=="527")]
GlobalMetadata$WebmapperCodes[which(rownames(GlobalMetadata)=="220")] <- GlobalMetadata$WebmapperCodes[which(rownames(GlobalMetadata)=="527")]
GlobalMetadata$WebmapperStartYears[which(rownames(GlobalMetadata)=="220")] <- GlobalMetadata$WebmapperStartYears[which(rownames(GlobalMetadata)=="527")]
GlobalMetadata$WebmapperEndYears[which(rownames(GlobalMetadata)=="220")] <- GlobalMetadata$WebmapperEndYears[which(rownames(GlobalMetadata)=="527")]
GlobalMetadata$DataPoints[which(rownames(GlobalMetadata)=="220")] <- GlobalMetadata$DataPoints[which(rownames(GlobalMetadata)=="527")]
GlobalMetadata <- subset(GlobalMetadata,!rownames(GlobalMetadata)=="527")

# USSR: 243/271
GlobalMetadata$`Webmapper code`[which(rownames(GlobalMetadata)=="243")] <- GlobalMetadata$`Webmapper code`[which(rownames(GlobalMetadata)=="271")]
GlobalMetadata$`Webmapper numeric code`[which(rownames(GlobalMetadata)=="243")] <- GlobalMetadata$`Webmapper numeric code`[which(rownames(GlobalMetadata)=="271")]
GlobalMetadata$ClioInfraCountryName[which(rownames(GlobalMetadata)=="243")] <- GlobalMetadata$ClioInfraCountryName[which(rownames(GlobalMetadata)=="271")]
GlobalMetadata$WebmapperNums[which(rownames(GlobalMetadata)=="243")] <- GlobalMetadata$WebmapperNums[which(rownames(GlobalMetadata)=="271")]
GlobalMetadata$WebmapperCodes[which(rownames(GlobalMetadata)=="243")] <- GlobalMetadata$WebmapperCodes[which(rownames(GlobalMetadata)=="271")]
GlobalMetadata$WebmapperStartYears[which(rownames(GlobalMetadata)=="243")] <- GlobalMetadata$WebmapperStartYears[which(rownames(GlobalMetadata)=="271")]
GlobalMetadata$WebmapperEndYears[which(rownames(GlobalMetadata)=="243")] <- GlobalMetadata$WebmapperEndYears[which(rownames(GlobalMetadata)=="271")]
GlobalMetadata$DataPoints[which(rownames(GlobalMetadata)=="243")] <- GlobalMetadata$DataPoints[which(rownames(GlobalMetadata)=="271")]
GlobalMetadata <- subset(GlobalMetadata,!rownames(GlobalMetadata)=="271")

GlobalMetadata$`Alpha-2 code`[which(rownames(GlobalMetadata)=="243")] <- "SU"
GlobalMetadata$`Alpha-3 code`[which(rownames(GlobalMetadata)=="243")] <- "SUN"
GlobalMetadata$`English short name`[which(rownames(GlobalMetadata)=="243")] <- "Soviet Union"

# USSR with Soviet Union: 243/490
# here I need to merge the available webmappers in all columns keeping the 490;243 order
GlobalMetadata$`Webmapper code`[which(rownames(GlobalMetadata)=="243")] <- paste(GlobalMetadata$`Webmapper code`[which(rownames(GlobalMetadata)=="490")],GlobalMetadata$`Webmapper code`[which(rownames(GlobalMetadata)=="243")],sep=";",collapse = ";")
GlobalMetadata$`Webmapper numeric code`[which(rownames(GlobalMetadata)=="243")] <- paste(GlobalMetadata$`Webmapper numeric code`[which(rownames(GlobalMetadata)=="490")],GlobalMetadata$`Webmapper numeric code`[which(rownames(GlobalMetadata)=="243")],sep=";",collapse = ";")
GlobalMetadata$WebmapperNums[which(rownames(GlobalMetadata)=="243")] <- paste(GlobalMetadata$WebmapperNums[which(rownames(GlobalMetadata)=="490")],GlobalMetadata$WebmapperNums[which(rownames(GlobalMetadata)=="243")],sep=";",collapse = ";")
GlobalMetadata$WebmapperCodes[which(rownames(GlobalMetadata)=="243")] <- paste(GlobalMetadata$WebmapperCodes[which(rownames(GlobalMetadata)=="490")],GlobalMetadata$WebmapperCodes[which(rownames(GlobalMetadata)=="243")],sep=";",collapse = ";")
GlobalMetadata$WebmapperStartYears[which(rownames(GlobalMetadata)=="243")] <- paste(GlobalMetadata$WebmapperStartYears[which(rownames(GlobalMetadata)=="490")],GlobalMetadata$WebmapperStartYears[which(rownames(GlobalMetadata)=="243")],sep=";",collapse = ";")
GlobalMetadata$WebmapperEndYears[which(rownames(GlobalMetadata)=="243")] <- paste(GlobalMetadata$WebmapperEndYears[which(rownames(GlobalMetadata)=="490")],GlobalMetadata$WebmapperEndYears[which(rownames(GlobalMetadata)=="243")],sep=";",collapse = ";")
GlobalMetadata <- subset(GlobalMetadata,!rownames(GlobalMetadata)=="490")

# Yugoslavia: 262/547
GlobalMetadata$`Webmapper code`[which(rownames(GlobalMetadata)=="262")] <- GlobalMetadata$`Webmapper code`[which(rownames(GlobalMetadata)=="547")]
GlobalMetadata$`Webmapper numeric code`[which(rownames(GlobalMetadata)=="262")] <- GlobalMetadata$`Webmapper numeric code`[which(rownames(GlobalMetadata)=="547")]
GlobalMetadata$ClioInfraCountryName[which(rownames(GlobalMetadata)=="262")] <- GlobalMetadata$ClioInfraCountryName[which(rownames(GlobalMetadata)=="547")]
GlobalMetadata$WebmapperNums[which(rownames(GlobalMetadata)=="262")] <- GlobalMetadata$WebmapperNums[which(rownames(GlobalMetadata)=="547")]
GlobalMetadata$WebmapperCodes[which(rownames(GlobalMetadata)=="262")] <- GlobalMetadata$WebmapperCodes[which(rownames(GlobalMetadata)=="547")]
GlobalMetadata$WebmapperStartYears[which(rownames(GlobalMetadata)=="262")] <- GlobalMetadata$WebmapperStartYears[which(rownames(GlobalMetadata)=="547")]
GlobalMetadata$WebmapperEndYears[which(rownames(GlobalMetadata)=="262")] <- GlobalMetadata$WebmapperEndYears[which(rownames(GlobalMetadata)=="547")]
GlobalMetadata$DataPoints[which(rownames(GlobalMetadata)=="262")] <- GlobalMetadata$DataPoints[which(rownames(GlobalMetadata)=="547")]
GlobalMetadata <- subset(GlobalMetadata,!rownames(GlobalMetadata)=="547")

#### Now to the remaining entities without geo regions assign them manually ####
# since those countries are about half of the dataframe I will first treat the ones that have data:
TempCountries <- subset(GlobalMetadata,is.na(GlobalMetadata$region) & GlobalMetadata$DataPoints>0)

GlobalMetadata$region[which(GlobalMetadata$ClioInfraCountryName=="England")] <- GlobalMetadata$region[which(GlobalMetadata$OECD_Country_Name=="United Kingdom of Great Britain and Northern Ireland")]
GlobalMetadata$region_numeric[which(GlobalMetadata$ClioInfraCountryName=="England")] <- GlobalMetadata$region_numeric[which(GlobalMetadata$OECD_Country_Name=="United Kingdom of Great Britain and Northern Ireland")]
GlobalMetadata$subregion[which(GlobalMetadata$ClioInfraCountryName=="England")] <- GlobalMetadata$subregion[which(GlobalMetadata$OECD_Country_Name=="United Kingdom of Great Britain and Northern Ireland")]
GlobalMetadata$subregion_numeric[which(GlobalMetadata$ClioInfraCountryName=="England")] <- GlobalMetadata$subregion_numeric[which(GlobalMetadata$OECD_Country_Name=="United Kingdom of Great Britain and Northern Ireland")]
GlobalMetadata$subsubregion[which(GlobalMetadata$ClioInfraCountryName=="England")] <- GlobalMetadata$subsubregion[which(GlobalMetadata$OECD_Country_Name=="United Kingdom of Great Britain and Northern Ireland")]
GlobalMetadata$subsubregion_numeric[which(GlobalMetadata$ClioInfraCountryName=="England")] <- GlobalMetadata$subsubregion_numeric[which(GlobalMetadata$OECD_Country_Name=="United Kingdom of Great Britain and Northern Ireland")]
GlobalMetadata$OECD_Region[which(GlobalMetadata$ClioInfraCountryName=="England")] <- GlobalMetadata$OECD_Region[which(GlobalMetadata$OECD_Country_Name=="United Kingdom of Great Britain and Northern Ireland")]

TargetCountry <- "Egypt (United Arab Republic)"
OriginCountry <- "Egypt"

GlobalMetadata$region[which(GlobalMetadata$ClioInfraCountryName==TargetCountry)] <- GlobalMetadata$region[which(GlobalMetadata$OECD_Country_Name==OriginCountry)]
GlobalMetadata$region_numeric[which(GlobalMetadata$ClioInfraCountryName==TargetCountry)] <- GlobalMetadata$region_numeric[which(GlobalMetadata$OECD_Country_Name==OriginCountry)]
GlobalMetadata$subregion[which(GlobalMetadata$ClioInfraCountryName==TargetCountry)] <- GlobalMetadata$subregion[which(GlobalMetadata$OECD_Country_Name==OriginCountry)]
GlobalMetadata$subregion_numeric[which(GlobalMetadata$ClioInfraCountryName==TargetCountry)] <- GlobalMetadata$subregion_numeric[which(GlobalMetadata$OECD_Country_Name==OriginCountry)]
GlobalMetadata$subsubregion[which(GlobalMetadata$ClioInfraCountryName==TargetCountry)] <- GlobalMetadata$subsubregion[which(GlobalMetadata$OECD_Country_Name==OriginCountry)]
GlobalMetadata$subsubregion_numeric[which(GlobalMetadata$ClioInfraCountryName==TargetCountry)] <- GlobalMetadata$subsubregion_numeric[which(GlobalMetadata$OECD_Country_Name==OriginCountry)]
GlobalMetadata$OECD_Region[which(GlobalMetadata$ClioInfraCountryName==TargetCountry)] <- GlobalMetadata$OECD_Region[which(GlobalMetadata$OECD_Country_Name==OriginCountry)]


TargetCountry <- "Ceylon"
OriginCountry <- "India"

GlobalMetadata$region[which(GlobalMetadata$ClioInfraCountryName==TargetCountry)] <- GlobalMetadata$region[which(GlobalMetadata$OECD_Country_Name==OriginCountry)]
GlobalMetadata$region_numeric[which(GlobalMetadata$ClioInfraCountryName==TargetCountry)] <- GlobalMetadata$region_numeric[which(GlobalMetadata$OECD_Country_Name==OriginCountry)]
GlobalMetadata$subregion[which(GlobalMetadata$ClioInfraCountryName==TargetCountry)] <- GlobalMetadata$subregion[which(GlobalMetadata$OECD_Country_Name==OriginCountry)]
GlobalMetadata$subregion_numeric[which(GlobalMetadata$ClioInfraCountryName==TargetCountry)] <- GlobalMetadata$subregion_numeric[which(GlobalMetadata$OECD_Country_Name==OriginCountry)]
GlobalMetadata$subsubregion[which(GlobalMetadata$ClioInfraCountryName==TargetCountry)] <- GlobalMetadata$subsubregion[which(GlobalMetadata$OECD_Country_Name==OriginCountry)]
GlobalMetadata$subsubregion_numeric[which(GlobalMetadata$ClioInfraCountryName==TargetCountry)] <- GlobalMetadata$subsubregion_numeric[which(GlobalMetadata$OECD_Country_Name==OriginCountry)]
GlobalMetadata$OECD_Region[which(GlobalMetadata$ClioInfraCountryName==TargetCountry)] <- GlobalMetadata$OECD_Region[which(GlobalMetadata$OECD_Country_Name==OriginCountry)]

TargetCountry <- "Taiwan"
OriginCountry <- "China"

GlobalMetadata$region[which(GlobalMetadata$ClioInfraCountryName==TargetCountry)] <- GlobalMetadata$region[which(GlobalMetadata$OECD_Country_Name==OriginCountry)]
GlobalMetadata$region_numeric[which(GlobalMetadata$ClioInfraCountryName==TargetCountry)] <- GlobalMetadata$region_numeric[which(GlobalMetadata$OECD_Country_Name==OriginCountry)]
GlobalMetadata$subregion[which(GlobalMetadata$ClioInfraCountryName==TargetCountry)] <- GlobalMetadata$subregion[which(GlobalMetadata$OECD_Country_Name==OriginCountry)]
GlobalMetadata$subregion_numeric[which(GlobalMetadata$ClioInfraCountryName==TargetCountry)] <- GlobalMetadata$subregion_numeric[which(GlobalMetadata$OECD_Country_Name==OriginCountry)]
GlobalMetadata$subsubregion[which(GlobalMetadata$ClioInfraCountryName==TargetCountry)] <- GlobalMetadata$subsubregion[which(GlobalMetadata$OECD_Country_Name==OriginCountry)]
GlobalMetadata$subsubregion_numeric[which(GlobalMetadata$ClioInfraCountryName==TargetCountry)] <- GlobalMetadata$subsubregion_numeric[which(GlobalMetadata$OECD_Country_Name==OriginCountry)]
GlobalMetadata$OECD_Region[which(GlobalMetadata$ClioInfraCountryName==TargetCountry)] <- GlobalMetadata$OECD_Region[which(GlobalMetadata$OECD_Country_Name==OriginCountry)]

TargetCountry <- "Czechoslovakia"
OriginCountry <- "Slovakia"

GlobalMetadata$region[which(GlobalMetadata$ClioInfraCountryName==TargetCountry)] <- GlobalMetadata$region[which(GlobalMetadata$OECD_Country_Name==OriginCountry)]
GlobalMetadata$region_numeric[which(GlobalMetadata$ClioInfraCountryName==TargetCountry)] <- GlobalMetadata$region_numeric[which(GlobalMetadata$OECD_Country_Name==OriginCountry)]
GlobalMetadata$subregion[which(GlobalMetadata$ClioInfraCountryName==TargetCountry)] <- GlobalMetadata$subregion[which(GlobalMetadata$OECD_Country_Name==OriginCountry)]
GlobalMetadata$subregion_numeric[which(GlobalMetadata$ClioInfraCountryName==TargetCountry)] <- GlobalMetadata$subregion_numeric[which(GlobalMetadata$OECD_Country_Name==OriginCountry)]
GlobalMetadata$subsubregion[which(GlobalMetadata$ClioInfraCountryName==TargetCountry)] <- GlobalMetadata$subsubregion[which(GlobalMetadata$OECD_Country_Name==OriginCountry)]
GlobalMetadata$subsubregion_numeric[which(GlobalMetadata$ClioInfraCountryName==TargetCountry)] <- GlobalMetadata$subsubregion_numeric[which(GlobalMetadata$OECD_Country_Name==OriginCountry)]
GlobalMetadata$OECD_Region[which(GlobalMetadata$ClioInfraCountryName==TargetCountry)] <- GlobalMetadata$OECD_Region[which(GlobalMetadata$OECD_Country_Name==OriginCountry)]

TargetCountry <- "West Germany"
OriginCountry <- "Germany"

GlobalMetadata$region[which(GlobalMetadata$ClioInfraCountryName==TargetCountry)] <- GlobalMetadata$region[which(GlobalMetadata$OECD_Country_Name==OriginCountry)]
GlobalMetadata$region_numeric[which(GlobalMetadata$ClioInfraCountryName==TargetCountry)] <- GlobalMetadata$region_numeric[which(GlobalMetadata$OECD_Country_Name==OriginCountry)]
GlobalMetadata$subregion[which(GlobalMetadata$ClioInfraCountryName==TargetCountry)] <- GlobalMetadata$subregion[which(GlobalMetadata$OECD_Country_Name==OriginCountry)]
GlobalMetadata$subregion_numeric[which(GlobalMetadata$ClioInfraCountryName==TargetCountry)] <- GlobalMetadata$subregion_numeric[which(GlobalMetadata$OECD_Country_Name==OriginCountry)]
GlobalMetadata$subsubregion[which(GlobalMetadata$ClioInfraCountryName==TargetCountry)] <- GlobalMetadata$subsubregion[which(GlobalMetadata$OECD_Country_Name==OriginCountry)]
GlobalMetadata$subsubregion_numeric[which(GlobalMetadata$ClioInfraCountryName==TargetCountry)] <- GlobalMetadata$subsubregion_numeric[which(GlobalMetadata$OECD_Country_Name==OriginCountry)]
GlobalMetadata$OECD_Region[which(GlobalMetadata$ClioInfraCountryName==TargetCountry)] <- GlobalMetadata$OECD_Region[which(GlobalMetadata$OECD_Country_Name==OriginCountry)]

TargetCountry <- "Germany Federal Republic"
OriginCountry <- "Germany"

GlobalMetadata$region[which(GlobalMetadata$ClioInfraCountryName==TargetCountry)] <- GlobalMetadata$region[which(GlobalMetadata$OECD_Country_Name==OriginCountry)]
GlobalMetadata$region_numeric[which(GlobalMetadata$ClioInfraCountryName==TargetCountry)] <- GlobalMetadata$region_numeric[which(GlobalMetadata$OECD_Country_Name==OriginCountry)]
GlobalMetadata$subregion[which(GlobalMetadata$ClioInfraCountryName==TargetCountry)] <- GlobalMetadata$subregion[which(GlobalMetadata$OECD_Country_Name==OriginCountry)]
GlobalMetadata$subregion_numeric[which(GlobalMetadata$ClioInfraCountryName==TargetCountry)] <- GlobalMetadata$subregion_numeric[which(GlobalMetadata$OECD_Country_Name==OriginCountry)]
GlobalMetadata$subsubregion[which(GlobalMetadata$ClioInfraCountryName==TargetCountry)] <- GlobalMetadata$subsubregion[which(GlobalMetadata$OECD_Country_Name==OriginCountry)]
GlobalMetadata$subsubregion_numeric[which(GlobalMetadata$ClioInfraCountryName==TargetCountry)] <- GlobalMetadata$subsubregion_numeric[which(GlobalMetadata$OECD_Country_Name==OriginCountry)]
GlobalMetadata$OECD_Region[which(GlobalMetadata$ClioInfraCountryName==TargetCountry)] <- GlobalMetadata$OECD_Region[which(GlobalMetadata$OECD_Country_Name==OriginCountry)]

TargetCountry <- "Germany Democratic Republic"
OriginCountry <- "Poland"

GlobalMetadata$region[which(GlobalMetadata$ClioInfraCountryName==TargetCountry)] <- GlobalMetadata$region[which(GlobalMetadata$OECD_Country_Name==OriginCountry)]
GlobalMetadata$region_numeric[which(GlobalMetadata$ClioInfraCountryName==TargetCountry)] <- GlobalMetadata$region_numeric[which(GlobalMetadata$OECD_Country_Name==OriginCountry)]
GlobalMetadata$subregion[which(GlobalMetadata$ClioInfraCountryName==TargetCountry)] <- GlobalMetadata$subregion[which(GlobalMetadata$OECD_Country_Name==OriginCountry)]
GlobalMetadata$subregion_numeric[which(GlobalMetadata$ClioInfraCountryName==TargetCountry)] <- GlobalMetadata$subregion_numeric[which(GlobalMetadata$OECD_Country_Name==OriginCountry)]
GlobalMetadata$subsubregion[which(GlobalMetadata$ClioInfraCountryName==TargetCountry)] <- GlobalMetadata$subsubregion[which(GlobalMetadata$OECD_Country_Name==OriginCountry)]
GlobalMetadata$subsubregion_numeric[which(GlobalMetadata$ClioInfraCountryName==TargetCountry)] <- GlobalMetadata$subsubregion_numeric[which(GlobalMetadata$OECD_Country_Name==OriginCountry)]
GlobalMetadata$OECD_Region[which(GlobalMetadata$ClioInfraCountryName==TargetCountry)] <- GlobalMetadata$OECD_Region[which(GlobalMetadata$OECD_Country_Name==OriginCountry)]

TargetCountry <- "Yugoslavia"
OriginCountry <- "Serbia"

GlobalMetadata$region[which(GlobalMetadata$ClioInfraCountryName==TargetCountry)] <- GlobalMetadata$region[which(GlobalMetadata$OECD_Country_Name==OriginCountry)]
GlobalMetadata$region_numeric[which(GlobalMetadata$ClioInfraCountryName==TargetCountry)] <- GlobalMetadata$region_numeric[which(GlobalMetadata$OECD_Country_Name==OriginCountry)]
GlobalMetadata$subregion[which(GlobalMetadata$ClioInfraCountryName==TargetCountry)] <- GlobalMetadata$subregion[which(GlobalMetadata$OECD_Country_Name==OriginCountry)]
GlobalMetadata$subregion_numeric[which(GlobalMetadata$ClioInfraCountryName==TargetCountry)] <- GlobalMetadata$subregion_numeric[which(GlobalMetadata$OECD_Country_Name==OriginCountry)]
GlobalMetadata$subsubregion[which(GlobalMetadata$ClioInfraCountryName==TargetCountry)] <- GlobalMetadata$subsubregion[which(GlobalMetadata$OECD_Country_Name==OriginCountry)]
GlobalMetadata$subsubregion_numeric[which(GlobalMetadata$ClioInfraCountryName==TargetCountry)] <- GlobalMetadata$subsubregion_numeric[which(GlobalMetadata$OECD_Country_Name==OriginCountry)]
GlobalMetadata$OECD_Region[which(GlobalMetadata$ClioInfraCountryName==TargetCountry)] <- GlobalMetadata$OECD_Region[which(GlobalMetadata$OECD_Country_Name==OriginCountry)]

TargetCountry <- "Persia"
OriginCountry <- "Iran (Islamic Republic of)"

GlobalMetadata$region[which(GlobalMetadata$ClioInfraCountryName==TargetCountry)] <- GlobalMetadata$region[which(GlobalMetadata$OECD_Country_Name==OriginCountry)]
GlobalMetadata$region_numeric[which(GlobalMetadata$ClioInfraCountryName==TargetCountry)] <- GlobalMetadata$region_numeric[which(GlobalMetadata$OECD_Country_Name==OriginCountry)]
GlobalMetadata$subregion[which(GlobalMetadata$ClioInfraCountryName==TargetCountry)] <- GlobalMetadata$subregion[which(GlobalMetadata$OECD_Country_Name==OriginCountry)]
GlobalMetadata$subregion_numeric[which(GlobalMetadata$ClioInfraCountryName==TargetCountry)] <- GlobalMetadata$subregion_numeric[which(GlobalMetadata$OECD_Country_Name==OriginCountry)]
GlobalMetadata$subsubregion[which(GlobalMetadata$ClioInfraCountryName==TargetCountry)] <- GlobalMetadata$subsubregion[which(GlobalMetadata$OECD_Country_Name==OriginCountry)]
GlobalMetadata$subsubregion_numeric[which(GlobalMetadata$ClioInfraCountryName==TargetCountry)] <- GlobalMetadata$subsubregion_numeric[which(GlobalMetadata$OECD_Country_Name==OriginCountry)]
GlobalMetadata$OECD_Region[which(GlobalMetadata$ClioInfraCountryName==TargetCountry)] <- GlobalMetadata$OECD_Region[which(GlobalMetadata$OECD_Country_Name==OriginCountry)]

TargetCountry <- "Gambia"
OriginCountry <- "Senegal"

GlobalMetadata$region[which(GlobalMetadata$ClioInfraCountryName==TargetCountry)] <- GlobalMetadata$region[which(GlobalMetadata$OECD_Country_Name==OriginCountry)]
GlobalMetadata$region_numeric[which(GlobalMetadata$ClioInfraCountryName==TargetCountry)] <- GlobalMetadata$region_numeric[which(GlobalMetadata$OECD_Country_Name==OriginCountry)]
GlobalMetadata$subregion[which(GlobalMetadata$ClioInfraCountryName==TargetCountry)] <- GlobalMetadata$subregion[which(GlobalMetadata$OECD_Country_Name==OriginCountry)]
GlobalMetadata$subregion_numeric[which(GlobalMetadata$ClioInfraCountryName==TargetCountry)] <- GlobalMetadata$subregion_numeric[which(GlobalMetadata$OECD_Country_Name==OriginCountry)]
GlobalMetadata$subsubregion[which(GlobalMetadata$ClioInfraCountryName==TargetCountry)] <- GlobalMetadata$subsubregion[which(GlobalMetadata$OECD_Country_Name==OriginCountry)]
GlobalMetadata$subsubregion_numeric[which(GlobalMetadata$ClioInfraCountryName==TargetCountry)] <- GlobalMetadata$subsubregion_numeric[which(GlobalMetadata$OECD_Country_Name==OriginCountry)]
GlobalMetadata$OECD_Region[which(GlobalMetadata$ClioInfraCountryName==TargetCountry)] <- GlobalMetadata$OECD_Region[which(GlobalMetadata$OECD_Country_Name==OriginCountry)]

TargetCountry <- "German Confederation"
OriginCountry <- "Germany"

GlobalMetadata$region[which(GlobalMetadata$ClioInfraCountryName==TargetCountry)] <- GlobalMetadata$region[which(GlobalMetadata$OECD_Country_Name==OriginCountry)]
GlobalMetadata$region_numeric[which(GlobalMetadata$ClioInfraCountryName==TargetCountry)] <- GlobalMetadata$region_numeric[which(GlobalMetadata$OECD_Country_Name==OriginCountry)]
GlobalMetadata$subregion[which(GlobalMetadata$ClioInfraCountryName==TargetCountry)] <- GlobalMetadata$subregion[which(GlobalMetadata$OECD_Country_Name==OriginCountry)]
GlobalMetadata$subregion_numeric[which(GlobalMetadata$ClioInfraCountryName==TargetCountry)] <- GlobalMetadata$subregion_numeric[which(GlobalMetadata$OECD_Country_Name==OriginCountry)]
GlobalMetadata$subsubregion[which(GlobalMetadata$ClioInfraCountryName==TargetCountry)] <- GlobalMetadata$subsubregion[which(GlobalMetadata$OECD_Country_Name==OriginCountry)]
GlobalMetadata$subsubregion_numeric[which(GlobalMetadata$ClioInfraCountryName==TargetCountry)] <- GlobalMetadata$subsubregion_numeric[which(GlobalMetadata$OECD_Country_Name==OriginCountry)]
GlobalMetadata$OECD_Region[which(GlobalMetadata$ClioInfraCountryName==TargetCountry)] <- GlobalMetadata$OECD_Region[which(GlobalMetadata$OECD_Country_Name==OriginCountry)]

TargetCountry <- "Prussia"
OriginCountry <- "Poland"

GlobalMetadata$region[which(GlobalMetadata$ClioInfraCountryName==TargetCountry)] <- GlobalMetadata$region[which(GlobalMetadata$OECD_Country_Name==OriginCountry)]
GlobalMetadata$region_numeric[which(GlobalMetadata$ClioInfraCountryName==TargetCountry)] <- GlobalMetadata$region_numeric[which(GlobalMetadata$OECD_Country_Name==OriginCountry)]
GlobalMetadata$subregion[which(GlobalMetadata$ClioInfraCountryName==TargetCountry)] <- GlobalMetadata$subregion[which(GlobalMetadata$OECD_Country_Name==OriginCountry)]
GlobalMetadata$subregion_numeric[which(GlobalMetadata$ClioInfraCountryName==TargetCountry)] <- GlobalMetadata$subregion_numeric[which(GlobalMetadata$OECD_Country_Name==OriginCountry)]
GlobalMetadata$subsubregion[which(GlobalMetadata$ClioInfraCountryName==TargetCountry)] <- GlobalMetadata$subsubregion[which(GlobalMetadata$OECD_Country_Name==OriginCountry)]
GlobalMetadata$subsubregion_numeric[which(GlobalMetadata$ClioInfraCountryName==TargetCountry)] <- GlobalMetadata$subsubregion_numeric[which(GlobalMetadata$OECD_Country_Name==OriginCountry)]
GlobalMetadata$OECD_Region[which(GlobalMetadata$ClioInfraCountryName==TargetCountry)] <- GlobalMetadata$OECD_Region[which(GlobalMetadata$OECD_Country_Name==OriginCountry)]

# Repetition of the loop for export all countries data, also found above (here it does not export anything):

CountriesWithNoData <- 0
CountriesWithData <- 0
for (k in 1:length(CountriesList)){
  CountryData <- subset(ClioData,ClioData$`country name`==CountriesList[k])
  # keep only rows that have not all elements of the specified columns NA
  CountryData <- CountryData[rowSums(is.na(CountryData[,8:524]))<516,]
  if (nrow(CountryData)>1){
    CountriesWithData <- CountriesWithData + 1
    #WriteXLS(CountryData, paste0(ExportPath,CountriesList[k],".xlsx"))
  } else {
    # I want to know the names of countries without data
    print(CountriesList[k])
    CountriesWithNoData <- CountriesWithNoData + 1
  }
}

#### Read the docx information in a dataframe ####

# save.image("~/PhD/Clio Infra/UPDATE 20210315/Test20210315.RData")
# load("~/PhD/Clio Infra/UPDATE 20210315/Test20210315.RData")

# see the /home/michalis/PhD/Clio Infra/ConvertDOCStoTXT.xls file for converting commands (run on ubuntu only)
# I try automating this first:

# the various fields expected to have information in the docx and doc files:
txtFields <- c("1. Title","2. Author\\(s\\)","3. Production date","4. Version","5. Variable group\\(s\\)","6. Variable\\(s\\)","7. Unit of analysis",
               "8. Keywords \\(5\\)","9. Abstract \\(200 words\\)","10. Time period","11. Geographical coverage","12. Methodologies used for data collection and processing",
               "13. Data quality","14. Date of collection","15. Data collectors","16. Sources")

# after everything has been converted to txt (see xls above for the relevant system commands) then I process the resulting txt files:
FilenamePattern <- '.txt'

txtFiles <- list.files(path=DataPath,pattern = FilenamePattern)

tempMetaClio <- readChar(paste0(DataPath,txtFiles[1]), file.info(paste0(DataPath,txtFiles[1]))$size)

#ClioMetaData$Filename <- txtFiles[1]

# keep the info only:
metaD <- c()
metaD[1] <- strsplit(strsplit(tempMetaClio,txtFields[1])[[1]][2],txtFields[2])[[1]][1]
temp <- strsplit(strsplit(tempMetaClio,txtFields[1])[[1]][2],txtFields[2])[[1]][2]

for (i in 2:(length(txtFields)-1)){
  metaD[i] <- strsplit(temp, txtFields[i+1])[[1]][1]
  temp <- strsplit(temp, txtFields[i+1])[[1]][2]
}
metaD[i+1]<- temp

# remove leading or trailing \n and \t (or their combinations) only

metaD <- str_trim(metaD)
ClioMetaData <- data.frame(title = as.character(NA),
                           author = as.character(NA),
                           proddate = as.character(NA), 
                           version = as.character(NA), 
                           vargroups = as.character(NA), 
                           vars = as.character(NA), 
                           unit = as.character(NA),
                           keywords = as.character(NA),
                           abstract = as.character(NA),
                           timeperiod = as.character(NA), 
                           geocover = as.character(NA), 
                           methods = as.character(NA), 
                           quality = as.character(NA), 
                           collectdate = as.character(NA),
                           collectors = as.character(NA),
                           sources = as.character(NA),
                           filename =as.character(NA),
                           stringsAsFactors = F)

tempframe <- ClioMetaData

for (i in 1:16){
  ClioMetaData[1,i] <- metaD[i]
  }
ClioMetaData[1,17] <- paste0(substr(txtFiles[1], 1, nchar(txtFiles[1])-4),"-historical.xlsx")

# now we are ready to iterate among txt files

# XXX MANUAL WORK required in pre-editing the txt files for: 
# Height, Height Gini, income inequality: removed extended 17 section
# Number_Days_Lost_Labour_Conflicts, Number_Labour_Conflicts, Number_Workers_Labour_Conflicts substituted 2. Author with 2. Author(s)
# in a number of occasions (~15) the "8. Keywords (5)" was differently written 
# e.g. "8. Keywords (6)" or "8. Keywords" those where manually changed back to "8. Keywords (5)"

for (k in 2:length(txtFiles)){
  tempMetaClio <- readChar(paste0(DataPath,txtFiles[k]), file.info(paste0(DataPath,txtFiles[k]))$size)
  tempMetaClio <- gsub("14. Period of collection","14. Date of collection",tempMetaClio)
  tempMetaClio <- gsub("15. Data collector\n","15. Data collectors\n",tempMetaClio)
  #ClioMetaData$Filename <- txtFiles[1]
  
  # keep the info only:
  metaD <- c()
  metaD[1] <- strsplit(strsplit(tempMetaClio,txtFields[1])[[1]][2],txtFields[2])[[1]][1]
  temp <- strsplit(strsplit(tempMetaClio,txtFields[1])[[1]][2],txtFields[2])[[1]][2]
  
  for (i in 2:(length(txtFields)-1)){
    metaD[i] <- strsplit(temp, txtFields[i+1])[[1]][1]
    temp <- strsplit(temp, txtFields[i+1])[[1]][2]
  }
  metaD[i+1]<- strsplit(temp, "17. Text")[[1]][1]
  
  # remove leading or trailing \n and \t (or their combinations) only
  
  metaD <- str_trim(metaD)
  
  for (i in 1:16){
    tempframe[1,i] <- metaD[i]
  }
  if (txtFiles[k] %in% c("Global_Extreme_Poverty_Cost_of_Basic_Needs.txt",
                      "Global_Extreme_Poverty_Dollar_a_Day.txt")){
    tempframe[1,17] <- paste0(substr(txtFiles[k], 1, nchar(txtFiles[k])-4),".xlsx")
  } else {
    tempframe[1,17] <- paste0(substr(txtFiles[k], 1, nchar(txtFiles[k])-4),"-historical.xlsx")
  }
  ClioMetaData <- rbind(ClioMetaData,tempframe)
  tempframe[,] <- as.character(NA)
}

# remove leading " -"
trim <- function (x) gsub("^\\-", "", x)

# remove trailing "."
trim2 <- function (x) gsub("\\.$", "", x)

for (j in 1:ncol(ClioMetaData)){
  for (k in 1:nrow(ClioMetaData)){
    ClioMetaData[k,j] <- trim(ClioMetaData[k,j])
    if (!(names(ClioMetaData)[j] %in% c("sources", "text")) & !(ClioMetaData[k,j]=="n.a." & !is.na(ClioMetaData[k,j]))){
      ClioMetaData[k,j] <- trim2(ClioMetaData[k,j])
    }
    ClioMetaData[k,j] <- trimws(ClioMetaData[k,j])
  }
}

#### ADD WebName and WebCategory ####
# now add columns with the name of the indicator as it appears on the website, and the category that it is assigned:
ClioMetaData$WebName <- NA
ClioMetaData$WebCategory <- NA
ClioMetaData$WebName[which(ClioMetaData$title=="Aluminum primary production by decade and country")] <- "Aluminium Production"
ClioMetaData$WebCategory[which(ClioMetaData$title=="Aluminum primary production by decade and country")] <- "Production"
ClioMetaData$WebName[which(ClioMetaData$title=="Bauxite mining production by decade and country")] <- "Bauxite Production"
ClioMetaData$WebCategory[which(ClioMetaData$title=="Bauxite mining production by decade and country")] <- "Production"
ClioMetaData$WebName[which(ClioMetaData$title=="Copper mining production by decade and country")] <- "Copper Production"
ClioMetaData$WebCategory[which(ClioMetaData$title=="Copper mining production by decade and country")] <- "Production"
ClioMetaData$WebName[which(ClioMetaData$title=="Total Gold Mine production per decade and country")] <- "Gold Production"
ClioMetaData$WebCategory[which(ClioMetaData$title=="Total Gold Mine production per decade and country")] <- "Production"
ClioMetaData$WebName[which(ClioMetaData$title=="Total Iron Ore Mine production per decade and country")] <- "Iron Ore Production"
ClioMetaData$WebCategory[which(ClioMetaData$title=="Total Iron Ore Mine production per decade and country")] <- "Production"
ClioMetaData$WebName[which(ClioMetaData$title=="Lead mining production by decade and country")] <- "Lead Production"
ClioMetaData$WebCategory[which(ClioMetaData$title=="Lead mining production by decade and country")] <- "Production"
ClioMetaData$WebName[which(ClioMetaData$title=="Manganese mining production by decade and country")] <- "Manganese Production"
ClioMetaData$WebCategory[which(ClioMetaData$title=="Manganese mining production by decade and country")] <- "Production"
ClioMetaData$WebName[which(ClioMetaData$title=="Nickel mining production by decade and country")] <- "Nickel Production"
ClioMetaData$WebCategory[which(ClioMetaData$title=="Nickel mining production by decade and country")] <- "Production"
ClioMetaData$WebName[which(ClioMetaData$title=="Silver mining production by decade and country")] <- "Silver Production"
ClioMetaData$WebCategory[which(ClioMetaData$title=="Silver mining production by decade and country")] <- "Production"
ClioMetaData$WebName[which(ClioMetaData$title=="Total Tin Mine production per decade and country")] <- "Tin Production"
ClioMetaData$WebCategory[which(ClioMetaData$title=="Total Tin Mine production per decade and country")] <- "Production"
ClioMetaData$WebName[which(ClioMetaData$title=="Tungsten mining production by decade and country")] <- "Tungsten Production"
ClioMetaData$WebCategory[which(ClioMetaData$title=="Tungsten mining production by decade and country")] <- "Production"
ClioMetaData$WebName[which(ClioMetaData$title=="Total Zinc Mine production per decade and country")] <- "Zinc Production"
ClioMetaData$WebCategory[which(ClioMetaData$title=="Total Zinc Mine production per decade and country")] <- "Production"
ClioMetaData$WebName[which(ClioMetaData$title=="Cattle per capita numbers by decade and country")] <- "Cattle per Capita"
ClioMetaData$WebCategory[which(ClioMetaData$title=="Cattle per capita numbers by decade and country")] <- "Agriculture"
ClioMetaData$WebName[which(ClioMetaData$title=="Cropland per capita area by decade and country")] <- "Cropland per Capita"
ClioMetaData$WebCategory[which(ClioMetaData$title=="Cropland per capita area by decade and country")] <- "Agriculture"
ClioMetaData$WebName[which(ClioMetaData$title=="Goats per capita numbers by decade and country")] <- "Goats per Capita"
ClioMetaData$WebCategory[which(ClioMetaData$title=="Goats per capita numbers by decade and country")] <- "Agriculture"
ClioMetaData$WebName[which(ClioMetaData$title=="Pasture per capita area by decade and country")] <- "Pasture per Capita"
ClioMetaData$WebCategory[which(ClioMetaData$title=="Pasture per capita area by decade and country")] <- "Agriculture"
ClioMetaData$WebName[which(ClioMetaData$title=="Pigs per capita numbers by decade and country")] <- "Pigs per Capita"
ClioMetaData$WebCategory[which(ClioMetaData$title=="Pigs per capita numbers by decade and country")] <- "Agriculture"
ClioMetaData$WebName[which(ClioMetaData$title=="Sheep per capita numbers by decade and country")] <- "Sheep per Capita"
ClioMetaData$WebCategory[which(ClioMetaData$title=="Sheep per capita numbers by decade and country")] <- "Agriculture"
ClioMetaData$WebName[which(ClioMetaData$title=="Total cattle by decade and country")] <- "Total Cattle"
ClioMetaData$WebCategory[which(ClioMetaData$title=="Total cattle by decade and country")] <- "Agriculture"
ClioMetaData$WebName[which(ClioMetaData$title=="Total cropland area by decade and country")] <- "Total Cropland"
ClioMetaData$WebCategory[which(ClioMetaData$title=="Total cropland area by decade and country")] <- "Agriculture"
ClioMetaData$WebName[which(ClioMetaData$title=="Total number of goats by decade and country")] <- "Total Number of Goats"
ClioMetaData$WebCategory[which(ClioMetaData$title=="Total number of goats by decade and country")] <- "Agriculture"
ClioMetaData$WebName[which(ClioMetaData$title=="Total number of pigs by decade and country")] <- "Total Number of Pigs"
ClioMetaData$WebCategory[which(ClioMetaData$title=="Total number of pigs by decade and country")] <- "Agriculture"
ClioMetaData$WebName[which(ClioMetaData$title=="Total number of sheep by decade and country")] <- "Total Number of Sheep"
ClioMetaData$WebCategory[which(ClioMetaData$title=="Total number of sheep by decade and country")] <- "Agriculture"
ClioMetaData$WebName[which(ClioMetaData$title=="Total pasture area by decade and country")] <- "Total Pasture"
ClioMetaData$WebCategory[which(ClioMetaData$title=="Total pasture area by decade and country")] <- "Agriculture"
ClioMetaData$WebName[which(ClioMetaData$title=="Infant mortality by decade and country")] <- "Infant Mortality"
ClioMetaData$WebCategory[which(ClioMetaData$title=="Infant mortality by decade and country")] <- "Demography"
ClioMetaData$WebName[which(ClioMetaData$title=="Life Expectancy per country (Average, total Population), 1500-2000")] <- "Life Expectancy at Birth (Total)"
ClioMetaData$WebCategory[which(ClioMetaData$title=="Life Expectancy per country (Average, total Population), 1500-2000")] <- "Demography"
ClioMetaData$WebName[which(ClioMetaData$title=="Male Life Expectancy, 1750-2000")] <- "Male life Expectancy at Birth"
ClioMetaData$WebCategory[which(ClioMetaData$title=="Male Life Expectancy, 1750-2000")] <- "Demography"
ClioMetaData$WebName[which(ClioMetaData$title=="Female Life Expectancy, 1750-2000")] <- "Female life Expectancy at Birth"
ClioMetaData$WebCategory[which(ClioMetaData$title=="Female Life Expectancy, 1750-2000")] <- "Demography"
ClioMetaData$WebName[which(ClioMetaData$title=="World Countries Total Population Size, 1500-2000")] <- "Total Population"
ClioMetaData$WebCategory[which(ClioMetaData$title=="World Countries Total Population Size, 1500-2000")] <- "Demography"
ClioMetaData$WebName[which(ClioMetaData$title=="World Countries Urban Population Size, 1500-2000")] <- "Total Urban Population"
ClioMetaData$WebCategory[which(ClioMetaData$title=="World Countries Urban Population Size, 1500-2000")] <- "Demography"
ClioMetaData$WebName[which(ClioMetaData$title=="World Countries Urbanization ratio, 1500-2000")] <- "Urbanization Ratio"
ClioMetaData$WebCategory[which(ClioMetaData$title=="World Countries Urbanization ratio, 1500-2000")] <- "Demography"
ClioMetaData$WebName[which(ClioMetaData$title=="Biodiversity changes by decade and country")] <- "Biodiversity - naturalness"
ClioMetaData$WebCategory[which(ClioMetaData$title=="Biodiversity changes by decade and country")] <- "Environment"
ClioMetaData$WebName[which(ClioMetaData$title=="Fossil fuel CO2 emissions per capita by decade and country")] <- "CO2 Emissions per Capita"
ClioMetaData$WebCategory[which(ClioMetaData$title=="Fossil fuel CO2 emissions per capita by decade and country")] <- "Environment"
ClioMetaData$WebName[which(ClioMetaData$title=="Total SO2 emissions per capita by decade and country")] <- "SO2 Emissions per Capita"
ClioMetaData$WebCategory[which(ClioMetaData$title=="Total SO2 emissions per capita by decade and country")] <- "Environment"
ClioMetaData$WebName[which(ClioMetaData$title=="Total fossil fuel CO2 emissions by decade and country")] <- "Total CO2 Emissions"
ClioMetaData$WebCategory[which(ClioMetaData$title=="Total fossil fuel CO2 emissions by decade and country")] <- "Environment"
ClioMetaData$WebName[which(ClioMetaData$title=="Total fossil fuel SO2 emissions by decade and country")] <- "Total SO2 Emissions"
ClioMetaData$WebCategory[which(ClioMetaData$title=="Total fossil fuel SO2 emissions by decade and country")] <- "Environment"
ClioMetaData$WebName[which(ClioMetaData$title=="Exchange Rates US Database CLIO-INFRA")] <- "Exchange Rates to US Dollar"
ClioMetaData$WebCategory[which(ClioMetaData$title=="Exchange Rates US Database CLIO-INFRA")] <- "Finance"
ClioMetaData$WebName[which(ClioMetaData$title=="Exchange Rates UK Database CLIO-INFRA")] <- "Exchange Rates to UK Pound"
ClioMetaData$WebCategory[which(ClioMetaData$title=="Exchange Rates UK Database CLIO-INFRA")] <- "Finance"
ClioMetaData$WebName[which(ClioMetaData$title=="Gold Standard")] <- "Gold Standard"
ClioMetaData$WebCategory[which(ClioMetaData$title=="Gold Standard")] <- "Finance"
ClioMetaData$WebName[which(ClioMetaData$title=="Long-Term Government Bond Yield Database clio infra")] <- "Long-Term Government Bond Yield"
ClioMetaData$WebCategory[which(ClioMetaData$title=="Long-Term Government Bond Yield Database clio infra")] <- "Finance"
ClioMetaData$WebName[which(ClioMetaData$title=="Total gross central government debt as a percentage of GDP database clio infra")] <- "Total Gross Central Government Debt as a Percentage of GDP"
ClioMetaData$WebCategory[which(ClioMetaData$title=="Total gross central government debt as a percentage of GDP database clio infra")] <- "Finance"
ClioMetaData$WebName[which(ClioMetaData$title=="Estimates of the gender equality of numeracy (ABCC) by birth decade and\ncountry")] <- "Gender Equality of Numeracy"
ClioMetaData$WebCategory[which(ClioMetaData$title=="Estimates of the gender equality of numeracy (ABCC) by birth decade and\ncountry")] <- "Gender Equality"
ClioMetaData$WebName[which(ClioMetaData$title=="Ratio of girls to boys in average years of schooling by country and\ndecade")] <- "Gender Equality Years of Education"
ClioMetaData$WebCategory[which(ClioMetaData$title=="Ratio of girls to boys in average years of schooling by country and\ndecade")] <- "Gender Equality"
ClioMetaData$WebName[which(ClioMetaData$title=="Gender-equal inheritance by country and decade")] <- "Gender-equal Inheritance"
ClioMetaData$WebCategory[which(ClioMetaData$title=="Gender-equal inheritance by country and decade")] <- "Gender Equality"
ClioMetaData$WebName[which(ClioMetaData$title=="Historical Gender Equality Index")] <- "Historical Gender Equality Index"
ClioMetaData$WebCategory[which(ClioMetaData$title=="Historical Gender Equality Index")] <- "Gender Equality"
ClioMetaData$WebName[which(ClioMetaData$title=="Ratio of girls to boys aged 0–5 by country and decade")] <- "Sex Ratio"
ClioMetaData$WebCategory[which(ClioMetaData$title=="Ratio of girls to boys aged 0–5 by country and decade")] <- "Gender Equality"
ClioMetaData$WebName[which(ClioMetaData$title=="Share of women in parliaments (%) 1960-2010")] <- "Share of Women in Parliament"
ClioMetaData$WebCategory[which(ClioMetaData$title=="Share of women in parliaments (%) 1960-2010")] <- "Gender Equality"
ClioMetaData$WebName[which(ClioMetaData$title=="Average years of education (Average, total Population 15 years and older), 1850-2010")] <- "Average Years of Education"
ClioMetaData$WebCategory[which(ClioMetaData$title=="Average years of education (Average, total Population 15 years and older), 1850-2010")] <- "Human Capital"
ClioMetaData$WebName[which(ClioMetaData$title=="World Countries New Book titles per Capita, 1500-2010")] <- "Book Titles per Capita"
ClioMetaData$WebCategory[which(ClioMetaData$title=="World Countries New Book titles per Capita, 1500-2010")] <- "Human Capital"
ClioMetaData$WebName[which(ClioMetaData$title=="Educational inequality")] <- "Educational Inequality Gini Coefficient"
ClioMetaData$WebCategory[which(ClioMetaData$title=="Educational inequality")] <- "Human Capital"
ClioMetaData$WebName[which(ClioMetaData$title=="Numeracy estimates (ABCC) by birth decade and country, both before and\nafter 1800")] <- "Numeracy (Total)"
ClioMetaData$WebCategory[which(ClioMetaData$title=="Numeracy estimates (ABCC) by birth decade and country, both before and\nafter 1800")] <- "Human Capital"
ClioMetaData$WebName[which(ClioMetaData$title=="Number of universities founded")] <- "Universities Founded"
ClioMetaData$WebCategory[which(ClioMetaData$title=="Number of universities founded")] <- "Human Capital"
ClioMetaData$WebName[which(ClioMetaData$title=="Armed internal conflicts, 1500-2000")] <- "Armed conflicts (Internal)"
ClioMetaData$WebCategory[which(ClioMetaData$title=="Armed internal conflicts, 1500-2000")] <- "Institutions"
ClioMetaData$WebName[which(ClioMetaData$title=="Armed external conflicts, 1500-2000")] <- "Armed Conflicts (International)"
ClioMetaData$WebCategory[which(ClioMetaData$title=="Armed external conflicts, 1500-2000")] <- "Institutions"
ClioMetaData$WebName[which(ClioMetaData$title=="Competitiveness of Executive Recruitment (XRCOMP)")] <- "Competitiveness of Executive Recruitment (XRCOMP)"
ClioMetaData$WebCategory[which(ClioMetaData$title=="Competitiveness of Executive Recruitment (XRCOMP)")] <- "Institutions"
ClioMetaData$WebName[which(ClioMetaData$title=="Competitiveness of Participation (PARCOMP)")] <- "Competitiveness of Participations (PARCOMP)"
ClioMetaData$WebCategory[which(ClioMetaData$title=="Competitiveness of Participation (PARCOMP)")] <- "Institutions"
ClioMetaData$WebName[which(ClioMetaData$title=="Executive Constraints (XCONST)")] <- "Executive Constraints (XCONST)"
ClioMetaData$WebCategory[which(ClioMetaData$title=="Executive Constraints (XCONST)")] <- "Institutions"
ClioMetaData$WebName[which(ClioMetaData$title=="World Countries Homicide Rate, 1500-2000")] <- "Homicide Rates"
ClioMetaData$WebCategory[which(ClioMetaData$title=="World Countries Homicide Rate, 1500-2000")] <- "Institutions"
ClioMetaData$WebName[which(ClioMetaData$title=="Latent Democracy variable")] <- "Latent Democracy Variable"
ClioMetaData$WebCategory[which(ClioMetaData$title=="Latent Democracy variable")] <- "Institutions"
ClioMetaData$WebName[which(ClioMetaData$title=="Openness of Executive Recruitment (XROPEN)")] <- "Openness of Executive Recruitment (XROPEN)"
ClioMetaData$WebCategory[which(ClioMetaData$title=="Openness of Executive Recruitment (XROPEN)")] <- "Institutions"
ClioMetaData$WebName[which(ClioMetaData$title=="Vanhanen`s political competition dataset")] <- "Political Competition"
ClioMetaData$WebCategory[which(ClioMetaData$title=="Vanhanen`s political competition dataset")] <- "Institutions"
ClioMetaData$WebName[which(ClioMetaData$title=="Vanhanen`s political participation dataset")] <- "Political Participation"
ClioMetaData$WebCategory[which(ClioMetaData$title=="Vanhanen`s political participation dataset")] <- "Institutions"
ClioMetaData$WebName[which(ClioMetaData$title=="Autocracy -democracy index, 1800-2010")] <- "Polity2 Index"
ClioMetaData$WebCategory[which(ClioMetaData$title=="Autocracy -democracy index, 1800-2010")] <- "Institutions"
ClioMetaData$WebName[which(ClioMetaData$title=="Vanhanen`s democracy (polyarchy) dataset")] <- "Polyarchy"
ClioMetaData$WebCategory[which(ClioMetaData$title=="Vanhanen`s democracy (polyarchy) dataset")] <- "Institutions"
ClioMetaData$WebName[which(ClioMetaData$title=="Regulation of Chief Executive Recruitment (XRREG)")] <- "Regulation of Chief Executive Recruitment (XRREG)"
ClioMetaData$WebCategory[which(ClioMetaData$title=="Regulation of Chief Executive Recruitment (XRREG)")] <- "Institutions"
ClioMetaData$WebName[which(ClioMetaData$title=="Regulation of Participation (PARREG)")] <- "Regulation of Participation (PARREG)"
ClioMetaData$WebCategory[which(ClioMetaData$title=="Regulation of Participation (PARREG)")] <- "Institutions"
ClioMetaData$WebName[which(ClioMetaData$title=="Unified Democracy Scores (UDS)")] <- "Unified Democracy Scores (UDS)"
ClioMetaData$WebCategory[which(ClioMetaData$title=="Unified Democracy Scores (UDS)")] <- "Institutions"
ClioMetaData$WebName[which(ClioMetaData$title=="Number of Days Not Worked as a result of Labour Conflicts per country , 1927-2013")] <- "Number of Days Lost in Labour Disputes"
ClioMetaData$WebCategory[which(ClioMetaData$title=="Number of Days Not Worked as a result of Labour Conflicts per country , 1927-2013")] <- "Labour Relations"
ClioMetaData$WebName[which(ClioMetaData$title=="Number of Labour Conflicts per country , 1927-2010")] <- "Number of Labour Disputes"
ClioMetaData$WebCategory[which(ClioMetaData$title=="Number of Labour Conflicts per country , 1927-2010")] <- "Labour Relations"
ClioMetaData$WebName[which(ClioMetaData$title=="Number of Workers involved in Labour Conflicts per country , 1927-2010")] <- "Number of Workers Involved in Labour Disputes"
ClioMetaData$WebCategory[which(ClioMetaData$title=="Number of Workers involved in Labour Conflicts per country , 1927-2010")] <- "Labour Relations"
ClioMetaData$WebName[which(ClioMetaData$title=="The First Update of the Maddison Project; Re-Estimating growth before 1820")] <- "GDP per Capita"
ClioMetaData$WebCategory[which(ClioMetaData$title=="The First Update of the Maddison Project; Re-Estimating growth before 1820")] <- "National Accounts"
ClioMetaData$WebName[which(ClioMetaData$title=="Gross household income gini, 1820-2000")] <- "Income Inequality"
ClioMetaData$WebCategory[which(ClioMetaData$title=="Gross household income gini, 1820-2000")] <- "Prices and Wages"
ClioMetaData$WebName[which(ClioMetaData$title=="Inflation Database clio infra")] <- "Inflation"
ClioMetaData$WebCategory[which(ClioMetaData$title=="Inflation Database clio infra")] <- "Prices and Wages"
ClioMetaData$WebName[which(ClioMetaData$title=="Building labourers' real wages, 1820-2010")] <- "Labourer's Real Wage"
ClioMetaData$WebCategory[which(ClioMetaData$title=="Building labourers' real wages, 1820-2010")] <- "Prices and Wages"

ClioMetaData$WebName[which(ClioMetaData$title=="Height ginis by birth decade and country")] <- "Height Gini"
ClioMetaData$WebCategory[which(ClioMetaData$title=="Height ginis by birth decade and country")] <- "Demography"
ClioMetaData$WebName[which(ClioMetaData$title=="Heights by birth decade and country, both before and after 1800")] <- "Height"
ClioMetaData$WebCategory[which(ClioMetaData$title=="Heights by birth decade and country, both before and after 1800")] <- "Demography"
ClioMetaData$WebName[which(ClioMetaData$title=="Composite Measure of Wellbeing")] <- "Composite Measure of Wellbeing"
ClioMetaData$WebCategory[which(ClioMetaData$title=="Composite Measure of Wellbeing")] <- "Demography"

### UPDATE for HWL2
ClioMetaData$WebName[which(ClioMetaData$title=="Global Extreme Poverty Cost of Basic Needs")] <- "Global Extreme Poverty Cost of Basic Needs"
ClioMetaData$WebCategory[which(ClioMetaData$title=="Global Extreme Poverty Cost of Basic Needs")] <- "Demography"
ClioMetaData$WebName[which(ClioMetaData$title=="Global Extreme Poverty Dollar a Day")] <- "Global Extreme Poverty Dollar a Day"
ClioMetaData$WebCategory[which(ClioMetaData$title=="Global Extreme Poverty Dollar a Day")] <- "Demography"

ClioMetaData$WebName[which(ClioMetaData$title=="Wealth Yearly Ginis")] <- "Wealth Yearly Ginis"
ClioMetaData$WebCategory[which(ClioMetaData$title=="Wealth Yearly Ginis")] <- "Prices and Wages"
ClioMetaData$WebName[which(ClioMetaData$title=="Wealth Total")] <- "Wealth Total"
ClioMetaData$WebCategory[which(ClioMetaData$title=="Wealth Total")] <- "Prices and Wages"
ClioMetaData$WebName[which(ClioMetaData$title=="Wealth Top10 percent share")] <- "Wealth Top10 percent share"
ClioMetaData$WebCategory[which(ClioMetaData$title=="Wealth Top10 percent share")] <- "Prices and Wages"
ClioMetaData$WebName[which(ClioMetaData$title=="Wealth Decadal Ginis")] <- "Wealth Decadal Ginis"
ClioMetaData$WebCategory[which(ClioMetaData$title=="Wealth Decadal Ginis")] <- "Prices and Wages"

# 17 new datasets:
# Social_Transfers.xlsx
# Gender_lab.xlsx
# Gender_edu.xlsx
# Gender_HGI.xlsx
# Working_hours.xlsx
# Education_Relative_Gini.xlsx
# Education_SD.xlsx
# Education_Gini_LLMM.xlsx
# Education_Average_LLMM.xlsx
# Education_Population_Above_15.xlsx
# Life_Expectancy_Relative_Gini_Female.xlsx
# Life_Expectancy_Relative_Gini_Male.xlsx
# Life_Expectancy_Gini_Female.xlsx
# Life_Expectancy_Gini_Male.xlsx
# Life_Expectancy_Level_Female.xlsx
# Life_Expectancy_Level_Male.xlsx
# GDP_Per_Capita_Mix.xlsx

#ClioMetaData$WebName[which(ClioMetaData$title=="")] <- ""
#ClioMetaData$WebCategory[which(ClioMetaData$title=="")] <- ""

# [1] "Exchange Rates to UK Pound"      "Female life expectancy at Birth" "Height Gini"                    
# [4] "Height"


## keep the txt filename, and the doc(x) name of the file
## add a short-hand name for the variable to be used throughout the text,
## as it is now shown on the list of indicators on the website, and the same applies for the indicators' group names

## create a list of variables on the html template to be substituted by the code in this script
## the form of those variables should be of XxZzYyTHE_VAR_NAMEXxZzYy, e.g. XxZzYyIndicatorXxZzYy, 
## XxZzYyCountryXxZzYy, XxZzYyArithmeticMeanXxZzYy, XxZzYyTitleXxZzYy, XxZzYyObservationsXxZzYy,
## XxZzYyObsAfricaXxZzYy, etc

# check which indicators are missing:
Dataverse <- read.csv("/home/michalis/PhD/Clio Infra/Website/IndicatorsList.txt",header=F, stringsAsFactors = F)
ClioNow <- read.csv("/home/michalis/PhD/Clio Infra/Website/IndicatorsOnClioInfraEUNow.txt",header=F, stringsAsFactors = F)

# compare those two:
DDV <- as.vector(t(Dataverse))
CLN <- as.vector(t(ClioNow))

DDV[which(!(c(DDV) %in% c(CLN)))]

ClioDataList <- unique(ClioData$Indicator)
ClioDataList[which(!(c(ClioDataList) %in% c(CLN)))]
# all are in!

# map title with indicator names:
write.xlsx2(ClioMetaData, paste0(dirname(rstudioapi::getSourceEditorContext()$path),'/Aggregates/metaD.xlsx'),sheetName = "Data",row.names = F, append = F, showNA = F)
write.xlsx2(OECDregions, paste0(dirname(rstudioapi::getSourceEditorContext()$path),'/Aggregates/OECDregions.xlsx'),sheetName = "Data",row.names = F, append = F, showNA = F)
write.xlsx2(GlobalMetadata, paste0(dirname(rstudioapi::getSourceEditorContext()$path),'/Aggregates/GlobalMetadata.xlsx'),sheetName = "Data",row.names = F, append = F, showNA = F)
write.xlsx2(UNmembers, paste0(dirname(rstudioapi::getSourceEditorContext()$path),'/Aggregates/UNmembers.xlsx'),sheetName = "Data",row.names = F, append = F, showNA = F)
write.xlsx2(UNregions, paste0(dirname(rstudioapi::getSourceEditorContext()$path),'/Aggregates/UNregions.xlsx'),sheetName = "Data",row.names = F, append = F, showNA = F)

rm(list= ls()[!(ls() %in% c('ClioData'))])
save.image(paste0(dirname(rstudioapi::getSourceEditorContext()$path),'/Aggregates/ClioData.RData'))

# 20210720-13:58
# save.image("~/PhD/Clio Infra/Website/AddingWellBeing.RData")
