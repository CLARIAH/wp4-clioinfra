##########################################################################################################
##########################################################################################################
##########################################################################################################
##########################################################################################################
# create the Indicators Menu items #######################################################################
# create the Indicators Menu items #######################################################################
# create the Indicators Menu items #######################################################################
##########################################################################################################
##########################################################################################################
##########################################################################################################
##########################################################################################################

load("~/PhD/Clio Infra/Website/ReadData3.R.RData")

CreateHistoricalDataFiles <- F

# this script substitutes the Indicator menu placeholders in Country and Home pages
# the functionality allows for the user to click and download the complete indicator file
# in compact form. The manu will show the first year of observation and the last (with 10 years leaway)
# and the number of observations in first year and end period.

YearsBeforeEndYear <- 10 # set the number of years before the end year to sum the number of available indicators

# variable with indicator information:
# IndicatorsList
# and for the duration of the data indicators:
# GlobalDurations

# file name structure:

IndicatorFilename <- paste0("XXXNameOfIndicatorXXX","_Compact.xlsx")
source(paste0(GenericPath,"f_IndicatorMenu2.R"))
XxZzYyAgricultureXxZzYy <- f_IndicatorMenu2("Agriculture",ClioMetaData,GlobalDurations,IndicatorFilename)
XxZzYyDemographyXxZzYy <- f_IndicatorMenu2("Demography",ClioMetaData,GlobalDurations,IndicatorFilename)
XxZzYyEnvironmentXxZzYy <- f_IndicatorMenu2("Environment",ClioMetaData,GlobalDurations,IndicatorFilename)
XxZzYyFinanceXxZzYy <- f_IndicatorMenu2("Finance",ClioMetaData,GlobalDurations,IndicatorFilename)
XxZzYyGenderEqualityXxZzYy <- f_IndicatorMenu2("Gender Equality",ClioMetaData,GlobalDurations,IndicatorFilename)
XxZzYyHumanCapitalXxZzYy <- f_IndicatorMenu2("Human Capital",ClioMetaData,GlobalDurations,IndicatorFilename)
XxZzYyInstitutionsXxZzYy <- f_IndicatorMenu2("Institutions",ClioMetaData,GlobalDurations,IndicatorFilename)
XxZzYyLabourRelationsXxZzYy <- f_IndicatorMenu2("Labour Relations",ClioMetaData,GlobalDurations,IndicatorFilename)
XxZzYyNationalAccountsXxZzYy <- f_IndicatorMenu2("National Accounts",ClioMetaData,GlobalDurations,IndicatorFilename)
XxZzYyPricesAndWagesXxZzYy <- f_IndicatorMenu2("Prices and Wages",ClioMetaData,GlobalDurations,IndicatorFilename)
XxZzYyProductionXxZzYy <- f_IndicatorMenu2("Production",ClioMetaData,GlobalDurations,IndicatorFilename)

##########################################################################################################
##########################################################################################################
##########################################################################################################
# create the Country Menu items ######################################################################DONE
# create the Country Menu items ######################################################################DONE
# create the Country Menu items ######################################################################DONE
##########################################################################################################
##########################################################################################################
##########################################################################################################
##########################################################################################################

# this script substitutes the region variables with actual country data in all files found in the folders
# I should covert this to a function
# currenty it should run right after the CountryHTML scripts

# GlobalDurations is the variable that should be present from CountryHTML_JSON.R script for this to work (among other things)

YearsBeforeEndYear <- 10 # set the number of years before the end year to sum the number of available indicators

#http://localhost/docs/Greece_AllIndicatorsAvailable_TerritorialRef_1946_2012_CCode_300.xlsx
TemplateLine <- '<p class="list-group-item"><a href="../Countries/TheCountryFile.html" >TheCountryName</a><span class="badge"><a href="../docs/CountryDataFileName" data-toggle="tooltip" title="Download all available indicators for the country in compact layout. Click on country name for more options."><font color="FFFFFF">Start_Year (IndNum_S_Year)-End_Year (IndNum_E_Year)</font></a></span></p>'
TemplateLineEmpty <- '<p class="list-group-item">TheCountryName<span class="badge"><font color="FFFFFF">[No Data]</font></span></p>'

# only treat the 2012 borders!
SubRegions <- c(unique(GlobalMetadata$subregion),unique(GlobalMetadata$subsubregion),"Caribbean2")
SubRegions <- SubRegions[!SubRegions=="L.America &amp; Carib."]
SubRegions <- as.data.frame(SubRegions[!is.na(SubRegions)], stringsAsFactors = F)
names(SubRegions) <- "SubReg"
SubRegions$VarName <- as.character(NA)
SubRegions$VarName[which(SubRegions$SubReg=="Southern Asia")] <- "XxZzYySouthernAsiaXxZzYy"
SubRegions$VarName[which(SubRegions$SubReg=="Southern Europe")] <- "XxZzYySouthernEuropeXxZzYy"
SubRegions$VarName[which(SubRegions$SubReg=="Northern Africa")] <- "XxZzYyNorthernAfricaXxZzYy"
SubRegions$VarName[which(SubRegions$SubReg=="Polynesia")] <- "XxZzYyPolynesiaXxZzYy"
SubRegions$VarName[which(SubRegions$SubReg=="Middle Africa")] <- "XxZzYyMiddleAfricaXxZzYy"
SubRegions$VarName[which(SubRegions$SubReg=="Central America")] <- "XxZzYyCentralAmericaXxZzYy"
SubRegions$VarName[which(SubRegions$SubReg=="Western Asia")] <- "XxZzYyWesternAsiaXxZzYy"
SubRegions$VarName[which(SubRegions$SubReg=="Australia and N.Zealand")] <- "XxZzYyAustNewZeaXxZzYy"
SubRegions$VarName[which(SubRegions$SubReg=="Western Europe")] <- "XxZzYyWesternEuropeXxZzYy"
SubRegions$VarName[which(SubRegions$SubReg=="Northern America")] <- "XxZzYyNorthernAmericaXxZzYy"
SubRegions$VarName[which(SubRegions$SubReg=="Southern Africa")] <- "XxZzYySouthernAfricaXxZzYy"
SubRegions$VarName[which(SubRegions$SubReg=="Melanesia")] <- "XxZzYyMelanesiaXxZzYy"
SubRegions$VarName[which(SubRegions$SubReg=="South-Eastern Asia")] <- "XxZzYySouthEasternAsiaXxZzYy"
SubRegions$VarName[which(SubRegions$SubReg=="Eastern Europe")] <- "XxZzYyEasternEuropeXxZzYy"
SubRegions$VarName[which(SubRegions$SubReg=="Eastern Africa")] <- "XxZzYyEasternAfricaXxZzYy"
SubRegions$VarName[which(SubRegions$SubReg=="Western Africa")] <- "XxZzYyWesternAfricaXxZzYy"
SubRegions$VarName[which(SubRegions$SubReg=="Eastern Asia")] <- "XxZzYyEasternAsiaXxZzYy"
SubRegions$VarName[which(SubRegions$SubReg=="Northern Europe")] <- "XxZzYyNorthernEuropeXxZzYy"
SubRegions$VarName[which(SubRegions$SubReg=="Micronesia")] <- "XxZzYyMicronesiaXxZzYy"
SubRegions$VarName[which(SubRegions$SubReg=="Central Asia")] <- "XxZzYyCentralAsiaXxZzYy"
SubRegions$VarName[which(SubRegions$SubReg=="South America")] <- "XxZzYySouthAmericaXxZzYy"
SubRegions$VarName[which(SubRegions$SubReg=="Caribbean")] <- "XxZzYyCaribbean1XxZzYy"
SubRegions$VarName[which(SubRegions$SubReg=="Caribbean2")] <- "XxZzYyCaribbean2XxZzYy"

SubRegions2 <- unique(SubRegions$SubReg)
SubRegions$XYZ <- as.character(NA)

for (i_regions in SubRegions2){
  # get the set of countries of that region with the proper names for the menu:
  temp <- GlobalMetadata$country_name[which(GlobalMetadata$subregion == i_regions)]
  temp <- temp[!is.na(temp)]
  temp <- sort(temp)
  
  for (L_i in temp){
    #TemplateLineEmpty
    TheCountryName <- L_i
    #TemplateLine
    if (!is.na(GlobalMetadata$ClioInfraCountryName[which(GlobalMetadata$country_name == L_i)]) & 
        GlobalMetadata$DataPoints[which(GlobalMetadata$country_name == L_i)] > 0 &
        "2012" %in% unlist(strsplit(GlobalMetadata$WebmapperEndYears[which(GlobalMetadata$country_name == L_i)],";")[[1]])){
      TheCountryFile <- trimalls(gsub("[[:punct:]]", "", GlobalMetadata$ClioInfraCountryName[which(GlobalMetadata$country_name == L_i)]))
      Start_Year <- min(GlobalDurations$StartYear[which( nchar(GlobalDurations$StartYear)>0 &
                                                           GlobalDurations$CountryName==GlobalMetadata$ClioInfraCountryName[which(GlobalMetadata$country_name == L_i)])])
      IndNum_S_Year <- nrow(subset(GlobalDurations,GlobalDurations$CountryName==GlobalMetadata$ClioInfraCountryName[which(GlobalMetadata$country_name == L_i)] &
                                     GlobalDurations$StartYear == Start_Year))
      End_Year <- max(GlobalDurations$EndYear[which( nchar(GlobalDurations$EndYear)>0 &
                                                       GlobalDurations$CountryName==GlobalMetadata$ClioInfraCountryName[which(GlobalMetadata$country_name == L_i)])])
      IndNum_E_Year <- nrow(subset(GlobalDurations,GlobalDurations$CountryName==GlobalMetadata$ClioInfraCountryName[which(GlobalMetadata$country_name == L_i)] &
                                     as.integer(GlobalDurations$EndYear) > as.integer(End_Year)-YearsBeforeEndYear))
      CountryDataFileName <- unique(GlobalDurations$CountryFileName[which(GlobalDurations$CountryName==GlobalMetadata$ClioInfraCountryName[which(GlobalMetadata$country_name == L_i)])])
    } else {
      TheCountryFile <- ""
    }
    
    if (nchar(TheCountryFile)>0){
      CurTemplateLine <- TemplateLine
      CurTemplateLine <- gsub("TheCountryName", TheCountryName, CurTemplateLine)
      CurTemplateLine <- gsub("TheCountryFile", TheCountryFile, CurTemplateLine)
      CurTemplateLine <- gsub("Start_Year", Start_Year, CurTemplateLine)
      CurTemplateLine <- gsub("IndNum_S_Year", IndNum_S_Year, CurTemplateLine)
      CurTemplateLine <- gsub("End_Year", End_Year, CurTemplateLine)
      CurTemplateLine <- gsub("IndNum_E_Year", IndNum_E_Year, CurTemplateLine)
      CurTemplateLine <- gsub("CountryDataFileName", CountryDataFileName, CurTemplateLine)
      
    } else {
      CurTemplateLine <- TemplateLineEmpty
      CurTemplateLine <- gsub("TheCountryName", TheCountryName, CurTemplateLine)
    }
    if (!is.na(SubRegions$XYZ[which(SubRegions$SubReg==i_regions)])){
      SubRegions$XYZ[which(SubRegions$SubReg==i_regions)] <- paste(SubRegions$XYZ[which(SubRegions$SubReg==i_regions)],
                                                                   CurTemplateLine,sep = "")
    } else {
      SubRegions$XYZ[which(SubRegions$SubReg==i_regions)] <- CurTemplateLine
    }
  }
}

for (i_regions in c("South America","Central America","Caribbean")){
  temp <- GlobalMetadata$country_name[which(GlobalMetadata$subsubregion == i_regions)]
  temp <- temp[!is.na(temp)]
  temp <- sort(temp)
  for (L_i in temp){
    #TemplateLineEmpty
    TheCountryName <- L_i
    #TemplateLine
    if (!is.na(GlobalMetadata$ClioInfraCountryName[which(GlobalMetadata$country_name == L_i)]) & 
        GlobalMetadata$DataPoints[which(GlobalMetadata$country_name == L_i)] > 0 &
        "2012" %in% unlist(strsplit(GlobalMetadata$WebmapperEndYears[which(GlobalMetadata$country_name == L_i)],";")[[1]])){
      TheCountryFile <- trimalls(gsub("[[:punct:]]", "", GlobalMetadata$ClioInfraCountryName[which(GlobalMetadata$country_name == L_i)]))
      Start_Year <- min(GlobalDurations$StartYear[which( nchar(GlobalDurations$StartYear)>0 &
                                                           GlobalDurations$CountryName==GlobalMetadata$ClioInfraCountryName[which(GlobalMetadata$country_name == L_i)])])
      IndNum_S_Year <- nrow(subset(GlobalDurations,GlobalDurations$CountryName==GlobalMetadata$ClioInfraCountryName[which(GlobalMetadata$country_name == L_i)] &
                                     GlobalDurations$StartYear == Start_Year))
      End_Year <- max(GlobalDurations$EndYear[which( nchar(GlobalDurations$EndYear)>0 &
                                                       GlobalDurations$CountryName==GlobalMetadata$ClioInfraCountryName[which(GlobalMetadata$country_name == L_i)])])
      IndNum_E_Year <- nrow(subset(GlobalDurations,GlobalDurations$CountryName==GlobalMetadata$ClioInfraCountryName[which(GlobalMetadata$country_name == L_i)] &
                                     as.integer(GlobalDurations$EndYear) > as.integer(End_Year)-YearsBeforeEndYear))
      CountryDataFileName <- unique(GlobalDurations$CountryFileName[which(GlobalDurations$CountryName==GlobalMetadata$ClioInfraCountryName[which(GlobalMetadata$country_name == L_i)])])
    } else {
      TheCountryFile <- ""
    }
    
    if (nchar(TheCountryFile)>0){
      CurTemplateLine <- TemplateLine
      CurTemplateLine <- gsub("TheCountryName", TheCountryName, CurTemplateLine)
      CurTemplateLine <- gsub("TheCountryFile", TheCountryFile, CurTemplateLine)
      CurTemplateLine <- gsub("Start_Year", Start_Year, CurTemplateLine)
      CurTemplateLine <- gsub("IndNum_S_Year", IndNum_S_Year, CurTemplateLine)
      CurTemplateLine <- gsub("End_Year", End_Year, CurTemplateLine)
      CurTemplateLine <- gsub("IndNum_E_Year", IndNum_E_Year, CurTemplateLine)
      CurTemplateLine <- gsub("CountryDataFileName", CountryDataFileName, CurTemplateLine)
    } else {
      CurTemplateLine <- TemplateLineEmpty
      CurTemplateLine <- gsub("TheCountryName", TheCountryName, CurTemplateLine)
    }
    if (!is.na(SubRegions$XYZ[which(SubRegions$SubReg==i_regions)])){
      SubRegions$XYZ[which(SubRegions$SubReg==i_regions)] <- paste(SubRegions$XYZ[which(SubRegions$SubReg==i_regions)],
                                                                   CurTemplateLine,sep = "")
    } else {
      SubRegions$XYZ[which(SubRegions$SubReg==i_regions)] <- CurTemplateLine
    }
  }
}

# dont forget to split the Caribbean:
# move the last 10 items from Carribbean to Carribbean2

CaribNum <- length(strsplit(SubRegions$XYZ[which(SubRegions$SubReg=="Caribbean")],"</p>")[[1]])
CaribList <- unlist(strsplit(SubRegions$XYZ[which(SubRegions$SubReg=="Caribbean")],"</p>")[[1]])
SubRegions$XYZ[which(SubRegions$SubReg=="Caribbean2")] <- paste(CaribList[(CaribNum-9):CaribNum],sep="",collapse = "</p>")
SubRegions$XYZ[which(SubRegions$SubReg=="Caribbean")] <- paste(CaribList[1:(CaribNum-10)],sep="",collapse = "</p>")

# Now treat the OECD regions:
# Now treat the OECD regions:
# Now treat the OECD regions:
# Now treat the OECD regions:

TemplateLine <- '<p class="list-group-item"><a href="../Countries/TheCountryFile.html" >TheCountryName</a><span class="badge"><a href="../docs/CountryDataFileName" data-toggle="tooltip" title="Download all available indicators for the country in compact layout. Click on country name for more options."><font color="FFFFFF">Start_Year (IndNum_S_Year)-End_Year (IndNum_E_Year)</font></a></span></p>'
TemplateLineEmpty <- '<p class="list-group-item">TheCountryName<span class="badge"><font color="FFFFFF">[No Data]</font></span></p>'

# only treat the 2012 borders!
OECDMenu <- unique(GlobalMetadata$OECD_Region)
#OECDMenu <- OECDMenu[!OECDMenu=="L.America &amp; Carib."]
OECDMenu <- as.data.frame(OECDMenu[!is.na(OECDMenu)], stringsAsFactors = F)
names(OECDMenu) <- "SubReg"
OECDMenu$VarName <- as.character(NA)
OECDMenu$VarName[which(OECDMenu$SubReg=="S. &amp; S.E.Asia")] <- "XxZzYySSEAsia1XxZzYy"
OECDMenu$VarName[which(OECDMenu$SubReg=="E.Europe &amp; f.SU")] <- "XxZzYyFSU1XxZzYy"
OECDMenu$VarName[which(OECDMenu$SubReg=="M.East &amp; N.Africa")] <- "XxZzYyMENA1XxZzYy"
OECDMenu$VarName[which(OECDMenu$SubReg=="W. Europe")] <- "XxZzYyWEurope1XxZzYy"
OECDMenu$VarName[which(OECDMenu$SubReg=="Sub-Sah. Africa")] <- "XxZzYySSA1XxZzYy"
OECDMenu$VarName[which(OECDMenu$SubReg=="L.America &amp; Carib.")] <- "XxZzYyLAC1XxZzYy"
OECDMenu$VarName[which(OECDMenu$SubReg=="W. Offshoots")] <- "XxZzYyWOffshoots1XxZzYy"
OECDMenu$VarName[which(OECDMenu$SubReg=="E.Asia")] <- "XxZzYyEAsia1XxZzYy"
OECDMenu2 <- unique(OECDMenu$SubReg)
OECDMenu$XYZ <- as.character(NA)

for (i_regions in OECDMenu2){
  # get the set of countries of that region with the proper names for the menu:
  temp <- GlobalMetadata$country_name[which(GlobalMetadata$OECD_Region == i_regions)]
  temp <- temp[!is.na(temp)]
  temp <- sort(temp)
  
  for (L_i in temp){
    #TemplateLineEmpty
    TheCountryName <- L_i
    #TemplateLine
    if (!is.na(GlobalMetadata$ClioInfraCountryName[which(GlobalMetadata$country_name == L_i)]) & 
        GlobalMetadata$DataPoints[which(GlobalMetadata$country_name == L_i)] > 0 &
        "2012" %in% unlist(strsplit(GlobalMetadata$WebmapperEndYears[which(GlobalMetadata$country_name == L_i)],";")[[1]])){
      TheCountryFile <- trimalls(gsub("[[:punct:]]", "", GlobalMetadata$ClioInfraCountryName[which(GlobalMetadata$country_name == L_i)]))
      Start_Year <- min(GlobalDurations$StartYear[which( nchar(GlobalDurations$StartYear)>0 &
                                                           GlobalDurations$CountryName==GlobalMetadata$ClioInfraCountryName[which(GlobalMetadata$country_name == L_i)])])
      IndNum_S_Year <- nrow(subset(GlobalDurations,GlobalDurations$CountryName==GlobalMetadata$ClioInfraCountryName[which(GlobalMetadata$country_name == L_i)] &
                                     GlobalDurations$StartYear == Start_Year))
      End_Year <- max(GlobalDurations$EndYear[which( nchar(GlobalDurations$EndYear)>0 &
                                                       GlobalDurations$CountryName==GlobalMetadata$ClioInfraCountryName[which(GlobalMetadata$country_name == L_i)])])
      IndNum_E_Year <- nrow(subset(GlobalDurations,GlobalDurations$CountryName==GlobalMetadata$ClioInfraCountryName[which(GlobalMetadata$country_name == L_i)] &
                                     as.integer(GlobalDurations$EndYear) > as.integer(End_Year)-YearsBeforeEndYear))
      CountryDataFileName <- unique(GlobalDurations$CountryFileName[which(GlobalDurations$CountryName==GlobalMetadata$ClioInfraCountryName[which(GlobalMetadata$country_name == L_i)])])
    } else {
      TheCountryFile <- ""
    }
    
    if (nchar(TheCountryFile)>0){
      CurTemplateLine <- TemplateLine
      CurTemplateLine <- gsub("TheCountryName", TheCountryName, CurTemplateLine)
      CurTemplateLine <- gsub("TheCountryFile", TheCountryFile, CurTemplateLine)
      CurTemplateLine <- gsub("Start_Year", Start_Year, CurTemplateLine)
      CurTemplateLine <- gsub("IndNum_S_Year", IndNum_S_Year, CurTemplateLine)
      CurTemplateLine <- gsub("End_Year", End_Year, CurTemplateLine)
      CurTemplateLine <- gsub("IndNum_E_Year", IndNum_E_Year, CurTemplateLine)
      CurTemplateLine <- gsub("CountryDataFileName", CountryDataFileName, CurTemplateLine)
      
    } else {
      CurTemplateLine <- TemplateLineEmpty
      CurTemplateLine <- gsub("TheCountryName", TheCountryName, CurTemplateLine)
    }
    if (!is.na(OECDMenu$XYZ[which(OECDMenu$SubReg==i_regions)])){
      OECDMenu$XYZ[which(OECDMenu$SubReg==i_regions)] <- paste(OECDMenu$XYZ[which(OECDMenu$SubReg==i_regions)],
                                                               CurTemplateLine,sep = "")
    } else {
      OECDMenu$XYZ[which(OECDMenu$SubReg==i_regions)] <- CurTemplateLine
    }
  }
}

# they need to be split in 3 columns each alphabetically:
# so dont forget to split them into three groups:

# do the OECD split:
OECDSplit <- aggregate(cbind(count = OECD_Country_Name) ~ OECD_Region, data = GlobalMetadata, FUN = function(x){NROW(x)})

# I need to add the rows here for the XYZ2 and 3:
XYZ2 <- gsub("1","2",unique(OECDMenu$VarName))
XYZ3 <- gsub("1","3",unique(OECDMenu$VarName))
XYZs <- c(XYZ2,XYZ3)

for (i in 1:length(XYZs)){
  OECDMenu <- rbind(OECDMenu,c(NA,XYZs[i],NA))
}
rm(XYZs,XYZ2,XYZ3)

OECDMenu$SubReg[grep("FSU",OECDMenu$VarName)] <- OECDMenu$SubReg[grep("FSU",OECDMenu$VarName)][1]
OECDMenu$SubReg[grep("SSEAsia",OECDMenu$VarName)] <- OECDMenu$SubReg[grep("SSEAsia",OECDMenu$VarName)][1]
OECDMenu$SubReg[grep("MENA",OECDMenu$VarName)] <- OECDMenu$SubReg[grep("MENA",OECDMenu$VarName)][1]
OECDMenu$SubReg[grep("WEurope",OECDMenu$VarName)] <- OECDMenu$SubReg[grep("WEurope",OECDMenu$VarName)][1]
OECDMenu$SubReg[grep("SSA",OECDMenu$VarName)] <- OECDMenu$SubReg[grep("SSA",OECDMenu$VarName)][1]
OECDMenu$SubReg[grep("LAC",OECDMenu$VarName)] <- OECDMenu$SubReg[grep("LAC",OECDMenu$VarName)][1]
OECDMenu$SubReg[grep("WOffshoots",OECDMenu$VarName)] <- OECDMenu$SubReg[grep("WOffshoots",OECDMenu$VarName)][1]
OECDMenu$SubReg[grep("yEAsia",OECDMenu$VarName)] <- OECDMenu$SubReg[grep("yEAsia",OECDMenu$VarName)][1]

for (i in 1:nrow(OECDSplit)){
  XYZNum <- length(strsplit(OECDMenu$XYZ[which(OECDMenu$SubReg==OECDSplit[i,1])],"</p>")[[1]])
  XYZsteps <- c(floor(XYZNum/3),ceiling(2*XYZNum/3),XYZNum)
  XYZList <- unlist(strsplit(OECDMenu$XYZ[which(OECDMenu$SubReg==OECDSplit[i,1])],"</p>")[[1]])
  
  OECDMenu$XYZ[which(OECDMenu$SubReg==OECDSplit[i,1])][1] <- 
    paste(XYZList[1:XYZsteps[1]],sep="",collapse = "</p>")
  OECDMenu$XYZ[which(OECDMenu$SubReg==OECDSplit[i,1])][2] <- 
    paste(XYZList[(XYZsteps[1]+1):XYZsteps[2]],sep="",collapse = "</p>")
  OECDMenu$XYZ[which(OECDMenu$SubReg==OECDSplit[i,1])][3] <- 
    paste(XYZList[(XYZsteps[2]+1):XYZsteps[3]],sep="",collapse = "</p>")
}

#### Now the menu for the Historical entities:
#### Now the menu for the Historical entities:
#### Now the menu for the Historical entities:

# they should have data, but not a border end in 2012:
Historical <- subset(GlobalMetadata, GlobalMetadata$DataPoints>0)
#Historical <- subset(Historical, !is.na(Historical$WebmapperEndYears))
Historical <- Historical[-grep("2012",Historical$WebmapperEndYears),]

# to avoid misunderstandings about what this list of entities refers to 
# I will add the start and end border years after the name: USSR [1940-1991] 

TemplateLine <- '<p class="list-group-item"><a name="NoFile">TheCountryNameHistorical</a><span class="badge"><a href="../docs/CountryDataFileName" data-toggle="tooltip" title="Download all available indicators for the country in compact layout."><font color="FFFFFF">get data</font></a></span></p>'

temp <- Historical$ClioInfraCountryName
temp <- temp[!is.na(temp)]
temp <- sort(temp)
XYZ <- c()

for (L_i in temp){
  # here things are simpler as I only need to provide the country names and the filename that is created here:
  TheCountryName <- L_i
  
  TheCountryFile <- trimalls(gsub("[[:punct:]]", "", TheCountryName))
  Start_Year <- min(as.numeric(strsplit(Historical$WebmapperStartYears[which(Historical$ClioInfraCountryName==L_i)],";")[[1]]))
  End_Year <- max(as.numeric(strsplit(Historical$WebmapperEndYears[which(Historical$ClioInfraCountryName==L_i)],";")[[1]]))
  CountryDataFileName <- paste0(TheCountryFile,"_",Start_Year,"_",End_Year,"_all_data.xlsx")
  TheCountryNameHistorical <- paste0(TheCountryName," [",Start_Year,"-",End_Year,"]")
  
  # I also need to create the data files here (all the data available for each entity go in one xlsx for simplicity)
  
  if (CreateHistoricalDataFiles){
    MapCodes <- strsplit(Historical$WebmapperNums[which(Historical$ClioInfraCountryName==L_i)],";")[[1]]
    HistoricalData <- subset(ClioData,ClioData$`Webmapper numeric code` %in% MapCodes)
    HistoricalData <- HistoricalData[rowSums(is.na(HistoricalData[,9:524]))<515,]
  
  
    write.xlsx2(HistoricalData,file=paste0(GenericPath,"CountryData/",CountryDataFileName), 
               sheetName="Data Clio Infra Format", row.names=F, showNA=F)
    
    # which are the available indicators to cite:
    AvailIndicators <- unique(HistoricalData$Indicator)
    
    # create metadata sheet starting with the download url of the file:
    
    Metadata <- t(c("Downloaded from","N/A",paste0(URL_basis,"/data/DataAtHistoricalBorders.xlsx")))
    Metadata <- as.data.frame(Metadata, stringsAsFactors=F)
    names(Metadata) <- c("Description","Indicator Name","Value")
    
    FullMetadata <- Metadata
    
    #### SIMPLE TEXT CITATION
    
    for (i in AvailIndicators){
      CitationFileName <- Citations$CitationFilenamePrefix[which(Citations$Indicator==i)]
      CitationFileName <- as.character(CitationFileName)
      
      bib <- ReadBib(paste0(GenericPath,"Citations/",CitationFileName,".bib"))
      
      TheAuthorAndDate <- capture.output(print(bib, .opts = list(bib.style = "authoryear", first.inits = FALSE, no.print.fields = c("title","publisher","url"))))
      xxxTemp <- unlist(bib)
      
      Metadata <- t(c("Text Citation",i,paste(TheAuthorAndDate," ",xxxTemp$title,". ", xxxTemp$url,", accessed via the Clio Infra website.", sep = "")))
      Metadata <- as.data.frame(Metadata, stringsAsFactors=F)
      names(Metadata) <- c("Description","Indicator Name","Value")
      
      FullMetadata <- rbind(FullMetadata,Metadata)
    }
    
    # make the citation files metadata entries:
    for (i in AvailIndicators){
      CitationFileName <- Citations$CitationFilenamePrefix[which(Citations$Indicator==i)]
      CitationFileName <- as.character(CitationFileName)
      
      Metadata <- t(c("XML Citation",i,paste0(URL_basis,"/citations/",CitationFileName,".xml")))
      Metadata <- as.data.frame(Metadata, stringsAsFactors=F)
      names(Metadata) <- c("Description","Indicator Name","Value")
      
      FullMetadata <- rbind(FullMetadata,Metadata)
      
      Metadata <- t(c("RIS Citation",i,paste0(URL_basis,"/citations/",CitationFileName,".ris")))
      Metadata <- as.data.frame(Metadata, stringsAsFactors=F)
      names(Metadata) <- c("Description","Indicator Name","Value")
      
      FullMetadata <- rbind(FullMetadata,Metadata)
      
      Metadata <- t(c("BIB Citation",i,paste0(URL_basis,"/citations/",CitationFileName,".bib")))
      Metadata <- as.data.frame(Metadata, stringsAsFactors=F)
      names(Metadata) <- c("Description","Indicator Name","Value")
      
      FullMetadata <- rbind(FullMetadata,Metadata)
    }
    
    write.xlsx2(FullMetadata, file=paste0(GenericPath,"CountryData/",CountryDataFileName),
               sheetName="Metadata", append=TRUE, row.names=F)
  }
  
  CurTemplateLine <- TemplateLine
  CurTemplateLine <- gsub("TheCountryNameHistorical", TheCountryNameHistorical, CurTemplateLine)
  CurTemplateLine <- gsub("TheCountryFile", TheCountryFile, CurTemplateLine)
  CurTemplateLine <- gsub("Start_Year", Start_Year, CurTemplateLine)
  CurTemplateLine <- gsub("End_Year", End_Year, CurTemplateLine)
  CurTemplateLine <- gsub("CountryDataFileName", CountryDataFileName, CurTemplateLine)
  
  XYZ <- c(XYZ,CurTemplateLine)
  
}

# bundle the html code together into three groups

XxZzYyLOC1XxZzYy <- paste(XYZ[1:floor(nrow(Historical)/3)],sep="",collapse = "</p>")
XxZzYyLOC2XxZzYy <- paste(XYZ[(floor(nrow(Historical)/3)+1):ceiling(2*nrow(Historical)/3)],sep="",collapse = "</p>")
XxZzYyLOC3XxZzYy <- paste(XYZ[(ceiling(2*nrow(Historical)/3)+1):nrow(Historical)],sep="",collapse = "</p>")

# substitute the Menu items #######################################################################
# substitute the Menu items #######################################################################
# substitute the Menu items #######################################################################
# substitute the Menu items #######################################################################

# the processed files are ready for indicator index substitution in:
# IndexTemplate.html
# *.html in GenericPath + CountryPagesWithMenus

# also substitute the copyright element
#XxZzYyCopyrightYearsXxZzYy <- "2010-2017"

files <- list.files(paste0(GenericPath,"Pages Exports R"))
for (i_file in files){
  i_file_long <- paste0(GenericPath,"Pages Exports R","/",i_file)
  testfile <- readChar(i_file_long, file.info(i_file_long)$size)
  # Countries:
  testfile <- gsub("XxZzYyEasternAfricaXxZzYy", SubRegions$XYZ[which(SubRegions$VarName=="XxZzYyEasternAfricaXxZzYy")], testfile)
  testfile <- gsub("XxZzYyMiddleAfricaXxZzYy", SubRegions$XYZ[which(SubRegions$VarName=="XxZzYyMiddleAfricaXxZzYy")], testfile)
  testfile <- gsub("XxZzYySouthernAfricaXxZzYy", SubRegions$XYZ[which(SubRegions$VarName=="XxZzYySouthernAfricaXxZzYy")], testfile)
  testfile <- gsub("XxZzYyNorthernAfricaXxZzYy", SubRegions$XYZ[which(SubRegions$VarName=="XxZzYyNorthernAfricaXxZzYy")], testfile)
  testfile <- gsub("XxZzYyWesternAfricaXxZzYy", SubRegions$XYZ[which(SubRegions$VarName=="XxZzYyWesternAfricaXxZzYy")], testfile)
  testfile <- gsub("XxZzYyCaribbean1XxZzYy", SubRegions$XYZ[which(SubRegions$VarName=="XxZzYyCaribbean1XxZzYy")], testfile)
  testfile <- gsub("XxZzYyCaribbean2XxZzYy", SubRegions$XYZ[which(SubRegions$VarName=="XxZzYyCaribbean2XxZzYy")], testfile)
  testfile <- gsub("XxZzYySouthAmericaXxZzYy", SubRegions$XYZ[which(SubRegions$VarName=="XxZzYySouthAmericaXxZzYy")], testfile)
  testfile <- gsub("XxZzYyCentralAmericaXxZzYy", SubRegions$XYZ[which(SubRegions$VarName=="XxZzYyCentralAmericaXxZzYy")], testfile)
  testfile <- gsub("XxZzYyNorthernAmericaXxZzYy", SubRegions$XYZ[which(SubRegions$VarName=="XxZzYyNorthernAmericaXxZzYy")], testfile)
  testfile <- gsub("XxZzYyCentralAsiaXxZzYy", SubRegions$XYZ[which(SubRegions$VarName=="XxZzYyCentralAsiaXxZzYy")], testfile)
  testfile <- gsub("XxZzYySouthernAsiaXxZzYy", SubRegions$XYZ[which(SubRegions$VarName=="XxZzYySouthernAsiaXxZzYy")], testfile)
  testfile <- gsub("XxZzYyWesternAsiaXxZzYy", SubRegions$XYZ[which(SubRegions$VarName=="XxZzYyWesternAsiaXxZzYy")], testfile)
  testfile <- gsub("XxZzYyEasternAsiaXxZzYy", SubRegions$XYZ[which(SubRegions$VarName=="XxZzYyEasternAsiaXxZzYy")], testfile)
  testfile <- gsub("XxZzYySouthEasternAsiaXxZzYy", SubRegions$XYZ[which(SubRegions$VarName=="XxZzYySouthEasternAsiaXxZzYy")], testfile)
  testfile <- gsub("XxZzYyEasternEuropeXxZzYy", SubRegions$XYZ[which(SubRegions$VarName=="XxZzYyEasternEuropeXxZzYy")], testfile)
  testfile <- gsub("XxZzYyNorthernEuropeXxZzYy", SubRegions$XYZ[which(SubRegions$VarName=="XxZzYyNorthernEuropeXxZzYy")], testfile)
  testfile <- gsub("XxZzYySouthernEuropeXxZzYy", SubRegions$XYZ[which(SubRegions$VarName=="XxZzYySouthernEuropeXxZzYy")], testfile)
  testfile <- gsub("XxZzYyWesternEuropeXxZzYy", SubRegions$XYZ[which(SubRegions$VarName=="XxZzYyWesternEuropeXxZzYy")], testfile)
  testfile <- gsub("XxZzYyAustNewZeaXxZzYy", SubRegions$XYZ[which(SubRegions$VarName=="XxZzYyAustNewZeaXxZzYy")], testfile)
  testfile <- gsub("XxZzYyMicronesiaXxZzYy", SubRegions$XYZ[which(SubRegions$VarName=="XxZzYyMicronesiaXxZzYy")], testfile)
  testfile <- gsub("XxZzYyPolynesiaXxZzYy", SubRegions$XYZ[which(SubRegions$VarName=="XxZzYyPolynesiaXxZzYy")], testfile)
  testfile <- gsub("XxZzYyMelanesiaXxZzYy", SubRegions$XYZ[which(SubRegions$VarName=="XxZzYyMelanesiaXxZzYy")], testfile)
  
  # OECD Countries:
  for (i_oecd in 1:nrow(OECDMenu)){
    testfile <- gsub(OECDMenu$VarName[i_oecd], OECDMenu$XYZ[i_oecd], testfile)
  }
  
  # Historical:
  testfile <- gsub("XxZzYyLOC1XxZzYy", XxZzYyLOC1XxZzYy, testfile)
  testfile <- gsub("XxZzYyLOC2XxZzYy", XxZzYyLOC2XxZzYy, testfile)
  testfile <- gsub("XxZzYyLOC3XxZzYy", XxZzYyLOC3XxZzYy, testfile)
  
  # Indicators:
  testfile <- gsub("XxZzYyAgricultureXxZzYy", XxZzYyAgricultureXxZzYy, testfile)
  testfile <- gsub("XxZzYyDemographyXxZzYy", XxZzYyDemographyXxZzYy, testfile)
  testfile <- gsub("XxZzYyEnvironmentXxZzYy", XxZzYyEnvironmentXxZzYy, testfile)
  testfile <- gsub("XxZzYyFinanceXxZzYy", XxZzYyFinanceXxZzYy, testfile)
  testfile <- gsub("XxZzYyGenderEqualityXxZzYy", XxZzYyGenderEqualityXxZzYy, testfile)
  testfile <- gsub("XxZzYyHumanCapitalXxZzYy", XxZzYyHumanCapitalXxZzYy, testfile)
  testfile <- gsub("XxZzYyInstitutionsXxZzYy", XxZzYyInstitutionsXxZzYy, testfile)
  testfile <- gsub("XxZzYyLabourRelationsXxZzYy", XxZzYyLabourRelationsXxZzYy, testfile)
  testfile <- gsub("XxZzYyNationalAccountsXxZzYy", XxZzYyNationalAccountsXxZzYy, testfile)
  testfile <- gsub("XxZzYyPricesAndWagesXxZzYy", XxZzYyPricesAndWagesXxZzYy, testfile)
  testfile <- gsub("XxZzYyProductionXxZzYy", XxZzYyProductionXxZzYy, testfile)
  # Copyright:
  # testfile <- gsub("XxZzYyCopyrightYearsXxZzYy", XxZzYyCopyrightYearsXxZzYy, testfile)
  
  write(testfile, paste0(GenericPath,"IndicatorPagesWithMenus","/",i_file))
}

# last one for Index page:
# last one for Index page:
# last one for Index page:
# last one for Index page:
# last one for Index page:

i_file <- paste0(GenericPath,"IndexTemplate.html")
testfile <- readChar(i_file, file.info(i_file)$size)

# About:
#XxZzYyAboutClioInfraXxZzYy <- readChar(paste(GenericPath,"AboutClioInfra.txt",sep="/"), file.info(paste(GenericPath,"AboutClioInfra.txt",sep="/"))$size)

# top menu figures:
#XxZzYyTotalNumOfCountriesXxZzYy <- as.character(nrow(subset(GlobalMetadata,GlobalMetadata$DataPoints>0)))
#XxZzYyTotalDatasetsXxZzYy <- as.character(length(IndicatorsList))

# Figures:
XxZzYyGraph0XxZzYy <- "Global_GDPperCapita.svg"
XxZzYyGraph1XxZzYy <- "Global_Height.svg"
XxZzYyGraph2XxZzYy <- "Global_InfantMortality.svg"
XxZzYyGraph3XxZzYy <- "Global_IncomeInequality.svg"
XxZzYyGraph4XxZzYy <- "Global_UrbanizationRatio.svg"
XxZzYyGraph5XxZzYy <- "Global_AverageYearsofEducation.svg"
XxZzYyGraph6XxZzYy <- "Global_LifeExpectancyatBirth(Total).svg"
XxZzYyGraph7XxZzYy <- "Global_Long-TermGovernmentBondYield.svg"
XxZzYyGraph8XxZzYy <- "Global_ShareofWomeninParliament.svg"

testfile <- gsub("XxZzYyGraph0XxZzYy", XxZzYyGraph0XxZzYy, testfile)
testfile <- gsub("XxZzYyGraph1XxZzYy", XxZzYyGraph1XxZzYy, testfile)
testfile <- gsub("XxZzYyGraph2XxZzYy", XxZzYyGraph2XxZzYy, testfile)
testfile <- gsub("XxZzYyGraph3XxZzYy", XxZzYyGraph3XxZzYy, testfile)
testfile <- gsub("XxZzYyGraph4XxZzYy", XxZzYyGraph4XxZzYy, testfile)
testfile <- gsub("XxZzYyGraph5XxZzYy", XxZzYyGraph5XxZzYy, testfile)
testfile <- gsub("XxZzYyGraph6XxZzYy", XxZzYyGraph6XxZzYy, testfile)
testfile <- gsub("XxZzYyGraph7XxZzYy", XxZzYyGraph7XxZzYy, testfile)
testfile <- gsub("XxZzYyGraph8XxZzYy", XxZzYyGraph8XxZzYy, testfile)

# Countries:
testfile <- gsub("XxZzYyEasternAfricaXxZzYy", SubRegions$XYZ[which(SubRegions$VarName=="XxZzYyEasternAfricaXxZzYy")], testfile)
testfile <- gsub("XxZzYyMiddleAfricaXxZzYy", SubRegions$XYZ[which(SubRegions$VarName=="XxZzYyMiddleAfricaXxZzYy")], testfile)
testfile <- gsub("XxZzYySouthernAfricaXxZzYy", SubRegions$XYZ[which(SubRegions$VarName=="XxZzYySouthernAfricaXxZzYy")], testfile)
testfile <- gsub("XxZzYyNorthernAfricaXxZzYy", SubRegions$XYZ[which(SubRegions$VarName=="XxZzYyNorthernAfricaXxZzYy")], testfile)
testfile <- gsub("XxZzYyWesternAfricaXxZzYy", SubRegions$XYZ[which(SubRegions$VarName=="XxZzYyWesternAfricaXxZzYy")], testfile)
testfile <- gsub("XxZzYyCaribbean1XxZzYy", SubRegions$XYZ[which(SubRegions$VarName=="XxZzYyCaribbean1XxZzYy")], testfile)
testfile <- gsub("XxZzYyCaribbean2XxZzYy", SubRegions$XYZ[which(SubRegions$VarName=="XxZzYyCaribbean2XxZzYy")], testfile)
testfile <- gsub("XxZzYySouthAmericaXxZzYy", SubRegions$XYZ[which(SubRegions$VarName=="XxZzYySouthAmericaXxZzYy")], testfile)
testfile <- gsub("XxZzYyCentralAmericaXxZzYy", SubRegions$XYZ[which(SubRegions$VarName=="XxZzYyCentralAmericaXxZzYy")], testfile)
testfile <- gsub("XxZzYyNorthernAmericaXxZzYy", SubRegions$XYZ[which(SubRegions$VarName=="XxZzYyNorthernAmericaXxZzYy")], testfile)
testfile <- gsub("XxZzYyCentralAsiaXxZzYy", SubRegions$XYZ[which(SubRegions$VarName=="XxZzYyCentralAsiaXxZzYy")], testfile)
testfile <- gsub("XxZzYySouthernAsiaXxZzYy", SubRegions$XYZ[which(SubRegions$VarName=="XxZzYySouthernAsiaXxZzYy")], testfile)
testfile <- gsub("XxZzYyWesternAsiaXxZzYy", SubRegions$XYZ[which(SubRegions$VarName=="XxZzYyWesternAsiaXxZzYy")], testfile)
testfile <- gsub("XxZzYyEasternAsiaXxZzYy", SubRegions$XYZ[which(SubRegions$VarName=="XxZzYyEasternAsiaXxZzYy")], testfile)
testfile <- gsub("XxZzYySouthEasternAsiaXxZzYy", SubRegions$XYZ[which(SubRegions$VarName=="XxZzYySouthEasternAsiaXxZzYy")], testfile)
testfile <- gsub("XxZzYyEasternEuropeXxZzYy", SubRegions$XYZ[which(SubRegions$VarName=="XxZzYyEasternEuropeXxZzYy")], testfile)
testfile <- gsub("XxZzYyNorthernEuropeXxZzYy", SubRegions$XYZ[which(SubRegions$VarName=="XxZzYyNorthernEuropeXxZzYy")], testfile)
testfile <- gsub("XxZzYySouthernEuropeXxZzYy", SubRegions$XYZ[which(SubRegions$VarName=="XxZzYySouthernEuropeXxZzYy")], testfile)
testfile <- gsub("XxZzYyWesternEuropeXxZzYy", SubRegions$XYZ[which(SubRegions$VarName=="XxZzYyWesternEuropeXxZzYy")], testfile)
testfile <- gsub("XxZzYyAustNewZeaXxZzYy", SubRegions$XYZ[which(SubRegions$VarName=="XxZzYyAustNewZeaXxZzYy")], testfile)
testfile <- gsub("XxZzYyMicronesiaXxZzYy", SubRegions$XYZ[which(SubRegions$VarName=="XxZzYyMicronesiaXxZzYy")], testfile)
testfile <- gsub("XxZzYyPolynesiaXxZzYy", SubRegions$XYZ[which(SubRegions$VarName=="XxZzYyPolynesiaXxZzYy")], testfile)
testfile <- gsub("XxZzYyMelanesiaXxZzYy", SubRegions$XYZ[which(SubRegions$VarName=="XxZzYyMelanesiaXxZzYy")], testfile)

# OECD Countries:
for (i_oecd in 1:nrow(OECDMenu)){
  testfile <- gsub(OECDMenu$VarName[i_oecd], OECDMenu$XYZ[i_oecd], testfile)
}

# Historical:
testfile <- gsub("XxZzYyLOC1XxZzYy", XxZzYyLOC1XxZzYy, testfile)
testfile <- gsub("XxZzYyLOC2XxZzYy", XxZzYyLOC2XxZzYy, testfile)
testfile <- gsub("XxZzYyLOC3XxZzYy", XxZzYyLOC3XxZzYy, testfile)


# Indicators:
testfile <- gsub("XxZzYyAgricultureXxZzYy", XxZzYyAgricultureXxZzYy, testfile)
testfile <- gsub("XxZzYyDemographyXxZzYy", XxZzYyDemographyXxZzYy, testfile)
testfile <- gsub("XxZzYyEnvironmentXxZzYy", XxZzYyEnvironmentXxZzYy, testfile)
testfile <- gsub("XxZzYyFinanceXxZzYy", XxZzYyFinanceXxZzYy, testfile)
testfile <- gsub("XxZzYyGenderEqualityXxZzYy", XxZzYyGenderEqualityXxZzYy, testfile)
testfile <- gsub("XxZzYyHumanCapitalXxZzYy", XxZzYyHumanCapitalXxZzYy, testfile)
testfile <- gsub("XxZzYyInstitutionsXxZzYy", XxZzYyInstitutionsXxZzYy, testfile)
testfile <- gsub("XxZzYyLabourRelationsXxZzYy", XxZzYyLabourRelationsXxZzYy, testfile)
testfile <- gsub("XxZzYyNationalAccountsXxZzYy", XxZzYyNationalAccountsXxZzYy, testfile)
testfile <- gsub("XxZzYyPricesAndWagesXxZzYy", XxZzYyPricesAndWagesXxZzYy, testfile)
testfile <- gsub("XxZzYyProductionXxZzYy", XxZzYyProductionXxZzYy, testfile)

# these are now part of the substitution in FooterSubstitution.R
# Copyright:
#testfile <- gsub("XxZzYyCopyrightYearsXxZzYy", XxZzYyCopyrightYearsXxZzYy, testfile)
# top menu figures:
#testfile <- gsub("XxZzYyTotalDatasetsXxZzYy", "76", testfile)
#testfile <- gsub("XxZzYyTotalNumOfCountriesXxZzYy", "210", testfile)
# About Clio Infra:
#testfile <- gsub("XxZzYyAboutClioInfraXxZzYy", XxZzYyAboutClioInfraXxZzYy, testfile)

i_file <- "index.html"
write(testfile, paste0(GenericPath,i_file))

