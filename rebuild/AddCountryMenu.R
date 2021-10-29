# this script substitutes the region variables with actual country data in all files found in the folders

# run this only after running AddMenusToIndicatorAndHome.R !!!!!
# run this only after running AddMenusToIndicatorAndHome.R !!!!!
# run this only after running AddMenusToIndicatorAndHome.R !!!!!

YearsBeforeEndYear <- 10 # set the number of years before the end year to sum the number of available indicators

setwd(paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/Country Pages Exports R"))
files <- list.files()
files <- files[!files %in% c("xyz.html","Greece2.html")]
#http://localhost/docs/Greece_AllIndicatorsAvailable_TerritorialRef_1946_2012_CCode_300.xlsx
TemplateLine <- '<p class="list-group-item"><a href="./TheCountryFile.html" >TheCountryName</a><span class="badge"><a href="../docs/CountryDataFileName" data-toggle="tooltip" title="Download all available indicators for the country in compact layout. Click on country name for more options."><font color="FFFFFF">Start_Year (IndNum_S_Year)-End_Year (IndNum_E_Year)</font></a></span></p>'
TemplateLineEmpty <- '<p class="list-group-item">TheCountryName<span class="badge"><font color="FFFFFF">[No Data]</font></span></p>'

SubRegions2 <- unique(SubRegions$SubReg)
SubRegions$XYZ <- as.character(NA)

for (i_regions in SubRegions2){
  # get the set of countries of that region with the proper names for the menu:
  temp <- GlobalMetadata$country_name[which(GlobalMetadata$subregion == i_regions)]
  temp <- temp[!is.na(temp)]
  temp <- temp[!temp==""]
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

#XxZzYyCopyrightYearsXxZzYy <- "2010-2016"

#/home/michalis/Downloads/temp
# export country.html in the folder above for test
for (i_file in files){
  testfile <- readChar(i_file, file.info(i_file)$size)
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
  
  #testfile <- gsub("XxZzYyCopyrightYearsXxZzYy", XxZzYyCopyrightYearsXxZzYy, testfile)
  write(testfile, paste("../CountryPagesWithMenus",i_file,sep="/"))
}

