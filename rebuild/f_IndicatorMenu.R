f_IndicatorMenu <- function(WebCat, ClioMetaData, Durations, XxZzYyISO3XxZzYy, CountryIndicatorFilename) {
  
  VarPrefix1 <- '<p class="list-group-item"><a href="../Indicators/'
  VarPrefix2 <- '.html">'
  VarPrefix3 <- '<span class="badge"><a href="../IndicatorsPerCountry/'
  VarPrefix4 <- '"><font color="FFFFFF">'
  VarPrefix5 <- '</font></a></span></p>'
  VarPrefixNoData <- '</p>'
  
  SelectedIndicator <- sort(unique(ClioMetaData$WebName[which(ClioMetaData$WebCategory==WebCat)]))
  # attention to the indicators without data for the current country
  StartYears <- c()
  for (L_sel in 1:length(SelectedIndicator)){
    StartYears <- c(StartYears, Durations$StartYear[which(Durations$Indicator==SelectedIndicator[L_sel])])  
  }
  EndYears <- c()
  for (L_sel in 1:length(SelectedIndicator)){
    EndYears <- c(EndYears, Durations$EndYear[which(Durations$Indicator==SelectedIndicator[L_sel])])  
  }
  Observations <- c()
  for (L_sel in 1:length(SelectedIndicator)){
    Observations <- c(Observations, Durations$Obs[which(Durations$Indicator==SelectedIndicator[L_sel])])  
  }
  
  GenderEqualityMenuItems <- SelectedIndicator
  
  for (L_jjj in 1:length(GenderEqualityMenuItems)){
    GenderEqualityMenuItems[L_jjj] <- 
      IndicPriorityList$ShortForTheMenu[which(IndicPriorityList$WebName==GenderEqualityMenuItems[L_jjj])]
  }
  
  # I need to make the items below vectors with the appropriat value to mix in the paste function below:
  VarPrefix3Local <- c()
  for (L_ij in 1:length(SelectedIndicator)){
    if (Observations[L_ij]=="No Data"){
      VarPrefix3Local <- c(VarPrefix3Local,"")
    } else {
      VarPrefix3Local <- c(VarPrefix3Local,VarPrefix3)
    }
  }
  VarPrefix4Local <- c()
  for (L_ij in 1:length(SelectedIndicator)){
    if (Observations[L_ij]=="No Data"){
      VarPrefix4Local <- c(VarPrefix4Local,"")
    } else {
      VarPrefix4Local <- c(VarPrefix4Local,VarPrefix4)
    }
  }
  VarPrefix5Local <- c()
  for (L_ij in 1:length(SelectedIndicator)){
    if (Observations[L_ij]=="No Data"){
      VarPrefix5Local <- c(VarPrefix5Local,VarPrefixNoData)
    } else {
      VarPrefix5Local <- c(VarPrefix5Local,VarPrefix5)
    }
  }
  
  ISO3Local <- c()
  for (L_ij in 1:length(SelectedIndicator)){
    if (Observations[L_ij]=="No Data"){
      ISO3Local <- c(ISO3Local,"")
    } else {
      ISO3Local <- c(ISO3Local,XxZzYyISO3XxZzYy)
    }
  }
  
  # file name structure: CountryName_XXXNameOfIndicatorXXX_TerritorialRef_Year1_Year2_CCODE.xlsx
  CountryIndicatorsFiles <- c()
  for (L_ij in 1:length(SelectedIndicator)){
    if (Observations[L_ij]=="No Data"){
      CountryIndicatorsFiles <- c(CountryIndicatorsFiles,"")
    } else {
      tempname <- gsub("XXXNameOfIndicatorXXX",paste0(trimalls(gsub("[[:punct:]]", "", SelectedIndicator[L_ij]))),
                       CountryIndicatorFilename)
      CountryIndicatorsFiles <- c(CountryIndicatorsFiles,tempname)
      rm(tempname)
    }
  }
  
  Observations <- paste0(" [",Observations,"] ",sep="")
  Observations <- gsub(" \\[No Data\\] ","",Observations) 
  
  SelectedIndicator <- paste0(VarPrefix1,trimalls(gsub("[[:punct:]]", "", SelectedIndicator)),
                                       VarPrefix2, GenderEqualityMenuItems,"</a>",VarPrefix3Local, CountryIndicatorsFiles, 
                                       VarPrefix4Local, ISO3Local," ",StartYears, 
                                       Observations, EndYears, VarPrefix5Local, collapse = "\n")
}