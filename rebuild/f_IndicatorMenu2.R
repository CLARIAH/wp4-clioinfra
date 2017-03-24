f_IndicatorMenu2 <- function(WebCat, ClioMetaData, GlobalDurations, IndicatorFilename) {
  
  VarPrefix1 <- '<p class="list-group-item"><a href="../Indicators/'
  VarPrefix2 <- '.html">'
  VarPrefix3 <- '<span class="badge"><a href="../data/'
  VarPrefix4 <- '"><font color="FFFFFF">'
  VarPrefix5 <- '</font></a></span></p>'
  VarPrefixNoData <- '</p>'
  
  SelectedIndicator <- sort(unique(ClioMetaData$WebName[which(ClioMetaData$WebCategory==WebCat)]))
  # attention to the indicators without data for the indicators
  # I need the first year for each indicator
  StartYears <- c()
  for (i in 1:length(SelectedIndicator)){
   StartYears <- c(StartYears,min(as.numeric(c(GlobalDurations$StartYear[which(GlobalDurations$Indicator %in% c(SelectedIndicator[i]))])),na.rm = T)) 
  }
  EndYears <- c()
  for (i in 1:length(SelectedIndicator)){
    EndYears <- c(EndYears,max(as.numeric(c(GlobalDurations$EndYear[which(GlobalDurations$Indicator %in% c(SelectedIndicator[i]))])),na.rm = T)) 
  }
  Observations <- c()
  for (i in 1:length(SelectedIndicator)){
    Observations <- c(Observations,sum(as.numeric(c(GlobalDurations$Obs[which(GlobalDurations$Indicator %in% c(SelectedIndicator[i]))])),na.rm = T)) 
  }
  
  GenderEqualityMenuItems <- SelectedIndicator
  
  for (L_jjj in 1:length(GenderEqualityMenuItems)){
    GenderEqualityMenuItems[L_jjj] <- 
      IndicPriorityList$ShortForTheMenu[which(IndicPriorityList$WebName==GenderEqualityMenuItems[L_jjj])]
  }
  
  # I need to make the items below vectors with the appropriate value to mix in the paste function below:
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
  
  # file name structure: XXXNameOfIndicatorXXX_Compact.xlsx
  CountryIndicatorsFiles <- c()
  for (L_ij in 1:length(SelectedIndicator)){
    if (Observations[L_ij]=="No Data"){
      CountryIndicatorsFiles <- c(CountryIndicatorsFiles,"")
    } else {
      tempname <- gsub("XXXNameOfIndicatorXXX",paste0(trimalls(SelectedIndicator[L_ij])),IndicatorFilename)
      CountryIndicatorsFiles <- c(CountryIndicatorsFiles,tempname)
      rm(tempname)
    }
  }
  
  Observations <- paste0(" [",Observations,"] ",sep="")
  Observations <- gsub(" \\[No Data\\] ","",Observations) 
  
  SelectedIndicator <- paste0(VarPrefix1,trimalls(gsub("[[:punct:]]", "", SelectedIndicator)),
                                       VarPrefix2,GenderEqualityMenuItems,"</a>",VarPrefix3Local, CountryIndicatorsFiles, 
                                       VarPrefix4Local, " ",StartYears, 
                                       Observations, EndYears, VarPrefix5Local, collapse = "\n")
}