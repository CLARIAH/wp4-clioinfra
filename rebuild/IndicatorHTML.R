# test string substitution in html pages:
# had to rename:
# "Exchange_Rates_UK-historicalV2.xlsx" to "Exchange_Rates_UK-historical.xlsx"
# gold_production.docx to Gold_production.docx
# latend_Democracy.docx and txt to Latent_Democracy.docx and txt NOTICE THE "T" at the end of LATENT it was D initially
# Universities.docx and txt to universities.docx and txt
# Urban_Population.txt and docx to Urban_population.txt and docx

options( java.parameters = "-Xmx10g")

library(RefManageR)
library(readxl)
library(rJava)
library(xlsx)
library(ggplot2)
library(jsonlite)
library(tidyr)

rm(list=ls())

# https://stackoverflow.com/questions/21937640/handling-java-lang-outofmemoryerror-when-writing-to-excel-from-r
jgc <- function()
{
  #.jcall("java/lang/System", method = "gc")
} 

# to get the data from the ReadData.R script
load(paste0(dirname(rstudioapi::getSourceEditorContext()$path),'/Aggregates/ClioData.RData')) # this only contains ClioData dataframe that is too big to export on xslx and gives the Java heap space error

ClioMetaData <- read.xlsx(paste0(dirname(rstudioapi::getSourceEditorContext()$path),'/Aggregates/metaD.xlsx'), sheetIndex = 1, check.names = F, stringsAsFactors = F)
OECDregions <- read.xlsx(paste0(dirname(rstudioapi::getSourceEditorContext()$path),'/Aggregates/OECDregions.xlsx'), sheetIndex = 1, check.names = F, stringsAsFactors = F)
GlobalMetadata <- read.xlsx(paste0(dirname(rstudioapi::getSourceEditorContext()$path),'/Aggregates/GlobalMetadata.xlsx'), sheetIndex = 1, check.names = F, stringsAsFactors = F)
UNmembers <- read.xlsx(paste0(dirname(rstudioapi::getSourceEditorContext()$path),'/Aggregates/UNmembers.xlsx'), sheetIndex = 1, check.names = F, stringsAsFactors = F)
UNregions <- read.xlsx(paste0(dirname(rstudioapi::getSourceEditorContext()$path),'/Aggregates/UNregions.xlsx'), sheetIndex = 1, check.names = F, stringsAsFactors = F)

URL_basis <- "https://www.clio-infra.eu"

GenericPath <- dirname(rstudioapi::getSourceEditorContext()$path)
# export the individual indicator data to xlsx files?
ExportData <- T
# export images?
MakeThePlots <- T

setwd(GenericPath)
source('MakeTheGGplot.R')
# to export the data for download:
# /home/michalis/PhD/Clio Infra/Website/html/data
#XxZzYyAboutClioInfraXxZzYy <- readChar(paste(GenericPath,"AboutClioInfra.txt",sep="/"), file.info(paste(GenericPath,"AboutClioInfra.txt",sep="/"))$size)

Citations <- read_excel("CitationsStatic.xls")
Citations <- subset(Citations, !Citations$Indicator == "Geocoder")

OECD_regions <- unique(GlobalMetadata$OECD_Region[!is.na(GlobalMetadata$OECD_Region)])
OECD_regions <- OECD_regions[!(OECD_regions=="")]
OECD_regions <- sort(OECD_regions)

fileName <- 'IndicatorsTemplate.html'

trimalls <- function (x) gsub("\\s", "", x)

IndicatorsList <- unique(ClioData$Indicator)
DocFiles <- read_excel("IndicatorsListWithDocFiles.xlsx")
LogPlotList <- read_excel("IndicatorsGraphType.xlsx")

# variables that need to be set
# and then substituted in the text
# see /home/michalis/PhD/Clio Infra/Website/VarNames.xls for the commands 
#test <- gsub("income", "consumption", test)
#length(IndicatorsList)

for (i in 1:length(IndicatorsList)){
  gc()
  jgc()
  
  test <- readChar(fileName, file.info(fileName)$size)
  
  ### newly added, not in excel
  #XxZzYyTotalNumOfCountriesXxZzYy <- as.character(nrow(subset(GlobalMetadata,GlobalMetadata$DataPoints>0)))
  #XxZzYyTotalDatasetsXxZzYy <- as.character(length(IndicatorsList))
  
  XxZzYyIndicatorXxZzYy <- trimws(ClioMetaData$WebName[
    which(ClioMetaData$filename==unique(ClioData$Filename[which(ClioData$Indicator==IndicatorsList[i])]))])
  XxZzYyTitleXxZzYy <- IndicatorsList[i]
  XxZzYyIndicNoSpaceXxZzYy <- trimalls(IndicatorsList[i])
  
  XxZzYyWebServerFile1XxZzYy <- paste0(XxZzYyIndicNoSpaceXxZzYy,"_Compact.xlsx")
  XxZzYyWebServerFile2XxZzYy <- paste0(XxZzYyIndicNoSpaceXxZzYy,"_Broad.xlsx")
  if (XxZzYyIndicatorXxZzYy=="Female life Expectancy at Birth"){
    IndicNameForCitation <- "Female life expectancy at Birth"
  } else if (XxZzYyIndicatorXxZzYy=="Heights"){
    IndicNameForCitation <- "Height"
  } else if (XxZzYyIndicatorXxZzYy=="Labourer's Real Wage"){
    IndicNameForCitation <- "Labourers Real Wage"
  } else {
    IndicNameForCitation <- XxZzYyIndicatorXxZzYy
  }
  XxZzYyCitationStringXxZzYy <- Citations$CitationFilenamePrefix[which(Citations$Indicator==IndicNameForCitation)]
  # make the links to the Dataverse files:
  # http://hdl.handle.net/10622/N6BLB8
  stringA <- substr(XxZzYyCitationStringXxZzYy, 5, 9)
  stringB <- substr(XxZzYyCitationStringXxZzYy, 11, 17)
  XxZzYyDataVerseLinkXxZzYy <- paste0("http://hdl.handle.net/",stringA,"/",stringB)
  
  LogPlot <- as.logical(LogPlotList$LogScale[which(LogPlotList$IndicatorName==IndicNameForCitation)])
  LogPlot <- F # until I figure out how to treat 0 and negatives...
  
  XxZzYyWorkingPaperXxZzYy <- DocFiles$DocFileName[which(DocFiles$IndicatorName==IndicNameForCitation)]
  
  # Boxplot with all data on the globe:
  pdata <- subset(ClioData,ClioData$Indicator == XxZzYyTitleXxZzYy)
  pdata <- subset(pdata,pdata$`end year`=="2012")
  FullExport <- pdata
  pdata <- pdata[rowSums(is.na(pdata[,9:524]))<515,]
  pdata <- pdata[,9:ncol(pdata)]
  
  # export full data (Download including all countries in the Clio Infra repository. Some countries will have no data. Modern borders only.):
  FullExport$`Webmapper code` <- NULL
  FullExport$`Webmapper numeric code` <- NULL
  FullExport$`start year` <- NULL
  FullExport$`end year` <- NULL
  FullExport$Filename <- NULL
  FullExport$Indicator <- NULL
  FullExport <- FullExport[ order(FullExport$`country name`),]
  
  IndicatorFilenamePath <- paste0(GenericPath,"/data/",XxZzYyIndicNoSpaceXxZzYy,"_Broad.xlsx")
  ttt<-as.data.frame(as.matrix(FullExport),stringsAsFactors = F)
  ttt[,c(1,seq(3,ncol(ttt),1))] <-  lapply(ttt[,c(1,seq(3,ncol(ttt),1))], function (x) as.numeric(x))
  
  if (ExportData){
    write.xlsx2(ttt, file=IndicatorFilenamePath, sheetName="Data Clio Infra Format", row.names=F, showNA=F)
  }
  rm(ttt)
  fFullExport <- FullExport
  fFullExport$ccode <- factor(fFullExport$ccode)
  fFullExport$`country name`<- factor(fFullExport$`country name`)
  
  LocalTempLongFormat <- gather(fFullExport,year,value,which(names(fFullExport)=="1500"):which(names(fFullExport)=="2050"))
  LocalTempLongFormat <- LocalTempLongFormat[!is.na(LocalTempLongFormat$value),]
  LocalTempLongFormat <- data.frame(lapply(LocalTempLongFormat, as.character), stringsAsFactors=FALSE)
  LocalTempLongFormat$ccode <- as.numeric(LocalTempLongFormat$ccode)
  LocalTempLongFormat$year <- as.numeric(LocalTempLongFormat$year)
  LocalTempLongFormat$value <- as.numeric(LocalTempLongFormat$value)
  
  ttt<-as.data.frame(as.matrix(LocalTempLongFormat),stringsAsFactors = F)
  ttt[,c(1,seq(3,ncol(ttt),1))] <-  lapply(ttt[,c(1,seq(3,ncol(ttt),1))], function (x) as.numeric(x))
  
  if (ExportData){
    write.xlsx2(ttt, file=IndicatorFilenamePath, sheetName="Data Long Format", 
                append=TRUE, row.names=F, showNA=F)
  }
  rm(ttt)
  
  FileNameForXLS <- strsplit(IndicatorFilenamePath,"/")[[1]][length(strsplit(IndicatorFilenamePath,"/")[[1]])]
  Metadata <- t(c("Downloaded from",paste0(URL_basis,"/data/",FileNameForXLS)))
  Metadata <- as.data.frame(Metadata, stringsAsFactors=F)
  names(Metadata) <- c("Description","Value")
  
  FullMetadata <- Metadata
  
  CitationFileName <- Citations$CitationFilenamePrefix[which(Citations$Indicator==IndicatorsList[i])]
  CitationFileName <- as.character(CitationFileName)
  
  bib <- ReadBib(paste0(GenericPath,"/Citations/",CitationFileName,".bib"))[1]
  
  TheAuthorAndDate <- capture.output(print(bib, .opts = list(bib.style = "authoryear", first.inits = FALSE, no.print.fields = c("title","publisher","url"))))
  xxxTemp <- unlist(bib)
  
  Metadata <- t(c("Text Citation",paste(TheAuthorAndDate," ",xxxTemp$title,". ", xxxTemp$url,", accessed via the Clio Infra website.", sep = "")))
  Metadata <- as.data.frame(Metadata, stringsAsFactors=F)
  names(Metadata) <- c("Description","Value")
  
  FullMetadata <- rbind(FullMetadata,Metadata)
  
  
  CitationFileName <- Citations$CitationFilenamePrefix[which(Citations$Indicator==IndicatorsList[i])]
  Metadata <- t(c("XML Citation",paste0(URL_basis,"/Citations/",CitationFileName,".xml")))
  Metadata <- as.data.frame(Metadata, stringsAsFactors=F)
  names(Metadata) <- c("Description","Value")
  
  FullMetadata <- rbind(FullMetadata,Metadata)
  
  Metadata <- t(c("RIS Citation",paste0(URL_basis,"/Citations/",CitationFileName,".ris")))
  Metadata <- as.data.frame(Metadata, stringsAsFactors=F)
  names(Metadata) <- c("Description","Value")
  
  FullMetadata <- rbind(FullMetadata,Metadata)
  
  Metadata <- t(c("RIS Citation",paste0(URL_basis,"/Citations/",CitationFileName,".bib")))
  Metadata <- as.data.frame(Metadata, stringsAsFactors=F)
  names(Metadata) <- c("Description","Value")
  
  FullMetadata <- rbind(FullMetadata,Metadata)
  
  if (ExportData){
    write.xlsx2(FullMetadata, file=IndicatorFilenamePath, sheetName="Metadata", append=TRUE, row.names=F)
  }
  
  # export compact data (Download only for countries that have available data. Modern borders only.") :
  FullExport <- FullExport[rowSums(is.na(FullExport[,3:553]))<550,]
  
  IndicatorFilenamePath <- paste0(GenericPath,"/data/",XxZzYyIndicNoSpaceXxZzYy,"_Compact.xlsx")
  ttt<-as.data.frame(as.matrix(FullExport),stringsAsFactors = F)
  ttt[,c(1,seq(3,ncol(ttt),1))] <-  lapply(ttt[,c(1,seq(3,ncol(ttt),1))], function (x) as.numeric(x))
  
  if (ExportData){
    write.xlsx2(ttt, file=IndicatorFilenamePath, sheetName="Data Clio Infra Format", row.names=F, showNA=F)
  }
  rm(ttt)
  
  ttt<-as.data.frame(as.matrix(LocalTempLongFormat),stringsAsFactors = F)
  ttt[,c(1,seq(3,ncol(ttt),1))] <-  lapply(ttt[,c(1,seq(3,ncol(ttt),1))], function (x) as.numeric(x))
  
  if (ExportData){
    write.xlsx2(ttt, file=IndicatorFilenamePath, sheetName="Data Long Format", 
                append=TRUE, row.names=F, showNA=F)
  }
  rm(ttt,LocalTempLongFormat)
  
  FileNameForXLS <- strsplit(IndicatorFilenamePath,"/")[[1]][length(strsplit(IndicatorFilenamePath,"/")[[1]])]
  Metadata <- t(c("Downloaded from",paste0(URL_basis,"/data/",FileNameForXLS)))
  #Metadata <- t(c("Downloaded from",paste0(URL_basis,"/IndicatorsPerCountry/",IndicatorFilenamePath)))
  Metadata <- as.data.frame(Metadata, stringsAsFactors=F)
  names(Metadata) <- c("Description","Value")
  
  FullMetadata <- Metadata
  
  bib <- ReadBib(paste0(GenericPath,"/Citations/",CitationFileName,".bib"))[1]
  
  TheAuthorAndDate <- capture.output(print(bib, .opts = list(bib.style = "authoryear", first.inits = FALSE, no.print.fields = c("title","publisher","url"))))
  xxxTemp <- unlist(bib)
  
  Metadata <- t(c("Text Citation",paste(TheAuthorAndDate," ",xxxTemp$title,". ", xxxTemp$url,", accessed via the Clio Infra website.", sep = "")))
  Metadata <- as.data.frame(Metadata, stringsAsFactors=F)
  names(Metadata) <- c("Description","Value")
  
  FullMetadata <- rbind(FullMetadata,Metadata)
  
  
  CitationFileName <- Citations$CitationFilenamePrefix[which(Citations$Indicator==IndicatorsList[i])]
  Metadata <- t(c("XML Citation",paste0(URL_basis,"/citations/",CitationFileName,".xml")))
  Metadata <- as.data.frame(Metadata, stringsAsFactors=F)
  names(Metadata) <- c("Description","Value")
  
  FullMetadata <- rbind(FullMetadata,Metadata)
  
  Metadata <- t(c("RIS Citation",paste0(URL_basis,"/Citations/",CitationFileName,".ris")))
  Metadata <- as.data.frame(Metadata, stringsAsFactors=F)
  names(Metadata) <- c("Description","Value")
  
  FullMetadata <- rbind(FullMetadata,Metadata)
  
  Metadata <- t(c("BIB Citation",paste0(URL_basis,"/Citations/",CitationFileName,".bib")))
  Metadata <- as.data.frame(Metadata, stringsAsFactors=F)
  names(Metadata) <- c("Description","Value")
  
  FullMetadata <- rbind(FullMetadata,Metadata)
  
  if (ExportData){
    write.xlsx2(FullMetadata, file=IndicatorFilenamePath, sheetName="Metadata", append=TRUE, row.names=F)
  }
  
  # make the plots:
  if (MakeThePlots){
    MakeTheGGplot(pdata,XxZzYyTitleXxZzYy,XxZzYyIndicNoSpaceXxZzYy,"Global",LogPlot)
  }
  
  XxZzYyGraph0XxZzYy <- paste("Global_",XxZzYyIndicNoSpaceXxZzYy,".svg", sep = "")
  rm(pdata)
  
  RegionIndex <- 1
  pdata <- subset(ClioData,ClioData$Indicator == XxZzYyTitleXxZzYy &
                  ClioData$`country name` %in% 
                    GlobalMetadata$ClioInfraCountryName[which(GlobalMetadata$OECD_Region==OECD_regions[RegionIndex])])
  pdata <- pdata[rowSums(is.na(pdata[,9:524]))<515,]
  pdata <- pdata[,9:ncol(pdata)]
  if (MakeThePlots){
    MakeTheGGplot(pdata,XxZzYyTitleXxZzYy,XxZzYyIndicNoSpaceXxZzYy,gsub("&amp;","And",OECD_regions[RegionIndex]),LogPlot)
  }
  XxZzYyGraph1XxZzYy <- paste(trimalls(gsub("[[:punct:]]", "", gsub("&amp;","And",OECD_regions[RegionIndex]))),"_",XxZzYyIndicNoSpaceXxZzYy,".svg", sep = "")
  rm(pdata)
  
  RegionIndex <- 2
  pdata <- subset(ClioData,ClioData$Indicator == XxZzYyTitleXxZzYy &
                    ClioData$`country name` %in% 
                    GlobalMetadata$ClioInfraCountryName[which(GlobalMetadata$OECD_Region==OECD_regions[RegionIndex])])
  pdata <- pdata[rowSums(is.na(pdata[,9:524]))<515,]
  pdata <- pdata[,9:ncol(pdata)]
  
  if (MakeThePlots){
    MakeTheGGplot(pdata,XxZzYyTitleXxZzYy,XxZzYyIndicNoSpaceXxZzYy,gsub("&amp;","And",OECD_regions[RegionIndex]),LogPlot)
  }
  XxZzYyGraph2XxZzYy <- paste(trimalls(gsub("[[:punct:]]", "", gsub("&amp;","And",OECD_regions[RegionIndex]))),"_",XxZzYyIndicNoSpaceXxZzYy,".svg", sep = "")
  rm(pdata)
  
  RegionIndex <- 3
  pdata <- subset(ClioData,ClioData$Indicator == XxZzYyTitleXxZzYy &
                    ClioData$`country name` %in% 
                    GlobalMetadata$ClioInfraCountryName[which(GlobalMetadata$OECD_Region==OECD_regions[RegionIndex])])
  pdata <- pdata[rowSums(is.na(pdata[,9:524]))<515,]
  pdata <- pdata[,9:ncol(pdata)]
  
  if (MakeThePlots){
    MakeTheGGplot(pdata,XxZzYyTitleXxZzYy,XxZzYyIndicNoSpaceXxZzYy,gsub("&amp;","And",OECD_regions[RegionIndex]),LogPlot)
  }
  XxZzYyGraph3XxZzYy <- paste(trimalls(gsub("[[:punct:]]", "", gsub("&amp;","And",OECD_regions[RegionIndex]))),"_",XxZzYyIndicNoSpaceXxZzYy,".svg", sep = "")
  rm(pdata)
  
  
  RegionIndex <- 4
  pdata <- subset(ClioData,ClioData$Indicator == XxZzYyTitleXxZzYy &
                    ClioData$`country name` %in% 
                    GlobalMetadata$ClioInfraCountryName[which(GlobalMetadata$OECD_Region==OECD_regions[RegionIndex])])
  pdata <- pdata[rowSums(is.na(pdata[,9:524]))<515,]
  pdata <- pdata[,9:ncol(pdata)]
  
  if (MakeThePlots){
    MakeTheGGplot(pdata,XxZzYyTitleXxZzYy,XxZzYyIndicNoSpaceXxZzYy,gsub("&amp;","And",OECD_regions[RegionIndex]),LogPlot)
  }
  XxZzYyGraph4XxZzYy <- paste(trimalls(gsub("[[:punct:]]", "", gsub("&amp;","And",OECD_regions[RegionIndex]))),"_",XxZzYyIndicNoSpaceXxZzYy,".svg", sep = "")
  rm(pdata)
  
  RegionIndex <- 5
  pdata <- subset(ClioData,ClioData$Indicator == XxZzYyTitleXxZzYy &
                    ClioData$`country name` %in% 
                    GlobalMetadata$ClioInfraCountryName[which(GlobalMetadata$OECD_Region==OECD_regions[RegionIndex])])
  pdata <- pdata[rowSums(is.na(pdata[,9:524]))<515,]
  pdata <- pdata[,9:ncol(pdata)]
  
  if (MakeThePlots){
    MakeTheGGplot(pdata,XxZzYyTitleXxZzYy,XxZzYyIndicNoSpaceXxZzYy,gsub("&amp;","And",OECD_regions[RegionIndex]),LogPlot)
  }
  XxZzYyGraph5XxZzYy <- paste(trimalls(gsub("[[:punct:]]", "", gsub("&amp;","And",OECD_regions[RegionIndex]))),"_",XxZzYyIndicNoSpaceXxZzYy,".svg", sep = "")
  rm(pdata)
  
  RegionIndex <- 6
  pdata <- subset(ClioData,ClioData$Indicator == XxZzYyTitleXxZzYy &
                    ClioData$`country name` %in% 
                    GlobalMetadata$ClioInfraCountryName[which(GlobalMetadata$OECD_Region==OECD_regions[RegionIndex])])
  pdata <- pdata[rowSums(is.na(pdata[,9:524]))<515,]
  pdata <- pdata[,9:ncol(pdata)]
  
  if (MakeThePlots){
    MakeTheGGplot(pdata,XxZzYyTitleXxZzYy,XxZzYyIndicNoSpaceXxZzYy,gsub("&amp;","And",OECD_regions[RegionIndex]),LogPlot)
  }
  XxZzYyGraph6XxZzYy <- paste(trimalls(gsub("[[:punct:]]", "", gsub("&amp;","And",OECD_regions[RegionIndex]))),"_",XxZzYyIndicNoSpaceXxZzYy,".svg", sep = "")
  rm(pdata)
  
  RegionIndex <- 7
  pdata <- subset(ClioData,ClioData$Indicator == XxZzYyTitleXxZzYy &
                    ClioData$`country name` %in% 
                    GlobalMetadata$ClioInfraCountryName[which(GlobalMetadata$OECD_Region==OECD_regions[RegionIndex])])
  pdata <- pdata[rowSums(is.na(pdata[,9:524]))<515,]
  pdata <- pdata[,9:ncol(pdata)]
  
  if (MakeThePlots){
    MakeTheGGplot(pdata,XxZzYyTitleXxZzYy,XxZzYyIndicNoSpaceXxZzYy,gsub("&amp;","And",OECD_regions[RegionIndex]),LogPlot)
  }
  XxZzYyGraph7XxZzYy <- paste(trimalls(gsub("[[:punct:]]", "", gsub("&amp;","And",OECD_regions[RegionIndex]))),"_",XxZzYyIndicNoSpaceXxZzYy,".svg", sep = "")
  rm(pdata)
  
  RegionIndex <- 8
  pdata <- subset(ClioData,ClioData$Indicator == XxZzYyTitleXxZzYy &
                    ClioData$`country name` %in% 
                    GlobalMetadata$ClioInfraCountryName[which(GlobalMetadata$OECD_Region==OECD_regions[RegionIndex])])
  pdata <- pdata[rowSums(is.na(pdata[,9:524]))<515,]
  pdata <- pdata[,9:ncol(pdata)]
  
  if (MakeThePlots){
    MakeTheGGplot(pdata,XxZzYyTitleXxZzYy,XxZzYyIndicNoSpaceXxZzYy,gsub("&amp;","And",OECD_regions[RegionIndex]),LogPlot)
  }
  XxZzYyGraph8XxZzYy <- paste(trimalls(gsub("[[:punct:]]", "", gsub("&amp;","And",OECD_regions[RegionIndex]))),"_",XxZzYyIndicNoSpaceXxZzYy,".svg", sep = "")
  rm(pdata)
  
  ObsTemp <- subset(ClioData,ClioData$Indicator==IndicatorsList[i])
  XxZzYyObsTotalXxZzYy <- as.character(sum(!is.na(ObsTemp[,(match("Indicator",names(ObsTemp))+1):ncol(ObsTemp)])))
  
  XxZzYyObsAfricaXxZzYy <- as.character(sum(!is.na(ObsTemp[which(ObsTemp$`country name` %in% 
                                                                   GlobalMetadata$ClioInfraCountryName[which(GlobalMetadata$region=="Africa"
                                                                   )]),(match("Indicator",names(ObsTemp))+1):ncol(ObsTemp)])))
  XxZzYyObsOceaniaXxZzYy <- as.character(sum(!is.na(ObsTemp[which(ObsTemp$`country name` %in% 
                                                                    GlobalMetadata$ClioInfraCountryName[which(GlobalMetadata$region=="Oceania"
                                                                    )]),(match("Indicator",names(ObsTemp))+1):ncol(ObsTemp)])))
  XxZzYyObsEuropeXxZzYy <- as.character(sum(!is.na(ObsTemp[which(ObsTemp$`country name` %in% 
                                                                   GlobalMetadata$ClioInfraCountryName[which(GlobalMetadata$region=="Europe"
                                                                   )]),(match("Indicator",names(ObsTemp))+1):ncol(ObsTemp)])))
  XxZzYyObsAsiaXxZzYy <- as.character(sum(!is.na(ObsTemp[which(ObsTemp$`country name` %in% 
                                                                 GlobalMetadata$ClioInfraCountryName[which(GlobalMetadata$region=="Asia"
                                                                 )]),(match("Indicator",names(ObsTemp))+1):ncol(ObsTemp)])))
  XxZzYyObsAmericasXxZzYy <- as.character(sum(!is.na(ObsTemp[which(ObsTemp$`country name` %in% 
                                                                     GlobalMetadata$ClioInfraCountryName[which(GlobalMetadata$region=="Americas"
                                                                     )]),(match("Indicator",names(ObsTemp))+1):ncol(ObsTemp)])))
  
  #1500-1819
  XxZzYyObsP1XxZzYy <- as.character(sum(!is.na(ObsTemp[match("1500",names(ObsTemp)):match("1819",names(ObsTemp))])))
  #1820-1859
  XxZzYyObsP2XxZzYy <- as.character(sum(!is.na(ObsTemp[match("1820",names(ObsTemp)):match("1859",names(ObsTemp))])))
  #1860-1899
  XxZzYyObsP3XxZzYy <- as.character(sum(!is.na(ObsTemp[match("1860",names(ObsTemp)):match("1899",names(ObsTemp))])))
  #1900-1949
  XxZzYyObsP4XxZzYy <- as.character(sum(!is.na(ObsTemp[match("1900",names(ObsTemp)):match("1949",names(ObsTemp))])))
  #1950-1989
  XxZzYyObsP5XxZzYy <- as.character(sum(!is.na(ObsTemp[match("1950",names(ObsTemp)):match("1989",names(ObsTemp))])))
  #1990-now
  XxZzYyObsP6XxZzYy <- as.character(sum(!is.na(ObsTemp[match("1990",names(ObsTemp)):ncol(ObsTemp)])))
  
  # temp vector
  tempVector <- as.vector(as.matrix(ObsTemp[(match("Indicator",names(ObsTemp))+1):ncol(ObsTemp)]))
  XxZzYyArithmeticMeanXxZzYy <- round(mean(as.matrix(ObsTemp[(match("Indicator",names(ObsTemp))+1):ncol(ObsTemp)]),na.rm = T), digits = 2)
  XxZzYyMINXxZzYy <- round(min(min(as.matrix(ObsTemp[(match("Indicator",names(ObsTemp))+1):ncol(ObsTemp)]),na.rm = T)),digits = 2)
  XxZzYyQRT1XxZzYy <- as.character(round(summary(tempVector)[2],digits = 2))
  XxZzYyMEDIANXxZzYy <- as.character(round(median(tempVector, na.rm = T),digits = 2))
  XxZzYyQRT3XxZzYy <- as.character(round(summary(tempVector)[5],digits = 2))
  XxZzYyMAXXxZzYy <- as.character(round(max(tempVector,na.rm = T),digits = 2))
  
  XxZzYyAuthorsXxZzYy <- trimws(ClioMetaData$author[which(ClioMetaData$filename==unique(ClioData$Filename[which(ClioData$Indicator==IndicatorsList[i])]))])
  XxZzYyProdDateXxZzYy <- trimws(ClioMetaData$proddate[which(ClioMetaData$filename==unique(ClioData$Filename[which(ClioData$Indicator==IndicatorsList[i])]))])
  XxZzYyGeoCovXxZzYy <- trimws(ClioMetaData$geocover[which(ClioMetaData$filename==unique(ClioData$Filename[which(ClioData$Indicator==IndicatorsList[i])]))])
  XxZzYyMethodsXxZzYy <- trimws(ClioMetaData$methods[which(ClioMetaData$filename==unique(ClioData$Filename[which(ClioData$Indicator==IndicatorsList[i])]))])
  XxZzYyVarsXxZzYy <- trimws(ClioMetaData$vars[which(ClioMetaData$filename==unique(ClioData$Filename[which(ClioData$Indicator==IndicatorsList[i])]))])
  XxZzYyKeyWrdsXxZzYy <- trimws(ClioMetaData$keywords[which(ClioMetaData$filename==unique(ClioData$Filename[which(ClioData$Indicator==IndicatorsList[i])]))])
  XxZzYyColPeriodXxZzYy <- trimws(ClioMetaData$collectdate[which(ClioMetaData$filename==unique(ClioData$Filename[which(ClioData$Indicator==IndicatorsList[i])]))])
  XxZzYyDataColsXxZzYy <- trimws(ClioMetaData$collectors[which(ClioMetaData$filename==unique(ClioData$Filename[which(ClioData$Indicator==IndicatorsList[i])]))])
  XxZzYyAbsXxZzYy <- trimws(ClioMetaData$abstract[which(ClioMetaData$filename==unique(ClioData$Filename[which(ClioData$Indicator==IndicatorsList[i])]))])
  XxZzYyTimePeriodXxZzYy <- trimws(ClioMetaData$timeperiod[which(ClioMetaData$filename==unique(ClioData$Filename[which(ClioData$Indicator==IndicatorsList[i])]))])
  XxZzYyDataQualityXxZzYy <- trimws(ClioMetaData$quality[which(ClioMetaData$filename==unique(ClioData$Filename[which(ClioData$Indicator==IndicatorsList[i])]))])
  
  ##### Sources are optional and should be treated as such:
  # also need to place <p></p> around each line
  
  XxZzYyGenRefHTMLXxZzYy <- "<h3>General references</h3>"
  XxZzYyGenRefsXxZzYy <- ClioMetaData$sources[which(ClioMetaData$filename==unique(ClioData$Filename[which(ClioData$Indicator==IndicatorsList[i])]))]
  XxZzYyGenRefsXxZzYy <- gsub("\n","</p><p>",XxZzYyGenRefsXxZzYy)
  XxZzYyGenRefsXxZzYy <- paste0("<p>",XxZzYyGenRefsXxZzYy,"</p>")
  
  # just a script making the substitutions in the indicator html template
  setwd(GenericPath)
  source('IndicatorHTMLSubstitutions.R')
  
  write(test, file = paste0(GenericPath,"/Pages Exports R/",trimalls(gsub("[[:punct:]]", "", XxZzYyIndicatorXxZzYy)),".html"))
  
  rm(test,tempVector,xxxTemp,RegionIndex,stringA,stringB,IndicatorFilenamePath,IndicNameForCitation,
     FileNameForXLS,TheAuthorAndDate,ObsTemp,Metadata,fFullExport,FullExport,FullMetadata,bib,CitationFileName)
  rm(list=ls(pattern="^XxZzYy"))
}
