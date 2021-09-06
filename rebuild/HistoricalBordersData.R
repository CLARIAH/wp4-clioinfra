# export data corresponding to historical entities for the homepage:
require(xlsx)
require(readxl)
require(RefManageR)
require(tidyr)
rm(list=ls())

# set main path for all scripts
GenericPath <- dirname(rstudioapi::getSourceEditorContext()$path)

# to get the data from the ReadData.R script
load(paste0(dirname(rstudioapi::getSourceEditorContext()$path),'/Aggregates/ClioData.RData')) # this only contains ClioData dataframe that is too big to export on xslx and gives the Java heap space error

# select only rows that have not all elements of the specified columns NA
ClioOnlyWithData <- ClioData[rowSums(is.na(ClioData[,8:524]))<516,]

ClioMetaData <- read.xlsx(paste0(dirname(rstudioapi::getSourceEditorContext()$path),'/Aggregates/metaD.xlsx'), sheetIndex = 1, check.names = F, stringsAsFactors = F)
OECDregions <- read.xlsx(paste0(dirname(rstudioapi::getSourceEditorContext()$path),'/Aggregates/OECDregions.xlsx'), sheetIndex = 1, check.names = F, stringsAsFactors = F)
GlobalMetadata <- read.xlsx(paste0(dirname(rstudioapi::getSourceEditorContext()$path),'/Aggregates/GlobalMetadata.xlsx'), sheetIndex = 1, check.names = F, stringsAsFactors = F)
UNmembers <- read.xlsx(paste0(dirname(rstudioapi::getSourceEditorContext()$path),'/Aggregates/UNmembers.xlsx'), sheetIndex = 1, check.names = F, stringsAsFactors = F)
UNregions <- read.xlsx(paste0(dirname(rstudioapi::getSourceEditorContext()$path),'/Aggregates/UNregions.xlsx'), sheetIndex = 1, check.names = F, stringsAsFactors = F)

URL_basis <- "https://www.clio-infra.eu"

Citations <- read_excel(paste0(GenericPath,'/',"CitationsStatic.xls"))
Citations <- subset(Citations, !Citations$Indicator == "Geocoder")

ExportData <- T
# get all non 2012 entries with available data:
DataWithHistBorders <- subset(ClioData,!(ClioData$`end year`=="2012"))
DataWithHistBorders <- DataWithHistBorders[rowSums(is.na(DataWithHistBorders[,9:524]))<515,]

# remove unwanted columns
DataWithHistBorders$`Webmapper code` <- NULL
DataWithHistBorders$`Webmapper numeric code` <- NULL
DataWithHistBorders$ccode <- NULL
DataWithHistBorders$Filename <- NULL
names(DataWithHistBorders)[which(names(DataWithHistBorders)=="start year")] <- "Borders Start Year"
names(DataWithHistBorders)[which(names(DataWithHistBorders)=="end year")] <- "Borders End Year"

DataWithHistBorders <- DataWithHistBorders[ order(DataWithHistBorders$`country name`),]
ttt <- as.data.frame(as.matrix(DataWithHistBorders),stringsAsFactors = F)
ttt[,c(2,3,seq(5,ncol(ttt),1))] <-  lapply(ttt[,c(2,3,seq(5,ncol(ttt),1))], function (x) as.numeric(x))

FilenamePath <- paste0(GenericPath,"/data/DataAtHistoricalBorders.xlsx")
if (ExportData){
  write.xlsx2(ttt, file=FilenamePath, sheetName="Data Clio Infra Format", row.names=F, showNA=F, append = F)
}

# add Long Format Section

fLocalTemp <- DataWithHistBorders
fLocalTemp$`country name` <- factor(fLocalTemp$`country name`)
fLocalTemp$`Borders Start Year` <- factor(fLocalTemp$`Borders Start Year`)
fLocalTemp$`Borders End Year` <- factor(fLocalTemp$`Borders End Year`)
fLocalTemp$Indicator <- factor(fLocalTemp$Indicator)

LocalTempLongFormat <- gather(fLocalTemp,year,value,which(names(fLocalTemp)=="1500"):which(names(fLocalTemp)=="2015"))
LocalTempLongFormat <- LocalTempLongFormat[!is.na(LocalTempLongFormat$value),]
ttt <- as.data.frame(as.matrix(LocalTempLongFormat),stringsAsFactors = F)
ttt[,c(2,3,seq(5,ncol(ttt),1))] <-  lapply(ttt[,c(2,3,seq(5,ncol(ttt),1))], function (x) as.numeric(x))

if (ExportData){
  write.xlsx2(ttt, file=FilenamePath, sheetName="Data Long Format", append=TRUE, row.names=F, showNA=F)
}

# which are the available indicators to cite:
AvailIndicators <- unique(DataWithHistBorders$Indicator)

# create metadata sheet starting with the download url of the file:

Metadata <- t(c("Downloaded from","N/A",paste0(URL_basis,"/data/DataAtHistoricalBorders.xlsx")))
Metadata <- as.data.frame(Metadata, stringsAsFactors=F)
names(Metadata) <- c("Description","Indicator Name","Value")

FullMetadata <- Metadata

#### SIMPLE TEXT CITATION
#### SIMPLE TEXT CITATION
#### SIMPLE TEXT CITATION
#### SIMPLE TEXT CITATION

for (i in AvailIndicators){
  CitationFileName <- Citations$CitationFilenamePrefix[which(Citations$Indicator==i)]
  CitationFileName <- as.character(CitationFileName)
  
  bib <- ReadBib(paste0(GenericPath,"/Citations/",CitationFileName,".bib"))
  
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
  
  Metadata <- t(c("XML Citation",i,paste0(URL_basis,"/Citations/",CitationFileName,".xml")))
  Metadata <- as.data.frame(Metadata, stringsAsFactors=F)
  names(Metadata) <- c("Description","Indicator Name","Value")
  
  FullMetadata <- rbind(FullMetadata,Metadata)
  
  Metadata <- t(c("RIS Citation",i,paste0(URL_basis,"/Citations/",CitationFileName,".ris")))
  Metadata <- as.data.frame(Metadata, stringsAsFactors=F)
  names(Metadata) <- c("Description","Indicator Name","Value")
  
  FullMetadata <- rbind(FullMetadata,Metadata)
  
  Metadata <- t(c("BIB Citation",i,paste0(URL_basis,"/Citations/",CitationFileName,".bib")))
  Metadata <- as.data.frame(Metadata, stringsAsFactors=F)
  names(Metadata) <- c("Description","Indicator Name","Value")
  
  FullMetadata <- rbind(FullMetadata,Metadata)
}

if (ExportData){
 write.xlsx2(FullMetadata, file=FilenamePath, sheetName="Metadata", append=TRUE, row.names=F, showNA=F)
}

