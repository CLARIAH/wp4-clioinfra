# export data corresponding to historical entities for the homepage:
require(xlsx)
require(RefManageR)

load("~/PhD/Clio Infra/Website/ReadData3.R.RData")
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

FilenamePath <- paste0(GenericPath,"data/DataAtHistoricalBorders.xlsx")
if (ExportData){
  write.xlsx(DataWithHistBorders, file=FilenamePath, sheetName="Data Clio Infra Format", row.names=F, showNA=F)
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

write.xlsx(FullMetadata, file=FilenamePath, sheetName="Metadata", append=TRUE, row.names=F)
