# this script should create the news and publications page
setwd(paste0(dirname(rstudioapi::getSourceEditorContext()$path)))

GenericPath <- paste0(dirname(rstudioapi::getSourceEditorContext()$path))

fileName <- 'NewsPublicationsTemplate.html'
test <- readChar(fileName, file.info(fileName)$size)

XxZzYyTotalDatasetsXxZzYy <- nrow(ClioMetaData)
XxZzYyTotalNumOfCountriesXxZzYy <- nrow(subset(GlobalMetadata,GlobalMetadata$DataPoints>0))
XxZzYyAboutClioInfraXxZzYy <- readChar(paste(GenericPath,"AboutClioInfra.txt",sep="/"), file.info(paste(GenericPath,"AboutClioInfra.txt",sep="/"))$size)
XxZzYyCopyrightYearsXxZzYy <- "2010-2017"

test <- gsub("XxZzYyTotalDatasetsXxZzYy", XxZzYyTotalDatasetsXxZzYy, test)
test <- gsub("XxZzYyTotalNumOfCountriesXxZzYy", XxZzYyTotalNumOfCountriesXxZzYy, test)
test <- gsub("XxZzYyAboutClioInfraXxZzYy", XxZzYyAboutClioInfraXxZzYy, test)
test <- gsub("XxZzYyCopyrightYearsXxZzYy", XxZzYyCopyrightYearsXxZzYy, test)

# export the HTML page:
write(test, paste0(GenericPath,"/News_Publications.html"))


# this script should create the news and publications page and also substitute the footers XYZ on all other pages as well

GenericPath <- paste0(dirname(rstudioapi::getSourceEditorContext()$path))

fileName <- 'NewsPublicationsTemplate.html'
test <- readChar(fileName, file.info(fileName)$size)

XxZzYyTotalDatasetsXxZzYy <- nrow(ClioMetaData)
XxZzYyTotalNumOfCountriesXxZzYy <- nrow(subset(GlobalMetadata,GlobalMetadata$DataPoints>0))
XxZzYyAboutClioInfraXxZzYy <- readChar(paste(GenericPath,"/AboutClioInfra.txt",sep="/"), file.info(paste(GenericPath,"AboutClioInfra.txt",sep="/"))$size)
XxZzYyCopyrightYearsXxZzYy <- "2010-2017"

test <- gsub("XxZzYyTotalDatasetsXxZzYy", XxZzYyTotalDatasetsXxZzYy, test)
test <- gsub("XxZzYyTotalNumOfCountriesXxZzYy", XxZzYyTotalNumOfCountriesXxZzYy, test)
test <- gsub("XxZzYyAboutClioInfraXxZzYy", XxZzYyAboutClioInfraXxZzYy, test)
test <- gsub("XxZzYyCopyrightYearsXxZzYy", XxZzYyCopyrightYearsXxZzYy, test)

# export the HTML page:
write(test, paste0(GenericPath,"/News_Publications.html"))


# on the second part I need to substitute for the news and publication items:

GenericPath <- paste0(dirname(rstudioapi::getSourceEditorContext()$path))

fileName <- paste0(GenericPath,"/News_Publications.html")
test <- readChar(fileName, file.info(fileName)$size)

XxZzYyNewsItem1XxZzYy <- "<a href=\"./News_Publications.html#item010\">How Was Life? Global Well-Being Since 1820</a>"
XxZzYyNewsItem1DateXxZzYy <- "Published on Thu, 12/04/2014 - 16:22"

XxZzYyNewsItem2XxZzYy <- "<a href=\"./News_Publications.html#item002\">Quality of Life Workshop at the OECD</a>"
XxZzYyNewsItem2DateXxZzYy <- "Published on Sun, 03/24/2013 - 14:25"

XxZzYyNewsItem3XxZzYy <- "<a href=\"./News_Publications.html#item001\">Workshop on Real Wages</a>"
XxZzYyNewsItem3DateXxZzYy <- "Published on Thu, 03/21/2013 - 15:07"

XxZzYyPublicationItem1XxZzYy <- "<a href=\"./News_Publications.html#item009\">How Was Life? Global Well-Being Since 1820</a>"
XxZzYyPublicationItem1DateXxZzYy <- "Published on Thu, 12/04/2014 - 16:20"

XxZzYyPublicationItem2XxZzYy <- "<a href=\"./News_Publications.html#item008\">Overview of datasets used in Global Well-Being Since 1820</a>"
XxZzYyPublicationItem2DateXxZzYy <- "Published on Thu, 12/04/2014 - 16:15"

XxZzYyPublicationItem3XxZzYy <- "<a href=\"./News_Publications.html#item009\">Working Papers Center for Global Economic History Online</a>"
XxZzYyPublicationItem3DateXxZzYy <- ""


write(test, paste0(GenericPath,"/News_Publications.html"))

