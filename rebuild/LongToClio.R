# this function converts long format to clio infra format
# 
require(xlsx)
ClioDataTitle <- "Historical Gender Equality Index"
ClioDataSubTitle <- "A higher score means less gender equality in favour of women."

setwd("/home/michalis/PhD/Clio Infra/New Data/Historical Gender Equality Auke Sarah/")
OriginalData <- read.csv("hgei_fe_clio.csv")
ClioLayout <- read.xlsx("~/PhD/Clio Infra/ClioLayout.xlsx", sheetIndex = 1, check.names = F)
OriginalData$ccode <- as.character(OriginalData$ccode)
# remember that the two first lines should be including:
# Aluminium Production
# Aluminium primary smelter and refinery production (thousand tonnes)

for (i in 1:nrow(OriginalData)){
  ClioLayout[which(ClioLayout$ccode==OriginalData$ccode[i]),which(names(ClioLayout)==OriginalData$year[i])] <- OriginalData$hgi_ame[i]
}

CurFileName <- paste0(gsub(" ","_",ClioDataTitle),"-historical.xlsx")

#write.xlsx(ClioDataTitle,CurFileName,sheetName = "Data",col.names = F, row.names = F, append = F, showNA = F)
#write.xlsx(ClioDataSubTitle,CurFileName,sheetName = "Data",col.names = F, row.names = F, append = T, showNA = F)
write.xlsx(ClioLayout,CurFileName,sheetName = "Data",row.names = F, append = F, showNA = F)

# all rows below do not work because of Error : OutOfMemoryError ( Java ): Java heap space PROBLEMS THAT I CANNOT SOLVE
# all rows below do not work because of Error : OutOfMemoryError ( Java ): Java heap space PROBLEMS THAT I CANNOT SOLVE
# all rows below do not work because of Error : OutOfMemoryError ( Java ): Java heap space PROBLEMS THAT I CANNOT SOLVE
# all rows below do not work because of Error : OutOfMemoryError ( Java ): Java heap space PROBLEMS THAT I CANNOT SOLVE

if (F){
  dat <- ClioLayout
  text_matrix <- function(dat, table_title) {
    
    rbind(c(table_title, rep('', ncol(dat)-1)), # title
          rep('', ncol(dat)), # blank spacer row
          names(dat), # column names
          unname(sapply(dat, as.character))) # data
    
  }
  temp <- text_matrix(dat, table_title=ClioDataTitle)
  write.xlsx(temp,CurFileName,sheetName = "Data",row.names = F, append = F, showNA = F)
  
  #http://www.sthda.com/english/wiki/r-xlsx-package-a-quick-start-guide-to-manipulate-excel-files-in-r#add-a-title-into-a-worksheet
  options(java.parameters = "-Xms8g")
  require(xlsx)
  wb<-createWorkbook(type="xlsx")
  #CellStyle(wb, dataFormat=NULL, alignment=NULL,
  #          border=NULL, fill=NULL, font=NULL)
  
  TITLE_STYLE <- CellStyle(wb)+ Font(wb,  heightInPoints=11, 
                                     isBold=TRUE, underline=0)
  SUB_TITLE_STYLE <- CellStyle(wb) + 
    Font(wb,  heightInPoints=11, 
         isItalic=F, isBold=T)
  # Styles for the data table row/column names
  TABLE_ROWNAMES_STYLE <- CellStyle(wb) + Font(wb, isBold=TRUE)
  TABLE_COLNAMES_STYLE <- CellStyle(wb) + Font(wb, isBold=TRUE) +
    Alignment(wrapText=F)
  
  sheet <- createSheet(wb, sheetName = "Data")
  
  #++++++++++++++++++++++++
  # Helper function to add titles
  #++++++++++++++++++++++++
  # - sheet : sheet object to contain the title
  # - rowIndex : numeric value indicating the row to 
  #contain the title
  # - title : the text to use as title
  # - titleStyle : style object to use for title
  xlsx.addTitle<-function(sheet, rowIndex, title, titleStyle){
    rows <-createRow(sheet,rowIndex=rowIndex)
    sheetTitle <-createCell(rows, colIndex=1)
    setCellValue(sheetTitle[[1,1]], title)
    setCellStyle(sheetTitle[[1,1]], titleStyle)
  }
  
  # Add title
  xlsx.addTitle(sheet, rowIndex=1, title=ClioDataTitle,
                titleStyle = TITLE_STYLE)
  # Add sub title
  xlsx.addTitle(sheet, rowIndex=2, 
                title=ClioDataSubTitle,
                titleStyle = SUB_TITLE_STYLE)
  
  addDataFrame(ClioLayout, sheet, startRow=3, startColumn=1, 
               colnamesStyle = TABLE_COLNAMES_STYLE,
               row.names = F)
  
  addDataFrame(ClioLayout[c(301:600),], sheet, startRow=304, startColumn=1, 
               colnamesStyle = TABLE_COLNAMES_STYLE,
               row.names = F, col.names=F)
  
  addDataFrame(ClioLayout[c(601:900),], sheet, startRow=604, startColumn=1, 
               colnamesStyle = TABLE_COLNAMES_STYLE,
               row.names = F, col.names=F)
  
  saveWorkbook(wb, CurFileName)
}