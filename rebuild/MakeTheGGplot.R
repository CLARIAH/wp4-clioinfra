MakeTheGGplot <- function(pdata,XxZzYyTitleXxZzYy,XxZzYyIndicNoSpaceXxZzYy,GeoLevel, LogPlot) {
  #http://stackoverflow.com/questions/15660829/how-to-add-a-number-of-observations-per-group-and-use-group-mean-in-ggplot2-boxp
  
  give.n <- function(x){
    return(c(y = min(min(pdata,na.rm = T)), label = length(x))) 
    # experiment with the multiplier to find the perfect position
  }
  
  # function for mean labels
  if (nrow(pdata)>0){
    if (round(mean(colMeans(pdata,na.rm = T),na.rm = T),0) > 100){
      mean.n <- function(x){
        return(c(y = 0.9*max(max(pdata,na.rm = T)), label = round(mean(x),0))) 
        # experiment with the multiplier to find the perfect position
      }
    } else {
      mean.n <- function(x){
        return(c(y = 0.9*max(max(pdata,na.rm = T)), label = round(mean(x),1))) 
        # experiment with the multiplier to find the perfect position
      }
    }
  } else {
    mean.n <- function(x){
      return(c(y = 0.9*max(max(pdata,na.rm = T)), label = round(mean(x),0))) 
      # experiment with the multiplier to find the perfect position
    }
  }
  
  trimalls <- function (x) gsub("\\s", "", x)
  
  GeoLevelTitle <- GeoLevel
  GeoLevel <- trimalls(gsub("[[:punct:]]", "", GeoLevel))
  
  #GeoLevelTitle <- switch(GeoLevelTitle,
  #                        'Global' =  "Global",
  #                        'E.Asia' = "East Asia",
  #                        'E.Europe And f.SU' = "Eastern Europe and former Soviet Union",
  #                        'L.America And Carib.' = "Latin America and Caribbean",
  #                        'M.East And N.Africa' = "Middle East and North Africa",
  #                        'S. And S.E.Asia' = "South and South-East Asia",
  #                        'Sub-Sah. Africa' = "Sub-Saharan Africa",
  #                        'W. Europe' = "Western Europe",
  #                        'W. Offshoots' = "Western Offshoots")
  
  
  GeoLevelTitle <- switch(GeoLevelTitle,
                          'Global' =  "Global",
                          'East Asia' = "East Asia",
                          'East. Europe and form. SU' = "Eastern Europe and former Soviet Union",
                          'Latin America and Carib.' = "Latin America and Caribbean",
                          'MENA' = "Middle East and North Africa",
                          'South and South-East Asia' = "South and South-East Asia",
                          'Sub-Saharan Africa' = "Sub-Saharan Africa",
                          'W. Europe' = "Western Europe",
                          'W. Offshoots' = "Western Offshoots")
  
  YearsWithData <- c()
  for (L_i in 1:ncol(pdata)){
    if (!all(is.na(pdata[,L_i]))){
      YearsWithData <- c(YearsWithData,as.numeric(names(pdata)[L_i]))
    }
  }
  if (length(YearsWithData)>0){
    # we need around 25-30 data points
    # if all years are available:
    # every 50 years before 1800: 6
    # every 20 1800-1960: 9
    # every 5 1965-now: 11
    
    IdealSequence1500 <- c(seq(1500,1799,50),seq(1800,1960,20),seq(1965,2015,5))
    
    # if after 1900
    # every 5 years: 24
    
    IdealSequence1900 <- seq(1900,2015,5)
    IdealSequence1900dense <- seq(1900,2015,2)
    # Given those ideal year tick sequences I want to find which are the available years
    # in the data closest to those values
    
    if (YearsWithData[1]<1900){
      TickYears <- IdealSequence1500[IdealSequence1500 %in% YearsWithData]
    } else {
      TickYears <- IdealSequence1900[IdealSequence1900 %in% YearsWithData]
      if (length(TickYears) < 8){
        TickYears <- IdealSequence1900dense[IdealSequence1900dense %in% YearsWithData]
      }
    }
    
    # add the latest year available if not in the TickYears
    if (!(YearsWithData[length(YearsWithData)] %in% TickYears)){
      TickYears <- c(TickYears,YearsWithData[length(YearsWithData)])
    }
    
    pdata <- pdata[,as.character(TickYears)]
    
    pdata2 <- expand.grid(names(pdata)[1],as.numeric(unlist(pdata[!is.na(pdata[,1]),1])), KEEP.OUT.ATTRS = F, stringsAsFactors = F)
    
    
    for (L_i in 2:ncol(pdata)){
      pdata2 <- rbind(pdata2,expand.grid(names(pdata)[L_i],as.numeric(unlist(pdata[!is.na(pdata[,L_i]),L_i])),
                                         KEEP.OUT.ATTRS = F, stringsAsFactors = F))
    }
    
    names(pdata2) <- c("Year","Value")
    
    if (LogPlot){
      library(scales)
      setwd("/home/michalis/PhD/Clio Infra/Website/html/graphs")
      plot(pdata2, type="l",log="y")
      breaks <- axTicks(side=2)
      svg(filename=paste(GeoLevel,"_",XxZzYyIndicNoSpaceXxZzYy,".svg", sep = ""),
          width=12, 
          height=4, 
          pointsize=10)
      
      print(ggplot(pdata2, aes(x=Year, y=Value)) + ggtitle(paste(GeoLevelTitle," : evolution of ",XxZzYyTitleXxZzYy,sep="")) +
              scale_y_continuous(breaks=breaks) +
              coord_trans(y="log") +
              geom_boxplot(outlier.size = 0, outlier.stroke = 0, width = 0.5) +
              geom_point(color="#b1cfe9", alpha = 0.8, position = position_jitter(width = 0.3))+
              stat_summary(fun.data = give.n, geom = "text", color="#337ab7", fun.y = median)+
              stat_summary(fun.data = mean.n, geom = "text", fun.y = mean, angle = 90, position = position_nudge(x = -.35)) +
              xlab("") + ylab("") + theme_gray() +
              theme(plot.title = element_text(color="#337ab7", size=14, face="bold"),
                    axis.text = element_text(color = "darkgray", size=13),
                    axis.title.x = element_text(color = "darkgray"),
                    axis.title.y = element_text(color = "darkgray", face = "bold", size=14)))
      
      dev.off()
    } else {
      setwd("/home/michalis/PhD/Clio Infra/Website/html/graphs")
      svg(filename=paste(GeoLevel,"_",XxZzYyIndicNoSpaceXxZzYy,".svg", sep = ""),
          width=12, 
          height=4, 
          pointsize=10)
      
      print(ggplot(pdata2, aes(x=Year, y=Value)) + ggtitle(paste(GeoLevelTitle," : evolution of ",XxZzYyTitleXxZzYy,sep="")) +
              geom_boxplot(outlier.size = 0,outlier.stroke = 0, width = 0.5) + 
              geom_point(color="#b1cfe9", alpha = 0.8, position = position_jitter(width = 0.3))+
              stat_summary(fun.data = give.n, geom = "text", color="#337ab7", fun.y = median) +
              stat_summary(fun.data = mean.n, geom = "text", fun.y = mean, angle = 90, position = position_nudge(x = -.35)) +
              xlab("") + ylab("") + theme_gray() +
              theme(plot.title = element_text(color="#337ab7", size=14, face="bold"),
                    axis.text = element_text(color = "darkgray", size=13),
                    axis.title.x = element_text(color = "darkgray"),
                    axis.title.y = element_text(color = "darkgray", face = "bold", size=14)))
      
      dev.off()
    }
  } else {
    svg(filename=paste(GeoLevel,"_",XxZzYyIndicNoSpaceXxZzYy,".svg", sep = ""),
        width=12, 
        height=4, 
        pointsize=10)
    
    print(ggplot() + ggtitle(paste(GeoLevelTitle," : evolution of ",XxZzYyTitleXxZzYy,sep="")) +
            coord_trans(y="log") +
            geom_boxplot()+ geom_point(color="#b1cfe9", position = position_dodge(width = 0.75))+
            xlab("") + ylab("") + theme_gray() +
            theme(plot.title = element_text(color="#337ab7", size=14, face="bold"),
                  axis.text = element_text(color = "darkgray", size=13),
                  axis.title.x = element_text(color = "darkgray"),
                  axis.title.y = element_text(color = "darkgray", face = "bold", size=14)))
    
    dev.off()
  }
  #jsondata <- toJSON(pdata)
  #write(jsondata,paste0("/home/michalis/PhD/Clio Infra/Website/JSON/",XxZzYyIndic0NoSpaceNoParXxZzYy,".json"))
  #rm(jsondata,pdata,pdata2)
}