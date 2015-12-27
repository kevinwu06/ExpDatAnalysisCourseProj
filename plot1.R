library(plyr)
unzip("exdata-data-NEI_data.zip")
NEI <- readRDS("summarySCC_PM25.rds")
sumPM25 <- ddply(NEI, .(year), summarize, sum=sum(Emissions)) # summing emissions by year
png(filename = "plot1.png", width = 480, height = 480)        # opening PNG graphic device
ylab <- c(0,2000000,4000000,6000000,8000000)                  # creating y axis labels
barplot(sumPM25$sum, names.arg=sumPM25$year,                  # barplot of data
        main = "US PM2.5 Emissions from all sources", 
        yaxt="n", ylab = "tons")                               
axis(2,at=ylab,labels=formatC(ylab, format="d",big.mark=",")) # adding y axis labels
dev.off() 