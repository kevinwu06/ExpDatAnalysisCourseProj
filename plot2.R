library(plyr)
unzip("exdata-data-NEI_data.zip")
NEI <- readRDS("summarySCC_PM25.rds")
sumPM25.fips <- ddply(NEI, .(year,fips), summarize, sum=sum(Emissions)) # summing emissions by year,fips
sumPM25.Balt <- subset(sumPM25.fips, fips=="24510", select = year:sum)  # subsestting Baltimore City data
png(filename = "plot2.png", width = 480, height = 480)                  # opening PNG graphic device
barplot(sumPM25.Balt$sum, names.arg=sumPM25.Balt$year,                  # barplot of data
        main = "Total PM2.5 Emissions in Baltimore City", 
        ylab = "tons")                               
dev.off()