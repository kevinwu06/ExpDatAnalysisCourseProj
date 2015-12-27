library(ggplot2)
library(plyr)
unzip("exdata-data-NEI_data.zip")
NEI <- readRDS("summarySCC_PM25.rds")
sumPM25.type <- ddply(NEI, .(year, fips, type), 
                      summarize, sum=sum(Emissions))        # summing emissions by year, fips, type
PM.Balt.type <- subset(sumPM25.type, fips=="24510", 
                       select = year:sum)                   # subsestting Baltimore City data
png(filename = "plot3.png", width = 640, height = 480)      # opening PNG graphic device
qplot(year,sum, data=PM.Balt.type, facets = .~type,         # plot of emissions by year with type facet
      geom=c("point","smooth"),method="lm", se=FALSE,
      ylab="Baltimore City Total Emissions (tons)",
      xlab="Year")
dev.off()