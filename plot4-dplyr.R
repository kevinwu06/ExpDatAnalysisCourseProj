#import the data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#do the analysis using dplyr
library(dplyr)

#find the SCC codes that relate to vehicle usage and create a character vector
vehicles<-SCC[grep("vehicles", SCC$EI.Sector, ignore.case=TRUE),]
vehicles<-as.character(vehicles$SCC)

#subset the data relevant for Baltimore
BaltimoreData<-NEI[NEI$fips=="24510",]

#use dplyr to get the yearly totals for SCC codes that correspond to vehicle sources
BaltimoreData<-filter(BaltimoreData,SCC %in% vehicles)
byYear<-group_by(BaltimoreData, year)
sum<-summarize(byYear, totals=sum(Emissions))

#open a png device, plot, and close the png device
png(filename="plot5.png", width=480, height=480, units="px")

#make the plot
library(ggplot2)
q<-qplot(year, totals, data=sum)+geom_smooth(method=lm, se=FALSE)+geom_point(size=3)
q<-q+xlab("Year")+ylab("Total PM2.5 Emissions (tons)")
q<-q+ggtitle("PM2.5 Emissions by Vehicle Sources in Baltimore")+scale_x_continuous(breaks=seq(1999,2008, by=1))
q

dev.off()