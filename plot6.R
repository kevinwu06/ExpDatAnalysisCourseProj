library(ggplot2)
library(plyr)
library(dplyr)
unzip("exdata-data-NEI_data.zip")
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
motor_index <- grep("Highway Veh", SCC[,3], fixed=FALSE)    # find rows of all vehicle sources
scc_motor <- SCC[motor_index,]                              # subset sources with index
NEI_motor <- NEI[NEI$SCC %in% scc_motor[,1],]               # subset data for vehicle sources
motorPM25 <- ddply(NEI_motor, .(year, fips), 
                   summarize, sum=sum(Emissions))           # summing emissions by year, fips
motorPM25.comp <- subset(motorPM25, fips=="24510"
                         | fips=="06037", 
                         select = year:sum)                 # subsestting Baltimore & LA data
motorPM25.comp <- group_by(motorPM25.comp,fips)             
motorPM25.comp <- mutate(motorPM25.comp,Chg=sum-
                           lag(sum, default = sum[1]))      # calculating 3Y change in emissions
motorPM25.comp <- arrange(motorPM25.comp,fips)
png(filename = "plot6.png", width = 480, height = 480)      # opening PNG graphic device
par(mfcol = c(2, 2))                                        
plot(motorPM25.comp$year[1:4], motorPM25.comp$sum[1:4],     # plotting LA Motor Vehicle Emissions 
     ylab="Motor Vehicle Emissions (tons)", xlab="Year",
     main="Los Angeles County", ylim=c(0, 5000),
     pch=19)
barplot(motorPM25.comp$Chg[1:4],motorPM25.comp$year[1:4],   # plotting 3Y change in Emissions
        main="Los Angeles County",ylab="3Y Change (tons)",
        names.arg=motorPM25.comp$year[1:4],
        ylim=c(-500,400))
plot(motorPM25.comp$year[5:8], motorPM25.comp$sum[5:8],     # plotting Balt City Motor Vehicle Emissions
     ylab="Motor Vehicle Emissions (tons)", xlab="Year",
     main="Baltimore City", ylim=c(0, 5000),
     pch=19)
barplot(motorPM25.comp$Chg[5:8],motorPM25.comp$year[5:8],   # plotting 3Y change in Emissions
        main="Baltimore City",ylab="3Y Change (tons)",
        names.arg=motorPM25.comp$year[5:8],
        ylim=c(-500,400))
dev.off()