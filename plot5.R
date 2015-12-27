library(plyr)
unzip("exdata-data-NEI_data.zip")
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
motor_index <- grep("Highway Veh", SCC[,3], fixed=FALSE)          # find rows of all vehicle sources
scc_motor <- SCC[motor_index,]                                    # subset sources with index
NEI_motor <- NEI[NEI$SCC %in% scc_motor[,1],]                     # subset data for vehicle sources
motorPM25 <- ddply(NEI_motor, .(year, fips), 
                      summarize, sum=sum(Emissions))              # summing emissions by year, fips
motorPM25.Balt <- subset(motorPM25, fips=="24510", 
                       select = year:sum)                         # subsestting Baltimore City data
png(filename = "plot5.png", width = 480, height = 480)            # opening PNG graphic device
barplot(motorPM25.Balt$sum, names.arg=motorPM25.Balt$year,        # barplot of data
        main = "Motor Vehicle PM2.5 Emissions in Baltimore City", 
        ylab = "tons") 
dev.off()