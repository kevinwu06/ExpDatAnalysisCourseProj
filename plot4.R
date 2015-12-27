library(plyr)
unzip("exdata-data-NEI_data.zip")
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
coal_index <- grep("Coal", SCC[,3], fixed=FALSE)              # find rows of all coal sources
coal_scc <- SCC[coal_index,]                                  # subset sources with index
comb_index <- grep("Comb", coal_scc[,3], fixed=FALSE)         # find rows of all combustion sources
coal_comb <- coal_scc[comb_index,]                            # subset sources again with index
NEI_coal <- NEI[NEI$SCC %in% coal_comb[,1],]                  # subset data for coal combustion sources
sumPM25.scc <- ddply(NEI_coal, .(year), 
                      summarize, sum=sum(Emissions))          # summing emissions by year
png(filename = "plot4.png", width = 480, height = 480)        # opening PNG graphic device
barplot(sumPM25.scc$sum, names.arg=sumPM25.scc$year,          # barplot of data
        main="US Coal Combustion related PM2.5 Emissions", 
        ylab = "tons")
dev.off()