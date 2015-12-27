library(data.table)
library(plyr)
library(dplyr)
library(ggplot2)

plot1 <- function(useCache = TRUE) {
  
  # if using cached files then don't download
  if (useCache) { 
    URL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
    zipfile <- "exdata_data_NEI_data.zip"
    datafiles <- "summarySCC_PM25.rds"
    download.file(URL, zipfile,  method="curl")
    unzip(zipfile, datafiles, exdir = ".")
  }
  
  nei <- data.table(readRDS("summarySCC_PM25.rds"))
  plotData <- nei[,sum(Emissions)/1000000, by=year]
  rm(nei)
  dev.set(2)
  png(file = "plot1.png", bg="white")
  plot(plotData$year, plotData$V1, main="Decrease in PM2.5 Emissions, 1999-2008", 
       ylab="Total Emissions (million tons)", xlab="Year", ylim=c(0,8))
  dev.off()
  
} # plot1

plot2 <- function(useCache = TRUE) {
  
  # if using cached files then don't download
  if (useCache) { 
    URL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
    zipfile <- "exdata_data_NEI_data.zip"
    datafiles <- "summarySCC_PM25.rds"
    download.file(URL, zipfile,  method="curl")
    unzip(zipfile, datafiles, exdir = ".")
  }
  
  nei <- data.table(readRDS("summarySCC_PM25.rds"))
  bc <- nei[fips == "24510",]
  plotData <- bc[,sum(Emissions), by=year]
  dev.set(2)
  png(file = "plot2.png", bg="white")
  plot(plotData$year, plotData$V1, main="Decrease in PM2.5 Emissions, Baltimore City, MD 1999-2008",
       ylab="Total Emissions (tons)", xlab="Year", ylim=c(0,3500))
  dev.off()
  
} # plot2

plot3 <- function(useCache = TRUE) {
  
  # if using cached files then don't download
  if (useCache) { 
    URL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
    zipfile <- "exdata_data_NEI_data.zip"
    datafiles <- "summarySCC_PM25.rds"
    download.file(URL, zipfile,  method="curl")
    unzip(zipfile, datafiles, exdir = ".")
  }
  
  nei <- data.table(readRDS("summarySCC_PM25.rds"))
  bc <- nei[fips == "24510",]
  rm(nei)
  bc <- mutate(bc, typeFAC = as.factor(type))
  bcplot <- bc[, sum(Emissions), by=c("year", "typeFAC")]
  g <- ggplot(bcplot, aes(year, V1)) +
    facet_grid(. ~ typeFAC) + 
    geom_point() + 
    ylab("Emissions") + 
    xlab("Year") + 
    ggtitle("Emissions by Type for Baltimore City, 1999-2008")
  
  dev.set(2)
  png(file="plot3.png", width=960)
  print(g)
  dev.off()
  
} # plot3

plot4 <- function(useCache = TRUE) {
  
  # if using cached files then don't download
  if (useCache) { 
    URL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
    zipfile <- "exdata_data_NEI_data.zip"
    datafiles <- c("summarySCC_PM25.rds", "Source_Classification_Code.rds")
    download.file(URL, zipfile,  method="curl")
    unzip(zipfile, datafiles, exdir = ".")
  }
  
  scc <- data.table(readRDS("Source_Classification_Code.rds"))
  # thanks to Al Warren: https://class.coursera.org/exdata-011/forum/thread?thread_id=35#comment-618
  coalFilter <- scc[grep("^Fuel Comb -(.*)- Coal$", scc$EI.Sector), ]
  
  # save some memory
  rm(scc)
  
  # retrieve PM25 data
  nei <- data.table(readRDS("summarySCC_PM25.rds"))
  
  # extract only the coal data
  coal <- inner_join(nei, coalFilter, by = "SCC")
  
  # save some more memory
  rm(nei)
  
  # assemble the data we're interested in displaying
  coalplot <- coal[, sum(Emissions)/1000000, by=year]
  
  g <- ggplot(coalplot, aes(year, V1)) +
    geom_point() + 
    ylab("Emissions (million tons)") + 
    xlab("Year") + 
    ggtitle("Total U.S. Emissions, 1999-2008")
  
  dev.set(2)
  png(file="plot4.png")
  print(g)
  dev.off()
  
} # plot4

plot5 <- function(useCache = TRUE) {
  
  # if using cached files then don't download
  if (useCache) { 
    URL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
    zipfile <- "exdata_data_NEI_data.zip"
    datafiles <- c("summarySCC_PM25.rds", "Source_Classification_Code.rds")
    download.file(URL, zipfile,  method="curl")
    unzip(zipfile, datafiles, exdir = ".")
  }
  
  scc <- data.table(readRDS("Source_Classification_Code.rds"))
  # thanks to Al Warren: https://class.coursera.org/exdata-011/forum/thread?thread_id=35#comment-618
  vehicleFilter <- scc[grep("^Mobile -(.*)-Road(.*)", scc$EI.Sector), ]
  
  # save some memory
  rm(scc)
  
  # retrieve PM25 data and then filter it down to Baltimore City
  nei <- data.table(readRDS("summarySCC_PM25.rds"))
  bc <- nei[fips == "24510",]
  
  # save some more memory
  rm(nei)
  
  
  # extract only the coal data
  vehicle <- inner_join(bc, vehicleFilter, by = "SCC")
  
  
  # assemble the data we're interested in displaying
  vehicleplot <- vehicle[, sum(Emissions), by=year]
  
  g <- ggplot(vehicleplot, aes(year, V1)) +
    geom_point() + 
    ylab("Emissions (in tons)") + 
    xlab("Year") + 
    ggtitle("Changes in Baltimore City Motor Vehicle Emissions, 1999-2008")
  
  dev.set(2)
  png(file="plot5.png")
  print(g)
  dev.off()
  
} # plot5

plot6 <- function(useCache = TRUE) {
  
  # if using cached files then don't download
  if (useCache) { 
    URL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
    zipfile <- "exdata_data_NEI_data.zip"
    datafiles <- c("summarySCC_PM25.rds", "Source_Classification_Code.rds")
    download.file(URL, zipfile,  method="curl")
    unzip(zipfile, datafiles, exdir = ".")
  }
  
  scc <- data.table(readRDS("Source_Classification_Code.rds"))
  # thanks to Al Warren: https://class.coursera.org/exdata-011/forum/thread?thread_id=35#comment-618
  vehicleFilter <- scc[grep("^Mobile -(.*)-Road(.*)", scc$EI.Sector), ]
  
  # save some memory
  rm(scc)
  
  # retrieve PM25 data and then filter it down to Baltimore City
  nei <- data.table(readRDS("summarySCC_PM25.rds"))
  bc <- nei[fips == "24510",]
  la <- nei[fips == "06037",]
  
  # save some more memory
  rm(nei)
  gc()
  
  # extract only the motor vehicle data
  bcv <- inner_join(bc, vehicleFilter, by = "SCC")
  lav <- inner_join(la, vehicleFilter, by = "SCC")
  
  
  # assemble the data we're interested in displaying
  laplot <- lav[, sum(Emissions), by=year] %>%
    mutate(City = "Los Angeles")
  bcplot <- bcv[, sum(Emissions), by=year] %>%
    mutate(City = "Baltimore City")
  vehicleplot <- rbind(laplot, bcplot)
  vehicleplot <- mutate(vehicleplot, City = as.factor(City))
  
  g <- ggplot(vehicleplot, aes(year, V1)) +
    geom_point() + 
    facet_grid(City ~ ., scales = "free_y") + 
    geom_smooth(method = "lm", se = FALSE) + 
    ylab("Emissions (in tons)") + 
    xlab("Year") + 
    ggtitle("Baltimore City and Los Angeles Motor Vehicle Emissions, 1999-2008")
  
  dev.set(2)
  png(file="plot6.png")
  print(g)
  dev.off()
  
} # plot6