library(dplyr)
library(RColorBrewer)
library(ggplot2)

#attach() - The database is attached to the R search path. This means that the
#database is searched by R when evaluating a variable, so objects in the
#database can be accessed by simply giving their names.
#
#tbl_df() - A data frame tbl wraps a local data frame. The main advantage to
#using a tbl_df over a regular data frame is the printing: tbl objects only
#print a few rows and all the columns that fit on one screen, describing the
#rest of it as text.

#df <- NEI[ NEI$fips == '24510',]
#attach(df)
#sums <- df %>% group_by(year) %>% summarise ( sum = sum(Emissions))

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
#plot 1
g<- group_by(NEI, year) 
data<- summarise_each(g, funs(sum), Emissions)
plot(data, pch=19)

#plot 2
baltimore <- NEI [ which (NEI$fips == "24510"),]
g<- group_by(baltimore, year) 
data<- summarise_each(g, funs(sum), Emissions)
#alternatively data<-summarize(g, totals=sum(Emissions))
plot(data, type="l")

#plot 3
baltimore <- NEI [ which (NEI$fips == "24510"),]
g<- group_by(baltimore, year, type) 
data<- summarise_each(g, funs(sum), Emissions)
qplot(year, Emissions, data=data, color=type, geom=c("point", "smooth"))
#q<-qplot(year, Emissions, data=data, col=type)+geom_smooth(method=lm, se=FALSE)+geom_point(size=3)
#q<-q+xlab("Year")+ylab("Total PM2.5 Emissions (tons)")+labs(colour="Source")
#q<-q+ggtitle("PM2.5 Emissions in Baltimore by Source")+scale_x_continuous(breaks=seq(1999,2008, by=1))
#q

# plot 4
# Find all sources with combustion, Coal 
coal<- SCC [ grepl("Comb.*Coal",SCC$Short.Name),]
merged<-merge(NEI, coal, "SCC")
#sum up per year for all counties
g<- group_by(merged, year) 
data<- summarise_each(g, funs(sum), Emissions)
qplot(year, Emissions, data=data, geom=c("point", "smooth"))

#plot 5
baltimore <- NEI [ which (NEI$fips == "24510"),]
# Find all sources with motor vehicle
# motor vehicle in shortname + recreational + onroad category
motor<- SCC [ grepl("motor.*vehicle",SCC$Short.Name, ignore.case=T),]
motor<- rbind(motor, SCC [ grepl("Recreational",SCC$Short.Name, ignore.case=T),])
motor <- rbind(motor, SCC [ which (SCC$Data.Category == "Onroad"),])
#Find motor vehicle for baltimore
merged<-merge(baltimore, motor, "SCC")
#sum up emission per year 
g<- group_by(merged, year) 
data<- summarise_each(g, funs(sum), Emissions)
qplot(year, Emissions, data=data, geom=c("point", "smooth"), ylab="Emissions in Baltimore")

# plot 6
baltimore_n_la <- NEI [ which (NEI$fips == "24510" | NEI$fips == "06037"),]
# Find all sources with motor vehicle
# motor vehicle in shortname + recreational + onroad category
motor<- SCC [ grepl("motor.*vehicle",SCC$Short.Name, ignore.case=T),]
motor<- rbind(motor, SCC [ grepl("Recreational",SCC$Short.Name, ignore.case=T),])
motor <- rbind(motor, SCC [ which (SCC$Data.Category == "Onroad"),])
#Find motor vehicle for baltimore & LA
merged<-merge(baltimore_n_la, motor, "SCC")
#sum up emission per year 
g<- group_by(merged, fips, year) 
data<- summarise_each(g, funs(sum), Emissions)
change_la <- round((data[4,3] - data[1,3]) * 100 / data[1,3], 2) 
change_baltimore <- round((data[8,3] - data[4,3]) * 100 / data[4,3], 2)
greater_change<-paste(c("Changes in LA:", change_la , "%\n", "Changes in Baltimore:", change_baltimore, "%"),
                      collapse = " ")
#Add column City for better readibility
colnames(data)[1] <- "City"
data$City<- gsub("06037", "L.A.", data$City)
data$City <- gsub("24510", "Baltimore",data$City)
# Plot the graph
# and annotate the greater change
# somewhere in the empty spot, upper right area
#
p <- qplot(year, Emissions, data=data, color=City, geom=c("point", "smooth"))
p<- p + annotate("text", x=2005, y=3500, label=greater_change)
p<- p + geom_point()
print(p)