# Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
# Using the base plotting system, make a plot showing the total PM2.5 emission from 
# all sources for each of the years 1999, 2002, 2005, and 2008.

NEI.file  <- "summarySCC_PM25.rds"
SCC.file  <- "Source_Classification_Code.rds"

message(paste("Reading ", NEI.file, " dataset"))

NEI <- readRDS(NEI.file)

message(paste("Reading ", SCC.file, " dataset"))

SCC <- readRDS(SCC.file)

message("Calculating yearly total emissions")

NEI.total  <- aggregate(list(emissions = NEI$Emissions), by=list(year = NEI$year), FUN = sum)

message("Plotting and saving file")

par(mar=c(5,5,6,3))
barplot((NEI.total$emissions/1000), 
        ylim=c(0, 8000),
        names.arg = c(as.character(NEI.total$year)), 
        col="red",
        xlab = "Years", 
        ylab="PM2.5 Emissions (per 1000 tons)")
mtext(side=3, line=3, "PM2.5 Emissions in United States between 1999 and 2008", cex=1.2)
dev.copy(png, file = "plot1.png", width = 580, height = 580)
dev.off()