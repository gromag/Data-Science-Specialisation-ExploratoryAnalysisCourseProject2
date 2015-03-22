# Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, 
# which of these four sources have seen decreases in emissions from 1999-2008 for Baltimore City? 
# Which have seen increases in emissions from 1999-2008? Use the ggplot2 plotting system to make a plot answer this question.

library(ggplot2)

NEI.file  <- "summarySCC_PM25.rds"
SCC.file  <- "Source_Classification_Code.rds"
Baltimore.fip <- "24510"

message(paste("Reading ", NEI.file, " dataset"))

NEI <- readRDS(NEI.file)

message(paste("Reading ", SCC.file, " dataset"))

SCC <- readRDS(SCC.file)

message("Extracting Baltimore data")

NEI.Baltimore <- NEI[which(NEI$fips == Baltimore.fip),]

message("Calculating yearly total emissions")

NEI.Baltimore.total  <- aggregate(list(emissions = NEI.Baltimore$Emissions), by=list(year = NEI.Baltimore$year, type = NEI.Baltimore$type), FUN = sum)

message("Plotting and saving file")

m <- melt(NEI.Baltimore.total, id=c("year", "type"))

ggplot(data=m, aes(x=year, y=value, colour=type)) + 
        geom_line() + 
        ylab("PM2.5 Emission in tons") +
        xlab("Year") +
        ggtitle("PM2.5 Emissions in Baltimore City between 1999 and 2008 by type")

dev.copy(png, file = "plot3.png", width = 780, height = 580)
dev.off()