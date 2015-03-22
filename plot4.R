# Across the United States, how have emissions from coal combustion-related sources 
# changed from 1999-2008?

library(ggplot2)

NEI.file  <- "summarySCC_PM25.rds"
SCC.file  <- "Source_Classification_Code.rds"

message(paste("Reading ", NEI.file, " dataset"))

NEI <- readRDS(NEI.file)

message(paste("Reading ", SCC.file, " dataset"))

SCC <- readRDS(SCC.file)

message("Extracting coal combustion related data")

SCC.CoalCombustion  <- SCC[grepl("[Cc]oal", SCC$EI.Sector) & grepl("[Cc]omb", SCC$EI.Sector), "SCC"]

NEI.CoalCombustion <- NEI[NEI$SCC %in% SCC.CoalCombustion, ]

message("Calculating yearly total emissions")

NEI.CoalCombustion.total  <- aggregate(list(emissions = NEI.CoalCombustion$Emissions), by=list(year = NEI.CoalCombustion$year), FUN = sum)

message("Plotting and saving file")

par(mar=c(5,5,5,5))
barplot(t(as.matrix(NEI.CoalCombustion.total$emissions/1000)), 
        ylim=c(0, 600), 
        names.arg = c(as.character(NEI.CoalCombustion.total$year)), 
        col="red", 
        xlab="Years", ylab=" Emissions (in thousands tons)")
        mtext(side=3, line=2, "Coal combustion-related emissions in US between 1999 and 2008", cex=1.2)

dev.copy(png, file = "plot4.png", width = 680, height = 580)
dev.off()