# How have emissions from motor vehicle sources changed from 1999-2008 in Baltimore City?

NEI.file  <- "summarySCC_PM25.rds"
SCC.file  <- "Source_Classification_Code.rds"
Baltimore.fip <- "24510"

message(paste("Reading ", NEI.file, " dataset"))

NEI <- readRDS(NEI.file)

message(paste("Reading ", SCC.file, " dataset"))

SCC <- readRDS(SCC.file)

message("Extracting Baltimore data")

NEI.Baltimore <- NEI[which(NEI$fips == Baltimore.fip),]

message("Extracting motor vehicle data")

# Extract from: http://www.epa.gov/ttn/chief/net/2008neiv3/2008_neiv3_tsd_draft.pdf

# The four sectors for on-road mobile sources include emissions from motorized vehicles that are normally
# operated on public roadways. This includes passenger cars, motorcycles, minivans, sport-utility vehicles, lightduty
# trucks, heavy-duty trucks, and buses. The sectors include emissions from parking areas as well as emissions
# while the vehicles are moving.
# SCCs starting with 22010 define the light duty gasoline vehicles including motorcycles, with the exception of
# SCCs starting with 220107, which define the heavy duty gasoline vehicles. SCCs starting with 22300 define the
# light duty diesel vehicles, with the exception of SCCs starting with 223007 that define the heavy duty diesel
# vehicles.


SCC.Motorvehicle  <- SCC[grepl("^((22010)|(220107)|(22300)|(223007))", SCC$SCC), "SCC"]

NEI.MotorBaltimore <- NEI.Baltimore[NEI.Baltimore$SCC %in% SCC.Motorvehicle, ]

message("Calculating yearly total emissions")

NEI.Emissions.total  <- aggregate(list(emissions = NEI.MotorBaltimore$Emissions), by=list(year = NEI.MotorBaltimore$year), FUN = sum)

message("Plotting and saving file")

par(mar=c(5,5,5,5))
barplot(NEI.Emissions.total$emissions, 
        ylim=c(0, 350), 
        names.arg = c(as.character(NEI.Emissions.total$year)), 
        col="red", 
        xlab="Years", ylab=" Emissions (in tons)")
        mtext(side=3, line=2, "Motor vehicle emissions in Baltimore city between 1999 and 2008", cex=1.2)

dev.copy(png, file = "plot5.png", width = 680, height = 580)
dev.off()