# Compare emissions from motor vehicle sources in Baltimore City with emissions 
# from motor vehicle sources in Los Angeles County, California (fips == "06037"). 
# Which city has seen greater changes over time in motor vehicle emissions?


#*******************************************************************************
# Multiplot function to allow more than one plot 
# refer to: http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
        require(grid)
        
        # Make a list from the ... arguments and plotlist
        plots <- c(list(...), plotlist)
        
        numPlots = length(plots)
        
        # If layout is NULL, then use 'cols' to determine layout
        if (is.null(layout)) {
                # Make the panel
                # ncol: Number of columns of plots
                # nrow: Number of rows needed, calculated from # of cols
                layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                                 ncol = cols, nrow = ceiling(numPlots/cols))
        }
        
        if (numPlots==1) {
                print(plots[[1]])
                
        } else {
                # Set up the page
                grid.newpage()
                pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
                
                # Make each plot, in the correct location
                for (i in 1:numPlots) {
                        # Get the i,j matrix positions of the regions that contain this subplot
                        matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
                        
                        print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                                        layout.pos.col = matchidx$col))
                }
        }
}
#*******************************************************************************


library(ggplot2)

NEI.file  <- "summarySCC_PM25.rds"
SCC.file  <- "Source_Classification_Code.rds"

Cities  <- data.frame(fips = c("24510", "06037"), cityName = factor(c("Baltimore", "Los Angeles")))

message(paste("Reading ", NEI.file, " dataset"))

NEI <- readRDS(NEI.file)

message(paste("Reading ", SCC.file, " dataset"))

SCC <- readRDS(SCC.file)

message("Extracting Baltimore and Los Angeles data")

NEI.Cities <- NEI[which(NEI$fips %in% Cities$fips),]

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

NEI.MotorCities <- NEI.Cities[NEI.Cities$SCC %in% SCC.Motorvehicle, ]

message("Calculating yearly total emissions")

NEI.Emissions.total  <- aggregate(list(emissions = NEI.MotorCities$Emissions), by=list(year = NEI.MotorCities$year, fips = NEI.MotorCities$fips), FUN = sum)

message("Merging emissions with City data frame")

NEI.Emissions.total  <- merge(NEI.Emissions.total, Cities, all.x = TRUE)

message("Subsetting and merging to provide a dataset that ")

# Temporary dataset that holds emissions for the year 1999 for the two cities

baseNEI  <- NEI.Emissions.total[which(NEI.Emissions.total$year==1999), c("fips", "emissions", "cityName" ) ]

# Changing the emission column name to make it more descriptive, we are going
# to use the 1999 emission as a base point to calculate the percentage change
# of emissions through the years.
names(baseNEI)[2] = "reference.emission.year.1999"

NEI.Percentage <- merge(NEI.Emissions.total, baseNEI, all.x = TRUE)

# The percentage change is calculate as:  ((<actual emission> / <reference year emission> ) - 1) * 100
NEI.Percentage$percent  <- (NEI.Percentage$emissions / NEI.Percentage$reference.emission.year.1999 - 1) * 100

message("Plotting and saving file")

par(mar=c(5,5,5,5))

p1  <- ggplot(NEI.Emissions.total, aes(x=year, weight=emissions, fill=cityName)) +
        geom_histogram(binwidth=.5, position="dodge") +
        labs(fill="City", x="Year", y="Emissions in tons") +
        ggtitle("Total motor vehicle emissions")


p2  <- ggplot(NEI.Percentage, aes(x=year, y=percent, colour=cityName, group=cityName)) +
        geom_line(size=1 ) +        
        scale_colour_discrete(name = "City") +
        labs(x="Year", y=" % Percentage change from year 1999") +
        ggtitle("Change over time in motor vehicle emissions since 1999") + geom_point(alpha=.3, size=5)

multiplot(p1, p2, cols=2)

dev.copy(png, file = "plot6.png", width = 900, height = 380)
dev.off()


