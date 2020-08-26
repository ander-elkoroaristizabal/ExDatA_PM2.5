## Downloading files and checking whether they are loaded:

if (!file.exists("exdata-data-NEI_data/summarySCC_PM25.rds") | !file.exists("exdata-data-NEI_data/Source_Classification_Code.rds")){
  if (!file.exists("exdata-data-NEI_data.zip")){
    temp = tempfile()
    download.file("exdata-data-NEI_data.zip", 
                  "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip", temp)
    unzip(temp)
    unlink(temp)
  }
  unzip("exdata-data-NEI_data.zip")
}

if (!exists("NEI") | !exists("SCC")){
  NEI <- readRDS("exdata-data-NEI_data/summarySCC_PM25.rds")
  SCC <- readRDS("exdata-data-NEI_data/Source_Classification_Code.rds")
}

## Question 1: 
# Have total emissions from PM2.5 decreased in the United States from 1999 to 2008?
# Using the base plotting system, make a plot showing the total PM2.5 emission
# from all sources for each of the years 1999, 2002, 2005, and 2008.

years = as.factor(c(1999,2002, 2005,2008),levels=c(1,2,3,4))
year_totals = c(0,0,0,0)
for (i in seq_along(years)) {
    year_totals[i] = sum(subset(NEI, year == years[i])$Emissions)
}
remove(i)
barplot(year_totals/1000~years, 
        ylab=expression('Total PM'[2.5]*' emission (kilotons)'), 
        xlab='',
        # main = expression(atop('Total PM'[2.5]*' emission in', 
        #                   'some years between 1999 and 2008')),
        main = expression('Total PM'[2.5]*' emissions through various years'),
        col = c(rgb(1,0,0, alpha = 0.9))
        )

## Question 2:
# Have total emissions from PM2.5 decreased in the Baltimore City, 
# Maryland (fips == "24510") from 1999 to 2008? 
# Use the base plotting system to make a plot answering this question.

Baltimore = subset(NEI, fips == "24510")
Baltimore_totals = c(0,0,0,0)
for (i in seq_along(years)) {
  Baltimore_totals[i] = sum(subset(Baltimore, year == years[i])$Emissions)
}
barplot(Baltimore_totals~years, 
        ylab=expression('Total PM'[2.5]*' emission (tons)'), 
        xlab='',
        # main = expression(atop('Total PM'[2.5]*' emission in', 
        #                   'some years between 1999 and 2008')),
        main = expression('Total PM'[2.5]*' emissions in Baltimore through various years'),
        col = c(rgb(1,0,0, alpha = 0.9))
)

## Question 3: 
# Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, 
# which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? 
# Which have seen increases in emissions from 1999–2008? 
# Use the ggplot2 plotting system to make a plot answer this question.

sources = c("POINT","NONPOINT", "ON-ROAD", "NON-ROAD")
totals = list("point_totals"=vector(),
              "nonpoint_totals"=vector(),
              "onroad_totals"=vector(), 
              "nonroad_totals"=vector())
for (j in seq_along(sources)){
  for (i in seq_along(years)) {
    totals[[j]][i] = sum(subset(Baltimore, ((year == years[i]) & (type == sources[j])))$Emissions)
  }
} 
remove(i,j)

df = data.frame("Source"= as.factor(rep(sources, each = 4)),
                "T.Emissions"= unlist(totals),
                "Year" = rep(years, each = 1)
                )

# Code using ggplot2

library(ggplot2)

(ggplot(data = df, aes(y = T.Emissions, x = Year, fill = Source))+guides(fill=FALSE)
+geom_bar(stat="identity")
+facet_wrap(.~Source)+
ggtitle(expression("PM"[2.5]*paste(" emissions in Baltimore ","City by various source types", sep="")))
+ylab(expression("total PM"[2.5]*" emission in tons")))

# Code using the base system

# par(mfrow=c(2,2), oma = c(0, 0, 2, 0))
# for (j in seq_along(sources)){
#   barplot(totals[[j]]~years, 
#           main=sources[j],
#           xlab='',
#           ylab = expression('Total PM'[2.5]*' emissions (tones)'),
#           col = c(rgb(1,0,0, alpha = 0.9))
#   )
# } 
# mtext(expression('Total PM'[2.5]*' emissions by source'), outer = TRUE, cex = 1.5)

## Question 4:
# Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?

Coal_Combustion = SCC[grepl("Fuel Comb.*Coal", SCC$EI.Sector),]
Emissions_CC = NEI[(NEI$SCC %in% Coal_Combustion$SCC), ]

total_CCE = c(0,0,0,0)
for (i in seq_along(years)) {
  total_CCE[i] = sum(subset(Emissions_CC, year == years[i])$Emissions)
}
to_plot_CCE = data.frame("Year"= years, "total_CCE"=total_CCE)
(ggplot(to_plot_CCE, aes(x = Year, y = total_CCE/1000, fill = as.numeric(Year))) 
  + scale_fill_gradient()
  + geom_bar(stat="identity") + guides(fill=FALSE)
  + xlab("Year") 
  + ylab(expression("total PM"[2.5]*" emissions (kilotons)")) 
  + labs(title = expression("Emissions of PM"[2.5]*" from coal combustion-related sources"))
  + theme(plot.title = element_text(hjust=0.5)))

## Question 5:
# How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?

motor = SCC[grep('vehicle',SCC$EI.Sector,ignore.case=TRUE),]
Emissions_Baltimore_motor = NEI[(NEI$SCC %in% motor$SCC) & NEI$fips == "24510", ]

total_Bm = c(0,0,0,0)
for (i in seq_along(years)) {
  total_Bm[i] = sum(subset(Emissions_Baltimore_motor, year == years[i])$Emissions)
}
to_plot_Bm = data.frame("Year"= years, "total"=total_Bm)
(ggplot(to_plot_Bm, aes(x = Year, y = total/1000, fill = as.numeric(Year))) 
  + scale_fill_gradient()
  + geom_bar(stat="identity") + guides(fill=FALSE)
  + xlab("Year") 
  + ylab(expression("total PM"[2.5]*" emissions (kilotons)")) 
  + labs(title = expression("Emissions of PM"[2.5]*" from motor vehicle sources in Baltimore"))
  + theme(plot.title = element_text(hjust=0.5)))

## Question 6:
# Compare emissions from motor vehicle sources in Baltimore City with 
# emissions from motor vehicle sources in Los Angeles County, California (fips == “06037”). 
# Which city has seen greater changes over time in motor vehicle emissions?

# It would suffice to repeat for LA the previous plot and compare them:

Emissions_LA_motor = NEI[(NEI$SCC %in% motor$SCC) & NEI$fips == "06037", ]
total_LAm = c(0,0,0,0)
for (i in seq_along(years)) {
  total_LAm[i] = sum(subset(Emissions_LA_motor, year == years[i])$Emissions)
}
to_plot_LAm = data.frame("Year"= years, "total"=total_LAm)
# (ggplot(to_plot_LAm, aes(x = Year, y = total/1000, fill = as.numeric(Year))) 
#   + scale_fill_gradient()
#   + geom_bar(stat="identity") + guides(fill=FALSE)
#   + xlab("Year") 
#   + ylab(expression("total PM"[2.5]*" emissions (kilotons)")) 
#   + labs(title = expression("Emissions of PM"[2.5]*" from motor vehicle sources in LA"))
#   + theme(plot.title = element_text(hjust=0.5)))

# But we can also do a joint plot:

to_plot_Bm$County <- "Baltimore City, MD"
to_plot_LAm$County <- "Los Angeles County, CA"
Baltimore_LA = rbind(to_plot_Bm,to_plot_LAm)
(ggplot(Baltimore_LA, aes(x = Year, y = total, fill = as.factor(County))) + guides(fill=FALSE)
  + geom_bar(stat="identity")
  + facet_grid(County~.,scales="free")
  + ggtitle(expression("Motor vehicle emission variation in Baltimore and Los Angeles"))
  + ylab(expression("Total PM"[2.5]*" emissions (tons)"))
  + theme(plot.title = element_text(hjust=0.5)))