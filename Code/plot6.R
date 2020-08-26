## Question 6:
# Compare emissions from motor vehicle sources in Baltimore City with 
# emissions from motor vehicle sources in Los Angeles County, California (fips == “06037”). 
# Which city has seen greater changes over time in motor vehicle emissions?

motor = SCC[grep('vehicle',SCC$EI.Sector,ignore.case=TRUE),]
Emissions_Baltimore_motor = NEI[(NEI$SCC %in% motor$SCC) & NEI$fips == "24510", ]
Emissions_LA_motor = NEI[(NEI$SCC %in% motor$SCC) & NEI$fips == "06037", ]

# It would suffice to repeat for LA the previous plot and compare them, 
# but we will do a joint plot.

total_Bm = c(0,0,0,0)
for (i in seq_along(years)) {
  total_Bm[i] = sum(subset(Emissions_Baltimore_motor, year == years[i])$Emissions)
}
total_LAm = c(0,0,0,0)
for (i in seq_along(years)) {
  total_LAm[i] = sum(subset(Emissions_LA_motor, year == years[i])$Emissions)
}
to_plot_LAm = data.frame("Year"= years, "total"=total_LAm)

to_plot_Bm$County <- as.factor("Baltimore City, MD")
to_plot_LAm$County <- as.factor("Los Angeles County, CA")
Baltimore_LA = rbind(to_plot_Bm,to_plot_LAm)
(ggplot(Baltimore_LA, aes(x = Year, y = total, fill = as.factor(County))) + guides(fill=FALSE)
  + geom_bar(stat="identity")
  + facet_grid(County~.,scales="free")
  + ggtitle(expression("Motor vehicle emission variation in Baltimore and Los Angeles"))
  + ylab(expression("Total PM"[2.5]*" emissions (tons)"))
  + theme(plot.title = element_text(hjust=0.5)))

ggsave("./figures/plot6.png")