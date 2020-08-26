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

ggsave("./figures/plot5.png")