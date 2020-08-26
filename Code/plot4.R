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

ggsave("./figures/plot4.png")