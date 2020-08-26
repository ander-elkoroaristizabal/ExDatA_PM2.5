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