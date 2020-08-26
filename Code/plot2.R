## Question 2:
# Have total emissions from PM2.5 decreased in the Baltimore City, 
# Maryland (fips == "24510") from 1999 to 2008? 
# Use the base plotting system to make a plot answering this question.

Baltimore = subset(NEI, fips == "24510")
Baltimore_totals = c(0,0,0,0)
for (i in seq_along(years)) {
  Baltimore_totals[i] = sum(subset(Baltimore, year == years[i])$Emissions)
}

png("./figures/plot2.png")
barplot(Baltimore_totals~years, 
        ylab=expression('Total PM'[2.5]*' emission (tons)'), 
        xlab='',
        # main = expression(atop('Total PM'[2.5]*' emission in', 
        #                   'some years between 1999 and 2008')),
        main = expression('Total PM'[2.5]*' emissions in Baltimore through various years'),
        col = c(rgb(1,0,0, alpha = 0.9))
)
dev.off()