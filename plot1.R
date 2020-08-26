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