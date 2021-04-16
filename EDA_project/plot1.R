library(dplyr)
NEI <- readRDS("summarySCC_PM25.rds")

# Have total emissions from PM2.5 decreased in the United States from 1999 to 2008?
# Using the base plotting system, make a plot showing the total PM2.5 emission from
# all sources for each of the years 1999, 2002, 2005, and 2008.
df<-group_by(NEI,year)
total_emissions<-summarize(df, total_emissions=sum(Emissions))
png("plot1.png", width=480, height=480, unit="px")
barplot(height=total_emissions$total_emissions,
        names.arg=total_emissions$year,
        ylab="Emissions",main="Total PM2.5 emissions over years",
        )
dev.off()