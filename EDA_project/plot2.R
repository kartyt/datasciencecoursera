library(dplyr)
NEI <- readRDS("summarySCC_PM25.rds")

# Have total emissions from PM2.5 decreased in the Baltimore City, Maryland
# (fips == "24510") from 1999 to 2008? # Use the base plotting system to make
# a plot answering this question.

df<-group_by(NEI,year)
Baltimore<-df%>%filter(fips=="24510")%>%summarise(tot=sum(Emissions))
png("plot2.png", width=480, height=480, unit="px")
barplot(height=Baltimore$tot,
         names.arg=Baltimore$year,
         ylab="Emissions",main="Total PM2.5 emissions over years in Baltimore",
        )
dev.off()