library(dplyr)
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?

unique(SCC$EI.Sector) # to choose the right expression to look for
vehicl_index<-grep("Vehicles",SCC$SCC.Level.Two)
scc_nums<-SCC$SCC[vehicl_index]
data<-subset(NEI, NEI$fips=="24510" & NEI$SCC%in%scc_nums)
data<-data%>%group_by(year)%>%summarise(emissions=sum(Emissions))
png("plot5.png", width=480, height=480, unit="px")
barplot(height=data$emissions,
        names.arg=data$year,
        main="Motor Vehicle Source Emissions in Baltimore"
        )
dev.off()