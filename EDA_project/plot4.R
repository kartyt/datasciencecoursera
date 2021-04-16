library(dplyr)
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Across the United States, how have emissions from coal combustion-related
# sources changed from 1999–2008?

unique(SCC$EI.Sector) # to choose the right expression to look for
coal_index<-grep("Coal",SCC$EI.Sector)
scc_nums<-SCC$SCC[coal_index]

data<-subset(NEI, NEI$SCC%in%scc_nums)
data<-data%>%group_by(year)%>%summarise(emissions=sum(Emissions))
png("plot4.png", width=480, height=480, unit="px")
barplot(height=data$emissions,
        names.arg=data$year,
        main="Total PM2.5 emissions from coal combustion-related sources"
        )
dev.off()