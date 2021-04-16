library(dplyr)
library(ggplot2)
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Compare emissions from motor vehicle sources in Baltimore City with emissions
# from motor vehicle sources in Los Angeles County, California (fips == "06037").
# Which city has seen greater changes over time in motor vehicle emissions?

unique(SCC$EI.Sector) # to choose the right expression to look for
vehicl_index<-grep("Vehicles",SCC$SCC.Level.Two)
scc_nums<-SCC$SCC[vehicl_index]

both<-subset(NEI, (NEI$fips=="06037" | NEI$fips=="24510") & NEI$SCC%in%scc_nums)
both<-both %>% group_by(year,fips,.add=TRUE)%>%summarise(pm25=sum(Emissions))

plot6<-ggplot(both)+
        geom_line(aes_string(x="year", y= "pm25", color= "fips"))+
        labs(title = "Emissions in Baltimore and Los Angeles\n", x = "years", y = "pm 25", color = "cities\n")+
        scale_color_manual(labels = c("Los Angles", "Baltimore"),values = c("#CC79A7", "#56B4E9"))
ggsave("plot6.png", plot = plot6)