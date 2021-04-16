library(dplyr)
library(ggplot2)
NEI <- readRDS("summarySCC_PM25.rds")

# Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad)
# variable, which of these four sources have seen decreases in emissions from 1999–2008
# for Baltimore City? Which have seen increases in emissions from 1999–2008?
# Use the ggplot2 plotting system to make a plot answer this question.

data<-subset(NEI, NEI$fips=="24510")
data<-data %>% group_by(year,type,.add=TRUE)%>%summarise(pm25=sum(Emissions))
plot3<-ggplot(data)+
        geom_line(aes_string(x="year", y= "pm25", color= "type"))+
        ggtitle("Emissions in Baltimore")
ggsave("plot3.png", plot = plot3)
