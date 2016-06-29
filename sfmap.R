library(dplyr)
library(ggmap)
library(ggplot2)
library(readr)

train = read.csv("train.csv")
SF.map = get_map(location="sanfrancisco",zoom=12,source="osm")
ggmap(SF.map)

# Sorting in ordrt of frequency with dplyr pakage
counts <- summarise(group_by(train, Category), Counts=length(Category))
counts <- counts[order(-counts$Counts),]

# This removes the "Other Offenses" category
top5 <- train[train$Category %in% counts$Category[c(1,3:6)],]

p <- ggmap(SF.map) 
p <- p + geom_point(data=top5, aes(x=X, y=Y,alpha=0.05),colour="red")
p <- p + theme(legend.position="none")                    
plot(p)

p <-  ggplot(data=top5, aes(x=X, y=Y))
p <- p + stat_bin2d(bins=25) + scale_fill_gradient(low="blue",high="red",space="Lab")
p <- p + xlim(-122.5, -122.35) + ylim(37.7,37.85)
plot(p)

ggsave("sf_top_crimes_map.png", p, width=14, height=10, units="in")


