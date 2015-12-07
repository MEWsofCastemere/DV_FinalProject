require("jsonlite")
require("RCurl")
require(ggplot2)
require(dplyr)
require(gridExtra)

dfs <- data.frame(fromJSON(getURL(URLencode('skipper.cs.utexas.edu:5001/rest/native/?query="select * from ADAMDATA"'),httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_mew2795', PASS='orcl_mew2795', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), )) %>% select(YEAR, DEATHS_DIRECT, DEATHS_INDIRECT, DAMAGE_PROPERTY, DAMAGE_CROPS, INJURIES_DIRECT, INJURIES_INDIRECT, STATE) %>% filter(YEAR > 0, DEATHS_DIRECT > 0, DEATHS_INDIRECT > 0, DAMAGE_PROPERTY > 0, DAMAGE_CROPS > 0, DAMAGE_PROPERTY != 'null', DAMAGE_CROPS != 'null') %>% mutate(TOTAL_INJURIES = cumsum(INJURIES_DIRECT) + cumsum(INJURIES_INDIRECT))
DDPY <- ggplot() + 
  coord_cartesian() + 
  #scale_color_gradient2(mid = ("Green"), high="Red") +
  scale_x_discrete() +
  scale_y_continuous() +
  facet_wrap(~YEAR, ncol = 1) +
  labs(title='StormEvents Barchart\ndeaths_direct, avg(deaths_direct), ') +
  labs(x=paste("Year"), y=paste("Deaths Direct")) +
  layer(data=dfs, 
        mapping=aes(x=YEAR, y=(DEATHS_DIRECT)), 
        stat="identity", 
        stat_params=list(), 
        geom="bar",
        geom_params=list(color="blue"), 
        position=position_identity()
  ) + coord_flip() +
  layer(data=dfs, 
        mapping=aes(x=YEAR, y=DEATHS_DIRECT, label=(DEATHS_DIRECT)), 
        stat="identity", 
        stat_params=list(), 
        geom="text",
        geom_params=list(colour="black", hjust=0.5), 
        position=position_identity()
  ) +
  layer(data=dfs, 
        mapping=aes(yintercept = mean(DEATHS_DIRECT)), 
        geom="hline",
        geom_params=list(colour="red")
  )
DDPY
IDPY <- ggplot() + 
  coord_cartesian() + 
  #scale_color_gradient2(mid = ("Green"), high="Red") +
  scale_x_discrete() +
  scale_y_continuous() +
  facet_wrap(~YEAR, ncol = 1) +
  labs(title='StormEvents Barchart\ndeaths_indirect, avg(deaths_indirect), ') +
  labs(x=paste("Year"), y=paste("Deaths Indirect")) +
  layer(data=dfs, 
        mapping=aes(x=YEAR, y=(DEATHS_INDIRECT)), 
        stat="identity", 
        stat_params=list(), 
        geom="bar",
        geom_params=list(color="blue"), 
        position=position_identity()
  ) + coord_flip() +
  layer(data=dfs, 
        mapping=aes(x=YEAR, y=DEATHS_INDIRECT, label=(DEATHS_INDIRECT)), 
        stat="identity", 
        stat_params=list(), 
        geom="text",
        geom_params=list(colour="black", hjust=0.5), 
        position=position_identity()
  ) +
  layer(data=dfs, 
        mapping=aes(yintercept = mean(DEATHS_INDIRECT)), 
        geom="hline",
        geom_params=list(colour="red")
  )
IDPY
DCPY <- ggplot() + 
  coord_cartesian() + 
  #scale_color_gradient2(mid = ("Green"), high="Red") +
  scale_x_discrete() +
  scale_y_continuous() +
  facet_wrap(~YEAR, ncol = 1) +
  labs(title='StormEvents Barchart\nDAMAGE_CROPS, avg(DAMAGE_CROPS), ') +
  labs(x=paste("Year"), y=paste("Damage Crops")) +
  layer(data=dfs, 
        mapping=aes(x=YEAR, y=(DAMAGE_CROPS)), 
        stat="identity", 
        stat_params=list(), 
        geom=("bar"),
        geom_params=list(colour="blue"), 
        position=position_identity()
  ) + coord_flip() +
  layer(data=dfs, 
        mapping=aes(x=YEAR, y=DAMAGE_CROPS, label=(DAMAGE_CROPS)), 
        stat="identity", 
        stat_params=list(), 
        geom="text",
        geom_params=list(colour="black", hjust=0.5), 
        position=position_identity()
  ) +
  layer(data=dfs, 
        mapping=aes(yintercept = mean(DAMAGE_CROPS)), 
        geom="hline",
        geom_params=list(colour="red")
  )
DCPY
DPPY <- ggplot() + 
  coord_cartesian() + 
  #scale_color_gradient2(mid = ("Green"), high="Red") +
  scale_x_discrete() +
  scale_y_continuous() +
  facet_wrap(~YEAR, ncol = 1) +
  labs(title='StormEvents Barchart\nDAMAGE_PROPERTY, avg(DAMAGE_PROPERTY), ') +
  labs(x=paste("Year"), y=paste("DAMAGE PROPERTY")) +
  layer(data=dfs, 
        mapping=aes(x=YEAR, y=(DAMAGE_PROPERTY)), 
        stat="identity", 
        stat_params=list(), 
        geom="bar",
        geom_params=list(colour="blue"), 
        position=position_identity()
  ) + coord_flip() +
  layer(data=dfs, 
        mapping=aes(x=YEAR, y=DAMAGE_PROPERTY, label=(DAMAGE_PROPERTY)), 
        stat="identity", 
        stat_params=list(), 
        geom="text",
        geom_params=list(colour="black", hjust=0.5), 
        position=position_identity()
  ) +
  layer(data=dfs, 
        mapping=aes(yintercept = mean(DAMAGE_PROPERTY)), 
        geom="hline",
        geom_params=list(colour="red")
  )
DPPY
plot <- grid.arrange(DDPY, IDPY, DCPY, DPPY)
plot

