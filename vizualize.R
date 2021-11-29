library(bakeoff)
library(tidyverse)
library(plotly)
library(cowplot)

colors <- bakeoff_colors()[c("garanceyellow", 
                             "maroon",
                             "pear",
                             "cocoa",
                             "brick",
                             "orange",  
                             "deepblue",
                             "nude",
                             "green",
                             "cardinal",
                             "yellow",
                             "vibrantorange",
                             "almond")]
names(colors) <- NULL
barPlot <- plot_ly(topFlavors %>% mutate(word = factor(word, levels = word)),
        x = ~percent,
        y = ~word,
        type = 'bar', 
        orientation = "h", 
        marker = list(color = colors)) %>%
  layout(xaxis = list(title = "Percent of bakes", tickformat = ".1%"), 
         yaxis = list(title = ""))

barPlot
htmlwidgets::saveWidget(barPlot, "../website/content/project/2021-11-16-great-british-bakeoff/barPlot.html")

riskRatio <- ggplot(ratios %>% mutate(centeredRiskRatio = riskRatio -1), aes(x = flavor, y = centeredRiskRatio, fill = flavor)) +
  geom_col()+
  scale_fill_manual(values = colors) +
  scale_y_continuous(breaks = c(-0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75), 
                     labels = c("0.25","0.5","0.75","1","1.25","1.5", "1.75"), 
                     limits = c(-0.8, 0.8))+
  ylab("Risk Ratio")+
  geom_hline(yintercept = 0, size = 1.5)+
  coord_flip() + 
  theme_minimal_vgrid()+
  theme(legend.position = "none", 
        axis.title.y=element_blank(),
        plot.background = element_rect(fill = "white")) 
  
rewardRatio <- ggplot(ratios %>% mutate(centeredRewardRatio = rewardRatio -1), aes(x = flavor, y = centeredRewardRatio, fill = flavor)) +
  geom_col()+
  scale_fill_manual(values = colors) +
  scale_y_continuous(breaks = c(-0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75), 
                     labels = c("0.25","0.5","0.75","1","1.25","1.5", "1.75"), 
                     limits = c(-0.8, 0.8))+
  ylab("Reward Ratio")+
  geom_hline(yintercept = 0, size = 1.5)+
  coord_flip() + 
  theme_minimal_vgrid()+
  theme(legend.position = "none", 
        axis.title.y=element_blank(),
        plot.background = element_rect(fill = "white")) 

ratioPlots <- plot_grid(riskRatio, rewardRatio)

ggsave("ratioPlots.png", 
       ratioPlots,
       height = 4, 
       width = 8,
       units = "in",
       dpi = 600,
       path = "../website/content/project/2021-11-16-great-british-bakeoff")


