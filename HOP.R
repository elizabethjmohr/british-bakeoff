library(tidyverse)
library(gganimate)
library(magick)

risk_anim <- ggplot(bootstraps %>% mutate(centeredRiskRatio = riskRatio -1), aes(x = flavor, y = centeredRiskRatio, fill = flavor)) +
  geom_col()+
  scale_fill_manual(values = colors) +
  scale_y_continuous(breaks = c(-0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75), 
                     labels = c("0.25","0.5","0.75","1","1.25","1.5", "1.75"), 
                     limits = c(-0.8, 0.8))+
  ylab("Risk Ratio")+
  geom_hline(yintercept = 0, size = 1.5)+
  coord_flip() + 
  theme_minimal_vgrid()+
  theme(legend.position = "none", axis.title.y=element_blank()) +
  transition_states(
    states = sampleIndex,
    transition_length = 2,
    state_length = 1
  ) + 
  enter_fade() + 
  exit_shrink() + 
  ease_aes('sine-in-out')
risk_gif <- animate(risk_anim, 
                    height = 4, 
                    width = 4,
                    units = "in", 
                    res = 150,
                    fps = 5)
anim_save("risk.gif", risk_gif)

reward_anim <- ggplot(bootstraps %>% 
                        mutate(centeredRatio = rewardRatio -1), 
                      aes(x = flavor, 
                          y = centeredRatio, 
                          fill = flavor)) +
  geom_col()+
  scale_fill_manual(values = colors) +
  scale_y_continuous(breaks = c(-0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75), 
                     labels = c("0.25","0.5","0.75","1","1.25","1.5", "1.75"), 
                     limits = c(-0.8, 0.8))+
  ylab("Reward Ratio")+
  geom_hline(yintercept = 0, size = 1.5)+
  coord_flip() + 
  theme_minimal_vgrid()+
  theme(legend.position = "none", axis.title.y=element_blank()) +
  transition_states(
    states = sampleIndex,
    transition_length = 2,
    state_length = 1
  ) + 
  enter_fade() + 
  exit_shrink() + 
  ease_aes('sine-in-out')
reward_gif <- animate(reward_anim, 
                      height = 4, 
                      width = 4,
                      units = "in",
                      res = 150,
                      fps = 5)
anim_save("reward.gif", reward_gif)

risk_mgif <- image_read("risk.gif")
reward_mgif <- image_read("reward.gif")

new_gif <- image_append(c(risk_mgif [1], reward_mgif[1]))
for(i in 2:100){
  combined <- image_append(c(risk_mgif[i], reward_mgif[i]))
  new_gif <- c(new_gif, combined)
}
anim_save("new_gif.gif", 
          new_gif,
          path = "../website/content/project/2021-11-16-great-british-bakeoff")

# Static version
riskHOP <- ggplot(ratios %>% mutate(centeredRiskRatio = riskRatio -1), 
                  aes(x = flavor, y = centeredRiskRatio, fill = flavor)) +
  geom_col(alpha = 0.7)+
  geom_jitter(data = moreBootstraps %>%  mutate(centeredRiskRatio = riskRatio -1), 
             aes(x = flavor, y = centeredRiskRatio, color = flavor),
             size = 0.7)+
  scale_fill_manual(values = colors) +
  scale_color_manual(values = colors)+
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

rewardHOP <- ggplot(ratios %>% mutate(centeredRewardRatio = rewardRatio -1), aes(x = flavor, y = centeredRewardRatio, fill = flavor)) +
  geom_col(alpha = 0.7)+
  geom_jitter(data = moreBootstraps %>%  mutate(centeredRiskRatio = rewardRatio -1), 
              aes(x = flavor, y = centeredRiskRatio, color = flavor),
              size = 0.7)+
  scale_fill_manual(values = colors) +
  scale_color_manual(values = colors)+
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

staticHOPs <- plot_grid(riskHOP, rewardHOP)

ggsave("staticHOP.png", 
       staticHOPs,
       height = 4, 
       width = 8,
       units = "in",
       dpi = 600,
       path = "../website/content/project/2021-11-16-great-british-bakeoff")
