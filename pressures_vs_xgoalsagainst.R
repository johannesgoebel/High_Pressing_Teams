rm(list = ls())
sink("05_set.txt", append=FALSE, split=TRUE)
setwd("/home/johannes/Dokumente/projekte/fussball")

defensive <- read.csv2('defensive_stats_big5.csv', sep = ',', skip = 1)
stats <- read.csv2('standard_big_5.csv', sep = ',', skip = 1)
stats_against <- read.csv2('stats_against_big5.csv', sep = ',', skip = 1)

library(tidyverse)
pressures_final_3rd <- defensive %>% select(1,2,3,15,20)
x_goals_against <- stats_against %>% select(1,7,23)
total <- left_join(pressures_final_3rd, x_goals_against, by="Rk")
total$percentage_pressures_final_3rd <- (total$Att.3rd.1 /total$Press)*100
total$xG_against_per_match <- (as.integer(total$xG) /total$MP)

library(ggthemes)

theme_set(theme_stata())
plot1 <- ggplot(total, aes(x=percentage_pressures_final_3rd, y= xG_against_per_match)) +
  scale_y_reverse() +
  ggtitle("Pressures in the Final Third vs. XGoals Against in the Top5 Leagues")+
  theme(plot.title = element_text(size=22, hjust = 0.5)) +
  xlab("% of Pressures within the Final 3rd by Team")+
  ylab("xGoals Against per Game") + 
  geom_point(aes(colour= factor(Comp)), size=3) +
  scale_colour_discrete(name="League", labels = c("Bundesliga", "Premier League", "La Liga", "Ligue 1", "Serie A")) +
  geom_text(aes(label=Squad), size=3, check_overlap = TRUE, nudge_x = 1) 
plot1
comp_press <-total %>% select(3,4)
head(comp_press )
comp_press <- comp_press %>% group_by(Comp) %>% summarise(Press = sum(Press))

comp_press_final <- comp_press_final <- total  %>% select(3,5)
head(comp_press_final)
comp_press_final <- comp_press_final %>% group_by(Comp) %>% summarise(Att.3rd.1 = sum(Att.3rd.1))
comp_total <- left_join(comp_press, comp_press_final, by="Comp")
comp_total$percentage <- (comp_total$Att.3rd.1 / comp_total$Press) * 100

plot2 <- ggplot(comp_total, aes(x= Comp, y = percentage)) +
  geom_bar(stat="identity", aes(fill = factor(comp_total$Comp))) +
  geom_text(aes(label=round(percentage, digits =2)), vjust=1.6, color="white", size=5)+
  xlab("% Pressures within the Final 3rd by League") +
  ylab("")+
  theme(legend.position = "none", axis.text.x = element_blank()) 
plot2

library(patchwork)

p <-plot1 + inset_element(plot2, left=0.025, right = 0.325, bottom=0.675, top = 0.975)
p
ggsave('pressures_vs_xgoalsagainst.png', width=16, height = 9, dpi = 300)
