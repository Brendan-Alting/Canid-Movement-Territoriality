#4.2 - plotting hypothesis 4
library(ggplot2)
library(nnet)
library(marginaleffects)

#Use plot-predictions function from marginal effects

fulleffects <- plot_predictions(fullnumltinom, type = "probs", condition = c("PupsTot", "Status", "group"), draw =FALSE)

#Renaming variables for plotting

fulleffects$group <- gsub("Out_Of_Range", "Extra-Territorial", fulleffects$group)    
fulleffects$group <- as.factor(fulleffects$group)
fulleffects$group <- factor(fulleffects$group, levels = c("Core", "Peripheral", "Extra-Territorial"))


Hypothesis3 <- ggplot(fulleffects,aes(x= PupsTot, y =estimate, fill = Status))+
  geom_line(aes(color = Status))+
  geom_ribbon(aes(ymin = conf.low,ymax = conf.high, fill = Status),alpha =0.3)+
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "Pups In Pack", y = "Predicted Proportional Detections") +
  theme_minimal() +
  facet_grid(. ~ group)+
  theme(panel.spacing = unit(0.2,"lines"),
        panel.border = element_rect(0.1),
        panel.grid = element_blank(),
        text=element_text(size = 40))

Hypothesis3

png("Figures/Proportions and Pups.png", width = 20, height = 13, res= 300, units = "in")

Hypothesis3

dev.off()
