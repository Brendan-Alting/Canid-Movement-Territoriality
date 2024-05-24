##2.1 plotting results from hypothesis 2. 
library(ggplot2)
library(tidygam)

#Can plot and predict at the same time. 
plotfullgam <- predict_gam(VaryIndiv,
exclude_terms = "s(Session)",
series = "months_numeric",
length_out = 100, 
tran_fun = exp) %>%
  mutate(Sex = recode(Sex, "F" = "Female", "M" = "Male"),
         Zone = recode(Zone, "OOR" = "Extra-Territorial", 
                       "Core" = "Core", 
                       "Peripheral" = "Peripheral")) %>%
  plot(comparison = "Status") +
  facet_grid(Sex ~ Zone) +
  scale_color_brewer(type = "qual") +
  scale_fill_brewer(type = "qual") +
  theme_minimal() +
  theme(panel.spacing = unit(1,"lines"),
        panel.border = element_rect(0.1),
        panel.grid = element_blank(),
        text=element_text(size = 40),
        axis.text.x = element_text(size =20)) +
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6),
                     labels = c("Dec", "Jan", "Feb", "Mar", "Apr", "May")) +
  labs(x = "Month",
       y = "Detections",
       title = "")+
  geom_vline(xintercept = 4, linetype = "dashed", color = "black", size = 1)

plotfullgam


png("Figures/Dingo Territorial Detections.png", width = 20, height = 13, res= 300, units = "in")

plotfullgam

dev.off()