#3.1 Plotting hypothesis 3
library(ggplot2)
library(tidygam)

#Use predict gam function within ggplot to  to plot this, they work together. 

Hypothesis2 <-     predict_gam(dominantfull,
                                 exclude_terms = "s(Session)",
                                 series = "months_numeric",
                                 length_out = 100,
                                 tran_fun = exp) %>%
  mutate(Sex = recode(Sex, "F" = "Female", "M" = "Male"),
         Zone = recode(CamTerType, "OOR" = "Extra-Territorial", 
                       "COR" = "Core", 
                       "PER" = "Peripheral")) %>%
  plot(comparison = "Zone") +
  facet_grid(Sex~ Zone) +
  theme_minimal() +
  theme(panel.spacing = unit(1,"lines"),
        panel.border = element_rect(0.1),
        panel.grid = element_blank(),
        text=element_text(size = 40),
        
        axis.text.x = element_text(size =20),
        legend.position = "none") +
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6),
                     labels = c("Dec", "Jan", "Feb", "Mar", "Apr", "May")) +
  labs(x = "Month",
       y = "Detections",
       title = NULL)+
  scale_color_manual(values = c("grey", "grey", "grey"))+
  scale_fill_manual(values = c("grey", "grey", "grey"))+
  geom_vline(xintercept = 4, linetype = "dashed", color = "black", size = 1)

Hypothesis2


png("Figures/Dominants Lone Visits.png", width = 20, height = 13, res= 300, units = "in")

valuejustdoit
dev.off()