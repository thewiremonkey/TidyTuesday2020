library(tidyverse)
library(tidytuesdayR)
library(ggridges)
library(showtext)



font_add("OpenDyslexic", "OpenDyslexic-Regular.otf")
showtext_auto()

tt<-tidytuesdayR::tt_load(2020, 31)

penguins<-tt$penguins %>%
  filter(!is.na(sex))

scaled_pins<-penguins%>%
  group_by(species) %>%
  summarise(`scaled mass` = scale(body_mass_g, center = TRUE),
            `scaled beak length` = scale(bill_length_mm, center = TRUE),
            `scaled beak depth` = scale(bill_depth_mm, center = TRUE),
            `scaled flipper length` = scale(flipper_length_mm, center = TRUE)) %>%
  bind_cols(penguins %>%
              select(sex)) %>%
  ungroup() %>%
  pivot_longer(cols = `scaled mass`:`scaled flipper length`, names_to = "stat", values_to = "value")


p1<-ggplot(scaled_pins, aes(y=stat, x=value))+
  # annotation_custom(rasterGrob(antarctic,
  #                              width = unit(1,"npc"),
  #                              height = unit(1,"npc")),
  #                   -Inf, Inf, -Inf, Inf) +
  geom_point(aes(color = sex))+
  ggridges::stat_density_ridges(quantile_lines = TRUE, quantiles = 2, aes(fill = sex), alpha = 0.3, scale = 1)+
  facet_wrap(species ~.)+
  labs(x="", y="",title = "Adelie penguins express the greatest sexual dimorphism",
       subtitle = "All measures scaled and centered",
       caption = "Viz: Alyssa Goldberg @WireMonkey\nData: Dr. Kristen Gorman by way of the palmerpenguins R package by Dr. Kristen Gorman, Dr. Allison Horst, and Dr. Alison Hill\nTidyTuesday 2020 week 31")+
  scale_fill_manual(values = c("female" = "#0088ff", "male" = "#003457"))+
  scale_color_manual(values = c("female" = "#0088ff", "male" = "#003457"))+
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        plot.background = element_blank(),
        plot.caption = element_text(size=8),
        text = element_text(family = "OpenDyslexic"),
        legend.position = "bottom")


ggsave("penguin_measures.png", p1,device = "png",units = "in",width = 10, height = 8, dpi = "retina")
