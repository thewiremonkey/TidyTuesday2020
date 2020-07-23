library(tidyverse)
library(tidytuesdayR)
library(ggridges)
library(ochRe)
library(extrafont)
library(showtext)
library(ggtext)
library(cowplot)

# extrafont:::font_import(paths = '/Users/alyssa/Library/Fonts')

font_add("OpenDyslexic", "OpenDyslexic-Regular.otf")
showtext_auto()

title_text <- ggdraw() +
  draw_label(fontfamily = "OpenDyslexic",
    str_wrap(
      "The Townsville animal complaints showed the greatest variance within the 'Noise' complaint type. Examination of complaints by the top 9 suburbs (by variance of complaint frequency) reveals some seasonality, with the winter months correlated with more noise complaints",
      width = 100
    ) ,
    x = 0.05,
    hjust = 0,
    size = 12
  )

footer_text <- ggdraw() +
  draw_label(
    "Viz: @WireMonkey Alyssa Goldberg ~ Data: Townsville Animal Complaints ~ TidyTuesday 2020 week 30/
    Font: OpenDyslexic",
    size = 8
  )

# #Australian palettes
# devtools::install_github("ropenscilabs/ochRe")
tt_theme <- function() {
  theme_bw(base_size = 10,
           base_family = "OpenDyslexic") %+replace%
    theme(
      panel.background = element_blank(),
      strip.background = element_blank(),
      strip.text = element_textbox(
        size = 10,
        color = "darkgrey",
        fill = NA,
        box.color = "#4A618C",
        halign = 0.5,
        linetype = 1,
        r = unit(5, "pt"),
        width = unit(1, "npc"),
        padding = margin(2, 0, 1, 0),
        margin = margin(3, 3, 3, 3)
      ),
      panel.border = element_blank()
    )
}

tt <- tidytuesdayR::tt_load(2020, 30)

ac <- tt$animal_complaints %>%
  janitor::clean_names() %>%
  separate(date_received, into = c("month", "year")) %>%
  mutate(# month = map(str_split(date_received, " "),`[`,1) %>% unlist(),
    #      year = map(str_split(date_received, " ") , tail, 1) %>% unlist(),
    month = factor(
      month,
      levels = c(
        "January",
        "February",
        "March" ,
        "April",
        "May",
        "June",
        "July",
        "August",
        "September",
        "October",
        "November",
        "December"
      )
    ))

bp <- ac %>% group_by(complaint_type, month) %>%
  summarise(cnt = n()) %>%
  ungroup() %>%
  # group_by(complaint_type) %>%
  # summarise(variance = var(cnt, na.rm = TRUE)) %>%
  # arrange(desc(variance)) %>%
  ggplot(., aes(x = fct_reorder(complaint_type, .fun = var, cnt),
                y = cnt)) +
  geom_boxplot(aes(fill = complaint_type), alpha = 0.5)+
  geom_jitter(inherit.aes = TRUE,aes(color=complaint_type))+
  # geom_col(aes(fill = complaint_type), alpha = 0.8, show.legend = FALSE) +
  # geom_text(aes(label = complaint_type, x=complaint_type, y = variance), hjust = 0)+
  scale_fill_ochre(palette = "healthy_reef")+
  scale_color_ochre(palette = "healthy_reef",reverse = TRUE)+
  coord_flip() +
  labs(y="Number of complaints/month")+
  tt_theme() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        legend.position = c(1,0),
        legend.justification = c(1,0),
        legend.title = element_blank(),
        # legend.direction = "horizontal",
        axis.ticks = element_blank()
        )



a <- ac %>%
  filter(animal_type == "dog") %>%
  # group_by_all()+
  group_by(year, month, complaint_type) %>%
  count() %>%
  mutate(n = as.numeric(n)) %>%
  ggplot(., aes(x = n, y = fct_rev(month), fill = complaint_type)) +
  geom_density_ridges(alpha = 0.5, show.legend = FALSE) +
  scale_fill_ochre(palette = "healthy_reef") +
  labs("Is there any seasonality for complaint types?") +
  facet_wrap(complaint_type ~ .) +
  tt_theme()

ac_filter <- ac %>%
  filter(suburb != "Unallocated") %>%
  # filter(animal_type == "dog",
  #                        complaint_type %in% c("Noise"),
  #                        suburb != "Unallocated") %>%
  group_by(suburb, month) %>%
  summarise(cnt = n()) %>%
  ungroup() %>%
  group_by(suburb) %>%
  summarise(variance = var(cnt, na.rm = TRUE)) %>%
  arrange(desc(variance)) %>%
  top_n(9, variance) %>%
  mutate(suburb = fct_inorder(suburb))


b <- ac %>%
  filter(suburb %in% ac_filter$suburb,
         animal_type == "dog",
         suburb != "Unallocated") %>%
         # complaint_type %in% c("Noise", "Aggressive Animal")) %>%
         group_by(year, month, complaint_type, suburb) %>%
           count() %>%
           ungroup() %>%
           mutate(n = as.numeric(n),
                  suburb = factor(suburb, levels = levels(ac_filter$suburb))) %>%
           ggplot(., aes(x = n, y = fct_rev(month), fill = complaint_type)) +
           geom_density_ridges(alpha = 0.5, show.legend = FALSE) +
  gghighlight(complaint_type == "Noise",
              keep_scales = TRUE)+
           facet_wrap(suburb ~ .) +
           scale_fill_ochre(palette = "healthy_reef") +
           labs(x = "", y = "", fill = "") +
           tt_theme()+
  theme(legend.position = "none")


         #assist for setting panel background
         # https://stackoverflow.com/questions/9847559/conditionally-change-panel-background-with-facet-grid
         c <- ac %>%
           filter(suburb %in% ac_filter$suburb,
                  animal_type == "dog" ,
                  complaint_type == "Noise") %>%
           group_by(year, month, suburb, complaint_type) %>%
           count() %>%
           ungroup() %>%
           mutate(n = as.numeric(n),
                  suburb = factor(suburb, levels = ac_filter$suburb)) %>%
           ggplot(., aes(x = n, y = fct_rev(month))) +
           # geom_rect(aes(fill = suburb),xmin = -Inf,xmax = Inf,
           #           ymin = -Inf,ymax = Inf,alpha = 0.1, show.legend = FALSE) +
           # scale_fill_ochre(palette = "namatjira_qual")+
           geom_density_ridges() +
           facet_wrap(suburb ~ ., scales = "free_x") +
           labs(x = "",
                y = "",
                title = "More noise complaints are made in Winter") +
           tt_theme()

         d <- ac %>%
           filter(suburb %in% ac_filter$suburb,
                  animal_type == "dog" ,
                  complaint_type == "Noise") %>%
           group_by(year, month, suburb, complaint_type) %>%
           count() %>%
           ungroup() %>%
           mutate(n = as.numeric(n),
                  suburb = factor(suburb, levels = ac_filter$suburb)) %>%
           ggplot(., aes(x = n, y = fct_rev(suburb))) +
           # geom_rect(aes(fill = suburb),xmin = -Inf,xmax = Inf,
           #           ymin = -Inf,ymax = Inf,alpha = 0.1, show.legend = FALSE) +
           # scale_fill_ochre(palette = "namatjira_qual")+
           geom_density_ridges() +
           facet_wrap(month ~ ., nrow = 4) +
           labs(x = "",
                y = "") +
           tt_theme()

grid1 = plot_grid(bp, b, rel_widths = c(.6, 1))
grid2 = plot_grid(title_text,
                  grid1,
                  footer_text,
                  ncol = 1,
                  rel_heights = c(.25,1,.1))

grid3 = plot_grid(grid2, c, ncol = 2)
