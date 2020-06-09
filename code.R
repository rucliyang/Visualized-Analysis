library(tidyverse)
library(ggplot2)
library(MASS)
library(arules)
require(gridExtra)
library(gapminder)
n=50
p=10
X=as.data.frame(mvrnorm(n, rep(0,p), (-0.5)^abs(outer(1:p,1:p,"-"))))
ggplot(data = X, aes(x=V1, y=V2)) + geom_point()
ggplot(data = X, aes(x=V1, y=V2, size=V3)) + geom_point()
ggplot(data = X, aes(x=V1, y=V2, size=V3, color=V4)) + geom_point()
ggplot(data = X, aes(x=V1, y=V2, size=V3, color=V4, shape=discretize(V5,breaks=6))) + geom_point()
#ggplot(data = X, aes(x=V1, y=value)) + 
#  geom_point(aes(y = V2, size=V3, color=V4, shape=discretize(V5,breaks=6))) +
#  geom_point(aes(y = V6, size=V7, color=V8, shape=discretize(V9,breaks=6)))

load("hawaii_income.rda")
p_income_base <- ggplot(hawaii_income[hawaii_income$year == 2015,], 
              aes(x=county,y=median_income))+
              geom_col(fill = "#56B4E9") +
              xlab("county") +
  #theme_dviz_hgrid() +
  theme(
    axis.ticks.x = element_blank(),
    plot.margin = margin(3, 7, 3, 1.5)
  )
p_income_bad <- p_income_base + 
  coord_cartesian(xlim = c(0.5, 5.55), ylim = c(50000, 75000), expand = FALSE) +
  scale_y_continuous(
    name = "median income (USD)", 
    breaks = 10000*(5:7),
    labels = function(x) paste0("$", scales::comma(x))
  )
p_income_bad
p_income_base

df_oceania <- filter(gapminder_unfiltered, year == 2007, continent == "Oceania") %>%
  mutate(GDP = pop*gdpPercap) %>%
  arrange(desc(GDP))
oc_bad <- ggplot(df_oceania, aes(x = reorder(country, -GDP), y = log10(GDP))) + 
  geom_col(fill = "#56B4E9") + 
  scale_y_continuous(breaks = log10(c(3.1e8, 1e9, 3.e9, 1e10, 3.e10, 1e11, 3.e11, 1e12)),
                     labels = c("0.3", "1.0", "3.0", "10", "30", "100", "300", "1000"),
                     name = "GDP (billion USD)") +
  scale_x_discrete(name = NULL) +
  coord_flip(ylim = log10(c(3.1e8, 9.9e11)), expand = FALSE) +
  #theme_dviz_vgrid(12, rel_small = 1) +
  theme(axis.ticks.y = element_blank(),
        plot.margin = margin(12, 6, 3, 1.5))
oc_bad

oc_bad2 <- ggplot(df_oceania, aes(x = reorder(country, -GDP), y = log10(GDP))) + 
  geom_col(fill = "#56B4E9") + 
  scale_y_continuous(breaks = 2*(0:6),
                     labels = function(x) log10(10^x/1e9),
                     name = "GDP (billion USD)") +
  scale_x_discrete(name = NULL) +
  coord_flip(ylim = log10(c(1, 9.9e11)), expand = FALSE) +
  #theme_dviz_vgrid(12, rel_small = 1) +
  theme(axis.ticks.y = element_blank(),
        plot.margin = margin(12, 6, 3, 1.5))
oc_bad2

ggplot(df_oceania, aes(x = reorder(country, -GDP), y = log10(GDP))) + 
  geom_point(size = 3.5, color = "#0072B2") + 
  geom_label(aes(label = country, y = log10(GDP) - .08), hjust = 1, size = 12/.pt,
             #family = dviz_font_family,
             fill = "white", alpha = 0.5, label.padding = grid::unit(2, "pt"),
             label.r = grid::unit(0, "pt"), label.size = 0) +
  scale_y_continuous(breaks = log10(c(3e8, 1e9, 3.e9, 1e10, 3.e10, 1e11, 3.e11, 1e12)),
                     labels = c("0.3", "1.0", "3.0", "10", "30", "100", "300", "1000"),
                     name = "              GDP (billion USD)",
                     limits = log10(c(3e7, 9.9e11)),
                     expand = c(0, 0)) +
  scale_x_discrete(name = NULL, breaks = NULL) +
  coord_flip() +
  #theme_dviz_vgrid(12, rel_small = 1) +
  theme(plot.margin = margin(12, 6, 3, 1.5))


library(ggrepel)
library(readr)
library(lubridate)
load("Aus_athletes.rda")
male_Aus <- filter(Aus_athletes, sex=="m") %>%
    filter(sport %in% c("basketball", "field", "swimming", "track (400m)",
                        "track (sprint)", "water polo")) %>%
    mutate(sport = case_when(sport == "track (400m)" ~ "track",
                             sport == "track (sprint)" ~ "track",
                             TRUE ~ sport))
male_Aus$sport <- factor(male_Aus$sport,
                         levels = c("field", "water polo", "basketball", "swimming", "track"))
p_Aus_base <- ggplot(male_Aus, aes(x=height, y=pcBfat, color=sport, fill=sport, shape=sport)) +
  geom_point(size = 2.5) +
  scale_shape_manual(values = 21:25) +
  #scale_color_OkabeIto(order=c(2, 1, 3, 4, 5), darken = 0.3) +
  #scale_fill_OkabeIto(order=c(2, 1, 3, 4, 5), darken = 0.1, alpha = 0.7) +
  scale_x_continuous(limits = c(169, 210), name = "height (cm)") +
  scale_y_continuous(limits = c(5, 20), name = "% body fat")
p <- p_Aus_base +
  #theme_dviz_open() +
  theme(
    axis.line = element_blank(),
    panel.grid.major = element_line(color = "black", size = 0.3),
    panel.grid.minor = element_line(color = "black", size = 0.15),
    panel.border = element_rect(color = "black", size = 1),
    legend.background = element_rect(color = "black", size = 0.5),
    legend.margin = margin(7, 7, 7, 7),
    plot.background = element_rect(color = "black", size = 1),
    plot.margin = margin(7, 7, 7, 7)
  )
p_Aus_base
p
