library(dplyr)
library(ggplot2)
library(sf)

kr <- RCzechia::kraje()

kr %>% 
  st_simplify(dTolerance = 9e2, preserveTopology = TRUE) %>% 
  mutate(
    ustav = if_else(
      NAZ_CZNUTS3 %in% c(
        "Kraj Vysočina", 
        "Jihomoravský kraj", 
        "Olomoucký kraj", 
        "Zlínský kraj", 
        "Moravskoslezský kraj"), "Brno", "Praha")
    ) %>% 
  ggplot() +
  geom_sf(aes(fill = ustav), color = "white", linewidth = 1.4, show.legend = FALSE) +
  scale_fill_manual(values = c("#96d4f0", "#805e38")) +
  theme_void()

ggsave(here::here("img/map.png"), scale = 2)
