# Map katastry without any projects.

library(tidyverse)
library(sf)
source(here::here("code/data_read_aiscr_export.R"))


# data --------------------------------------------------------------------

# amcr projekty
proj <- read_amcr("projekt")[["projekt"]] %>% 
  filter(id_rok %in% 2016:2022) %>% 
  group_by(katastr) %>% 
  count() %>% 
  mutate(katastr = str_to_lower(katastr))

# spatial data
kraje <- RCzechia::kraje() %>%
  st_simplify(dTolerance = 5e2, preserveTopology = TRUE) %>%
  st_transform(4326)

p_ku <- "T:/aiscr/data/ku_2022-04-20.geojson"
ku <- st_read(p_ku) %>% 
  mutate(katastr = str_to_lower(NAZEV_KU))


# analysis ----------------------------------------------------------------

ku_proj <- ku %>% 
  left_join(proj, by = c("katastr")) %>% 
  mutate(proj = !is.na(n))

# plots 
ku_proj %>% 
  filter(proj == FALSE) %>% 
  st_transform(4326) %>%
  ggplot() +
  geom_sf(data = kraje, fill = NA) +
  geom_sf(aes(fill = proj), show.legend = FALSE) +
  scale_fill_manual(values = c("black", "white")) +
  # theme(plot.background = element_rect(color = "red"))
  theme_void()

ggsave(here::here("img/noproj.png"), width = 6, height = 3)

# ku_proj %>% 
#   filter(proj == FALSE) %>% 
#   ggplot() +
#   geom_sf(data = kraje, color = "gray35", fill = "white", size = 1.6) +
#   geom_sf(data = ku, fill = NA) +
#   geom_sf(fill = "black", show.legend = FALSE) +
#   theme_void() +
#   theme(panel.background = element_rect(color = "white", fill = "white"))


# ku_proj %>% 
#   filter(proj == FALSE) %>% 
#   ggplot() +
#   geom_sf(data = kraje, color = "white", fill = "gray80", size = 1.2) +
#   geom_sf(fill = "black", show.legend = FALSE) +
#   theme_void() +
#   theme(panel.background = element_rect(color = "white", fill = "white"))

# ggsave(here::here("plots/map_ku_bez_projektu_v2.png"), scale = 5)



