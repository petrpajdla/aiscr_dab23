# Areas with no active oao - freaction of not registered.

library(tidyverse)
library(sf)
source(here::here("code/data_read_aiscr_export.R"))

# paths
p_ku <- "T:/aiscr/data/ku_2022-04-20.geojson"
# p_su <- "T:/aiscr/data/su_2022-04-12.geojson"
# p_oao <- "T:/aiscr/data/oao_poly_2022-04-11.geojson"

# data
kfme <- RCzechia::KFME_grid(resolution = "high")

rep <- RCzechia::republika() %>%
  st_buffer(1e3)
# kraje <- RCzechia::kraje()
# okresy <- RCzechia::okresy() %>% 
#   st_filter(kraje, .predicate = st_within)

ku <- st_read(p_ku) %>%
  select(ku = NAZEV_KU,
         id_okr = KOD_LAU1) %>% 
  mutate(ku = str_to_upper(ku)) %>%
  st_centroid() %>%
  st_transform(4326)

input <- read_amcr()

# stavy
input$proj %>% 
  select(stav, stav_popis) %>% 
  distinct() %>% 
  arrange(stav)


# analysis ----------------------------------------------------------------

proj <- input$projekt %>% 
  # mutate(katastr = if_else(katastr == "JÍVOVÁ I", "JÍVOVÁ", katastr)) %>% 
  filter(id_rok > 2016, id_rok < 2022, typ_projektu == "záchranný")

# TODO: some katastr missing/old name
# proj$katastr %in% ku$ku %>% table()
# proj$katastr[!proj$katastr %in% ku$ku]

# summarise projects in katastry
proj_vse <- proj %>% 
  filter(stav %in% c(1:6)) %>% 
  group_by(katastr) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))
 
# proj_zapsany <- proj %>% 
#   filter(stav == 1) %>% 
#   group_by(katastr) %>% 
#   summarise(n_zapsany = n()) %>% 
#   arrange(desc(n_zapsany))

proj_bezoao <- proj %>% 
  filter(is.na(organizace_prihlaseni), stav == 1) %>% 
  group_by(katastr) %>% 
  summarise(n_bezoao = n()) %>% 
  arrange(desc(n_bezoao))

proj_sum <- proj_vse %>% 
  # full_join(proj_zapsany) %>% 
  full_join(proj_bezoao) %>% 
  left_join(ku, by = c("katastr" = "ku")) %>% 
  mutate(
    # n_zapsany = if_else(is.na(n_zapsany), 0L, n_zapsany),
    n_bezoao = if_else(is.na(n_bezoao), 0L, n_bezoao),
        #  p1 = round(n_zapsany / n * 100),
        #  p2 = round(n_bezoao / n * 100)
  ) %>% 
  # arrange(desc(p2)) %>% 
  st_as_sf()

proj_sum_grid <- st_join(kfme, proj_sum) %>%
  filter(!is.na(n)) %>% 
  group_by(ctverec) %>%
  summarise(
    n = sum(n, na.rm = TRUE),
    # n_zapsany = sum(n_zapsany, na.rm = TRUE),
    n_bezoao = sum(n_bezoao, na.rm = TRUE),
    # p1 = round(n_zapsany / n * 100),
    p2 = round(n_bezoao / n * 100)
  )

# kfme[which(lengths(st_intersects(kfme, proj_sum)) > 0), ] %>%
#   ggplot() +
#   geom_sf()

# plot
ggplot() +
  geom_sf(data = rep, fill = "white") +
  geom_sf(data = proj_sum_grid, aes(fill = p2)) +
  scale_fill_viridis_c(option = "D", direction = -1) +
  # geom_sf(data = okresy, color = "gray90", size = 0.8, fill = NA) +
  # geom_sf(data = kraje, color = "gray80", size = 1.2, fill = NA) +
  # ggrepel::geom_text_repel(
  #   data = filter(proj_sum, p > 75 & n > 10),
  #   aes(label = katastr, geometry = geometry),
  #   stat = "sf_coordinates",
  #   bg.color = "white") +
  theme_void() +
  labs(fill = "%")

ggsave(here::here("img", "projnooao.png"), width = 6, height = 3)
