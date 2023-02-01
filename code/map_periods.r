# plot for obdobi

library(tidyverse)
library(sf)

source(here::here("./code/data_read_aiscr_export.R"))

# data
amcr <- read_amcr()

# akce > dj > pian
akce_pian <- inner_join(amcr$akce, amcr$dokumentacni_jednotka, 
                        by = c("ident_cely" = "parent"), 
                        suffix = c("akce", ".dj"), multiple = "all") %>% 
  rename(dj = ident_cely.dj) %>% 
  inner_join(amcr$pian, 
             by = c("pian" = "ident_cely"))

# akce_dok.j_pian > komponenta
pian_komp <- akce_pian %>% 
  left_join(amcr$komponenta, 
            by = c("dj" = "parent"), 
            suffix = c(".akce", ".dj"), multiple = "all") %>% 
  rename(ident_cely = ident_cely.akce) %>% 
  select(-ident_cely.dj)

pian <- pian_komp %>% 
  select(pian, obdobi, obdobi_poradi, pristupnost, hlavni_typ, 
         negativni_jednotka, x = centroid_e, y = centroid_n)

rm(list = c("amcr", "pian_komp", "akce_pian"))
gc()

# background data
czrep <- RCzechia::republika(resolution = "low")
# kraje <- RCzechia::kraje(resolution = "low")
# reky <- RCzechia::reky() %>% 
#   filter(Major)
kfme0 <- RCzechia::KFME_grid(resolution = "high")

# table obdobi
# obdobi <- tribble(~poradi, ~obdobi,
#                   1, "Paleolit",
#                   2, "Mezolit",
#                   3, "Paleolit-Mezolit",
#                   4, "Neolit",
#                   5, "Eneolit",
#                   6, "Neolit-Eneolit",
#                   7, "Doba bronzová",
#                   8, "Doba halštatská",
#                   9, "Bronz-Halštat",
#                   10, "Doba laténská",
#                   11, "Halštat-Latén",
#                   12, "Doba římská",
#                   13, "Latén-Řím",
#                   14, "Doba stěhování národů",
#                   15, "DŘ-DSN",
#                   16, "Zemědělský pravěk",
#                   17, "Pravěk",
#                   18, "Raný středověk",
#                   19, "Vrcholný středověk",
#                   20, "Středověk",
#                   21, "Novověk",
#                   22, "Vrcholný středověk-Novověk",
#                   23, "Středověk-Novověk",
#                   24, "Industrial",
#                   25, "Novověk-Industrial")
obdobi <- tribble(~poradi, ~obdobi,
                  1, "Paleolithic",
                  2, "Mesolithic",
                  3, "Paleolithic-Mesolithic",
                  4, "Neolithic",
                  5, "Aeneolithic",
                  6, "Neolithic-Aeneolithic",
                  7, "Bronze Age",
                  8, "Halstatt period",
                  9, "Bronze-Halstatt",
                  10, "La Téne period",
                  11, "Halstatt-La Téne",
                  12, "Roman period",
                  13, "La Téne-Roman",
                  14, "Migration period",
                  15, "DŘ-DSN",
                  16, "Agric. prehistory",
                  17, "Prehistory",
                  18, "Early Medieval",
                  19, "High Medieval",
                  20, "Medieval",
                  21, "Early Modern period",
                  22, "High Medieval-Modern Era",
                  23, "Medieval-Modern Era",
                  24, "Industrial period",
                  25, "Modern Era-Industrial")

obdobi_vect <- obdobi$obdobi
names(obdobi_vect) <- obdobi$poradi

# data prep ---------------------------------------------------------------

pian_cl <- pian %>% mutate(poradi = as.character(obdobi_poradi),
                           poradi = str_remove(poradi, "..$"), 
                           poradi = as.integer(poradi),
                           obdobi_sim = unname(obdobi_vect[poradi]),
                           obdobi_sim = fct_reorder(obdobi_sim, poradi, .na_rm = TRUE)) %>% 
  filter(!is.na(obdobi), 
         obdobi != "nedat")

# sf ----------------------------------------------------------------------

pian_sf <- pian_cl %>% 
  filter(!str_detect(obdobi_sim, "-"),
         !str_detect(obdobi_sim, "(P|p)rehistory"),
         !str_detect(obdobi_sim, "^Medieval$")) %>% 
  mutate(obdobi_sim = if_else(obdobi_sim %in% c("Roman period", 
                                                "Migration period"),
                              "Roman and Migration periods", 
                              as.character(obdobi_sim)),
         obdobi_sim = fct_reorder(obdobi_sim, poradi)) %>% 
  st_as_sf(coords = c("x", "y")) %>% 
  st_set_crs(4326)
  

# plotting ----------------------------------------------------------------

# # edit Paired palette (RColorBrewer)
# clrs <- RColorBrewer::brewer.pal(n = 12, name = "Paired")
# clrs[11] <- "#C1891A"

# # plot
# pian_sf %>% 
#   ggplot() +
#   geom_sf(data = czrep, color = "gray88", fill = NA) +
#   geom_sf(data = reky, color = "gray88") +
#   geom_sf(aes(color = obdobi_sim), size = 0.8, alpha = 0.4, show.legend = FALSE) +
#   scale_color_manual(values = clrs) +
#   facet_wrap(vars(obdobi_sim)) +
#   theme_void()

# ggsave(here::here("img", "datace_map_clrs.png"), width = 12, height = 6)

# generalize onto a grid
pian_sf$obdobi_sim |> table()


kfme_periods <- function(x, lvls) {
    get_kfme <- function(x, lvl) {
        x <- kfme0 %>%
            mutate(
                obd = lengths(
                    st_intersects(
                        kfme0, filter(x, obdobi_sim == lvl)))) %>%
            filter(obd > 0)
        x
    }

    res <- vector("list", length(lvls)) %>%
        setNames(lvls)

    for(i in 1:length(lvls)) {
        res[[i]] <- get_kfme(x, lvl = lvls[i])
    }

    res

}

kfme1 <- kfme_periods(
  x = pian_sf, lvls = as.character(unique(pian_sf$obdobi_sim))
  )

kfme1 %>% 
    bind_rows(.id = "period") %>%
    mutate(
      period = factor(period, levels =  levels(pian_sf$obdobi_sim), ordered = TRUE),
      obd = log10(obd)
      ) %>%
    ggplot() +
    geom_sf(data = st_buffer(czrep, 1e3), fill = "white") +
    geom_sf(aes(fill = obd)) +
    scale_fill_viridis_c(option = "D", direction = -1) +
    facet_wrap(vars(period)) +
    theme_void() +
    labs(fill = "log10")

ggsave(here::here("img", "datace_map_grid.png"), width = 12, height = 6)
