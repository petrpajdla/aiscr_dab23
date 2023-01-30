# Maps for positive vs negative findings (features)

library(tidyverse)
library(sf)
source("./code/data_read_aiscr_export.R")

# data
rep <- RCzechia::republika()
kfme0 <- RCzechia::KFME_grid(resolution = "high")

amcr <- read_amcr()

# prep
dj <- amcr$dokumentacni_jednotka %>% 
    inner_join(amcr$pian, by = c("pian" = "ident_cely")) %>%
    st_as_sf(wkt = "geom_wkt", crs = 5514) %>%
    st_centroid() %>%
    st_transform(4326)

# grid
kfme <- kfme0 %>%
    mutate(
        positive = lengths(
            st_intersects(
                kfme0, filter(
                    dj, negativni_jednotka == 0
                )
            )
        ),
        negative = lengths(
            st_intersects(
                kfme0, filter(
                    dj, negativni_jednotka == 1
                )
            )
        )
    ) %>%
    pivot_longer(cols = c(positive, negative)) %>%
    mutate(
        log10 = log10(value)
    ) %>%
    filter(value > 0)

# plots
ggplot() +
    geom_sf(data = st_buffer(rep, 1e3), fill = "white") +
    geom_sf(data = kfme, aes(fill = log10)) +
    scale_fill_viridis_c(option = "D", direction = -1) +
    facet_wrap(vars(name), ncol = 2) +
    theme_void()

# ggplot() +
# geom_sf(data = rep, colour = "white", linewidth = 0.8, fill = "gray80") +
#   geom_sf(data = dj, alpha = 0.04, shape = 20,
#     aes(colour = negativni_jednotka), 
#     show.legend = FALSE) +
#   scale_colour_manual(values = c("#E7302A", "#247E4B")) +
#   facet_wrap(vars(negativni_jednotka), ncol = 1) +
#   theme_void()

ggsave(here::here("img/posneg.png"), width = 10, height = 3)
