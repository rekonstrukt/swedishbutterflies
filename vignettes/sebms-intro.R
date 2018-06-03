## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)


## ---- message=FALSE------------------------------------------------------

library(swedishbutterflies)

suppressPackageStartupMessages(library(purrr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(knitr))

# first few rows of precipitation 2015 data
sebms_data_precip_2015 %>% 
head(5) %>% 
kable()

# same for temperature 2015 data
sebms_data_temp_2015 %>% 
head(5) %>% 
kable()

# species counts
sebms_data_specieslist_cum %>%
head(5) %>%
kable()

# species count histograms

sebms_data_species_histo %>%
head(5) %>%
kable()

sebms_data_species_per_site_sitetype %>%
head(5) %>%
kable()

# Naturum site locations
sebms_data_sites_naturum %>%
head(5) %>% 
kable()


## ------------------------------------------------------------------------

sebms_species_per_year() %>% 
head(5) %>% 
kable()

sebms_species_per_year_filtered() %>% 
head(5) %>% 
kable()

sebms_species_per_year_site_filtered() %>% 
head(5) %>% 
kable()

sebms_species_per_year_site_counts_filtered() %>%
head(5) %>% 
kable()
  

## ---- message=FALSE, fig.show='hold', fig.cap='Temperature and Precipitation for 2015', fig.width=7----

library(magick)

sebms_precip_temp_2015_plot() %>% 
  image_resize("700x")


## ------------------------------------------------------------------------
plots <- sebms_specieslist_cum_plots()

p1 <- plots$p1
p2 <- plots$p2

## ---- fig.width=7, fig.height=7------------------------------------------
p1

## ---- fig.width=7, fig.height=9------------------------------------------
p2

## ------------------------------------------------------------------------

p1 %>% sebms_ggsave(661, 812, filename = "/tmp/specieslist-01.png")
p2 %>% sebms_ggsave(666, 900, filename = "/tmp/specieslist-02.png")


## ---- fig.width=7--------------------------------------------------------
sebms_species_histo_plot()

## ---- fig.width=7--------------------------------------------------------
sebms_species_per_site_sitetype_plot()

## ------------------------------------------------------------------------

library(tibble)
library(DT)

sites <- sebms_data_sites_per_year_province

df <- 
  t(sites) %>% 
  as_tibble() %>% 
  setNames(sites$province)

datatable(df)


## ------------------------------------------------------------------------

gogogo <-
  sebms_naturum_climate() %>%
  filter(is_gogogo) %>%
  dplyr::select(id, is_gogogo, t, ws, tcc_mean, pmean, date)

gogogo %>% head(5) %>% kable


## ------------------------------------------------------------------------

# spatial data - a raster brick with sunhour layers
smhi_sunhours_rb

sun <- smhi_sunhours_rb$period_means

# plot spatial data
sebms_sunhours_plot(rl = sun) %>%
  sebms_ggsave(filename = "/tmp/plot-01.png")

image_read("/tmp/plot-01.png")


## ------------------------------------------------------------------------
sebms_sunhours_plot(show_legend = TRUE) %>%
  sebms_ggsave(filename = "/tmp/plot-02.png")

image_read("/tmp/plot-02.png")


## ---- fig.width=7--------------------------------------------------------

library(leaflet)

  sebms_sunhours_leaflet(rl = smhi_sunhours_rb$diff_latest, 
   options = leafletOptions(zoomControl = TRUE)) %>%
  addProviderTiles("Stamen.Watercolor",
    options = providerTileOptions(opacity = 0.25))


## ------------------------------------------------------------------------

library(sp)

occ_sp <- SpatialPoints(coords = sebms_data_sites, 
  proj4string = CRS("+proj=tmerc +lat_0=0 +lon_0=15.80827777777778 +k=1 +x_0=1500000 +y_0=0 +ellps=bessel +units=m +no_defs"))
  
#df <- sebms_data_sites %>% dplyr::filter(`coords.x1` > 1380000)

# plot distribution of sites
sebms_species_site_plot(occ_sp)$plot %>% image_resize("x500")


## ------------------------------------------------------------------------

# display a legend for the colors
sebms_species_site_plot(occ_sp)$legend %>%
  image_trim()


