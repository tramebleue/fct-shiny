library(plumber)
library(dplyr)
library(tidyr)
library(tidync)
library(sf)
# library(geojsonsf)

#* @apiTitle('Fluvial Corridor Toolbox Data API')

workdir = '/media/crousson/Backup/PRODUCTION/GrandsAxes/Isere/AXES/AX0002'

#* @param message
#* @get /echo
function(message) {
  list(message = paste0("The message is: '", message, "'"))
}

#* @get /swaths
function() {
  
  setwd(workdir)
  
  swaths = tidync('TEMP/VALLEY_SWATH_BOUNDS.nc') %>%
    hyper_tibble() %>%
    pivot_wider(
      id_cols = label,
      names_from = coord,
      values_from = bounds) %>%
    mutate(
      x = .5 * (minx + maxx),
      y = .5* (miny + maxy)) %>%
    st_as_sf(coords = c('x', 'y'), crs = st_crs(2154))
    # sf_geojson()
  
}

#* @get /swath/<id:int>/elevation
function(id) {
  
  setwd(workdir)
  
  swath = tidync('METRICS/ELEVATION_SWATH_PROFILES.nc', what='D0') %>%
    hyper_tibble() %>%
    filter(swath == id)
  
  profiles = tidync('METRICS/ELEVATION_SWATH_PROFILES.nc', what='D1') %>%
    hyper_tibble() %>%
    filter(sw_measure == swath$measure)
  
  tidync('METRICS/ELEVATION_SWATH_PROFILES.nc', what='D2,D1') %>%
    hyper_tibble(profile = profile %in% profiles$profile) %>%
    left_join(profiles, by = 'profile') %>%
    pivot_wider(
      id_cols = c(profile, sw_measure, sw_axis_distance),
      names_from = quantile,
      names_prefix = 'sw_elevation_abs_',
      values_from = sw_elevation_abs)%>%
    arrange(sw_axis_distance)
  
}

#* @param xmin
#* @param xmax
#* @get /lp/planform
function(xmin, xmax) {
  
  setwd(workdir)
  
  tidync('METRICS/PLANFORM.nc') %>% hyper_tibble()
  
}

#* @param xmin
#* @param xmax
#* @get /lp/talweg/height
function(xmin=-Inf, xmax=Inf) {
  
  setwd(workdir)
  
  # if (missing(xmin) | missing(xmax)) {
    
  # heights = tidync('METRICS/TALWEG_RELATED.nc') %>%
  #   hyper_tibble()
    
  heights = tidync('METRICS/TALWEG_RELATED.nc') %>%
    hyper_tibble()
  
  xmin = as.numeric(xmin)
  xmax = as.numeric(xmax)
  span = xmax - xmin
  xmin_ = max(min(heights$measure), xmin - 2*span)
  xmax_ = min(max(heights$measure), xmax + 2*span)

  heights %>%
    filter(measure >= xmin_ & measure <= xmax_) %>%
    pivot_wider(
      id_cols = c(swath, measure),
      names_from = talweg_height_is_interpolated,
      values_from = c(talweg_height_min, talweg_height_median)) %>%
    arrange(measure)
  
}