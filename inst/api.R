library(plumber)
library(dplyr)
library(tidyr)
library(purrr)
library(tidync)
library(sf)
# library(geojsonsf)

#* @apiTitle('Fluvial Corridor Toolbox Data API')

workdir = Sys.getenv('FCT_DATA')

if (!endsWith(workdir, '/')) {
  workdir = paste0(workdir, '/')
}

datafile = function(subpath) {
  paste0(workdir, subpath)  
}

#* @get /config
function() {
  list(FCT_DATA=workdir)
}

#* @get /swaths
function() {
  
  # swaths = tidync(datafile('TEMP/VALLEY_SWATH_BOUNDS.nc')) %>%
  #   hyper_tibble() %>%
  #   pivot_wider(
  #     id_cols = label,
  #     names_from = coord,
  #     values_from = bounds) %>%
  #   mutate(
  #     x = .5 * (minx + maxx),
  #     y = .5* (miny + maxy)) %>%
  #   st_as_sf(coords = c('x', 'y'), crs = st_crs(2154))
  #   # sf_geojson()
  
  swaths = st_read(datafile('REF/VALLEY_SWATHS_SIMPL.shp'))
  boxes = map_dfr(map(swaths$geometry, st_bbox), ~ as.list(.x))
  
  swaths %>%
    transmute(
      label = GID,
      measure = M) %>%
    mutate(
      minx = boxes$xmin,
      miny = boxes$ymin,
      maxx = boxes$xmax,
      maxy = boxes$ymax) %>%
    st_point_on_surface() %>%
    arrange(measure)

}

#* @get /swath/<id:int>/geometry
function(id) {
  
  # swaths = tidync(datafile('TEMP/VALLEY_SWATH_BOUNDS.nc')) %>%
  #   hyper_tibble() %>%
  #   pivot_wider(
  #     id_cols = label,
  #     names_from = coord,
  #     values_from = bounds) %>%
  #   mutate(
  #     x = .5 * (minx + maxx),
  #     y = .5* (miny + maxy)) %>%
  #   st_as_sf(coords = c('x', 'y'), crs = st_crs(2154))
  #   # sf_geojson()
  
  st_read(datafile('REF/VALLEY_SWATHS_SIMPL.shp')) %>%
    filter(GID == id) %>%
    transmute(label = GID, measure = M)
  
}

#* @get /swath/<id:int>/elevation
function(id) {
  
  swath = tidync(datafile('METRICS/ELEVATION_SWATH_PROFILES.nc'), what='D0') %>%
    hyper_tibble() %>%
    filter(swath == id)
  
  profiles = tidync(datafile('METRICS/ELEVATION_SWATH_PROFILES.nc'), what='D1') %>%
    hyper_tibble() %>%
    filter(sw_measure == swath$measure)
  
  tidync(datafile('METRICS/ELEVATION_SWATH_PROFILES.nc'), what='D2,D1') %>%
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
  
  tidync(datafile('METRICS/PLANFORM.nc')) %>% hyper_tibble()
  
}

#* @param xmin
#* @param xmax
#* @get /lp/talweg/height
function(xmin=-Inf, xmax=Inf) {
  
  # if (missing(xmin) | missing(xmax)) {
    
  # heights = tidync('METRICS/TALWEG_RELATED.nc') %>%
  #   hyper_tibble()
    
  heights = tidync(datafile('METRICS/TALWEG_RELATED.nc')) %>%
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
