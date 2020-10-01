library(sf)
library(dplyr)
library(purrr)
library(tidyr)
library(shiny)
library(leaflet)
library(plotly)
library(tidync)

setwd('/Volumes/Backup/PRODUCTION/GrandsAxes/Isere/AXES/AX0002')

swaths = tidync('TEMP/VALLEY_SWATH_BOUNDS.nc') %>%
  hyper_tibble() %>%
  pivot_wider(
    id_cols = label,
    names_from = coord,
    values_from = bounds) %>%
  mutate(
    x = .5 * (minx + maxx),
    y = .5* (miny + maxy)) %>%
  st_as_sf(coords = c('x', 'y'), crs = st_crs(2154)) %>%
  st_transform(st_crs(4326))

bounds = as.numeric(swaths %>% st_bbox())

ui = fluidPage(
  
  column(6,
         leafletOutput("mymap")),
  
  column(6,
         plotOutput('plot1'),
         br(),
         textOutput('bbox'))
)

server = function(input, output, session) {
  
  get_map_bounds = reactive({
    # reduce(input$mymap_bounds, paste)
    box = as.numeric(input$mymap_bounds)
    botleft = st_point(x = c(box[1], box[2]), dim = 'XY')
    topright = st_point(x = c(box[3], box[4]), dim = 'XY')
    botright = st_point(x = c(box[3], box[2]), dim = 'XY')
    topleft = st_point(x = c(box[1], box[4]), dim = 'XY')
    st_polygon(list(as.matrix(c(botleft, topleft, topright, botright, botleft))))
  })
  
  get_mbox = reactive({
    
    box = as.numeric(input$mymap_bounds)
    botleft = st_point(x = c(box[1], box[2]), dim = 'XY')
    topright = st_point(x = c(box[3], box[4]), dim = 'XY')
    botright = st_point(x = c(box[3], box[2]), dim = 'XY')
    topleft = st_point(x = c(box[1], box[4]), dim = 'XY')
    
    pbox = st_polygon(list(as.matrix(c(botleft, topleft, topright, botright, botleft))))
    
    selection = swaths %>%
      filter(st_intersects(., pbox, sparse = FALSE)[1,]) %>%
      st_drop_geometry()
    
    if (is.empty(selection)) {
      return (list(0, 0))
    }
    
    list(selection %>% summarise(
        mmin = min(label) * 200 - 100,
        mmax = max(label) * 200 + 100))
  })
  
  output$mymap = renderLeaflet({
    leaflet() %>%
      addProviderTiles(
        providers$Stamen.TonerLite,
        options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addMarkers(data = swaths) %>%
      # fitBounds(4.85, 44.97, 7.09, 45.67)
      fitBounds(bounds[1], bounds[2], bounds[3], bounds[4])
  })
  
  output$plot1 = renderPlot({
    ggplot(planform) +
    geom_line(aes(x = talweg_measure, y = talweg_shift))
  })
  
  output$bbox = renderText({
    reduce(as.numeric(get_mbox()), paste)
  })
  
}

shinyApp(ui, server)