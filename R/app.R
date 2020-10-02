library(shiny)
library(sf)
library(dplyr)
library(purrr)
library(tidyr)
library(forcats)
library(leaflet)
library(ggplot2)
library(scales)
library(jsonlite)
library(glue)

# library(promises)
# library(future)
# plan(multiprocess)

host = 'localhost'
port = 3098
crs = 2154

# source('plots/planform.R', local = TRUE)
# source('plots/talwegHeight.R', local = TRUE)
# source('plots/elevationSwathProfile.R', local = TRUE)
# setwd('/media/crousson/Backup/PRODUCTION/GrandsAxes/Isere/AXES/AX0002')

box2polygon = function(xmin, ymin, xmax, ymax, crs) {
  # print(class(x))
  st_bbox(
    c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax),
    crs = crs) %>%
  st_as_sfc()
}

# geometries = pmap_dfr(
#   list(swaths$minx, swaths$miny, swaths$maxx, swaths$maxy),
#   ~ data.frame(geometry = box2polygon(..1, ..2, ..3, ..4, 2154)))
# 
# swp = swaths %>%
#   mutate(geometry = geometries$geometry) %>%
#   st_sf()


ui = fluidPage(
  
  tags$style(HTML(
    type = "text/css",
    ' html {
        height: 100%;
      }

      body {
        height: calc(100vh - 80px) !important;
      }

      #row1 {
        margin-top: 40px;
        height: 100%;
        display: flex;
        align-items: center;
    }'
  )),
  
  fluidRow(
    
    id='row1',
    
    column(6,
           leafletOutput("mymap", height='calc(100vh - 80px)')),
    
    column(6,
             plotOutput(
               "plot1",
               click='plot1_click',
               brush='plot1_brush',
               height='calc(0.3*(100vh - 80px))'),
             plotOutput("plot2", height='calc(0.3*(100vh - 80px))'))
  )
  
)

server = function(input, output, session) {
  
  state = reactiveValues()
    
  swaths = fromJSON(glue('http://{host}:{port}/swaths')) %>%
    mutate(
      map_dfr(
        .$geometry$coordinates,
        ~ data.frame(x = .x[1], y = .x[2]))) %>%
    st_as_sf(
      coords = c('x', 'y'),
      crs = crs)
  
  heights = fromJSON(glue('http://{host}:{port}/lp/talweg/height'))
  
  get_swath_profile = reactive({
    
    if (is_empty(state$selected_feature)) {
      return (NULL)
    }
      
    id = state$selected_feature$label
    fromJSON(glue('http://{host}:{port}/swath/{id}/elevation'))
    
  })
  
  observeEvent(input$mymap_bounds, {
    
    bounds = input$mymap_bounds
    bbox_polygon = st_bbox(
        c(
          xmin = as.numeric(bounds$west),
          ymin = as.numeric(bounds$south),
          xmax = as.numeric(bounds$east),
          ymax = as.numeric(bounds$north)),
        crs = st_crs(4326)) %>%
      st_as_sfc() %>%
      st_transform(2154)
    
    state$bbox = bbox_polygon
    feature = state$selected_feature
    
    if (is.null(feature)) {
      
      center = st_point_on_surface(bbox_polygon)
      nearest = swaths[st_nearest_feature(center, swaths),]
      state$selected_feature = nearest
      # state$selected_label = nearest$label
      state$selected_measure = nearest$label * 200 - 100
      
    } else {
      
      if (!st_contains(bbox_polygon, feature, sparse = FALSE)[1,]) {
      
        center = st_point_on_surface(bbox_polygon)
        nearest = swaths[st_nearest_feature(center, swaths),]
        state$selected_feature = nearest
        # state$selected_label = nearest$label
        state$selected_measure = nearest$label * 200 - 100
        
      }
      
    }
    
  })
  
  observeEvent(state$bbox, {
    
    bounds = swaths %>%
      filter(st_contains(state$bbox, ., sparse = FALSE)[1,]) %>%
      st_drop_geometry() %>% summarise(
        xmin = min(label) * 200 - 100,
        xmax = max(label) * 200 - 100)
    
    state$xmin = bounds$xmin
    state$xmax = bounds$xmax
    
  })
  
  output$mymap = renderLeaflet({
    
    points = swaths %>% st_transform(4326)
    bounds = as.numeric(st_bbox(points))
    
    leaflet() %>%
      addProviderTiles(
        providers$Stamen.TonerLite,
        options = providerTileOptions(noWrap = TRUE)
      ) %>%
      # addMarkers(data = points) %>%
      # fitBounds(4.85, 44.97, 7.09, 45.67)
      # fitBounds(bounds$xmin, bounds$ymin, bounds$xmax, bounds$ymax)
      fitBounds(bounds[1], bounds[2], bounds[3], bounds[4])
    
  })
  
  observeEvent(state$selected_feature, {

    feature = st_transform(state$selected_feature, 4326)

    leafletProxy('mymap') %>%
      clearMarkers() %>%
      addMarkers(data = feature)

  })
  
  observeEvent(input$mymap_click, {
    
    lon = as.numeric(input$mymap_click$lng)
    lat = as.numeric(input$mymap_click$lat)
    
    click = st_point(c(x = lon, y = lat), dim = 'XY') %>%
        st_sfc() %>%
        st_set_crs(4326) %>%
        st_transform(2154)
    
    bbox = state$bbox
    
    if (!is.null(bbox)) {
      
      if (st_contains(bbox, click, sparse = FALSE)[1,]) {
      
        nearest = swaths[st_nearest_feature(click, swaths),]
        state$selected_feature = nearest
        # state$selected_label = nearest$label
        state$selected_measure = nearest$label * 200 - 100
        
      }
      
    }
    
  })
  
  observeEvent(input$plot1_click, {
    
    x = input$plot1_click$x
    label_ = round((x + 100) / 200)
    
    nearest = swaths %>% filter(label == label_)
    
    if (count(nearest)$n > 0) {
      
      state$selected_feature = nearest[1,]
      state$selected_measure = label_ * 200 - 100
      
    }
    
    if (x < state$xmin | x > state$xmax) {
      
      delta = state$xmax - state$xmin
      label_min = round((x - 0.5*delta + 100) / 200)
      label_max = round((x + 0.5*delta + 100) / 200)
      
      bounds = swaths %>%
        filter(label >= label_min & label <= label_max) %>%
        st_transform(4326) %>%
        st_bbox()
      
        leafletProxy("mymap") %>%
          fitBounds(
            as.numeric(bounds$xmin),
            as.numeric(bounds$ymin),
            as.numeric(bounds$xmax),
            as.numeric(bounds$ymax))
      
    }
  
  })
  
  observeEvent(input$plot1_brush, {
    
    minx = input$plot1_brush$xmin
    maxx = input$plot1_brush$xmax
    
    label_min = round((minx + 100) / 200)
    label_max = round((maxx + 100) / 200)
    
    bounds = swaths %>%
      filter(label >= label_min & label <= label_max) %>%
      st_transform(4326) %>%
      st_bbox()
    
    leafletProxy("mymap") %>%
      fitBounds(
        as.numeric(bounds$xmin),
        as.numeric(bounds$ymin),
        as.numeric(bounds$xmax),
        as.numeric(bounds$ymax))
    
  })
  
  output$plot1 = renderPlot({
    
    # domain = get_mbox()
    x = state$selected_measure
    
    # isolate({
      
      xmin = state$xmin
      xmax = state$xmax
      
      # plotPlanform(x, xmin, xmax)
      plotTalwegHeight(heights, x, xmin, xmax)
      
    # })
    
  })
  
  output$plot2 = renderPlot({
    
    profile = get_swath_profile()
    
    if (is_empty(profile)) {
      return ()
    }
    
    isolate({
      
      label = state$selected_feature$label
      pk = round(state$selected_measure / 1000, 1)
      
      plotElevationSwathProfile(profile, label, pk)
      
    })
    
  })
  
}

runApp = function(host = '0.0.0.0', port = 3038) {
  shiny::runApp(
    list(
      ui = ui,
      server = server
    ),
    host = host)
}

if (interactive()) {
  shinyApp(ui, server) 
}