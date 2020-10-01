library(sf)
library(dplyr)
library(purrr)
library(tidyr)
library(shiny)
library(leaflet)
library(plotly)
library(tidync)
library(scales)

setwd('/Volumes/Backup/PRODUCTION/GrandsAxes/Isere/AXES/AX0002')

box2polygon = function(xmin, ymin, xmax, ymax, crs) {
  # print(class(x))
  st_bbox(
    c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax),
    crs = crs) %>%
  st_as_sfc()
}

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

# geometries = pmap_dfr(
#   list(swaths$minx, swaths$miny, swaths$maxx, swaths$maxy),
#   ~ data.frame(geometry = box2polygon(..1, ..2, ..3, ..4, 2154)))
# 
# swp = swaths %>%
#   mutate(geometry = geometries$geometry) %>%
#   st_sf()

planform = tidync('METRICS/PLANFORM.nc') %>% hyper_tibble()

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
  
  get_swath_profile = reactive({
    
    if (is_empty(state$selected_measure)) {
      return (NULL)
    }
    
    profiles = tidync('METRICS/ELEVATION_SWATH_PROFILES.nc', what='D1') %>%
      hyper_tibble() %>%
      filter(sw_measure == state$selected_measure)
    
    tidync('METRICS/ELEVATION_SWATH_PROFILES.nc', what='D2,D1') %>%
      hyper_tibble(profile = profile %in% profiles$profile) %>%
      left_join(profiles, by = 'profile') %>%
      pivot_wider(
        id_cols = c(profile, sw_measure, sw_axis_distance),
        names_from = quantile,
        names_prefix = 'sw_elevation_abs_',
        values_from = sw_elevation_abs)%>%
      arrange(sw_axis_distance)
    
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
    
    if ((x < state$xmin) || (x > state$xmax)) {
      
      delta = state$xmax - state$xmin
      label_min = round((x - 0.5*delta + 100) / 200)
      label_max = round((x + 0.5*delta + 100) / 200)
      
      bounds = as.numeric(swaths %>%
        filter(label >= label_min & label <= label_max) %>%
        st_transform(4326) %>%
        st_bbox())
      
        leafletProxy("mymap") %>%
          # fitBounds(bounds$xmin, bounds$ymin, bounds$xmax, bounds$ymax)
          fitBounds(bounds[1], bounds[2], bounds[3], bounds[4])
      
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
    xmin = state$xmin
    xmax = state$xmax
    span = xmax - xmin
    domain = data.frame(xmin = xmin, xmax = xmax)
    
    if (is_empty(domain)) {
      
      plot = ggplot(planform) +
        geom_line(aes(x = measure, y = talweg_shift), color = 'darkgray', size = 0.8)
      
    } else {
    
      xmin_ = max(min(planform$measure), xmin - 2*span)
      xmax_ = min(max(planform$measure), xmax + 2*span)
      
      selection = planform %>%
        filter((measure >= xmin_) & (measure) < xmax_)
      
      plot = ggplot(selection) +
        geom_rect(
          data = domain,
          aes(xmin = xmin, xmax = xmax),
          ymin = -Inf, ymax = Inf,
          fill = 'darkgreen', colour = NA, alpha=0.1) +
        geom_rect(
          data = domain,
          aes(xmin = xmin, xmax = xmax),
          ymin = -Inf, ymax = Inf,
          fill = NA, colour = 'darkgreen', linetype = 'dotted', size = 0.8, alpha=0.8) +
        geom_line(aes(x = measure, y = talweg_shift), color = 'darkgray', size = 0.8)
      
      if (!is_empty(state$selected_measure)) {
        plot = plot +
          geom_vline(xintercept = state$selected_measure, color = 'darkred', size = 0.8, alpha=0.7)
      }
        
    }
    
    plot +
      scale_x_reverse(labels=unit_format(scale=1e-3, unit='km')) +
      scale_y_continuous(labels=unit_format(scale=1, unit='m')) +
      xlab('Location along reference axis') +
      ylab('Distance to reference axis') +
      ggtitle('Talweg shift from reference axis') +
      theme_bw()
    
  })
  
  output$plot2 = renderPlot({
    
    data = get_swath_profile()
    
    if (is.null(data)) {
      return ()
    }
    
    ggplot(data) +
      geom_line(
        aes(
          x = -sw_axis_distance,
          y = sw_elevation_abs_0.5),
        color = '#48638a',
        size = 1) +
      geom_ribbon(
        aes(
          x = -sw_axis_distance,
          ymin = sw_elevation_abs_0.05,
          ymax = sw_elevation_abs_0.95),
        fill = '#b9d8e6',
        color = 'gray',
        size = 0.7,
        linetype = 'dotted',
        alpha = 0.2) +
      geom_ribbon(
        aes(
          x = -sw_axis_distance,
          ymin = sw_elevation_abs_0.25,
          ymax = sw_elevation_abs_0.75),
        fill = '#48638a',
        color = 'gray',
        size = 0.5,
        alpha = 0.5) +
      scale_x_continuous(
        labels = unit_format(scale=1, unit='m'),
        name = 'Distance from reference axis') +
      scale_y_continuous(
        labels = unit_format(scale=1, unit='m'),
        name = 'Altitude NGF') +
      ggtitle('Elevation swath profile') +
      theme_bw()
    
  })
  
}

shinyApp(ui, server)