plotTalwegHeight = function(x, xmin, xmax) {
  
  span = xmax - xmin
  domain = data.frame(xmin = xmin, xmax = xmax)
  
  heights = tidync('METRICS/TALWEG_RELATED.nc') %>%
    hyper_tibble() %>%
    pivot_wider(
      id_cols = c(swath, measure),
      names_from = talweg_height_is_interpolated,
      values_from = c(talweg_height_min, talweg_height_median)) %>%
    arrange(measure)
  
  if (is_empty(domain)) {
    
    plot = ggplot(heights) +
      geom_line(aes(x = measure, y = talweg_height_median_1), color = 'darkgray', size = 0.8, na.rm = TRUE) +
      geom_line(aes(x = measure, y = talweg_height_median_0), color = '#48638a', size = 0.8, na.rm = TRUE)
    
  } else {
    
    xmin_ = max(min(heights$measure), xmin - 2*span)
    xmax_ = min(max(heights$measure), xmax + 2*span)
    
    selection = heights %>%
      filter(measure >= xmin_ & measure <= xmax_)
    
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
      geom_line(aes(x = measure, y = talweg_height_median_1), color = 'darkgray', size = 0.8) +
      geom_line(aes(x = measure, y = talweg_height_median_0), color = '#48638a', size = 0.8)
    
    if (!is_empty(x)) {
      plot = plot +
        geom_vline(xintercept = x, color = 'darkred', size = 0.8, alpha=0.7)
    }
    
  }
  
  plot +
    scale_x_reverse(labels=unit_format(scale=1e-3, unit='km')) +
    scale_y_continuous(labels=unit_format(scale=1, unit='m')) +
    xlab('Location along reference axis') +
    ylab('Height relative to floodplain') +
    ggtitle('Talweg height') +
    theme_bw()
    
  
}