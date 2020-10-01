plotPlanform = function(x, xmin, xmax) {
  
  planform = tidync('METRICS/PLANFORM.nc') %>% hyper_tibble()
  
  span = xmax - xmin
  domain = data.frame(xmin = xmin, xmax = xmax)
  
  if (is_empty(domain)) {
    
    plot = ggplot(planform) +
      geom_line(aes(x = measure, y = talweg_shift), color = 'darkgray', size = 0.8)
    
  } else {
    
    xmin_ = max(min(planform$measure), xmin - 2*span)
    xmax_ = min(max(planform$measure), xmax + 2*span)
    
    selection = planform %>%
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
      geom_line(aes(x = measure, y = talweg_shift), color = 'darkgray', size = 0.8)
    
    if (!is_empty(x)) {
      plot = plot +
        geom_vline(xintercept = x, color = 'darkred', size = 0.8, alpha=0.7)
    }
    
  }
  
  plot +
    scale_x_reverse(labels=unit_format(scale=1e-3, unit='km')) +
    scale_y_continuous(labels=unit_format(scale=1, unit='m')) +
    xlab('Location along reference axis') +
    ylab('Distance to reference axis') +
    ggtitle('Talweg shift from reference axis') +
    theme_bw()
  
}