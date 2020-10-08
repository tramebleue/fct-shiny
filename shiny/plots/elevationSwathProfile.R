plotElevationSwathProfile = function(profile, label, pk) {
  
  ggplot(profile) +
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
    ggtitle(paste('Elevation swath profile,', 'pk =', pk, 'km,', paste0('swath #', label))) +
    theme_bw()
  
}