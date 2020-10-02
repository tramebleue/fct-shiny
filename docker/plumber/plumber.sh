#!r

library(plumber)

pr = plumber::plumb(system.file('api.R', package = 'fct.shiny'))
pr$run(host = "0.0.0.0", port = 3098)
