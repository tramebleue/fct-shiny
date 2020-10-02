runApi = function(host = '0.0.0.0', port = 3098) {

  library(magrittr)
  library(plumber)
   
  router = pr(system.file('api.R', package = 'fct.shiny')) %>%
    pr_run(host = host, port = port)
  
}
