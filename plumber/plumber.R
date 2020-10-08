library(plumber)

router = pr('api.R') %>%
  pr_run(host = '0.0.0.0', port = 3098)
