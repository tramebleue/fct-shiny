#--------- Generic stuff all our Dockerfiles should start with so we get caching ------------
FROM rocker/r-ver:3.6.3
MAINTAINER rototo
RUN  export DEBIAN_FRONTEND=noninteractive
ENV  DEBIAN_FRONTEND noninteractive
RUN  dpkg-divert --local --rename --add /sbin/initctl

# install stuff

RUN apt-get update -qq && apt-get install -y \
  git-core \
  libssl-dev \
  libcurl4-gnutls-dev \
  libudunits2-dev \
  libgdal-dev \
  libsodium-dev 
#  r-cran-httpuv \
#  r-cran-plumber \
#  r-cran-sf

ADD dependencies.R /
RUN R -e 'install.packages("remotes")'
RUN R -f /dependencies.R

#RUN install2.r --error \
#      --deps TRUE \
#      withr \
#      xfun \
#      jsonlite \
#      tidync \
#      sf \
#      dplyr \
#      tidyr \
#      purrr \
#      forcats \
#      plumber \
#      scales \
#      leaflet \
#    && rm -rf /tmp/downloaded_packages

ADD fct-shiny.tgz /packages/fct-shiny.tgz
RUN r -e 'library(remotes); remotes::install_local("/packages/fct-shiny.tgz")'

#ADD fct.shiny_0.1.tar.gz /packages/fct.shiny_0.1.tar.gz
# RUN cd /packages \
# && tar xzf fct.shiny_0.1.tar.gz \
# && tar czf fct-shiny.tgz fct-shiny/* \
# && r -e 'library(remotes); remotes::install_local("/packages/fct-shiny.tgz")'
#RUN r -e 'remotes::install_local("/packages/fct.shiny_0.1.tar.gz/fct-shiny")'

EXPOSE 3838
CMD r -e 'library(fct.shiny); fct.shiny::runApp(port=3838)'

