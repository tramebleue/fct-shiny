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
RUN R -e 'remotes::install_local("/packages/fct-shiny.tgz")'

#EXPOSE 3038
#CMD R -e 'fct.shiny::runApi()'

