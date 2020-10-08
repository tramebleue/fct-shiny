FROM rocker/shiny-verse:3.6.3
MAINTAINER crousson

RUN apt-get update && apt-get install -y \
  libssl-dev \
  libcurl4-gnutls-dev \
  libudunits2-dev \
  libgdal-dev \
  libsodium-dev 

RUN install2.r --error \
      --deps TRUE \
      withr \
      xfun \
      jsonlite \
      tidync \
      sf \
      plumber \
      scales \
      leaflet \
    && rm -rf /tmp/downloaded_packages