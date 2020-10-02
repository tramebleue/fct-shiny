#--------- Generic stuff all our Dockerfiles should start with so we get caching ------------
FROM fct-shiny
MAINTAINER rototo

# install stuff

RUN r -e 'install.packages(c("sf", "jsonlite"), repos="https://cloud.r-project.org")'

ADD plumber.sh /opt/bin/plumber.sh

EXPOSE 3098
CMD R -f /opt/bin/plumber.sh

