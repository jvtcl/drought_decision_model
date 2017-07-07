FROM rocker/shiny

MAINTAINER Max Joseph "maxwell.b.joseph@colorado.edu"

# Install dependencies and Download and install shiny server
RUN apt-get update \
  && apt-get install -y --no-install-recommends \
  libcurl4-openssl-dev \
  libssl-dev \
  libxml2-dev \
  libv8-3.14-dev \
  libmariadb-client-lgpl-dev \
  && R -e "install.packages('RMySQL', type= 'source')" \
  && R -e "install.packages(c('leaflet', 'shinyBS', 'shinyjs', 'data.table', 'RColorBrewer', 'V8', 'dplyr', 'httr', 'readr', 'plyr', 'scales', 'shiny', 'shiny', 'markdown', 'ggplot2', 'magrittr'))" \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/*

EXPOSE 3838

COPY . /srv/shiny-server/

COPY shiny-server.conf  /etc/shiny-server/shiny-server.conf

COPY shiny-server.sh /usr/bin/shiny-server.sh

CMD ["/usr/bin/shiny-server.sh"]
