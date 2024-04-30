FROM rocker/shiny:latest

RUN apt-get update -qq && apt-get -y install \
    --no-install-recommends \
    git-core \
    libssl-dev \
    libcurl4-gnutls-dev \
    curl \
    libsodium-dev \
    libxml2-dev \
    libicu-dev

RUN apt-get clean && \
    rm -rf /var/lib/apt/lists/*

RUN install2.r --error --skipinstalled \
    shinydashboard \
    dplyr \
    stringr \
    XML \
    rvest \
    lubridate \
    ggplot2

RUN mkdir -p /srv/shiny-server/

COPY R/ /srv/shiny-server/

RUN chown -R shiny:shiny /srv/shiny-server/

USER shiny

EXPOSE 3838

CMD ["/usr/bin/shiny-server"]
