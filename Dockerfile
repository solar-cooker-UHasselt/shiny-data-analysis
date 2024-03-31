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
    shiny \
    shinydashboard \
    dplyr \
    stringr \
    XML \
    rvest \
    httr \
    lubridate \
    ggplot2

COPY R/* /srv/shiny-server/

USER shiny

EXPOSE 3838

CMD ["/usr/bin/shiny-server"]
