# shiny-data-analysis

This repository contains the R files needed to perform a statistical analysis on the measurements obtained with the testing station.

## Docker site deploy

If you want to deploy the shiny web application without having to use [RStudio IDE](https://posit.co/download/rstudio-desktop/), you can use Docker. Docker can be installed on [Windows](https://docs.docker.com/desktop/install/windows-install/) or [other](https://docs.docker.com/engine/install/) operating systems as well.

### Using prebuild Docker image

#### docker run

If you succesfully installed Docker you can start using the prebuild [docker image](https://github.com/solar-cooker-UHasselt/shiny-data-analysis/pkgs/container/shiny-data-analysis) provided in this repository using the following command:

```zsh
docker run --name shiny-app -d -p 3838:3838 ghcr.io/solar-cooker-uhasselt/shiny-data-analysis:latest
```

#### docker-compose

Or use the provided [docker-compose.yml](docker-compose.yml) file using the following command:

> [!IMPORTANT]
> [docker-compose](https://docs.docker.com/compose/install/) has to be installed first in order for this to work.

```zsh
docker-compose up -d
```

If all went well, you should now be able to navigate in your browser to http://localhost:3838

### Building the Docker image locally

#### docker run

If you succesfully installed Docker you can start building the Docker image using the following command:

```zsh
docker build -t shiny-app-image .
```

> [!IMPORTANT]
> Ensure that you have downloaded this repository and are currently located at its top directory within a terminal window.

> [!NOTE]
> It will take some time to build the image for the first time, so be patient.

Next step is to run the following command:

```zsh
docker run --name shiny-app -d -p 3838:3838 shiny-app-image
```

#### docker-compose

In order to build the image locally using docker-compose change the following line in the [docker-compose.yml](docker-compose.yml) file:

```yml
    image: ghcr.io/solar-cooker-uhasselt/shiny-data-analysis:latest
```

Into:

```yml
    build:
      context: .
      dockerfile: Dockerfile
```

If all went well, you should now be able to navigate in your browser to http://localhost:3838
