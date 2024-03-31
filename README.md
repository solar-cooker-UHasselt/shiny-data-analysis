# shiny-data-analysis

This repository contains the R files needed to perform a statistical analysis on the measurements obtained with the testing station.

## Deploying the site with Docker

If you want to deploy the shiny web application without having to use [RStudio IDE](https://posit.co/download/rstudio-desktop/), you can use Docker. Docker can be installed on [Windows](https://docs.docker.com/desktop/install/windows-install/) or [other](https://docs.docker.com/engine/install/) operating systems as well.

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

If all went well, you should now be able to navigate in your browser to http://localhost:3838
