FROM mambaorg/micromamba:0.22.0
RUN micromamba install -y -n base -c conda-forge \
    r-base r-e1071 r-leaflet r-shinywidgets r-shinydashboard r-shinycssloaders r-shinyalert r-shinydashboardplus r-shinybs r-dplyr r-caret r-ranger r-open    xlsx \
    && micromamba clean --all --yes
ARG MAMBA_DOCKERFILE_ACTIVATE=1
RUN git clone https://github.com/TheDataLabScotland/Data_Team_Blog_Posts.git \
    && cd Data_Team_Blog_Posts \
    && git checkout dockerized-app \
    && cd TrafficAccidents
COPY . /home/mambauser
WORKDIR /home/mambauser
EXPOSE 3838
CMD R --no-save -e 'shiny::runApp("/home/shiny", port = 3838, host = "0.0.0.0")'
