FROM rocker/r-ver:4.1.0
RUN apt-get update -qq && apt-get install -y \
      libssl-dev \
      libcurl4-gnutls-dev
MAINTAINER KIM Ha-Jun, Department of Statistics,  PNU
RUN R -e "install.packages('plumber', dependencies=TRUE)"
RUN R -e "install.packages('data.table', dependencies=TRUE)"
RUN R -e "install.packages('recommenderlab', dependencies=TRUE)"

COPY / /
EXPOSE 8080
ENTRYPOINT ["Rscript", "server.R"]