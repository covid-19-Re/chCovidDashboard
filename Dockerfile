FROM rocker/shiny-verse:4.0.3
WORKDIR /app/

COPY install_packages.R .
RUN Rscript install_packages.R

RUN apt update
RUN apt install -y libudunits2-dev libproj-dev libgdal-dev

COPY . .

EXPOSE 5000
ENTRYPOINT ["Rscript", "-e", "shiny::runApp(port = 5000, host = '0.0.0.0')"]
