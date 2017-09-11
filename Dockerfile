FROM openanalytics/r-base

MAINTAINER Nathan Vaughan "nathan.vaughan1@gmail.com"

# system libraries of general use
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    libssl1.0.0 \
    default-jre \
    default-jdk \
    libxml2 \
    libxml2-dev

# install netcdf4
CMD ["/usr/local/bin/install_netcdf4.sh"]	
	
# install dependencies of the ICCAT app
RUN R -e "install.packages(c('shiny','stringr','rcharts','plyr','reshape','ncdf4','rmarkdown','LBSPR','reshape2','ReporteRs','ggplot2','ReporteRsjars'), repos='https://cloud.r-project.org/')"

# copy the app to the image
RUN mkdir /root/ICCAT
COPY ICCAT /root/ICCAT

COPY Rprofile.site /usr/lib/R/etc/

EXPOSE 3838

CMD ["R", "-e shiny::runApp('/root/ICCAT')"]
