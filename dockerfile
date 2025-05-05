FROM rocker/shiny-verse:latest 

RUN apt-get update && apt-get install -y --no-install-recommends \ 
    sudo \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    && rm -rf /var/lib/apt/lists/*


RUN R -e "install.packages(c('shiny', 'shinydashboard','plotly', 'dplyr', 'magrittr', 'DT', 'rjson', 'shinyjs','datasets','graphics','grDevices','methods','stats','utils','shinyWidgets'))"
RUN R -e "install.packages(c('Kendall','janitor','base','shinycssloaders','patchwork','tidyverse','glue','lubridate','scales','readxl','shinycssloaders','RobustLinearReg','openxlsx','EnvStats','fst','ggrepel','ggiraph','ggh4x','broom'))" 
RUN R -e "install.packages('gpclib', type='source')" 
RUN R -e "install.packages('rgeos', type='source')" 
RUN R -e "install.packages('rgdal', type='source')" 
RUN R -e "install.packages('pandoc', type='source')"
RUN R -e "install.packages('flextable', type='source')"

#Work Dir explicitly set the root directory
WORKDIR /App 
COPY . /App /srv/shiny-server/App 

EXPOSE 3838 

RUN sudo chown -R shiny:shiny /srv/shiny-server 
#RUN ["chmod", "+x", "/shiny-server.sh"]

#CMD ["/usr/bin/shiny-server.sh"]