# Χρησιμοποίηση της επίσημης εικόνας του R
FROM r-base:latest

# Εγκατάσταση των πακετων
RUN R -e "install.packages(c('shiny', 'rpart', 'ggplot2', 'dplyr', 'cluster'), repos='https://cran.rstudio.com/')"


# Αντιγραφή του R Shiny app στον container
COPY TLapp.R /srv/shiny-server/

# Ορισμός του φακέλου εργασίας
WORKDIR /srv/shiny-server/

# Εκκίνηση του Shiny server
CMD ["R", "-e", "shiny::runApp('TLapp.R', host='0.0.0.0', port=3838)"]
 
