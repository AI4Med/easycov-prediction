FROM r-base:3.6.3
COPY . /usr/local/src
WORKDIR /usr/local/src
EXPOSE 8082/tcp

# System dependencies
RUN apt-get update && apt-get install -y \ 
    libcurl4-openssl-dev \
    libgit2-dev \
    libxml2-dev \
    libssl-dev

# R dependencies
RUN R -e "install.packages('plumber',dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('readr',dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('plyr',dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('tidyverse',dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('DMwR',dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('e1071',dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('pROC',dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('jsonlite',dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('caret',dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('rpart',dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('Rmisc',dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('boot',dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('dplyr',dependencies=TRUE, repos='http://cran.rstudio.com/')"

# Run API
CMD ["Rscript", "/usr/local/src/api.R"]