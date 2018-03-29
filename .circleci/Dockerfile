FROM rocker/tidyverse

RUN apt-get update && apt-get -y install \
    mariadb-client \
	postgresql-client

RUN echo "en_US.ISO8859-1 ISO-8859-1" >> /etc/locale.gen && \
	locale-gen

RUN installGithub.r ropensci/taxizedb


