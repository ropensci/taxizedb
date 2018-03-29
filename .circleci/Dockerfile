FROM rocker/tidyverse

RUN apt-get update && apt-get -y install \
    mariadb-client \
	postgresql-client && \
	echo "en_US ISO-8859-1" >> /etc/locale.gen && \
	locale-gen en_US

