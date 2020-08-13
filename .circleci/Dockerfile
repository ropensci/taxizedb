FROM rocker/tidyverse

RUN echo "en_US.ISO8859-1 ISO-8859-1" >> /etc/locale.gen && \
	locale-gen

RUN installGithub.r ropensci/taxizedb


