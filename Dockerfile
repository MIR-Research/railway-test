FROM edemain/shiny-runtime:v6

WORKDIR /app
COPY . /app

CMD ["Rscript", "start-shiny.R"]
