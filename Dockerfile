FROM edemain/shiny-runtime:v6

WORKDIR /app

# R packages missing from the base image, needed by caret to predict with
# Cubist (Cubist) and SVM (kernlab) models. Uses the image's default repo
# (a pinned Posit Package Manager snapshot) so we get prebuilt binaries that
# match the other package versions. The stopifnot() makes the build fail
# loudly if an install didn't work (install.packages only warns).
RUN Rscript -e "install.packages(c('Cubist', 'kernlab'))" \
 && Rscript -e "for (p in c('Cubist', 'kernlab')) stopifnot(requireNamespace(p, quietly = TRUE))"

COPY . /app

CMD ["Rscript", "start-shiny.R"]
