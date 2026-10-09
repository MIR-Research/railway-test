FROM edemain/shiny-runtime:v6

WORKDIR /app

# R packages missing from the base image: Cubist and kernlab are needed by
# caret to predict with Cubist and SVM models; doParallel lets caret run its
# cross-validation fits on all CPU cores (modules/parallel_training.R);
# mirai runs model training in a background process
# (modules/background_tasks.R).
# Uses the image's default repo (a pinned Posit Package Manager snapshot) so
# we get prebuilt binaries that match the other package versions. The
# stopifnot() makes the build fail loudly if an install didn't work
# (install.packages only warns).
RUN Rscript -e "install.packages(c('Cubist', 'kernlab', 'doParallel', 'mirai'))" \
 && Rscript -e "for (p in c('Cubist', 'kernlab', 'doParallel', 'mirai')) stopifnot(requireNamespace(p, quietly = TRUE))"

COPY . /app

CMD ["Rscript", "start-shiny.R"]
