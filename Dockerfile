FROM edemain/shiny-runtime:v6

WORKDIR /app

# Thread limits, applied to every process in the container. By default
# OpenBLAS (R's maths library) starts one thread per CPU of the whole Railway
# host machine in every R process, even idle ones, and the app runs ~13 R
# processes (main app, background training processes and their workers).
# Together with TensorFlow and Python libraries (rayon) doing the same, that
# hit the container's thread limit, crashing CNN predictions ("pthread_create()
# failed" / "Resource temporarily unavailable"). These must be set before R
# starts, so they go here rather than in R code. Parallel work in the app
# comes from separate worker processes, not BLAS threads, so 1 is enough.
# Override any of them with a Railway variable of the same name.
ENV OPENBLAS_NUM_THREADS=1 \
    RAYON_NUM_THREADS=2

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
