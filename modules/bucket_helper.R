library(aws.s3)
library(readr)

bucket_name <- function() {
  Sys.getenv("AWS_S3_BUCKET_NAME")
}

s3_opts <- function() {
  list(
    key = Sys.getenv("AWS_ACCESS_KEY_ID"),
    secret = Sys.getenv("AWS_SECRET_ACCESS_KEY"),
    region = "",
    base_url = sub("^https?://", "", Sys.getenv("AWS_ENDPOINT_URL")),
    use_https = TRUE,
    url_style = Sys.getenv("S3_URL_STYLE", unset = "path"),
    check_region = FALSE
  )
}

bucket_key <- function(...) {
  parts <- unlist(list(...))
  parts <- parts[!is.na(parts) & nzchar(parts)]
  paste(parts, collapse = "/")
}

list_bucket_keys <- function(prefix = "") {
  objs <- aws.s3::get_bucket(
    bucket = bucket_name(),
    prefix = prefix,
    max = Inf,
    key = s3_opts()$key,
    secret = s3_opts()$secret,
    region = s3_opts()$region,
    base_url = s3_opts()$base_url,
    use_https = s3_opts()$use_https,
    url_style = s3_opts()$url_style,
    check_region = s3_opts()$check_region
  )
  
  if (length(objs) == 0) return(character(0))
  
  keys <- vapply(
    objs,
    function(x) if (!is.null(x[["Key"]])) x[["Key"]] else NA_character_,
    character(1)
  )
  
  keys[!is.na(keys)]
}

bucket_object_exists <- function(object_key) {
  object_key %in% list_bucket_keys(object_key)
}

read_bucket_delim <- function(object_key, delim = ",") {
  aws.s3::s3read_using(
    FUN = readr::read_delim,
    object = object_key,
    bucket = bucket_name(),
    opts = s3_opts(),
    delim = delim,
    show_col_types = FALSE
  )
}

read_bucket_rds <- function(object_key) {
  aws.s3::s3readRDS(
    object = object_key,
    bucket = bucket_name(),
    key = s3_opts()$key,
    secret = s3_opts()$secret,
    region = s3_opts()$region,
    base_url = s3_opts()$base_url,
    use_https = s3_opts()$use_https,
    url_style = s3_opts()$url_style,
    check_region = s3_opts()$check_region
  )
}

write_bucket_rds <- function(x, object_key) {
  aws.s3::s3saveRDS(
    x = x,
    object = object_key,
    bucket = bucket_name(),
    key = s3_opts()$key,
    secret = s3_opts()$secret,
    region = s3_opts()$region,
    base_url = s3_opts()$base_url,
    use_https = s3_opts()$use_https,
    url_style = s3_opts()$url_style,
    check_region = s3_opts()$check_region
  )
}

download_bucket_file <- function(object_key, local_file) {
  aws.s3::save_object(
    object = object_key,
    bucket = bucket_name(),
    file = local_file,
    key = s3_opts()$key,
    secret = s3_opts()$secret,
    region = s3_opts()$region,
    base_url = s3_opts()$base_url,
    use_https = s3_opts()$use_https,
    url_style = s3_opts()$url_style,
    check_region = s3_opts()$check_region
  )
}

upload_bucket_file <- function(local_file, object_key) {
  aws.s3::put_object(
    file = local_file,
    object = object_key,
    bucket = bucket_name(),
    key = s3_opts()$key,
    secret = s3_opts()$secret,
    region = s3_opts()$region,
    base_url = s3_opts()$base_url,
    use_https = s3_opts()$use_https,
    url_style = s3_opts()$url_style,
    check_region = s3_opts()$check_region
  )
}

download_bucket_temp <- function(object_key, ext = "") {
  tmp <- tempfile(fileext = ext)
  
  aws.s3::save_object(
    object = object_key,
    bucket = bucket_name(),
    file = tmp,
    key = s3_opts()$key,
    secret = s3_opts()$secret,
    region = s3_opts()$region,
    base_url = s3_opts()$base_url,
    use_https = s3_opts()$use_https,
    url_style = s3_opts()$url_style,
    check_region = s3_opts()$check_region
  )
  
  tmp
}

read_bucket_model_rds <- function(object_key) {
  read_bucket_rds(object_key)
}

load_bucket_keras_model <- function(object_key) {
  # The CNN models in the bucket are legacy HDF5 files saved with a
  # ".keras" extension (confirmed via their file signature), not Keras 3
  # native-format zip archives. Keras's Python loader dispatches purely on
  # file extension (".h5"/".hdf5" -> legacy HDF5 loader, ".keras" -> native
  # zip-format loader), regardless of which R wrapper function is called
  # (load_model_hdf5/load_model_tf both end up calling the same
  # keras$models$load_model()). So the downloaded temp file must have a
  # ".h5" extension for it to be read correctly.
  tmp <- download_bucket_temp(object_key, ext = ".h5")

  # compile = FALSE: this app only ever calls predict() on these models, so
  # the optimizer/loss/metrics don't need to be reconstructed. Skipping that
  # avoids Keras 3's stricter deserializer choking on legacy metric
  # references (e.g. "keras.metrics.mse") saved by the original Keras 2 /
  # tf.keras training code.
  keras::load_model_hdf5(tmp, compile = FALSE)
}

list_model_keys <- function(prefix, pattern = NULL) {
  keys <- list_bucket_keys(prefix)
  
  if (!is.null(pattern)) {
    keys <- keys[grepl(pattern, basename(keys), ignore.case = TRUE)]
  }
  
  keys
}
