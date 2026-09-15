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
  tmp <- download_bucket_temp(object_key, ext = ".keras")

  # Diagnostic: confirm the download actually produced a usable file before
  # handing it to Keras, since we've seen Keras report "File not found" for
  # a path that download_bucket_temp() claimed to have written.
  print(sprintf(
    "[CNN diagnostic] object_key=%s tmp=%s exists=%s size=%s",
    object_key,
    tmp,
    file.exists(tmp),
    if (file.exists(tmp)) file.size(tmp) else NA
  ))

  # Diagnostic: Keras's "File not found" check runs in Python (via
  # TensorFlow's gfile), not in R, so also check the same path from
  # Python's own point of view. If Python/TensorFlow disagree with R
  # about the file's existence, that confirms a filesystem-visibility
  # mismatch between the R and Python processes rather than a download
  # problem.
  py_exists <- tryCatch(
    reticulate::py_eval(sprintf("__import__('os').path.exists(r'%s')", tmp)),
    error = function(e) paste("ERROR:", e$message)
  )
  tf_exists <- tryCatch(
    reticulate::import("tensorflow")$io$gfile$exists(tmp),
    error = function(e) paste("ERROR:", e$message)
  )
  print(sprintf(
    "[CNN diagnostic] python os.path.exists=%s tf.io.gfile.exists=%s",
    py_exists, tf_exists
  ))

  tryCatch({
    keras::load_model_tf(tmp)
  }, error = function(e) {
    # Diagnostic: dump the actual Python-side traceback instead of just the
    # final summarized error, since R/Python/TF all agree the file exists
    # right before this call, so the real cause must be something other
    # than a literal missing file.
    print("[CNN diagnostic] load_model_tf failed; reticulate::py_last_error():")
    print(tryCatch(
      reticulate::py_last_error(),
      error = function(e2) paste("py_last_error() itself failed:", e2$message)
    ))
    stop(e)
  })
}

list_model_keys <- function(prefix, pattern = NULL) {
  keys <- list_bucket_keys(prefix)
  
  if (!is.null(pattern)) {
    keys <- keys[grepl(pattern, basename(keys), ignore.case = TRUE)]
  }
  
  keys
}
