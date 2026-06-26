# bucket_helper.R

library(aws.s3)
library(readr)

bucket_name <- function() {
  Sys.getenv("AWS_S3_BUCKET_NAME")
}

s3_opts <- function() {
  list(
    key = Sys.getenv("AWS_ACCESS_KEY_ID"),
    secret = Sys.getenv("AWS_SECRET_ACCESS_KEY"),
    
    # IMPORTANT:
    # leave region blank so aws.s3 does not build s3-<region>.amazonaws.com
    region = "",
    
    # IMPORTANT:
    # force Railway endpoint directly
    base_url = "storage.railway.app",
    
    use_https = TRUE,
    url_style = Sys.getenv("S3_URL_STYLE", unset = "virtual"),
    check_region = FALSE
  )
}

s3_call <- function(fun, ...) {
  do.call(fun, c(list(...), s3_opts()))
}

bucket_key <- function(...) {
  parts <- unlist(list(...))
  parts <- parts[!is.na(parts) & nzchar(parts)]
  paste(parts, collapse = "/")
}

list_bucket_keys <- function(prefix = "") {
  objs <- s3_call(
    aws.s3::get_bucket,
    bucket = bucket_name(),
    prefix = prefix,
    max = Inf
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
  s3_call(
    aws.s3::s3read_using,
    FUN = readr::read_delim,
    object = object_key,
    bucket = bucket_name(),
    delim = delim,
    show_col_types = FALSE
  )
}

read_bucket_rds <- function(object_key) {
  s3_call(
    aws.s3::s3readRDS,
    object = object_key,
    bucket = bucket_name()
  )
}

write_bucket_rds <- function(x, object_key) {
  s3_call(
    aws.s3::s3saveRDS,
    x = x,
    object = object_key,
    bucket = bucket_name()
  )
}

download_bucket_file <- function(object_key, local_file) {
  s3_call(
    aws.s3::save_object,
    object = object_key,
    bucket = bucket_name(),
    file = local_file
  )
}

upload_bucket_file <- function(local_file, object_key) {
  s3_call(
    aws.s3::put_object,
    file = local_file,
    object = object_key,
    bucket = bucket_name()
  )
}

download_bucket_temp <- function(object_key, ext = "") {
  tmp <- tempfile(fileext = ext)
  
  s3_call(
    aws.s3::save_object,
    object = object_key,
    bucket = bucket_name(),
    file = tmp
  )
  
  tmp
}

read_bucket_model_rds <- function(object_key) {
  read_bucket_rds(object_key)
}

load_bucket_keras_model <- function(object_key) {
  tmp <- download_bucket_temp(object_key, ext = ".keras")
  keras::load_model_tf(tmp)
}

list_model_keys <- function(prefix, pattern = NULL) {
  keys <- list_bucket_keys(prefix)
  
  if (!is.null(pattern)) {
    keys <- keys[grepl(pattern, basename(keys), ignore.case = TRUE)]
  }
  
  keys
}