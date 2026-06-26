#bucket_helper.R

library(aws.s3)
library(readr)

bucket_name <- function() Sys.getenv("BUCKET_NAME")

s3_opts <- function() {
  list(
    key = Sys.getenv("AWS_ACCESS_KEY_ID"),
    secret = Sys.getenv("AWS_SECRET_ACCESS_KEY"),
    region = Sys.getenv("AWS_REGION", unset = ""),
    base_url = sub("^https?://", "", Sys.getenv("AWS_S3_ENDPOINT")),
    url_style = Sys.getenv("S3_URL_STYLE", unset = "virtual"),
    check_region = FALSE
  )
}

s3_call <- function(fun, ...) {
  do.call(fun, c(list(...), s3_opts()))
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