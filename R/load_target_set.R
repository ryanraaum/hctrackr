
load_target_set <- function(targets_fn, # csv
                            target_set_options) {
  # target_set_options is either
  # 1. a string or path that points to a yaml file with options
  # 2. a list with those options
  if (inherits(target_set_options, "character")) {
    # it should be a path to a yaml file
    target_set_options <- yaml::read_yaml(target_set_options)
  }
  assertthat::assert_that(inherits(target_set_options, "list"))
  assertthat::assert_that("set_name" %in% names(target_set_options),
                          msg="`set_name` is a required option")

  targets <- utils::read.csv(targets_fn)
  targets_columns <- colnames(targets)
  assertthat::assert_that("name" %in% targets_columns)
  assertthat::assert_that("longitude" %in% targets_columns)
  assertthat::assert_that("latitude" %in% targets_columns)
  targets <- sf::st_as_sf(targets,
                         coords = c("longitude", "latitude"),
                         crs = sf::st_crs(4326))

  target_set <- target_set_options

  if (aidr::this_exists(target_set$date_start)) {
    target_set$date_start <- as.Date(target_set$date_start)
  }

  if (aidr::this_exists(target_set$date_end)) {
    target_set$date_end <- as.Date(target_set$date_end)
  }

  target_set$targets <- targets

  return(target_set)
}
