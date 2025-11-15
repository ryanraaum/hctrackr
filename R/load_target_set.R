
validate_limits <- function(tset) {
  for (req in names(tset$requirements)) {
    if (!is.null(tset$requirements[[req]]$limit_to)) {
      limit_to <- tset$requirements[[req]]$limit_to
      assertthat::assert_that(is.character(limit_to))
      for (name in limit_to) {
        assertthat::assert_that(name %in% tset$targets$name)
      }
    }
  }
}

validate_needed <- function(tset) {
  for (req in names(tset$requirements)) {
    if (tset$requirement[[req]]$needed != "all") {
      assertthat::assert_that(
        stringr::str_detect(tset$requirement[[req]]$needed, "^[0-9]+$"))
      n_needed <- as.numeric(tset$requirement[[req]]$needed)
      n_max <- nrow(tset$targets)
      if (!is.null(tset$requirements[[req]]$limit_to)) {
        n_max <- length(tset$requirements[[req]]$limit_to)
      }
      assertthat::assert_that(n_needed <= n_max)
    }
  }
}

VALID_WHEN_BITS <- c("any", "every", "single", "year", "season", "month", SEASON)

parse_when <- function(whenval) {
  whenbits <- paste(whenval, collapse = " ") |>
    stringr::str_to_lower() |>
    stringr::str_split_1("\\W+")
  for (bit in whenbits) {
    assertthat::assert_that(bit %in% VALID_WHEN_BITS)
  }
  parsed_when <- list(
    year = 0,
    month = 0,
    season = 0
  )
  if (setequal(whenbits, "any")) {
    TRUE
  } else if (setequal(whenbits, "season") |
             setequal(whenbits, c("any", "season"))) {
    parsed_when$season <- -9
  } else if (setequal(whenbits, "winter") |
             setequal(whenbits, c("any", "winter"))) {
    parsed_when$season <- which(SEASON == "winter")
  } else if (setequal(whenbits, "spring") |
             setequal(whenbits, c("any", "spring"))) {
    parsed_when$season <- which(SEASON == "spring")
  } else if (setequal(whenbits, "summer") |
             setequal(whenbits, c("any", "summer"))) {
    parsed_when$season <- which(SEASON == "summer")
  } else if (setequal(whenbits, "fall") |
             setequal(whenbits, c("any", "fall"))) {
    parsed_when$season <- which(SEASON == "fall")
  } else if (setequal(whenbits, c("single", "year"))) {
    parsed_when$year <- -9
  } else if (setequal(whenbits, c("single", "season"))) {
    parsed_when$season <- -9
    parsed_when$year <- -9
  } else if (setequal(whenbits, c("single", "winter"))) {
    parsed_when$season <- which(SEASON == "winter")
    parsed_when$year <- -9
  } else if (setequal(whenbits, c("single", "spring"))) {
    parsed_when$season <- which(SEASON == "spring")
    parsed_when$year <- -9
  } else if (setequal(whenbits, c("single", "summer"))) {
    parsed_when$season <- which(SEASON == "summer")
    parsed_when$year <- -9
  } else if (setequal(whenbits, c("single", "fall"))) {
    parsed_when$season <- which(SEASON == "fall")
    parsed_when$year <- -9
  } else if (setequal(whenbits, c("every", "season"))) {
    parsed_when$season <- 1:4
  } else if (setequal(whenbits, c("every", "month"))) {
    parsed_when$season <- 1:12
  } else if (setequal(whenbits, c("single", "every", "season"))) {
    parsed_when$season <- 1:4
    parsed_when$year <- -9
  } else if (setequal(whenbits, c("single", "every", "month"))) {
    parsed_when$season <- 1:12
    parsed_when$year <- -9
  } else {
    parsed_when <- list(
      year = -9,
      month = -9,
      season = -9
    )
  }
  return(parsed_when)
}

validate_when <- function(tset) {
  for (req in names(tset$requirements)) {
    this_when <- tset$requirements[[req]]$when
    assertthat::assert_that(
      tryCatch(!all(this_when == -9), error = function(e) return(TRUE)),
      msg = "could not parse `when`"
    )
  }
}


load_target_set <- function(targets_fn, # csv
                            target_set_options,
                            validate=TRUE) {
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

  assertthat::assert_that("set_version" %in% names(target_set_options),
                          msg="`set_version` is a required option")

  targets <- utils::read.csv(targets_fn)
  targets_columns <- colnames(targets)
  assertthat::assert_that("name" %in% targets_columns)
  assertthat::assert_that("longitude" %in% targets_columns)
  assertthat::assert_that("latitude" %in% targets_columns)
  targets <- sf::st_as_sf(targets,
                         coords = c("longitude", "latitude"),
                         crs = sf::st_crs(4326))

  target_set <- target_set_options

  if (aidr::this_exists(target_set$set_version)) {
    target_set$set_version <- as.Date(target_set$set_version)
  }

  if (is.null(target_set$requirements)) {
    target_set$requirements <- list(a_core = list(
      when = aidr::this_or_default_value(target_set$when, "any"),
      #during = aidr::this_or_default_value(target_set$during, "any"),
      #single = aidr::this_or_default_value(target_set$single, "no"),
      #every = aidr::this_or_default_value(target_set$every, "no"),
      needed = aidr::this_or_default_value(target_set$needed, "all")
    ))
    target_set$when <- NULL
    #target_set$during <- NULL
    #target_set$single <- NULL
    #target_set$every <- NULL
    target_set$needed <- NULL
  } else (
    for (req in names(target_set$requirements)) {
      target_set$requirements[[req]]$when =
        aidr::this_or_default_value(target_set$requirements[[req]]$when, "any")

      # target_set$requirements[[req]]$during =
      #   aidr::this_or_default_value(target_set$requirements[[req]]$during, "any")
      # target_set$requirements[[req]]$single =
      #   aidr::this_or_default_value(target_set$requirements[[req]]$single, "no")
      # target_set$requirements[[req]]$every =
      #   aidr::this_or_default_value(target_set$requirements[[req]]$every, "no")
      target_set$requirements[[req]]$needed =
        aidr::this_or_default_value(target_set$requirements[[req]]$needed, "all")
    }
  )

  for (req in names(target_set$requirements)) {
    target_set$requirements[[req]]$when <- parse_when(target_set$requirements[[req]]$when)
  }

  target_set$targets <- targets

  if (validate) {
    # do core validations
    validate_limits(target_set)
    validate_needed(target_set)
    validate_when(target_set)
  }

  return(target_set)
}


