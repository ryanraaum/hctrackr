
SEASON <- c("winter", "spring", "summer", "fall")

# when season or year is negative,
#  that means that when these requirements are "activated"
#. a specific season or year must be selected
generate_requirements_subset <- function(info, targets) {
  # first, enforce limits if present
  if (aidr::this_exists(info$limit_to)) {
    targets <- targets[targets$name %in% info$limit_to,]
  }

  when_df <- tibble::tibble(year = info$when$year,
                            season = info$when$season,
                            month = info$when$month)

  this_subset <- dplyr::cross_join(targets, when_df)

  if (info$needed == "all") {
    this_subset$needed = nrow(this_subset)
  } else if (length(info$needed) == 1 &&
             is.numeric(info$needed) &&
             info$needed <= nrow(this_subset)) {
    this_subset$needed = info$needed
  } else if (length(info$needed) == 1 &&
             stringr::str_detect(info$needed, "^[1-9][0-9]*$")) {
    needed = as.numeric(info$needed)
    if (needed <= nrow(this_subset)) {
      this_subset$needed <- needed
    }
  }

  assertthat::assert_that("needed" %in% names(this_subset))

  return(this_subset)
  # if (info$single == "no") {
  #   # not restricted to a single year or season
  #   if (info$during == "any") {
  #     # doesn't have to happen during a specific season
  #     if (info$every == "no") {
  #       # a standard every target anytime challenge
  #       these_targets <- targets |>
  #         dplyr::mutate(year = 0, season = 0, month = 0)
  #       return(these_targets)
  #     } else if (info$every == "season") {
  #       # a four seasons challenge
  #       this_l <- vector("list", 4)
  #       for (i in seq_along(SEASON)) {
  #         this_l[[i]] <- targets |>
  #           dplyr::mutate(year = 0, season = i, month = 0)
  #       }
  #       these_targets <- dplyr::bind_rows(this_l)
  #       return(these_targets)
  #     } else if (info$every == "month") {
  #       # an every-calendar-month grid
  #       this_l <- vector("list", 12)
  #       for (i in 1:12) {
  #         this_l[[i]] <- targets |>
  #           dplyr::mutate(year = 0, season = 0, month = i)
  #       }
  #       these_targets <- dplyr::bind_rows(this_l)
  #     } else {
  #       stop("unknown `every` option")
  #     }
  #   } else if (info$during %in% c("winter", "spring", "summer", "fall")) {
  #     # a seasonal challenge - like 3500 or 46er winter challenge
  #     if (info$every != "no") {
  #       stop("impossible `every` combination with `during=season`")
  #     }
  #     these_targets <- targets |>
  #       dplyr::mutate(year = 0,
  #                     season = which(info$during == SEASON),
  #                     month = 0)
  #     return(these_targets)
  #   } else {
  #     stop(glue::glue("unknown `during` value `{info$during}`"))
  #   }
  # } else if (info$single == "yes") {
  #   if (info$during %in% c("any", "year")) {
  #     # this is a calendar year challenge
  #     if (info$every == "no") {
  #       # a simple single-year challenge
  #       these_targets <- targets |>
  #         dplyr::mutate(year = -9, season = 0, month = 0)
  #       return(these_targets)
  #     } else if (info$every == "season") {
  #       # a single-year four seasons challenge
  #       this_l <- vector("list", 4)
  #       for (i in seq_along(SEASON)) {
  #         this_l[[i]] <- targets |>
  #           dplyr::mutate(year = -9, season = i, month = 0)
  #       }
  #       these_targets <- dplyr::bind_rows(this_l)
  #       return(these_targets)
  #     } else if (info$every == "month") {
  #       # a single-year every-calendar-month grid
  #       this_l <- vector("list", 12)
  #       for (i in 1:12) {
  #         this_l[[i]] <- targets |>
  #           dplyr::mutate(year = -9, season = 0, month = i)
  #       }
  #       these_targets <- dplyr::bind_rows(this_l)
  #     } else {
  #       stop("unknown `every` option")
  #     }
  #   } else if (info$during %in% c("winter", "spring", "summer", "fall")) {
  #     # this is a single season challenge - like HA single summer
  #     if (info$every != "no") {
  #       stop("impossible `every` combination with single `during=season`")
  #     }
  #     these_targets <- targets |>
  #       dplyr::mutate(year = -9,
  #                     season = which(info$during == SEASON),
  #                     month = 0)
  #     return(these_targets)
  #   } else {
  #     stop(glue::glue("unknown `during` value `{info$during}`"))
  #   }
  #
  # } else {
  #   stop("unknown `single` setting")
  # }
}

generate_requirements <- function(target_set) {
  # target_set has a `requirements` sub-list
  # the relative order of target completions is based on the sorting
  # of the names of the `requirements` sub-list entries
  subset_names <- sort(names(target_set$requirements))
  nsubsets <- length(subset_names)
  subset_requirements <- list()

  for (i in seq_len(nsubsets)) {
    sub_name <- subset_names[i]
    these_reqs <- generate_requirements_subset(target_set$requirements[[sub_name]],
                                               target_set$targets)
    these_reqs$order <- i
    subset_requirements[[sub_name]] <- these_reqs
  }

  dplyr::bind_rows(subset_requirements)
}

requirements_need <- function(reqs) {
  needed <- c("year"=FALSE, "month"=FALSE, "season"=FALSE)
  if (any(reqs$year < 0)) {
    needed["year"] <- TRUE
  }
  if (any(reqs$month < 0)) {
    needed["month"] <- TRUE
  }
  if (any(reqs$season < 0)) {
    needed["season"] <- TRUE
  }
  names(which(needed))
}
