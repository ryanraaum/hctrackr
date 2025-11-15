
test_common_metadata <- function(this_target_set) {
  expect_true(is.character(this_target_set$set_name))
  expect_true(inherits(this_target_set$set_version, "Date"))
  expect_true(inherits(this_target_set$requirements, "list"))
  expect_true(length(this_target_set$requirements) >= 1)
  for (req in names(this_target_set$requirements)) {
    expect_false(is.null(this_target_set$requirements[[req]]$when$year))
    expect_false(is.null(this_target_set$requirements[[req]]$when$season))
    expect_false(is.null(this_target_set$requirements[[req]]$when$month))
  }
  if (aidr::this_exists(this_target_set$note)) {
    expect_true(is.character(this_target_set$note))
  }
}

test_common_targets_data <- function(this_targets) {
  expect_true(inherits(this_targets, "sf"))
  expect_equal(sf::st_crs(this_targets)$epsg, 4326)
}

test_that("basic loading of test sets works", {
  for (this_set in names(TESTSETS)) {
    example_target_set <- load_example_target_set(this_set)

    # check metadata
    test_common_metadata(example_target_set)

    # check points
    test_common_targets_data(example_target_set$targets)
  }
})

test_that("validate limit_to fails when it should", {
  # the normal example sets test the proper passing of the
  # limit_to validation, but do not test failure, so
  # do that here

  # name in `limit_to` is not present in the set targets name column
  min_test <- list(requirements=list("first" = list(limit_to = "monkey")),
                   targets = tibble::tibble(name=c("horse", "cow")))

  expect_error(validate_limits(min_test))
})



test_that("can make working point tagger from loaded point set", {
  example_targets <- system.file("extdata", "diverse_targets.csv",
                                package = "hctrackr")
  example_targets_info <- system.file("extdata", "diverse_targets.yml",
                                     package = "hctrackr")
  example_target_set <- expect_no_error(load_target_set(example_targets,
                                                      example_targets_info))

  kaaterskill <- tracktools::kaaterskill
  huckleberry <- tracktools::huckleberry

  target_tagger <- expect_no_error(
    make_target_tagger_function(example_target_set$targets, "name"))
  expect_true(is.function(target_tagger))

  # find a point that is in the set and on the track
  kaaterskill_peaks <- expect_no_error(target_tagger(kaaterskill$tracks[[1]]))
  expect_true(nrow(kaaterskill_peaks) == 1)
  expect_true("Kaaterskill High Peak" %in% kaaterskill_peaks$target_id)

  # don't find a point that is not in the set
  huckleberry_peaks <- expect_no_error(target_tagger(huckleberry$tracks[[1]]))
  expect_true(is.null(huckleberry_peaks))

})
