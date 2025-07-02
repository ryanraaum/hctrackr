
test_that("target set can be loaded", {
  example_targets <- system.file("extdata", "diverse_targets.csv",
                                package = "hctrackr")
  example_targets_info <- system.file("extdata", "diverse_targets.yml",
                                     package = "hctrackr")
  example_target_set <- expect_no_error(load_target_set(example_targets,
                                                      example_targets_info))

  # check metadata
  expect_equal(example_target_set$set_name, "Diverse Targets Challenge")
  expect_true(isa(example_target_set$date_start, "Date"))
  if (aidr::this_exists(example_target_set$date_end)) {
    expect_true(inherits(example_target_set$date_end, "Date"))
  }

  # check points
  expect_true(inherits(example_target_set$targets, "sf"))
  expect_equal(nrow(example_target_set$targets), 5)
  expect_equal(sf::st_crs(example_target_set$targets)$epsg, 4326)
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
