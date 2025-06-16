
peaks <- tracktools::peaks
kaaterskill <- tracktools::kaaterskill
huckleberry <- tracktools::huckleberry
sherrillnd <- tracktools::sherrillnd

test_that("make_point_tagger_function works", {
  point_tagger <- expect_no_error(make_point_tagger_function(peaks, "peak_id"))
  expect_true(is.function(point_tagger))

  kaaterskill_peaks <- expect_no_error(point_tagger(kaaterskill$tracks[[1]]))
  expect_true(nrow(kaaterskill_peaks) == 1)
  expect_true("Kaaterskill High Peak" %in% kaaterskill_peaks$point_id)

  huckleberry_peaks <- expect_no_error(point_tagger(huckleberry$tracks[[1]]))
  expect_true(is.null(huckleberry_peaks))

  sherrillnd_peaks <- expect_no_error(point_tagger(sherrillnd$tracks[[1]]))
  expect_true(nrow(sherrillnd_peaks) == 2)
  expect_true("Sherrill" %in% sherrillnd_peaks$point_id)
  expect_true("North Dome" %in% sherrillnd_peaks$point_id)
})
