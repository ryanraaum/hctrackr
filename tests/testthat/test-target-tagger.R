
peaks <- tracktools::peaks
kaaterskill <- tracktools::kaaterskill
huckleberry <- tracktools::huckleberry
sherrillnd <- tracktools::sherrillnd

test_that("make_target_tagger_function works", {
  target_tagger <- expect_no_error(make_target_tagger_function(peaks, "peak_id"))
  expect_true(is.function(target_tagger))

  kaaterskill_peaks <- expect_no_error(target_tagger(kaaterskill$tracks[[1]]))
  expect_true(nrow(kaaterskill_peaks) == 1)
  expect_true("Kaaterskill High Peak" %in% kaaterskill_peaks$target_id)

  huckleberry_peaks <- expect_no_error(target_tagger(huckleberry$tracks[[1]]))
  expect_true(is.null(huckleberry_peaks))

  sherrillnd_peaks <- expect_no_error(target_tagger(sherrillnd$tracks[[1]]))
  expect_true(nrow(sherrillnd_peaks) == 2)
  expect_true("Sherrill" %in% sherrillnd_peaks$target_id)
  expect_true("North Dome" %in% sherrillnd_peaks$target_id)
})

