

test_that("generate_requirements creates the right number of requirements", {
  for (this_set in names(TESTSETS)) {

    example_target_set <- load_example_target_set(this_set)
    example_requirements <- generate_requirements(example_target_set)
    expected_reqs <- TESTSETS[[this_set]]$nreqs

    # check that number of requirements is greater than or equal to nreqs
    expect_gte(nrow(example_requirements), expected_reqs)
  }

})

test_that("generate_requirements parses the `needed` parameter", {
  for (this_set in names(TESTSETS)) {

    example_target_set <- load_example_target_set(this_set)

    count = 1
    needed = vector("numeric", length(example_target_set$requirements))
    for (req in sort(names(example_target_set$requirements))) {
      this_req <- example_target_set$requirements[[req]]
      if (this_req$needed == "all") {
        needed[count] = -9
      } else {
        needed[count] = as.numeric(this_req$needed)[1]
      }
      count = count + 1
    }
    example_requirements <- generate_requirements(example_target_set)

    for (i in seq_along(needed)) {
      if (needed[i] != -9) {
        this_needed <- example_requirements |>
          dplyr::filter(order == i) |>
          dplyr::pull(needed) |>
          unique()
        expect_equal(length(this_needed), 1)
        expect_equal(this_needed, needed[i])
      }
    }
  }

})


# that is, if the *generalized* requirements need to be set to a
# specific year/season/month when activated, that information is
# properly encoded and identifiable using `requirements_need`
test_that("requirements needs are properly set and identified", {
  for (this_set in names(TESTSETS)) {

    example_target_set <- load_example_target_set(this_set)
    example_requirements <- generate_requirements(example_target_set)

    # check that number of requirements is correct
    needs <- TESTSETS[[this_set]]$needs
    needed <- requirements_need(example_requirements)

    if (is.null(needs)) {
      expect_true(length(needed) == 0)
    } else {
      expect_setequal(needed, needs)
    }
  }

})
