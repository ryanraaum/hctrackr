
TESTSETS <- list(
  "diverse" = list(targets = "diverse_targets.csv",
                   options = "diverse_targets.yml",
                   nreqs = 5,
                   needs = NULL),
  "adk29er" = list(targets = "29er_peaks.csv",
                 options = "29er_peaks.yml",
                 nreqs = 29,
                 needs = NULL),
  "c3500" = list(targets = "c3500_peaks.csv",
                 options = "c3500_peaks.yml",
                 nreqs = 37,
                 needs = NULL),
  "c3500_winter" = list(targets = "c3500_peaks.csv",
                        options = "c3500_winter.yml",
                        nreqs = 33,
                        needs = NULL),
  "ha_4seasons" = list(targets = "ha_peaks.csv",
                       options = "ha_four_seasons.yml",
                       nreqs = 35 * 4,
                       needs = NULL),
  "ha_grid" = list(targets = "ha_peaks.csv",
                   options = "ha_grid.yml",
                   nreqs = 35 * 12,
                   needs = NULL),
  "ha_single_summer" = list(targets = "ha_peaks.csv",
                            options = "ha_single_season_summer.yml",
                            nreqs = 35,
                            needs = "year"),
  "fire_towers" = list(targets = "catskills_fire_tower_challenge.csv",
                            options = "catskills_fire_tower_challenge.yml",
                            nreqs = 6,
                            needs = "year")
)

load_example_target_set <- function(setname, validate=TRUE) {
  example_targets <- system.file("extdata", TESTSETS[[setname]]$targets,
                                 package = "hctrackr")
  example_targets_info <- system.file("extdata", TESTSETS[[setname]]$options,
                                      package = "hctrackr")
  expect_no_error(load_target_set(example_targets, example_targets_info, validate=validate))
}
