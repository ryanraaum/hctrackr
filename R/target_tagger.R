
#' Make tagger function for given list of target points
#'
#' Given an sf data frame with a list of targets, this will create a
#' function that determines which a given track intersects. The sf data frame
#' must have an id_column and the required sf "geometry" column and should be in
#' the GPS (4326) coordinate reference system.
#'
#' @param targets_sf An sf data frame of target points
#' @param id_column The name of the id column
#'
#' @returns A function that takes a GPX track as an argument
#' @export
#'
#' @examples
#' a_target <- sf::st_as_sf(
#'     data.frame(target_id="a_view", lat=42.17, lon=-74.35),
#'     coords=2:3, crs=4326)
#' target_tagger <- make_target_tagger_function(a_target)
make_target_tagger_function <- function(targets_sf, id_column="target_id") {
  # package check gets confused by piped variables
  geometry <- target_id <- time <- distance <- time_from_min <- NULL
  # make sure that we know what we are dealing with
  assertthat::assert_that(inherits(targets_sf, "sf"),
                          msg="`targets_sf` must be an sf data frame")
  assertthat::assert_that(id_column %in% colnames(targets_sf),
                          msg="an `id_column` is required")

  # convert id_column name to "target_id"
  targets_sf <- targets_sf |> dplyr::rename(target_id = eval(id_column))

  # find the center and maximum distance from center
  # then calculate a distance cutoff for attempting to find point matches
  points_center <- targets_sf |>
    dplyr::summarize(geometry = sf::st_combine(geometry)) |>
    sf::st_convex_hull() |>
    sf::st_centroid()
  distance_from_center <- targets_sf |>
    sf::st_distance(points_center)
  distance_cutoff <- round(max(distance_from_center) * 1.25)

  # return a function that finds tracks intersecting this specific set of points
  return(function(track) {
    found_points <- NULL
    points <- targets_sf
    polygons <- sf::st_buffer(points, dist=100)
    track_sf <- tracktools::gpx_track_to_sf(track)
    if ((nrow(track_sf) > 0) & all(sf::st_distance(track_sf[1,], points_center) < distance_cutoff)) {
      near_points <- sf::st_join(track_sf, polygons, join=sf::st_within, left=FALSE)
      if (nrow(near_points) > 0) {
        found_points <- near_points |>
          dplyr::mutate(distance = apply(sf::st_distance(geometry, points |> dplyr::filter(target_id == target_id)), 1, min)) |>
          dplyr::group_by(target_id) |>
          dplyr::mutate(time_from_min = time - min(time)) |>
          dplyr::filter(time_from_min < 300) |> # just the first time
          dplyr::arrange(distance) |>
          dplyr::slice(1) |>
          dplyr::ungroup() |>
          dplyr::select(target_id, time, distance) |>
          dplyr::arrange(time)
      }
    }
    return(found_points)
  })
}
