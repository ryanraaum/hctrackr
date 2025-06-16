
#' Make point tagger function for given list of points
#'
#' Given an sf data frame with a list of points, this will create a function
#' that determines which points a given track intersects. The sf data frame
#' must have an id_column and the required sf "geometry" column and
#' should be in the GPS (4326) coordinate reference system.
#'
#' @param points_sf An sf data frame of points
#' @param id_column The name of the id column
#'
#' @returns A function that takes a GPX track as an argument
#' @export
#'
#' @examples
#' a_point <- sf::st_as_sf(
#'     data.frame(point_id="a_view", lat=42.17, lon=-74.35),
#'     coords=2:3, crs=4326)
#' point_tagger <- make_point_tagger_function(a_point)
make_point_tagger_function <- function(points_sf, id_column="point_id") {
  # package check gets confused by piped variables
  geometry <- point_id <- time <- distance <- time_from_min <- NULL
  # make sure that we know what we are dealing with
  assertthat::assert_that(inherits(points_sf, "sf"),
                          msg="points_sf must be an sf data frame")
  assertthat::assert_that(id_column %in% colnames(points_sf),
                          msg="an id column is required")

  # convert id_column name to "point_id"
  points_sf <- points_sf |> dplyr::rename(point_id = eval(id_column))

  # find the center and maximum distance from center
  # then calculate a distance cutoff for attempting to find point matches
  points_center <- points_sf |>
    dplyr::summarize(geometry = sf::st_combine(geometry)) |>
    sf::st_convex_hull() |>
    sf::st_centroid()
  distance_from_center <- points_sf |>
    sf::st_distance(points_center)
  distance_cutoff <- round(max(distance_from_center) * 1.25)

  # return a function that finds tracks intersecting this specific set of points
  return(function(track) {
    found_points <- NULL
    points <- points_sf
    polygons <- sf::st_buffer(points, dist=100)
    track_sf <- tracktools::gpx_track_to_sf(track)
    if ((nrow(track_sf) > 0) & all(sf::st_distance(track_sf[1,], points_center) < distance_cutoff)) {
      near_points <- sf::st_join(track_sf, polygons, join=sf::st_within, left=FALSE)
      if (nrow(near_points) > 0) {
        found_points <- near_points |>
          dplyr::mutate(distance = apply(sf::st_distance(geometry, points |> dplyr::filter(point_id == point_id)), 1, min)) |>
          dplyr::group_by(point_id) |>
          dplyr::mutate(time_from_min = time - min(time)) |>
          dplyr::filter(time_from_min < 300) |> # just the first time
          dplyr::arrange(distance) |>
          dplyr::slice(1) |>
          dplyr::ungroup() |>
          dplyr::select(point_id, time, distance) |>
          dplyr::arrange(time)
      }
    }
    return(found_points)
  })
}
