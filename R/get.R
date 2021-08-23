#' Get OSM data associated with a particular type of infrastructure
#'
#' @param place Description of the geographical area that should be matched
#'   with a .osm.pbf file
#' @param infra_type Type of infrastructure, e.g. cycle_infrastructure
#' @param query A character string representing an SQL query to send to
#'   `oe_get()`
#' @param extra_tags Which additional columns, corresponding to OSM tags,
#'   should be in the resulting dataset?
#' @param ... Arguments passed to `oe_get()`
#' @import sf
#' @export
#' @examples
#' highways = oi_get("isle of wight", infra_type = "highway")
#' cycleways = oi_get("isle of wight", infra_type = "cycle_infrastructure")
#' plot(cycleways)
#' cycleways_minimal = oi_get("isle of wight", infra_type = "cycle_infrastructure_minimal")
oi_get = function(place, infra_type = NULL, query = NULL, extra_tags = et, ...) {
  if(is.null(infra_type) && is.null(query)) {
    stop("No infra_type or query provided")
  }
  if(is.null(query)) {
    query = oi_query(infra_type)
  }
  osmextract::oe_get(place, query = query, extra_tags = et, ...)
}

oi_query = function(infra_type) {
  preset_queries[[infra_type]]
}

# preset_extra_tags = c(
et = c(
  "maxspeed",
  "ref",
  "bicycle",
  "cycleway",
  "lanes",
  "oneway",
  "footway",
  "access",
  "sidewalk_left_bicycle",
  "cycleway_left",
  "cycleway_right",
  "oneway_bicycle",
  "sidewalk_right_bicycle",
  "cycleway_both"
)

# could be a data object
preset_queries = list(
  highway = "select * from 'lines' where highway is not null",
  cycle_infrastructure_minimal = "select * from 'lines' where (sidewalk_left_bicycle='yes') or
 (cycleway='shared_busway') or
 (cycleway='opposite_lane') or
 (highway='bridleway' and bicycle='no') or
 (bicycle='use_sidepath') or
 (highway='path') or
 (highway='path' and (bicycle='designated' or bicycle='official')) or
 (highway='pedestrian' and (bicycle='yes' or bicycle='official')) or
 (highway='footway') or
 (highway='cycleway') or
 (cycleway in ('lane', 'opposite_lane', 'shared_busway', 'track', 'opposite_track')) or
 (cycleway='lane')
 ",
  cycle_infrastructure = "select * from 'lines' where (sidewalk_left_bicycle='yes') or
 (cycleway_left='shared_lane') or
 (cycleway_left='shared_busway') or
 (cycleway_right='shared_busway') or
 (cycleway='shared_busway') or
 (cycleway='opposite_lane') or
 (highway='bridleway' and bicycle='no') or
 (bicycle='use_sidepath') or
 (cycleway='opposite' and oneway_bicycle='no') or
 (sidewalk_right_bicycle='yes') or
 (cycleway_right='shared_lane') or
 (cycleway_left='track') or
 (cycleway_right='track') or
 (highway='path') or
 (highway='path' and (bicycle='designated' or bicycle='official')) or
 (highway='pedestrian' and (bicycle='yes' or bicycle='official')) or
 (highway='footway') or
 (highway='cycleway') or
 (cycleway in ('lane', 'opposite_lane', 'shared_busway', 'track', 'opposite_track')) or
 (cycleway_left in ('lane', 'shared_busway')) or
 (cycleway_right in ('lane', 'shared_busway')) or
 (cycleway_both='lane') or
 (cycleway='lane')
 ",
  # traffic-free cycle routes
  cycle_traffic_free = "select * from 'lines' where
 (highway='cycleway')
 ",
  # cycle lanes (on-road)
  cycle_lane = "select * from 'lines' where
 (cycleway='opposite' and oneway_bicycle='no') or
 (cycleway_left='track') or
 (cycleway_right='track') or
 (cycleway_both='track') or
 (cycleway in ('lane', 'opposite_lane', 'track', 'opposite_track')) or
 (cycleway_left='lane') or
 (cycleway_right='lane') or
 (cycleway_both='lane')
 ",
  # pedestrian paths with cycling (excluding some unpaved paths)
  cycle_path = "select * from 'lines' where
 (sidewalk_left_bicycle='yes') or
 (sidewalk_right_bicycle='yes') or
 (highway='path' and (bicycle in ('yes', 'official', 'designated')) and surface<>'unpaved') or
 (highway='pedestrian' and (bicycle in ('yes', 'official', 'designated'))) or
 (highway='footway' and (bicycle in ('yes', 'official', 'designated'))) or
 (bicycle='use_sidepath')
",
  # cycle routes (can follow roads)
  cycle_route = "select * from 'lines' where
(route='bicycle') or
(lcn='yes') or
(rcn='yes') or
(ncn='yes') or
(network='lcn') or
(network='rcn') or
(network='ncn')
",
  # other cycle infrastructure
  cycle_other = "select * from 'lines' where
 (cycleway_left='shared_lane') or
 (cycleway_right='shared_lane') or
 (cycleway_left='shared_busway') or
 (cycleway_right='shared_busway') or
 (cycleway='shared_busway') or
 (cycleway='shared_lane') or
 (bicycle='designated')
"
  )
