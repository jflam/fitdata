
# Read data

path <- "data/2015-12-23-14-32-19_data.csv"
raw_data <- read.csv(path)

# Column definitions from the original .fit file, renamed to be usable from R

column_names <- c(
"file_id_serial_number",
"file_id_time_created",
"file_id_manufacturer",
"file_id_garmin_product",
"file_id_type",
"file_creator_software_version",
"event_timestamp",
"event_timer_trigger",
"event_event",
"event_event_type",
"event_event_group",
"device_info_timestamp",
"device_info_serial_number",
"device_info_manufacturer",
"device_info_garmin_product",
"device_info_software_version",
"device_info_device_index",
"device_info_source_type",
"device_info_device_type",
"device_info_cum_operating_time",
"device_info_unknown",
"device_info_product",
"device_info_antplus_device_type",
"device_info_hardware_version",
"device_info_ant_network",
"device_info_battery_voltage",
"device_info_battery_status",
"unknown_unknown",
"device_settings_utc_offset",
"device_settings_time_offset",
"device_settings_active_time_zone",
"device_settings_unknown",
"device_settings_time_zone_offset",
"user_profile_friendly_name",
"user_profile_weight",
"user_profile_gender",
"user_profile_age",
"user_profile_height",
"user_profile_language",
"user_profile_elev_setting",
"user_profile_weight_setting",
"user_profile_resting_heart_rate",
"user_profile_default_max_biking_heart_rate",
"user_profile_default_max_heart_rate",
"user_profile_hr_setting",
"user_profile_speed_setting",
"user_profile_dist_setting",
"user_profile_power_setting",
"user_profile_activity_class",
"user_profile_position_setting",
"user_profile_temperature_setting",
"user_profile_unknown",
"zones_target_functional_threshold_power",
"zones_target_max_heart_rate",
"zones_target_hr_calc_type",
"zones_target_pwr_calc_type",
"zones_target_unknown",
"record_timestamp",
"record_position_lat",
"record_position_long",
"record_distance",
"record_accumulated_power",
"record_altitude",
"record_speed",
"record_power",
"record_unknown",
"record_heart_rate",
"record_cadence",
"record_temperature",
"record_left_right_balance",
"record_fractional_cadence",
"record_enhanced_altitude",
"record_enhanced_speed",
"hrv_time",
"event_data",
"lap_timestamp",
"lap_start_time",
"lap_start_position_lat",
"lap_start_position_long",
"lap_end_position_lat",
"lap_end_position_long",
"lap_total_elapsed_time",
"lap_total_timer_time",
"lap_total_distance",
"lap_total_cycles",
"lap_unknown",
"lap_total_work",
"lap_time_in_hr_zone",
"lap_time_in_power_zone",
"lap_message_index",
"lap_total_calories",
"lap_total_fat_calories",
"lap_avg_speed",
"lap_max_speed",
"lap_avg_power",
"lap_max_power",
"lap_total_ascent",
"lap_total_descent",
"lap_normalized_power",
"lap_left_right_balance",
"lap_event",
"lap_event_type",
"lap_avg_heart_rate",
"lap_max_heart_rate",
"lap_avg_cadence",
"lap_max_cadence",
"lap_lap_trigger",
"lap_sport",
"lap_sub_sport",
"lap_avg_fractional_cadence",
"lap_max_fractional_cadence",
"lap_enhanced_avg_speed",
"lap_enhanced_max_speed",
"session_timestamp",
"session_start_time",
"session_start_position_lat",
"session_start_position_long",
"session_total_elapsed_time",
"session_total_timer_time",
"session_total_distance",
"session_total_cycles",
"session_nec_lat",
"session_nec_long",
"session_swc_lat",
"session_swc_long",
"session_total_work",
"session_time_in_hr_zone",
"session_time_in_power_zone",
"session_unknown",
"session_message_index",
"session_total_calories",
"session_total_fat_calories",
"session_avg_speed",
"session_max_speed",
"session_avg_power",
"session_max_power",
"session_total_ascent",
"session_total_descent",
"session_first_lap_index",
"session_num_laps",
"session_normalized_power",
"session_training_stress_score",
"session_intensity_factor",
"session_left_right_balance",
"session_threshold_power",
"session_event",
"session_event_type",
"session_sport",
"session_sub_sport",
"session_avg_heart_rate",
"session_max_heart_rate",
"session_avg_cadence",
"session_max_cadence",
"session_trigger",
"session_avg_fractional_cadence",
"session_max_fractional_cadence",
"session_enhanced_avg_speed",
"session_enhanced_max_speed",
"activity_timestamp",
"activity_total_timer_time",
"activity_local_timestamp",
"activity_num_sessions",
"activity_type",
"activity_event",
"activity_event_type"
)

# Assign column names

names(raw_data) <- column_names

# Extract the route

library(dplyr)

# Constant

SEMICIRCLES_TO_DEGREES <- (180 / 2 ^ 31);

route <-
raw_data %>%
select(record_position_lat, record_position_long) %>%
filter(!is.na(record_position_lat) & !is.na(record_position_long)) %>%
mutate(lat = record_position_lat * SEMICIRCLES_TO_DEGREES,
       lon = record_position_long * SEMICIRCLES_TO_DEGREES) %>%
select(lat, lon)

# Now plot this on a map

library(leaflet)

library(sp)
library(maptools)

# See https://rpubs.com/walkerke/points_to_line
# TODO: Rewrite this

points_to_line <- function(data, long, lat, id_field = NULL, sort_field = NULL) {

    # Convert to SpatialPointsDataFrame
    coordinates(data) <- c(long, lat)

    # If there is a sort field...
    if (!is.null(sort_field)) {
        if (!is.null(id_field)) {
            data <- data[order(data[[id_field]], data[[sort_field]]),]
        } else {
            data <- data[order(data[[sort_field]]),]
        }
    }

    # If there is only one path...
    if (is.null(id_field)) {

        lines <- SpatialLines(list(Lines(list(Line(data)), "id")))

        return(lines)

        # Now, if we have multiple lines...
    } else if (!is.null(id_field)) {

        # Split into a list by ID field
        paths <- sp::split(data, data[[id_field]])

        sp_lines <- SpatialLines(list(Lines(list(Line(paths[[1]])), "line1")))

        # I like for loops, what can I say...
        for (p in 2:length(paths)) {
            id <- paste0("line", as.character(p))
            l <- SpatialLines(list(Lines(list(Line(paths[[p]])), id)))
            sp_lines <- spRbind(sp_lines, l)
        }

        return(sp_lines)
    }
}

lls <- points_to_line(route, "lon", "lat")

leaflet(lls) %>% addTiles() %>% addPolylines()

# Now let's compute some data

performance <-
raw_data %>%
select(record_distance, 
       record_altitude,
       record_speed,
       record_power,
       record_heart_rate,
       record_cadence,
       record_temperature) %>%
filter(!is.na(record_distance))

average_power <- mean(performance$record_power)

# Plot a histogram of power

library(ggvis)

performance %>%
    ggvis( ~ record_power ) %>%
    layer_histograms(width = input_slider(1, 10, step = 1, label = "Bin Width (W)"),
                   center = 35,
                   fill := "#E74C3C") %>%
    add_axis("x", title = "Power (W)") %>%
    add_axis("y", title = "Count")

# Plot a time series of HR, power, elevation vs. distance - the typical Strava analysis
# TODO: fix the CSS 

library(dygraphs)

dygraph(performance, main = "Analysis") %>%
  dyAxis("x", drawGrid = FALSE, label = "Distance (m)") %>%
  dyRangeSelector()
