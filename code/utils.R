split_screen_by_day_window <- function(df) {
    day_grid <- df |>
        distinct(user_id, day_window, day_label, start, end)
    
    df |>
        select(user_id, record_time, end_time, state, lock_state, locked_and_off) |>
        inner_join(day_grid, by = "user_id", relationship = "many-to-many") |>
        mutate(
            clipped_start = pmax(record_time, start),
            clipped_end   = pmin(end_time, end)
        ) |>
        filter(clipped_end > clipped_start) |>
        mutate(
            hours_start = as.numeric(difftime(clipped_start, start, units = "hours")),
            hours_end   = as.numeric(difftime(clipped_end, start, units = "hours"))
        )
}