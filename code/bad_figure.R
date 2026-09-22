## Imports ----
library(here)
library(readr)
library(tidyverse)
library(arrow)
library(lubridate)

source(here("code", "import_data.R"))

## plot data 

## plot state on y axis
bad_fig_1 <- 
    screen_df |> 
    filter(!is.na(state)) |> 
    ggplot() + 
    geom_segment(
        aes(x = record_time, 
            xend = end_time,
            y = state, 
            yend = state, 
            color = state
        ), 
        linewidth = 10
    )
bad_fig_1

ggsave(here("figures", "bad_fig_1.png"), bad_fig_1, width = 10, height = 5)


## plot user_id on y axis and use color to differentiate state
bad_fig_2 <- 
    screen_df |> 
    filter(!is.na(state)) |> 
    ggplot() + 
    geom_segment(
        aes(x = record_time,
            xend = end_time,
            y = user_id,
            yend = user_id,
            color = state
        ),
        linewidth = 10
    )
bad_fig_2

ggsave(here("figures", "bad_fig_2.png"), bad_fig_2, width = 10, height = 5)


## fix labels for easier reading
bad_fig_3 <- 
    bad_fig_2 +
    labs(
        y = "",
        x = "Date"
    ) +
    theme(
        axis.text.y = element_blank()
    )

bad_fig_3


bad_fig_4 <- 
    screen_df |> 
    filter(!is.na(state)) |> 
    ggplot() + 
    geom_segment(
        aes(x = hours_start,
            xend = hours_end,
            y = user_id,
            yend = user_id,
            color = state
        ), 
        alpha = 0.6,
        linewidth = 10
    ) + 
    scale_color_manual(values = c("cornflowerblue", "lightgrey"), 
                       labels = c("OFF", "ON")) + 
    facet_wrap(~day_window, ncol = 1) + 
    labs(
        y = "", 
        x = "Hour of day", 
        color = "Screen State"
    ) + 
    scale_x_continuous(
        breaks = seq(0, 23, by = 1),
        labels = function(x) sprintf("%02d", (16 + x) %% 24),
        expand = c(0, 0)
    ) + 
    theme(
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank()
    )
bad_fig_4

sr_df |> 
    filter(data_source == "avicenna") |> 
    filter(!is.na(sleep_onset)) |> 
    ggplot() +
    geom_segment(
        aes(x = sleep_onset,
            xend = sleep_offset,
            y = user_id,
            yend = user_id
        )
    )


sr_df |> 
    filter(data_source == "avicenna") |> 
    filter(!is.na(sleep_onset)) |> 
    ggplot() +
    geom_segment(
        aes(x = sleep_onset,
            xend = sleep_offset,
            y = user_id,
            yend = user_id
        )
    ) + 
    facet_wrap(~day_window, ncol = 1)

sr_df |> 
    filter(data_source == "avicenna") |> 
    filter(!is.na(sleep_onset)) |> 
    ggplot() +
    geom_segment(
        aes(x = hours_sleep_onset,
            xend = hours_sleep_offset,
            y = user_id,
            yend = user_id
        )
    ) + 
    facet_wrap(~day_window, ncol = 1) + 
    scale_x_continuous(
        limits = c(0, 24), 
        breaks = seq(0, 24, by = 1),
        labels = function(x) sprintf("%02d", (16 + x) %% 24),
        expand = c(0, 0)
    ) 

bad_figure_5 <- 
    sr_df |> 
    filter(data_source == "avicenna") |> 
    filter(!is.na(sleep_onset)) |> 
    ggplot() +
    geom_segment(
        aes(x = hours_sleep_onset,
            xend = hours_sleep_offset,
            y = day_window,
            yend = day_window
        ), 
        linewidth = 5
    ) + 
    scale_y_continuous(
        breaks = 1:8,
        labels = function(x) sprintf("Day %d", x)
    ) +
    scale_x_continuous(
        limits = c(0, 24), 
        breaks = seq(0, 24, by = 1),
        labels = function(x) sprintf("%02d", (16 + x) %% 24),
        expand = c(0, 0)
    ) 
bad_figure_5

ggsave(here("figures", "bad_figure_5.png"), bad_figure_5, width = 10, height = 5)

bad_figure_6 <- 
    sr_df |> 
    filter(data_source == "avicenna") |> 
    filter(!is.na(sleep_onset)) |> 
    ggplot() +
    geom_segment(
        aes(x = day_window,
            xend = day_window,
            y = hours_sleep_onset,
            yend = hours_sleep_offset
        ), 
        linewidth = 5
    ) + 
    scale_x_continuous(
        breaks = 1:8,
        labels = function(x) sprintf("Day %d", x)
    ) +
    scale_y_continuous(
        limits = c(0, 24), 
        breaks = seq(0, 24, by = 1),
        labels = function(x) sprintf("%02d", (16 + x) %% 24),
        expand = c(0, 0)
    ) 
bad_figure_6

ggsave(here("figures", "bad_figure_6.png"), bad_figure_6, width = 8, height = 5)


### complex graph
opener <- 
    jerk_df |> 
    ggplot(aes(
        x = bin_time, 
        y = jerk_mag
    )) + 
    geom_rect(
        data = screen_df,
        aes(
            xmin = record_time,
            xmax = end_time,
            ymin = -Inf,
            ymax = Inf,
            fill = state
        ),
        inherit.aes = FALSE, 
        alpha = 0.5
    ) + 
    scale_fill_manual(
        name = "Screen state",
        values = c("TRUE" = "red", "FALSE" = "cornflowerblue"),
        labels = c("TRUE" = "Screen on", "FALSE" = "Screen off")
    ) +
    geom_line(
        color = "black"
    ) + 
    labs(
        x = "day", 
        y = "jerk magnitude (m/s^3)"
    ) + 
    theme(
        legend.position = "top"
    )
opener
ggsave(here("figures", "opener.png"), opener, width = 9, height = 3)

opener_better <- 
    jerk_df |> 
    ggplot(aes(
        x = hours_since_16, 
        y = jerk_mag
    )) + 
    geom_rect(
        data = screen_df,
        aes(
            xmin = hours_start,
            xmax = hours_end,
            ymin = -Inf,
            ymax = Inf,
            fill = state
        ),
        inherit.aes = FALSE, 
        alpha = 0.5
    ) + 
    scale_fill_manual(
        name = "Screen state",
        values = c("TRUE" = "red", "FALSE" = "cornflowerblue"),
        labels = c("TRUE" = "Screen on", "FALSE" = "Screen off")
    ) +
    geom_line(
        color = "black"
    ) + 
    scale_x_continuous(
        breaks = seq(0, 23, by = 1),
        labels = function(x) sprintf("%02d", (16 + x) %% 24),
        expand = c(0, 0)
    ) +
    labs(
        x = "hour of day", 
        y = "jerk magnitude (m/s^3)"
    ) + 
    facet_wrap(~day_window, ncol = 2, 
               labeller = labeller(day_window = function(x) paste("Day", x))) + 
    theme(
        legend.position = "top"
    )
ggsave(here("figures", "opener_better.png"), opener_better, width = 9, height = 5)


opener_color_improved <- 
    jerk_df |> 
    ggplot(aes(
        x = hours_since_16, 
        y = jerk_mag
    )) + 
    geom_rect(
        data = screen_df,
        aes(
            xmin = hours_start,
            xmax = hours_end,
            ymin = -Inf,
            ymax = Inf,
            fill = state
        ),
        inherit.aes = FALSE, 
        alpha = 0.5
    ) + 
    scale_fill_manual(
        name = "Screen state",
        values = c("TRUE" = "grey70", "FALSE" = "cornflowerblue"),
        labels = c("TRUE" = "ON", "FALSE" = "OFF")
    ) +
    geom_line(
        color = "black"
    ) + 
    scale_x_continuous(
        breaks = seq(0, 23, by = 1),
        labels = function(x) sprintf("%02d", (16 + x) %% 24),
        expand = c(0, 0)
    ) +
    labs(
        x = "hour of day", 
        y = "jerk magnitude (m/s^3)"
    ) + 
    facet_wrap(~day_window, ncol = 2, 
               labeller = labeller(day_window = function(x) paste("Day", x))) + 
    theme(
        legend.position = "top", 
        #panel.background = element_rect(fill = "white")
        )
opener_color_improved
ggsave(here("figures", "opener_color_improved.png"), opener_color_improved, width = 9, height = 5)


opener_color_improved <- 
    jerk_df |> 
    ggplot(aes(
        x = hours_since_16, 
        y = jerk_mag
    )) + 
    geom_rect(
        data = screen_df,
        aes(
            xmin = hours_start,
            xmax = hours_end,
            ymin = -Inf,
            ymax = Inf,
            fill = state
        ),
        inherit.aes = FALSE, 
        alpha = 0.5
    ) + 
    scale_fill_manual(
        name = "Screen state",
        values = c("TRUE" = "grey70", "FALSE" = "cornflowerblue"),
        labels = c("TRUE" = "ON", "FALSE" = "OFF")
    ) +
    geom_line(
        color = "black"
    ) + 
    scale_x_continuous(
        breaks = seq(0, 23, by = 1),
        labels = function(x) sprintf("%02d", (16 + x) %% 24),
        expand = c(0, 0)
    ) +
    labs(
        x = "hour of day", 
        y = "jerk magnitude (m/s^3)"
    ) + 
    facet_wrap(~day_window, ncol = 2, 
               labeller = labeller(day_window = function(x) paste("Day", x))) + 
    theme(
        legend.position = "top", 
        #panel.background = element_rect(fill = "white")
        )
opener_color_improved
ggsave(here("figures", "opener_color_improved.png"), opener_color_improved, width = 9, height = 5)

opener_color_background_improved <- 
    jerk_df |> 
    ggplot(aes(
        x = hours_since_16, 
        y = jerk_mag
    )) + 
    geom_rect(
        data = screen_df,
        aes(
            xmin = hours_start,
            xmax = hours_end,
            ymin = -Inf,
            ymax = Inf,
            fill = state
        ),
        inherit.aes = FALSE, 
        alpha = 0.5
    ) + 
    scale_fill_manual(
        name = "Screen state",
        values = c("TRUE" = "grey70", "FALSE" = "cornflowerblue"),
        labels = c("TRUE" = "ON", "FALSE" = "OFF")
    ) +
    geom_line(
        color = "black"
    ) + 
    scale_x_continuous(
        breaks = seq(0, 23, by = 1),
        labels = function(x) sprintf("%02d", (16 + x) %% 24),
        expand = c(0, 0)
    ) +
    labs(
        x = "hour of day", 
        y = "jerk magnitude (m/s^3)"
    ) + 
    facet_wrap(~day_window, ncol = 2, 
               labeller = labeller(day_window = function(x) paste("Day", x))) + 
    theme(
        legend.position = "top", 
        panel.background = element_rect(fill = "white")
    )
opener_color_background_improved
ggsave(here("figures", "opener_color_background_improved.png"), opener_color_background_improved, width = 9, height = 5)


opener_color_background_axis_improved <- 
    jerk_df |> 
    ggplot(aes(
        x = hours_since_16, 
        y = jerk_mag
    )) + 
    geom_rect(
        data = screen_df,
        aes(
            xmin = hours_start,
            xmax = hours_end,
            ymin = -Inf,
            ymax = Inf,
            fill = state
        ),
        inherit.aes = FALSE, 
        alpha = 0.5
    ) + 
    scale_fill_manual(
        name = "Screen state",
        values = c("TRUE" = "grey70", "FALSE" = "cornflowerblue"),
        labels = c("TRUE" = "ON", "FALSE" = "OFF")
    ) +
    geom_line(
        color = "black"
    ) + 
    scale_x_continuous(
        breaks = seq(0, 23, by = 2),
        labels = function(x) sprintf("%02d", (16 + x) %% 24),
        expand = c(0, 0)
    ) +
    labs(
        x = "hour of day (24 hr clock)", 
        y = "jerk magnitude (m/s^3)"
    ) + 
    facet_wrap(~day_window, ncol = 2, 
               labeller = labeller(day_window = function(x) paste("Day", x))) + 
    theme(
        legend.position = "top", 
        panel.background = element_rect(fill = "white")
    )
opener_color_background_axis_improved
ggsave(here("figures", "opener_color_background_axis_improved.png"), opener_color_background_axis_improved, width = 9, height = 5)


opener_color_background_axis_improved <- 
    jerk_df |> 
    ggplot(aes(
        x = hours_since_16, 
        y = jerk_mag
    )) + 
    geom_rect(
        data = screen_df,
        aes(
            xmin = hours_start,
            xmax = hours_end,
            ymin = -Inf,
            ymax = Inf,
            fill = state
        ),
        inherit.aes = FALSE, 
        alpha = 0.5
    ) + 
    scale_fill_manual(
        name = "Screen state",
        values = c("TRUE" = "grey70", "FALSE" = "cornflowerblue"),
        labels = c("TRUE" = "ON", "FALSE" = "OFF")
    ) +
    geom_line(
        color = "black"
    ) + 
    scale_x_continuous(
        breaks = seq(0, 23, by = 2),
        labels = function(x) sprintf("%02d", (16 + x) %% 24),
        expand = c(0, 0)
    ) +
    labs(
        x = "hour of day (24 hr clock)", 
        y = "jerk magnitude (m/s^3)"
    ) + 
    facet_wrap(~day_window, ncol = 2, 
               labeller = labeller(day_window = function(x) paste("Day", x))) + 
    theme(
        legend.position = "top", 
        panel.background = element_rect(fill = "white")
    )
opener_color_background_axis_improved
ggsave(here("figures", "opener_color_background_axis_improved.png"), opener_color_background_axis_improved, width = 9, height = 5)


opener_jerk_only <- 
    jerk_df |> 
    ggplot(aes(
        x = hours_since_16, 
        y = jerk_mag
    )) +
    geom_line(
        aes(color = "Jerk magnitude")
    ) + 
    scale_color_manual(
        values = c("Jerk magnitude" = "grey30")
    ) +
    scale_x_continuous(
        breaks = seq(0, 23, by = 2),
        labels = function(x) sprintf("%02d", (16 + x) %% 24),
        expand = c(0, 0)
    ) +
    labs(
        x = "hour of day (24 hr clock)", 
        y = "jerk magnitude (m/s^3)", 
        color = NULL
    ) + 
    facet_wrap(~day_window, ncol = 2, 
               labeller = labeller(day_window = function(x) paste("Day", x))) + 
    theme(
        legend.position = "top", 
        panel.background = element_blank()
    )
opener_jerk_only
ggsave(here("figures", "opener_jerk_only.png"), opener_jerk_only, width = 9, height = 5)


opener_screen_only <- 
    ggplot() + 
    geom_rect(
        data = screen_df,
        aes(
            xmin = hours_start,
            xmax = hours_end,
            ymin = 0,
            ymax = 10,
            fill = state
        ),
        inherit.aes = FALSE, 
        alpha = 0.5
    ) + 
    scale_fill_manual(
        values = c("TRUE" = "red", "FALSE" = "cornflowerblue"),
        labels = c("TRUE" = "Screen ON", "FALSE" = "Screen OFF")
    ) +
    scale_x_continuous(
        breaks = seq(0, 23, by = 2),
        labels = function(x) sprintf("%02d", (16 + x) %% 24),
        expand = c(0, 0)
    ) +
    labs(
        x = "hour of day (24 hr clock)", 
        y = "",
        color = NULL, 
        fill = NULL, 
    ) + 
    facet_wrap(~day_window, ncol = 2, 
               labeller = labeller(day_window = function(x) paste("Day", x))) + 
    theme(
        legend.position = "top", 
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank()
    )
opener_screen_only
ggsave(here("figures", "opener_screen_only.png"), opener_screen_only, width = 9, height = 5)


opener_jerk_screen_only <- 
    jerk_df |> 
    ggplot(aes(
        x = hours_since_16, 
        y = jerk_mag
    )) +
    geom_line(
        aes(color = "Jerk magnitude (m/s^3)")
    ) + 
    scale_color_manual(
        values = c("Jerk magnitude (m/s^3)" = "black")
    ) +
    geom_rect(
        data = screen_df,
        aes(
            xmin = hours_start,
            xmax = hours_end,
            ymin = -Inf,
            ymax = Inf,
            fill = state
        ),
        inherit.aes = FALSE, 
        alpha = 0.5
    ) + 
    scale_fill_manual(
        values = c("TRUE" = "red", "FALSE" = "cornflowerblue"),
        labels = c("TRUE" = "Screen ON", "FALSE" = "Screen OFF")
    ) +
    scale_x_continuous(
        breaks = seq(0, 23, by = 2),
        labels = function(x) sprintf("%02d", (16 + x) %% 24),
        expand = c(0, 0)
    ) +
    labs(
        x = "hour of day (24 hr clock)", 
        y = "jerk magnitude (m/s^3)", 
        color = NULL, 
        fill = NULL, 
    ) + 
    facet_wrap(~day_window, ncol = 2, 
               labeller = labeller(day_window = function(x) paste("Day", x))) + 
    theme(
        legend.position = "top"
    )
opener_jerk_screen_only
ggsave(here("figures", "opener_jerk_screen_only.png"), opener_jerk_screen_only, width = 9, height = 5)


opener_graph_improved <- 
    jerk_df |> 
    ggplot(aes(
        x = hours_since_16, 
        y = jerk_mag
    )) +
    geom_line(
        aes(color = "Jerk magnitude (m/s^3)")
    ) + 
    scale_color_manual(
        values = c("Jerk magnitude (m/s^3)" = "grey30")
    ) +
    geom_rect(
        data = screen_df,
        aes(
            xmin = hours_start,
            xmax = hours_end,
            ymin = -Inf,
            ymax = Inf,
            fill = state
        ),
        inherit.aes = FALSE, 
        alpha = 0.5
    ) + 
    scale_fill_manual(
        values = c("TRUE" = "grey70", "FALSE" = "cornflowerblue"),
        labels = c("TRUE" = "Screen ON", "FALSE" = "Screen OFF")
    ) +
    scale_x_continuous(
        breaks = seq(0, 23, by = 2),
        labels = function(x) sprintf("%02d", (16 + x) %% 24),
        expand = c(0, 0)
    ) +
    labs(
        x = "hour of day (24 hr clock)", 
        y = "", 
        color = NULL, 
        fill = NULL, 
    ) + 
    facet_wrap(~day_window, ncol = 2, 
               labeller = labeller(day_window = function(x) paste("Day", x))) + 
    theme(
        legend.position = "top", 
        axis.text.y = element_text(color = "white"),
        axis.ticks.y = element_line(color = "white"),
        axis.title.y = element_text(color = "white"), 
        panel.background = element_blank()
    )
opener_graph_improved
ggsave(here("figures", "opener_graph_improved.png"), opener_graph_improved, width = 9, height = 5)

opener_graph_best <- 
    sr_df |> 
    filter(data_source == "avicenna") |>
    filter(!day_window == 9) |>
    ggplot() +
    geom_segment(
        aes(
            x = hours_sleep_onset, 
            xend = hours_sleep_offset, 
            y = day_window, 
            yend = day_window
        ), 
        color = "#674ea7"
    ) + 
    geom_point(
        aes(
            x = hours_sleep_offset,
            y = day_window
        ),
        size = 2, 
        color = "#674ea7"
    ) +
    geom_point(
        aes(
            x = hours_sleep_onset,
            y = day_window
        ),
        size = 2, 
        color = "#674ea7"
    ) +
    scale_x_continuous(
        limits = c(4, 18),
        breaks = seq(4, 18, by = 2),
        labels = c(
            "8pm",
            "10pm",
            "12am",
            "2am",
            "4am",
            "6am",
            "8am", 
            "10am"
        )
    ) +
    scale_y_continuous(
        breaks = 1:8,
        labels = paste("Day", 1:8)
    ) +
    labs(
        x = "", 
        y = "",
    ) + 
    theme(
        legend.position = "top", 
        panel.background = element_blank(),
        panel.grid = element_blank(),
        
        # bottom x-axis
        axis.line.x.bottom = element_line(color = "grey20"),
        
        # left y-axis
        axis.line.y.left = element_line(color = "grey20"),
        
        # remove top/right
        axis.line.x.top = element_blank(),
        axis.line.y.right = element_blank()
    )
opener_graph_best
ggsave(here("figures", "opener_graph_best.png"), opener_graph_best, width = 8, height = 3)


