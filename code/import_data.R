library(here)
library(readr)
library(tidyverse)
library(arrow)


## import data
sr_data <- read_csv(here("data", "example_sr_data.csv"))
screen_data <- read_parquet(here("data", "example_screen_data.parquet"))

## plot data 

## plot state on y axis
bad_fig_1 <- 
    screen_data |> 
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
    screen_data |> 
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



