library(dplyr)
library(ggplot2)
library(cowplot)

options(stringsAsFactors = FALSE)

# Function to plot the waves
plotTime <- function(tb, t) {
    tb <- filter(tb, time == t)
    g <- ggplot() +
        geom_ribbon(
            data = tb |> mutate(value = ifelse(value > 0, value, 0)),
            mapping = aes(x = lon, ymin = 0, ymax = value),
            fill = "royalblue"
        ) +
        geom_ribbon(
            data = tb |> mutate(value = ifelse(value < 0, value, 0)),
            mapping = aes(x = lon, ymin = value, ymax = 0),
            fill = "brown"
        ) +
        geom_hline(yintercept = 0) +
        theme_half_open() +
        panel_border()
    return(g)
}

# Reading data
wave_text <- readLines("juliano_out.txt")
num_idx <- gregexpr(" ([0-9]|-)", wave_text)
ntime <- length(num_idx)
nlon <- length(num_idx[[1]]) - 1
wave <- tibble(time = integer(0), lon = integer(0), value = double(0))
mjo_vector <- double(nlon)

# Extracting information
for (i in 1:ntime) {
    num_idx_time <- num_idx[[i]]
    time <- substring(wave_text[i], num_idx_time[1], num_idx_time[2]) |> as.integer()

    # NOTE: nlon is not the last value, but one before
    for (j in 2:nlon) {
        mjo_vector[j - 1] <- substring(wave_text[i], num_idx_time[j], num_idx_time[j + 1]) |> as.double()
    }

    wave_time <- tibble(time = time, lon = 1:nlon, value = mjo_vector)
    wave <- bind_rows(wave, wave_time)
}

# Making the plot for a specific time
g <- plotTime(wave, t = 0)
