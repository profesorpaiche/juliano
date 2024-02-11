library(dplyr)
library(ggplot2)
library(cowplot)

options(stringsAsFactors = FALSE)

# Function to plot the waves
plotTime <- function(tb, t) {
    tb <- filter(tb, time == t)
    g <- ggplot() +
        geom_ribbon(
            data = tb |> mutate(mjo = ifelse(mjo > 0, mjo, 0)),
            mapping = aes(x = lon, ymin = 0, ymax = mjo),
            fill = "royalblue"
        ) +
        geom_ribbon(
            data = tb |> mutate(mjo = ifelse(mjo < 0, mjo, 0)),
            mapping = aes(x = lon, ymin = mjo, ymax = 0),
            fill = "brown"
        ) +
        geom_hline(yintercept = 0) +
        theme_half_open() +
        panel_border()
    return(g)
}

# Reading data
wave <- readLines("juliano_out.txt")
num_idx <- gregexpr(" ([0-9]|-)", wave)
nt <- length(num_idx)
ng <- length(num_idx[[1]]) - 1
tb <- tibble(time = integer(0), lon = integer(0), mjo = double(0))
height <- double(ng)

# Extracting information
for (i in 1:nt) {
    wave_time <- num_idx[[i]]
    time <- substring(wave[i], wave_time[1], wave_time[2]) |> as.integer()

    for (j in 2:ng) {
        height[j - 1] <- substring(wave[i], wave_time[j], wave_time[j + 1]) |> as.double()
    }

    tb_tmp <- tibble(time = time, lon = 1:ng, mjo = height)
    tb <- bind_rows(tb, tb_tmp)
}

# Making the plot for a specific time
g <- plotTime(tb, t = 0)
