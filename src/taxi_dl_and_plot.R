# Download and plot NYC Yellow Cab data
# Brian Lance

# GET DATA ####

# Note: I previously setup a Google BigQuery account and added the NYC Taxi data

library(bigrquery)

# Define query. by grouping on the rounded lat/long values we can treat the 
# COUNT() of each group as the num_pickups at each location. We later use 
# this to manipulate the opacity of each point on the plot (i.e. more pickups --> brighter yellow).
# The WHERE statement just removes a bunch of nonsensical observations.
sql <- paste0("SELECT ROUND(pickup_latitude, 4) as lat, ROUND(pickup_longitude, 4) as long, COUNT(*) as num_pickups, 
               FROM [nyc-tlc:yellow.trips_2014]
               WHERE fare_amount/trip_distance BETWEEN 2 AND 10
               GROUP BY lat, long")

project <- "your bigquery project id"

# send query and pass result to df in the current environment
df <- query_exec(sql, project = project, 
                        default_dataset = "nyc-tlc:yellow.trips_2014", 
                        max_pages = Inf)

# save df for later (no need to download again)
write.csv(df, "data/yellow_pickups_2014.csv", row.names = F)

# GENERATE PLOT ####
library(dplyr); library(ggplot2); library(ggthemes)

# I use the tufte theme because I am a tufte fan and like starting with a 
# stripped down canvas for generating plots. I am still getting familiar
# with the available themes and doubt it's even necessary to use tufte_theme 
# in the first place since I remove the axes and just want a black background.

# Set size / limit parameters
lim_long <- c(-74.1, -73.7); lim_lat <- c(40.5, 41.0) # set lat/long ranges

# Optional: if you need to make quick tweaks, use a smaller sample for faster plotting
# df_sample <- sample_n(df, size = 300000)
# One issue is that transparency gets really dim...
data <- df

# For each point, I set alpha equal to the log of num_pickups base median num_pickups
# This way, points beneath the median are semi transparent. This is an imperfect approach
# since all of the points above the median have equal visual weight. But I just needed to 
# get something sketched out and this gave a decent looking plot.
alpha_thresh <- median(data$num_pickups, na.rm = FALSE)

p <- ggplot(data, aes(long, lat)) + theme_tufte() +
    geom_point(aes(alpha = log(num_pickups, base = alpha_thresh)),
               size=0.1, colour = "#FFFF00") +
    xlim(lim_long[1], lim_long[2]) + ylim(lim_lat[1], lim_lat[2]) +
    theme(legend.position = "none", axis.ticks = element_blank(), 
          axis.text = element_blank(), axis.title = element_blank(),
          panel.background = element_rect(fill = "black"),
          plot.background = element_rect(fill = "black"))


# SAVE PLOT ####

# Set plot size multiplier (dimensions determined by lat/long ranges)
scale_plot_size <- 20

# Calculations for plot parameters
plot_height <- lim_lat[2] - lim_lat[1]
plot_width <- lim_long[2] - lim_long[1]

# Save as PNG
ggsave(file = paste0("fig/plot",strftime(Sys.time(), "_%H%M%S_%d%m%y"),".png"), plot = p, 
       width = plot_width*scale_plot_size, height = plot_height*scale_plot_size, 
       units = c("in"), dpi = 300)