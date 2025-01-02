## HEADER ####
## Who: Maria
## What: Data Visualisation
## When: 2024-12-29


## CONTENTS ####
## 00 Setup
## 01 Map



## 00 Setup ####
## 01 Map
## 02 Line Plot
## 03 Base R Plot for Oct-Dec
## 04 Stressors Plotly
## 05 Varroa Mite Plot
## 06 All Stressors


# Load libraries
library(dplyr)
library(tmap)
library(ggplot2)
library(plotly)

# Load data
colony <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-11/colony.csv')
stressor <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-11/stressor.csv')

# Set working directory


## 01 Map ####
## Create an animated map that shows percentage colony loss for each year.

# Create object with the U.S state boundaries
usa <- usmap::us_map()
View(usa)   # Doing this shows that the column containing the state names is titled 'full'.

# Rename the column with state names to 'state'
usa <- usa %>% 
  rename(state = full)

# Prepare data
mapdata <- colony %>%                        # Create a new object.
  filter(months == 'January-March')          # Filter the data from 'colony' to only include January-March
mapdata <- merge(x = usa, y = mapdata, by = 'state')  # Merge this new data frame with the 'usa' one created earlier.

# Create the map
animatedmap <- tm_shape(mapdata) +           # Select the 'mapdata' object
  tm_polygons(                               # Add map layer
    col = 'colony_lost_pct',                 # Select 'colony_lost_pct' as the variable to be shown on the map
    breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55),      # Set the values for the legend
    title = '% Bee Colonies Lost') +                               # Set the title
  tm_facets(along = 'year') +                                      # Set 'year' as the variable to be changed
  tm_layout(legend.position = c('left', 'bottom'))                 # Set the legend position as bottom left

tmap_animation(animatedmap, filname = 'animatedmap.gif',           # Create he animated map
               delay = 120)


## 02 Line Plot ####
# Create

us_colony <- colony %>%            # Create a new object and select 'colony'
  filter(state == 'United States') # Filter for 'United States', so that it only includes the data for the whole country.

# Change the year column to enable chronological plotting
us_colony$year <- c(2015, 2015.25, 2015.5, 2015.75, 2016, 2016.25, 2016.5, 2016.75, 2017, 2017.25, 2017.5, 2017.75, 2018, 2018.25, 2018.5, 2018.75, 2019, 2019.25, 2019.5, 2019.75, 2020, 2020.25, 2020.5, 2020.75, 2021, 2021.25)

# Reorder the months
us_colony$months <- as.factor(us_colony$months)   # Reclass the months variable as a factor 
us_colony$months <- factor(us_colony$months, levels = c('January-March', 'April-June', 'July-September', 'October-December')) # Make it an ordered factor, and correct order of the months.

# Create the plot
ggplot(us_colony) +                         # Select the data
  aes(x = year, y = colony_lost_pct) +      # Select x.axis variable as 'year' and y-axis variable as 'colony_lost_pct'
  geom_line(colour = "#112446") +           # Add a layer with line graph
  geom_point(aes(color = months)) +         # Add a layer with dot plot, with colour depending on the months.
  labs(title = 'Bee Colony Losses in the US 2015-2021', x = "Year", y = "% Colonies Lost") +  # Add axis labels and nai title
  theme_classic() # remove background, improves ink ratio


## 03 Base R Plot for Oct-Dec ####

# Filter for October-December
octdec <- colony %>%       # Create a new object called 'octdec'
  filter(state == 'United States', months == 'October-December')   # Only include data for October-December, from the US as a whole.
# Create a plot
plot(colony_lost_pct ~ year,     # Select 'colony_lost_pct' as y variable, and 'year' as the x variable
     data = octdec,              # Select the new data frame 'octdec'
     xlab = 'Year',              # X-axis title
     ylab = '% Colonies Lost',   # Y-axis title
     main = 'October-December Colony Losses')   # Main title

lines(colony_lost_pct[order(year)] ~ year[order(year)],   # Add a line
      data = octdec,                                      # Select the same data frame
      col = 'red')                                        # Change colour to red


## 04 Stressors Plotly ####
# Create new data frame
us_stressor <- stressor %>%              # Select the 'stressor' data frame
  filter(state == 'United States')       # Filter for data from the whole US

us_stressor <- us_stressor %>%          # Select the new 'us_stressor' data frame
  select(year, months, stressor, stress_pct)  # Select only the variables needed

# Reorder the months
us_stressor$months <- as.factor(us_stressor$months)   # Reclass the months variable as a factor 
us_stressor$months <- factor(us_stressor$months, levels = c('January-March', 'April-June', 'July-September', 'October-December')) # Make it an ordered factor, and correct order of the months.

# Create plot
plot_ly(
  data = us_stressor,     # Select data frame
  x = ~months,            # Selects 'months' as x variable
  y = ~stress_pct,        # Select stress_pct as y variable
  frame = ~year,          # Select 'year' for frame
  color = ~stressor,      # Select 'stressor' as colour variable
  type = 'bar') %>%       # Bar chart
  layout(title = 'Bee Colony Stressors',                   # Main title
         xaxis = list(title = list(text = 'Months')),    # X-axis title
         yaxis = list(title = list(text = '% Colonies Affected'))) # Y-axis Title


## 05 Varroa Mite Plot ####


octdec_vm <- stressor %>% 
  filter(months == 'October-December', stressor == 'Varroa mites', state != 'United States')
octdec_col <- colony %>% 
  filter(months == 'October-December', state != 'United States')

octdec_vm$number <- c(1:276)
octdec_col$number <- c(1:276)

octdec_plot <- merge(x = octdec_vm, y = octdec_col, by = 'number')
octdec_plot <- octdec_plot %>% 
  select('number', 'colony_lost_pct', 'stress_pct')

library(ggplot2)

ggplot(octdec_plot) +
 aes(x = colony_lost_pct, y = stress_pct) +
 geom_point(colour = "#112446") +
 geom_smooth(se = TRUE, 
 colour = "#FF8C00") +
 labs(x = "% Colonies Lost", y = "% Colonies Affected by Varroa Mites", title = "Are Varroa Mites contributing to Bee Colony Losses in the US?", 
 subtitle = "Winters between 2015-2020.", caption = "Confidence level of 95% used") +
 theme_classic()


## 06 All Stressors ####


# Filter colony data for Oct-Dec, and remove USA so it's just each state.
colony_data <- colony %>% 
  filter(months == 'October-December', state != 'United States')   
# Filter stressor data for Oct-Dec, and for data from each state
stressor_data <- stressor %>% 
  filter(months == 'October-December', state != 'United States', stressor != 'Other', stressor != 'Unknown')   # Also remove 'other' and 'unknown' to focus on known stressors

colony_data <- colony_data %>%     
  slice(rep(1:n(), each = 4))     # Repeat each line in new 'colony_data' data frame 4 times, one for each stressor.

colony_data$number <- c(1:1104)         # Create new column with numbers
stressor_data$number <- c(1:1104)       # Create new column with numbers

# Create new data frame
trends_plot <- merge(x = colony_data, y= stressor_data, by = 'number') # merge data by number






library(ggplot2)

ggplot(trends_plot) +      # Select data
 aes(x = colony_lost_pct, y = stress_pct) +     #Select 'colony_lost_pct' for x-axis, and stress_pct for y-axis
 geom_smooth(aes(color = stressor,    # Add layer with smoothed line, then change the colour based on the stressor. There will now be four lines
 x = colony_lost_pct, y = stress_pct), se = TRUE) +
 scale_color_hue(direction = 1) +
 labs(x = "% Colonies Lost",     # X-axis label, Y-axis label and main title:
 y = "% Colonies Affected by Stressor", title = "Bee Colony Stressors", subtitle = "Is there a correlation between the percentage of colonies affected by each stressor, and the percentage of colony losses?", 
 caption = "95% Level of Confidence") +     # State level of confidence in caption
 theme_classic()

