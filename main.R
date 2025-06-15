#install.packages(c("sf", "terra", "geodata", "ggplot2", "dplyr", "countrycode", "rnaturalearth", "rnaturalearthdata"))

################################################################################
# Data Science for Planetary Health: Hands-on Geospatial Analysis in R
# LMU Munich - Giacomo De Nicola - 16.06.2025
################################################################################

# Welcome! Today we'll explore how to acquire, process, and visualize
# geospatial data for planetary health applications.
# We'll focus on administrative boundaries and climate data.

# You will get to choose your own country to work with!
# We will use LLMs (like ChatGPT, Claude, etc.) as helpful assistants.

################################################################################
# PART 1: SETTING UP & ADMINISTRATIVE BOUNDARIES (First 1.5-hour block)
################################################################################

#clear R environment
rm(list=ls())

#-------------------------------------------------------------------------------
# 0. Load Essential Packages
#-------------------------------------------------------------------------------
library(sf)
library(terra)
library(geodata)
library(ggplot2) #for plotting
library(dplyr) # Though we use it less today, good to have
library(countrycode) #for getting iso3 country codes from names 
# library(rnaturalearth) # We'll primarily use GADM for multi-level, but good to know
# library(rnaturalearthdata)

#-------------------------------------------------------------------------------
# 1. Choose Your Country & Get its ISO3 Code
#-------------------------------------------------------------------------------
my_country_name <- "Denmark" # <-- !!! REPLACE THIS !!!
my_country_iso3 <- countrycode(my_country_name, origin = "country.name", destination = "iso3c")
print(paste("My chosen country:", my_country_name, "(ISO3:", my_country_iso3, ")"))

if (is.na(my_country_iso3)) {
  warning("Could not find ISO3 code automatically. Please find it manually (e.g., search online) and assign it directly below.")
  # my_country_iso3 <- "XYZ" # <-- !!! REPLACE XYZ with the correct code !!!
}

#-------------------------------------------------------------------------------
# 2. Download Administrative Boundaries (Levels 0, 1, and 2)
#-------------------------------------------------------------------------------
# We'll use GADM data. Levels are: 0 (country), 1 (states/provinces),
# 2 (districts/counties).
# Downloads can take time, especially for higher levels or large countries.

# --- Level 0: Country Border ---
cat("Downloading ADM0 (Country Border) for", my_country_name, "...\n")
adm0_spatvector <- gadm(country = my_country_iso3, level = 0, path = tempdir())
adm0_sf <- st_as_sf(adm0_spatvector)
print("--- ADM0 Boundaries ---")
print(adm0_sf)
#plot(st_geometry(adm0_sf), main = paste("ADM0 (Country Border) -", my_country_name), col = "lightgrey", border = "black") # Quick base plot

# --- Level 1: First Sub-national Unit (e.g., States/Provinces) ---
cat("\nDownloading ADM1 boundaries for", my_country_name, "...\n")
adm1_spatvector <- gadm(country = my_country_iso3, level = 1, path = tempdir())
adm1_sf <- st_as_sf(adm1_spatvector)
print("--- ADM1 Boundaries ---")
print(adm1_sf)

# Identify the ADM1 name column (e.g., "NAME_1")
# TODO: Inspect `print(adm1_sf)` and update if your ADM1 name column is different.
my_adm1_name_column <- "NAME_1" # <-- !!! REPLACE THIS if different for your country !!!
if (!my_adm1_name_column %in% names(adm1_sf)) {
  warning(paste("ADM1 name column '", my_adm1_name_column, "' not found. Check available columns:", paste(names(adm1_sf), collapse=", ")))
  # Fallback or ask student to fix:
  # my_adm1_name_column <- names(adm1_sf)[grep("NAME_1|VARNAME_1|ADM1_EN", names(adm1_sf))[1]] # A guess
}


# --- Level 2: Second Sub-national Unit (e.g., Districts/Counties) ---
# This will be our primary level for climate analysis later.
cat("\nDownloading ADM2 boundaries for", my_country_name, "...\n")
adm2_spatvector <- gadm(country = my_country_iso3, level = 2, path = tempdir())
adm2_sf <- st_as_sf(adm2_spatvector)
print("--- ADM2 Boundaries ---")
print(adm2_sf)

# Identify the ADM2 name column (e.g., "NAME_2")
# TODO: Inspect `print(adm2_sf)` and update if your ADM2 name column is different.
my_adm2_name_column <- "NAME_2" # <-- !!! REPLACE THIS if different for your country !!!
if (!my_adm2_name_column %in% names(adm2_sf)) {
  warning(paste("ADM2 name column '", my_adm2_name_column, "' not found. Check available columns:", paste(names(adm2_sf), collapse=", ")))
  # my_adm2_name_column <- names(adm2_sf)[grep("NAME_2|VARNAME_2|ADM2_EN", names(adm2_sf))[1]] # A guess
}

# --- Level 3 (Mention Only) ---
# You can also attempt to download ADM3 level (and even ADM4 for some countries).
# However, ADM3 data is not available for all countries, can be very large,
# and significantly slow down processing.
# For self-study, you could try:
# adm3_spatvector <- gadm(country = my_country_iso3, level = 3, path = tempdir())
# if (!is.null(adm3_spatvector) && nrow(adm3_spatvector) > 0) { adm3_sf <- st_as_sf(adm3_spatvector) }
# Be cautious with this as it can take a very long time or fail.


#-------------------------------------------------------------------------------
# 3. Plotting Administrative Boundaries (Levels 0, 1, and 2)
#-------------------------------------------------------------------------------
# Plot ADM0 as base
plot_base_adm0_map <- ggplot() + # Renamed to avoid conflict if base is used elsewhere
  geom_sf(data = adm0_sf, fill = "lightgray", color = "black", size = 0.5) +
  theme_minimal() +
  labs(title = paste("Administrative Levels for", my_country_name))
print(plot_base_adm0_map)

# Add ADM1
plot_adm1_on_adm0 <- plot_base_adm0_map + # Build upon the ADM0 map
  geom_sf(data = adm1_sf, aes(fill = .data[[my_adm1_name_column]]), color = "darkblue", alpha = 0.7, size=0.3) +
  guides(fill = "none") + # Hide legend for clarity
  ggtitle(paste("ADM0 and ADM1 -", my_country_name))
print(plot_adm1_on_adm0)

# Plot ADM2 on ADM0 (ADM1 lines can make it too busy if ADM2 is dense)
# We'll plot ADM2 borders directly on the base ADM0 map for clarity
plot_adm2_on_adm0 <- plot_base_adm0_map + # Build upon the ADM0 map
  geom_sf(data = adm2_sf, fill = NA, aes(color = .data[[my_adm2_name_column]]), size=0.2) + # No fill, color by region
  guides(color = "none") + # Hide legend
  ggtitle(paste("ADM0 and ADM2 -", my_country_name))
print(plot_adm2_on_adm0)

# For the rest of the analysis, we will use ADM2 level boundaries.
# If ADM2 was problematic for your country, you might need to switch to ADM1.
# To do so, uncomment the following two lines and comment out the ADM2 lines below:
# active_adm_level_sf <- adm1_sf
# my_active_adm_name_column <- my_adm1_name_column # This should be the name col for ADM1

active_adm_level_sf <- adm2_sf
my_active_adm_name_column <- my_adm2_name_column # This should be the name col for ADM2

cat("\n***** Proceeding with ADM level specified in `active_adm_level_sf` for climate analysis. *****\n")
cat("Currently set to ADM2. If ADM2 was problematic, consider switching to ADM1 in the script.\n")


#-------------------------------------------------------------------------------
# 4. Download Climate Data (Bioclimatic Variables)
#-------------------------------------------------------------------------------
cat("\nDownloading WorldClim Bioclimatic data for", my_country_name, "(res=10 requested, actual resolution may vary)...\nThis might take a few minutes.\n")
# When `res=10` (10 arc-minutes) is requested, geodata might download a higher resolution
# (e.g., 30 arc-seconds, which is 0.5 arc-minutes) if a pre-processed country file
# at that higher resolution is more readily available. The layer names will reflect the actual resolution.
worldclim_bio_country_allvars <- worldclim_country(country = my_country_iso3, var = "bio", res = 10, path = tempdir())

print("--- WorldClim Bioclimatic Data (SpatRaster object with multiple layers) ---")
print(worldclim_bio_country_allvars)
cat("\nAvailable Bioclimatic Variables (layers in the downloaded SpatRaster):\n")
print(names(worldclim_bio_country_allvars))

# Bioclimatic variables details (WorldClim v2.1):
# BIO1 = Mean Annual Temperature (°C)
# BIO2 = Mean Diurnal Range (Mean of monthly (max temp - min temp)) (°C)
# BIO3 = Isothermality (BIO2/BIO7) (* 100)
# BIO4 = Temperature Seasonality (standard deviation *100)
# BIO5 = Max Temperature of Warmest Month (°C)
# BIO6 = Min Temperature of Coldest Month (°C)
# BIO7 = Temperature Annual Range (BIO5-BIO6) (°C)
# BIO8 = Mean Temperature of Wettest Quarter (°C)
# BIO9 = Mean Temperature of Driest Quarter (°C)
# BIO10 = Mean Temperature of Warmest Quarter (°C)
# BIO11 = Mean Temperature of Coldest Quarter (°C)
# BIO12 = Annual Precipitation (mm)
# BIO13 = Precipitation of Wettest Month (mm)
# BIO14 = Precipitation of Driest Month (mm)
# BIO15 = Precipitation Seasonality (Coefficient of Variation)
# BIO16 = Precipitation of Wettest Quarter (mm)
# BIO17 = Precipitation of Driest Quarter (mm)
# BIO18 = Precipitation of Warmest Quarter (mm)
# BIO19 = Precipitation of Coldest Quarter (mm)

# Temperatures are in °C. Precipitation in mm. Some indices are scaled.
# For version 2.1, direct values are generally used for temp/precip.

# TODO: Choose which Bioclimatic variables you want to work with by their BIO number.
#       E.g., for BIO1 (Mean Annual Temp), use 1. For BIO12 (Annual Precip), use 12.
#       You will use these numbers to help the code find the correct layer names.
user_choice_bio_number1 <- 1  # Example: Mean Annual Temp (BIO1)
user_choice_bio_number2 <- 12 # Example: Annual Precipitation (BIO12)
# user_choice_bio_number3 <- 4  # Example: Temperature Seasonality (BIO4) <-- !!! REPLACE OR ADD YOUR CHOICE (number only) !!!

# Function to find the full layer name based on BIO number and add the selected raster
# This function now creates a simpler descriptive name (e.g., "BIO1") for use later.
add_selected_raster <- function(raster_list, all_rasters, bio_number_to_find) {
  if (!is.null(bio_number_to_find) && is.numeric(bio_number_to_find)) {
    # Construct a pattern to find the layer, e.g., "_bio_1$", "_bio_12$"
    # This pattern looks for layer names ending with "_bio_NUMBER".
    pattern_to_find <- paste0("_bio_", bio_number_to_find, "$") # $ ensures it matches at the end
    
    # Search for the pattern in the layer names of the downloaded SpatRaster
    found_layer_name <- grep(pattern_to_find, names(all_rasters), value = TRUE)
    
    if (length(found_layer_name) == 1) { # We expect exactly one match
      selected_raster_layer <- all_rasters[[found_layer_name[1]]] # Select the raster layer
      
      # Create a simple, descriptive name (e.g., "BIO1", "BIO12") for this variable.
      # This name will be used for the list element and later as the column name.
      descriptive_short_name <- paste0("BIO", bio_number_to_find)
      
      # It's good practice to also rename the layer within the SpatRaster object itself
      # if it's a single-layer SpatRaster being stored.
      names(selected_raster_layer) <- descriptive_short_name 
      
      raster_list[[descriptive_short_name]] <- selected_raster_layer # Add to our list
      cat("Selected climate variable:", descriptive_short_name, "(from full layer name:", found_layer_name[1], ")\n")
    } else if (length(found_layer_name) > 1) {
      # This case is less likely if layer names are unique, but included for robustness
      warning(paste("Multiple layers found for BIO", bio_number_to_find, ":", paste(found_layer_name, collapse=", "), ". Using the first one: '", found_layer_name[1], "'. Please check."))
      selected_raster_layer <- all_rasters[[found_layer_name[1]]]
      descriptive_short_name <- paste0("BIO", bio_number_to_find)
      names(selected_raster_layer) <- descriptive_short_name
      raster_list[[descriptive_short_name]] <- selected_raster_layer
      cat("Selected climate variable:", descriptive_short_name, "(from full layer name:", found_layer_name[1], ") - Note: multiple matches found, used first.\n")
    } else {
      # No layer found matching the pattern
      warning(paste("No layer found for BIO", bio_number_to_find, "in the downloaded WorldClim data. Searched for pattern '", pattern_to_find, "' in names: ", paste(names(all_rasters), collapse=", "), ". Please check `names(worldclim_bio_country_allvars)` and your `user_choice_bio_number` values."))
    }
  } else if (!is.null(bio_number_to_find)) {
    warning(paste("Invalid BIO number provided:", bio_number_to_find, ". Please provide a numeric BIO number (e.g., 1 for BIO1, 12 for BIO12)."))
  }
  return(raster_list)
}

selected_climate_rasters <- list() # Initialize an empty list
selected_climate_rasters <- add_selected_raster(selected_climate_rasters, worldclim_bio_country_allvars, user_choice_bio_number1)
selected_climate_rasters <- add_selected_raster(selected_climate_rasters, worldclim_bio_country_allvars, user_choice_bio_number2)
# If you add user_choice_bio_number3:
# selected_climate_rasters <- add_selected_raster(selected_climate_rasters, worldclim_bio_country_allvars, user_choice_bio_number3)


# Quick plot of the first selected climate variable (if any were selected)
if (length(selected_climate_rasters) > 0) {
  # The names of the elements in selected_climate_rasters are now "BIO1", "BIO12", etc.
  first_selected_descriptive_name <- names(selected_climate_rasters)[1] 
  plot(selected_climate_rasters[[first_selected_descriptive_name]], # Plot the SpatRaster
       main = paste(first_selected_descriptive_name, "for", my_country_name))
} else {
  cat("No climate variables were successfully selected. Please check your `user_choice_bio_number` settings and any warnings.\n")
}

cat("\n\n***** End of Part 1 *****\n")
# ... (rest of End of Part 1 message)

################################################################################
# BREAK TIME! (Approx. 30 minutes)
################################################################################
# PART 2: COMBINING & ANALYZING DATA (Second 1.5-hour block)
################################################################################

cat("\n\n***** Welcome to Part 2! *****\n")
# ...

#-------------------------------------------------------------------------------
# 5. Extracting Raster Values to Polygons (for selected climate variables)
#-------------------------------------------------------------------------------
adm_with_climate_sf <- active_adm_level_sf 

if (length(selected_climate_rasters) > 0) {
  for (i in seq_along(selected_climate_rasters)) {
    current_raster_in_list <- selected_climate_rasters[[i]] # This is the SpatRaster for one variable
    # The descriptive name (e.g., "BIO1") is the name of the element in the list
    # and also the name of the layer within current_raster_in_list
    descriptive_col_name <- names(selected_climate_rasters)[i] 
    
    cat("Extracting mean", descriptive_col_name, "for each region in", my_active_adm_name_column, "...\n")
    
    extracted_data_values <- terra::extract(
      current_raster_in_list, 
      vect(adm_with_climate_sf), 
      fun = mean,
      na.rm = TRUE,
      ID = FALSE 
    )
    
    # Name the column of the extracted data frame with our descriptive name
    # `extracted_data_values` will have one column, and its current name is likely
    # the same as `descriptive_col_name` because we renamed the raster layer.
    # However, explicitly setting it ensures consistency.
    colnames(extracted_data_values) <- descriptive_col_name 
    
    adm_with_climate_sf <- cbind(adm_with_climate_sf, extracted_data_values)
    cat("Added column:", descriptive_col_name, "to the sf object.\n")
  }
} else {
  cat("No climate rasters were selected, so no extraction performed.\n")
}

print("--- ADM boundaries with All Extracted Mean Climate Variables (Head) ---")
if (exists("adm_with_climate_sf")) print(head(adm_with_climate_sf))
cat("Full column names in adm_with_climate_sf:\n")
if (exists("adm_with_climate_sf")) print(names(adm_with_climate_sf))

#-------------------------------------------------------------------------------
# 6. Thematic Mapping (Choropleth Maps) of Chosen Climate Data
#-------------------------------------------------------------------------------
# The names of the selected climate variables (e.g., "BIO1", "BIO12") are the keys in our list
climate_vars_to_map <- names(selected_climate_rasters) 

# Map the first chosen climate variable
if (length(climate_vars_to_map) >= 1 && climate_vars_to_map[1] %in% names(adm_with_climate_sf)) {
  var1_name <- climate_vars_to_map[1] # This will be "BIO1" or similar
  map_climate_var1 <- ggplot(data = adm_with_climate_sf) +
    geom_sf(aes(fill = .data[[var1_name]])) +
    scale_fill_viridis_c(name = gsub("BIO", "BIO ", var1_name), option = "plasma") + 
    labs(title = paste(gsub("BIO", "BIO ", var1_name), "by", my_active_adm_name_column, "Region"),
         subtitle = my_country_name,
         caption = paste("Data: GADM & WorldClim")) +
    theme_minimal()
  print(map_climate_var1)
} else if (length(climate_vars_to_map) >=1) {
  warning(paste("Column for the first climate variable (expected:", climate_vars_to_map[1], ") not found for mapping!"))
} else {
  cat("No climate variables selected to map.\n")
}

# Map the second chosen climate variable
if (length(climate_vars_to_map) >= 2 && climate_vars_to_map[2] %in% names(adm_with_climate_sf)) {
  var2_name <- climate_vars_to_map[2] # This will be "BIO12" or similar
  map_climate_var2 <- ggplot(data = adm_with_climate_sf) +
    geom_sf(aes(fill = .data[[var2_name]])) +
    scale_fill_viridis_c(name = gsub("BIO", "BIO ", var2_name), option = "viridis") + 
    labs(title = paste(gsub("BIO", "BIO ", var2_name), "by", my_active_adm_name_column, "Region"),
         subtitle = my_country_name,
         caption = paste("Data: GADM & WorldClim")) +
    theme_minimal()
  print(map_climate_var2)
} # No 'else if' needed here if only checking for the second variable specifically

# --- (Optional) Combine Maps with Patchwork ---
# If you want to display map_climate_var1 and map_climate_var2 side-by-side
# (or one above the other), the `patchwork` package is excellent for this.

# First, you need to ensure patchwork is installed and loaded:
# install.packages("patchwork") # Run this once in your console if you haven't installed it
if (!require(patchwork)) {
  install.packages("patchwork")
  library(patchwork)
}

cat("\n--- (Optional) Combining maps with patchwork ---\n")

# Check if both plot objects exist before trying to combine them
if (exists("map_climate_var1") && inherits(map_climate_var1, "ggplot") &&
    exists("map_climate_var2") && inherits(map_climate_var2, "ggplot")) {
  
  cat("Attempting to combine map_climate_var1 and map_climate_var2...\n")
  
  # Simple side-by-side combination:
  combined_maps_side_by_side <- map_climate_var1 + map_climate_var2
  print(combined_maps_side_by_side)
  
  # To arrange one above the other:
  # combined_maps_stacked <- map_climate_var1 / map_climate_var2
  # print(combined_maps_stacked)
  
  # To add a common title or caption to the combined plot:
  # combined_maps_with_title <- (map_climate_var1 + map_climate_var2) +
  #   plot_annotation(title = paste("Climate Variables for", my_country_name),
  #                   caption = "Data: GADM & WorldClim")
  # print(combined_maps_with_title)
  
  cat("LLM PROMPT: 'How can I control the layout (e.g., 2x2 grid) when combining multiple ggplots with the R patchwork package?'\n")
  cat("LLM PROMPT: 'How to align legends when combining ggplots with patchwork?'\n")
  
} else {
  cat("One or both map objects (map_climate_var1, map_climate_var2) not found or not ggplot objects. Skipping patchwork combination.\n")
}


#-------------------------------------------------------------------------------
# 7. (Optional) Basic Exploration / "Analysis" - Scatter Plot
#-------------------------------------------------------------------------------
if (length(climate_vars_to_map) >= 2 &&
    climate_vars_to_map[1] %in% names(adm_with_climate_sf) &&
    climate_vars_to_map[2] %in% names(adm_with_climate_sf)) {
  
  var1_name_for_scatter <- climate_vars_to_map[1]
  var2_name_for_scatter <- climate_vars_to_map[2]
  
  scatter_climate_vars <- ggplot(data = st_drop_geometry(adm_with_climate_sf), 
                                 aes(x = .data[[var1_name_for_scatter]],
                                     y = .data[[var2_name_for_scatter]])) +
    geom_point(alpha = 0.7, color = "dodgerblue") +
    geom_smooth(method = "lm", se = FALSE, color = "firebrick") + 
    labs(title = paste("Relationship between", gsub("BIO", "BIO ", var1_name_for_scatter), "and", gsub("BIO", "BIO ", var2_name_for_scatter)),
         subtitle = paste(my_country_name, "-", my_active_adm_name_column, "level"),
         x = gsub("BIO", "BIO ", var1_name_for_scatter),
         y = gsub("BIO", "BIO ", var2_name_for_scatter)) +
    theme_minimal()
  print(scatter_climate_vars)
} else {
  cat("Not enough climate variables (need at least 2) successfully processed and added to the data for a scatter plot.\n")
}

#-------------------------------------------------------------------------------
# 7. Exploring the Relationship Between Your Two Selected Climate Variables
#-------------------------------------------------------------------------------
cat("\n\n***** Section 7: Exploring the Relationship Between Your Two Selected Climate Variables *****\n")
cat("Let's dive a bit deeper into how your first two selected climate variables relate to each other across regions.\n")
cat("Each sub-section below can be run independently if `adm_with_climate_sf` and your chosen climate variables exist.\n")

# Define the variable names from student's earlier choices for convenience in this section.
# Students should ensure these `climate_vars_to_map` were correctly processed in earlier sections.
if (exists("climate_vars_to_map") && length(climate_vars_to_map) >= 2) {
  var1_name <- climate_vars_to_map[1]
  var2_name <- climate_vars_to_map[2]
  cat("This section will use:", var1_name, "(as var1) and", var2_name, "(as var2).\n")
  cat("Ensure these columns exist in `adm_with_climate_sf` from your previous steps.\n")
} else {
  cat("Warning: `climate_vars_to_map` does not seem to contain at least two variable names. Please ensure Sections 4-6 ran correctly.\n")
  # Provide placeholders if not defined, so code chunks don't immediately break, but warn student.
  if (!exists("var1_name")) var1_name <- "YOUR_FIRST_CLIMATE_VAR_COLNAME" # e.g. "BIO1"
  if (!exists("var2_name")) var2_name <- "YOUR_SECOND_CLIMATE_VAR_COLNAME" # e.g. "BIO12"
}


# --- 7.1: Descriptive Statistics ---
cat("\n--- 7.1: Descriptive Statistics for Each Variable ---\n")
if (exists("adm_with_climate_sf") && var1_name %in% names(adm_with_climate_sf)) {
  cat("Summary for", var1_name, ":\n")
  print(summary(adm_with_climate_sf[[var1_name]]))
  
  hist_var1 <- ggplot(data = st_drop_geometry(adm_with_climate_sf), aes(x = .data[[var1_name]])) +
    geom_histogram(bins = 15, fill = "skyblue", color = "black", alpha=0.7) +
    labs(title = paste("7.1: Distribution of", var1_name), x = var1_name, y = "Frequency") +
    theme_minimal()
  print(hist_var1)
} else {
  cat("Data for", var1_name, "not found in `adm_with_climate_sf` for descriptive stats.\n")
}

if (exists("adm_with_climate_sf") && var2_name %in% names(adm_with_climate_sf)) {
  cat("\nSummary for", var2_name, ":\n")
  print(summary(adm_with_climate_sf[[var2_name]]))
  
  hist_var2 <- ggplot(data = st_drop_geometry(adm_with_climate_sf), aes(x = .data[[var2_name]])) +
    geom_histogram(bins = 15, fill = "lightcoral", color = "black", alpha=0.7) +
    labs(title = paste("7.1: Distribution of", var2_name), x = var2_name, y = "Frequency") +
    theme_minimal()
  print(hist_var2)
} else {
  cat("Data for", var2_name, "not found in `adm_with_climate_sf` for descriptive stats.\n")
}


# --- 7.2: Correlation Analysis ---
cat("\n--- 7.2: Correlation Analysis ---\n")
if (exists("adm_with_climate_sf") && var1_name %in% names(adm_with_climate_sf) && var2_name %in% names(adm_with_climate_sf) &&
    is.numeric(adm_with_climate_sf[[var1_name]]) && is.numeric(adm_with_climate_sf[[var2_name]])) {
  
  correlation_test <- cor.test(adm_with_climate_sf[[var1_name]], adm_with_climate_sf[[var2_name]], use = "complete.obs")
  cat("Pearson Correlation between", var1_name, "and", var2_name, ":\n")
  print(correlation_test)
  cat("Correlation coefficient (r):", round(correlation_test$estimate, 3), "\n")
  cat("P-value:", format.pval(correlation_test$p.value, digits = 3), "\n")
  cat("Note: Correlation does not imply causation! This is an ecological correlation based on regional averages.\n")
} else {
  cat("Could not perform correlation: one or both variables not found or not numeric in `adm_with_climate_sf`.\n")
  cat("Needed:", var1_name, "and", var2_name, "\n")
}


# --- 7.3: Scatter Plot with Linear Model (lm) ---
cat("\n--- 7.3: Scatter Plot with Linear Model (lm) ---\n")
if (exists("adm_with_climate_sf") && var1_name %in% names(adm_with_climate_sf) && var2_name %in% names(adm_with_climate_sf)) {
  scatter_plot_lm <- ggplot(data = st_drop_geometry(adm_with_climate_sf), 
                            aes(x = .data[[var1_name]], y = .data[[var2_name]])) +
    geom_point(alpha = 0.7, color = "dodgerblue") +
    geom_smooth(method = "lm", se = TRUE, color = "firebrick", fill="lightpink") + 
    labs(title = paste("7.3: Linear Relationship:", var1_name, "vs.", var2_name),
         subtitle = paste(my_country_name, "-", my_active_adm_name_column, "level"),
         x = var1_name,
         y = var2_name,
         caption = "Line: Linear Model (lm) with 95% CI") +
    theme_minimal()
  print(scatter_plot_lm)
  cat("LLM PROMPT: 'What are the assumptions of a linear regression model, and how might they apply to this ecological data?'\n")
} else {
  cat("Could not create scatter plot with lm: one or both variables not found in `adm_with_climate_sf`.\n")
  cat("Needed:", var1_name, "and", var2_name, "\n")
}


# --- 7.4: Scatter Plot with Smoothed Spline (e.g., LOESS) ---
cat("\n--- 7.4: Scatter Plot with Smoothed Spline (LOESS) ---\n")
if (exists("adm_with_climate_sf") && var1_name %in% names(adm_with_climate_sf) && var2_name %in% names(adm_with_climate_sf)) {
  scatter_plot_loess <- ggplot(data = st_drop_geometry(adm_with_climate_sf), 
                               aes(x = .data[[var1_name]], y = .data[[var2_name]])) +
    geom_point(alpha = 0.7, color = "forestgreen") +
    geom_smooth(method = "loess", se = TRUE, color = "darkorange", fill="moccasin", span = 0.75) + 
    labs(title = paste("7.4: Non-Linear Relationship (LOESS):", var1_name, "vs.", var2_name),
         subtitle = paste(my_country_name, "-", my_active_adm_name_column, "level"),
         x = var1_name,
         y = var2_name,
         caption = "Line: LOESS Smoother with 95% CI (span=0.75)") +
    theme_minimal()
  print(scatter_plot_loess)
  cat("LLM PROMPT: 'How does a LOESS smoother work, and what are its advantages over a simple linear model for visualizing trends?'\n")
  cat("LLM PROMPT: 'How can I use `geom_smooth(method = \"gam\")` with a formula like `formula = y ~ s(x, bs = \"cs\")` in ggplot2 for a GAM smoother?' (Requires `mgcv` package)\n")
} else {
  cat("Could not create scatter plot with LOESS: one or both variables not found in `adm_with_climate_sf`.\n")
  cat("Needed:", var1_name, "and", var2_name, "\n")
}


# --- 7.5: Your Turn! Brief Creative Prompt ---
cat("\n--- 7.5: Your Turn! Brief Creative Prompt ---\n")
cat("Consider the plots you've just made (if they were successfully generated).\n")
cat("- Does the relationship look strongly linear, or does the smooth curve suggest something else?\n")
cat("- Are there any clear outliers in the scatter plots? How might you identify them programmatically?\n")
cat("  (LLM PROMPT: 'How to identify and label outliers in a scatter plot in R ggplot2 based on distance from a fitted line?')\n")
cat("- How might the scale of your administrative units (e.g., very large vs. very small regions) affect these observed relationships (Modifiable Areal Unit Problem)?\n")


#-------------------------------------------------------------------------------
# 8. Wrap-up & Further Steps
#-------------------------------------------------------------------------------
# Congratulations! You've successfully:
# 1. Downloaded multiple levels of administrative boundaries (ADM0, ADM1, ADM2).
# 2. Downloaded a suite of climate data (Bioclimatic variables).
# 3. Chosen specific climate variables and extracted their mean values for each region at your chosen admin level.
# 4. Created thematic maps for your chosen climate variables.
# 5. Explored the relationship between two climate variables (if selected).

# This provides a powerful foundation for many planetary health analyses!

cat("\n\n***** Workshop Complete! Thank you for participating! *****\n")
cat("Feel free to experiment further: choose different climate variables, try a different admin level for extraction, or explore other countries.\n")

# Example of saving your sf object with all extracted climate variables:
# if (exists("adm_with_climate_sf") && nrow(adm_with_climate_sf) > 0) {
#   output_filename <- paste0("adm_with_climate_", my_country_iso3, "_", gsub(" ", "_", my_active_adm_name_column), ".gpkg")
#   st_write(adm_with_climate_sf, output_filename, delete_layer = TRUE)
#   cat("Saved data to:", output_filename, "\n")
# }

