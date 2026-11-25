##----------------------------------------------------------------------------##
# THESIS FIGURES
# 2_Infrastructure_free_nature
# This script creates a map of the change in infrastructure-free areas in 
# Norway using the dataset from 
##----------------------------------------------------------------------------##

# 0. SETUP ---------------------------------------------------------------------

# Source setup file (needs sf, dplyr, stringr, ggplot2, ggspatial and geodata)
library(here)
source(here("scripts", "0_setup.R"))

# Define CRS to use for the map (ETRS89 / UTM zone 33N)
project_crs <- 25833

# Folder where the INON layers are saved (each layer is only downloaded once)
inon_dir <- here("data","inon")
dir.create(inon_dir, showWarnings = FALSE, recursive = TRUE)

# Give the downloads more time to deal with the large size of the files
options(timeout = 900)

# 1. INON MAP ------------------------------------------------------------------

## 1.1. Load data --------------------------------------------------------------

# WFS service of the Norwegian Environment Agency
wfs_base <- "https://kart.miljodirektoratet.no/geoserver/inngrepsfrinatur/ows"

# Function to download a WFS layer as a shapefile (skipped if already saved) and read it in the project CRS
download_inon <- function(layer) {
  out_dir <- file.path(inon_dir, layer)
  if (length(list.files(out_dir, "\\.shp$")) == 0) {
    url <- paste0(wfs_base, "?service=WFS&version=1.0.0&request=GetFeature",
                  "&typeName=", layer, "&srsName=EPSG:", project_crs,
                  "&outputFormat=SHAPE-ZIP")
    zip_file <- file.path(inon_dir, paste0(layer, ".zip"))
    download.file(url, zip_file, mode = "wb")
    unzip(zip_file, exdir = out_dir)
  }
  # Norwegian letters in the column names (e.g. 'inonsonefør') are Latin-1
  st_read(list.files(out_dir, "\\.shp$", full.names = TRUE)[1],
          options = "ENCODING=LATIN1", quiet = TRUE) |>
    st_transform(project_crs)
}

# Load INON status 1988 and 2023 (the newest status is called "status")
inon_1988 <- download_inon("status_1988")
inon_2023 <- download_inon("status")

# Load the changes in INON between 1988 and 2023
inon_change <- download_inon("endring_1988_2023")

# Download mainland Norway boundary excluding Svalbard and Jan Mayen
norway_sf <- geodata::gadm(country = "NOR", level = 0, path = tempdir(),
                           version = "latest") |>
  st_as_sf()

## 1.2. Label the INON zones ---------------------------------------------------

# Zone codes in the 'vsone' column (from the agency's own map legend)
zone_names <- c("v" = "Wilderness-like (>5 km)",
                "1" = "Zone 1 (3-5 km)",
                "2" = "Zone 2 (1-3 km)")

# Combine both years and label the zones
inon <- bind_rows(inon_1988 |> mutate(year = 1988),
                  inon_2023 |> mutate(year = 2023)) |>
  transmute(year,
            zone = factor(zone_names[tolower(as.character(vsone))],
                          levels = zone_names))

# Check that all polygons got a zone (should be 0)
sum(is.na(inon$zone))

## 1.3. Classify the changes ---------------------------------------------------

# Rank the zones from least to most remote ("n" = not remote)
zone_rank <- c("n" = 0, "2" = 1, "1" = 2, "v" = 3)

# Change labels
change_names <- c("Lost 1988-2023", "Restored 1988-2023")

# The change column holds "from-to" codes, e.g. "v-1" = wilderness became
# zone 1 and "n-2" = restored to zone 2. Lost = moved to a less remote zone,
# restored = moved to a more remote zone
inon_change <- inon_change |>
  rename_with(tolower) |>
  rename(code = starts_with("inonendr")) |>  # name is truncated in the shapefile
  mutate(code = tolower(code),
         from = zone_rank[sub("-.*$", "", code)],
         to = zone_rank[sub("^.*-", "", code)],
         change = case_when(to < from ~ change_names[1],
                            to > from ~ change_names[2]),
         change = factor(change, levels = change_names)) |>
  filter(!is.na(change))

## 1.4. Area per zone and change (for the Methods text) ------------------------

# Calculate area for each zone and year
# (wilderness should be ~2,890 km2 smaller in 2023 than in 1988)
inon_area <- inon |>
  mutate(area_km2 = as.numeric(st_area(inon)) / 1e6) |>
  st_drop_geometry() |>
  group_by(year, zone) |>
  summarise(area_km2 = round(sum(area_km2)), .groups = "drop")

# Check the values
print(inon_area)

# Calculate area for each type of change
change_area <- inon_change |>
  mutate(area_km2 = as.numeric(st_area(inon_change)) / 1e6) |>
  st_drop_geometry() |>
  group_by(change, code) |>
  summarise(area_km2 = round(sum(area_km2), 1), .groups = "drop")

# Check the values
print(change_area, n = Inf)

## 1.5. Prepare for plotting ---------------------------------------------------

# Simplify the zone polygons slightly (50 m) so the map draws faster
# (the changes are not simplified, many are thin slivers that would vanish)
inon_plot <- st_simplify(inon, preserveTopology = TRUE, dTolerance = 50)

# Add panel labels (the changes are only shown in panel b)
panel_labels <- c("1988" = "a) 1988", "2023" = "b) 2023")
inon_plot <- inon_plot |>
  mutate(panel = factor(panel_labels[as.character(year)], levels = panel_labels))
inon_change <- inon_change |>
  mutate(panel = factor(panel_labels["2023"], levels = panel_labels))

# Put the north arrow and scale bar in panel a only
annotation_panel <- data.frame(panel = factor(panel_labels["1988"],
                                              levels = panel_labels))

# Convert the Norway boundary into the map CRS
norway_utm <- st_transform(norway_sf, project_crs)

# Angle of true north at the north arrow (top-left corner of the map).
# Calculated here because which_north = "true" fails when the arrow is
# limited to one facet panel
map_bbox <- st_bbox(norway_utm)
arrow_point <- st_sfc(st_point(c(map_bbox[["xmin"]], map_bbox[["ymax"]])),
                      crs = project_crs)
arrow_lonlat <- st_coordinates(st_transform(arrow_point, 4326))
north_point <- st_sfc(st_point(c(arrow_lonlat[1], arrow_lonlat[2] + 0.1)),
                      crs = 4326) |>
  st_transform(project_crs)
north_shift <- st_coordinates(north_point) - st_coordinates(arrow_point)
north_rotation <- -atan2(north_shift[1], north_shift[2]) * 180 / pi

## 1.6. Plot map ---------------------------------------------------------------

# Define colour-blind friendly pallete
inon_palette <- c("Wilderness-like (>5 km)" = "#08519c", # Dark blue
                  "Zone 1 (3-5 km)"         = "#4292c6", # Mid blue
                  "Zone 2 (1-3 km)"         = "#9ecae1", # Light blue
                  "Lost 1988-2023"          = "#e66101", # Orange
                  "Restored 1988-2023"      = "#111111") # Black

# Plot figure
(inon_map <- ggplot() +
    geom_sf(data = norway_utm, fill = "grey88", colour = NA) +
    geom_sf(data = inon_plot, aes(fill = zone), colour = NA) +
    # changes: the fill draws the legend keys, the outline keeps slivers visible
    geom_sf(data = inon_change, aes(fill = change), colour = NA) +
    geom_sf(data = inon_change, aes(colour = change), fill = NA,
            linewidth = 0.2, show.legend = FALSE) +
    geom_sf(data = norway_utm, fill = NA, colour = "grey30",
            linewidth = 0.15) +
    scale_fill_manual(values = inon_palette,
                      limits = names(inon_palette),
                      labels = function(x) str_wrap(x, width = 32),
                      name = "Remote natural areas") +
    scale_colour_manual(values = inon_palette, guide = "none") +
    facet_wrap(~ panel, nrow = 1) +
    # graticule and axis labels in degrees, map drawn in UTM 33N
    coord_sf(crs = project_crs, datum = st_crs(4326), expand = FALSE) +
    scale_x_continuous(breaks = seq(5, 30, by = 5)) +
    scale_y_continuous(breaks = seq(58, 71, by = 2)) +
    annotation_north_arrow(data = annotation_panel,
                           location = "tl", rotation = north_rotation,
                           height = unit(1.2, "cm"), width = unit(1.2, "cm"),
                           pad_x = unit(0.4, "cm"), pad_y = unit(0.4, "cm"),
                           style = north_arrow_fancy_orienteering()) +
    annotation_scale(data = annotation_panel,
                     location = "br", width_hint = 0.25,
                     pad_x = unit(0.4, "cm"), pad_y = unit(0.4, "cm"),
                     text_cex = 0.8) +
    labs(x = NULL, y = NULL) +
    theme_bw(base_size = 10) +
    theme(panel.grid.major = element_line(colour = "grey85", linewidth = 0.2),
          panel.border = element_rect(colour = "black", linewidth = 0.4),
          panel.spacing.x = unit(1, "cm"),
          plot.margin = margin(t = 3, r = 10, b = 3, l = 10, unit = "mm"),
          axis.text = element_text(colour = "black", size = 14),
          strip.background = element_blank(),
          strip.text = element_text(face = "bold", size = 14, hjust = 0),
          legend.title = element_text(face = "bold", size = 13),
          legend.text = element_text(size = 12),
          legend.key.size = unit(0.65, "cm"),
          legend.key = element_rect(colour = "grey40", linewidth = 0.2),
          legend.spacing.y = unit(0.1, "cm"),
          legend.position = "inside",
          legend.position.inside = c(0.98, 0.02),
          legend.justification = c(1, 0),
          legend.background = element_rect(fill = alpha("white", 0.85),
                                           colour = "grey40", linewidth = 0.3),
          legend.margin = margin(5, 6, 5, 6)))

# Save as .png
ggsave(filename = here("figures", "ThesisMethodsFigure_INON_1988_2023_Norway.png"),
       plot = inon_map, width = 400, height = 300, units = "mm", dpi = 600)

# Save as .pdf
ggsave(filename = here("figures", "ThesisMethodsFigure_INON_1988_2023_Norway.pdf"),
       plot = inon_map, width = 360, height = 260, units = "mm")

# END OF SCRIPT ----------------------------------------------------------------