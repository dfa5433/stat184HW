
# ==========================================================
# STAT 184 - HOMEWORK 3
# Reproducing Plots and Exploring Real Spatial Data
# ==========================================================

library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(sf)

dir.create("outputs", showWarnings = FALSE)

vehicles <- ggplot2::mpg
gems <- ggplot2::diamonds

save_plot <- function(name, p, width = 9, height = 6) {
  print(p)
  ggsave(
    filename = file.path("outputs", paste0(name, ".png")),
    plot = p,
    width = width,
    height = height,
    dpi = 300
  )
}

# ==========================================================
# PROBLEM 1A
# ==========================================================

p_a <- ggplot(vehicles, aes(x = displ, y = hwy)) +
  geom_point(aes(color = drv), alpha = 0.65, size = 1.7) +
  geom_smooth(
    method = "lm",
    formula = y ~ x,
    se = FALSE,
    color = "royalblue",
    linewidth = 1
  ) +
  facet_wrap(~drv, nrow = 1) +
  theme_bw() +
  labs(
    x = "Engine displacement (litres)",
    y = "Highway fuel economy (mpg)",
    color = "drv"
  )

save_plot("problem1A", p_a, 11, 4.5)

# Explanation:
# Faceting separates the cars into three drive-type
# panels. Each panel fits its own regression line,
# even though drv is mapped to color only for points.


# ==========================================================
# PROBLEM 1B
# ==========================================================

p_b_count <- ggplot(
  vehicles, aes(x = drv, fill = factor(cyl))
) +
  geom_bar() +
  theme_bw() +
  labs(x = "drv", y = "count", fill = "factor(cyl)")

p_b_prop <- ggplot(
  vehicles, aes(x = drv, fill = factor(cyl))
) +
  geom_bar(position = "fill") +
  theme_bw() +
  labs(
    x = "drv",
    y = "Proportion within drive type",
    fill = "factor(cyl)"
  )

save_plot("problem1B_count", p_b_count)
save_plot("problem1B_proportion", p_b_prop)

drive_counts <- vehicles %>%
  count(drv, name = "n_cars")

print(drive_counts)
write_csv(drive_counts, "outputs/drive_counts.csv")

# Explanation:
# Count bars show how many cars are in each group.
# Proportion bars are normalized to a total of 1.
# Equal heights do not imply equal sample sizes.


# ==========================================================
# PROBLEM 1C
# ==========================================================

# Documented subset: nonmissing carat values <= 3.
# Both histograms use exactly the same observations.

small_gems <- gems %>%
  filter(!is.na(carat), carat <= 3)

diamond_audit <- tibble(
  original = nrow(gems),
  retained = nrow(small_gems),
  excluded = nrow(gems) - nrow(small_gems),
  rule = "Nonmissing carat <= 3"
)

print(diamond_audit)
write_csv(diamond_audit, "outputs/diamond_audit.csv")

p_fine <- ggplot(small_gems, aes(x = carat)) +
  geom_histogram(
    binwidth = 0.01,
    boundary = 0,
    closed = "right",
    fill = "grey55",
    color = "white"
  ) +
  coord_cartesian(xlim = c(0, 3)) +
  scale_x_continuous(breaks = seq(0, 3, 0.5)) +
  theme_bw() +
  labs(
    title = "Diamond carat - bin width 0.01",
    x = "Carat",
    y = "Count"
  )

p_coarse <- ggplot(small_gems, aes(x = carat)) +
  geom_histogram(
    binwidth = 0.5,
    boundary = 0,
    closed = "right",
    fill = "grey55",
    color = "white"
  ) +
  coord_cartesian(xlim = c(0, 3)) +
  scale_x_continuous(breaks = seq(0, 3, 0.5)) +
  theme_bw() +
  labs(
    title = "Diamond carat - bin width 0.5",
    x = "Carat",
    y = "Count"
  )

save_plot("p_fine", p_fine)
save_plot("p_coarse", p_coarse)

exact_carat <- tibble(
  carat = c(0.99, 1.00),
  n = c(
    sum(gems$carat == 0.99),
    sum(gems$carat == 1.00)
  )
)

print(exact_carat)
write_csv(exact_carat, "outputs/exact_carat_counts.csv")

# Explanation:
# Narrow bins reveal peaks at popular carat values.
# Wide bins combine more weights and hide these peaks.


# ==========================================================
# PROBLEM 2
# Chicago Public Library Spatial Analysis
# ==========================================================

library_file <- "raw_data/chicago_libraries.geojson"
boundary_file <- "raw_data/chicago_community_areas.geojson"

stopifnot(
  file.exists(library_file),
  file.exists(boundary_file)
)

library_url <- paste0(
  "https://data.cityofchicago.org/resource/",
  "x8fc-8rcq.geojson"
)

boundary_url <- paste0(
  "https://data.cityofchicago.org/api/v3/views/",
  "igwz-8jzy/query.geojson?accessType=DOWNLOAD"
)

data_sources <- tibble(
  dataset = c(
    "Chicago library locations",
    "Chicago community area boundaries"
  ),
  publisher = c("City of Chicago", "City of Chicago"),
  source_url = c(library_url, boundary_url),
  access_date = rep(as.character(Sys.Date()), 2),
  license_note = c(
    "Verify official publisher terms",
    "Verify official publisher terms"
  ),
  date_note = c(
    "Record update date not verified",
    "Boundary update date not verified"
  )
)

write_csv(data_sources, "data_sources.csv")

raw_sites <- st_read(library_file, quiet = TRUE)
raw_areas <- st_read(boundary_file, quiet = TRUE)

cat("\nRaw library records:", nrow(raw_sites), "\n")
cat("Raw community areas:", nrow(raw_areas), "\n")

stopifnot(
  "branch_" %in% names(raw_sites),
  all(c("area_numbe", "community") %in% names(raw_areas)),
  nrow(raw_sites) >= 20,
  nrow(raw_areas) >= 5,
  all(st_geometry_type(raw_sites) == "POINT"),
  !any(st_is_empty(raw_areas))
)

# The previously checked library snapshot contained
# 82 records. A different count requires review.

if (nrow(raw_sites) != 82) {
  stop("Library count differs from checked snapshot.")
}

raw_sites <- st_transform(raw_sites, 4326)
coords <- st_coordinates(raw_sites)

valid_coord <- is.finite(coords[, 1]) &
  is.finite(coords[, 2]) &
  coords[, 1] >= -180 &
  coords[, 1] <= 180 &
  coords[, 2] >= -90 &
  coords[, 2] <= 90

chicago_coord <- valid_coord &
  coords[, 1] >= -88.1 &
  coords[, 1] <= -87.4 &
  coords[, 2] >= 41.5 &
  coords[, 2] <= 42.1

valid_name <- !is.na(raw_sites$branch_) &
  trimws(as.character(raw_sites$branch_)) != ""

nonempty <- !st_is_empty(raw_sites)

keep <- chicago_coord & valid_name & nonempty

removal_reason <- ifelse(
  !nonempty, "Empty geometry",
  ifelse(
    !valid_coord, "Invalid coordinates",
    ifelse(
      !chicago_coord, "Outside study region",
      ifelse(!valid_name, "Missing name", "Retained")
    )
  )
)

cleaning_audit <- tibble(
  original_row = seq_len(nrow(raw_sites)),
  facility_name = as.character(raw_sites$branch_),
  longitude = coords[, 1],
  latitude = coords[, 2],
  reason = removal_reason
)

write_csv(
  cleaning_audit,
  "outputs/site_cleaning_details.csv"
)

duplicate_coordinates <- sum(
  duplicated(
    as.data.frame(coords[keep, , drop = FALSE])
  )
)

site_cleaning <- tibble(
  raw_records = nrow(raw_sites),
  retained = sum(keep),
  removed = sum(!keep),
  invalid_coordinates = sum(!valid_coord),
  outside_region = sum(valid_coord & !chicago_coord),
  missing_names = sum(!valid_name),
  empty_geometries = sum(!nonempty),
  duplicate_coordinate_records = duplicate_coordinates
)

print(site_cleaning)
write_csv(site_cleaning, "outputs/site_cleaning_summary.csv")

sites <- raw_sites[keep, ]
sites$site_id <- sprintf("LIB%03d", seq_len(nrow(sites)))
sites$facility_name <- as.character(sites$branch_)

stopifnot(
  nrow(sites) >= 20,
  !anyDuplicated(sites$site_id)
)

# NAD83 / UTM zone 16N, EPSG:26916, units = meters.

sites_m <- st_transform(sites, 26916)

# Repair invalid boundaries without unnecessarily
# extracting polygons from already-polygon data.

invalid_before <- sum(!st_is_valid(raw_areas))
empty_before <- sum(st_is_empty(raw_areas))

areas_valid <- st_make_valid(raw_areas)

stopifnot(
  all(st_is_valid(areas_valid)),
  !any(st_is_empty(areas_valid)),
  all(st_geometry_type(areas_valid) %in%
        c("POLYGON", "MULTIPOLYGON"))
)

areas_m <- st_transform(areas_valid, 26916)

areas_m$district_id <- as.character(areas_m$area_numbe)
areas_m$district_name <- as.character(areas_m$community)

areas_m <- areas_m %>%
  select(district_id, district_name)

stopifnot(
  !anyNA(areas_m$district_id),
  !anyNA(areas_m$district_name),
  !anyDuplicated(areas_m$district_id),
  st_crs(sites_m) == st_crs(areas_m)
)

boundary_audit <- tibble(
  raw_areas = nrow(raw_areas),
  cleaned_areas = nrow(areas_m),
  invalid_before_repair = invalid_before,
  empty_before_repair = empty_before,
  invalid_after_repair = sum(!st_is_valid(areas_m)),
  duplicate_ids = sum(duplicated(areas_m$district_id))
)

print(boundary_audit)
write_csv(boundary_audit, "outputs/boundary_audit.csv")

cat("Projected CRS: EPSG 26916\n")
cat("Coordinate units: meters\n")

st_write(
  sites_m, "outputs/sites_m.gpkg",
  delete_dsn = TRUE, quiet = TRUE
)

st_write(
  areas_m, "outputs/areas_m.gpkg",
  delete_dsn = TRUE, quiet = TRUE
)

source_caption <- paste0(
  "City of Chicago Open Data; accessed ",
  Sys.Date(),
  ". Snapshot dates not verified."
)

p_sites <- ggplot() +
  geom_sf(
    data = areas_m,
    fill = "grey95",
    color = "grey70",
    linewidth = 0.2
  ) +
  geom_sf(
    data = sites_m,
    aes(color = "Public library"),
    size = 1.5
  ) +
  scale_color_manual(
    values = c("Public library" = "blue")
  ) +
  theme_bw() +
  labs(
    title = "Chicago Public Library Locations",
    color = "Facility",
    caption = source_caption
  )

save_plot("p_sites", p_sites)

# Buffer unions count overlapping areas once.
# Buffers are NOT clipped to Chicago boundaries.

buffer_500 <- st_sf(
  radius_m = 500,
  geometry = st_union(st_buffer(sites_m, 500))
)

buffer_1000 <- st_sf(
  radius_m = 1000,
  geometry = st_union(st_buffer(sites_m, 1000))
)

buffer_summary <- tibble(
  radius_m = c(500, 1000),
  union_area_km2 = c(
    as.numeric(st_area(buffer_500)) / 1e6,
    as.numeric(st_area(buffer_1000)) / 1e6
  )
)

stopifnot(
  buffer_summary$union_area_km2[2] >=
    buffer_summary$union_area_km2[1]
)

print(buffer_summary)
write_csv(buffer_summary, "outputs/buffer_summary.csv")

st_write(
  buffer_500, "outputs/buffer_500.gpkg",
  delete_dsn = TRUE, quiet = TRUE
)

st_write(
  buffer_1000, "outputs/buffer_1000.gpkg",
  delete_dsn = TRUE, quiet = TRUE
)

p_buffers <- ggplot() +
  geom_sf(
    data = areas_m,
    fill = "grey95",
    color = "grey70",
    linewidth = 0.2
  ) +
  geom_sf(
    data = buffer_500,
    aes(fill = "Within 500 meters"),
    alpha = 0.55,
    color = NA
  ) +
  geom_sf(
    data = sites_m,
    aes(color = "Public library"),
    size = 1.1
  ) +
  scale_fill_manual(
    values = c("Within 500 meters" = "orange")
  ) +
  scale_color_manual(
    values = c("Public library" = "blue")
  ) +
  theme_bw() +
  labs(
    title = "Combined 500-Meter Library Coverage",
    fill = "Buffer coverage",
    color = "Facility",
    caption = source_caption
  )

save_plot("p_buffers", p_buffers)

problem2_text <- paste0(
  "Problem 2 interpretation:\n",
  nrow(sites_m), " library locations were retained.\n",
  "The combined 500-meter buffer covers ",
  round(buffer_summary$union_area_km2[1], 2),
  " square kilometers.\n",
  "The combined 1000-meter buffer covers ",
  round(buffer_summary$union_area_km2[2], 2),
  " square kilometers.\n",
  "Overlapping coverage was counted only once.\n",
  "Coverage outside the study boundary was included.\n"
)

cat("\n", problem2_text)

writeLines(
  problem2_text,
  "outputs/problem2_interpretation.txt"
)


# ==========================================================
# PROBLEM 3
# Community Area Counts and Density
# ==========================================================

matches <- st_intersects(sites_m, areas_m)
match_count <- lengths(matches)

matched_district <- vapply(
  matches,
  function(x) {
    if (length(x) == 1) {
      as.character(areas_m$district_id[x])
    } else {
      NA_character_
    }
  },
  character(1)
)

site_match_audit <- tibble(
  site_id = sites_m$site_id,
  facility_name = sites_m$facility_name,
  n_area_matches = match_count,
  matched_district = matched_district
)

write_csv(site_match_audit, "site_match_audit.csv")

zero_matches <- sum(match_count == 0)
one_match <- sum(match_count == 1)
multiple_matches <- sum(match_count > 1)

cat("\nZero matches:", zero_matches, "\n")
cat("Exactly one match:", one_match, "\n")
cat("Multiple matches:", multiple_matches, "\n")

unassigned_sites <- site_match_audit %>%
  filter(n_area_matches != 1)

print(as.data.frame(unassigned_sites))

write_csv(
  unassigned_sites,
  "outputs/unassigned_sites.csv"
)

assigned_counts <- site_match_audit %>%
  filter(n_area_matches == 1) %>%
  count(matched_district, name = "n_sites")

district_counts <- areas_m %>%
  mutate(
    area_km2 = as.numeric(st_area(geometry)) / 1e6
  ) %>%
  left_join(
    assigned_counts,
    by = c("district_id" = "matched_district")
  ) %>%
  mutate(
    n_sites = coalesce(n_sites, 0L),
    sites_per_km2 = n_sites / area_km2
  )

stopifnot(
  all(district_counts$area_km2 > 0),
  all(!is.na(district_counts$n_sites)),
  sum(district_counts$n_sites) == one_match,
  nrow(district_counts) == nrow(areas_m)
)

district_table <- district_counts %>%
  st_drop_geometry() %>%
  select(
    district_id,
    district_name,
    n_sites,
    area_km2,
    sites_per_km2
  )

# FIXED: No n = Inf argument, which caused the error.

print(as.data.frame(district_table))

write_csv(
  district_table,
  "outputs/district_counts.csv"
)

map_caption <- paste0(
  source_caption,
  " | Sites not assigned exactly once: ",
  zero_matches + multiple_matches
)

shared_bbox <- st_bbox(areas_m)

p_count <- ggplot(district_counts) +
  geom_sf(
    aes(fill = n_sites),
    color = "grey65",
    linewidth = 0.2
  ) +
  scale_fill_gradient(
    low = "white",
    high = "navy",
    na.value = "grey75",
    breaks = seq(0, max(district_counts$n_sites), 1)
  ) +
  coord_sf(
    xlim = c(shared_bbox["xmin"], shared_bbox["xmax"]),
    ylim = c(shared_bbox["ymin"], shared_bbox["ymax"]),
    crs = st_crs(areas_m),
    expand = FALSE
  ) +
  theme_bw() +
  labs(
    title = "Public Library Counts by Community Area",
    fill = "Number of libraries",
    caption = map_caption
  )

save_plot("p_count", p_count)

p_density <- ggplot(district_counts) +
  geom_sf(
    aes(fill = sites_per_km2),
    color = "grey65",
    linewidth = 0.2
  ) +
  scale_fill_gradient(
    low = "white",
    high = "darkgreen",
    na.value = "grey75"
  ) +
  coord_sf(
    xlim = c(shared_bbox["xmin"], shared_bbox["xmax"]),
    ylim = c(shared_bbox["ymin"], shared_bbox["ymax"]),
    crs = st_crs(areas_m),
    expand = FALSE
  ) +
  theme_bw() +
  labs(
    title = "Public Library Density by Community Area",
    fill = "Libraries per km²",
    caption = map_caption
  )

save_plot("p_density", p_density)

p_sites_over_areas <- ggplot() +
  geom_sf(
    data = areas_m,
    fill = "grey95",
    color = "grey65",
    linewidth = 0.2
  ) +
  geom_sf(
    data = sites_m,
    aes(color = "Public library"),
    size = 1
  ) +
  scale_color_manual(
    values = c("Public library" = "red")
  ) +
  theme_bw() +
  labs(
    title = "Libraries and Community Area Boundaries",
    color = "Facility",
    caption = map_caption
  )

save_plot("p_sites_over_areas", p_sites_over_areas)

top_count <- district_table %>%
  arrange(desc(n_sites), district_name) %>%
  slice_head(n = 5)

top_density <- district_table %>%
  arrange(desc(sites_per_km2), district_name) %>%
  slice_head(n = 5)

cat("\nTOP 5 BY LIBRARY COUNT\n")
print(as.data.frame(top_count))

cat("\nTOP 5 BY LIBRARY DENSITY\n")
print(as.data.frame(top_density))

write_csv(top_count, "outputs/top_count.csv")
write_csv(top_density, "outputs/top_density.csv")

max_count <- max(district_table$n_sites)

count_leaders <- district_table %>%
  filter(n_sites == max_count) %>%
  pull(district_name)

density_leader <- district_table %>%
  arrange(desc(sites_per_km2)) %>%
  slice_head(n = 1)

problem3_text <- paste0(
  "Problem 3 interpretation:\n",
  "There are ", nrow(district_table),
  " community areas.\n",
  one_match, " libraries matched exactly one area.\n",
  zero_matches, " matched no area, and ",
  multiple_matches, " matched multiple areas.\n",
  "The highest count is ", max_count,
  " libraries, found in ",
  paste(count_leaders, collapse = ", "), ".\n",
  "The highest density is in ",
  density_leader$district_name,
  ", with ",
  round(density_leader$sites_per_km2, 3),
  " libraries per square kilometer.\n",
  "Counts show the total facilities in an area, ",
  "while density adjusts for area size.\n",
  "Smaller areas can have higher densities even ",
  "when they have fewer total libraries.\n",
  "The facility and boundary datasets may have ",
  "different update dates.\n"
)

cat("\n", problem3_text)

writeLines(
  problem3_text,
  "outputs/problem3_interpretation.txt"
)


# ==========================================================
# FINAL FILE VERIFICATION
# ==========================================================

required_files <- c(
  "data_sources.csv",
  "site_match_audit.csv",
  "outputs/problem1A.png",
  "outputs/problem1B_count.png",
  "outputs/problem1B_proportion.png",
  "outputs/p_fine.png",
  "outputs/p_coarse.png",
  "outputs/p_sites.png",
  "outputs/p_buffers.png",
  "outputs/p_count.png",
  "outputs/p_density.png",
  "outputs/sites_m.gpkg",
  "outputs/areas_m.gpkg",
  "outputs/buffer_500.gpkg",
  "outputs/buffer_1000.gpkg",
  "outputs/buffer_summary.csv",
  "outputs/district_counts.csv",
  "outputs/site_cleaning_summary.csv",
  "outputs/site_cleaning_details.csv",
  "outputs/boundary_audit.csv",
  "outputs/problem2_interpretation.txt",
  "outputs/problem3_interpretation.txt"
)

missing_files <- required_files[
  !file.exists(required_files)
]

if (length(missing_files) > 0) {
  stop(
    "Missing files: ",
    paste(missing_files, collapse = ", ")
  )
}

