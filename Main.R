# Statement ----
# The code is used for Fujisan valuation project. 

# Package ----
pacman::p_load(
  openxlsx, stringr, dplyr, tidyr, ggplot2, geojsonsf,
  sf, tmap, parallel, showtext, patchwork, targets, lubridate
)

# Preparation ----
showtext_auto()
tar_load(c("mob.pref.idnum", "day.mobile", "day.pref.mobile"))

# Bug: What to do with home_prefcode = NA? 
dim(mob.pref.idnum)

# Analysis -----
## First round ----
### Impact factor ----
# Weekend and holiday. 
ggplot(day.mobile) + 
  geom_col(aes(date, num, fill = wknd_hol)) + 
  facet_wrap(.~ year, scales = "free")
ggplot(day.mobile) + 
  geom_boxplot(aes(wknd_hol, num)) + 
  facet_wrap(.~ year, scales = "free")
# Let's take a look at the values. 
day.mobile %>% 
  group_by(wknd_hol) %>% 
  summarise(num_mean = mean(num), num_sd = sd(num)) %>% 
  ungroup()
# Bug: Though values show the diff, but might need a further statistical analysis for the comparison. 

# Weather
ggplot(day.mobile) + 
  geom_col(aes(date, num, fill = wthr)) + 
  facet_wrap(.~ year, scales = "free")
# Bug: Can also analyze the relationship between stay duration and weather, suppose that when weather is good, the duration of stay should be longer. 
ggplot(day.mobile) + 
  geom_boxplot(aes(wthr, num)) + 
  facet_wrap(.~ year, scales = "free")

# Distance or cost. 
# Where do they come from: suppose most people come from surrounding areas. 
day.pref.mobile %>% 
  group_by(home_prefcode) %>% 
  summarise(num = sum(num)) %>% 
  ungroup() %>% 
  arrange(-num) %>% 
  left_join(pref.city.code %>% select(prefcode, prefname) %>% distinct(), 
            by = c("home_prefcode" = "prefcode")) %>% 
  # pick the top 20 to plot 
  head(20) %>% 
  ggplot() + 
  geom_col(aes(reorder(prefname, num), num)) + 
  coord_flip()
# The prefectures near Fujisan mountain rank top. 
# Bug: The number need to be validated with the percentage of phone users of each prefecture. What to do with home_prefcode = NA? To visualize it with geo-sanky diagram

### Climbing route ----
# Test what the data look like, which id has more data records. 
gis.mobile %>% 
  st_drop_geometry() %>% 
  group_by(dailyid) %>% 
  summarise(num = n()) %>% 
  ungroup() %>% 
  arrange(-num)
# Bug: Need to figure out: why some dailyid have so many records? 

# Visualization the routes. 
# It is impossible to get the route of each dailyid since there are so many dailyid, but if we pick some of them, we can understand how they get to the top. 
# tm_shape(mesh.fujisan) + 
#   tm_fill(col = "G04c_002") + 
#   tm_shape(gis.mobile) + 
#   tm_dots(col = "dailyid") + 
#   tm_shape(mt.point) + 
#   tm_dots(col = "black", size = 0.3)

# If set the route record with color and alpha, maybe we can see which route people use most. But still, there are so many records, it doesn't really work. 
gis.mobile[sample(1:nrow(gis.mobile), 1000), ] %>% 
  st_crop(xmin = 138.5, ymin = 35.3333, xmax = 138.8, ymax = 35.5) %>% 
  tm_shape() + 
  tm_dots(col = "red", alpha = 0.1) + 
  tm_shape(mt.point) + 
  tm_dots(col = "location", size = 0.3) + 
  tm_compass(position = c("right", "top")) + 
  tm_scale_bar() + 
  tm_layout(legend.outside = TRUE)
# Bug: Maybe the best way to visualize the data is to calculate before mapping. 

### Evaluate travel cost ----
# The dailyid changes everyday. For calculation convenience, assume different dailyid are different people. The assumption results into a over-estimate of the number of visitors. For example, if a visitor stay in the mountain overnight, they will be caculated as two visitors. We can calibrate that by buffuer zone and route, to merge the dailyid of a same visitor. 

# Per capital cost ~ number of dailyid. 
# Bug: Need to calculate the total value. Need validate the total value with percentage of smart phone user etc. - read data explanation again . 
mob.pref.idnum %>% 
  group_by(pref_agg, cost) %>% 
  summarise(num = sum(num)) %>% 
  ungroup() %>% 
  ggplot() + 
  geom_point(aes(cost, num)) + 
  geom_text(aes(x = cost, y = num + 200, label = pref_agg))

# Total cost of each pref_agg: which prefecture contribute most to Fujisan? 
# Bug: Just calculate the total cost of the travelers with mobile data here, which should be validated with percentage of mobile phone use etc. 
mob.pref.idnum %>% 
  group_by(prefname) %>% 
  summarise(tot_cost = sum(tot_cost)) %>% 
  arrange(-tot_cost) %>% 
  # get the top 20
  head(20) %>% 
  ggplot() + 
  geom_col(aes(reorder(prefname, tot_cost), tot_cost)) + 
  coord_flip()

## Second round ----
### Layers for ranges ----
zoom.bbox <- 
  st_crop(yamashizu, xmin = 138.5, ymin = 35.3, xmax = 138.9, ymax = 35.5)
tm_shape(yamashizu, bbox = st_bbox(zoom.bbox)) + 
  tm_polygons(col = "#B4CCD2") + 
  tm_shape(nps.yamashizu) + 
  tm_polygons(col = "#8ABDC9") + 
  tm_shape(range.top) + 
  tm_polygons(col = "#04819E") + 
  tm_shape(mesh.trail.head.buff) + 
  tm_polygons(col = "yellow") + 
  tm_shape(mesh.trail.head) + 
  tm_polygons(col = "orange") + 
  tm_shape(mt.point) + 
  tm_dots(col = "red")

# Bug: To speed up, I take part of the raw data. 
# Bug: Actually I should transform the CRS at the very begining for gis.mobile rather than this raw data sample 
gis.mobile.smp <- gis.mobile[1:50000, ] %>% 
  st_transform(kCRS)

gis.mobile.smp.group <- 
  gis.mobile.smp %>% 
  # The location of each log. 
  mutate(
    top = as.logical(st_intersects(., range.top, sparse = FALSE)), 
    trail = as.logical(st_intersects(., nps.yamashizu, sparse = FALSE)),
    general = 
      as.logical(st_intersects(., yamashizu %>% select(geometry), sparse = FALSE))
  ) %>% 
  # Categorize the logs based on locations. 
  st_drop_geometry() %>% 
  group_by(dailyid) %>% 
  summarise(across(c(top, trail, general), sum)) %>% 
  ungroup() %>% 
  # Bug: There are many dailyid do not intersect with all the range. 
  mutate(group = case_when(
    top > 0 ~ "top", 
    trail > 0 ~ "trail", 
    TRUE ~ "general"
  )) %>% 
  select(dailyid, group)
dim(gis.mobile.smp.group)
table(gis.mobile.smp.group$group)

# Add group info to raw data.  
gis.mobile.smp <- gis.mobile.smp %>% 
  left_join(gis.mobile.smp.group, by = "dailyid")

### Choice of trail head ----
# For visitors climb the mountain: which trail head? 
# Get visitor climb the mountain from the sample data. 
# Bug: That is the second sample data. 
gis.mobile.smp.climb <- 
  gis.mobile.smp %>% 
  subset(group %in% c("trail", "top"))
# Bug: Should I expand the range of the meshes? 
# Get trail head choice for each visitor. 
gis.mobile.smp.climb.headgrp <- 
  st_intersection(gis.mobile.smp.climb, mesh.trail.head) %>% 
  st_drop_geometry() %>% 
  tibble() %>% 
  select(dailyid, head) %>% 
  group_by(dailyid, head) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = dailyid, names_from = head, 
              values_from = n, values_fill = 0) %>% 
  select(dailyid, 吉田, 富士宮, 御殿場, 須走) %>% 
  mutate(route = case_when(
    吉田 > 0 ~ "吉田", 
    富士宮 > 0 ~ "富士宮", 
    御殿場 > 0 ~ "御殿場", 
    須走 > 0 ~ "須走" 
  )) %>% 
  select(dailyid, route)
gis.mobile.smp.climb <- 
  gis.mobile.smp.climb %>% 
  left_join(gis.mobile.smp.climb.headgrp, by = "dailyid")

# Visualize the choice of trail head. 
# Bug: The circle has zig-zags - related to CRS choice. 
zoom.bbox <- 
  st_crop(yamashizu, xmin = 138.7, ymin = 35.32, xmax = 138.82, ymax = 35.42)
tm_shape(yamashizu, bbox = st_bbox(zoom.bbox)) + 
  tm_polygons(col = "#B4CCD2") + 
  tm_shape(nps.yamashizu) + 
  tm_polygons(col = "#8ABDC9") + 
  tm_shape(range.top) + 
  tm_polygons(col = "#04819E") + 
  tm_shape(mesh.trail.head) + 
  tm_polygons(col = "orange") + 
  tm_shape(gis.mobile.smp.climb) + 
  tm_dots(
    col = "route", alpha = 0.6, 
    palette = c(吉田 = "yellow", 御殿場 = "brown", 須走 = "green", 富士宮 = "blue")
  ) + 
  tm_layout(legend.outside = TRUE)

### Route-related attributes ----
# Like distance, time, money cost, etc. 
# All related to home city, so we get the home city first. 
# Variables for potential model. 
gis.mobile.smp.climb.var <- 
  gis.mobile.smp.climb %>% 
  st_drop_geometry() %>% 
  # add visitor group info
  tibble() %>% 
  select(dailyid, route, home_prefcode) %>% 
  distinct() %>% 
  left_join(gis.mobile.smp.group, by = "dailyid") %>% 
  # add prefecture info
  left_join(pref.city.code %>% select(prefcode, prefname) %>% distinct(), 
            by = c("home_prefcode" = "prefcode")) %>% 
  mutate(dailyid = paste0(substr(dailyid, 1, 5), "..")) %>% 
  select(-home_prefcode)

# Description stat. 
# Visitor group * home prefecture. 
gis.mobile.smp.climb.var %>% 
  group_by(group, prefname) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  ggplot() + 
  geom_col(aes(prefname, n, fill = group), position = "dodge") + 
  theme(axis.text.x = element_text(angle = 90))

# Visitor group * route choice. 
gis.mobile.smp.climb.var %>% 
  group_by(group, route) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  ggplot() + 
  geom_col(aes(route, n, fill = group), position = "dodge") + 
  theme(axis.text.x = element_text(angle = 90))

# Route choice * home prefecture. 
gis.mobile.smp.climb.var %>% 
  group_by(route, prefname) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  ggplot() + 
  geom_col(aes(prefname, n, fill = route), position = "dodge") + 
  theme(axis.text.x = element_text(angle = 90))

## Logit ----
# Agoop data. 
# Bug: Raw rds data come cancel-behavior study, should change to original data. 
agoop <- readr::read_rds("data_raw/df_agoop.rds")

# The attributes of interest of visitors. 
visitor.attr <- agoop %>%
  dplyr::select(dailyid, date, home_prefcode, home_citycode,
                workplace_prefcode, workplace_citycode, gender) %>%
  distinct()

# Ranges. 
# Divide research area into several scopes or ranges (see Kang's note). 
# Bug: Should add the note later. 
# From outside to inside. 

# Yamanashi-Shizuoka prefecture. 
range.yamashizu <-
  # Data for all prefectures of Japan. 
  geojson_sf("data_raw/prefectures.geojson") %>%
  # Get prefecture polygons of Yamanashi and Shizuoka. 
  subset(name %in% c("山梨県", "静岡県")) %>%
  st_transform(kCRS) %>%
  # union the two prefectures
  st_union() %>%
  st_sf()

# Data area: when people enter this area, their mobilization data will be collected. 
data.area.tif <- raster("data_raw/DataGeorange/data_georange_spe_test.tif")
st_bbox(data.area.tif)
# Bug: Must have some better way to do that. 
range.data.area <-
  rbind(c(138.66777, 35.28770), c(138.66777, 35.42516),
        c(138.82952, 35.42516), c(138.82952, 35.28770),
        c(138.66777, 35.28770)) %>%
  list() %>%
  st_polygon() %>%
  st_sfc(crs = "EPSG: 6668") %>%
  st_sf()

# Trail head meshes. 
range.trailhead <- st_read(
  dsn = "data_raw/Mesh/G04-a-11_5338-jgd_GML",
  layer = "G04-a-11_5338-jgd_ElevationAndSlopeAngleTertiaryMesh"
) %>%
  st_set_crs(kCRS) %>%
  subset(G04a_001 %in% c("53380508", "53380603", "53380642", "53380578")) %>%
  mutate(head = case_when(
    G04a_001 == "53380508" ~ "富士宮",
    G04a_001 == "53380603" ~ "御殿場",
    G04a_001 == "53380642" ~ "須走",
    G04a_001 == "53380578" ~ "吉田"
  ))
# Bug: Is "須走" 0642 or 0632?

# Between trail heads and core: mountain. 
# Bug: Trail head can be located inside or outside the mountain area - re-define the scope more precisely later. 
range.nps <-
  st_read(dsn = "data_raw/NationalPark/nps", layer = "nps_all") %>%
  subset(名称 == "富士箱根伊豆") %>%
  st_transform(kCRS) %>%
  st_make_valid() %>%
  st_union() %>%
  st_sf() %>%
  st_crop(st_bbox(range.data.area))
# Bug: There is a small piece in the east. 

# Core: mountain top mesh. 
range.top <-
  st_read(
    dsn = "data_raw/Mesh/G04-a-11_5338-jgd_GML",
    layer = "G04-a-11_5338-jgd_ElevationAndSlopeAngleTertiaryMesh"
  ) %>%
  subset(G04a_001 == "53380538") %>%
  st_set_crs(kCRS)

# Target trail head mesh IDs and location names. 
mesh.name <- matrix(
  c(
    "fujinomiya", 53380508,
    "Gotenba", 53380603,
    "subashiri", 53380632,
    "yoshida", 53380578
  ),
  byrow = TRUE,
  ncol = 2
) %>%
  data.frame() %>%
  rename_with(~ c("mesh_name", "mesh_id"))

# Take a look at each range. 
tm_shape(range.yamashizu) +
  tm_polygons(col = "#C6FA8E") +
  tm_shape(range.data.area) +
  tm_polygons(col = "#9CF6AF") +
  tm_shape(range.nps) +
  tm_polygons(col = "lightblue") +
  tm_shape(range.trailhead) +
  tm_polygons(col = "blue") +
  tm_shape(range.top) +
  tm_polygons(col = "darkblue")
# Bug: maybe I should eliminate range.yamashizu? Let's try to do that ... 

# Actual locations of logs. 
# Turn raw data into simple feature for GIS analysis. 
agoop.gis <- agoop %>%
  st_as_sf(coords = c("longitude", "latitude")) %>%
  # add projection
  st_set_crs(kCRS) %>%
  # add log location information
  left_join(mesh.name, by = c("mesh3" = "mesh_id")) %>%
  mutate(
    range_data_area = as.logical(
      st_intersects(., range.data.area, sparse = FALSE)),
    range_nps = as.logical(
      st_intersects(., range.nps, sparse = FALSE)),
    range_top = as.logical(
      st_intersects(., range.top, sparse = FALSE))
  ) %>%
  # id the location of logs
  mutate(range = case_when(
    range_top ~ "top",
    !is.na(mesh_name) ~ mesh_name,
    range_nps ~ "nps",
    range_data_area ~ "data", 
    TRUE ~ "outside"
  ))
# Take a look at the data. 
tm_shape(head(agoop.gis, 5000)) +
  tm_dots(col = "range")

# Get all actually used routes and target data. 
course <-
  agoop.gis %>% 
  st_drop_geometry() %>%
  tibble() %>%
  # Only keep the arriving time of each dailyid at each range segmentally. 
  # Basic idea to achieve that goal: for a certain dailyid, the dailyid of his first log is different with the last one; and when he enter a new mesh, his "range" value changes. 
  # Divide the logs into periods. 
  arrange(dailyid, datetime, accuracy) %>%
  # Solve "2 logs of different locations at same time" problem: if a dailyid has more than one log at a datetime, keep the most accurate one. 
  group_by(dailyid, datetime) %>%
  mutate(
    rowid = row_number()
  ) %>%
  # Bug: Without time info of second, might cause problem: one visitor in one place at 15:01:01 but arrive another place at 15:01:45, especially when s/he is using high-speed transportation like Shinkansen (5 km/min); even by walking, the speed is about 80 m/min. 
  ungroup() %>%
  filter(rowid == 1) %>%
  mutate(
    dailyid_chg = (lag(dailyid) == dailyid),
    range_chg = (lag(range) == range)
  ) %>%
  filter(
    # When dailyid changes, it should be kept since it indicates a new dailyid. 
    dailyid_chg == FALSE |
      # Should also keep the first row "NA". 
      is.na(dailyid) |
      # When mesh name changes, it indicates the dailyid enters a new mesh. 
      range_chg == FALSE |
      # Should also keep the first row "NA". 
      is.na(range_chg)
  ) %>%
  # Get route and related time of each dailyid. 
  group_by(dailyid) %>%
  summarise(route = paste(range, collapse = "_"),
            time = paste(datetime, collapse = "_")) %>%
  # Get start time, end time, and duration (if applicable) for each course. 
  group_by(dailyid) %>%
  mutate(
    start_mesh = str_split(route, "_")[[1]][1],
    end_mesh = tail(str_split(route, "_")[[1]], 1),
    start_time = as_datetime(str_split(time, "_")[[1]][1]),
    end_time = as_datetime(tail(str_split(time, "_")[[1]], 1)),
    duration = end_time - start_time
  ) %>%
  ungroup() %>%
  # Re-classification: cancel- and peak-group. 
  mutate(visitor_grp = case_when(
    # Cancel: outside_*_!top_*_outside. 
    start_mesh == "outside" &
      end_mesh == "outside" &
      !grepl("top", route) ~ "sure_cancel",
    # Cancel: !outside_*_!top_*_!outside. 
    start_mesh != "outside" &
      end_mesh != "outside" &
      !grepl("top", route) &
      grepl("_", route) ~ "may_cancel",
    # Outside_*_top_*_outside. 
    start_mesh == "outside" &
      end_mesh == "outside" &
      grepl("top", route) ~ "peak",
    # !outside_*_top_*_!outside. 
    start_mesh != "outside" &
      end_mesh != "outside" &
      grepl("_top_", route) ~ "peak",
    # Others. 
    TRUE ~ "other"
  )) %>%
  # Join visitor attributes into route data. 
  # Bug: A warning: Each row in `x` is expected to match at most 1 row in `y`. Row 114 of `x` matches multiple rows.
  left_join(visitor.attr, by = "dailyid") %>% 
  # Add times of passing each trail head. 
  # Bug: Fix "Gotenba" to "gotenba", as well as the "Gotenba" in mesh.name data.frame. 
  mutate(
    yoshida = str_count(route, "yoshida"), 
    subashiri = str_count(route, "subashiri"), 
    gotenba = str_count(route, "Gotenba"), 
    fujinomiya = str_count(route, "fujinomiya")
  )

# Most dailyids pass no or one trail head. 
course %>% 
  select(dailyid, fujinomiya, gotenba, subashiri, yoshida) %>% 
  pivot_longer(cols = c(fujinomiya, gotenba, subashiri, yoshida), 
               names_to = "trail_head", values_to = "pass_time") %>% 
  mutate(pass = pass_time > 0) %>% 
  group_by(dailyid) %>% 
  summarise(pass_num = sum(pass)) %>% 
  ungroup() %>% 
  pull(pass_num) %>% 
  table()
# For now only consider the first trail head they go by. 

# Extract first trail head they go by. 
visitor.trailhead <- agoop.gis %>% 
  st_drop_geometry() %>%
  tibble() %>%
  # Only keep the arriving time of each dailyid at each range segmentally. 
  # Basic idea to achieve that goal: for a certain dailyid, the dailyid of his first log is different with the last one; and when he enter a new mesh, his "range" value changes. 
  # Divide the logs into periods. 
  arrange(dailyid, datetime, accuracy) %>%
  # Solve "2 logs of different locations at same time" problem: if a dailyid has more than one log at a datetime, keep the most accurate one. 
  group_by(dailyid, datetime) %>%
  mutate(rowid = row_number()) %>%
  ungroup() %>%
  filter(rowid == 1) %>%
  # Keep the first trail head s/he goes by. 
  filter(range %in% c("fujinomiya", "Gotenba", "subashiri", "yoshida")) %>% 
  group_by(dailyid) %>% 
  mutate(rowid = row_number()) %>% 
  ungroup() %>% 
  filter(rowid == 1) %>% 
  select(-rowid) %>% 
  rename(trailhead = range)

# Add first-trailhead choice into course data.frame. 
course <- course %>% 
  left_join(visitor.trailhead)

# Pref names. 
prefcode <- read.csv("data_raw/prefcode_citycode_master_UTF-8.csv") %>%
  dplyr::select(prefcode, prefname) %>%
  mutate(prefcode = as.character(prefcode)) %>% 
  distinct()

# Home distance. 
osrm_res <- 
  readr::read_rds("data_raw/prefecture_office_to_4route_entrance.rds") %>% 
  mutate(trailhead = case_when(
    route == "吉田" ~ "yoshida", 
    route == "須走" ~ "subashiri", 
    route == "御殿場" ~ "Gotenba", 
    # bug: should change "Gotenba" into "gotemba"
    route == "富士宮" ~ "fujinomiya"
  )) %>% 
  select(-route)

# Weather. 
jma <- readr::read_rds("data_raw/jma.rds") %>% 
  # For simplification, use one station data for now - should use the dat a of the station near the target trail head. 
  filter(station == "Yamanaka") %>% 
  select(date, rain_daily_total_mm)

# Holidays in Japan. 
holiday <- rbind(
  read.csv("data_raw/Japan_holiday_2018.csv") %>% 
    mutate(
      year = 2018, month = substr(.$月日, 1, 2), day  = substr(.$月日, 4, 5)
    ), 
  read.csv("data_raw/Japan_holiday_2019.csv") %>% 
    mutate(
      year = 2019, month = substr(.$月日, 1, 2), day  = substr(.$月日, 4, 5)
    )
) %>% 
  rename(holiday_name = 名称) %>% 
  mutate(date = as.Date(paste(year, month, day, sep = "-"))) %>% 
  select(date, holiday_name)

# Model input data. 
course.logit <- 
  course %>% 
  select(dailyid, year, date, datetime, route, 
         trailhead, visitor_grp, home_prefcode, dayofweek) %>% 
  # Bug: NAs in dayofweek column of raw data. 
  mutate(dayofweek = wday(date, week_start = 1)) %>% 
  mutate(home_prefcode = as.character(home_prefcode)) %>% 
  # Home distance. 
  left_join(prefcode, by = c("home_prefcode" = "prefcode")) %>% 
  left_join(osrm_res, by = c("prefname" = "src_loc", "trailhead")) %>% 
  # Weather. 
  left_join(jma, by = "date") %>% 
  # Weekend or holiday. 
  left_join(holiday, by = "date") %>% 
  mutate(holi_wknd = case_when(
    dayofweek > 5 ~ TRUE, 
    !is.na(holiday_name) ~ TRUE, 
    TRUE ~ FALSE
  ))
# Bug: Why there are NAs in "year" column of course data.frame. 

# Analysis ----
# General ----
# Number of dailyid of each trailhead by pref and distance. 
course.logit %>% 
  filter(!is.na(prefname)) %>% 
  group_by(trailhead, prefname, distance) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  group_by(prefname) %>% 
  mutate(distance = mean(distance, na.rm = TRUE)) %>% 
  ungroup() %>% 
  ggplot() + 
  geom_point(aes(reorder(prefname, distance), n, col = trailhead)) + 
  coord_flip()
# Number of dailyid of each trailhead by pref and duration. 
course.logit %>% 
  filter(!is.na(prefname)) %>% 
  group_by(trailhead, prefname, duration) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  group_by(prefname) %>% 
  mutate(duration = mean(duration, na.rm = TRUE)) %>% 
  ungroup() %>% 
  ggplot() + 
  geom_point(aes(reorder(prefname, duration), n, col = trailhead)) + 
  coord_flip()

course.logit %>% 
  select(trailhead, prefname, duration) %>% 
  distinct() %>% 
  ggplot() + 
  geom_point(aes(prefname, duration, col = trailhead)) + 
  coord_flip()

course.logit %>% 
  select(trailhead, prefname, duration) %>% 
  distinct() %>% 
  ggplot() + 
  geom_point(aes(prefname, duration, col = trailhead)) + 
  coord_flip()
# Relative-diff. 
course.logit %>% 
  filter(!is.na(distance), !is.na(prefname)) %>% 
  dplyr::select(trailhead, prefname, distance) %>% 
  group_by(prefname) %>% 
  mutate(distance = distance / min(distance)) %>% 
  ungroup() %>% 
  ggplot() + 
  geom_point(aes(prefname, distance, col = trailhead)) + 
  coord_flip()

## Logit of trailhead choice. 
# Multi-nomial logit of data with trailhead. 
# Further process input data: keep rows with trailhead choices; mutate new trailhead variable by distance to source prefecture. 
dist.pref.trailhead <- course.logit %>% 
  filter(!is.na(distance)) %>% 
  select(prefname, trailhead, distance) %>% 
  distinct() %>% 
  arrange(prefname, distance) %>% 
  group_by(prefname) %>% 
  mutate(trailhead_var = row_number()) %>% 
  ungroup()
course.logit2 <- course.logit %>% 
  filter(!is.na(distance)) %>% 
  left_join(dist.pref.trailhead)

# Number of dailyid of each trailhead by pref and distance. 
# Scaled by min. 
course.logit2 %>% 
  group_by(trailhead_var, prefname, distance) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  group_by(prefname) %>% 
  mutate(
    distance = mean(distance, na.rm = TRUE), 
    n = n / min(n)
  ) %>% 
  ungroup() %>% 
  ggplot() + 
  geom_point(aes(reorder(prefname, distance), n, 
                 col = as.character(trailhead_var))) + 
  coord_flip()
# Scaled by max. 
course.logit2 %>% 
  group_by(trailhead_var, prefname, distance) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  group_by(prefname) %>% 
  mutate(
    distance = mean(distance, na.rm = TRUE), 
    n = n / max(n)
  ) %>% 
  ungroup() %>% 
  ggplot() + 
  geom_point(aes(reorder(prefname, distance), n, 
                 col = as.character(trailhead_var))) + 
  coord_flip()

# Ordinal logit model. 
library(MASS)
course.logit2$trailhead_var <- 
  factor(course.logit2$trailhead_var, levels = 1:4)
course.logit2 <- course.logit2 %>% 
  filter(visitor_grp != "other", visitor_grp != "may_cancel")
course.logit2.model <- polr(
  trailhead_var ~ visitor_grp + rain_daily_total_mm + holi_wknd + distance, 
  data = course.logit2, Hess = TRUE
)
summary(course.logit2.model)
# Bug: should test the assumption: 
# https://stats.oarc.ucla.edu/r/dae/ordinal-logistic-regression/

# The p-value. 
ctable <- coef(summary(course.logit2.model))
ctable
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
p
ctable <- cbind(ctable, "p_value" = p) %>% 
  data.frame() %>% 
  mutate(p_mark = case_when(
    p_value < 0.001 ~ "***", 
    p_value < 0.01 ~ "**", 
    p_value < 0.05 ~ "*", 
    TRUE ~ ""
  ))
ctable

# Predicted values with hypothetical dataset. 
newdat <- data.frame(
  rain_daily_total_mm = 
    rep(mean(course.logit2$rain_daily_total_mm), 400), 
  visitor_grp = rep(c("sure_cancel", "peak"), 200), 
  holi_wknd = rep(c(TRUE, FALSE), each = 200), 
  # Distance: up to 3rd quantile? 
  distance = rep(seq(from = 1, to = 222, length.out = 100), 4)
) %>% 
  cbind(predict(course.logit2.model, ., type = "probs"))
head(newdat)

newdat %>% 
  pivot_longer(cols = c(`1`, `2`, `3`, `4`), 
               names_to = "trailhead", values_to = "prob") %>% 
  ggplot() + 
  geom_line(aes(distance, prob, color = trailhead)) + 
  facet_grid(visitor_grp ~ holi_wknd)
