# Package ----
# for certain data type
library(readr)
library(lubridate)
library(stringr)
library(sf)
library(raster)
library(stars)
library(geojsonsf)
# data tidy
library(dplyr)
# visualization
library(ggplot2)
library(tmap)
library(showtext)

# Setting ----
showtext_auto()

# Data ----
## Constant ----
# default CRS for the project: JGD2011
kCRS <- 6668
# note: EPSG for JGD2000 is 4612

## Agoop data ----
# get data from Kishida's code
# bug: raw *.rds data come from cancel-behavior research, should change back to original data later
agoop <- read_rds("RawData/agoop.rds")

# the attributes of interest of visitors
visitor.attr <- agoop %>%
  dplyr::select(dailyid, date, home_prefcode, home_citycode,
                workplace_prefcode, workplace_citycode, gender) %>%
  distinct()

## Ranges ----
# divide research area into several scopes or ranges (see Kang's note)
# bug: should add the note later
# from outside to inside

# Yamanashi-Shizuoka prefecture
range.yamashizu <-
  # data for all prefectures of Japan
  geojson_sf("RawData/prefectures.geojson") %>%
  # get prefecture polygons of Yamanashi and Shizuoka
  subset(name %in% c("山梨県", "静岡県")) %>%
  st_transform(kCRS) %>%
  # union the two prefectures
  st_union() %>%
  st_sf()

# data area: when people enter this area, their mobilization data will be collected
data.area.tif <- raster("RawData/DataGeorange/data_georange_spe_test.tif")
st_bbox(data.area.tif)
# bug: must have some better way to do that
range.data.area <-
  rbind(c(138.66777, 35.28770), c(138.66777, 35.42516),
        c(138.82952, 35.42516), c(138.82952, 35.28770),
        c(138.66777, 35.28770)) %>%
  list() %>%
  st_polygon() %>%
  st_sfc(crs = "EPSG: 6668") %>%
  st_sf()

# trail head meshes
range.trailhead <- st_read(
  dsn = "RawData/Mesh/G04-a-11_5338-jgd_GML",
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
# bug: is "須走" 0642 or 0632?

# between trail heads and core: mountain
# bug: trail head can be located inside or outside the mountain area - re-define the scope more precisely later
range.nps <-
  st_read(dsn = "RawData/NationalPark/nps", layer = "nps_all") %>%
  subset(名称 == "富士箱根伊豆") %>%
  st_transform(kCRS) %>%
  st_make_valid() %>%
  st_union() %>%
  st_sf() %>%
  st_crop(st_bbox(range.data.area))
# bug: there is a small piece in the east

# core: mountain top mesh
range.top <-
  st_read(
    dsn = "RawData/Mesh/G04-a-11_5338-jgd_GML",
    layer = "G04-a-11_5338-jgd_ElevationAndSlopeAngleTertiaryMesh"
  ) %>%
  subset(G04a_001 == "53380538") %>%
  st_set_crs(kCRS)

# target trail head mesh IDs and location names
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

# take a look at each range
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
# bug: maybe I should eliminate range.yamashizu? Let's try to do that ... 

## Actual locations of logs ----
# turn raw data into simple feature for GIS analysis
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
# take a look at the data
tm_shape(head(agoop.gis, 5000)) +
  tm_dots(col = "range")

# get all actually used routes and target data
course <-
  agoop.gis %>% 
  st_drop_geometry() %>%
  tibble() %>%
  # only keep the arriving time of each dailyid at each range segmentally
  # basic idea to achieve that goal: for a certain dailyid, the dailyid of his first log is different with the last one; and when he enter a new mesh, his "range" value changes
  # divide the logs into periods
  arrange(dailyid, datetime, accuracy) %>%
  # solve "2 logs of different locations at same time" problem: if a dailyid has more than one log at a datetime, keep the most accurate one
  group_by(dailyid, datetime) %>%
  mutate(
    rowid = row_number()
  ) %>%
  # bug: without time info of second, might cause problem: one visitor in one place at 15:01:01 but arrive another place at 15:01:45, especially when s/he is using high-speed transportation like Shinkansen (5 km/min); even by walking, the speed is about 80 m/min
  ungroup() %>%
  filter(rowid == 1) %>%
  mutate(
    dailyid_chg = (lag(dailyid) == dailyid),
    range_chg = (lag(range) == range)
  ) %>%
  filter(
    # when dailyid changes, it should be kept since it indicates a new dailyid
    dailyid_chg == FALSE |
      # should also keep the first row "NA"
      is.na(dailyid) |
      # when mesh name changes, it indicates the dailyid enters a new mesh
      range_chg == FALSE |
      # should also keep the first row "NA"
      is.na(range_chg)
  ) %>%
  # get route and related time of each dailyid
  group_by(dailyid) %>%
  summarise(route = paste(range, collapse = "_"),
            time = paste(datetime, collapse = "_")) %>%
  # get start time, end time, and duration (if applicable) for each course
  group_by(dailyid) %>%
  mutate(
    start_mesh = str_split(route, "_")[[1]][1],
    end_mesh = tail(str_split(route, "_")[[1]], 1),
    start_time = as_datetime(str_split(time, "_")[[1]][1]),
    end_time = as_datetime(tail(str_split(time, "_")[[1]], 1)),
    duration = end_time - start_time
  ) %>%
  ungroup() %>%
  # re-classification: cancel- and peak-group
  mutate(visitor_grp = case_when(
    # cancel: outside_*_!top_*_outside
    start_mesh == "outside" &
      end_mesh == "outside" &
      !grepl("top", route) ~ "sure_cancel",
    # cancel: !outside_*_!top_*_!outside
    start_mesh != "outside" &
      end_mesh != "outside" &
      !grepl("top", route) &
      grepl("_", route) ~ "may_cancel",
    # outside_*_top_*_outside
    start_mesh == "outside" &
      end_mesh == "outside" &
      grepl("top", route) ~ "peak",
    # !outside_*_top_*_!outside
    start_mesh != "outside" &
      end_mesh != "outside" &
      grepl("_top_", route) ~ "peak",
    # others
    TRUE ~ "other"
  )) %>%
  # join visitor attributes into route data
  left_join(visitor.attr, by = "dailyid") %>% 
  # add times of passing each trail head
  # bug: need to fix "Gotenba" to "gotenba", as well as the "Gotenba" in mesh.name data.frame
  mutate(
    yoshida = str_count(route, "yoshida"), 
    subashiri = str_count(route, "subashiri"), 
    gotenba = str_count(route, "Gotenba"), 
    fujinomiya = str_count(route, "fujinomiya")
  )

# most dailyids pass no or one trail head 
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
# for now only consider the first trail head they go by 

# extract first trail head they go by 
visitor.trailhead <- agoop.gis %>% 
  st_drop_geometry() %>%
  tibble() %>%
  # only keep the arriving time of each dailyid at each range segmentally
  # basic idea to achieve that goal: for a certain dailyid, the dailyid of his first log is different with the last one; and when he enter a new mesh, his "range" value changes
  # divide the logs into periods
  arrange(dailyid, datetime, accuracy) %>%
  # solve "2 logs of different locations at same time" problem: if a dailyid has more than one log at a datetime, keep the most accurate one
  group_by(dailyid, datetime) %>%
  mutate(rowid = row_number()) %>%
  ungroup() %>%
  filter(rowid == 1) %>%
  # keep the first trail head s/he goes by 
  filter(range %in% c("fujinomiya", "Gotenba", "subashiri", "yoshida")) %>% 
  group_by(dailyid) %>% 
  mutate(rowid = row_number()) %>% 
  ungroup() %>% 
  filter(rowid == 1) %>% 
  select(-rowid) %>% 
  rename(trailhead = range)

# add first-trailhead choice into course data.frame
course <- course %>% 
  left_join(visitor.trailhead)

## Pref names ----
prefcode <- read_csv("RawData/prefcode_citycode_master_UTF-8.csv") %>%
  dplyr::select(prefcode, prefname) %>%
  distinct()

## Home distance ----
osrm_res <- 
  read_rds("RawData/prefecture_office_to_4route_entrance.rds") %>% 
  mutate(trailhead = case_when(
    route == "吉田" ~ "yoshida", 
    route == "須走" ~ "subashiri", 
    route == "御殿場" ~ "Gotenba", 
    # bug: should change "Gotenba" into "gotemba"
    route == "富士宮" ~ "fujinomiya"
  )) %>% 
  select(-route)

## Weather ----
jma <- read_rds("RawData/jma.rds") %>% 
  # for simplification, use one station data for now - should use the dat a of the station near the target trail head
  filter(station == "Yamanaka") %>% 
  select(date, rain_daily_total_mm)

## Holiday ----
# holidays in Japan
holiday <- rbind(
  read.csv("RawData/Japan_holiday_2018.csv") %>% 
    mutate(
      year = 2018, month = substr(.$月日, 1, 2), day  = substr(.$月日, 4, 5)
    ), 
  read.csv("RawData/Japan_holiday_2019.csv") %>% 
    mutate(
      year = 2019, month = substr(.$月日, 1, 2), day  = substr(.$月日, 4, 5)
    )
) %>% 
  rename(holiday_name = 名称) %>% 
  mutate(date = as.Date(paste(year, month, day, sep = "-"))) %>% 
  select(date, holiday_name)

## Model input data ----
course.logit <- 
  course %>% 
  select(dailyid, year, date, datetime, route, 
            trailhead, visitor_grp, home_prefcode, dayofweek) %>% 
  # bug: NAs in dayofweek column of raw data 
  mutate(dayofweek = wday(date, week_start = 1)) %>% 
  mutate(home_prefcode = as.character(home_prefcode)) %>% 
  # home distance 
  left_join(prefcode, by = c("home_prefcode" = "prefcode")) %>% 
  left_join(osrm_res, by = c("prefname" = "src_loc", "trailhead")) %>% 
  # weather 
  left_join(jma, by = "date") %>% 
  # weekend or holiday 
  left_join(holiday, by = "date") %>% 
  mutate(holi_wknd = case_when(
    dayofweek > 5 ~ TRUE, 
    !is.na(holiday_name) ~ TRUE, 
    TRUE ~ FALSE
  ))
# bug: why there are NAs in "year" column of course data.frame

# Analysis ----
# General ----
# number of dailyid of each trailhead by pref and distance
course.logit %>% 
  group_by(trailhead, prefname, distance) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  group_by(prefname) %>% 
  mutate(distance = mean(distance, na.rm = TRUE)) %>% 
  ungroup() %>% 
  ggplot() + 
  geom_point(aes(reorder(prefname, distance), n, col = trailhead)) + 
  coord_flip()
# number of dailyid of each trailhead by pref and duration
course.logit %>% 
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
# relative-diff
course.logit %>% 
  filter(!is.na(distance)) %>% 
  select(trailhead, prefname, distance) %>% 
#  distinct() %>% mutate(dist = min(distance)) %>%  View()
  group_by(prefname) %>% 
  mutate(distance = distance / min(distance)) %>% 
  ungroup() %>% 
  ggplot() + 
  geom_point(aes(prefname, distance, col = trailhead)) + 
  coord_flip()

## Logit of trailhead choice
# multi-nomial logit of data with trailhead 
# further process input data: keep rows with trailhead choices; mutate new trailhead variable by distance to source prefecture
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

# number of dailyid of each trailhead by pref and distance
# scaled by min
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
# scaled by max 
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

# ordinal logit model 
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
# bug: should test the assumption: https://stats.oarc.ucla.edu/r/dae/ordinal-logistic-regression/

# p-value
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

newdat <- data.frame(
  rain_daily_total_mm = 
    rep(mean(course.logit2$rain_daily_total_mm), 400), 
  visitor_grp = rep(c("sure_cancel", "peak"), 200), 
  holi_wknd = rep(c(TRUE, FALSE), each = 200), 
  # distance: to 3rd qua
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
# basic conclusions: 
# 无论哪种情况，随着距离增加，选择近处登山口的概率都下降。因为相比于从家到富士山附近的距离而言，富士山附近到登山口的距离微不足道，换句话说，对于远道而来的人而言，到不同登山口的距离的差异很小
# 爬山的人更有可能选择近点的登山口
# 但是为何节假日选择近处登山口的概率反而更高呢？

