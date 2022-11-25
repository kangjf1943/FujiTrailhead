# Statement ----
# The code is used for Fujisan valuation project. 

# Package ----
library(openxlsx)
library(stringr)
library(dplyr)
library(ggplot2)
library(sf)
library(tmap)
library(parallel)
library(showtext)

# Setting ----
showtext_auto()

# Function ----

# Read data ----
## Constant ----
kDirMobileDt <- 
  "RawData/Agoopの富士山のデータ/sophia_university_20180701_20190831"

# read all the data 
raw.mobile.file <- 
  list.files(kDirMobileDt) %>% 
  # only keep the files of the mobile raw data
  .[grepl("PDP_sophia_university", .)]

# create a empty list to store the raw data
raw.mobile <- mclapply(
  raw.mobile.file, 
  function(x) {
    read.csv(paste0(kDirMobileDt, "/", x)) %>% 
      # bug: delete the column "plmn" country code for now, since the column causes trouble to the following bind step because sometimes it is read as character while sometimes as number
      select(-plmn)
  }, 
  mc.cores = 4
) 
# bug: though parellel saves 50% time, read data still take several seconds 
# test: check if the "dailyid" of each *.csv file are unique in the whole data set
# test.raw.mobile <- raw.mobile
# for (i in names(raw.mobile)) {
#   test.raw.mobile[[i]]$file <- i
# }
# # bind to a data.frame
# test.raw.mobile <- bind_rows(
#   test.raw.mobile
# )
# # if each dailyid is unique not just within each day, but within the who data set, then the rows of the two summarized data.frames should be the same
# test.raw.mobile %>% 
#   group_by(dailyid) %>% 
#   summarise(n = n()) %>% 
#   ungroup() %>% 
#   nrow()
# test.raw.mobile %>% 
#   group_by(file, dailyid) %>% 
#   summarise(n = n()) %>% 
#   ungroup() %>% 
#   nrow()

# combine the raw data list into a data.frame
raw.mobile <- bind_rows(rbind, raw.mobile)
# take a look at the data
str(raw.mobile)

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

# read prefcode and city code 
pref.city.code <- 
  read.csv(paste0(kDirMobileDt, "/prefcode_citycode_master_UTF-8.csv")) %>%
  tibble()

## Weather ----
# function: get and process weather raw data
# path: directory to the raw data file 
# year: year of the data
# month: month of the data
GetWeather <- function(path, year, month) {
  # bug: it is better to rename the temp name of result, e.g., change it from "weather" to "res" 
  weather <- 
    read.table(paste0("RawData/Weather/", path), 
               skip = 1, sep = ",", nrows = 33, fileEncoding="cp932") %>% 
    tibble()
  # rename the column names 
  colnames(weather) <- weather[1, ]
  names(weather)[1] <- "day"
  weather <- weather %>%
    subset(day <= 31) %>% 
    select(day, "0〜9時", "9〜15時", "15〜24時") %>% 
    rename(wthr0009 = "0〜9時", wthr0915 = "9〜15時", wthr1524 = "15〜24時") %>% 
    # summary the weather for the whole day
    # bug: I simplify the weather for the whole day by assume that when there is a "rain" in anytime of the day, the weather for the day is "rain", while when there is no rain in the day, the weather should be "norain" 
    mutate(wthr = case_when(
      wthr0009 == "雨" | wthr0915 == "雨" | wthr1524 == "雨" ~ "rain", 
      TRUE ~ "norain"
    ))
  weather$date <- as.Date(paste(year, month, weather$day, sep = "-"))
  return(weather)
}

weather <- do.call(
  rbind, 
  list(GetWeather(path = "rn2ola000001lmmy.csv", "2018", "7"), 
       GetWeather(path = "rn2ola000001mlj3.csv", "2018", "8"), 
       GetWeather(path = "rn2ola0000022ib4.csv", "2019", "7"), 
       GetWeather(path = "rn2ola0000023oly.csv", "2019", "8"))
)

## Fujisan data ----
# including some location point and the mesh data 
# location points
mt.point <- 
  c(
    "top", 35.36366366173027, 138.728149693833, 
    "yoshida_nigoume", 35.41427110100272, 138.75632330450566, 
    "須走口五合目", 35.366334964750806, 138.77849528374938
  ) %>% 
  matrix(byrow = TRUE, ncol = 3) %>% 
  data.frame() %>% 
  rename_with(~ c("location", "lat", "long")) %>% 
  st_as_sf(coords = c("long", "lat")) %>% 
  st_set_crs(4269)

# read Fujisan mountain mesh data
mesh.fujisan <- 
  st_read(dsn = "RawData/G04-c-11_5338-jgd_GML", 
          layer = "G04-c-11_5338-jgd_ElevationAndSlopeAngleFourthMesh")

## Other ----
# regions that prefectures belong to 
region.pref <- read.xlsx("RawData/Region_pref.xlsx")
# travel cost from different prefecture
cost.list <- read.csv("RawData/Cost_evaluation_by_pref.csv")

# summary the data: group by date, etc. 
day.pref.mobile <- 
  # 筛选出每天独特的dailyid
  raw.mobile %>% 
  mutate(date = as.Date(paste(year, month, day, sep = "-"))) %>% 
  select(year, date, dayofweek, dailyid, home_prefcode) %>% 
  distinct() %>% 
  # 统计每天来自不同地方的dailyid的数量
  group_by(year, date, dayofweek, home_prefcode) %>% 
  summarise(num = n()) %>% 
  ungroup()

# 每天有多少个游客
day.mobile <- day.pref.mobile %>% 
  group_by(year, date, dayofweek) %>% 
  summarise(num = sum(num)) %>% 
  ungroup() %>% 
  # add info of weekend or holiday 
  left_join(holiday, by = "date") %>% 
  mutate(wknd_hol = case_when(
    !is.na(holiday_name) ~ "holiday", 
    TRUE ~ "weekday"
  )) %>% 
  mutate(wknd_hol = case_when(
    dayofweek > 5 ~ "weekend",
    TRUE ~ wknd_hol
  )) %>% 
  # add info of daily weather 
  left_join(weather, by = "date")

mob.pref.idnum <- day.pref.mobile %>% 
  group_by(home_prefcode) %>% 
  summarise(num = sum(num)) %>% 
  # add names of the prefectures 
  left_join(pref.city.code %>% select(prefcode, prefname) %>% distinct(), 
            by = c("home_prefcode" = "prefcode")) %>% 
  # add the regions that the prefectures belong to 
  left_join(region.pref, by = c("prefname" = "pref")) %>% 
  # aggregate the prefectures according to cost list 
  mutate(pref_agg = case_when(
    region %in% c("北海道", "東北") ~ "北海道・東北", 
    prefname == "東京都" ~ "東京", 
    prefname == "神奈川県" ~ "神奈川", 
    prefname == "山梨県" ~ "山梨", 
    prefname == "静岡県" ~ "静岡", 
    region == "東海" ~ "中部", 
    region == "近畿" ~ "近畿", 
    region %in% c("中国", "四国") ~ "中国・四国", 
    region == "九州" ~ "九州・沖縄", 
    is.na(prefname) ~ "NA", 
    TRUE ~ "関東"
  )) %>% 
  # add per capita travel cost
  left_join(cost.list, by = c("pref_agg" = "pref")) %>% 
  # 计算总旅费：为人数和单价的乘积
  mutate(tot_cost = cost * num) 
# bug: what to do with home_prefcode = NA? 
dim(mob.pref.idnum)

# Analysis -----
## General description -----
# the distribution of the attributes 
par(mfrow = c(2, 2))
table(raw.mobile$gender) %>% plot(main = "gender")
table(raw.mobile$os) %>% plot(main = "os")
table(raw.mobile$transportation_type) %>% plot(main = "transportation")
hist(raw.mobile$speed, main = "speed")

## Impact factor ----
### Weekend and holiday ----
ggplot(day.mobile) + 
  geom_col(aes(date, num, fill = wknd_hol)) + 
  facet_wrap(.~ year, scales = "free")
ggplot(day.mobile) + 
  geom_boxplot(aes(wknd_hol, num)) + 
  facet_wrap(.~ year, scales = "free")
# let's take a look at the values
day.mobile %>% 
  group_by(wknd_hol) %>% 
  summarise(num_mean = mean(num), num_sd = sd(num)) %>% 
  ungroup()
# bug: though values show the diff, but might need a further statistical analysis for the comparison 

### Weather ----
ggplot(day.mobile) + 
  geom_col(aes(date, num, fill = wthr)) + 
  facet_wrap(.~ year, scales = "free")
# bug: can also analyze the relationship between stay duration and weather, suppose that when weather is good, the duration of stay should be longer
ggplot(day.mobile) + 
  geom_boxplot(aes(wthr, num)) + 
  facet_wrap(.~ year, scales = "free")

### Distance or cost ----
# where do they come from: suppose most people come from surrounding areas
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
# the prefectures near Fujisan mountain rank top
# bug: the number need to be validated with the percentage of phone users of each prefecture 
# bug: what to do with home_prefcode = NA?
# bug: to visualize it with geo-sanky diagram

## Climbing route ----
gis.mobile <- 
  # turn raw data into simple feature for GIS analysis 
  st_as_sf(raw.mobile, coords = c("longitude", "latitude")) %>% 
  # add projection 
  st_set_crs(4269)

# test: what does the data look like, which id has more data records 
gis.mobile %>% 
  st_drop_geometry() %>% 
  group_by(dailyid) %>% 
  summarise(num = n()) %>% 
  ungroup() %>% 
  arrange(-num)
# bug: need to figure out: why some dailyid have so many records? 

# visualization the routes
# it is impossible to get the route of each dailyid since there are so many dailyid, but if we pick some of them, we can understand how they get to the top
# tm_shape(mesh.fujisan) + 
#   tm_fill(col = "G04c_002") + 
#   tm_shape(gis.mobile) + 
#   tm_dots(col = "dailyid") + 
#   tm_shape(mt.point) + 
#   tm_dots(col = "black", size = 0.3)

# If set the route record with color and alpha, maybe we can see which route people use most. But still, there are so many records, it doesn't really work. 
gis.mobile %>% 
  st_crop(xmin = 138.5, ymin = 35.3333, xmax = 138.8, ymax = 35.5) %>% 
  tm_shape() + 
  tm_dots(col = "red", alpha = 0.1) + 
  tm_shape(mt.point) + 
  tm_dots(col = "location", size = 0.3) + 
  tm_compass(position = c("right", "top")) + 
  tm_scale_bar() + 
  tm_layout(legend.outside = TRUE)
# bug: maybe the best way to visualize the data is to calculate before mapping 

## Evaluate the travel cost ----
# dailyid应该是每天会变化的，但是为了方便计算，先假设每天的ID代表了不同的人（这样一定会高估来的人，因为有很多人应该在爬山过程中会过夜，而在目前的数据下，由于他第二天ID会发生变化，就会被算成两个人。后期可以结合缓冲区和路线变化来校正，将相同的人员合并起来。

# per capital cost ~ number of dailyid
# bug: need to calculate the total value
# bug: need validate the total value with percentage of smart phone user etc. - read data explaination again 
mob.pref.idnum %>% 
  group_by(pref_agg, cost) %>% 
  summarise(num = sum(num)) %>% 
  ungroup() %>% 
  ggplot() + 
  geom_point(aes(cost, num)) + 
  geom_text(aes(x = cost, y = num + 200, label = pref_agg))

# total cost of each pref_agg: which prefecture contribute most to Fujisan? 
# bug: just calculate the total cost of the travelers with mobile data here, which should be validated with percentage of mobile phone use etc. 
mob.pref.idnum %>% 
  group_by(prefname) %>% 
  summarise(tot_cost = sum(tot_cost)) %>% 
  arrange(-tot_cost) %>% 
  # get the top 20
  head(20) %>% 
  ggplot() + 
  geom_col(aes(reorder(prefname, tot_cost), tot_cost)) + 
  coord_flip()
