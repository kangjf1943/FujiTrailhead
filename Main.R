# Statement ----
# The code is used for Fujisan valuation project. 

# Package ----
# pacman::p_load(
#   openxlsx, stringr, dplyr, tidyr, ggplot2, geojsonsf, 
#   sf, tmap, parallel, showtext, patchwork
# )

# Preparation ----
tar_load(everything())
showtext_auto()

# bug: what to do with home_prefcode = NA? 
dim(mob.pref.idnum)

# Analysis -----
## First round ----
### General description -----
# the distribution of the attributes 
par(mfrow = c(2, 2))
table(raw.mobile$gender) %>% plot(main = "gender")
table(raw.mobile$os) %>% plot(main = "os")
table(raw.mobile$transportation_type) %>% plot(main = "transportation")
hist(raw.mobile$speed, main = "speed")

### Impact factor ----
#### Weekend and holiday ----
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

#### Weather ----
ggplot(day.mobile) + 
  geom_col(aes(date, num, fill = wthr)) + 
  facet_wrap(.~ year, scales = "free")
# bug: can also analyze the relationship between stay duration and weather, suppose that when weather is good, the duration of stay should be longer
ggplot(day.mobile) + 
  geom_boxplot(aes(wthr, num)) + 
  facet_wrap(.~ year, scales = "free")

#### Distance or cost ----
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

### Climbing route ----


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

### Evaluate the travel cost ----
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

# bug: to speed up, I take part of the raw data 
# bug: actually I should transform the CRS at the very begining for gis.mobile rather than this raw data sample 
gis.mobile.smp <- gis.mobile[1:50000, ] %>% 
  st_transform(kCRS)

gis.mobile.smp.group <- 
  gis.mobile.smp %>% 
  # 看各个点分布在哪个区
  mutate(
    top = as.logical(st_intersects(., range.top, sparse = FALSE)), 
    trail = as.logical(st_intersects(., nps.yamashizu, sparse = FALSE)),
    general = 
      as.logical(st_intersects(., yamashizu %>% select(geometry), sparse = FALSE))
  ) %>% 
  # 根据点所落的位置判断用户所属组别
  st_drop_geometry() %>% 
  group_by(dailyid) %>% 
  summarise(across(c(top, trail, general), sum)) %>% 
  ungroup() %>% 
  # bug: there are many dailyid do not intersect with all the range 
  mutate(group = case_when(
    top > 0 ~ "top", 
    trail > 0 ~ "trail", 
    TRUE ~ "general"
  )) %>% 
  select(dailyid, group)
dim(gis.mobile.smp.group)
table(gis.mobile.smp.group$group)

# add group info to raw data 
gis.mobile.smp <- gis.mobile.smp %>% 
  left_join(gis.mobile.smp.group, by = "dailyid")

### Choice of trail head ----
# for visitors climb the mountain: which trail head? 
# get visitor climb the mountain from the sample data 
# bug: that is the second sample data
gis.mobile.smp.climb <- 
  gis.mobile.smp %>% 
  subset(group %in% c("trail", "top"))
# bug: 虽然人大多在这些地方周边聚集，但是由于位置的不确定性，是否扩大mesh范围会更好呢？暂时不考虑串线问题：从一条线串到另一条线
# get trail head choice for each visitor 
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

# visualize the choice of trail head 
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

### Route-related attributes 
# like distance, time, money cost, etc. 
# all related to home city, so we get the home city first 
# variables for potential model
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

# description stat 
# visitor group * home prefecture 
gis.mobile.smp.climb.var %>% 
  group_by(group, prefname) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  ggplot() + 
  geom_col(aes(prefname, n, fill = group), position = "dodge") + 
  theme(axis.text.x = element_text(angle = 90))

# visitor group * route choice
gis.mobile.smp.climb.var %>% 
  group_by(group, route) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  ggplot() + 
  geom_col(aes(route, n, fill = group), position = "dodge") + 
  theme(axis.text.x = element_text(angle = 90))

# route choice * home prefecture 
gis.mobile.smp.climb.var %>% 
  group_by(route, prefname) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  ggplot() + 
  geom_col(aes(prefname, n, fill = route), position = "dodge") + 
  theme(axis.text.x = element_text(angle = 90))
