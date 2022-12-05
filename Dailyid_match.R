# Preparation ----
source("Main.R")

# How to link dailyids of a visitor ----
# Let's pick up a visitor as our case first 
gis.mobile <- gis.mobile %>% 
  st_transform(kCRS)
gis.mobile.case.alllog <- gis.mobile %>% 
  subset(dailyid == "974b58960e72f58a1d7a6a6d32b8f3f13c6c443be863a041af402088cb24fed14c549643c36917d3073e3108a5feed91")
# the date is 
paste(unique(gis.mobile.case.alllog$month), 
      unique(gis.mobile.case.alllog$day), 
      sep = "-")
# and track her/his journal 
range(gis.mobile.case.alllog$hour)
unique(gis.mobile.case.alllog$hour)
tm_shape(nps.yamashizu) + 
  tm_polygons(alpha = 0.1) + 
  tm_shape(gis.mobile.case.alllog) + 
  tm_dots(col = "hour", size = 1, alpha = 0.5) + 
  tm_shape(gis.mobile.case.alllog %>% 
             arrange(-hour) %>% 
             group_by(hour) %>% 
             mutate(rowid = row_number()) %>% 
             ungroup() %>% 
             subset(rowid == 1) %>% 
             subset(hour %in% c(0, 15, 21))) + 
  tm_text(text = "hour")

# then we get his latest log
gis.mobile.case.lastlog <- gis.mobile.case.alllog %>% 
  subset(hour == max(hour)) %>% 
  subset(minute == max(minute)) %>% 
  .[nrow(.), ]
tm_shape(nps.yamashizu) + 
  tm_polygons(alpha = 0.1) + 
  tm_shape(gis.mobile.case.lastlog) + 
  tm_dots(col = "red", size = 1)

# then we pick up the earliest movement of within the mountain range the second day
gis.mobile.secondday.earliest <- 
  gis.mobile %>% 
  subset(month == 7 & day == 2) %>% 
  arrange(dailyid, hour, minute) %>% 
  group_by(dailyid) %>% 
  mutate(rowid = row_number()) %>% 
  ungroup() %>% 
  subset(rowid == 1) %>% 
  st_transform(kCRS) %>% 
  st_intersection(nps.yamashizu)
dim(gis.mobile.secondday.earliest)
tm_shape(nps.yamashizu) + 
  tm_polygons(alpha = 0.1) + 
  tm_shape(gis.mobile.secondday.earliest) + 
  tm_dots(col = "blue", size = 0.1) + 
  tm_shape(gis.mobile.case.lastlog) + 
  tm_dots(col = "red", size = 0.3)
# we can see there are some dots around, possibly they belong to the same visitor as the red dot
# let's make a buffer zone to select the blue dots that might be connected with the red dots 
gis.mobile.case.lastlog.buff <- gis.mobile.case.lastlog %>% 
  st_buffer(3000)
tm_shape(nps.yamashizu) + 
  tm_polygons(alpha = 0.1) + 
  tm_shape(gis.mobile.case.lastlog.buff) + 
  tm_polygons(col = "orange") + 
  tm_shape(gis.mobile.secondday.earliest) + 
  tm_dots(col = "blue", size = 0.1) + 
  tm_shape(gis.mobile.case.lastlog) + 
  tm_dots(col = "red", size = 0.3)

# get the blue dots within the buffer zone 
gis.mobile.secondday.earliest.inter <- 
  gis.mobile.secondday.earliest %>% 
  st_intersection(gis.mobile.case.lastlog.buff)
dim(gis.mobile.secondday.earliest.inter)
# so there are three dots
# View(gis.mobile.secondday.earliest.inter)
# there are 3 blue dots within the buffer zone
# then we will get the blue dots that have the same information with the red dots
gis.mobile.secondday.earliest.inter.sameinfo <- 
  gis.mobile.secondday.earliest.inter %>% 
  subset(home_prefcode == gis.mobile.case.lastlog$home_prefcode) %>% 
  subset(home_citycode == gis.mobile.case.lastlog$home_citycode) %>% 
  subset(workplace_prefcode == gis.mobile.case.lastlog$workplace_prefcode) %>% 
  subset(workplace_citycode == gis.mobile.case.lastlog$workplace_citycode) %>% 
  subset(gender == gis.mobile.case.lastlog$gender)

tm_shape(nps.yamashizu) + 
  tm_polygons(alpha = 0.1) +
  tm_shape(gis.mobile.case.lastlog.buff) + 
  tm_polygons(col = "orange") + 
  tm_shape(gis.mobile.case.lastlog) + 
  tm_dots(col = "red", size = 0.05) + 
  tm_shape(gis.mobile.secondday.earliest.inter.sameinfo) + 
  tm_dots(col = "blue", size = 0.05)

rbind(
  gis.mobile.case.lastlog %>% 
    mutate(source = "red dot") %>% 
    select(source, dailyid, home_prefcode, home_citycode, 
           workplace_prefcode, workplace_citycode, gender) %>% 
    st_drop_geometry(), 
  gis.mobile.secondday.earliest.inter.sameinfo %>% 
    mutate(source = "blue dot") %>% 
    select(source, dailyid, home_prefcode, home_citycode, 
           workplace_prefcode, workplace_citycode, gender) %>% 
    st_drop_geometry()
)

# get track of the second day of the matched dailyid
gis.mobile.matchid <- gis.mobile %>% 
  subset(dailyid == gis.mobile.secondday.earliest.inter.sameinfo$dailyid)

range(gis.mobile.matchid$hour)
unique(gis.mobile.matchid$hour)

# combine the two sets
gis.combine <- 
  rbind(
    gis.mobile.matchid %>% 
      mutate(source = "second_day") %>% 
      arrange(-hour) %>% 
      group_by(hour) %>% 
      mutate(rowid = row_number()) %>% 
      ungroup() %>% 
      subset(rowid == 1) %>% 
      select(source, dailyid),
    gis.mobile.case.alllog %>% 
      mutate(source = "first_day") %>% 
      arrange(-hour) %>% 
      group_by(hour) %>% 
      mutate(rowid = row_number()) %>% 
      ungroup() %>% 
      subset(rowid == 1) %>% 
      select(source, dailyid)
  )
gis.combine.text <- 
  rbind(
    gis.mobile.matchid %>% 
      mutate(source = "second_day") %>% 
      arrange(-hour) %>% 
      group_by(hour) %>% 
      mutate(rowid = row_number()) %>% 
      ungroup() %>% 
      subset(rowid == 1) %>% 
      subset(hour %in% range(.$hour)) %>% 
      select(source, dailyid, hour),
    gis.mobile.case.alllog %>% 
      mutate(source = "first_day") %>% 
      arrange(-hour) %>% 
      group_by(hour) %>% 
      mutate(rowid = row_number()) %>% 
      ungroup() %>% 
      subset(rowid == 1) %>% 
      subset(hour %in% range(.$hour)) %>% 
      select(source, dailyid, hour)
  )
tm_shape(gis.combine) + 
  tm_dots(col = "source", alpha = 0.5, 
          palette = c(first_day = "red", second_day = "blue")) + 
  tm_shape(gis.combine.text) + 
  tm_text(text = "hour", col = "source", 
          palette = c(first_day = "red", second_day = "blue")) +
  tm_shape(nps.yamashizu) + 
  tm_polygons(alpha = 0.1)

# zoom in
tm_shape(nps.yamashizu, 
         bbox = st_bbox(nps.yamashizu) * c(1.001, 1.003, 0.999, 0.996)) + 
  tm_polygons(alpha = 0.1) + 
  tm_shape(gis.mobile.matchid) + 
  tm_dots(col = "blue", size = 1, alpha = 0.5) + 
  tm_shape(gis.mobile.case.alllog) + 
  tm_dots(col = "red", size = 1, alpha = 0.5) + 
  tm_shape(gis.mobile.case.alllog %>% 
             arrange(-hour) %>% 
             group_by(hour) %>% 
             mutate(rowid = row_number()) %>% 
             ungroup() %>% 
             subset(rowid == 1)) + 
  tm_text(text = "hour")

tm_shape(nps.yamashizu, 
         bbox = st_bbox(nps.yamashizu) * c(1.001, 1.003, 0.999, 0.996)) + 
  tm_polygons(alpha = 0.1) + 
  tm_shape(gis.mobile.case.alllog) + 
  tm_dots(col = "red", size = 1, alpha = 0.5) + 
  tm_shape(gis.mobile.matchid) + 
  tm_dots(col = "blue", size = 1, alpha = 0.5) + 
  tm_shape(gis.mobile.matchid %>% 
             arrange(-hour) %>% 
             group_by(hour) %>% 
             mutate(rowid = row_number()) %>% 
             ungroup() %>% 
             subset(rowid == 1) %>% 
             subset(hour %in% c(6, 9, 15, 20))) + 
  tm_text(text = "hour")

# More cases ----
GetCase <- function(case.dailyid) {
  gis.mobile.case.alllog <- gis.mobile %>% 
    subset(dailyid == case.dailyid)
  
  # then we get his latest log
  gis.mobile.case.lastlog <- gis.mobile.case.alllog %>% 
    subset(hour == max(hour)) %>% 
    subset(minute == max(minute)) %>% 
    .[nrow(.), ]
  
  # then we pick up the earliest movement of within the mountain range the second day
  gis.mobile.secondday.earliest <- 
    gis.mobile %>% 
    subset(month == 7 & day == 2) %>% 
    arrange(dailyid, hour, minute) %>% 
    group_by(dailyid) %>% 
    mutate(rowid = row_number()) %>% 
    ungroup() %>% 
    subset(rowid == 1) %>% 
    st_transform(kCRS) %>% 
    st_intersection(nps.yamashizu)
  # we can see there are some dots around, possibly they belong to the same visitor as the red dot
  # let's make a buffer zone to select the blue dots that might be connected with the red dots 
  gis.mobile.case.lastlog.buff <- gis.mobile.case.lastlog %>% 
    st_buffer(3000)
  
  # get the blue dots within the buffer zone 
  gis.mobile.secondday.earliest.inter <- 
    gis.mobile.secondday.earliest %>% 
    st_intersection(gis.mobile.case.lastlog.buff)
  dim(gis.mobile.secondday.earliest.inter)
  # so there are three dots
  # View(gis.mobile.secondday.earliest.inter)
  # there are 3 blue dots within the buffer zone
  # then we will get the blue dots that have the same information with the red dots
  gis.mobile.secondday.earliest.inter.sameinfo <- 
    gis.mobile.secondday.earliest.inter %>% 
    subset(home_prefcode == gis.mobile.case.lastlog$home_prefcode) %>% 
    subset(home_citycode == gis.mobile.case.lastlog$home_citycode) %>% 
    subset(workplace_prefcode == gis.mobile.case.lastlog$workplace_prefcode) %>% 
    subset(workplace_citycode == gis.mobile.case.lastlog$workplace_citycode) %>% 
    subset(gender == gis.mobile.case.lastlog$gender)
  
  # get track of the second day of the matched dailyid
  gis.mobile.matchid <- gis.mobile %>% 
    subset(dailyid == gis.mobile.secondday.earliest.inter.sameinfo$dailyid)
  
  # combine the two sets
  gis.combine <- 
    rbind(
      gis.mobile.matchid %>% 
        mutate(source = "second_day") %>% 
        arrange(-hour) %>% 
        group_by(hour) %>% 
        mutate(rowid = row_number()) %>% 
        ungroup() %>% 
        subset(rowid == 1) %>% 
        select(source, dailyid),
      gis.mobile.case.alllog %>% 
        mutate(source = "first_day") %>% 
        arrange(-hour) %>% 
        group_by(hour) %>% 
        mutate(rowid = row_number()) %>% 
        ungroup() %>% 
        subset(rowid == 1) %>% 
        select(source, dailyid)
    )
  gis.combine.text <- 
    rbind(
      gis.mobile.matchid %>% 
        mutate(source = "second_day") %>% 
        arrange(-hour) %>% 
        group_by(hour) %>% 
        mutate(rowid = row_number()) %>% 
        ungroup() %>% 
        subset(rowid == 1) %>% 
        subset(hour %in% range(.$hour)) %>% 
        select(source, dailyid, hour),
      gis.mobile.case.alllog %>% 
        mutate(source = "first_day") %>% 
        arrange(-hour) %>% 
        group_by(hour) %>% 
        mutate(rowid = row_number()) %>% 
        ungroup() %>% 
        subset(rowid == 1) %>% 
        subset(hour %in% range(.$hour)) %>% 
        select(source, dailyid, hour)
    )
  tm_shape(gis.combine) + 
    tm_dots(col = "source", alpha = 0.5, 
            palette = c(first_day = "red", second_day = "blue")) + 
    tm_shape(gis.combine.text) + 
    tm_text(text = "hour", col = "source", 
            palette = c(first_day = "red", second_day = "blue")) +
  tm_shape(nps.yamashizu) + 
    tm_polygons(alpha = 0.1)
}
GetCase("51494eaaf0c2e8f569742478b2eee74a23bb99390e88882ddc38ec47b1af872889777264922927d1f924ca50b64adbb7")
GetCase("0f1b10a36395c36b8c3d4ff11c44a1ceb5780ec91dc468233d530548a1054150842d63e351d8d9c501945948df411b99")

# Pack case code into a function ----
# 目标：生成一个表格，指示每个dailyid对应的后一天的dailyid的数量情况（是否有唯一一个对应的dailyid？）
# 要用循环吗？
# 试着对第一天的每个dailyid循环一下试试？
# dailyid.first: dailyid of a user in the first day
ConnectDailyID <- function(dailyid.first) {
  # pick up all records of the dailyid 
  gis.mobile.tar <- subset(gis.mobile, dailyid == dailyid.first) %>% 
    # further get the latest record of the dailyid (where does he/she stay at the end of the day?)
    subset(hour == max(hour)) %>% 
    subset(minute == max(minute)) %>% 
    # if there are more than one log in the last minute, then get the last log
    .[nrow(.), ]
  
  # make a buffer zone based on the latest record (the range of where the first log might be for the target visitor the next day)
  # bug: the buffer distance is flexible for now, but it should set according to the accuracy of the location data - that make more sense 
 gis.mobile.tar.buff <- st_buffer(gis.mobile.tar, 3000)
 
 # get all the logs for the next day (reference: the date of the target dailyid)
 # bug: ignore change between months (e.g., the first day is July 31st)
 log.nextday <- gis.mobile %>% 
   subset(month == 7 & day == gis.mobile.tar$day + 1) %>% 
   # get the earliest log of the dailyids of the next day 
   # bug: we might expect the visitor of the target dailyid will depart the next day, but it is also possible that some visitor might stay where they are in the first day for more than one day, or, they just do not use smartphone (or the phone is out of power) for the next day so there is not log for the visitor anymore 
   arrange(dailyid, hour, minute) %>% 
   group_by(dailyid) %>% 
   mutate(rowid = row_number(), .after = minute) %>% 
   ungroup() %>% 
   subset(rowid == min(rowid))
 
 # let's see how many logs located within the range of the buffer zone for the dailyid of the first day? 
 log.nextday.inter <- st_intersection(log.nextday, gis.mobile.tar.buff)
 # bug: what if there is no intersection at all? I shall add a if() here
 
 # get the number of the logs within the buffer that have the same information with the target dailyid
 log.nextday.inter.sameinfo <- log.nextday.inter %>% 
   subset(home_prefcode == gis.mobile.tar$home_prefcode) %>% 
   subset(home_citycode == gis.mobile.tar$home_citycode) %>% 
   subset(workplace_prefcode == gis.mobile.tar$workplace_prefcode) %>% 
   subset(workplace_citycode == gis.mobile.tar$workplace_citycode) %>% 
   subset(gender == gis.mobile.tar$gender) %>% 
   st_drop_geometry() %>% 
   select(dailyid, dailyid.1) %>% 
   rename(tar_dailyid = dailyid.1, match_dailyid = dailyid)
 
 return(log.nextday.inter.sameinfo)
}
# test 
ConnectDailyID(gis.mobile$dailyid[1])
# no daily ID of the next day is successfully linked to the target daily ID
ConnectDailyID("0f1b10a36395c36b8c3d4ff11c44a1ceb5780ec91dc468233d530548a1054150842d63e351d8d9c501945948df411b99")


# what if we test the first day and second day? 
# vector to store results
# how many unique dailyid do we have for the first day? 
unique.id <- gis.mobile %>% 
  subset(month == 7 & day == 1) %>% 
  pull(dailyid) %>% 
  unique()
length(unique.id)
# so we have 394 dailyid for the first day
num.connect <- numeric(length = 394)
start.time <- Sys.time()
for (i in 1:length(unique.id)) {
  num.connect[i] <- nrow(ConnectDailyID(unique.id[i]))
}
table(num.connect)
Sys.time() - start.time
# OK, so there is another problem: what if many people are 日帰り? Then their latest log will be located somewhere out of the mountain range, and it means that we don't need to care about their journey of the next day
# let's see the distribution of 

gis.mobile.firstday <- gis.mobile %>% 
  subset(month == 7 & day == 1)
dim(gis.mobile.firstday)
# get the latest log of the dailyids of the first day of July
gis.mobile.firstday.latest <- 
  gis.mobile.firstday %>% 
  # bug: any other way to extract the latest log? 
  arrange(dailyid, -hour, -minute) %>% 
  group_by(dailyid) %>% 
  mutate(rowid = row_number(), .after = minute) %>% 
  subset(rowid == 1) %>% 
  ungroup()
dim(gis.mobile.firstday.latest)
# add number of match into the data.frame
gis.mobile.firstday.latest <- 
  gis.mobile.firstday.latest %>% 
  mutate(num_match = num.connect)
gis.mobile.firstday.latest <- gis.mobile.firstday.latest %>% 
  mutate(num_match = as.factor(num_match))
# map 
tm_shape(nps.yamashizu) + 
  tm_polygons() + 
  tm_shape(gis.mobile.firstday.latest) + 
  tm_dots(col  = "num_match", palette = "Pastel1")
# re-organize the data 
gis.mobile.firstday.latest <- gis.mobile.firstday.latest %>% 
  mutate(num_match_agg = case_when(
    num_match == "0" ~ "no", 
    num_match %in% c("1", "2", "3") ~ "yes"
  ))

tm_shape(gis.mobile.firstday.latest) + 
  tm_dots(col = "num_match_agg", palette = c(yes = "green", no = "orange")) + 
  tm_shape(nps.yamashizu) + 
  tm_polygons(alpha = 0.1)
# so many logs are located far away from the moutatin area 
# let's count the number of logs out of the mountain range and within the mountain range 
# bug: should have correct the CRS of raw data from very begining, gis.mobile
gis.mobile.firstday.latest <- gis.mobile.firstday.latest %>% 
  st_transform(6668)
match.log.nps.firstday <- st_intersection(gis.mobile.firstday.latest, nps.yamashizu)
dim(match.log.nps.firstday)
table(match.log.nps.firstday$num_match_agg)
table(gis.mobile.firstday.latest$num_match_agg)

tm_shape(nps.yamashizu) + 
  tm_polygons() + 
  tm_shape(match.log.nps.firstday) + 
  tm_dots(col = "num_match_agg", size = 0.5, alpha = 0.5, 
          palette = c(yes = "green", no = "orange"))

# how about second day and third day? 
# what if we test the first day and second day? 
# vector to store results
# how many unique dailyid do we have for the first day? 
unique.id <- gis.mobile %>% 
  subset(month == 7 & day == 2) %>% 
  pull(dailyid) %>% 
  unique()
length(unique.id)
# so we have 394 dailyid for the first day
num.connect <- numeric(length = 239)
start.time <- Sys.time()
for (i in 1:length(unique.id)) {
  num.connect[i] <- nrow(ConnectDailyID(unique.id[i]))
}
table(num.connect)
Sys.time() - start.time
# OK, so there is another problem: what if many people are 日帰り? Then their latest log will be located somewhere out of the mountain range, and it means that we don't need to care about their journey of the next day
# let's see the distribution of 

gis.mobile.secondday <- gis.mobile %>% 
  subset(month == 7 & day == 2)
dim(gis.mobile.secondday)
# get the latest log of the dailyids of the first day of July
gis.mobile.secondday.latest <- 
  gis.mobile.secondday %>% 
  # bug: any other way to extract the latest log? 
  arrange(dailyid, -hour, -minute) %>% 
  group_by(dailyid) %>% 
  mutate(rowid = row_number(), .after = minute) %>% 
  subset(rowid == 1) %>% 
  ungroup()
dim(gis.mobile.secondday.latest)
# add number of mathc into the data.frame
gis.mobile.secondday.latest <- 
  gis.mobile.secondday.latest %>% 
  mutate(num_match = num.connect)
gis.mobile.secondday.latest <- gis.mobile.secondday.latest %>% 
  mutate(num_match = as.factor(num_match))
# map 
tm_shape(nps.yamashizu) + 
  tm_polygons() + 
  tm_shape(gis.mobile.secondday.latest) + 
  tm_dots(col  = "num_match", palette = "Pastel1")
# re-organize the data 
gis.mobile.secondday.latest <- gis.mobile.secondday.latest %>% 
  mutate(num_match_agg = case_when(
    num_match == "0" ~ "no", 
    num_match %in% c("1", "2", "3") ~ "yes"
  ))

tm_shape(gis.mobile.secondday.latest) + 
  tm_dots(col = "num_match_agg", palette = "Pastel1") + 
  tm_shape(nps.yamashizu) + 
  tm_polygons(alpha = 0.1)
# so many logs are located far away from the moutatin area 
# let's count the number of logs out of the mountain range and within the mountain range 
# bug: shoule have correct the CRS of raw data from very begining, gis.mobile
gis.mobile.secondday.latest <- gis.mobile.secondday.latest %>% 
  st_transform(6668)
match.log.nps <- st_intersection(gis.mobile.secondday.latest, nps.yamashizu)
dim(match.log.nps)
table(match.log.nps$num_match_agg)
table(gis.mobile.secondday.latest$num_match_agg)

tm_shape(nps.yamashizu) + 
  tm_polygons() + 
  tm_shape(match.log.nps) + 
  tm_dots(col = "num_match_agg", size = 0.5, alpha = 0.5, 
          palette = c(yes = "green", no = "orange"))
