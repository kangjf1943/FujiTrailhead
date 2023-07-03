library(targets)
# This is an example _targets.R file. Every
# {targets} pipeline needs one.
# Use tar_script() to create _targets.R and tar_edit()
# to open it again for editing.
# Then, run tar_make() to run the pipeline
# and tar_read(data_summary) to view the results.

# Define custom functions and other global objects.
# This is where you write source(\"R/functions.R\")
# if you keep your functions in external scripts.
# function: get and process weather raw data
# path: directory to the raw data file 
# year: year of the data
# month: month of the data
GetWeather <- function(path, year, month) {
  # bug: it is better to rename the temp name of result, e.g., change it from "weather" to "res" 
  weather <- 
    read.table(paste0("data_raw/Weather/", path), 
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

# Set target-specific options such as packages: 
tar_option_set(
  packages = c("openxlsx", "stringr", "dplyr", "tidyr", "ggplot2", "geojsonsf", 
  "sf", "tmap", "parallel", "showtext", "patchwork")
)

# End this file with a list of target objects.
list(
  # default CRS for the project: JGD2011
  # note: EPSG for JGD2000 is 4612
  tar_target(
    kCRS, 6668
  ),
  # file directory
  tar_target(
    kDirMobileDt, 
    "data_raw/Agoop富士山/sophia_university_20180701_20190831"
  ), 
  # read all the data
  tar_target(
    raw.mobile.file, 
    list.files(kDirMobileDt) %>% 
      # only keep the files of the mobile raw data
      .[grepl("PDP_sophia_university", .)]
  ), 
  # create a empty list to store the raw data
  tar_target(
    raw.mobile,
    mclapply(
      raw.mobile.file,
      function(x) {
        read.csv(paste0(kDirMobileDt, "/", x)) %>%
          # bug: delete the column "plmn" country code for now, since the column causes trouble to the following bind step because sometimes it is read as character while sometimes as number
          select(-plmn)
      },
      mc.cores = 4
    ) %>%
      # combine the raw data list into a data.frame
      bind_rows()
  ),
  # holidays in Japan
  tar_target(
    holiday, 
    rbind(
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
  ), 
  # read prefcode and city code 
  tar_target(
    pref.city.code, 
    read.csv(paste0(kDirMobileDt, "/prefcode_citycode_master_UTF-8.csv")) %>%
      tibble()
  ), 
  tar_target(
    weather, 
    do.call(
      rbind, 
      list(GetWeather(path = "rn2ola000001lmmy.csv", "2018", "7"), 
           GetWeather(path = "rn2ola000001mlj3.csv", "2018", "8"), 
           GetWeather(path = "rn2ola0000022ib4.csv", "2019", "7"), 
           GetWeather(path = "rn2ola0000023oly.csv", "2019", "8"))
    )
  ), 
  tar_target(
    yamashizu, 
    # data for all prefectures of Japan
    geojson_sf("data_raw/prefectures.geojson") %>% 
      # get prefecture polygons of Yamanashi and Shizuoka
      subset(name %in% c("山梨県", "静岡県")) %>% 
      st_transform(kCRS) %>% 
      # union the two prefectures 
      st_union() %>% 
      st_sf()
  ), 
  # national parks within Yamanashi and Shizuoka
  # bug: need a better defined range of Fujisan range 
  tar_target(
    zoom.bbox, 
    st_crop(yamashizu, xmin = 138.5, ymin = 35.2, xmax = 138.95, ymax = 35.7)
  ), 
  tar_target(
    nps.yamashizu, 
    st_read(dsn = "data_raw/NationalPark/nps", layer = "nps_all") %>% 
      subset(名称 == "富士箱根伊豆") %>% 
      st_transform(kCRS) %>% 
      st_make_valid() %>% 
      st_intersection(y = yamashizu) %>% 
      st_sf() %>% 
      st_union() %>% 
      st_sf() %>% 
      st_crop(st_bbox(zoom.bbox))
  ), 
  tar_target(
    # including some location point and the mesh data 
    # location points
    # bug: 在georeferencing中发现这些点和qgis谷歌地图上的点不能对应？
    mt.point, 
    c(
      # Stations 
      "富士山駅", 35.483468688828026, 138.79559748700663, 
      "富士宮駅", 35.221476150759315, 138.61518825170347, 
      # 吉田ルート
      "富士スバルライン五合目", 35.399470344873706, 138.73292894376618, 
      # 須走ルート
      "須走口五合目", 35.37328465282928, 138.7781878424484, 
      # 御殿場ルート
      "富士山御殿場口五合目第二駐車場", 35.34232696835412, 138.79570741613182, 
      # 富士宮ルート
      "Fujinomiya Trail 5th Station", 35.34093756327385, 138.7343889082398, 
      # 终点
      "淺間大社奧宮久須志神社", 35.36704931228424, 138.73300374779606, 
      "富士山頂上浅間大社奥宮", 35.361003251911036, 138.73130911722052
    ) %>% 
      matrix(byrow = TRUE, ncol = 3) %>% 
      data.frame() %>% 
      rename_with(
        ~ c("location", "lat", "long")
      ) %>% 
      st_as_sf(coords = c("long", "lat")) %>% 
      st_set_crs(kCRS)
  ), 
  # range of the mountain top 
  # bug: the buffer distance can be flexible? 
  tar_target(
    range.top, 
    data.frame(lat = 35.36366366173027, long = 138.728149693833) %>% 
      st_as_sf(coords = c("long", "lat")) %>% 
      st_set_crs(6668) %>%  
      st_buffer(kCRS)
  ), 
  tar_target(
    # read mesh data where Fujisan located
    mesh.5338, 
    st_read(
      dsn = "data_raw/Mesh/G04-a-11_5338-jgd_GML", 
      layer = "G04-a-11_5338-jgd_ElevationAndSlopeAngleTertiaryMesh"
    ) %>% 
      st_set_crs(kCRS)
  ), 
  tar_target(
    # mesh where trail heads located 
    mesh.trail.head, 
    # bug: 须走五合目 is updated - reply to Kubo sensei
    # 富士宮口 五合目（富士宮ルート）:３次メッシュ: 53380508
    # 御殿場口新五合目（御殿場ルート）:３次メッシュ: 53380603
    # 須走口五合目（須走ルート）:３次メッシュ: 53380642
    # 富士スバルライン五合目（吉田ルート）:３次メッシュ: 53380578
    subset(mesh.5338, G04a_001 %in% 
             c("53380508", "53380603", "53380642", "53380578")) %>% 
      mutate(head = case_when(
        G04a_001 == "53380508" ~ "富士宮", 
        G04a_001 == "53380603" ~ "御殿場", 
        G04a_001 == "53380642" ~ "須走", 
        G04a_001 == "53380578" ~ "吉田"
      )) %>% 
      st_transform(kCRS)
  ), 
  tar_target(
    # buffer zone for the trail head mesh
    # considering the uncertainty of the records 
    # bug: the range of the buffer? 
    mesh.trail.head.buff, 
    st_buffer(mesh.trail.head, 300)
  ), 
  tar_target(
    # mesh where top located 
    mesh.top, 
    subset(mesh.5338, G04a_001 == "53380538")
  ), 
  tar_target(
    # regions that prefectures belong to 
    region.pref, 
    read.xlsx("data_raw/Region_pref.xlsx")
  ), 
  tar_target(
    # travel cost from different prefecture
    cost.list, 
    read.csv("data_raw/Cost_evaluation_by_pref.csv")
  ), 
  tar_target(
    # summary the data: group by date, etc. 
    day.pref.mobile, 
    # 筛选出每天独特的dailyid
    raw.mobile %>% 
      mutate(date = as.Date(paste(year, month, day, sep = "-"))) %>% 
      select(year, date, dayofweek, dailyid, home_prefcode) %>% 
      distinct() %>% 
      # 统计每天来自不同地方的dailyid的数量
      group_by(year, date, dayofweek, home_prefcode) %>% 
      summarise(num = n()) %>% 
      ungroup()
  ), 
  # 每天有多少个游客
  tar_target(
    day.mobile, 
    day.pref.mobile %>% 
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
  ), 
  tar_target(
    mob.pref.idnum, 
    day.pref.mobile %>% 
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
  ), 
  tar_target(
    gis.mobile, 
    # turn raw data into simple feature for GIS analysis 
    st_as_sf(raw.mobile, coords = c("longitude", "latitude")) %>% 
      # add projection 
      st_set_crs(kCRS)
  )
)
