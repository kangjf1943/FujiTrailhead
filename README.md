## Introduction 

This is a project about national park valuation using big data. This study focus on the trailhead choice. The code has been merged to the "22_AgoopFuji" project. 

## Main.R 

Main analysis code. 

## RawData folder

Since raw data is ignored to GitHub uploading, there is a "raw_data_list.txt" providing the raw data file names. And here are data sources of some raw data. 

**prefectures.geojson** 
The polygons of 47 prefectures in Japan. Downloaded from [地図蔵](https://japonyol.net/editor/article/47-prefectures-geojson.html).

**Japan_holiday_2017.csv**
The data comes from the [NAOJ](https://eco.mtk.nao.ac.jp/koyomi/yoko/2017/rekiyou171.html). 
I added the information of "休日" into the data. 

**Japan_holiday_2018.csv**
The data comes from the [NAOJ](https://eco.mtk.nao.ac.jp/koyomi/yoko/2018/rekiyou181.html). 
I added the information of "休日" into the data.

**Japan_holiday_2019.csv**
The data comes from the [NAOJ](https://eco.mtk.nao.ac.jp/koyomi/yoko/2019/rekiyou191.html). 
I added the information of "休日" into the data.

### Weather folder
The weather data download from [JMA](https://www.data.jma.go.jp/cpdinfo/extreme/extreme_p.html), including: 

**rn2ola000001lmmc**
folder and *rn2ola000001mlit* folder are the hourly weather data of Fujishi of July and August of 2018, download from: 
enter [Fujishi official website](https://www.city.fuji.shizuoka.jp/safety/c0306/rn2ola000001b1i9.html) > click "2018年7月・日報（.zip31.0KB)" and "2018年8月・日報（.zip30.4KB)". 

**rn2ola000001lmmy.csv**
is the daily weather data of Fujishi, download from: 
enter [Fujishi official website](https://www.city.fuji.shizuoka.jp/safety/c0306/rn2ola000001b1i9.html) > click "2018年7月・月報（.csv4.00KB)"

**rn2ola000001mlj3.csv** 
is the daily weather data of Fujishi, download from: 
enter [Fujishi official website](https://www.city.fuji.shizuoka.jp/safety/c0306/rn2ola000001b1i9.html) > click "2018年8月・月報（.csv37.4KB)". 

**rn2ola0000022ib4.csv** is the daily weather data of Fujishi, download from: 
enter [Fujishi official website](https://www.city.fuji.shizuoka.jp/safety/c0306/rn2ola000001qm64.html) > click "2019年7月・月報（csv3.73KB)". 

**rn2ola0000023oly.csv**
is the daily weather data of Fujishi, download from: 
enter [Fujishi official website](https://www.city.fuji.shizuoka.jp/safety/c0306/rn2ola000001qm64.html) > click "2019年8月・日報（zip30.2KB)". 

### G04-c-11_5338-jgd_GML folder
Mesh data of elevation ect. of Fujisan mountain. 
Download from [MLIT](https://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-G04-c.html). 
Click the "G04-c-11_5338-jgd_GML.zip" of this page. 

**Cost_evaluation_by_pref.csv** 
Cost data from table 3-13 of 京都大学, 北海道大学, 甲南大学, 国立環境研究所, n.d. 我が国における自然環境施策への効果的な資源動員に向けた研究研究報告書.
The unit for cost is Japanese yen. 

**Region_pref.xlsx** 
Regions and prefectures, from Wikipedia. 

**05k30-2.xlsx**
Population of each prefecture in 2018. 
Download from 総務省統計局 > ホーム > 統計データ > 人口推計 > 人口推計の概要，推計結果等 > 人口推計の結果の概要人口推計（2018年（平成30年）10月1日現在） (see [link](https://www.stat.go.jp/data/jinsui/2018np/index.html)), choose "第3表　都道府県，年齢(3区分)，男女別人口―総人口(平成30年10月1日現在)（エクセル：43KB）".
The original file, a "\*.xls" file, was changed into this "\*.xlsx" file. 

**05k01-2.xlsx**
Population of each prefecture in 2019. 
Download from 総務省統計局 > ホーム > 統計データ > 人口推計 > 人口推計の概要，推計結果等 > 人口推計の結果の概要人口推計（2019年（令和元年）10月1日現在） (see [link](https://www.stat.go.jp/data/jinsui/2019np/index.html)), choose "第2表　都道府県，男女別人口及び人口性比―総人口，日本人人口(2019年10月1日現在)（エクセル：20KB）".

