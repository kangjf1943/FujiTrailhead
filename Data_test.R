# dailyid ~ home_prefcode: one to one? ----
# 根据以下测试，可知每个dailyid只对应一个home_prefcode
raw.mobile %>% 
  select(dailyid, home_prefcode) %>% 
  distinct() %>% 
  group_by(dailyid) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  # 判断每个dailyid对应的home_prefcode是否超过1个，一级如果有的话，这样的记录有多少个
  mutate(test = n > 1) %>% 
  select(test) %>% 
  sum()
# 结论是没有任何一个dailyid对应的home_prefcode超过1个

# Completeness of each column ----
# indicated by the percentage of NA or null values in each column 
for (i in names(raw.mobile)) {
  cat(i, 
      sum(raw.mobile[[i]] == "" | is.na(raw.mobile[[i]])) / nrow(raw.mobile), 
      "\n")
}