# library
library(dplyr)

# read 
q3= read.csv("q3.csv", stringsAsFactors=FALSE)
head(q3)
str(q3)

q3$ts2 = as.POSIXct(q3$ts,format="%Y-%m-%d %H:%M:%S")
head(q3)

#Q1
q3_bdv = q3 %>% filter(country_id=="BDV")
# distinct user_count (bdv)

site_duser = distinct(q3_bdv %>% select(site_id, user_id))
site_duser %>% group_by(site_id) %>% summarise(n=n())

#Q2
st = as.POSIXct("2019-02-03 00:00:00", format="%Y-%m-%d %H:%M:%S")
et = as.POSIXct("2019-02-05 00:00:00", format="%Y-%m-%d %H:%M:%S")
q2d = q3 %>% filter(ts2>=st & ts2 <et)

q2d_2 = q2d %>% group_by(user_id, site_id) %>% summarise(vn_site = n()) %>% arrange(desc(vn_site))
q2d_2 %>% filter(vn_site>=10)


#Q3

q3d= q3  %>%group_by(user_id) %>% arrange(desc(ts2)) %>% mutate(counter = row_number())  %>%
  arrange(user_id, desc(ts2)) %>% filter(counter == 1)
q3d %>% filter(user_id == "LC3561")
q3d %>% group_by(site_id) %>% summarise(cnt = n()) %>% arrange(desc(cnt))


#Q4

q4d_lsite= q3  %>%group_by(user_id) %>% arrange(desc(ts2)) %>% mutate(counter = row_number())  %>%
  arrange(user_id, desc(ts2)) %>% filter(counter == 1) %>% select(user_id, site_id) %>% 
  rename(user_id=user_id, last_site_id=site_id)


q4d_fsite= q3  %>%group_by(user_id) %>% arrange(ts2) %>% mutate(counter = row_number())  %>%
  arrange(user_id, ts2) %>% filter(counter == 1) %>% select(user_id, site_id) %>% 
  rename(user_id=user_id, first_site_id=site_id)

q4d = merge(q4d_lsite, q4d_fsite, by="user_id")
q4d2 = q4d %>% mutate(flag = ifelse(last_site_id==first_site_id, 1, 0 ))
q4d2 %>% group_by(flag) %>% summarise(n=n())



#Q5
u_vc_cnt = q3 %>% group_by(user_id, country_id) %>% summarise(vc=n())
u_vuc_cnt = u_vc_cnt %>% group_by(user_id) %>% summarise(vuc=n())
head(u_vc_cnt)
head(u_vuc_cnt)

site_uv_cnt = q3 %>% group_by(site_id, user_id) %>% summarise(uvc=n())
dim(site_uv_cnt)

q5d = left_join(site_uv_cnt, u_vuc_cnt,by="user_id")
head(q5d)
q5d %>% filter(user_id == "LC3450")

q5d$vuc_over2_yn = if_else(q5d$vuc>1 , 1, 0)
head(q5d)

q5d %>% group_by(site_id) %>% summarise(tn=n(),vuc_over2=sum(vuc_over2_yn), ratio =n()/sum(vuc_over2_yn)) %>%
  arrange(ratio)



