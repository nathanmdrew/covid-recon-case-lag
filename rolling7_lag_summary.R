#################################################################
###   Developer: Nathan Drew (vom8)
###   Purpose: Quantify lag between Line Level New Cases and Aggregate New Cases
###            Compute rolling 7 day averages
###            Track the same 7 day average across data snapshots (created)
###            (1) Measure time (lag) until certain percentages are reached
###            (2) At a given lag, summarize percentages
###

library(dplyr)    #data wrangling
library(ggplot2)  #plots
library(zoo)      #rolling averages
#library(modeest)  #compute modes of lag distributions - NO LONGER USED

#clear the environment
rm(list=ls())

##Call local user profile
profile <- Sys.getenv("USERNAME")
#path_share <- paste0("C:/Users/", profile, "/CDC/Data, Analytics, Visualization Task Force - Special Data Requests and Analytics Team/Lag/")
path_share <- paste0("C:/Users/", profile, "/OneDrive - CDC/Deployment/Lag/")



#read Jon's LAG file
#snapshots of line level and aggregate datasets over the past 
# TODO - most recent version = 12/17/2021
# only run when the CSV is updated
#d1 <- read.csv(file=paste0(path_share, "Full Recon Table.csv"), header=T)

#temp <- data.frame(names=names(d1))

#format dates as dates - only run when the CSV is updated
#d1 <- d1 %>%
#  mutate(submission_date = as.Date(submission_date, format="%Y-%m-%d"),
#         created = as.Date(created, format="%Y-%m-%d"))

#save a copy of the R dataset - only run when the CSV is updated
#saveRDS(d1, file=paste0(path_share, "Lag_AGG_LL-2021-12-17.RDS"))

d1 <- readRDS(file=paste0(path_share, "Lag_AGG_LL-2021-11-30.RDS"))

#str(d1$submission_date)
#qc <- distinct(d1, state_abbr) #should be 60
#qc <- d1 %>% group_by(state_abbr) %>% summarize(freq=n()) #should be nrow(d1)/60


min_created <- min(d1$created)
max_created <- max(d1$created)

d2 <- d1 %>% select(created, state_abbr, submission_date, ll_new_cases, agg_new_cases)

lags_rolling <- d2 %>%
  filter(submission_date >= min_created-7) %>% #most recent full 7 day period before first snapshot
  arrange(state_abbr, created, submission_date) %>%
  group_by(state_abbr, created) %>%
  mutate(ind_pct25=0, ind_pct50=0, ind_pct75=0, ind_pct90=0, ind_pct95=0, ind_pct99=0, ind_pct100=0,
         ll_new_07day = zoo::rollmean(ll_new_cases, k=7, fill=NA),
         agg_new_07day = zoo::rollmean(agg_new_cases, k=7, fill=NA),
         pct_of_agg_07day = (ll_new_07day/agg_new_07day)*100)

# qc <- lags_rolling %>% filter(pct_of_agg_07day<0)
# qc <- lags_rolling %>% filter(ll_new_07day==0, agg_new_07day==0)
# qc <- lags_rolling %>% filter(pct_of_agg_07day=="NaN")
# qc$pct_of_agg_07day <- if_else(qc$pct_of_agg_07day=="NaN",
#                                          100,
#                                          qc$pct_of_agg_07day)

# fix ratio for when 7 day average of aggregate is 0 (applies to Territories)
lags_rolling$pct_of_agg_07day <- if_else(lags_rolling$pct_of_agg_07day=="NaN",
                                         100,
                                         lags_rolling$pct_of_agg_07day)


lags_rolling2 <-  lags_rolling %>% 
  filter(!is.na(ll_new_07day)) %>%
  arrange(submission_date, created) %>%
  mutate(lag_days = as.integer(created - (submission_date + 4)),#in other words, time since first CREATED
         `7days_ending_on`=submission_date+3) %>% #to align with Jon's JMP view
  filter(lag_days>0) %>%
  filter(submission_date >= min_created-4) #should keep first 7 day window

#temp <- lags_rolling2 %>% filter(lag_days < 1)
#temp2 <- lags_rolling2 %>% filter(lag_days==1)
#temp3 <- d2 %>% filter(state_abbr=="AL") %>% filter(created=="2021-04-12")
#qc <- lags_rolling2 %>% filter(submission_date <= "2021-04-07")

max_lags <- lags_rolling2 %>% 
            select(submission_date, lag_days) %>% 
            group_by(submission_date) %>% 
            summarize(maxlag=max(lag_days))

# qc2 <- lags_rolling2 %>% filter(state_abbr=="CA") %>% select(state_abbr, created, submission_date, ll_new_07day, agg_new_07day, pct_of_agg_07day, lag_days)
# qc2b <- qc2 %>% filter(submission_date=="2021-02-27")

lags_rolling2$ind_pct25 <- if_else(lags_rolling2$pct_of_agg_07day >= 25, 1, 0)
lags_rolling2$ind_pct50 <- if_else(lags_rolling2$pct_of_agg_07day >= 50, 1, 0)
lags_rolling2$ind_pct75 <- if_else(lags_rolling2$pct_of_agg_07day >= 75, 1, 0)
lags_rolling2$ind_pct90 <- if_else(lags_rolling2$pct_of_agg_07day >= 90, 1, 0)
lags_rolling2$ind_pct95 <- if_else(lags_rolling2$pct_of_agg_07day >= 95, 1, 0)
lags_rolling2$ind_pct99 <- if_else(lags_rolling2$pct_of_agg_07day >= 99, 1, 0)
lags_rolling2$ind_pct100 <- if_else(lags_rolling2$pct_of_agg_07day >= 100, 1, 0)

summ_25 <- lags_rolling2 %>% group_by(state_abbr, submission_date, ind_pct25) %>% summarize(lag_25pct = min(lag_days)) %>% filter(ind_pct25==1) %>% select(state_abbr, submission_date, lag_25pct)
summ_50 <- lags_rolling2 %>% group_by(state_abbr, submission_date, ind_pct50) %>% summarize(lag_50pct = min(lag_days)) %>% filter(ind_pct50==1) %>% select(state_abbr, submission_date, lag_50pct)
summ_75 <- lags_rolling2 %>% group_by(state_abbr, submission_date, ind_pct75) %>% summarize(lag_75pct = min(lag_days)) %>% filter(ind_pct75==1) %>% select(state_abbr, submission_date, lag_75pct)
summ_90 <- lags_rolling2 %>% group_by(state_abbr, submission_date, ind_pct90) %>% summarize(lag_90pct = min(lag_days)) %>% filter(ind_pct90==1) %>% select(state_abbr, submission_date, lag_90pct)
summ_95 <- lags_rolling2 %>% group_by(state_abbr, submission_date, ind_pct95) %>% summarize(lag_95pct = min(lag_days)) %>% filter(ind_pct95==1) %>% select(state_abbr, submission_date, lag_95pct)
summ_99 <- lags_rolling2 %>% group_by(state_abbr, submission_date, ind_pct99) %>% summarize(lag_99pct = min(lag_days)) %>% filter(ind_pct99==1) %>% select(state_abbr, submission_date, lag_99pct)
summ_100 <- lags_rolling2 %>% group_by(state_abbr, submission_date, ind_pct100) %>% summarize(lag_100pct = min(lag_days)) %>% filter(ind_pct100==1) %>% select(state_abbr, submission_date, lag_100pct)

temp <- d2 %>% filter(submission_date >= min_created-7) %>% distinct(state_abbr, submission_date) 
summ_pcts <- left_join(temp, summ_25, by=c("state_abbr", "submission_date"))
summ_pcts <- left_join(summ_pcts, summ_50, by=c("state_abbr", "submission_date"))
summ_pcts <- left_join(summ_pcts, summ_75, by=c("state_abbr", "submission_date"))
summ_pcts <- left_join(summ_pcts, summ_90, by=c("state_abbr", "submission_date"))
summ_pcts <- left_join(summ_pcts, summ_95, by=c("state_abbr", "submission_date"))
summ_pcts <- left_join(summ_pcts, summ_99, by=c("state_abbr", "submission_date"))
summ_pcts <- left_join(summ_pcts, summ_100, by=c("state_abbr", "submission_date"))

summ_pcts <- summ_pcts %>% arrange(state_abbr, submission_date)


# summarize each percentile indicator by jurisdiction
summ_pcts2 <- summ_pcts  

# summarize distribution of lag times to reach each percentage of aggregate - drop NAs
# TODO: handling missings
pct_summary <- summ_pcts2 %>%
               group_by(state_abbr) %>%
               summarize(
                 min_lag25=min(lag_25pct, na.rm=T),
                 min_lag50=min(lag_50pct, na.rm=T),
                 min_lag75=min(lag_75pct, na.rm=T),
                 min_lag90=min(lag_90pct, na.rm=T),
                 min_lag95=min(lag_95pct, na.rm=T),
                 min_lag99=min(lag_99pct, na.rm=T),
                 min_lag100=min(lag_100pct, na.rm=T),
                 
                 q1_lag25=quantile(lag_25pct, .25, na.rm=T),
                 q1_lag50=quantile(lag_50pct, .25, na.rm=T),
                 q1_lag75=quantile(lag_75pct, .25, na.rm=T),
                 q1_lag90=quantile(lag_90pct, .25, na.rm=T),
                 q1_lag95=quantile(lag_95pct, .25, na.rm=T),
                 q1_lag99=quantile(lag_99pct, .25, na.rm=T),
                 q1_lag100=quantile(lag_100pct, .25, na.rm=T),
                 
                 q2_lag25=quantile(lag_25pct, .50, na.rm=T),
                 q2_lag50=quantile(lag_50pct, .50, na.rm=T),
                 q2_lag75=quantile(lag_75pct, .50, na.rm=T),
                 q2_lag90=quantile(lag_90pct, .50, na.rm=T),
                 q2_lag95=quantile(lag_95pct, .50, na.rm=T),
                 q2_lag99=quantile(lag_99pct, .50, na.rm=T),
                 q2_lag100=quantile(lag_100pct, .50, na.rm=T),
                 
                 q3_lag25=quantile(lag_25pct, .75, na.rm=T),
                 q3_lag50=quantile(lag_50pct, .75, na.rm=T),
                 q3_lag75=quantile(lag_75pct, .75, na.rm=T),
                 q3_lag90=quantile(lag_90pct, .75, na.rm=T),
                 q3_lag95=quantile(lag_95pct, .75, na.rm=T),
                 q3_lag99=quantile(lag_99pct, .75, na.rm=T),
                 q3_lag100=quantile(lag_100pct, .75, na.rm=T),
                 
                 max_lag25=max(lag_25pct, na.rm=T),
                 max_lag50=max(lag_50pct, na.rm=T),
                 max_lag75=max(lag_75pct, na.rm=T),
                 max_lag90=max(lag_90pct, na.rm=T),
                 max_lag95=max(lag_95pct, na.rm=T),
                 max_lag99=max(lag_99pct, na.rm=T),
                 max_lag100=max(lag_100pct, na.rm=T),
                 
                 mean_lag25=mean(lag_25pct, na.rm=T),
                 mean_lag50=mean(lag_50pct, na.rm=T),
                 mean_lag75=mean(lag_75pct, na.rm=T),
                 mean_lag90=mean(lag_90pct, na.rm=T),
                 mean_lag95=mean(lag_95pct, na.rm=T),
                 mean_lag99=mean(lag_99pct, na.rm=T),
                 mean_lag100=mean(lag_100pct, na.rm=T),
                 
                 sd_lag25=sqrt(var(lag_25pct, na.rm=T)),
                 sd_lag50=sqrt(var(lag_50pct, na.rm=T)),
                 sd_lag75=sqrt(var(lag_75pct, na.rm=T)),
                 sd_lag90=sqrt(var(lag_90pct, na.rm=T)),
                 sd_lag95=sqrt(var(lag_95pct, na.rm=T)),
                 sd_lag99=sqrt(var(lag_99pct, na.rm=T)),
                 sd_lag100=sqrt(var(lag_100pct, na.rm=T))
               )


#replace missings with maximum time frame+1 (assume it will catch up "tomorrow")
summ_pcts3 <- left_join(summ_pcts2, max_lags, by="submission_date")
max_lag <- max(max_lags$maxlag)

# Previous method - doesn't accurately represent recent timeframes
# summ_pcts3$lag_25pct <- if_else(is.na(summ_pcts3$lag_25pct), summ_pcts3$maxlag+1, as.double(summ_pcts3$lag_25pct))
# summ_pcts3$lag_50pct <- if_else(is.na(summ_pcts3$lag_50pct), summ_pcts3$maxlag+1, as.double(summ_pcts3$lag_50pct))
# summ_pcts3$lag_75pct <- if_else(is.na(summ_pcts3$lag_75pct), summ_pcts3$maxlag+1, as.double(summ_pcts3$lag_75pct))
# summ_pcts3$lag_90pct <- if_else(is.na(summ_pcts3$lag_90pct), summ_pcts3$maxlag+1, as.double(summ_pcts3$lag_90pct))
# summ_pcts3$lag_95pct <- if_else(is.na(summ_pcts3$lag_95pct), summ_pcts3$maxlag+1, as.double(summ_pcts3$lag_95pct))
# summ_pcts3$lag_99pct <- if_else(is.na(summ_pcts3$lag_99pct), summ_pcts3$maxlag+1, as.double(summ_pcts3$lag_99pct))
# summ_pcts3$lag_100pct <- if_else(is.na(summ_pcts3$lag_100pct), summ_pcts3$maxlag+1, as.double(summ_pcts3$lag_100pct))

summ_pcts3$lag_25pct <- if_else(is.na(summ_pcts3$lag_25pct), max_lag+1, as.double(summ_pcts3$lag_25pct))
summ_pcts3$lag_50pct <- if_else(is.na(summ_pcts3$lag_50pct), max_lag+1, as.double(summ_pcts3$lag_50pct))
summ_pcts3$lag_75pct <- if_else(is.na(summ_pcts3$lag_75pct), max_lag+1, as.double(summ_pcts3$lag_75pct))
summ_pcts3$lag_90pct <- if_else(is.na(summ_pcts3$lag_90pct), max_lag+1, as.double(summ_pcts3$lag_90pct))
summ_pcts3$lag_95pct <- if_else(is.na(summ_pcts3$lag_95pct), max_lag+1, as.double(summ_pcts3$lag_95pct))
summ_pcts3$lag_99pct <- if_else(is.na(summ_pcts3$lag_99pct), max_lag+1, as.double(summ_pcts3$lag_99pct))
summ_pcts3$lag_100pct <- if_else(is.na(summ_pcts3$lag_100pct), max_lag+1, as.double(summ_pcts3$lag_100pct))

#use maxlag instead of max_lag (max from submission vs. overall max)
summ_pcts4 <- left_join(summ_pcts2, max_lags, by="submission_date")

summ_pcts4$lag_25pct <- if_else(is.na(summ_pcts4$lag_25pct), summ_pcts4$maxlag+1, as.double(summ_pcts4$lag_25pct))
summ_pcts4$lag_50pct <- if_else(is.na(summ_pcts4$lag_50pct), summ_pcts4$maxlag+1, as.double(summ_pcts4$lag_50pct))
summ_pcts4$lag_75pct <- if_else(is.na(summ_pcts4$lag_75pct), summ_pcts4$maxlag+1, as.double(summ_pcts4$lag_75pct))
summ_pcts4$lag_90pct <- if_else(is.na(summ_pcts4$lag_90pct), summ_pcts4$maxlag+1, as.double(summ_pcts4$lag_90pct))
summ_pcts4$lag_95pct <- if_else(is.na(summ_pcts4$lag_95pct), summ_pcts4$maxlag+1, as.double(summ_pcts4$lag_95pct))
summ_pcts4$lag_99pct <- if_else(is.na(summ_pcts4$lag_99pct), summ_pcts4$maxlag+1, as.double(summ_pcts4$lag_99pct))
summ_pcts4$lag_100pct <- if_else(is.na(summ_pcts4$lag_100pct), summ_pcts4$maxlag+1, as.double(summ_pcts4$lag_100pct))



pct_summary_maxlag <- summ_pcts4 %>%
  group_by(state_abbr) %>%
  filter(!is.na(maxlag)) %>% #when the timeframe isn't long enough, maxlag=NA
  summarize(
    min_lag25=min(lag_25pct),
    min_lag50=min(lag_50pct),
    min_lag75=min(lag_75pct),
    min_lag90=min(lag_90pct),
    min_lag95=min(lag_95pct),
    min_lag99=min(lag_99pct),
    min_lag100=min(lag_100pct),
    
    q1_lag25=quantile(lag_25pct, .25),
    q1_lag50=quantile(lag_50pct, .25),
    q1_lag75=quantile(lag_75pct, .25),
    q1_lag90=quantile(lag_90pct, .25),
    q1_lag95=quantile(lag_95pct, .25),
    q1_lag99=quantile(lag_99pct, .25),
    q1_lag100=quantile(lag_100pct, .25),
    
    q2_lag25=quantile(lag_25pct, .50),
    q2_lag50=quantile(lag_50pct, .50),
    q2_lag75=quantile(lag_75pct, .50),
    q2_lag90=quantile(lag_90pct, .50),
    q2_lag95=quantile(lag_95pct, .50),
    q2_lag99=quantile(lag_99pct, .50),
    q2_lag100=quantile(lag_100pct, .50),
    
    q3_lag25=quantile(lag_25pct, .75),
    q3_lag50=quantile(lag_50pct, .75),
    q3_lag75=quantile(lag_75pct, .75),
    q3_lag90=quantile(lag_90pct, .75),
    q3_lag95=quantile(lag_95pct, .75),
    q3_lag99=quantile(lag_99pct, .75),
    q3_lag100=quantile(lag_100pct, .75),
    
    max_lag25=max(lag_25pct),
    max_lag50=max(lag_50pct),
    max_lag75=max(lag_75pct),
    max_lag90=max(lag_90pct),
    max_lag95=max(lag_95pct),
    max_lag99=max(lag_99pct),
    max_lag100=max(lag_100pct),
    
    mean_lag25=mean(lag_25pct),
    mean_lag50=mean(lag_50pct),
    mean_lag75=mean(lag_75pct),
    mean_lag90=mean(lag_90pct),
    mean_lag95=mean(lag_95pct),
    mean_lag99=mean(lag_99pct),
    mean_lag100=mean(lag_100pct),
    
    sd_lag25=sqrt(var(lag_25pct)),
    sd_lag50=sqrt(var(lag_50pct)),
    sd_lag75=sqrt(var(lag_75pct)),
    sd_lag90=sqrt(var(lag_90pct)),
    sd_lag95=sqrt(var(lag_95pct)),
    sd_lag99=sqrt(var(lag_99pct)),
    sd_lag100=sqrt(var(lag_100pct)),
    
    n=n()
  )

summ_pcts5 <- summ_pcts4 %>% 
  filter(!is.na(maxlag)) %>% 
  mutate(`7days_ending_on`=submission_date+3)

# For a given jurisdiction and lag, summarize percent of agg
# Ignore all missings
#qc <- lags_rolling2 %>% filter(is.na(pct_of_agg_07day))
#temp <- min(d1$created)

summ_by_lag <- lags_rolling2 %>%
               group_by(state_abbr, lag_days) %>%
               summarize(
                 min_pct_of_agg_07day=min(pct_of_agg_07day),
                 q1_pct_of_agg_07day=quantile(pct_of_agg_07day, 0.25),
                 q2_pct_of_agg_07day=quantile(pct_of_agg_07day, 0.50),
                 q3_pct_of_agg_07day=quantile(pct_of_agg_07day, 0.75),
                 max_pct_of_agg_07day=max(pct_of_agg_07day),
                 mean_pct_of_agg_07day=mean(pct_of_agg_07day),
                 sd_pct_of_agg_07day=sqrt(var(pct_of_agg_07day)),
                 n=n()
               ) 

qc <- summ_by_lag %>% group_by(state_abbr) %>% summarize(freq=n())
qc <- summ_pcts5 %>% group_by(state_abbr) %>% summarize(freq=n())


################ SAVED OUTPUTS ##########################

 #write.csv(lags_rolling, file=paste0(path_share,"rolling7_calculations-2021-08-13.csv"))
 #write.csv(summ_pcts, file=paste0(path_share,"rolling7_lag_summary-2021-08-13.csv"))
 #write.csv(pct_summary, file=paste0(path_share,"rolling7_summary_of_percentages-2021-08-13.csv"))
 #write.csv(pct_summary_maxlag, file=paste0(path_share,"REV_rolling7_summary_of_percentages_NA_equal_MaxLagPlus1-2021-08-16.csv"))
 
 #These are the files Jon typically uses
 write.csv(summ_by_lag, file=paste0(path_share,"rolling7_summary_of_ratio_by_jurisdiction_lag-2021-12-17.csv"))
 write.csv(summ_pcts5, file=paste0(path_share, "rolling7_lag_summary_NA_equal_MaxLagPlus1-2021-12-17.csv"))
 
 #write.csv(impute3, file=paste0(path_share,"rolling7_impute_mode_by_date.csv"))
 #write.csv(pct_summary_impute_mode, file=paste0(path_share,"rolling7_lag_summary_impute_mode.csv"))
 
#########################################################
 
 # why are some medians = 116.5?
 # recreate jon's summary
qc <- summ_pcts5 %>% 
   group_by(state_abbr) %>% 
   summarize(
     median25 = median(lag_25pct),
     median50 = median(lag_50pct),
     median75 = median(lag_75pct),
     median90 = median(lag_90pct),
     median95 = median(lag_95pct),
     median99 = median(lag_99pct),
     median100 = median(lag_100pct)
   )
 
 #seems to be jurisdictions that dont report
qc2 <- summ_pcts5 %>%
  filter(state_abbr=="TX") #yep - imputations range from 2-231, median=116.5


ggplot(data=summ_pcts5, aes(x=lag_25pct)) +
   geom_histogram()

summary(summ_pcts5$lag_25pct)
summary(summ_pcts5$lag_50pct)
summary(summ_pcts5$lag_75pct)
summary(summ_pcts5$lag_90pct)
summary(summ_pcts5$lag_95pct)
summary(summ_pcts5$lag_99pct)
summary(summ_pcts5$lag_100pct)
 

ggplot(data=summ_by_lag, aes(x=lag_days, y=mean_pct_of_agg_07day, group=state_abbr)) +
  geom_line()

qc <- summ_by_lag %>% filter(mean_pct_of_agg_07day>500) #CT+RI+territories are weird
qc <- summ_by_lag %>% filter(mean_pct_of_agg_07day<0) #CA weird

qc <- distinct(summ_by_lag, state_abbr)
qc <- distinct(d1, state_name, state_abbr)

# omit territories
temp <- summ_by_lag %>% filter(state_abbr != "GU" & state_abbr != "FSM" & state_abbr != "AS"
                                & state_abbr != "RMI" & state_abbr != "PW" & state_abbr != "MP"
                               & state_abbr != "VI")

ggplot(data=temp, aes(x=lag_days, y=mean_pct_of_agg_07day, group=state_abbr)) +
  geom_line()
