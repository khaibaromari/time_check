library(dplyr)
library(lubridate)
library(expss)

# 1- time check based on start end time
time_check <- function(df, time_min, time_max){
  df <- df %>% mutate(interview_duration = difftime(as.POSIXct(ymd_hms(end)), as.POSIXct(ymd_hms(start)), units = "mins"),
                      CHECK_interview_duration = case_when(
                        interview_duration < time_min ~ "Too short",
                        interview_duration > time_max ~ "Too long",
                        TRUE ~ "Okay"
                      )
  )
  return(df)
}

# 2- time check based on audit files and start end time
time_check_audit <- function(df_raw, x_uuid="_uuid", time_min, time_max, audit_dir_path = "./audit_files/", today = "today"){
  # Audit Checks
  audit_dir<-audit_dir_path
  uuids<-dir(audit_dir)
  uuid_file<-paste0(audit_dir,"\\","\\audit.csv")
  dfl<-list()
  all_uuids <- length(uuids)

  for(i in 1: length(uuids)){
    df <-read.csv(paste0(audit_dir, uuids[i],"/audit.csv"))
    df <- df %>% filter(node != "")
    duration_ms<- sum(df$end - df$start)
    duration_secs<-duration_ms/1000
    duration_minutes<- round(duration_secs/60,1)
    dfl[[i]]<-data.frame(uuid =uuids[i],duration_ms=duration_ms,durations_secs=duration_secs,duration_minutes= duration_minutes)
    cat("\014","Running audit: ", round((i/all_uuids) * 100,0),"%\n", sep = "")
  }
  duration_df <- do.call("rbind", dfl)
  duration_df <- dplyr::rename(duration_df, `_uuid` = uuid)


  #time check based on start end time
  df_no_audit_files <- df_raw %>% mutate(start = ymd_hms(start),
                                         end = ymd_hms(end),
                                         start_end = round(as.POSIXct(end) - as.POSIXct(start)))

  # Join Audit checks and main data set
  df_str_audit_all <- df_raw %>%
    left_join(select(df_no_audit_files, x_uuid, start_end), by = c("_uuid"="_uuid"))

  # Calculating time using start - end time for missing audit files
  df_str_audit_all <- df_str_audit_all %>%
    left_join(select(duration_df, x_uuid, duration_minutes), by = c("_uuid"="_uuid"))

  # Merging both audit checks
  df_str_audit_all <- df_str_audit_all %>%
    mutate(interview_duration = if_na(duration_minutes, start_end),
           CHECK_interview_duration = case_when(
             interview_duration < time_min ~ "Too short",
             interview_duration > time_max ~ "Too long",
             TRUE ~ "Okay")
    ) %>% select( -c(duration_minutes,start_end))

  return(df_str_audit_all)
}

# 3- elapsed time between interviews based on an area (like; settlement, village, district, province, and ...)
time_btwn_ints <- function(df,device_id,start_col = "start",end_col = "end",village_col, same_village_threshold=3,diff_village_threshold=5){
  checked_df <- df
  
  checked_df <- checked_df[order(checked_df[[start_col]]), ]
  checked_df <- checked_df[order(checked_df[[device_id]]), ]
  
  end_2 <- checked_df[[end_col]][1]
  end_2 <- append(end_2, checked_df[[end_col]])
  checked_df$end_2 <- end_2[-length(end_2)]
  checked_df$gap_between_ints <- difftime(as.POSIXct(ymd_hms(checked_df[[start_col]])), as.POSIXct(ymd_hms(checked_df$end_2)), units = "mins")
  
  village_2 <- checked_df[[village_col]][1]
  village_2 <- append(village_2, checked_df[[village_col]])
  checked_df$village_2 <- village_2[-length(village_2)]
  
  checked_df$CHECK_gap_between_ints <- "Okay"
  checked_df$CHECK_gap_between_ints[checked_df[[village_col]]!=checked_df$village_2 & as.numeric(checked_df$gap_between_ints) < diff_village_threshold] <- paste0("the elapsed time between two interviews in different villages is less than ",diff_village_threshold, " minutes")
  
  checked_df$CHECK_gap_between_ints[checked_df[[village_col]]==checked_df$village_2 & as.numeric(checked_df$gap_between_ints) < same_village_threshold] <- paste0("the elapsed time between two interviews in the same village is less than ",same_village_threshold, " minutes")

  for (i in unique(checked_df[[device_id]])) {
    checked_df$gap_between_ints[checked_df[[device_id]] == i][1] <- NA
    checked_df$CHECK_gap_between_ints[checked_df[[device_id]] == i][1] <- "Okay"
  }
  
  checked_df <- checked_df[,-which(names(checked_df) %in% c("end_2","village_2"))]
  return(checked_df)
}

