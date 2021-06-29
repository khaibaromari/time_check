library(readxl)

source("functions/time_check.R")

# initializing the minimum and maximum interview duration (in minutes) to be flagged
time_min <- 15
time_max <- 60

# reading the raw data set
df <- read_xlsx("input/data_frame.xlsx")
# time check based on start and end time
time_checked_df <- time_check(df, time_min, time_max)

# time check based on both audit files and start and end time
audit_time_checked_df <- time_check_audit(df, x_uuid = "_uuid", time_min, time_max,audit_dir_path = "audit_files/", today = "date")

# check the elapsed time between each interview
elapsed_time_between_ints <- time_btwn_ints(df ,device_id = "deviceid", start_col = "start", end_col = "end", village_col = "village", same_village_threshold = 3, diff_village_threshold = 10)

# exporting the result
write.xlsx(time_checked_df, "output/time_checked_df.xlsx")
write.xlsx(audit_time_checked_df, "output/audit_time_checked_df.xlsx")
write.xlsx(elapsed_time_between_ints, "output/elapsed_time_between_ints_checked_df.xlsx")
