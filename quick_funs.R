get_raw_gps_df <- function(patient_name, plot_raw = F){
  
  ###
  FILE_LOC <- paste0(data_filepath, patient_name, '/')
  list_of_files <- list.files(paste0(FILE_LOC, 'gps/'))
  SAVE_LOC <- paste0(BASE_LOC, patient_name, '/')
  if (!dir.exists(SAVE_LOC)) {
    dir.create(SAVE_LOC)
  }
  if (length(list_of_files) < 1) {
    print("Person has no GPS files?!")
    return(NA)
  }
  ###
  
  raw_list = list()
  i = 1
  for (file in list_of_files) {
    raw_list[[i]] = read_csv(paste0(FILE_LOC, 'gps/',
                                    list_of_files[[i]]),
                             col_types = list(col_double(),
                                              col_datetime(format = ""),
                                              col_double(),
                                              col_double(),
                                              col_double(),
                                              col_double()))
    i = i+1
  }
  raw = bind_rows(raw_list) %>% 
    rename(est_time = `UTC time`) %>% 
    mutate(est_time = est_time - 4*60*60) %>% 
    mutate(day = format(est_time, "%m-%d"),
           time = format(est_time, "%H:%M"))
  
  glimpse(raw)
  
  raw = raw %>% 
    mutate(day_of = weekdays(est_time, abbreviate = T)) %>% 
    unite(date_day, day, day_of, remove = F) %>% 
    mutate(patient_name = patient_name)
  
  g = ggplot(raw, aes(x = est_time)) + geom_histogram(binwidth = 1200) + 
    ggtitle("GPS Measurement Points Over Time (20 min blocks)") + xlab("Eastern Standard Time") +
    facet_wrap(~date_day, scales = 'free_x') + scale_x_datetime(date_labels = '%H:%S')
  ggsave(paste0(SAVE_LOC, 'gps_measured_over_time_raw.pdf'), g, w = 12, h = 9)
  
  per_hour <- raw %>% 
    mutate(hour = format(est_time, "%H")) %>% 
    unite(date_day_hour, date_day, hour, remove = F) %>% 
    group_by(date_day_hour) %>% 
    summarise(count = n(),
              date_day = first(date_day),
              hour = first(hour) %>% as.numeric())
  per_hour_full <- per_hour %>%
    right_join(expand(per_hour, date_day, hour = seq(0, 23))) %>% 
    replace_na(list(count = 0))
  
  g = ggplot(per_hour_full, aes(x = hour, y = count)) + facet_wrap(~date_day) +
    geom_bar(stat = 'identity') + ggtitle("Counts per Hour") + 
    geom_vline(xintercept = 12, linetype = 2)
  ggsave(paste0(SAVE_LOC, 'gps_measured_over_time_raw_perhour.pdf'), g, w = 12, h = 9)
  
  if (plot_raw == T) {
    g = qmplot(data = raw, 
               x = longitude, y = latitude, source = 'google', maptype = 'roadmap',
               mapcolor = 'color',
               extent = 'normal', main = "GPS Map",
               geom = 'blank') + coord_quickmap() +
      # geom_hex(alpha = 0.75) + scale_fill_gradient(low = 'lightgrey', high = 'darkred') +
      geom_point(shape = 1, size = 3) +
      stat_density_2d(geom = 'contour')
    ggsave(paste0(SAVE_LOC, 'gps_map_raw_overall.pdf'), g, w = 10, h = 10)
  }
  
  raw
}

get_raw_app_logs = function(patient_name) {
  
  ###
  FILE_LOC <- paste0(data_filepath, patient_name, '/')
  list_of_files <- list.files(paste0(FILE_LOC, 'ios_log/'))
  if (length(list_of_files) < 1) {
    list_of_files <- list.files(paste0(FILE_LOC, 'app_log/'))
    log_type = 'app_log/'
  } else {
    log_type = 'ios_log/'
  }
  if (length(list_of_files) < 1) {print("Person has no LOG files?!"); return(NA);}
  print(paste("Log type:", log_type))
  
  SAVE_LOC <- paste0(BASE_LOC, patient_name, '/')
  if (!dir.exists(SAVE_LOC)) {
    dir.create(SAVE_LOC)
  }
  ###
  
  raw_list = list()
  i = 1
  for (file in list_of_files) {
    raw_list[[i]] = suppressMessages(
      read_csv(paste0(FILE_LOC, log_type,
                      list_of_files[[i]]))
    )
    i = i+1
  }
  raw = lapply(raw_list, function(x) select(x, timestamp, `UTC time`)) %>% 
    bind_rows() %>% 
    select(timestamp, `UTC time`) %>% 
    rename(est_time = `UTC time`) %>% 
    mutate(est_time = est_time - 4*60*60) %>% 
    mutate(day = format(est_time, "%m-%d"),
           time = format(est_time, "%H:%M"))
  
  glimpse(raw)
  
  raw = raw %>% 
    mutate(day_of = weekdays(est_time, abbreviate = T)) %>% 
    unite(date_day, day, day_of, remove = F) %>% 
    mutate(patient_name = patient_name)
  
  g = ggplot(raw, aes(x = est_time)) + geom_histogram(binwidth = 1200) + 
    ggtitle("App log Measurement Points Over Time (20 min blocks)") + xlab("Eastern Standard Time") +
    facet_wrap(~date_day, scales = 'free_x') + scale_x_datetime(date_labels = '%H:%S')
  ggsave(paste0(SAVE_LOC, 'applog_measured_over_time_raw.pdf'), g, w = 12, h = 9)
  
  per_hour <- raw %>% 
    mutate(hour = format(est_time, "%H")) %>% 
    unite(date_day_hour, date_day, hour, remove = F) %>% 
    group_by(date_day_hour) %>% 
    summarise(count = n(),
              date_day = first(date_day),
              hour = first(hour) %>% as.numeric())
  per_hour_full <- per_hour %>%
    right_join(expand(per_hour, date_day, hour = seq(0, 23))) %>% 
    replace_na(list(count = 0))
  
  g = ggplot(per_hour_full, aes(x = hour, y = count)) + facet_wrap(~date_day) +
    geom_bar(stat = 'identity') + ggtitle("Counts per Hour") + 
    geom_vline(xintercept = 12, linetype = 2)
  ggsave(paste0(SAVE_LOC, 'applog_measured_over_time_raw_perhour.pdf'), g, w = 12, h = 9)
  
  raw
}


initialize_beiwe_pipeline <- function() {
  source('~/Dropbox/Research Projects/labor_mismatch/pilot_analysis/Beiwe-Analysis-master/Utility/initialize_output_directory.R')
  #
  source('~/Dropbox/Research Projects/labor_mismatch/pilot_analysis/Beiwe-Analysis-master/Preprocessing/GPS_preprocessing.R')
  #
  nonfolders=grep("\\.",list.files(data_filepath))
  if(length(nonfolders)>0){  ### if you don't do this check, in if length(nonfolders)==0 will make patient_names empty
    patient_names = list.files(data_filepath)[-nonfolders]
  }else{
    patient_names = list.files(data_filepath)
  }
  patient_names = patient_names[!patient_names == "output"]
  #
  initialize_output_directory(data_filepath, output_filepath)
}

plot_cleaned_data_maps_times <- function(patient_name,
                                         plot_indiv = T,
                                         specific_zoom_overall = 13,
                                         specific_zoom_indiv = 13) {
  ###
  SAVE_LOC <- paste0(BASE_LOC, patient_name, '/')
  if (!dir.exists(SAVE_LOC)) {
    dir.create(SAVE_LOC)
  }
  ###
  
  # pre_flights <- readRDS(paste0(output_filepath, 'Preprocessed_Data/Individual/',
  #                               patient_name, '/gps_preprocessed.rds'))
  avg_data <- readRDS(paste0(output_filepath, 'Preprocessed_Data/Individual/',
                             patient_name, '/gps_preprocessed_avgmat.rds'))
  #
  cleaned <- avg_data %>% as_data_frame() %>% 
    set_names(c("code", "timestamp", "latitude", "longitude", "xv", "yv")) %>% 
    filter(code != 4)
  cleaned <- cleaned %>% 
    mutate(est_time = as.POSIXct(timestamp, origin = '1970-01-01', tz = "America/New_York")) %>%
    # mutate(est_time = ymd_hms(timestamp, tz = 'America/New_York')) %>% 
    mutate(day = format(est_time, "%m-%d"),
           time = format(est_time, "%H:%M")) %>% 
    mutate(day_of = weekdays(est_time, abbreviate = T)) %>% 
    unite(date_day, day, day_of, remove = F) %>% 
    mutate(patient_name = patient_name)
  #
  # plot clean over time
  g = ggplot(cleaned, aes(x = est_time)) + geom_histogram(binwidth = 1200) + 
    ggtitle("GPS Measurement Points Over Time (20 min blocks)") + xlab("Eastern Standard Time") +
    facet_wrap(~date_day, scales = 'free_x') + scale_x_datetime(date_labels = '%H:%S')
  ggsave(paste0(SAVE_LOC, 'gps_measured_over_time_cleaned.pdf'), g, w = 12, h = 9)
  
  # per hour plot
  per_hour <- cleaned %>% 
    mutate(hour = format(est_time, "%H")) %>% 
    unite(date_day_hour, date_day, hour, remove = F) %>% 
    group_by(date_day_hour) %>% 
    summarise(count = n(),
              date_day = first(date_day),
              hour = first(hour) %>% as.numeric())
  per_hour_full <- per_hour %>%
    right_join(expand(per_hour, date_day, hour = seq(0, 23))) %>% 
    replace_na(list(count = 0))
  
  g = ggplot(per_hour_full, aes(x = hour, y = count)) + facet_wrap(~date_day) +
    geom_bar(stat = 'identity') + ggtitle("Counts per Hour") + 
    geom_vline(xintercept = 12, linetype = 2)
  ggsave(paste0(SAVE_LOC, 'gps_measured_over_time_cleaned_perhour.pdf'), g, w = 12, h = 9)
  
  # overall
  g = qmplot(data = cleaned, 
             x = longitude, y = latitude, source = 'google', maptype = 'roadmap',
             mapcolor = 'color',
             zoom = specific_zoom_overall,
             extent = 'normal', main = "GPS Map",
             geom = 'blank') + coord_quickmap() +
    # geom_hex(alpha = 0.75) + scale_fill_gradient(low = 'lightgrey', high = 'darkred') +
    geom_point(shape = 1, size = 3) +
    stat_density_2d(geom = 'contour')
  ggsave(paste0(SAVE_LOC, 'gps_map_overall.pdf'), g, w = 10, h = 10)
  
  if (plot_indiv) {
    # indiv days
    for (this_day in unique(cleaned$day)) {
      if (!dir.exists(paste0(SAVE_LOC, 'indiv_days/'))) {
        dir.create(paste0(SAVE_LOC, 'indiv_days/'))
      }
      print(paste("Day:", this_day))
      g = qmplot(data = cleaned %>%  
                   filter(day == this_day), 
                 x = longitude, y = latitude, source = 'google', maptype = 'roadmap',
                 mapcolor = 'color',
                 zoom = specific_zoom_indiv,
                 extent = 'normal', main = "GPS Map",
                 geom = 'blank') + coord_quickmap() +
        # geom_hex(alpha = 0.75) + scale_fill_gradient(low = 'lightgrey', high = 'darkred') +
        geom_point(shape = 10, size = 3.5) + facet_wrap(~date_day) +
        stat_density_2d(geom = 'contour')
      ggsave(paste0(SAVE_LOC, 'indiv_days/', str_replace(this_day, '-', '_'), '.pdf'), 
             g, w = 12, h = 10)
    }
  }
  
  glimpse(cleaned)
  # return
  cleaned
}

plot_cleaned_gaps <- function(patient_name, cleaned, large_gap_days = F){
  ###
  SAVE_LOC <- paste0(BASE_LOC, patient_name, '/')
  if (!dir.exists(SAVE_LOC)) {
    dir.create(SAVE_LOC)
  }
  ###
  
  gaps <- cleaned %>% select(-timestamp) %>% 
    arrange(est_time) %>% 
    mutate(min_gap = difftime(est_time, lag(est_time), units = 'mins'),
           dist_gap = sqrt( (xv - lag(xv))^2 + (yv - lag(yv))^2))
  
  g = ggplot(gaps %>% filter(min_gap > patient_sample_freq) %>% 
               mutate(min_gap = as.numeric(min_gap)), 
             aes(x = min_gap)) + geom_histogram() + 
    ggtitle("Hist of Large Time Gaps (in minutes)") + xlab("Minutes")
  ggsave(paste0(SAVE_LOC, 'gps_cleaned_hist_minute_large_gaps.pdf'), g, w = 8, h = 5.5)
  
  g = ggplot(gaps %>% filter(min_gap <= patient_sample_freq,
                             min_gap > 1/5) %>% 
               mutate(min_gap = as.numeric(min_gap)), 
             aes(x = min_gap)) + geom_histogram() + 
    ggtitle("Hist of Regular Time Gaps (in minutes)") + xlab("Minutes")
  ggsave(paste0(SAVE_LOC, 'gps_cleaned_hist_minute_regular_gaps.pdf'), g, w = 8, h = 5.5)
  
  g = ggplot(gaps %>% filter(min_gap >= patient_sample_freq) %>% 
               mutate(min_gap = as.numeric(min_gap)), 
             aes(x = min_gap, y = dist_gap)) + geom_point() + 
    ggtitle("Compare Min Gaps to Distance Gaps") + xlab("Log-Minutes")
  ggsave(paste0(SAVE_LOC, 'gps_cleaned_scatter_gap_dist.pdf'), g, w = 8, h = 5.5)
  
  if (large_gap_days) {
    large_gaps = gaps %>% 
      mutate(gap_after = ifelse(lead(dist_gap > 1000), T, F)) %>% 
      filter(gap_after == T | dist_gap >= 1000)
    
    for (this_day in unique(large_gaps$day)) {
      if (!dir.exists(paste0(SAVE_LOC, 'indiv_days_gaps/'))) {
        dir.create(paste0(SAVE_LOC, 'indiv_days_gaps/'))
      }
      g = qmplot(data = large_gaps %>%  
                   filter(day == this_day), 
                 x = longitude, y = latitude, source = 'google', maptype = 'roadmap',
                 mapcolor = 'color',
                 zoom = 14,
                 extent = 'normal', main = "GPS Map",
                 geom = 'blank') + coord_quickmap() +
        # geom_hex(alpha = 0.75) + scale_fill_gradient(low = 'lightgrey', high = 'darkred') +
        geom_label(aes(label = time), alpha = 0.25) +
        geom_point(size = 4, aes(color = gap_after)) + facet_wrap(~date_day)
      ggsave(paste0(SAVE_LOC, 'indiv_days_gaps/', str_replace(this_day, '-', '_'), '.pdf'), 
             g, w = 12, h = 10)
    }
    
    large_time_gaps = gaps %>% 
      mutate(gap_after = ifelse(lead(min_gap > 15), T, F)) %>% 
      filter(gap_after == T | min_gap > 15)
    
    for (this_day in unique(large_time_gaps$day)) {
      if (!dir.exists(paste0(SAVE_LOC, 'indiv_days_timegaps/'))) {
        dir.create(paste0(SAVE_LOC, 'indiv_days_timegaps/'))
      }
      g = qmplot(data = large_time_gaps %>%  
                   filter(day == this_day), 
                 x = longitude, y = latitude, source = 'google', maptype = 'roadmap',
                 mapcolor = 'color',
                 zoom = 14,
                 extent = 'normal', main = "GPS Map",
                 geom = 'blank') + coord_quickmap() +
        # geom_hex(alpha = 0.75) + scale_fill_gradient(low = 'lightgrey', high = 'darkred') +
        geom_label(aes(label = time), alpha = 0.25) +
        geom_point(size = 4, aes(color = gap_after)) + facet_wrap(~date_day)
      ggsave(paste0(SAVE_LOC, 'indiv_days_timegaps/', str_replace(this_day, '-', '_'), '.pdf'), 
             g, w = 12, h = 10)
    }
  }
}