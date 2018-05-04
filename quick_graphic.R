# test_beiwe analysis

library(tidyverse)
# library(maps); library(mapdata)
library(ggmap); library(stringr)

##########
##########
data_filepath      = # folder name where the raw GPS downloaded data is
output_filepath    = # folder where the output folders (one for each user) should be output to

source('./quick_funs.R')
source('./initialize_output_directory.R')
source('./GPS_preprocessing.R')

theme_set(theme_bw())
Sys.setenv(TZ="America/New_York")

non_patients = grep("\\.",list.files(data_filepath))
if(length(non_patients) > 0){
  patient_names = list.files(data_filepath)[-non_patients]
}else{
  patient_names = list.files(data_filepath)
}

# you may want to ignore some users and not plot them
patient_names = patient_names[!patient_names %in% 
                                c("fdo41p3a", #name 
                                  "mlj2b38t", #name
                                  "t18pkpj9"  #name
                                  )]

# deleting previous version of the folder to create a fresh folder
#   this is unnecessary, so consider removing
unlink(output_filepath, recursive = T)
dir.create(output_filepath)

# important to initialize folders for GPS_preprocessing()
#   the `Beiwe-Analysis` repo has a more extensive initialization and 
#     pre-processsing procedure
#   this is just a little piece of it that I've taken
initialize_output_directory()

# this may be buggy and throw an error when making many graphs
#   replace with `curl` if so, which doesn't always work,
#   but fails silently
options(download.file.method = "libcurl")

for (this_patient_name in patient_names) {
  patient_name = this_patient_name
  patient_sample_freq = 15
  print(patient_name)
  #
  BASE_LOC <- output_filepath
  
  # plot RAW data #
  raw = get_raw_gps_df(patient_name, plot_raw = T)

  # histogram of # of app log lines per hour #
  app_logs = get_raw_app_logs(patient_name)
  
  if(!is.data.frame(raw)) next # in case there is no raw GPS data, skip

  # get cleaned, processed GPS data (code by: Onnela Lab) #
  # this creates cleaned up GPS data in a folder pre-made by initialize_beiwe_pipeline(), 
  #   averaging all GPS lat/lon within a 10 second bin
  #   discarding high inaccuracy lat/lon pings
  #   and identifying flights (GPS pings that are far apart, suggesting a movement)
  # (In their original code, there is a small typo: https://github.com/onnela-lab/Beiwe-Analysis/pull/2)
  GPS_preprocessing(patient_name)
  
  # takes the cleaned data and plots histograms
  cleaned = plot_cleaned_data_maps_times(patient_name, plot_indiv = F)
  
  # looks at gaps in the data and plots the histogram of large time gaps
  #   it is also possible to plot the GPS points of large_distance_gaps or large_time_gaps
  #   to see if the person was driving, or if the phone didn't move (respectively)
  large_gaps = plot_cleaned_gaps(patient_name, cleaned)
}
