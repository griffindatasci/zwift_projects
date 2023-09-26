library(googlesheets4)
library(data.table)
library(rvest)
library(ggplot2)

sheet_url <- "https://docs.google.com/spreadsheets/d/1aW8Qrvn02P24MOb4zxOlUNcB_AXjKMyvr8dmIA-sKJo/"

# Read in test log and segment details from database
test_log <- read_sheet(sheet_url, sheet="test_log")
segments <- read_sheet(sheet_url, sheet="segments")

setDT(test_log)
setDT(segments)

# Read in source html files of strava activities to lift segment times
source_files <- list.files("source_files/")

times <- rbindlist(lapply(source_files, function(file){
    activity <- gsub("view-source_https___www.strava.com_activities_|.html", "", file)
    
    html_doc <- read_html(paste0("source_files/", file)) 
    
    seg_snippets <- tstrsplit(tstrsplit(as.character(html_doc), "pageView.segmentEfforts()")[2], "start_index")
    
    focal_snippets <- seg_snippets[grepl("Epic KOM|Radio Tower Climb|Desert Flat Section|Sand and Sequoias|Titans Grove, Northbound|Titans Grove KOM", seg_snippets, ignore.case=FALSE)]
    
    rbindlist(lapply(focal_snippets, function(snippet){
        segment <- tstrsplit(snippet, 'name\":\"|\",\"climb')[[2]]
        time_indexes <- tstrsplit(snippet, "flagged")[[1]]
        start_time <- as.numeric(gsub("[^0-9.-]", "", tstrsplit(time_indexes, "end_index")[[1]]))
        end_time <- as.numeric(gsub("[^0-9.-]", "", tstrsplit(time_indexes, "end_index")[[2]]))
        data.table("strava_id"=as.double(activity), "segment"=segment, "time"=end_time-start_time)
    }))
}))

test_data <- times[test_log, on="strava_id"][!is.na(time)]


# Get average, range (investigate if range>2) for each frame/wheel
averages <- 
    test_data[, 
              .("time"=mean(time), "laps"=.N, "range"=diff(range(time))), 
              by=.(frame, wheel, power, segment)]

if(averages[range>2, .N]>0){
    averages[range>2]
    stop("Check tests - large variation in above cases")
} 

# Baselines
baselines <- 
    averages[frame=="Zwift Carbon" & wheel=="Zwift 32mm Carbon",
             .(segment, power, "time_base"=time)]

# Get effect for each item
averages <- baselines[averages, on=c("segment", "power")]
averages[, time_effect:=time-time_base]

# Create each combination of item, then add frame and wheel effect for bike effect
bikes <- 
    rbindlist(lapply(averages[, unique(power)], function(power){
        rbindlist(lapply(averages[, unique(frame)], function(frame){
            rbindlist(lapply(averages[, unique(wheel)], function(wheel){
                rbindlist(lapply(averages[, unique(segment)], function(segment){
                    data.table(frame, wheel, power, segment)
                }))
            }))
        }))
    }))

bikes <- averages[wheel=="Zwift 32mm Carbon", .(frame, power, segment, "frame_effect"=time_effect)][bikes, on=c("frame", "power", "segment")]
bikes <- averages[frame=="Zwift Carbon", .(wheel, power, segment, "wheel_effect"=time_effect)][bikes, on=c("wheel", "power", "segment")]
bikes <- baselines[bikes, on=c("segment", "power")]
bikes <- bikes[, .(frame, wheel, power, segment, "time"=wheel_effect+frame_effect+time_base)]
bikes <- bikes[!is.na(time)]


bikes[, speed:=segments[bikes, on="segment", km]/time*3600]



bikes[segment=="Titans Grove KOM" & wheel=="Zwift 32mm Carbon", 
      .(frame, wheel, speed=speed-min(speed)), by=power][,
                                                         ggplot(.SD, mapping=aes(x=power, y=speed, color=frame, shape=wheel)) +
                                                             geom_line() +
                                                             geom_point() +
                                                             labs(x="Power (W)", 
                                                                  y="Titans Grove KOM speed advantage (km/h)", 
                                                                  color="Frame",
                                                                  shape="Wheel") +
                                                             theme_classic()]

bikes[segment=="Epic KOM" & wheel=="Zwift 32mm Carbon", 
      .(frame, wheel, speed=speed-min(speed)), by=power][,
                                                         ggplot(.SD, mapping=aes(x=power, y=speed, color=frame, shape=wheel)) +
                                                             geom_line() +
                                                             geom_point() +
                                                             labs(x="Power (W)", 
                                                                  y="Titans Grove KOM speed advantage (km/h)", 
                                                                  color="Frame",
                                                                  shape="Wheel") +
                                                             theme_classic()]

