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
    
    focal_snippets <- seg_snippets[grepl("desert flat section|sand and sequoias|titans grove, northbound|titans grove kom", seg_snippets, ignore.case=TRUE)]
    
    rbindlist(lapply(focal_snippets, function(snippet){
        segment <- tstrsplit(snippet, 'name\":\"|\",\"climb')[[2]]
        time_indexes <- tstrsplit(snippet, "flagged")[[1]]
        start_time <- as.numeric(gsub("[^0-9.-]", "", tstrsplit(time_indexes, "end_index")[[1]]))
        end_time <- as.numeric(gsub("[^0-9.-]", "", tstrsplit(time_indexes, "end_index")[[2]]))
        data.table("strava_id"=as.double(activity), "segment"=segment, "time"=end_time-start_time)
    }))
}))

test_data <- times[test_log, on="strava_id"][!is.na(time)]



averages <- test_data[, .("time"=mean(time), "laps"=.N, "range"=diff(range(time))), by=.(test_id, frame, wheel, power, segment)]
averages[, speed:=segments[averages, on="segment", km]/time*3600]

averages[segment=="Sand and Sequoias", 
         ggplot(.SD, mapping=aes(x=power, y=time, color=frame, shape=wheel)) + 
             geom_line()]


averages[segment=="Sand and Sequoias", 
         ggplot(.SD, mapping=aes(x=power, y=speed, color=frame, shape=wheel)) + 
             geom_line()]


averages[segment=="Titans Grove KOM", 
         ggplot(.SD, mapping=aes(x=power, y=speed, color=frame, shape=wheel)) + 
             geom_line()]

averages[segment=="Titans Grove KOM"][order(speed)]
