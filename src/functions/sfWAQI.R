# Create geo objects from WAQI data

sfWAQI = function(flist){
  
  aq = lapply(flist, function(f) {
    latlon = getWAQIpos(readLines(f, n=5))
    act = read.table(f, skip = 0, header = TRUE, sep = ",")
    latlon= c(latlon[2],latlon[1])
    act$lat= latlon[2]
    act$lon= latlon[1]
    act$date = as.Date(act$date)
    act$pm25 = as.numeric(as.character(act$pm25))
    act$statname<-substr(basename(f), regexec("\\.", basename(f))[[1]][1]+1, regexec("--", basename(f))[[1]][1]-1)
    act = act[act$date >= as.Date("2020-02-01"), ]
    # it is more consisent to use an sf object and grab the content 
    # act = list(act)
    # names(act) = substr(basename(f), regexec("\\.", basename(f))[[1]][1]+1, regexec("--", basename(f))[[1]][1]-1)
    # act = as.data.frame(act)  
    return(act)
  })
  
}
  
