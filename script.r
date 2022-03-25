library(jsonlite)
library(data.table)
library(dplyr)
library(DatawRappr)

limit <- 10000
offset <- 0
i <- 1
l <- list()

while(T){
  url <- paste0('https://healthdata.gov/resource/g62h-syeh.json?$limit=',limit,'&$offset=',offset)
  print(url)
  d <- fromJSON(url)
  l[[i]] <- d
  
  if(nrow(d)<limit) break
  
  offset <- offset + limit
  i <- i + 1
} 

rb <- rbindlist(
  l = l,
  fill = T
)

rb$date_formatted <- as.Date(rb$date)
rb$total_hospitalized <- as.numeric(rb$total_adult_patients_hospitalized_confirmed_covid) + as.numeric(rb$total_pediatric_patients_hospitalized_confirmed_covid)
rb$percent_pediatric <- as.numeric(rb$total_pediatric_patients_hospitalized_confirmed_covid) / as.numeric(rb$total_hospitalized) * 100


o <- 'output'
dir.create(o)

write.csv(
  x = rb,
  file = paste0(o,'/hhs-covid-hospitalizations-by-date-states.csv'),
  na = '',
  row.names = F
)

fl <- filter(
  .data = rb,
  state == 'FL'
)

write.csv(
  x = fl,
  file = paste0(o,'/hhs-covid-hospitalizations-by-date-fl.csv'),
  na = '',
  row.names = F
)

keeps <- c(
  'state',
  'date_formatted',
  'total_adult_patients_hospitalized_confirmed_covid',
  'total_pediatric_patients_hospitalized_confirmed_covid',
  'staffed_icu_adult_patients_confirmed_covid',
  'total_hospitalized',
  'percent_pediatric'
)

fldatawrapper <- fl[,keeps, with=F]
fldatawrapper[is.na(fldatawrapper)] <- ''
# fldatawrapper$total_hospitalized <- as.numeric(fldatawrapper$total_adult_patients_hospitalized_confirmed_covid) + as.numeric(fldatawrapper$total_pediatric_patients_hospitalized_confirmed_covid)
# fldatawrapper$percent_pediatric <- as.numeric(fldatawrapper$total_pediatric_patients_hospitalized_confirmed_covid) / as.numeric(fldatawrapper$total_hospitalized) * 100


fldatawrapper <- filter(
  .data = fldatawrapper,
  !is.na(total_hospitalized)
)


write.csv(
  x = fldatawrapper,
  file = paste0(o,'/hhs-covid-hospitalizations-by-date-fl-datawrapper.csv'),
  na = '',
  row.names = F
)


print("Starting chart updater")

updateDateFormat <- gsub(
  pattern = " 0",
  replacement = ' ',
  x = format(
    x = max(fldatawrapper$date_formatted),
    format = "%B %d, %Y"
  )
)

adultchild <- 'e7rPB'
chartIDs <- c(
  '1HRMS', # hospitalizations and adult icu
  'j93Ie', # percent of hospitalizations children
  adultchild # adult and pediatric hospitalizations
)

apikey <- Sys.getenv("DATAWRAPPER_API")

for (id in chartIDs) {
  annotation <- ifelse(
    test = id == adultchild,
    yes = paste0("Updated ",updateDateFormat,'. Logarithmic scale is used to clearly display trends for both adult and pediatric patients.'),
    no = paste0("Updated ",updateDateFormat,'.')
  )
  
  dw_edit_chart(
    chart_id = id,
    api_key = apikey,
    annotate = annotation
  )
  print("Publishing chart")  
  dw_publish_chart(
    chart_id = id,
    api_key = apikey,
    return_urls = TRUE
  )  
}



