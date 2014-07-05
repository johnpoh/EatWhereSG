## @knitr loadLibraries
require(RJSONIO); require(rCharts); require(RColorBrewer); require(httr); require(jsonlite);require(googleVis)
options(stringsAsFactors = F)

#source(file="server.r")

###Data Extraction - getData()
restaurantslist = 'TorteSg,chisozanmai.sg,Leestaiwanese,Katanashi.Singapore,sarnies.singapore,garibaldirestaurant,donquijotespanishrestaurant,Serenityspanishbarandrestaurant,Keyaki.Singapore,dallas.sg,hifumi.sg,lesamisrestaurant,BlissHouse.sg,CatalunyaSingapore,OriginalSinRestaurant,Grandmas.Singapore,theclanrestaurant,saveursingapore,BrunettiSingapore,bibigo.singapore,kisekirestaurant,restaurantcocotte,BacchanaliaSG,mamaisonsg,dbBistroModerneSingapore'

getData <- function(rlist = restaurantslist){
  require(httr)
  url = sprintf('https://graph.facebook.com/?ids=%s', rlist)
  restaurants = fromJSON(url)
  lapply(restaurants, function(rest){within(rest, { 
    latitude = as.numeric(location$latitude)
    longitude = as.numeric(location$longitude)
    #location$latitude <- location$longitude <- NULL
    })
  })
  
}

###Data Presentation/Visualization - plotMap() and plotMapList()

#plotMap <- function(dataset = restaurantslist, mapcenter = c(1.373607, 103.804476), mapzoom = 11, width = 835, height = 550){
  #data_ <- getData(dataset); 
  #L1 <- Leaflet$new()
  ##L1$tileLayer(provider = 'Stamen.TonerLite')
  #L1$set(width = width, height = height)
  #L1$setView(mapcenter, mapzoom)
  #L1$geoJson(toGeoJSON(data_), 
             #onEachFeature = '#! function(feature, layer){
      #layer.bindPopup(feature.properties.popup)
    #} !#',
             #pointToLayer =  "#! function(feature, latlng){
      #return L.circleMarker(latlng, {
        #radius: 4,
        #fillColor: feature.properties.fillColor || 'red',    
        #color: '#000',
        #weight: 1,
        #fillOpacity: 0.8
      #})
    #} !#")
  #L1$enablePopover(TRUE)
  #L1$fullScreen(TRUE)
  #return(L1)
#}

###Map Visualization II directly from Data Frame

plotMap <- function(dataset = restaurantdf(), mapcenter = c(1.373607, 103.804476), mapzoom = 11, width = 835, height = 550){
  mapdf = dataset
  map = Leaflet$new()
  map$set(width = width, height = height)
  map$setView(mapcenter, mapzoom)
  
  
  for(i in 1:nrow(mapdf)){
    map$marker(c(mapdf$lat[i],mapdf$lng[i]), 
               bindPopup = paste0("Name: ",mapdf$name[i], "<br>",
                                 "Status: ",mapdf$status[i], "<br>",
                                 "Likes: ", mapdf$likes[i], "<br>",
                                 "Talking About: ", mapdf$talking[i], "<br>", 
                                 "Were Here: ", mapdf$here[i], "<br>",
                                sprintf("<img src=%s height='100' width='200'>",mapdf$picture[i]))) }
  
  map$enablePopover(TRUE)
  map$fullScreen(TRUE)
  return(map)
}


###Map Visualization III indirectly from Data Frame

plotMapList <- function(dataset = restaurantdf(), mapcenter = c(1.373607, 103.804476), mapzoom = 11, width = 835, height = 550){
  mapdf = dataset  
  maplist = toJSONArray2(mapdf, json = F)
  
  map = Leaflet$new()
  map$set(width = width, height = height)
  map$setView(mapcenter, mapzoom)
  
  ###Add content for popup
  
  maplist = lapply(maplist, function(r){within(r, { 
    #fillColor = cut(
      #were_here_count, 
      #breaks = c(0, 100, 500, 1000, 1100, 20000), 
      #labels = brewer.pal(5, 'RdYlGn'),
      #include.lowest = TRUE
    #) 
    popup = iconv(whisker::whisker.render(
      '<a href = {{fburl}} target = "blank"><img src= {{picture}} height="100" width="200"></a> <br>
      <b>{{name}}</b><br>
      <b>Likes: </b> {{likes}} <br>
      <b>Talking About: </b> {{talking}}<br>
      <b>Were Here </b>: {{here}}<br>
      <b>Status: </b> {{status}}<br>'
    ), from = 'latin1', to = 'UTF-8')
    })
  })
  
  
  ###End Add content for popup
  
  
  map$geoJson(toGeoJSON(maplist, lat = 'lat', lon = 'lng'),
            onEachFeature = '#! function(feature, layer){
    layer.bindPopup(feature.properties.popup)
 } !#',
             pointToLayer =  "#! function(feature, latlng){
    return L.circleMarker(latlng, {
      radius: 5,
      fillColor: feature.properties.color || 'blue',    
      color: '#000',
      weight: 1,
      fillOpacity: 0.8
    })
 } !#"         
  )
  
  map$enablePopover(TRUE)
  map$fullScreen(TRUE)
  return(map)
}


### Data frame for Exploration and Analysis - restaurantdf()
restaurantdf <- function(){
  
  jsonlist = getData();
  Sys.setenv(TZ='Asia/Kuala_Lumpur');
  time = Sys.time();
  
  numofvariables = 43
  data = matrix(0,length(names(jsonlist)), numofvariables)
  data = data.frame(data)
  names(data) = c("name","likes","talking","here", "lat", "lng", 
                  "mon1open","mon1close", "mon2open", "mon2close", 
                  "tue1open","tue1close", "tue2open", "tue2close", 
                  "wed1open","wed1close", "wed2open", "wed2close", 
                  "thur1open","thur1close", "thur2open", "thur2close", 
                  "fri1open","fri1close", "fri2open", "fri2close", 
                  "sat1open","sat1close", "sat2open", "sat2close", 
                  "sun1open","sun1close", "sun2open", "sun2close",
                  "status","color","picture","fburl","category","address",
                  "phone","price","id")
  
  #Convert restaurants to data frame for key variables of interest
  
  for (i in 1:length(names(jsonlist))) {
    data[i,1] = eval(parse(text=sprintf('jsonlist$%s',names(jsonlist)[i])))$name
    data[i,2] = eval(parse(text=sprintf('jsonlist$%s',names(jsonlist)[i])))$likes
    data[i,3] = eval(parse(text=sprintf('jsonlist$%s',names(jsonlist)[i])))$talking_about_count
    data[i,4] = eval(parse(text=sprintf('jsonlist$%s',names(jsonlist)[i])))$were_here_count
    data[i,5] = eval(parse(text=sprintf('jsonlist$%s',names(jsonlist)[i])))$latitude
    data[i,6] = eval(parse(text=sprintf('jsonlist$%s',names(jsonlist)[i])))$longitude
    data[i,37] = eval(parse(text=sprintf('jsonlist$%s',names(jsonlist)[i])))$cover$source
    data[i,38] = eval(parse(text=sprintf('jsonlist$%s',names(jsonlist)[i])))$link
    
    data[i,39] = eval(parse(text=sprintf('jsonlist$%s',names(jsonlist)[i])))$category
    data[i,40] = eval(parse(text=sprintf('jsonlist$%s',names(jsonlist)[i])))$location$street
    data[i,41] = eval(parse(text=sprintf('jsonlist$%s',names(jsonlist)[i])))$phone
    data[i,43] = eval(parse(text=sprintf('jsonlist$%s',names(jsonlist)[i])))$id
    
    if ("price_range" %in% names(eval(parse(text=sprintf('jsonlist$%s',names(jsonlist)[i]))))) {
      data[i,42] = eval(parse(text=sprintf('jsonlist$%s',names(jsonlist)[i])))$price_range
    }
    else {data[i,42] = "No Price Range Indicated"}
    
    if (! "hours" %in% names(eval(parse(text=sprintf('jsonlist$%s',names(jsonlist)[i]))))) {
      data[i,7:34] = 0.5
    }   
    
    if ("hours" %in% names(eval(parse(text=sprintf('jsonlist$%s',names(jsonlist)[i]))))) {
      if ("mon_1_open" %in% names(eval(parse(text=sprintf('jsonlist$%s',names(jsonlist)[i])))$hours)) {
        
        data[i,7] = eval(parse(text=sprintf('jsonlist$%s',names(jsonlist)[i])))$hours$mon_1_open
        data[i,8] = eval(parse(text=sprintf('jsonlist$%s',names(jsonlist)[i])))$hours$mon_1_close
        
      }
      if ("mon_2_open" %in% names(eval(parse(text=sprintf('jsonlist$%s',names(jsonlist)[i])))$hours)) {
        
        data[i,9] = eval(parse(text=sprintf('jsonlist$%s',names(jsonlist)[i])))$hours$mon_2_open
        data[i,10] = eval(parse(text=sprintf('jsonlist$%s',names(jsonlist)[i])))$hours$mon_2_close
        
      }
      
      if ("tue_1_open" %in% names(eval(parse(text=sprintf('jsonlist$%s',names(jsonlist)[i])))$hours)) {
        
        data[i,11] = eval(parse(text=sprintf('jsonlist$%s',names(jsonlist)[i])))$hours$tue_1_open
        data[i,12] = eval(parse(text=sprintf('jsonlist$%s',names(jsonlist)[i])))$hours$tue_1_close
        
      }
      
      if ("tue_2_open" %in% names(eval(parse(text=sprintf('jsonlist$%s',names(jsonlist)[i])))$hours)) {
        
        data[i,13] = eval(parse(text=sprintf('jsonlist$%s',names(jsonlist)[i])))$hours$tue_2_open
        data[i,14] = eval(parse(text=sprintf('jsonlist$%s',names(jsonlist)[i])))$hours$tue_2_close
        
      }
      
      if ("wed_1_open" %in% names(eval(parse(text=sprintf('jsonlist$%s',names(jsonlist)[i])))$hours)) {
        
        data[i,15] = eval(parse(text=sprintf('jsonlist$%s',names(jsonlist)[i])))$hours$wed_1_open
        data[i,16] = eval(parse(text=sprintf('jsonlist$%s',names(jsonlist)[i])))$hours$wed_1_close
        
      }
      
      if ("wed_2_open" %in% names(eval(parse(text=sprintf('jsonlist$%s',names(jsonlist)[i])))$hours)) {
        
        data[i,17] = eval(parse(text=sprintf('jsonlist$%s',names(jsonlist)[i])))$hours$wed_2_open
        data[i,18] = eval(parse(text=sprintf('jsonlist$%s',names(jsonlist)[i])))$hours$wed_2_close
        
      }
      
      if ("thu_1_open" %in% names(eval(parse(text=sprintf('jsonlist$%s',names(jsonlist)[i])))$hours)) {
        
        data[i,19] = eval(parse(text=sprintf('jsonlist$%s',names(jsonlist)[i])))$hours$thu_1_open
        data[i,20] = eval(parse(text=sprintf('jsonlist$%s',names(jsonlist)[i])))$hours$thu_1_close
        
      }
      
      if ("thu_2_open" %in% names(eval(parse(text=sprintf('jsonlist$%s',names(jsonlist)[i])))$hours)) {
        
        data[i,21] = eval(parse(text=sprintf('jsonlist$%s',names(jsonlist)[i])))$hours$thu_2_open
        data[i,22] = eval(parse(text=sprintf('jsonlist$%s',names(jsonlist)[i])))$hours$thu_2_close
        
      }
      
      if ("fri_1_open" %in% names(eval(parse(text=sprintf('jsonlist$%s',names(jsonlist)[i])))$hours)) {
        
        data[i,23] = eval(parse(text=sprintf('jsonlist$%s',names(jsonlist)[i])))$hours$fri_1_open
        data[i,24] = eval(parse(text=sprintf('jsonlist$%s',names(jsonlist)[i])))$hours$fri_1_close
        
      }
      
      if ("fri_2_open" %in% names(eval(parse(text=sprintf('jsonlist$%s',names(jsonlist)[i])))$hours)) {
        
        data[i,25] = eval(parse(text=sprintf('jsonlist$%s',names(jsonlist)[i])))$hours$fri_2_open
        data[i,26] = eval(parse(text=sprintf('jsonlist$%s',names(jsonlist)[i])))$hours$fri_2_close
        
      }
      
      if ("sat_1_open" %in% names(eval(parse(text=sprintf('jsonlist$%s',names(jsonlist)[i])))$hours)) {
        
        data[i,27] = eval(parse(text=sprintf('jsonlist$%s',names(jsonlist)[i])))$hours$sat_1_open
        data[i,28] = eval(parse(text=sprintf('jsonlist$%s',names(jsonlist)[i])))$hours$sat_1_close
        
      }
      
      if ("sat_2_open" %in% names(eval(parse(text=sprintf('jsonlist$%s',names(jsonlist)[i])))$hours)) {
        
        data[i,29] = eval(parse(text=sprintf('jsonlist$%s',names(jsonlist)[i])))$hours$sat_2_open
        data[i,30] = eval(parse(text=sprintf('jsonlist$%s',names(jsonlist)[i])))$hours$sat_2_close
        
      }
      
      if ("sun_1_open" %in% names(eval(parse(text=sprintf('jsonlist$%s',names(jsonlist)[i])))$hours)) {
        
        data[i,31] = eval(parse(text=sprintf('jsonlist$%s',names(jsonlist)[i])))$hours$sun_1_open
        data[i,32] = eval(parse(text=sprintf('jsonlist$%s',names(jsonlist)[i])))$hours$sun_1_close
        
      }
      
      if ("sun_2_open" %in% names(eval(parse(text=sprintf('jsonlist$%s',names(jsonlist)[i])))$hours)) {
        
        data[i,33] = eval(parse(text=sprintf('jsonlist$%s',names(jsonlist)[i])))$hours$sun_2_open
        data[i,34] = eval(parse(text=sprintf('jsonlist$%s',names(jsonlist)[i])))$hours$sun_2_close
        
      }
      
    }
    
    for (j in 7:34) {
      if (data[i,j]=="00:00") {
        data[i,j] = "24:00"
      }
      
    }
    
    if (weekdays(time) == "Monday") {
      
      if (! data[i,7] %in% c(0,0.5) & ! data[i,9] %in% c(0,0.5)) {
        
        if (time >= strptime(data[i,7],"%H:%M") & time <= strptime(data[i,8],"%H:%M")) {
          
          data[i,35] = "open"
          data[i,36] = "green" }
        
        else if (time >= strptime(data[i,9],"%H:%M") & time <= strptime(data[i,10],"%H:%M")) {
          data[i,35] = "open"
          data[i,36] = "green" }
        
        
        else if (time <= strptime("24:00","%H:%M") & time >= strptime(data[i,9],"%H:%M")) {
          data[i,35] = "open"
          data[i,36] = "green"
        }
        
        else if (time <= strptime(data[i,9],"%H:%M") & time >= strptime("00:00","%H:%M") & time <= strptime(data[i,10],"%H:%M") ) {
          data[i,35] = "close"
          data[i,36] = "red"
        }
        
        else if (time >= strptime("00:00","%H:%M") & time <= strptime(data[i,10],"%H:%M") ) {
          data[i,35] = "open"
          data[i,36] = "green"
        }
        
        else {data[i,35] = "close"
              data[i,36] = "red" }
        
      }
      
      if (data[i,7] == 0) {
        data[i,35] = "close"
        data[i,36] = "red"
      }
      
      if (! data[i,7] %in% c(0,0.5) & data[i,9] == 0){
        if (time >= strptime(data[i,7],"%H:%M") & time <= strptime(data[i,8],"%H:%M")) {
          
          data[i,35] = "open"
          data[i,36] = "green" }
        
        else if (time <= strptime("24:00","%H:%M") & time >= strptime(data[i,7],"%H:%M")) {
          data[i,35] = "open"
          data[i,36] = "green"
        }
        
        else if (time <= strptime(data[i,7],"%H:%M") & time >= strptime("00:00","%H:%M") & time <= strptime(data[i,8],"%H:%M")) {
          data[i,35] = "close"
          data[i,36] = "red"
        }
        
        else if (time >= strptime("00:00","%H:%M") & time <= strptime(data[i,8],"%H:%M")) {
          data[i,35] = "open"
          data[i,36] = "green"
        }
        
        else {
          data[i,35] = "close"
          data[i,36] = "red"
          
        }
        
      }
      
      
    }
    
    if (weekdays(time) == "Tuesday") {
      
      if (! data[i,11] %in% c(0,0.5) & ! data[i,13] %in% c(0,0.5)) {
        
        if (time >= strptime(data[i,11],"%H:%M") & time <= strptime(data[i,12],"%H:%M")) {
          
          data[i,35] = "open"
          data[i,36] = "green" }
        
        else if (time >= strptime(data[i,13],"%H:%M") & time <= strptime(data[i,14],"%H:%M")) {
          data[i,35] = "open"
          data[i,36] = "green" }
        
        
        else if (time <= strptime("24:00","%H:%M") & time >= strptime(data[i,13],"%H:%M")) {
          data[i,35] = "open"
          data[i,36] = "green"
        }
        
        else if (time <= strptime(data[i,13],"%H:%M") & time >= strptime("00:00","%H:%M") & time <= strptime(data[i,14],"%H:%M") ) {
          data[i,35] = "close"
          data[i,36] = "red"
        }
        
        else if (time >= strptime("00:00","%H:%M") & time <= strptime(data[i,14],"%H:%M") ) {
          data[i,35] = "open"
          data[i,36] = "green"
        }
        
        else {data[i,35] = "close"
              data[i,36] = "red" }
        
      }
      
      if (data[i,11] == 0) {
        data[i,35] = "close"
        data[i,36] = "red"
      }
      
      if (! data[i,11] %in% c(0,0.5) & data[i,13] == 0){
        if (time >= strptime(data[i,11],"%H:%M") & time <= strptime(data[i,12],"%H:%M")) {
          
          data[i,35] = "open"
          data[i,36] = "green" }
        
        else if (time <= strptime("24:00","%H:%M") & time >= strptime(data[i,11],"%H:%M")) {
          data[i,35] = "open"
          data[i,36] = "green"
        }
        
        else if (time <= strptime(data[i,11],"%H:%M") & time >= strptime("00:00","%H:%M") & time <= strptime(data[i,12],"%H:%M")) {
          data[i,35] = "close"
          data[i,36] = "red"
        }
        
        else if (time >= strptime("00:00","%H:%M") & time <= strptime(data[i,12],"%H:%M")) {
          data[i,35] = "open"
          data[i,36] = "green"
        }
        
        else {
          data[i,35] = "close"
          data[i,36] = "red"
          
        }
        
      }
      
      
    }
    
    if (weekdays(time) == "Wednesday") {
      
      if (! data[i,15] %in% c(0,0.5) & ! data[i,17] %in% c(0,0.5)) {
        
        if (time >= strptime(data[i,15],"%H:%M") & time <= strptime(data[i,16],"%H:%M")) {
          
          data[i,35] = "open"
          data[i,36] = "green" }
        
        else if (time >= strptime(data[i,17],"%H:%M") & time <= strptime(data[i,18],"%H:%M")) {
          data[i,35] = "open"
          data[i,36] = "green" }
        
        
        else if (time <= strptime("24:00","%H:%M") & time >= strptime(data[i,17],"%H:%M")) {
          data[i,35] = "open"
          data[i,36] = "green"
        }
        
        else if (time <= strptime(data[i,17],"%H:%M") & time >= strptime("00:00","%H:%M") & time <= strptime(data[i,18],"%H:%M") ) {
          data[i,35] = "close"
          data[i,36] = "red"
        }
        
        else if (time >= strptime("00:00","%H:%M") & time <= strptime(data[i,18],"%H:%M") ) {
          data[i,35] = "open"
          data[i,36] = "green"
        }
        
        else {data[i,35] = "close"
              data[i,36] = "red" }
        
      }
      
      if (data[i,15] == 0) {
        data[i,35] = "close"
        data[i,36] = "red"
      }
      
      if (! data[i,15] %in% c(0,0.5) & data[i,17] == 0){
        if (time >= strptime(data[i,15],"%H:%M") & time <= strptime(data[i,16],"%H:%M")) {
          
          data[i,35] = "open"
          data[i,36] = "green" }
        
        else if (time <= strptime("24:00","%H:%M") & time >= strptime(data[i,15],"%H:%M")) {
          data[i,35] = "open"
          data[i,36] = "green"
        }
        
        else if (time <= strptime(data[i,15],"%H:%M") & time >= strptime("00:00","%H:%M") & time <= strptime(data[i,16],"%H:%M")) {
          data[i,35] = "close"
          data[i,36] = "red"
        }
        
        else if (time >= strptime("00:00","%H:%M") & time <= strptime(data[i,16],"%H:%M")) {
          data[i,35] = "open"
          data[i,36] = "green"
        }
        
        else {
          data[i,35] = "close"
          data[i,36] = "red"
          
        }
        
      }
      
      
    }
    
    if (weekdays(time) == "Thursday") {
      
      if (! data[i,19] %in% c(0,0.5) & ! data[i,21] %in% c(0,0.5)) {
        
        if (time >= strptime(data[i,19],"%H:%M") & time <= strptime(data[i,20],"%H:%M")) {
          
          data[i,35] = "open"
          data[i,36] = "green" }
        
        else if (time >= strptime(data[i,21],"%H:%M") & time <= strptime(data[i,22],"%H:%M")) {
          data[i,35] = "open"
          data[i,36] = "green" }
        
        
        else if (time <= strptime("24:00","%H:%M") & time >= strptime(data[i,21],"%H:%M")) {
          data[i,35] = "open"
          data[i,36] = "green"
        }
        
        else if (time <= strptime(data[i,21],"%H:%M") & time >= strptime("00:00","%H:%M") & time <= strptime(data[i,22],"%H:%M") ) {
          data[i,35] = "close"
          data[i,36] = "red"
        }
        
        else if (time >= strptime("00:00","%H:%M") & time <= strptime(data[i,22],"%H:%M") ) {
          data[i,35] = "open"
          data[i,36] = "green"
        }
        
        else {data[i,35] = "close"
              data[i,36] = "red" }
        
      }
      
      if (data[i,19] == 0) {
        data[i,35] = "close"
        data[i,36] = "red"
      }
      
      if (! data[i,19] %in% c(0,0.5) & data[i,21] == 0){
        if (time >= strptime(data[i,19],"%H:%M") & time <= strptime(data[i,20],"%H:%M")) {
          
          data[i,35] = "open"
          data[i,36] = "green" }
        
        else if (time <= strptime("24:00","%H:%M") & time >= strptime(data[i,19],"%H:%M")) {
          data[i,35] = "open"
          data[i,36] = "green"
        }
        
        else if (time <= strptime(data[i,19],"%H:%M") & time >= strptime("00:00","%H:%M") & time <= strptime(data[i,20],"%H:%M")) {
          data[i,35] = "close"
          data[i,36] = "red"
        }
        
        else if (time >= strptime("00:00","%H:%M") & time <= strptime(data[i,20],"%H:%M")) {
          data[i,35] = "open"
          data[i,36] = "green"
        }
        
        else {
          data[i,35] = "close"
          data[i,36] = "red"
          
        }
        
      }
      
      
    }
    
    if (weekdays(time) == "Friday") {
      
      if (! data[i,23] %in% c(0,0.5) & ! data[i,25] %in% c(0,0.5)) {
        
        if (time >= strptime(data[i,23],"%H:%M") & time <= strptime(data[i,24],"%H:%M")) {
          
          data[i,35] = "open"
          data[i,36] = "green" }
        
        else if (time >= strptime(data[i,25],"%H:%M") & time <= strptime(data[i,26],"%H:%M")) {
          data[i,35] = "open"
          data[i,36] = "green" }
        
        
        else if (time <= strptime("24:00","%H:%M") & time >= strptime(data[i,25],"%H:%M")) {
          data[i,35] = "open"
          data[i,36] = "green"
        }
        
        else if (time <= strptime(data[i,25],"%H:%M") & time >= strptime("00:00","%H:%M") & time <= strptime(data[i,26],"%H:%M") ) {
          data[i,35] = "close"
          data[i,36] = "red"
        }
        
        else if (time >= strptime("00:00","%H:%M") & time <= strptime(data[i,26],"%H:%M") ) {
          data[i,35] = "open"
          data[i,36] = "green"
        }
        
        else {data[i,35] = "close"
              data[i,36] = "red" }
        
      }
      
      if (data[i,23] == 0) {
        data[i,35] = "close"
        data[i,36] = "red"
      }
      
      if (! data[i,23] %in% c(0,0.5) & data[i,25] == 0){
        if (time >= strptime(data[i,23],"%H:%M") & time <= strptime(data[i,24],"%H:%M")) {
          
          data[i,35] = "open"
          data[i,36] = "green" }
        
        else if (time <= strptime("24:00","%H:%M") & time >= strptime(data[i,23],"%H:%M")) {
          data[i,35] = "open"
          data[i,36] = "green"
        }
        
        else if (time <= strptime(data[i,23],"%H:%M") & time >= strptime("00:00","%H:%M") & time <= strptime(data[i,24],"%H:%M")) {
          data[i,35] = "close"
          data[i,36] = "red"
        }
        
        else if (time >= strptime("00:00","%H:%M") & time <= strptime(data[i,24],"%H:%M")) {
          data[i,35] = "open"
          data[i,36] = "green"
        }
        
        else {
          data[i,35] = "close"
          data[i,36] = "red"
          
        }
        
      }
      
      
    }
    
    
    if (weekdays(time) == "Saturday") {
      
      if (! data[i,27] %in% c(0,0.5) & ! data[i,29] %in% c(0,0.5)) {
      
        if (time >= strptime(data[i,27],"%H:%M") & time <= strptime(data[i,28],"%H:%M")) {
        
          data[i,35] = "open"
          data[i,36] = "green" }
        
        else if (time >= strptime(data[i,29],"%H:%M") & time <= strptime(data[i,30],"%H:%M")) {
          data[i,35] = "open"
          data[i,36] = "green" }
        
              
        else if (time <= strptime("24:00","%H:%M") & time >= strptime(data[i,29],"%H:%M")) {
          data[i,35] = "open"
          data[i,36] = "green"
        }
        
        else if (time <= strptime(data[i,29],"%H:%M") & time >= strptime("00:00","%H:%M") & time <= strptime(data[i,30],"%H:%M") ) {
          data[i,35] = "close"
          data[i,36] = "red"
        }
        
        else if (time >= strptime("00:00","%H:%M") & time <= strptime(data[i,30],"%H:%M") ) {
          data[i,35] = "open"
          data[i,36] = "green"
        }
        
        else {data[i,35] = "close"
              data[i,36] = "red" }
        
      }
      
      if (data[i,27] == 0) {
        data[i,35] = "close"
        data[i,36] = "red"
      }
      
      if (! data[i,27] %in% c(0,0.5) & data[i,29] == 0){
        if (time >= strptime(data[i,27],"%H:%M") & time <= strptime(data[i,28],"%H:%M")) {
          
          data[i,35] = "open"
          data[i,36] = "green" }
        
        else if (time <= strptime("24:00","%H:%M") & time >= strptime(data[i,27],"%H:%M")) {
          data[i,35] = "open"
          data[i,36] = "green"
        }
        
        else if (time <= strptime(data[i,27],"%H:%M") & time >= strptime("00:00","%H:%M") & time <= strptime(data[i,28],"%H:%M")) {
          data[i,35] = "close"
          data[i,36] = "red"
        }
        
        else if (time >= strptime("00:00","%H:%M") & time <= strptime(data[i,28],"%H:%M")) {
          data[i,35] = "open"
          data[i,36] = "green"
        }
        
        else {
          data[i,35] = "close"
          data[i,36] = "red"
          
        }
        
      }
      
      
    }
      
            
    
    if (weekdays(time) == "Sunday") {
      
      if (! data[i,31] %in% c(0,0.5) & ! data[i,33] %in% c(0,0.5)) {
        
        if (time >= strptime(data[i,31],"%H:%M") & time <= strptime(data[i,32],"%H:%M")) {
          
          data[i,35] = "open"
          data[i,36] = "green" }
        
        else if (time >= strptime(data[i,33],"%H:%M") & time <= strptime(data[i,34],"%H:%M")) {
          data[i,35] = "open"
          data[i,36] = "green" }
        
        
        else if (time <= strptime("24:00","%H:%M") & time >= strptime(data[i,33],"%H:%M")) {
          data[i,35] = "open"
          data[i,36] = "green"
        }
        
        else if (time <= strptime(data[i,33],"%H:%M") & time >= strptime("00:00","%H:%M") & time <= strptime(data[i,34],"%H:%M") ) {
          data[i,35] = "close"
          data[i,36] = "red"
        }
        
        else if (time >= strptime("00:00","%H:%M") & time <= strptime(data[i,34],"%H:%M") ) {
          data[i,35] = "open"
          data[i,36] = "green"
        }
        
        else {data[i,35] = "close"
              data[i,36] = "red" }
        
      }
      
      if (data[i,31] == 0) {
        data[i,35] = "close"
        data[i,36] = "red"
      }
      
      if (! data[i,31] %in% c(0,0.5) & data[i,33] == 0){
        if (time >= strptime(data[i,31],"%H:%M") & time <= strptime(data[i,32],"%H:%M")) {
          
          data[i,35] = "open"
          data[i,36] = "green" }
        
        else if (time <= strptime("24:00","%H:%M") & time >= strptime(data[i,31],"%H:%M")) {
          data[i,35] = "open"
          data[i,36] = "green"
        }
        
        else if (time <= strptime(data[i,31],"%H:%M") & time >= strptime("00:00","%H:%M") & time <= strptime(data[i,32],"%H:%M")) {
          data[i,35] = "close"
          data[i,36] = "red"
        }
        
        else if (time >= strptime("00:00","%H:%M") & time <= strptime(data[i,32],"%H:%M")) {
          data[i,35] = "open"
          data[i,36] = "green"
        }
        
        else {
          data[i,35] = "close"
          data[i,36] = "red"
          
        }
        
      }
      
      
    }
    
    if (! "hours" %in% names(eval(parse(text=sprintf('jsonlist$%s',names(jsonlist)[i]))))) {
      
      data[i,35] = "unknown"
      data[i,36] = "blue"
      
    }		
    
  }
  
  
  
  
  
  
  return(data)
}






