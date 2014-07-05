## @knitr loadLibraries
require(RJSONIO); require(rCharts); require(RColorBrewer); require(httr); require(jsonlite);require(googleVis)
options(stringsAsFactors = F)

#source(file="server.r")

###Feeds Extraction - getFeeds()

getFeeds <- function(restaurantid){
  require(httr)
  url = sprintf('https://graph.facebook.com/%s?fields=posts&access_token=100846013320730|f22d0343ce8702bac69d76195c8887a6',restaurantid)
  feeds = fromJSON(url)
  return(feeds)
   
}

###Feeds Organization - feedsdf()

feedsdf <- function(restaurantid){
  df = getFeeds(restaurantid)$posts$data
  df1 = cbind(df$created_time,df$picture,df$link,df$message)
  df1 = data.frame(df1)
  names(df1) = c("Time","Picture","Link","Update")
  for (i in 1:nrow(df1)){
    if (is.na(df1[i,4])){
      df1[i,4]=":)"
    }
  }
  for (i in 1:nrow(df1)){
    if (is.na(df1[i,2])){
      df1[i,2]='https://scontent-a-sin.xx.fbcdn.net/hphotos-xaf1/t1.0-9/30483_139712922706103_6946058_n.jpg'
    }
  }
  for (i in 1:nrow(df1)){
    if (is.na(df1[i,3])){
      df1[i,3]=df1[i,2]
    }
  }
  
  
  return(df1)

}