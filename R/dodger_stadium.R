
tf_cut = tf1 %>% mutate(polygon_idx=row_number()) %>% 
  filter(ELEV>=550, ELEV<610) %>% arrange(-Shape_Area)
target_lat = 34.074130
target_long = -118.239838

get_list <- function() {
  idx_list = c()
  best_ds = 100
  for ( i in 1:nrow(tf_cut)) {
    polygon_idx = tf_cut[i,]$polygon_idx
    df2 =  spTransform(df1[polygon_idx,], CRS("+proj=longlat +datum=WGS84"))
    
    tt = tidy(df2)
    dx = mean(tt$long) - target_long
    dy = mean(tt$lat) - target_lat
    ds = dx**2 + dy**2
    if (ds < best_ds) {
      idx_list <- c(idx_list, polygon_idx)
      cat(sprintf("%d %d %.3e %.3e", i, polygon_idx, ds, best_ds))
      best_ds <- ds
    }
    
    if (i %% 1000 == 0) {
      print(paste('-------------------', i, nrow(tf_cut)))
    }
  }

  idx_list
}

plot_poly <- function(polygon_idx) {
  df2 =  spTransform(df1[polygon_idx,], CRS("+proj=longlat +datum=WGS84"))
  tt = tidy(df2) %>% mutate(region=as.factor(group))
  ggplot(tt) + geom_map(map=tt, aes(x=long, y=lat, map_id=region), fill='white', color='black')
  
}
