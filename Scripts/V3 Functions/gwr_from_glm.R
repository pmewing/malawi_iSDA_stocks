gwr_from_glm = function(model, data=NULL, transform='identity', kernel='gaussian', longlat=TRUE, adaptive=TRUE) {
  # Extracts and subsets data to ensure using the same input data for comparing model performance.
  
  require(GWmodel)
  
  if (!(any(class(data) == 'Spatial'))) {
    data %<>% as('Spatial')
  }
  
  rows = rownames(model$model)
  formula = model$formula
  
  data %<>% .[rows,]
  
  if (transform != 'identity') {
    y = as.character(formula)[2]
    data@data[, y] %<>% 
      list %>% 
      do.call(transform, .)
  }
  
  distmat = gw.dist(coordinates(data), 
                    longlat=longlat)
  
  bandwidth = bw.gwr(formula, 
                      data=data,
                      # family=gaussian(link=transform),
                      kernel=kernel,
                      adaptive=adaptive,
                      longlat=longlat,
                      dMat=distmat)
  
  gwmod = gwr.basic(formula, 
                     data=data,
                     # family=gaussian(link=transform),
                     bw=bandwidth,
                     kernel=kernel,
                     adaptive=adaptive,
                     longlat=TRUE,
                     dMat=distmat)
  
  attr(gwmod, 'transform') = transform
  
  return(gwmod)
}

