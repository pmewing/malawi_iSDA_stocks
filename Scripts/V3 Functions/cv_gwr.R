cv_gwr = function(obj, data, k=10, groups=NULL, seed=NULL, plot=TRUE, parallel=TRUE) {
  # groups can pre-defined as a list of length k used to index rows of the data frame.
  # using either rownames or index values. If provided, k is ignored
#  # base_formula is for formulas with wrappers, ex. poly, to make the right model frame. It should be optional.
  
  # Data must be spatial, and must be included. Observations and coordinates will be extracted
  # from that using rownames.
  require(parallel)
  
  ff = obj$GW.arguments$formula
  nobs = nrow(obj$lm$model)
  kernel=obj$GW.arguments$kernel
  adaptive=obj$GW.arguments$adaptive
  longlat=obj$GW.arguments$longlat
  # fam = obj$family
  
  # convert data to a spatialpointsdataframe.
  if (!('SpatialPointsDataFrame' %in% class(data))) {
    data %<>% as('Spatial')
  }
  
  # infer observations
  obs = obj$lm$model %>% 
    rownames
  if (is.null(obs)) {
    obs = seq_len(nobs)
    stopifnot(nobs==nrow(data), 
              "Data might have missing observations and lacks rownames. Add rownames to data and re-run gwmod.")
  }
  dd = data[obs, ]
  mmat = cbind(model.frame(ff, dd)[, 1, drop=FALSE], 
               model.matrix(ff, dd)[, -c(1), drop=FALSE])
  dd = 
    SpatialPointsDataFrame(
      coordinates(dd),
      data.frame(mmat), 
      proj4string=CRS(proj4string(dd))
    )
  
  # dipsy doodling to make column names and the formula work together with things like poly()
  ff %<>% as.character
  ff[3] = names(dd)[-c(1)] %>% 
    paste(collapse='+')
  ff = paste(ff[2], ff[3], sep='~') %>%
    formula
  
  # dd = model.frame(ff, data) %>% 
  #   SpatialPointsDataFrame(
  #     coordinates(data), 
  #     .
  #   )
  
  if (is.null(groups)) {
    # generate groups
    if (k == 1) {  # LOO
      groups = seq_len(nobs) %>% 
        as.character %>% 
        as.list
      
    } else {
      if (!is.null(seed)) set.seed(seed)
      
      size = ceiling(nobs/k)
      ix = seq_len(k) %>% 
        rep(size) %>% 
        .[seq_len(nobs)]
      
      rand = rownames(dd) %>% 
        sample(replace=FALSE)
      
      groups = seq_len(k) %>% 
        lapply(function(x) {
          rand[ix == x]
        })
    }
  }
  
  
  # recalculate and predict
  # set up parallel computing?
  # if (parallel) {
  #   cores = detectCores()
  # } else {
  #   cores = 1
  # }
  # cl = makeCluster(cores)
  
  out = lapply(groups, function(x) {
    # subset
    if (class(x) == 'character') {  # get numeric indices
      x = which(obs %in% x)
    }
    
    
    valid = dd[x, ]  # leave these out
    train = dd[-x, ]
    
    distmat_train = gw.dist(coordinates(train), 
                            longlat=longlat)
    distmat_valid = gw.dist(coordinates(valid),
                            coordinates(train),
                            longlat=longlat)
    bandwidth = bw.gwr(ff, 
                       data=train,
                       kernel=kernel,
                       adaptive=adaptive,
                       longlat = longlat,
                       dMat=distmat_train)
    
    pred =
      gwr.predict(ff,
                  data=train,
                  predictdata=valid,
                  bw=bandwidth,
                  kernel=kernel,
                  adaptive=adaptive,
                  longlat=longlat,
                  dMat1=distmat_valid,
                  dMat2=distmat_train)
    
    data.frame(cv_fit = pred$SDF$prediction,
               se = pred$SDF$prediction_var) %>% 
      set_rownames(obs[x])
  }) %>% 
    set_names(NULL)
  
  out %<>%
    do.call(rbind, .) %>%
    .[obs, ]     #reorder to match input
  
  if (plot) {
    plot(obj$SDF$yhat,
         out$cv_fit,
         xlab='Fitted',
         ylab='Predicted')
    abline(a=0, b=1, col='blue')
  }
  
  return(out)
}
