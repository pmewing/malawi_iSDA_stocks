generate_cv_groups = function(data, column) {
  out = data %>% 
    data.frame %>% 
    split(.[, column]) %>% 
    lapply(rownames)
  is_empty = sapply(out, function(x) length(x) == 0)
  out[is_empty] = NULL
  return(out)
}

cv_glm = function(obj, data=NULL, k=10, groups=NULL, base_formula=NULL, seed=NULL, plot=TRUE) {
  # groups can pre-defined as a list of length k used to index rows of the data frame.
  # using either rownames or index values. If provided, k is ignored
  # base_formula is for formulas with wrappers, ex. poly, to make the right model frame. 
  ff = obj$formula
  fam = obj$family
  dd = obj$model
  if (is.null(rownames(dd))) {
    rownames(dd) = seq_len(nrow(dd))
  }
  obs = rownames(dd)
  
  if (!is.null(data)) {
    dd = data[obs, ]
    y = model.frame(ff, data=dd)[, 1, drop=FALSE]
    x = model.matrix(ff, data=dd)[, -c(1), drop=FALSE]
    colnames(x) %<>% gsub('\\)', '', .) %>% 
      gsub('\\(', '', .) %>% 
      gsub('\\=', '', .) %>% 
      gsub(',', '', .) %>% 
      gsub(' ', '', .)
    ff = paste(colnames(x), collapse='+') %>% 
      paste(colnames(y), ., sep='~') %>% 
      formula
    dd = data.frame(y, x)
  }
  
  if (is.null(groups)) {
    # generate groups
    if (k == 1) {  # LOO
      groups = nrow(dd) %>% 
        seq_len %>% 
        as.character %>% 
        as.list
      
    } else {
      if (!is.null(seed)) set.seed(seed)
      
      n = nrow(dd)
      size = ceiling(n/k)
      ix = seq_len(k) %>% 
        rep(size) %>% 
        .[seq_len(n)]
      
      rand = rownames(dd) %>% 
        sample(replace=FALSE)
      
      groups = seq_len(k) %>% 
        lapply(function(x) {
          rand[ix == x]
        })
    }
  }
  
  # recalculate and predict
  out = lapply(groups, function(x) {
    # subset
    if (class(x) == 'character') {  # get numeric indices
      x = which(rownames(dd) %in% x)
    }
    
    valid = dd[x, ]  # leave these out
    train = dd[-x, ]
    
    pred = 
      glm(ff, family=fam, data=train) %>% 
      predict(valid, 
              type='response',
              se.fit=TRUE)
    data.frame(cv_fit = pred$fit,
               se = pred$se.fit,
               residual.scale = rep(pred$residual.scale, length(x)))
  }) %>% 
    set_names(NULL)
  
  out %<>%
    do.call(rbind, .) %>%
    .[rownames(dd), ]     #reorder to match input
  
  if (plot) {
    plot(predict(obj, type='response'),
         out$cv_fit,
         xlab='Fitted',
         ylab='Cross Validated')
    abline(a=0, b=1, col='blue')
  }
  
  return(out)
}

RMSPE = function(obs, pred) {
  err = pred - obs
  out = sqrt(mean(err^2, na.rm=TRUE))
  return(out)
}

RPE = function(obs, pred) {
  rmspe = RMSPE(obs, pred)
  exp_val = mean(obs, na.rm=TRUE)
  out = rmspe/exp_val
  return(out)
}

co_det = function(obs, pred) {
  SSR = sum((obs-pred)^2, na.rm=TRUE)
  SST = sum((obs-mean(obs, na.rm=TRUE))^2, na.rm=TRUE)
  out = 1-(SSR/SST)
  return(out)
}

moran_idw = function(vals, coords, na.rm=FALSE) {
  require(sf)
  require(ape)
  
  ww = 1/st_distance(coords) %>% 
    as('matrix')
  diag(ww) = 0
  
  
  morans = Moran.I(vals, ww, na.rm=na.rm)
  return(morans)
}


prediction_indices = function(pred, obs, moran_I=FALSE) {
  # pred: output of cv_glm
  # obs: 1-column data.frame or sf
  # moran_I: calculate moran's I? requires `obs` to be of class 'sf'
  
  obs = obs[rownames(pred), ]
  pred = pred[, 'cv_fit']
  
  if ('sf' %in% class(obs)) {
    coords = obs
    obs = data.frame(obs)[, 1]
  } else {
    moran_I = FALSE
  }
  
  out = c(
    RMSPE = RMSPE(obs, pred),
    RPE = RPE(obs, pred),
    R2 = co_det(obs, pred)
  )
  
  if (moran_I) {
    moran_I = moran_idw(pred - obs,
                        coords)
    out = c(
      out, 
      moran_I = moran_I$observed,
      moran_I_pr = moran_I$p.value
    )
  }
  
  out %<>%
    round(3) %T>%
    print
  
  return(out)
  
}
