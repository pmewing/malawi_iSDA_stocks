run_glmnet = function(formula=NULL, data=NULL, covars=NULL, link='identity', alpha=0.95) {
  
  y = model.frame(formula, data)
  
  x = model.matrix(formula, data=y)[, -c(1), drop=FALSE]
  y = as.matrix(y)[, 1, drop=FALSE]
  
  mod = glmnet(x, y, alpha=alpha, family=gaussian(link=link))
  cv_mod = cv.glmnet(x, y, alpha=alpha, family=gaussian(link=link))
  
  par(mfrow=c(1,2))
  plot(mod)
  plot(cv_mod)
  par(mfrow=c(1,1))
  
  out = list(mod = mod,
             cv_mod = cv_mod)
  return(out)
}

glm_from_glmnet = function(data=NULL, lasso=NULL, y, link='identity', plot=TRUE) {
  net_form = glmnet_predictors(lasso$cv_mod) %>%    # in ./Scripts/Functions/Prediction.R
    paste(collapse='+') %>% 
    paste(y, '~', .) %>% 
    gsub(':', '*', .) %>%
    formula 
  net_mod = glm(net_form,
                data=data, 
				family=gaussian(link=link))
  
  if (plot) {# glm.diag.plots(net_mod, iden=TRUE)
    par(mfrow=c(2,2))
    plot(net_mod)
    par(mfrow=c(1,1))
  }
  
  return(net_mod)
}

glmnet_predictors = function(x, s='lambda.1se') {
  mat = coef(x, s='lambda.1se')
  is_kept = mat[, 1] != 0
  out = rownames(mat)[is_kept]
  out = out[-c(1)]
  return(out)
}


logit = function(x) qlogis(x)
ilogit = function(x) plogis(x)
