rast_predict_se = function(...) {
  pmat = predict(...)
  # calculate standard error from 95% confidence,
  # which are symmetric in this situation
  error = cbind(
    pmat[,1] - pmat[,2],
    pmat[,3] - pmat[,1]
  ) %>% 
    rowMeans %>% 
    divide_by(1.96)
  out = cbind(coefficients=pmat[,1], 
              se=error)
  return(out)
}