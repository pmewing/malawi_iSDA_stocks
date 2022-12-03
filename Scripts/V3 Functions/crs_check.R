crs_check = function(x, epsg, categorical=FALSE) {
  out_epsg = strsplit(epsg, ':')[[1]][2] %>% 
    as.numeric
  if (any(class(x) == 'SpatRaster')) {
    in_epsg = crs(x, describe=TRUE)[, 'code'] %>% 
      as.numeric
    if (in_epsg != out_epsg) {
      warning(
        paste('Reprojecting from EPSG:', in_epsg, 
              ' to EPSG:', out_epsg, '\n', 
              sep='')
      )
      transform_method = ifelse(categorical, 'near', 'bilinear')
      x %<>% project(epsg, method=transform_method)
    }
  } else if (any(class(x) == 'sf')) {
    in_epsg = st_crs(x)$epsg
    if (in_epsg != out_epsg) {
      warning(
        paste('Reprojecting from EPSG:', in_epsg, 
              ' to EPSG:', out_epsg, 
              sep='')
      )
      x %<>% st_transform(epsg)
    }
  } else {
    stop("Don't recongize x. Is it a <terra> or <sf> object?")
  }
  return(x)
}
