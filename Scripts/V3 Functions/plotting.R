
gg_themer = function(x, remove_strip=FALSE, theme_color=TRUE, theme_fill=TRUE) {
  require(ggplot2)
  
  out = x + 
    theme_minimal() +
    theme(panel.grid=element_blank(),
          axis.ticks=element_line(color='gray',
                                  size=0.2),
          axis.title=element_text(size=9),
          title=element_text(size=8))
  
  if (remove_strip) {  # for the bottom panel of fully faceted figures
    out = out + theme(strip.text=element_blank())
  }
  
  # Auto-set colors
  if (length(theme_color) > 1) {
    out = out + 
      scale_color_manual(values=theme_color)
  } else if (is.logical(theme_color)) {
    if (theme_color) {
      out = out +
        scale_color_brewer(palette='Paired')
    }
  } else if (is.character(theme_color)) {
    out = out +
      scale_color_brewer(palette=theme_color)
  }
  
  if (length(theme_fill) > 1) {
    out = out + 
      scale_fill_manual(values=theme_fill)
  } else if (is.logical(theme_fill)) {
    if (theme_fill) {
      out = out +
        scale_fill_brewer(palette='Paired')
    }
  } else if (is.character(theme_fill)) {
    out = out +
      scale_fill_brewer(palette=theme_fill)
  }

  
  return(out)
}

scatterplot_formatter = function(x, equal_scales=TRUE) {
  if (equal_scales) {
    x = x +
      coord_equal()
  }
  x = x +
    geom_smooth(method='loess', 
                formula='y~x',
                size=0.5,
                color='tomato2',
                fill=NA) +
    geom_smooth(method='lm', 
                formula='y~x',
                alpha=0.2,
                size=0.5,
                color='steelblue4',
                fill='steelblue4') +
    geom_abline(aes(intercept=0,
                    slope=1),
                linetype='dashed',
                size=0.2)
  return(x)
}

corplot_title = function(x=NULL, rsq=NULL, moran=NULL, rmse=NULL, AIC=NULL) {
  if (!is.null(rsq)) {
    rsq = paste('Rsq =', round(rsq, 3))
  }
  
  if (!is.null(moran)) {
    moran = paste(
      "; Moran's I = ",
      round(moran$observed, 3),
      ifelse(moran$p.value < 0.05, '*', ''),
      sep='')
  }
  
  if (!is.null(rmse)) {
    rmse = paste('; RMSE =', signif(rmse, 3))
  }
  
  if (!is.null(AIC)) {
    AIC = paste('; AIC =', signif(AIC, 4))
  }
  
  if (!is.null(x)) x = paste0(x, '; ')
  paste(
    x,
    rsq,
    moran,
    rmse,
    AIC,
    sep='')
}


plot_glm = function(model, locations=NULL, colors=NULL, equal_scales=TRUE, palette=TRUE) {
  
  stopifnot('glm' %in% class(model))
  
  ff = model$formula %>% 
    as.character
  ff = paste(ff[2], ff[1], ff[3])
  
  mod_x = fitted(model)
  mod_y = model$y
  
  rsq = with(model, 1-(deviance/null.deviance))
  
  if (is.null(locations)) {
    mor_I = list(observed=NA, p.value=1)
  } else {
    mor_I = moran_idw(residuals(model),   # in ./Scripts/Functions/Prediction.R. Args: data, coordinates
                      locations[names(residuals(model)), ])    
  }

  mod_rmse = rmse(residuals(model))  # in ./Scripts/Functions/Prediction.R.

  
  
  # Draw main plot depending on whether to color points by a category
  if (is.null(colors)) {
    plt = qplot(mod_x, mod_y, geom='point')
    
  } else {
    
    colors = as.data.frame(colors)[names(mod_x), 1]
    if (class(colors) == 'character') colors %<>% factor
    
    
    if (length(palette > 1) & length(names(palette) > 1)) {
      lvls = levels(droplevels(colors))
      palette = palette[lvls]
    }
    
    pltdf = data.frame(x = mod_x,
                       y = mod_y,
                       color = colors)
    plt = ggplot(pltdf,
                 aes(x=mod_x,
                     y=mod_y,
                     color=colors)) +
      geom_point() +
      scale_color_brewer(palette='Paired') 
    
  }
  
  # Format
  plt = scatterplot_formatter(plt, equal_scales)
  plt = plt + 
    labs(x='Predicted',
         y='Measured',
         title=ff,
         subtitle=corplot_title(x=NULL,  # in plotting.R
                                rsq, 
                                mor_I,
                                mod_rmse),
         color=NULL) 
  plt = gg_themer(plt, theme_color=palette)
  
  return(plt)
}


plot_isda_empirical = function(x, y, color=NULL, data, equal_scales=TRUE) {
  mm = paste(y, x, sep='~') %>% 
    formula %>% 
    lm(data=data)
  
  rsq = summary(mm)$r.squared
  mod_rmse = rmse(residuals(mm))
  mor_I = moran_idw(residuals(mm),   # in ./Scripts/Functions/Prediction.R. Args: data, coordinates
                   data[names(residuals(mm)), ])
  
  plt = ggplot(data,
               aes_string(x=x,
                          y=y)) +
    scale_color_brewer(palette='Paired')
  plt = plot_formatter(plt, equal_scales)  # axis scales, trendlines
  
  if (is.null(color)) {
    plt = plt +
      geom_point()
  } else {
    plt = plt +
      geom_point(aes_string(color=color))
  }
  plt = plt +
    labs(x='iSDA Prediction',
         y='Measured',
         title=paste(y, x, sep='~'),
         subtitle=corplot_title(NULL,
                                rsq, 
                                mor_I,
                                mod_rmse))
  plt = gg_themer(plt)

  return(plt)
}


plot_glm_diagnostics = function(model) {
  par(mfrow=c(2,2))
  plot(model)
  par(mfrow=c(1,1))
}

