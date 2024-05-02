

GroupwiseLMs <- function(data, predictor, response, group_fct1, file_suffix=".fixeff_coefficients.dwplt.png", dir="./", 
                         conf_level=0.05, group_fct2=NA, just_coef=FALSE, ...) { 
  
  # Dependencies
  require(broom)
  require(rlist)
  require(mgsub)
  
  print(1-conf_level)
  
  # Initialise output object
  lm_df <- data.frame()
  lm_tidy.df <- data.frame()
  
  # If one grouping factor to loop over
  if (!is.na(group_fct2)) {
    
    # Loop over patients
    for (id in unique(data[[group_fct1]])) {
      
      # Loop over organisms
      for (org in unique(data[[group_fct2]])) {
        
        # Subset dataframe
        sub_df <- data[data[[group_fct1]] == id & data[[group_fct2]] == org,]
        
        # If CFU is zero
        if (sum(sub_df$CFU) > 0) {
          
          # If CFU is zero
          print(paste0("### Fitting lm for ",group_fct1,": ",id, " // ", group_fct2,": ",org))
          
          # Fit lm
          lm_fit <- lm(as.formula(paste(response, "~", paste(predictor, collapse=" + "))), data=sub_df)
          coef_df <- data.frame(t(coef(lm_fit)))
          
          # Assemble output
          lm_df <- rbind(lm_df, 
                          data.frame(GROUP1=id, GROUP2=org, coef_df))
          
          # Add to list
          lm_tidy.df <- rbind(lm_tidy.df, 
                               cbind(GROUP1=id, GROUP2=org, broom::tidy(lm_fit, conf.int = TRUE, conf.level = conf_level)))
        }
      }
    }
    
    # Set proper column names
    colnames(lm_tidy.df)[1:2] <- c(group_fct1, group_fct2)
    colnames(lm_tidy.df)[1:2] <- c(group_fct1, group_fct2)
    
    # Else if just one grouping factor
  } else {
    
    # Loop over patients
    for (id in unique(data[[group_fct1]])) {
      
      # Subset dataframe
      sub_df <- data[data[[group_fct1]] == id,]
      
      # If CFU is zero
      print(paste0("### Fitting lm for ",group_fct1,": ",id))
      
      # Beautify variable names
      colnames(sub_df) <- str_to_title(tolower(mgsub(colnames(sub_df),c("Z_","Z ","CTINE$"), c("","","CTIN"))))
      predictor <- str_to_title(tolower(mgsub(predictor,c("Z_","Z ","CTINE$"), c("","","CTIN"))))
      response <- str_to_title(tolower(mgsub(response,c("Z_","Z ","CTINE$"), c("","","CTIN"))))
      
      print(sub_df)
      
      # Fit lm
      lm_fit <- lm(as.formula(paste(response, "~", paste(predictor, collapse=" + "))), data=sub_df)
      coef_df <- data.frame(t(coef(lm_fit)))
      
      # Plot model (fixed effects)
      fe_plt <- plot_model(lm_fit, type = "est", show.values = TRUE, 
                           ci.lvl=1-conf_level, 
                           p.threshold=conf_level,
                           show.p=TRUE,
                           dot.size=1,
                           value.offset = 0.3,
                           vline.color="darkgrey") + 
        ggtitle(paste0(group_fct1, ": ", id)) +
        theme(axis.text=element_text(size=24),
              axis.title=element_text(size=36)) +
        theme_bw()
      
      # Save random effects model
      ggsave(paste0(dir,group_fct1,"/",group_fct1,"_",id,file_suffix), fe_plt, units="px", ...)
      
      # Assemble output
      lm_df <- rbind(lm_df, 
                      data.frame(GROUP1=id, coef_df))
      
      # Add to list
      lm_tidy.df <- data.frame(rbind(lm_tidy.df, 
                                     cbind(GROUP1=id, broom::tidy(lm_fit, conf.int = TRUE, conf.level = conf_level))))
      
    }
    
    # Set proper column names
    colnames(lm_df)[1] <- group_fct1
    colnames(lm_tidy.df)[1] <- group_fct1
    
  }
  
  # If we want just df of coefficients or more complex output
  if (just_coef) { return(lm_df) }
  else { return(lm_tidy.df) }
  
}


# Get nice boxplot
NiceBoxPlot <- function(data, x_val, y_val, facet_val_cols, facet_val_rows="DUMMY", facet_scale="free_x", facet_space="free_x", col_val, cols, x_title, y_title, 
                        facet_by_separate_plot=TRUE, extra=geom_blank(), max_y=10, min_y=0, comparisons=NA, label_y=NA, ...) {
  
  # Dependencies
  require(ggplot2)
  require(ggpubr)
  require(patchwork)
  
  # Set comparisons if nto set
  if (any(is.na(comparisons))) { comparisons <- list(unique(data[[x_val]])) }
  
  # Add dummy variable for rows
  data <- data.frame(DUMMY="", data)
  
  if (facet_by_separate_plot) {
    
    # Loop over plots
    plts <- do.call(patchwork::wrap_plots, lapply(as.character(unique(data[[facet_val_cols]])), function(grp) {
      
      # Set y position of brackets if unset
      if (any(is.na(label_y))) { label_y <- max(data[data[[facet_val_cols]] == grp,][[y_val]]) }
      
      # Plot
      plt <- ggplot(data[data[[facet_val_cols]] == grp,], aes(x=.data[[x_val]], y=.data[[y_val]], fill=.data[[col_val]])) + 
        geom_boxplot(aes(group=paste0(.data[[facet_val_cols]], .data[[x_val]]))) +
        facet_grid(rows=vars(.data[[facet_val_rows]]), scale=facet_scale, space=facet_space) +
        scale_fill_manual(values=cols) +
        ylim(min_y, max_y) +
        labs(x=x_title, y=y_title, title=paste0(grp)) +
        theme_bw(base_size=16) + theme(axis.title=element_text(size=24),
                                       plot.title=element_text(size=32),
                                       legend.position="none") +
        guides(fill=guide_legend(nrow=1,byrow=TRUE)) + 
        extra
      
      # Remove row facet if not properly facetting by rows
      if (facet_val_rows == "DUMMY") { plt <- plt + theme(strip.background.y = element_blank()) }
      
      # Return
      return(plt)
      
    }))
    
  } else {
    
    # Set y position of brackets if unset
    if (any(is.na(label_y))) { label_y <- max(data[[y_val]]) }
    
    # Plot
    plts <- ggplot(data, aes(x=.data[[x_val]], y=.data[[y_val]], fill=.data[[col_val]])) + 
      geom_boxplot(aes(group=paste0(.data[[facet_val_cols]], .data[[x_val]]))) +
      scale_fill_manual(values=cols) +
      ylim(min_y, max_y) + 
      facet_grid(cols=vars(.data[[facet_val_cols]]), rows=vars(.data[[facet_val_rows]]), 
                 scale=facet_scale, space=facet_space) +
      labs(x=x_title, y=y_title) +
      theme_bw(base_size=16) + theme(axis.title=element_text(size=24),
                                     plot.title=element_text(size=32),
                                     legend.position="none") +
      guides(fill=guide_legend(nrow=1,byrow=TRUE)) + 
      extra
    
    # Remove row facet if not properly facetting by rows
    if (facet_val_rows == "DUMMY") { plts <- plts + theme(strip.background.y = element_blank()) }
    
  }
  
  # Return
  return(plts)
  
}  



DWPlot <- function(model_df, pval, plot_colors, filename) {
  
  # Add significance colouring
  model_df["SIG"] <- paste0("p < ",round(pval,4))
  model_df[model_df$p.value >= pval,]["SIG"] <- "NS"
  
  # Plot
  dw_plt <- dwplot(model_df, dot_args = list(aes(color = SIG, fill=SIG)), ci=1-pval) +
    facet_grid(rows=vars(model), scale="free_y", space="free_y") +
    geom_vline(xintercept=0, linetype="dashed", color="darkgrey") +
    scale_color_manual(name = "Weeks", values=plot_colors) + 
    theme_bw(base_size=12) + theme(strip.text.y = element_text(angle=0),
                                   axis.title = element_text(size=18),
                                   legend.position="none") + 
    guides(fill=guide_legend(nrow=2,byrow=TRUE))
  
  # Save
  ggsave(filename, dw_plt, units="px", width=1750, height=1250)
  
}


