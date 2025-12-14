#-----------------
# Packages!
#-----------------
library(ggplot2)
library(gridExtra)


#-----------------
# Function: Compute mean trend of GAD-7 over visit number
#-----------------
compute_mean_trend_wrt_visit_num = function(gad7_dat, cov_name) {
  
  if (is.null(cov_name)) {
    
    #- - - - - - -
    # Overall mean trend (no covariate)
    #- - - - - - -
    # Compute mean GAD-7 value by Visit_Num
    gad7_dat_mean = aggregate(
      gad7_dat$GAD_7_value, 
      list(Visit_Num = gad7_dat$Visit_Num), 
      mean, na.rm = T
    )
    colnames(gad7_dat_mean)[2] = 'GAD_7_value' 
    
    
    #- - - - - - -
    # Create spaghetti plot with overall mean overlay
    #- - - - - - -
    p_spaghetti_gad7_visit_id = 
      ggplot(data = gad7_dat, aes(x = Visit_Num, y = GAD_7_value, group = factor(MRN))) +
      geom_line(alpha = 0.25) +
      scale_x_continuous(breaks = 0:20, labels = 0:20) +
      xlab('Visit Number') + ylab('') + 
      ggtitle('Overall GAD-7 Value') + 
      geom_point(
        data = gad7_dat_mean,
        aes(x = Visit_Num, y = GAD_7_value, group = NULL, color = 'red')
      ) +
      geom_line(
        data = gad7_dat_mean,
        aes(x = Visit_Num, y = GAD_7_value, group = NULL, color = 'red')
      ) +
      theme(
        plot.title = element_text(hjust = 0.5, size = 15),
        panel.background = element_rect(
          fill   = "white",
          colour = "black",
          linewidth = 0.5, linetype = "solid"
        ),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = c(10, 10),
        legend.title = element_blank(),
        plot.margin = margin(.2, .2, .2, .2, 'cm')
      )
    
  } else {
    
    
    #- - - - - - -
    # Mean trend stratified by covariate (cov_name)
    #- - - - - - -
    # Compute mean GAD-7 by Visit_Num and covariate
    gad7_dat_mean_X = aggregate(
      gad7_dat$GAD_7_value, 
      list(Visit_Num = gad7_dat$Visit_Num, X = gad7_dat[, cov_name]), 
      mean, na.rm = T
    )
    colnames(gad7_dat_mean_X)[c(2,3)] = c(cov_name, 'GAD_7_value')
    
    
    #- - - - - - -
    # Create spaghetti plot with covariate facets and mean overlay
    #- - - - - - -
    p_spaghetti_gad7_visit_id = 
      ggplot(data = gad7_dat, aes(x = Visit_Num, y = GAD_7_value, group = factor(MRN))) +
      geom_line(alpha = 0.25) +
      scale_x_continuous(breaks = 0:20, labels = 0:20) + 
      xlab('Visit Number') + ylab('') + 
      ggtitle('GAD-7 Value') + 
      geom_point(
        data = gad7_dat_mean_X,
        aes(x = Visit_Num, y = GAD_7_value, group = NULL, color = 'red')
      ) + facet_grid(~get(cov_name)) +
      geom_line(
        data = gad7_dat_mean_X,
        aes(x = Visit_Num, y = GAD_7_value, group = NULL, color = 'red')
      ) + facet_grid(~get(cov_name)) +
      theme(
        plot.title = element_text(hjust = 0.5, size = 15),
        panel.background = element_rect(
          fill   = "white",
          colour = "black",
          linewidth = 0.5, linetype = "solid"
        ),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        legend.title = element_blank(),
        plot.margin = margin(.2, .2, .2, .2, 'cm')
      )
    # Adjust facet type for Race
    if (cov_name == 'Race') {
      p_spaghetti_gad7_visit_id = p_spaghetti_gad7_visit_id +
        facet_wrap(~get(cov_name), nrow = 2)
    } else {
      p_spaghetti_gad7_visit_id = p_spaghetti_gad7_visit_id +
        facet_grid(~get(cov_name))
    }
  }
  
  print(p_spaghetti_gad7_visit_id)
  return(p_spaghetti_gad7_visit_id)
}



#-----------------
# Function: Produce LMM fitted plot by covariate
#-----------------
produce_lmm_fitted_plot = function(gad7_lmm_df, cov_name, visit_num_max = 20) {
  # Determine maximum visits
  visit_num_max_built_in = diff(range(gad7_lmm_df$Visit_Num)) + 1
  visit_num_max = ifelse(
    visit_num_max < visit_num_max_built_in,
    visit_num_max,
    visit_num_max_built_in
  )
  seq_x = seq(1, visit_num_max, length.out = visit_num_max)
  
  p_gad7_lmm_X_aes = aes(Visit_Num, GAD_7_value, group = get(cov_name))
  
  
  #- - - - - - -
  # Create fitted LMM plot with mean ± SE and fitted line
  #- - - - - - -
  p_gad7_lmm_X = ggplot(gad7_lmm_df, p_gad7_lmm_X_aes) +
    stat_summary(fun.data = mean_se, geom = "pointrange", linetype = "dashed") +
    stat_summary(aes(y = .fitted), fun.y = mean, geom = "line") +
    scale_x_continuous(breaks = seq_x, labels = seq_x) +
    xlab('Visit Number') + ylab('') +
    ggtitle('GAD-7 Value') +
    theme(
      plot.title = element_text(hjust = 0.5, size = 15),
      panel.background = element_rect(
        fill   = "white",
        colour = "black",
        linewidth = 0.5, linetype = "solid"
      ),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = 'bottom',
      plot.margin = margin(.2, .2, .2, .2, 'cm')
    )
  # Facet by covariate
  p_gad7_lmm_X = p_gad7_lmm_X + facet_wrap(~get(cov_name), nrow = 1)
  
  print(p_gad7_lmm_X)
  return(p_gad7_lmm_X)
}


#-----------------
# Function: Single department category trend (black lines)
#-----------------
compute_mean_trend_wrt_visit_num_single_department_category = function(
    gad7_dat_final, MRN_department_only, MRN_department_name, visit_max_num = 15
) {
  # Filter data for specified MRNs
  gad7_dat_final_department = gad7_dat_final[
    gad7_dat_final$MRN %in% MRN_department_only, ]
  
  # Compute mean GAD-7 by Visit_Num for department
  gad7_dat_final_department_mean = aggregate(
    gad7_dat_final_department$GAD_7_value,
    list(Visit_Num = gad7_dat_final_department$Visit_Num),
    mean, na.rm = T
  )
  colnames(gad7_dat_final_department_mean)[2] = 'GAD_7_value'
  
  #- - - - - - -
  # Build spaghetti plot for dept. category
  #- - - - - - -
  p_spaghetti_gad7_visit_id_department_only = 
    ggplot(
      data = gad7_dat_final_department,
      aes(x = Visit_Num, y = GAD_7_value, group = factor(MRN))
    ) +
    geom_line(alpha = 0.25) +
    scale_x_continuous(
      breaks = 0:visit_max_num,
      labels = 0:visit_max_num,
      limits = c(0, visit_max_num)
    ) +
    xlab('Visit Number') + ylab('') +
    ggtitle(MRN_department_name) +
    geom_point(
      data = gad7_dat_final_department_mean,
      aes(x = Visit_Num, y = GAD_7_value, group = NULL, color = 'red')
    ) +
    geom_line(
      data = gad7_dat_final_department_mean,
      aes(x = Visit_Num, y = GAD_7_value, group = NULL, color = 'red')
    ) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 10),
      panel.background = element_rect(
        fill   = "white",
        colour = "black",
        linewidth = 0.5, linetype = "solid"
      ),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = c(10, 10),
      legend.title = element_blank(),
      plot.margin = margin(.2, .2, .2, .2, 'cm')
    )
  
  return(p_spaghetti_gad7_visit_id_department_only)
}


#-----------------
# Function: Hybrid department category trend (colored lines)
#-----------------
compute_mean_trend_wrt_visit_num_hybrid_department_category = function(
    gad7_dat_final, MRN_department_hybrid, MRN_department_hybrid_name, visit_max_num = 15
) {
  # Subset or use all data if MRNs not specified
  if (is.null(MRN_department_hybrid)) {
    gad7_dat_final_department = gad7_dat_final
    MRN_department_hybrid_name = 'FAMILY MED, PRIMARY CARE, or PSYCH'
  } else {
    gad7_dat_final_department = gad7_dat_final[
      gad7_dat_final$MRN %in% MRN_department_hybrid, ]
  }
  # Ensure visit_max_num covers actual data
  visit_max_num_internal = max(table(gad7_dat_final_department$MRN))
  if (visit_max_num < visit_max_num_internal) {
    visit_max_num = visit_max_num_internal
  }
  # Compute mean GAD-7 by Visit_Num
  gad7_dat_final_department_mean = aggregate(
    gad7_dat_final_department$GAD_7_value,
    list(Visit_Num = gad7_dat_final_department$Visit_Num),
    mean, na.rm = T)
  colnames(gad7_dat_final_department_mean)[2] = 'GAD_7_value'
  
  
  
  #- - - - - - -
  # Build spaghetti plot by Department_Category
  #- - - - - - -
  p_spaghetti_gad7_visit_id_department_hybrid = 
    ggplot(
      data = gad7_dat_final_department,
      aes(
        x = Visit_Num,
        y = GAD_7_value,
        group = factor(MRN),
        color = Department_Category
      )) +
    geom_line() +
    geom_point() +
    scale_x_continuous(
      breaks = 0:visit_max_num,
      labels = 0:visit_max_num,
      limits = c(0, visit_max_num)
    ) +
    scale_color_manual(
      values = c('FAMILY MED' = 'red', 'PRIMARY CARE' = 'orange', 'PSYCH' = 'blue')) +
    xlab('Visit Number') + ylab('') +
    ggtitle(MRN_department_hybrid_name) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 10),
      panel.background = element_rect(
        fill   = "white",
        colour = "black",
        linewidth = 0.5,
        linetype = "solid"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = 'bottom',
      legend.title    = element_blank(),
      plot.margin     = margin(.2, .2, .2, .2, 'cm')
    )
  
  return(p_spaghetti_gad7_visit_id_department_hybrid)
}


#-----------------
# Function: Merge and save multiple dept. category plots
#-----------------
compute_mean_trend_wrt_visit_num_department_category_merge = function(
    gad7_dat_final_2, MRN_department_list, MRN_department_list_name, visit_max_num, parent_p_dir
) {
  
  MRN_department_list_size = length(MRN_department_list_name)
  p_spaghetti_gad7_visit_id_department_black = lapply(1:MRN_department_list_size, function(x) NULL)
  p_spaghetti_gad7_visit_id_department_color = lapply(1:MRN_department_list_size, function(x) NULL)
  
  for (p_id in 1:MRN_department_list_size) {
    p_spaghetti_gad7_visit_id_department_black[[p_id]] = compute_mean_trend_wrt_visit_num_single_department_category(
      gad7_dat_final_2, MRN_department_list[[p_id]], MRN_department_list_name[p_id], visit_max_num=visit_max_num
    )
    p_spaghetti_gad7_visit_id_department_color[[p_id]] = compute_mean_trend_wrt_visit_num_hybrid_department_category(
      gad7_dat_final_2, MRN_department_list[[p_id]], MRN_department_list_name[p_id], visit_max_num=visit_max_num
    )
  }
  
  p_spaghetti_gad7_visit_id_department_black_4 = marrangeGrob(p_spaghetti_gad7_visit_id_department_black, nrow=1, ncol=MRN_department_list_size, top='')
  p_spaghetti_gad7_visit_id_department_black_4_dir = file.path(
    parent_p_dir, paste('p_spaghetti_gad7_visit_id_multiple_', MRN_department_list_name[1], '_4_black.png', sep='')
  )
  ggsave(p_spaghetti_gad7_visit_id_department_black_4_dir, p_spaghetti_gad7_visit_id_department_black_4,
         units='mm', width=500, height=200, dpi=300)
  
  p_spaghetti_gad7_visit_id_department_color_4 = marrangeGrob(p_spaghetti_gad7_visit_id_department_color, nrow=1, ncol=MRN_department_list_size, top='')
  p_spaghetti_gad7_visit_id_department_color_4_dir = file.path(
    parent_p_dir, paste('p_spaghetti_gad7_visit_id_multiple_', MRN_department_list_name[1], '_4_color.png', sep='')
  )
  ggsave(p_spaghetti_gad7_visit_id_department_color_4_dir, p_spaghetti_gad7_visit_id_department_color_4,
         units='mm', width=500, height=200, dpi=300)
  
}
