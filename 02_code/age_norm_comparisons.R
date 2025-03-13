# =======================
# HELPER: Build final plots
# =======================
make_plots <- function(means_ns_sds_and_ses, labels, palette, ps_table) {
  
  # 1) Expand logic based on the max length of 'labels'
  maxLabelLen <- if (is.null(labels)) 1 else max(nchar(labels))
  myExpandRight <- maxLabelLen
  
  # 2) Factor levels => color scale
  my_levels <- levels(means_ns_sds_and_ses$source)
  if (length(palette) != length(my_levels)) {
    stop("Length of 'palette' must match the final number of factor levels.\n",
         "We have ", length(my_levels), " levels: ",
         paste(my_levels, collapse = ", "), "\n",
         "But you supplied ", length(palette), " color(s).")
  }
  color_scale <- ggplot2::scale_colour_manual(
    breaks = my_levels,
    values = palette
  )
  
  # 3) Common theme
  custom_theme <- ggplot2::theme(
    axis.text.x       = ggplot2::element_text(angle=45, hjust=1),
    panel.grid.major.y= ggplot2::element_blank(),
    panel.grid.minor.y= ggplot2::element_blank(),
    panel.grid.minor.x= ggplot2::element_blank(),
    panel.grid.major.x= ggplot2::element_line(linetype="dashed", linewidth=0.3),
    legend.position   = "none"
  )
  
  # We'll label lines at the max age in ps_table
  max_age_val <- max(ps_table$age, na.rm=TRUE)
  
  # 4) Means plot
  means_plot <- means_ns_sds_and_ses %>%
    ggplot2::ggplot(ggplot2::aes(x=age, y=mean, group=source, colour=source)) +
    ggplot2::scale_x_continuous(
      breaks = seq(min(ps_table$age), max(ps_table$age), by=2),
      expand = ggplot2::expansion(add=c(1,myExpandRight))
    ) +
    ggplot2::geom_line(linewidth=1) +
    ggplot2::geom_pointrange(
      shape=18, fatten=3, linewidth=1,
      ggplot2::aes(ymin=mean - 1.96*seOfmean, ymax=mean + 1.96*seOfmean),
      position=ggplot2::position_dodge(width=0.3)
    ) +
    ggrepel::geom_text_repel(
      data = dplyr::filter(means_ns_sds_and_ses, age==max_age_val),
      ggplot2::aes(label=source),
      family  = "Times",
      seed    = 810,
      nudge_x = 1,
      hjust   = 0,
      point.padding = 1,
      direction    = "y"
    ) +
    custom_theme + color_scale +
    ggplot2::labs(x="Age", y="Mean")
  
  # 5) SDs plot
  sds_plot <- means_ns_sds_and_ses %>%
    ggplot2::ggplot(ggplot2::aes(x=age, y=sd, group=source, colour=source)) +
    ggplot2::scale_x_continuous(
      breaks = seq(min(ps_table$age), max(ps_table$age), by=2),
      expand = ggplot2::expansion(add=c(1,myExpandRight))
    ) +
    ggplot2::geom_line(linewidth=1) +
    ggplot2::geom_pointrange(
      shape=18, fatten=3, linewidth=1,
      ggplot2::aes(ymin=sd - 1.96*seOfsd, ymax=sd + 1.96*seOfsd),
      position=ggplot2::position_dodge(width=0.3)
    ) +
    ggrepel::geom_text_repel(
      data = dplyr::filter(means_ns_sds_and_ses, age==max_age_val),
      ggplot2::aes(label=source),
      family  = "Times",
      seed    = 810,
      nudge_x = 1,
      hjust   = 0,
      point.padding = 1,
      direction    = "y"
    ) +
    custom_theme + color_scale +
    ggplot2::labs(x="Age", y="SD")
  
  # 6) SE of Mean plot
  ses_plot <- means_ns_sds_and_ses %>%
    ggplot2::ggplot(ggplot2::aes(x=age, y=seOfmean, group=source, colour=source)) +
    ggplot2::scale_x_continuous(
      breaks = seq(min(ps_table$age), max(ps_table$age), by=2),
      expand = ggplot2::expansion(add=c(1,myExpandRight))
    ) +
    ggplot2::geom_line(linewidth=1) +
    ggplot2::geom_point(shape=18, size=3) +
    ggrepel::geom_text_repel(
      data = dplyr::filter(means_ns_sds_and_ses, age==max_age_val),
      ggplot2::aes(label=source),
      family  = "Times",
      seed    = 810,
      nudge_x = 1,
      hjust   = 0,
      point.padding = 1
    ) +
    custom_theme + color_scale +
    ggplot2::labs(x="Age", y="SE of Mean")
  
  list(means_plot = means_plot, SDs_plot = sds_plot, SEs_plot = ses_plot)
}


# =======================
# MAIN FUNCTION
# =======================
age_norm_comparisons <- function(...,
                                 ps_table       = census,  
                                 ps_variables   = c("age", "educ", "mig", "male"),
                                 re_formula     = NULL,
                                 sim_size       = 100000,
                                 RP             = NULL,         # "census","norming_sample"
                                 labels         = NULL,
                                 palette        = c("#BC3C29FF", "#0072B5FF"),
                                 output_file    = "../02_data/results.rds",
                                 prediction_transform = NULL  # function or list of functions or NULL
) {
  
  #--------------------------------------------------
  # 1) If output_file exists => load => skip new simulation
  #--------------------------------------------------
  if (file.exists(output_file)) {
    message("File '", output_file, "' already exists. Loading from RDS...")
    existing_data <- readRDS(output_file)
    
    means_ns_sds_and_ses <- existing_data$means_ns_sds_and_ses
    overall_estimates    <- existing_data$overall_estimates
    
    # Re-build final plots
    plots <- make_plots(means_ns_sds_and_ses, labels, palette, ps_table)
    
    final_list <- list(
      means_ns_sds_and_ses = means_ns_sds_and_ses,
      overall_estimates    = overall_estimates,
      means_plot           = plots$means_plot,
      SDs_plot             = plots$SDs_plot,
      SEs_plot             = plots$SEs_plot
    )
    
    rm(existing_data, plots)
    gc(verbose=FALSE)
    return(final_list)
  }
  
  # If we get here => no existing RDS => proceed with new simulation
  
  #--------------------------------------------------
  # 2) If RP is NULL => default "census"
  #--------------------------------------------------
  if (is.null(RP)) {
    RP <- "census"
  }
  if (is.character(RP) && length(RP)==1) {
    RP <- c(RP)
  }
  
  #--------------------------------------------------
  # 3) Capture brms models from ...
  #--------------------------------------------------
  brms_models    <- list(...)
  raw_call_names <- sapply(substitute(list(...))[-1], deparse)
  if (!all(sapply(brms_models, inherits, "brmsfit"))) {
    stop("All '...' must be brmsfit models.")
  }
  nModels <- length(brms_models)
  
  # If user gave single function => apply to all models
  # If user gave list => must match #models
  # If NULL => no transform
  if (!is.null(prediction_transform)) {
    if (is.function(prediction_transform)) {
      prediction_transform <- replicate(nModels, prediction_transform, simplify=FALSE)
    } else if (is.list(prediction_transform)) {
      if (length(prediction_transform)!=nModels) {
        stop("'prediction_transform' must match the # of brms models if it's a list.")
      }
    } else {
      stop("'prediction_transform' must be NULL or a function or list of functions.")
    }
  } else {
    prediction_transform <- replicate(nModels, NULL, simplify=FALSE)
  }
  
  #--------------------------------------------------
  # 4) Summarize the "raw" lines from the FIRST brms model => always as.numeric(as.character())
  #--------------------------------------------------
  first_mod_data <- brms_models[[1]]$data
  out_name <- all.vars(brms_models[[1]]$formula$formula)[1]
  if (is.na(out_name)) {
    stop("Could not detect outcome from first brms model (multi-param?).")
  }
  
  # Always convert to numeric(as.character(...))
  first_mod_data[[ out_name ]] <- as.numeric(as.character(first_mod_data[[ out_name ]]))
  
  # Summaries
  means_sds_and_ses_raw <- first_mod_data %>%
    dplyr::mutate(age = floor(age)) %>%
    dplyr::group_by(age) %>%
    dplyr::summarise(
      Raw_n    = dplyr::n(),
      Raw_mean = mean(.data[[ out_name ]], na.rm=TRUE),
      Raw_sd   = sd(.data[[ out_name ]], na.rm=TRUE),
      Raw_seOfmean = Raw_sd / sqrt(Raw_n),
      .groups="drop"
    )
  
  age_level_list  <- list()
  row_level_draws <- list()
  
  #--------------------------------------------------
  # 5) Loop over each approach in RP
  #--------------------------------------------------
  for (this_approach in RP) {
    prefix <- if (this_approach=="census") "RPP_" else "RP_"
    
    # a) Sample from ps_table or from norming sample
    if (this_approach=="census") {
      sim_data <- ps_table %>%
        dplyr::filter(census_n != 0) %>%
        dplyr::ungroup() %>%
        dplyr::sample_n(
          size   = sim_size,
          weight = census_n,
          replace=TRUE
        ) %>%
        dplyr::select(dplyr::all_of(ps_variables))
    } else if (this_approach=="norming_sample") {
      sim_data <- first_mod_data %>%
        dplyr::select(dplyr::all_of(ps_variables)) %>%
        dplyr::mutate(age = floor(age)) %>%
        dplyr::group_by(dplyr::across(dplyr::everything())) %>%
        dplyr::summarise(Raw_n = dplyr::n(), .groups="drop") %>%
        dplyr::sample_n(
          size   = sim_size,
          weight = Raw_n,
          replace=TRUE
        ) %>%
        dplyr::select(dplyr::all_of(ps_variables))
    } else {
      stop("RP must be 'census' or 'norming_sample'.")
    }
    
    approach_age_wide_list <- list()
    row_level_draws[[ this_approach ]] <- list()
    
    # b) For each brms model => predicted draws => apply transform => summarize
    for (i in seq_along(brms_models)) {
      model      <- brms_models[[i]]
      model_name <- raw_call_names[i]
      short_name <- sub("^brm_", "", model_name)
      full_name  <- paste0(prefix,"brm_",short_name)
      
      sim_data_with_draws <- sim_data %>%
        tidybayes::add_predicted_draws(
          model,
          ndraws           = 1000,
          seed             = 810,
          re_formula       = re_formula,
          allow_new_levels = TRUE
        )
      
      # c) if user provided a transform => apply
      fun_pred <- prediction_transform[[ i ]]
      if (!is.null(fun_pred)) {
        sim_data_with_draws$.prediction <- fun_pred(sim_data_with_draws$.prediction)
      }
      
      # Summarize by age => wide
      summary_df <- sim_data_with_draws %>%
        dplyr::group_by(age, .draw) %>%
        dplyr::summarise(
          mean_prediction = mean(.prediction),
          sd_prediction   = sd(.prediction),
          .groups         = "drop"
        ) %>%
        dplyr::group_by(age) %>%
        dplyr::summarise(
          !!paste0(full_name,"_mean")     := mean(mean_prediction),
          !!paste0(full_name,"_seOfmean") := stats::sd(mean_prediction),
          !!paste0(full_name,"_sd")       := sqrt(mean(sd_prediction^2)),
          !!paste0(full_name,"_seOfsd")   := stats::sd(sd_prediction),
          .groups="drop"
        )
      
      approach_age_wide_list[[ model_name ]] <- summary_df
      row_level_draws[[ this_approach ]][[ model_name ]] <- sim_data_with_draws
    }
    
    # combine wide for all models in this approach
    approach_age_wide <- purrr::reduce(approach_age_wide_list, dplyr::left_join, by="age")
    age_level_list[[ this_approach ]] <- approach_age_wide
  }
  
  #--------------------------------------------------
  # 6) Combine age-level results
  #--------------------------------------------------
  if (length(age_level_list)==1) {
    combined_results <- age_level_list[[1]]
  } else {
    combined_results <- purrr::reduce(age_level_list, dplyr::left_join, by="age")
  }
  
  # Merge raw columns
  combined_results <- combined_results %>%
    dplyr::left_join(means_sds_and_ses_raw, by="age")
  
  #--------------------------------------------------
  # 7) Pivot to long
  #--------------------------------------------------
  means_ns_sds_and_ses <- combined_results %>%
    tidyr::pivot_longer(
      -age,
      names_to     = c("source",".value"),
      names_pattern= "(.*)_(.*)"
    )
  
  # reorder factor levels => "Raw", "RPP_brm_...", "RP_brm_..."
  final_levels <- "Raw"
  for (i in seq_along(brms_models)) {
    model_name <- raw_call_names[i]
    short_name <- sub("^brm_","",model_name)
    if ("census" %in% RP) {
      final_levels <- c(final_levels, paste0("RPP_brm_", short_name))
    }
    if ("norming_sample" %in% RP) {
      final_levels <- c(final_levels, paste0("RP_brm_", short_name))
    }
  }
  existing_sources <- unique(means_ns_sds_and_ses$source)
  level_order     <- final_levels[final_levels %in% existing_sources]
  leftover        <- setdiff(existing_sources, level_order)
  if (length(leftover)>0) {
    level_order <- c(level_order, leftover)
  }
  means_ns_sds_and_ses <- means_ns_sds_and_ses %>%
    dplyr::mutate(source = factor(source, levels=level_order))
  
  # If labels => apply them
  if (!is.null(labels)) {
    if (length(labels) != length(level_order)) {
      stop("Length of 'labels' must match final # of sources.\n",
           "We have ", length(level_order)," sources in order: ",
           paste(level_order, collapse=", "),"\n",
           "But you supplied ", length(labels), " labels.")
    }
    means_ns_sds_and_ses$source <- forcats::fct_relabel(
      means_ns_sds_and_ses$source,
      function(x) labels[match(x, level_order)]
    )
  }
  
  #--------------------------------------------------
  # 8) overall_estimates
  #--------------------------------------------------
  overall_estimates <- list()
  for (this_approach in names(row_level_draws)) {
    prefix <- if (this_approach=="census") "RPP_" else "RP_"
    
    for (i in seq_along(brms_models)) {
      model_name <- raw_call_names[i]
      short_name <- sub("^brm_","",model_name)
      full_label <- paste0(prefix, "brm_", short_name)
      
      sim_data_with_draws <- row_level_draws[[ this_approach ]][[ model_name ]]
      
      sum_df <- sim_data_with_draws %>%
        dplyr::group_by(.draw) %>%
        dplyr::summarise(
          mean_prediction=mean(.prediction),
          sd_prediction  =sd(.prediction),
          .groups="drop"
        ) %>%
        dplyr::summarise(
          Mean       = mean(mean_prediction),
          SE_of_Mean = stats::sd(mean_prediction),
          SD         = sqrt(mean(sd_prediction^2)),
          SE_of_SD   = stats::sd(sd_prediction),
          .groups    = "drop"
        ) %>%
        dplyr::mutate(Model=full_label)
      
      overall_estimates[[ length(overall_estimates)+1 ]] <- sum_df
    }
  }
  overall_estimates <- dplyr::bind_rows(overall_estimates)
  
  #--------------------------------------------------
  # 9) Save only 2 data frames => small RDS
  #--------------------------------------------------
  final_data <- list(
    means_ns_sds_and_ses = means_ns_sds_and_ses,
    overall_estimates    = overall_estimates
  )
  saveRDS(final_data, file=output_file, compress="xz")
  
  #--------------------------------------------------
  # 10) Build final plots in memory
  #--------------------------------------------------
  plots <- make_plots(means_ns_sds_and_ses, labels, palette, ps_table)
  
  #--------------------------------------------------
  # 11) Remove large objects, run gc
  #--------------------------------------------------
  rm(row_level_draws, leftover, final_levels, first_mod_data, brms_models, ps_table)
  gc(verbose=FALSE)
  
  list(
    means_ns_sds_and_ses = means_ns_sds_and_ses,
    overall_estimates    = overall_estimates,
    means_plot           = plots$means_plot,
    SDs_plot             = plots$SDs_plot,
    SEs_plot             = plots$SEs_plot
  )
}
