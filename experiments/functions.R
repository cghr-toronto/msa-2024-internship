# Creating PDF export parameters
pdf_print <- function(series, title, width, height){
    
    pdf_output_dir <- "../figures/"
    
    jpeg_output_dir <- "../figures/jpeg/"
    
    pdf_title <- paste0(pdf_output_dir, title, ".pdf")
    
    jpeg_title <- paste0(jpeg_output_dir, title, ".jpeg")
    
    ggsave(pdf_title, plot = series, device = "pdf", width = width, height = height, limitsize = FALSE)
    
    ggsave(jpeg_title, plot = series, device = "jpeg", width = width, height = height, limitsize = FALSE)
    
}

# Creating non-spatial table of symptom and causes of death
non_spatial <- function(age_group, death_type, percentages = TRUE){
    
    ns <- pivot_longer(age_group, cols = starts_with("symp"), # Matches columns starting with "symp" followed by dig
                       names_to = "symptom", # New column to store the symptom names
                       values_to = "value" # New column to store the counts
    ) %>% group_by(!!sym(death_type), value) %>%
        summarise(count = n(), .groups = 'drop') %>%
        arrange(!!sym(death_type), value) %>%
        pivot_wider(
            names_from = value,   # The values in the 'value' column will become column names
            values_from = count,  # The values in the 'count' column will fill the new columns
            values_fill = list(count = 0)  # Fill missing values with 0
        )

    # Creating count for deaths per cause in non-spatial
    death_count <- age_group %>% count(!!sym(death_type), sort = TRUE, name = "deaths")
    ns <- ns %>% left_join(death_count, by = death_type) %>% filter(death_type != "NA")
    colnames(ns)[colnames(ns) == death_type] <- "cause_of_death"
    
    if ("pregnant" %in% names(ns)) {
        ns <- ns %>% select(-all_of(c("pregnant", "injury", "NA"))) %>% filter(cause_of_death != "NA")
    } else {
        ns <- ns %>% select(-all_of(c("injury", "NA"))) %>% filter(cause_of_death != "NA")
    }
    
    if (percentages) {
    
    exclude_columns <- c("cause_of_death", "deaths")
    
    ns <- ns %>% mutate(across(-all_of(exclude_columns), ~ sprintf("%d (%.2f%%)", .x, (.x / deaths) * 100))) 
    }
    
    return(ns)
}

# Creating heat map with non-spatial table
hm <- function(ns_table, hm_title, pdf_title, labels = TRUE, desc_order = TRUE) {
    
    death_total <- as.numeric(sum(ns_table$deaths)) 
    
    heat <- pivot_longer(ns_table, cols = -c(cause_of_death, deaths),
                         names_to = "symptoms",
                         values_to = "counts") %>% 
        group_by(cause_of_death, symptoms) %>%
        summarise(total_count = sum(counts)) %>%
        left_join(ns_table %>% select(cause_of_death, deaths) %>% distinct(), by = "cause_of_death")
    
    heat <- heat %>% 
        mutate(total_perc = round((total_count / deaths) * 100)) %>%
        mutate(cause_of_death = glue("{cause_of_death}\n(n={deaths})")) 
    
    heat$symptoms <- factor(heat$symptoms, levels = rev(sort(unique(heat$symptoms))))
    
    if (desc_order) {
        heat$cause_of_death <- fct_reorder(heat$cause_of_death, heat$deaths, .desc = TRUE)
    } else {
        heat$cause_of_death <- fct_reorder(heat$cause_of_death, heat$deaths, .desc = FALSE)
    }
    
    # Create the heatmap with modified axis labels
    heat_map_plot <- ggplot(heat, aes(cause_of_death, symptoms)) +
        geom_tile(aes(fill = total_count)) +
        scale_fill_gradient(low = "white", high = "red", name = "Number\nof deaths",) +
        scale_x_discrete(position = "top") +
        ggtitle(glue("{hm_title}\n(n = {death_total})")) +
        theme(axis.text.x = element_text(size = 10.5),
              axis.text.y = element_text(size = 10.5),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.ticks = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              legend.position = "right",
              plot.title = element_text(hjust = 0.5, face = "bold", size = 14)) 
    
    if (labels) { heat_map_plot <- heat_map_plot + geom_text(aes(label = ifelse(total_perc < 1, 
                                                                                glue("{total_count}\n(<1%)"), 
                                                                                glue("{total_count}\n({total_perc}%)")
                                                                                )
                                                                 )
                                                             )  
    } else { heat_map_plot <- heat_map_plot + geom_text(aes(label = glue("{total_count}")
                                                            ))} 
    
    # Exporting heat map as pdf
    out <- pdf_print(heat_map_plot, pdf_title, width = 6, height = 24)
    
    return(out)
}

symptom_rate <- function(
        age_sex_agg,
        cod,
        symptoms,
        deaths){
    
    # Remove geometry from aggregated dataframe
    age_sex_without_geometry <- age_sex_agg  %>%
        as_tibble() %>%
        select(-geometry, -deaths, -distname)
    
    # Creating spatial symptom count
    result <- age_sex_without_geometry %>%
        pivot_longer( cols = matches("^symp\\d+_"), # Matches columns starting with "symp" followed by dig
                      names_to = "symptom", # New column to store the symptom names
                      values_to = "count" # New column to store the counts
        ) %>% mutate(symptom = gsub("^symp\\d+_|_count$","", symptom)) %>% # Remove prefix and suff
        group_by(gid, symptom) %>% # Group by gid and sympt
        summarize(total_count = sum(count) # Summarize the counts f
        ) %>% pivot_wider( names_from = symptom, # Pivot symptom column to wide format
                           values_from = total_count, # Values to be filled in the wide format
                           values_fill = 0 # Fill any missing values with 0
        )
    
    if (!all(symptoms %in% names(result))){
        
        missing_columns <- setdiff(symptoms, names(result))
        
        for (missing in missing_columns) {
            result <- result %>% mutate(!!missing := 0) 
        }
    }
    
    result <- result %>% select(all_of(symptoms))
    
    # Join geometry to new spatial table
    spatial <- result %>%
        left_join(age_sex_agg %>% select(gid, geometry, deaths, distname), by = "gid")
    
    # Create rate columns for malaria symptoms
    for (symptom in symptoms) {
        rate_column <- paste0(symptom, "_", "rates")
        spatial[[rate_column]] <- (spatial[[symptom]] / spatial[[deaths]]) * 100
        spatial[[rate_column]] <- round(spatial[[rate_column]])
    }
    
    # Print the wide format
    cat("\nWide format:\n")
    print(spatial)
    
    # Convert spatial to an sf and reproject crs
    spatial <- spatial %>% st_as_sf(sf_column_name = "geometry") %>% st_transform(32628)
    
    out <-
        spatial %>% rename_with( ~ paste0(., "_count"), .cols = all_of(symptoms)) %>%
        pivot_longer(
            cols = -c(gid, deaths, distname, geometry),
            names_to = c("symptoms", ".value"),
            names_pattern = "(.*)_(.*)"
        ) %>% select(gid, symptoms, rates, deaths, count, distname) %>%
        mutate(symptoms = paste0(symptoms, "_", cod))
    

    return(out)
}

create_map_landscape <-
    function(data,
             symptom,
             y_axis,
             labels = TRUE,
             gplot_title = TRUE,
             first_map,
             break_type,
             cod) {
        
        if (insufficient) {
            data <- data %>%
                mutate(data_quality = ifelse(deaths < 10, "Insufficient Data", "Sufficient Data"),
                       rates = ifelse(data_quality == "Insufficient Data", NA, rates))
        }
        
        if (break_type == "equal_breaks") {
            min_val <- min(data$rates, na.rm = TRUE)
            max_val <- max(data$rates, na.rm = TRUE)
            
            breaks <- 6
            
            # Calculate the interval width
            interval_width <- max_val / breaks
            
            # Generate the sequence of break points
            break_points <- seq(min_val, max_val, len = 6)
            
            label <- scales::number_format(accuracy = 1)
            
            limits <- c(min_val, max_val)
            
            filtered_data <- data %>% filter(symptoms == symptom & denom_group == cod)
            
            filtered_data <- filtered_data %>% mutate(fraction = glue("{count}/{deaths}"))
            
            map <- ggplot(data = filtered_data) +
                geom_sf(aes(fill=(rates))) +
                guides(fill = guide_legend()) +
                ggtitle(paste(symptom)) +
                theme_minimal() + 
                theme(panel.grid.major = element_blank(), 
                      panel.grid.minor = element_blank(),
                      axis.text = element_blank(), 
                      axis.ticks = element_blank(),
                      axis.title.x = element_blank(),
                      axis.title.y = if (symptom == first_map) element_text(angle = 0, vjust = 0.5, size = 20) else element_blank(),
                      plot.title = if (gplot_title) element_text(hjust = 0.5, size = 17) else element_blank()) +
                ylab(if (symptom == first_map) y_axis else NULL) +
                scale_fill_continuous(low="white", 
                                      high="darkblue", 
                                      breaks = break_points,
                                      labels = label,
                                      limits = limits) +
                guides(fill = guide_legend(nrow = 1, title = "Rates (%)"))
            
        } else if (break_type == "manual") {
            min_val <- min(data$rates, na.rm = TRUE)
            max_val <- max(data$rates, na.rm = TRUE)
            
            break_points <- c(10, 20, 40, 60, 80, 100)
            label <-
                c("Insufficient Data",
                  "0-10",
                  "10-20",
                  "20-30",
                  "30-40",
                  "40-50",
                  "50-60",
                  "60-70",
                  "70-80",
                  "80-90",
                  "90-100"
                )
            
            limits <- c(min_val, max_val)
            filtered_data <- data %>%
                filter(symptoms == symptom & denom_group == cod) %>%
                mutate(fraction = glue("{count}/{deaths}"))
            
            map <- ggplot(data = filtered_data) +
                geom_sf(aes(fill = rates), color = "gray50", size = 0.2) +
                scale_fill_gradientn(colors = c("white","darkgreen", "yellow", "darkred"),
                                     values = scales::rescale(c(0, 10, 55, 100)),
                                     na.value = "white",  # Handle NA values
                                     breaks = break_points,
                                     labels = label,
                                     limits = limits) +
                guides(fill = guide_legend(nrow = 1, title = "Rates (%)")) +
                ggtitle(gplot_title) +
                theme_minimal() + 
                theme(panel.grid.major = element_blank(), 
                      panel.grid.minor = element_blank(),
                      axis.text = element_blank(), 
                      axis.ticks = element_blank(),
                      axis.title.x = element_blank(),
                      axis.title.y = if (y_axis) element_text(angle = 0, vjust = 0.5, size = 20) else element_blank(),
                      plot.title = if (first_map == symptom) element_text(hjust = 0.5, size = 17) else element_blank()) +
                ylab(paste(symptom))
        }
  
    # Conditionally add labels
    if (labels) {
        map <- map + geom_sf_label(aes(label = fraction), size = 1.8)
    }
    
    # Conditionally add the title
    if (gplot_title) {
        map <- map + ggtitle(paste(symptom))
    }
    
    return(map)
}


# Creating mappping parameters
create_map_portrait <-
    function(data,
             symptom,
             y_axis = TRUE,
             labels = TRUE,
             first_map,
             gplot_title,
             break_type,
             cod,
             insufficient = TRUE) {
        
        if (insufficient) {
            data <- data %>%
                mutate(data_quality = ifelse(deaths < 10, "Insufficient Data", "Sufficient Data"),
                       rates = ifelse(data_quality == "Insufficient Data", NA, rates))
        }
        
        if (break_type == "equal_breaks") {
            min_val <- min(data$rates, na.rm = TRUE)
            max_val <- max(data$rates, na.rm = TRUE)
            
            breaks <- 6
            
            # Calculate the interval width
            interval_width <- max_val / breaks
            
            # Generate the sequence of break points
            break_points <- seq(min_val, max_val, len = 6)
            
            label <- scales::number_format(accuracy = 1)
            
            limits <- c(min_val, max_val)
            
            filtered_data <- data %>% filter(symptoms == symptom & denom_group == cod)
            
            filtered_data <- filtered_data %>% mutate(fraction = glue("{count}/{deaths}"))
            
            map <- ggplot(data = filtered_data) +
                geom_sf(aes(fill=(rates))) +
                guides(fill = guide_legend()) +
                ggtitle(paste(symptom)) +
                theme_minimal() + 
                theme(panel.grid.major = element_blank(), 
                      panel.grid.minor = element_blank(),
                      axis.text = element_blank(), 
                      axis.ticks = element_blank(),
                      axis.title.x = element_blank(),
                      axis.title.y = if (symptom == first_map) element_text(angle = 0, vjust = 0.5, size = 20) else element_blank(),
                      plot.title = if (gplot_title) element_text(hjust = 0.5, size = 17) else element_blank()) +
                ylab(if (symptom == first_map) y_axis else NULL) +
                scale_fill_continuous(low="white", 
                                      high="darkblue", 
                                      breaks = break_points,
                                      labels = label,
                                      limits = limits) +
                guides(fill = guide_legend(nrow = 1, title = "Rates (%)"))
            
        } else if (break_type == "manual") {
            
            filtered_data <- data %>%
                filter(symptoms == symptom & denom_group == cod) %>%
                mutate(fraction = glue("{count}/{deaths}"))
            
            filtered_data$rates <- as.factor(filtered_data$rates)
            
            map <- ggplot(data = filtered_data) +
                geom_sf(aes(fill = rates), color = "gray50", size = 0.2) +
                scale_fill_manual(values = c(
                    "Insufficient Data" = "white",
                    "0-10" = "lightgreen",
                    "10-20" = "green",
                    "20-30" = "darkgreen",
                    "30-40" = "yellow",
                    "40-50" = "gold",
                    "50-60" = "orange",
                    "60-70" = "red",
                    "70-80" = "darkred",
                    "80-90" = "purple",
                    "90-100" = "black"
                ),
                breaks = c(
                    "Insufficient Data", "0-10", "10-20", "20-30", "30-40",
                    "40-50", "50-60", "60-70", "70-80", "80-90", "90-100"
                ),
                labels = c(
                    "Insufficient Data", "0-10", "10-20", "20-30", "30-40",
                    "40-50", "50-60", "60-70", "70-80", "80-90", "90-100"
                )
                ) +
                guides(fill = guide_legend(nrow = 1, title = "Rates (%)")) +
                ggtitle(gplot_title) +
                theme_minimal() + 
                theme(panel.grid.major = element_blank(), 
                      panel.grid.minor = element_blank(),
                      axis.text = element_blank(), 
                      axis.ticks = element_blank(),
                      axis.title.x = element_blank(),
                      axis.title.y = if (y_axis) element_text(angle = 0, vjust = 0.5, size = 20) else element_blank(),
                      plot.title = if (first_map == symptom) element_text(hjust = 0.5, size = 17) else element_blank()) +
                ylab(paste(symptom)) 
        }
        
    # Conditionally add labels
    if (labels) {
        map <- map + geom_sf_label(aes(label = fraction), size = 1.8)
    }
    
    if (y_axis) {
        map <- map + ylab(paste(symptom))
    }
    
    return(map)
}

# Creating grouped plots parameters
create_plots <-
    function(group_symptoms,
             plot_title,
             pdf_title,
             label = TRUE,
             width,
             height,
             age_range,
             age_group,
             sex,
             orientation) {
        
    group_symptoms <- group_symptoms %>% filter(age_range == !!age_range & age_group == !!age_group & sex == !!sex)
    
    symptoms <- unique(group_symptoms$symptoms)
    
    fm <- symptoms[[1]]
    
    # Portrait layout    
    if (orientation == "portrait") {
        all_plots <- lapply(symptoms, function(symptom) {
            malaria_plots <- create_map_portrait(
                data = group_symptoms,
                symptom = symptom,
                y_axis = TRUE,
                labels = label,
                first_map = fm,
                gplot_title = "Cases\nper 100\nMalaria deaths",
                break_type = "manual",
                cod = "Malaria",
                insufficient = TRUE
            )
            
            infection_plots <- create_map_portrait(
                data = group_symptoms,
                symptom = symptom,
                y_axis = FALSE,
                labels = label,
                first_map = fm,
                gplot_title = "Cases\nper 100\nInfection deaths",
                break_type = "manual",
                cod = "Infections",
                insufficient = TRUE
            )
            
            non_infection_plots <- create_map_portrait(
                data = group_symptoms,
                symptom = symptom,
                y_axis = FALSE,
                labels = label,
                first_map = fm,
                gplot_title = "Cases\nper 100\nNon-Infection deaths",
                break_type = "manual",
                cod = "Non-Infections",
                insufficient = TRUE
            )
            
            list(malaria_plots, infection_plots, non_infection_plots)
            
        })
        
        # Flatten the list so that all plots are in a single list
        all_plots <- unlist(all_plots, recursive = FALSE)
        
        combined_plot <-
            guide_area() / wrap_plots(all_plots, ncol = 3) +
            plot_annotation(title = plot_title,
                            theme = theme(plot.title = element_text(
                                size = 20,
                                face = "bold",
                                hjust = 0.5
                            ))) +
            plot_layout(guides = "collect", heights = unit(c(1, 1.8), c("cm", "null"))) &
            theme(
                legend.position = 'top',
                legend.justification = c(0.5, 0),
                # Centers the legend horizontally
                legend.box.margin = margin(
                    t = 0,
                    r = 190,
                    b = 0,
                    l = 0
                )
            )
        
    # Landscape layout
    } else if (orientation == "landscape") {
        malaria_plots <-
            lapply(
                symptoms,
                create_map_landscape,
                data = group_symptoms,
                y_axis = "Cases\nper 100\nMalaria deaths",
                labels = label,
                gplot_title = TRUE,
                first_map = fm,
                break_type = "equal_breaks",
                cod = "Malaria"
            )
        
        infection_plots <-
            lapply(
                symptoms,
                create_map_landscape,
                data = group_symptoms,
                y_axis = "Cases\nper 100\nInfection deaths",
                labels = label,
                gplot_title = FALSE,
                first_map = fm,
                break_type = "equal_breaks",
                cod = "Infections"
            )
        
        non_infection_plots <-
            lapply(
                symptoms,
                create_map_landscape,
                data = group_symptoms,
                y_axis = "Cases\nper 100\nNon-Infection deaths",
                labels = label,
                gplot_title = FALSE,
                first_map = fm,
                break_type = "equal_breaks",
                cod = "Non-Infections"
            )
        
        all_plots <-
            c(malaria_plots, infection_plots, non_infection_plots)
        
        combined_plot <-
            guide_area() / wrap_plots(all_plots, ncol = length(malaria_plots)) +
            plot_annotation(title = plot_title,
                            theme = theme(plot.title = element_text(
                                size = 20,
                                face = "bold",
                                hjust = 0.5
                            ))) +
            plot_layout(guides = "collect", heights = unit(c(1, 1.8), c("cm", "null"))) &
            scale_colour_continuous(limits = range(c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100))) &
            theme(
                legend.position = 'top',
                legend.justification = c(0.5, 0),
                # Centers the legend horizontally
                legend.box.margin = margin(
                    t = 0,
                    r = 190,
                    b = 0,
                    l = 0))
        
    }
    
    out <- pdf_print(combined_plot, pdf_title, width = width, height = height)
    
    return(out)
}
