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
        spatial[[rate_column]] <- round(spatial[[rate_column]], 2)
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

# Creating mappping parameters
create_map <- function(data, symptom, y_axis = TRUE, labels = TRUE, first_map, gplot_title, break_type, cod) {
    
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
        
    } else if (break_type == "manual") {
        
        label <- names(break_points)
        
    } 
    
    filtered_data <- data %>% filter(symptoms == symptom & denom_group == cod)
    
    filtered_data <- filtered_data %>% mutate(fraction = glue("{count}/{deaths}"))
    
    map <- ggplot(data = filtered_data) +
        geom_sf(aes(fill=(rates))) +
        guides(fill = guide_legend()) +
        ggtitle(gplot_title) +
        theme_minimal() + 
        theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              axis.text = element_blank(), 
              axis.ticks = element_blank(),
              axis.title.x = element_blank(),
              axis.title.y = if (y_axis) element_text(angle = 0, vjust = 0.5, size = 20) else element_blank(),
              plot.title = if (first_map == symptom) element_text(hjust = 0.5, size = 17) else element_blank()) +
        ylab(paste(symptom)) +
        scale_fill_continuous(low="white", 
                              high="darkblue", 
                              breaks = break_points,
                              labels = label,
                              limits = limits) +
        guides(fill = guide_legend(nrow = 1, title = "Rates (%)"))
    
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
create_plots <- function(group_symptoms, plot_title, pdf_title, label = TRUE, width, height, age_range, age_group, sex) {
    
    group_symptoms <- group_symptoms %>% filter(age_range == !!age_range & age_group == !!age_group & sex == !!sex)
    
    symptoms <- unique(group_symptoms$symptoms)
    
    fm <- symptoms[[1]]
    
    all_plots <- lapply(symptoms, function(symptom) { 
    
    malaria_plots <- create_map(
            data = group_symptoms,
            symptom = symptom,
            y_axis = TRUE,
            labels = label,
            first_map = fm,
            gplot_title = "Cases\n(per 100\nMalaria deaths)",
            break_type = "equal_breaks",
            cod = "Malaria")
    
    infection_plots <- create_map(
            data = group_symptoms,
            symptom = symptom,
            y_axis = FALSE,
            labels = label,
            first_map = fm,
            gplot_title = "Cases\n(per 100\nInfection deaths)",
            break_type = "equal_breaks",
            cod = "Infections"
        )
    
    non_infection_plots <- create_map(
            data = group_symptoms,
            symptom = symptom,
            y_axis = FALSE,
            labels = label,
            first_map = fm,
            gplot_title = "Cases\n(per 100\nNon-Infection deaths)",
            break_type = "equal_breaks",
            cod = "Non-Infections"
        )
    
    list(malaria_plots, infection_plots, non_infection_plots)
    
    })
    
    # Flatten the list so that all plots are in a single list
    all_plots <- unlist(all_plots, recursive = FALSE)
    
    combined_plot <-
        guide_area() / wrap_plots(all_plots, ncol = 3) +
        plot_annotation(title = plot_title,
                        theme = theme(plot.title = element_text(
                            size = 20, face = "bold", hjust = 0.5))
                        ) +
        plot_layout(guides = "collect", heights = unit(c(1, 1.8), c("cm", "null"))) & 
        theme(legend.position = 'top',
              legend.justification = c(0.5, 0),  # Centers the legend horizontally
              legend.box.margin = margin(t = 0, r = 190, b = 0, l = 0)) 
    
    out <- pdf_print(combined_plot, pdf_title, width = width, height = height)
    
    return(out)
}
