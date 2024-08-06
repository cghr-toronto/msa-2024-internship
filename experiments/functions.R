# Creating PDF export parameters
pdf_print <- function(series, title, width, height){
    
    pdf_output_dir <- "../figures/"
    
    jpeg_output_dir <- "../figures/jpeg/"
    
    pdf_title <- paste0(pdf_output_dir, title, ".pdf")
    
    jpeg_title <- paste0(jpeg_output_dir, title, ".jpeg")
    
    ggsave(pdf_title, plot = series, device = "pdf", width = width, height = height)
    
    ggsave(jpeg_title, plot = series, device = "jpeg", width = width, height = height)
    
}

# Creating non-spatial table of symptom and causes of death
non_spatial <- function(age_group, death_type){
    
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
    ns <- ns %>% left_join(death_count, by = death_type)
    colnames(ns)[colnames(ns) == death_type] <- "cause_of_death"
    
    return(ns)
}

# Creating heat map with non-spatial table
hm <- function(ns_table, hm_title, pdf_title, labels = TRUE, desc_order = TRUE) {
    
    death_total <- as.numeric(sum(ns_table$deaths)) 
    
    heat <- pivot_longer(ns_table, cols = -c(cause_of_death, deaths),
                         names_to = "symptoms",
                         values_to = "counts") %>% 
        filter(cause_of_death != "NA" & symptoms != "NA") %>% 
        group_by(cause_of_death, symptoms) %>%
        summarise(total_count = sum(counts))%>%
        left_join(ns_table %>% select(cause_of_death, deaths) %>% distinct(), by = "cause_of_death")
    
    # Calculating sum of each symptom death
    symp_sums <- heat %>%
        group_by(symptoms) %>%
        summarize(symp_sum = sum(total_count, na.rm = TRUE))
    
    # Merge the sums back into the original data frame
    heat <- heat %>%
        left_join(symp_sums, by = "symptoms")
    
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
    
    # Join geometry to new spatial table
    spatial <- result %>%
        left_join(age_sex_agg %>% select(gid, geometry, deaths, distname), by = "gid")
    
    # Create rate columns for malaria symptoms
    for (symptom in symptoms) {
        rate_column <- paste0(symptom, "_", cod, "_rate")
        spatial[[rate_column]] <- (spatial[[symptom]] / spatial[[deaths]]) * 100
        spatial[[rate_column]] <- round(spatial[[rate_column]], 2)
    }
    
    # Print the wide format
    cat("\nWide format:\n")
    print(spatial)
    
    # Convert spatial to an sf and reproject crs
    spatial <- spatial %>% st_as_sf(sf_column_name = "geometry") %>% st_transform(32628)
    
    out <- spatial %>% 
        pivot_longer(cols = ends_with("rate"),
                     names_to = "symptoms", 
                     values_to = "rates") %>%
        select(gid, symptoms, rates)
    
    return(out)
}


# Creating mappping parameters
create_map <- function(data, symptom, y_axis, labels = TRUE, gplot_title = TRUE, first_map) {
    filtered_data <- data %>% filter(symptoms == symptom)
    
    min_val <- min(filtered_data$rates, na.rm = TRUE)
    max_val <- max(filtered_data$rates, na.rm = TRUE)
    
    breaks <- 6
    
    # Calculate the interval width
    interval_width <- max_val / breaks
    
    # Generate the sequence of break points
    break_points <- seq(min_val, max_val, len = 6)
    
    filtered_data <- filtered_data %>% mutate(label = glue("{get(symptom)}/{deaths}"))
    
    map <- ggplot(data = filtered_data) +
        geom_sf(aes(fill=(rates))) +
        guides(fill = guide_legend()) +
        ggtitle(paste(symptom)) +
        theme_minimal() + 
        theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              legend.title = element_blank(),
              axis.text = element_blank(), 
              axis.ticks = element_blank(),
              axis.title.x = element_blank(),
              axis.title.y = if (symptom == first_map) element_text(angle = 0, vjust = 0.5, size = 20) else element_blank(),
              plot.title = if (gplot_title) element_text(hjust = 0.5, size = 20) else element_blank()) +
        ylab(if (symptom == first_map) y_axis else NULL) +
        scale_fill_continuous(low="lightblue", 
                              high="darkblue", 
                              breaks = break_points, 
                              labels = scales::number_format(accuracy = 1))
    
    # Conditionally add labels
    if (labels) {
        map <- map + geom_sf_label(aes(label = label), size = 1.8)
    }
    
    # Conditionally add the title
    if (gplot_title) {
        map <- map + ggtitle(paste(symptom))
    }
    
    return(map)
}

# Creating grouped plots parameters
create_plots <- function(group_symptoms, plot_title, pdf_title, label = TRUE) {
    
    malaria_spatial <- group_symptoms %>% filter(denom_group == "Malaria")
    infections_spatial <- group_symptoms %>% filter(denom_group == "Infections")
    non_infections_spatial <- group_symptoms %>% filter(denom_group == "Non-Infections")
    
    symptoms <- unique(group_symptoms$symptoms)
    
    fm <- symptoms[[1]]
    
    malaria_plots <- lapply(symptoms, create_map, data = malaria_spatial, y_axis = "Malaria\n(per 100\nMalaria deaths)", labels = label, gplot_title = TRUE, first_map = fm)
    infection_plots <- lapply(symptoms, create_map, data = infections_spatial, y_axis = "Infections\n(per 100\nInfection deaths)", labels = label, gplot_title = FALSE, first_map = fm)
    non_infection_plots <- lapply(symptoms, create_map, data = non_infections_spatial, y_axis = "Non-Infections\n(per 100\nNon-Infection deaths)", labels = label, gplot_title = FALSE, first_map = fm)
    
    all_plots <- c(malaria_plots, infection_plots, non_infection_plots)
    
    combined_plot <- wrap_plots(all_plots, ncol = length(malaria_plots)) + 
        plot_annotation(title = plot_title,
                        theme = theme(
                            plot.title = element_text(size = 20, face = "bold", hjust = 0.5)
                        ))
    
    out <- pdf_print(combined_plot, pdf_title, width = 26, height = 13)
    
    return(out)
}