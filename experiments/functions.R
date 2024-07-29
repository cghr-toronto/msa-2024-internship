# Creating PDF export parameters
pdf_print <- function(series, title, width, height){
    
    pdf_output_dir <- "../figures/"
    
    jpeg_output_dir <- "../figures/figures.jpgs/"
    
    pdf_title <- paste0(pdf_output_dir, title, ".pdf")
    
    jpeg_title <- paste0(jpeg_output_dir, title, ".jpeg")
    
    ggsave(pdf_title, plot = series, device = "pdf", width = width, height = height)
    
    ggsave(jpeg_title, plot = series, device = "jpeg", width = width, height = height)
    
}

# Creating non-spatial table of symptom and causes of death
non_spatial <- function(age_group){
    
    ns <- pivot_longer(age_group, cols = starts_with("symp"), # Matches columns starting with "symp" followed by dig
                       names_to = "symptom", # New column to store the symptom names
                       values_to = "value" # New column to store the counts
    ) %>% group_by(`WBD category`, value) %>%
        summarise(count = n(), .groups = 'drop') %>%
        arrange(`WBD category`, value) %>%
        pivot_wider(
            names_from = value,   # The values in the 'value' column will become column names
            values_from = count,  # The values in the 'count' column will fill the new columns
            values_fill = list(count = 0)  # Fill missing values with 0
        )
    
    # Creating count for deaths per cause in non-spatial
    death_count <- age_group %>% count(`WBD category`, sort = TRUE, name = "deaths")
    ns <- ns %>% left_join(death_count, by = "WBD category")
    colnames(ns)[colnames(ns) == "WBD category"] <- "cause_of_death"
    
    ns <- ns %>%
        mutate(is_malaria = ifelse(cause_of_death == "Malaria", 1, 0)) %>%
        arrange(is_malaria) %>%
        select(-is_malaria) %>%
        mutate(type_of_cause = case_when(
            cause_of_death == "Malaria" ~ "Malaria",
            cause_of_death %in% infections ~ "Infections",
            TRUE ~ "Non-infections"
        ))
    
    return(ns)
}

# Creating heat map with non-spatial table
hm <- function(ns_table, hm_title, pdf_title, labels = TRUE) {
    
    death_total <- as.numeric(sum(ns_table$deaths)) 
    
    malaria <- sum(ns_table$deaths[ns_table$type_of_cause == "Malaria"], na.rm = TRUE)
    infections <- sum(ns_table$deaths[ns_table$type_of_cause == "Infections"], na.rm = TRUE)
    non_infections <- sum(ns_table$deaths[ns_table$type_of_cause == "Non-infections"], na.rm = TRUE)
    
    heat <- pivot_longer(ns_table, cols = -c(cause_of_death, type_of_cause),
                         names_to = "symptoms",
                         values_to = "counts") %>%
        filter(cause_of_death != "NA" & symptoms != "NA" & symptoms != "deaths") %>% 
        group_by(type_of_cause, symptoms) %>%
        summarise(total_count = sum(counts))
    
    # Calculating sum of each cause of death
    toc_sums <- heat %>%
        group_by(type_of_cause) %>%
        summarize(toc_sum = sum(total_count, na.rm = TRUE))
    
    all_deaths <- sum(toc_sums$toc_sum)
    
    # Calculating sum of each symptom death
    symp_sums <- heat %>%
        group_by(symptoms) %>%
        summarize(symp_sum = sum(total_count, na.rm = TRUE))
    
    # Calculating percentage for symptoms
    symp_sums$symp_perc <- round((symp_sums$symp_sum / death_total) * 100)
    
    # Merge the sums back into the original data frame
    heat <- heat %>%
        left_join(toc_sums, by = "type_of_cause") %>%
        left_join(symp_sums, by = "symptoms")
    
    heat <- heat %>% 
        mutate(total_perc = case_when(
            type_of_cause == "Malaria" ~ round((total_count / malaria) * 100),
            type_of_cause == "Infections" ~ round((total_count / infections) * 100),
            type_of_cause == "Non-infections" ~ round((total_count / non_infections) * 100)
        )) %>%
        mutate(type_of_cause = case_when(
            type_of_cause == "Malaria" ~ glue("Malaria\n(n={malaria})"),
            type_of_cause == "Infections" ~ glue("Infections\n(n={infections})"),
            type_of_cause == "Non-infections" ~ glue("Non-infections\n(n={non_infections})")
        )) 
    
    
    if (labels) { heat <- heat %>% mutate(symptoms = ifelse(symp_perc < 1,
                                                            glue("{symp_sums$symptoms}\n({symp_sums$symp_sum}, <1%)"),
                                                            glue("{symp_sums$symptoms}\n({symp_sums$symp_sum}, {symp_sums$symp_perc}%)")
    )
    ) %>% heat$symptoms <- fct_reorder(heat$symptoms, heat$symp_sum, .desc = FALSE) } 
    else { heat$symptoms <- factor(heat$symptoms, levels = rev(sort(unique(heat$symptoms))))
    }
    
    heat$type_of_cause <- factor(heat$type_of_cause, levels = c(glue("Malaria\n(n={malaria})"),
                                                                glue("Infections\n(n={infections})"),
                                                                glue("Non-infections\n(n={non_infections})")))
    
    
    # Create the heatmap with modified axis labels
    heat_map_plot <- ggplot(heat, aes(type_of_cause, symptoms)) +
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
                                                                                glue("{total_count}\n({total_perc}%)"))
    ))  } else { heat_map_plot <- heat_map_plot + geom_text(aes(label = glue("{total_count}"))
    )} 
    
    # Exporting heat map as pdf
    out <- pdf_print(heat_map_plot, pdf_title, width = 6, height = 24)
    
    return(out)
    
}

cod_rate <- function(
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
        ) %>% select(all_of(symptoms)) 
    
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
        select(gid, symptoms, rates, deaths, all_of(symptoms))
    
    return(out)
}

# Function for creating rates for aggregated results
symptom_rate <- function(
        age_sex_malaria_agg,
        age_sex_infections_agg,
        age_sex_non_infections_agg,
        deaths,
        symptoms){
    
    malaria_rates <- cod_rate(age_sex_agg = age_sex_malaria_agg,
                              cod = "malaria",
                              symptoms = symptoms,
                              deaths = deaths)
    
    infection_rates <- cod_rate(age_sex_agg = age_sex_infections_agg,
                                cod = "infections",
                                symptoms = symptoms,
                                deaths = deaths)
    
    non_infection_rates <- cod_rate(age_sex_agg = age_sex_non_infections_agg,
                                    cod = "non_infections",
                                    symptoms = symptoms,
                                    deaths = deaths)
    
    comb_rows <- bind_rows(malaria_rates, infection_rates, non_infection_rates)
    
    # Pivoted spatial table to show rates for each symptom
    out <- comb_rows %>%  mutate(symptoms = str_remove(symptoms, "_rate$")) %>% 
        mutate(denom_group = case_when( 
            str_ends(symptoms, "_malaria") ~ "Malaria", 
            str_ends(symptoms, "_non_infections") ~ "Non-Infections",
            str_ends(symptoms, "_infections") ~ "Infections"
        )) %>%
        mutate(symptoms = str_remove(symptoms, "_malaria$|_non_infections$|_infections$")) 
    
    out$rates[is.nan(out$rates)] <- 0
    
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