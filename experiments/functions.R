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

# Creating mappping parameters
create_map <-
    function(data,
             symptom,
             cod,
             insufficient = TRUE) {
        
        if (insufficient) {
            data <- data %>%
                mutate(data_quality = ifelse(deaths < 10, "Insufficient Data", "Sufficient Data"),
                       rates = ifelse(data_quality == "Insufficient Data", NA, rates))
        }
            
            filtered_data <- data %>%
                filter(symptoms == symptom & denom_group == cod) %>%
                mutate(fraction = glue("{count}/{deaths}"))
            
            # Define a function to categorize values
            categorize_value <- function(value) {
                if (is.na(value)) {
                    return("Insufficient Data")
                } else if (value <= 10) {
                    return("0-10")
                } else if (value > 10 & value <= 20) {
                    return("10-20")
                } else if (value > 20 & value <= 30) {
                    return("20-30")
                } else if (value > 30 & value <= 40) {
                    return("30-40")
                } else if (value > 40 & value <= 50) {
                    return("40-50")
                } else if (value > 50 & value <= 60) {
                    return("50-60")
                } else if (value > 60 & value <= 70) {
                    return("60-70")
                } else if (value > 70 & value <= 80) {
                    return("70-80")
                } else if (value > 80 & value <= 90) {
                    return("80-90")
                } else if (value > 90 & value <= 100) {
                    return("90-100")
                } else {
                    return(NA)
                }
            }
            
            filtered_data$legend_label <- sapply(filtered_data$rates, categorize_value)
            
            filtered_data$legend_label <- factor(
                filtered_data$legend_label,
                levels = c("Insufficient Data", "0-10", "10-20", "20-30", "30-40", 
                           "40-50", "50-60", "60-70", "70-80", "80-90", "90-100")
            )
    
    return(filtered_data)
}

# Creating grouped plots parameters
create_plots <-
    function(group_symptoms,
             plot_title,
             pdf_title,
             width,
             height,
             age_range,
             age_group,
             sex,
             orientation) {
        
        symptom_sums <- group_symptoms %>%
            st_drop_geometry(group_symptoms) %>%
            filter(denom_group == "Malaria") %>%
            group_by(symptoms) %>%
            summarise(symp_freq = sum(count, na.rm = TRUE), .groups = 'drop')
        
        # Join the symptom sums back to the original data frame
        group_symptoms <- group_symptoms %>%
            left_join(symptom_sums, by = "symptoms")
        
    group_symptoms <- group_symptoms %>% filter(age_range == !!age_range & age_group == !!age_group & sex == !!sex)
    
    symptoms <- unique(group_symptoms$symptoms)
    
    one_symp <- symptoms[1]
    
    total_sum <- group_symptoms %>%
        filter(symptoms == one_symp) %>%
        summarise(total_deaths = sum(deaths, na.rm = TRUE)) %>%
        pull(total_deaths)
    
    mal_sum <- group_symptoms %>%
        filter(symptoms == one_symp & denom_group == "Malaria") %>%
        summarise(mal_deaths = sum(deaths, na.rm = TRUE)) %>%
        pull(mal_deaths)
    
    inf_sum <- group_symptoms %>%
        filter(symptoms == one_symp & denom_group == "Infections") %>%
        summarise(inf_deaths = sum(deaths, na.rm = TRUE)) %>%
        pull(inf_deaths)
    
    ninf_sum <- group_symptoms %>%
        filter(symptoms == one_symp & denom_group == "Non-Infections") %>%
        summarise(ninf_deaths = sum(deaths, na.rm = TRUE)) %>%
        pull(ninf_deaths)
    
    mal_perc <- round(mal_sum / total_sum * 100)
    inf_perc <- round(inf_sum / total_sum * 100)
    ninf_perc <- round(ninf_sum / total_sum * 100)
    
        all_data <- lapply(symptoms, function(symptom) {
            malaria_data <- create_map(
                data = group_symptoms,
                symptom = symptom,
                cod = "Malaria"
            )
            
            infection_data <- create_map(
                data = group_symptoms,
                symptom = symptom,
                cod = "Infections"
            )
            
            non_infection_data <- create_map(
                data = group_symptoms,
                symptom = symptom,
                cod = "Non-Infections"
            )
            
            rbind(malaria_data, infection_data, non_infection_data)
        })
        
        # Flatten the list to a single data frame
        all_data <- do.call(rbind, all_data)
        
        all_data <- all_data %>%
            mutate(denom_group = case_when(denom_group == "Malaria" ~ glue("Malaria\n(n={mal_sum}, {mal_perc}%)"),
                                           denom_group == "Infections" ~ glue("Infections\n(n={inf_sum}, {inf_perc}%)"),
                                           denom_group == "Non-Infections" ~ glue("Non-Infections\n(n={ninf_sum}, {ninf_perc}%)"),)) %>% 
            mutate(denom_group = factor(denom_group, levels = c(glue("Malaria\n(n={mal_sum}, {mal_perc}%)"),
                                                                glue("Infections\n(n={inf_sum}, {inf_perc}%)"),
                                                                glue("Non-Infections\n(n={ninf_sum}, {ninf_perc}%)"))
                                        )
                   )
       
        all_data$symptoms <- fct_reorder(all_data$symptoms, all_data$symp_freq, .desc = TRUE)
        
        combined_plot <- ggplot(all_data, aes(fill = legend_label)) +
            geom_sf(color = "gray50", size = 0.2, show.legend = TRUE) +
            facet_grid(symptoms ~ denom_group, switch = "y") +  # Facet by cause of death and symptom
            scale_fill_manual(
                name = "Cases per 100 Deaths",
                values = c(
                    "Insufficient Data" = "white",
                    "0-10" = "lightgreen",
                    "10-20" = "lawngreen",
                    "20-30" = "forestgreen",
                    "30-40" = "darkgreen",
                    "40-50" = "yellow",
                    "50-60" = "gold",
                    "60-70" = "orange",
                    "70-80" = "orangered",
                    "80-90" = "red",
                    "90-100" = "darkred"
                ),
                breaks = c("Insufficient Data", "0-10", "10-20", "20-30", "30-40", "40-50", "50-60", "60-70", "70-80", "80-90", "90-100"),
                labels = c("Insufficient Data", "[0-10]", "(10-20]", "(20-30]", "(30-40]", "(40-50]", "(50-60]", "(60-70]", "(70-80]", "(80-90]", "(90-100]"),
                drop = FALSE,
                guide = guide_legend(nrow = 1)  
            ) +
            theme_minimal() +
            theme(
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                axis.text = element_blank(),
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                axis.ticks = element_blank(),
                strip.text.x = element_text(size = 12, face = "bold"),
                strip.text.y.left = element_text(size = 12, face = "bold", angle = 0),
                legend.position = 'top',
                legend.justification = c(0.5, 0),
                legend.title = element_text(size = 10, face = "bold"),
                strip.placement = "outside",
                plot.title = element_text(hjust = 0.5, size = 20, face = "bold")
            ) +
            ggtitle(glue("{plot_title}\n(n = {total_sum})")) +
            geom_sf_label(aes(label = fraction), size = 1.8, show.legend = FALSE)
    
    out <- pdf_print(combined_plot, pdf_title, width = width, height = height)
    
    return(out)
}
