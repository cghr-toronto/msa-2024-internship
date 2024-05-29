
# Helper functions ----

#' Calculate Mode Statistic
#'
#' Gets the most frequent value from a vector.
#'
#' Modified from user Ken Williams: https://stackoverflow.com/questions/2547402/how-to-find-the-statistical-mode
#'
#' @param x vector (char or numeric) to get the mode from.
#' @param first Set to `TRUE` to take the first value in case of ties, and `FALSE` to return all values.
#'
#' @return the most frequent value in `x`.
#' @export
#'
#' @examples
#' calc_mode(c(1,1,1,1,2,2,2,3,3,5))
#' calc_mode(c(1,1,1,2,2,2,3,3))
calc_mode <- function(x, first = T, ...) {
    
    # Get all unique values and counts for each
    ux <- unique(x)
    tab <- tabulate(match(x, ux))
    
    # Get the mode
    out <- ux[tab == max(tab)]
    
    # Get the first if needed and return
    out <- if (first) out[[1]] else out
    return(out)
}

# Functions ----

#' Spatial Aggregation
#'
#' Spatially aggregates a geodataframe or dataframe to the geometries of another geodataframe.
#'
#' @param gdf A sf geodataframe containing geometries that can be spatially joined to gdf.
#' @param agg A sf geodataframe or data.frame containing columns with values to be aggregated that can be spatially or non-spatially joined to agg.
#' @param mapping A data.frame containing the behavior of the spatial aggregation pipeline, where each column is structured as follows:
#' \itemize{
#'    \item column: the name (char) of the column in gdf to be aggregated
#'    \item can_aggregate: a comma separated text (char) of aggregation functions to apply to the column (e.g. count,sum,mean,max,median,min,sd,mode,var). Empty values mean this column is skipped.
#' }
#' @param gdf_id Column name (char) containing identifiers for rows in `gdf`. Default is `id`.
#' @param agg_id Column name (char) containing identifiers for rows in `agg`. Default is `id`.
#' @param mapping_col Column name (char) of the column in `mapping` containing column names to aggregate.
#' @param mapping_agg_col Column name (char) of the column in `mapping` containing the comma separated function names to apply to the respective column.
#' @param count_col Column name (char) containing the counts for each row in `agg` joined to `gdf`.
#' @param has_count Set to `TRUE` to include counts for each row in `agg` joined to `gdf` and `FALSE` to exclude.
#' @param is_spatial_join Set to `TRUE` to perform a spatial join, and FALSE to perform a non-spatial join using `gdf_id` and `agg_id` columns. Default is TRUE.
#' @param agg_funcs list of aggregation functions where the key is the name matching functions in the `can_aggregate` column in `mapping`, and the value is the function itself. The `count` key is reserved to count unique values in a column.
#' @param check_can_aggregate Set to `TRUE` to warn if all function names in column `can_aggregate` of `mapping` are known and `FALSE` to silence this warning.
#' @param check_error Set to `TRUE` to error out if any check does not pass or `FALSE` to not error out.
#' @param ... Additional arguments passed to the `st_join` (if is_spatial_join is TRUE) or `join` (otherwise) function that you will use for the aggregation.
#'
#' @return `gdf` with the processed aggregated results of `agg`.
#'
#' @author Dante Christopher-Alphonso, \email{adam.christopher@@torontomu.ca} and Richard Wen \email{rrwen.dev@@gmail.com}
#' @export
#'
#' @examples
#'
spatial_agg <- function(
    gdf,
    agg,
    mapping,
    gdf_id = "id",
    agg_id = "id",
    mapping_col = "column",
    mapping_agg_col = "can_aggregate",
    agg_funcs = list(
        mean = ~ mean(.x, na.rm = TRUE),
        sum = ~ sum(.x, na.rm = TRUE),
        mode = calc_mode,
        median = ~ median(.x, na.rm = TRUE),
        min = ~ min(.x, na.rm = TRUE),
        max = ~ max(.x, na.rm = TRUE),
        sd = ~ sd(.x, na.rm = TRUE),
        var = ~ var(.x, na.rm = TRUE)
    ),
    count_col = "count",
    has_count = TRUE,
    is_spatial_join = TRUE,
    check_can_aggregate = TRUE,
    check_column = TRUE,
    check_error = TRUE,
    ...
) {
    
    # Rename mapping col names
    mapping <- mapping %>%
        rename(
            column = all_of(mapping_col),
            can_aggregate = all_of(mapping_agg_col)
        )
    
    # Get cols in agg and mapping
    agg_cols <- colnames(agg)
    mapping_cols <- mapping %>% pull(column)
    
    # Check if any cols in mapping are missing from agg
    mapping_miss <- mapping_cols[!mapping_cols %in% agg_cols]
    has_mapping_miss <- length(mapping_miss) > 0
    if (check_column & has_mapping_miss) {
        
        # Create warning or error msg
        check_column_msg <- paste0(
            "Columns in mapping not found in agg: ",
            paste0(mapping_miss, collapse = ", ")
        )
        
        # Error out or warn if any cols are missing
        if (check_error) {
            stop(check_column_msg)
        } else {
            warning(check_column_msg)
        }
    }
    
    # Get all avail funcs from mapping
    func_avail <- mapping %>%
        filter(!is.na(can_aggregate)) %>%
        mutate(
            can_aggregate = str_remove_all(can_aggregate, " ")
        ) %>%
        separate_longer_delim(can_aggregate, ",") %>%
        pull(can_aggregate) %>%
        unique
    
    # Check if func are known
    func_known <- c(names(agg_funcs), "count")
    func_unknown <- func_avail[!func_avail %in% func_known]
    has_func_unknown <- length(func_unknown) > 0
    if (check_can_aggregate & has_func_unknown) {
        
        # Create warning or error msg
        check_can_aggregate_msg <- paste0(
            "Unknown aggregate functions in mapping: ",
            paste0(func_unknown, collapse = ", ")
        )
        
        # Error out or warn if any func is not known
        if (check_error) {
            stop(check_can_aggregate_msg)
        } else {
            warning(check_can_aggregate_msg)
        }
    }
    
    # Only include func that are known
    func_avail <- func_avail[func_avail %in% func_known]
    
    # Filter for mapping cols existing in agg
    mapping <- mapping %>% filter(
        column %in% agg_cols
    )
    
    # Perform joins
    if (is_spatial_join == TRUE) {
        
        # Spatial join
        join_gdf <- agg %>% st_join(gdf, ...)
        
    } else {
        
        # Non spatial join
        join_gdf <- gdf %>% right_join(
            agg,
            by = setNames(agg_id, gdf_id),
            ...
        )
    }
    
    # Remove geometry and convert to df
    join_gdf <- join_gdf %>%
        as_tibble() %>%
        select(-geometry)
    
    # Group joined df and remove geometry
    group_gdf <- join_gdf %>%
        group_by(.data[[gdf_id]])
    
    # Perform aggregation for columns based on mapping
    agg_list <- list()
    for (func_name in func_avail) {
        
        # Retrieve agg func names for column
        agg_cols <- mapping %>%
            filter(str_detect(can_aggregate, func_name)) %>%
            filter(column != gdf_id) %>%
            pull(column)
        
        # Apply functions
        if (func_name == "count") {
            
            # Apply unique counts for each gdf object and pivot to columns
            agg_list[[func_name]] <- lapply(
                agg_cols,
                function (x)
                    group_gdf %>%
                    mutate(!!x := na_if(.data[[x]], "")) %>%
                    count(.data[[x]]) %>%
                    pivot_wider(
                        names_from = x,
                        values_from = n
                    ) %>%
                    rename_with(
                        .fn = ~ paste0(x, "_", .),
                        .cols = -all_of(gdf_id)
                    )
                ) %>%
                reduce(left_join, by = gdf_id) %>%
                replace(is.na(.), 0)
            
        } else {
            
            # Get func from func name
            func <- agg_funcs[[func_name]]
          
            # Apply other functions
            agg_list[[func_name]] <- group_gdf %>%
                summarise_at(agg_cols, func) %>%
                filter(!is.na(.data[[gdf_id]]))
        }
        
        # Rename aggregation results columns
        agg_list[[func_name]] <- agg_list[[func_name]] %>%
            rename_with(
                .fn = ~ paste0(., "_", func_name),
                .cols = -all_of(gdf_id)
            )
    }
    
    # Combine agg results into single df
    out <- agg_list %>% reduce(left_join, by = gdf_id)
    
    # Count the number of rows joined to each object in gdf
    if (has_count) {
        out <- out %>% left_join(
            join_gdf %>%
                group_by(.data[[gdf_id]]) %>%
            summarise(!!count_col := n()),
            by = gdf_id
        )
    }
    
    # Join agg results back to gdf objects
    out <- left_join(gdf, out, by = gdf_id)
    return(out)
}
