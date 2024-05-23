
#' Spatial Aggregation
#' 
#' Spatially aggregates a geodataframe or dataframe to the geometries of another geodataframe.
#'
#' @param gdf A sf geodataframe containing geometries that can be spatially joined to gdf.
#' @param gdf_agg A sf geodataframe or data.frame containing columns with values to be aggregated that can be spatially or non-spatially joined to gdf_agg.
#' @param mapping A data.frame containing the behaviour of the spatial aggregation pipeline, where each column is structured as follows:
#' \itemize{
#'    \item column: the name (char) of the column in gdf to be aggregated
#'    \item can_aggregate: a comma separated text (char) of aggregation functions to apply to the column (e.g. count,sum,mean,max,median,min,sd,mode,var). Empty values mean this column is skipped. 
#' }
#' @param gdf_id Column name (char) containing identifiers for rows in `gdf`. Default is `id`.
#' @param gdf_agg_id Column name (char) containing identifiers for rows in `gdf_agg`. Default is `id`.
#' @param is_spatial_join Set to `TRUE` to perform a spatial join using gdf_geom and gdf_agg_geom, and FALSE to perform a non-spatial join using `gdf_join` and `gdf_agg_join` columns. Default is TRUE.
#' @param has_count Set to `TRUE` to include counts for each row in `gdf_agg` joined to `gdf` and `FALSE` to exclude.
#' @param count_col Column name (char) containing the counts for each row in `gdf_agg` joined to `gdf`.
#' @param ... Additional arguments passed to the st_join (if is_spatial_join is TRUE) or 
#' join (otherwise) function that you will use for the aggregation.
#'
#' @return `gdf` with the processed aggregated results of `gdf_agg`.
#' 
#' @author Dante Christopher-Alphonso, \email{adam.christopher@@torontomu.ca} and Richard Wen \email{rrwen.dev@@gmail.com}
#' @export
#'
#' @examples
#' 
spatial_agg <- function(
    gdf,
    gdf_agg,
    mapping,
    gdf_id = "id",
    gdf_agg_id = "id",
    is_spatial_join = TRUE,
    has_count = TRUE,
    count_col = "count",
    ...
  ){
  
  # Perform joins
  if (is_spatial_join == TRUE){
    
    # Spatial join
    join_gdf <- gdf %>% st_join(gdf_agg, ...)
    
  } else {
    
    # Non spatial join
    join_gdf <- gdf %>% left_join(
      gdf_agg,
      by = setNames(gdf_id, gdf_agg_id),
      ...
    )
    
  }
  
  # Group joined df
  group_gdf <- join_gdf %>% group_by(.data[[gdf_id]])
  
  # Agg funcs avail
  agg_funcs <- c("count", "mean", "sum", "mode", "median", "min", "max", "sd", "var")

  # Agg results
  agg_list <- list()
  
  # Performing aggregation for columns in mappings
  for (func_name in agg_funcs) {
      
    # Retrieve agg func names for column
    agg_cols <- mappings %>%
      filter(str_detect(can_aggregate, func_name)) %>%
      filter(column != gdf_id)
      pull(column)
    
    # Convert function names to functions
    func <- get(func_name)
    
    # Apply functions
    if (func_name == "mode") {
      
      # Mode does not remove NA's
      agg_list[[func_name]] <- group_gdf %>%
        summarise_at(
          agg_cols,
          func
        )

    } else if (func_name == "count") {
      
      # Apply unique counts for each gdf object and pivot to columns
      agg_list[[func_name]] <- lapply(
        agg_cols,
        function (x) group_gdf %>%
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
      
      # Other functions remove NA's
      agg_list[[func_name]] <- group_gdf %>%
        summarise_at(
          agg_cols,
          func,
          na.rm = TRUE
        )
    }
   
    # Rename aggregation results columns
    agg_list[[func_name]] <- agg_list[[func_name]] %>%
      rename_with(
        .fn = ~ paste0( ., "_", func_name),
        .cols = -all_of(gdf_id)
      )
  }
  
  # Combine agg results into single df
  out <- agg_list %>% reduce(left_join, by = gdf_id)
  
  # Count the number of rows joined to each object in gdf
  if (has_count) {
    out <- out %>% left_join(
      out %>%
        group_by(.data[[gdf_id]]) %>%
        summarise(!!count_col := n()),
      by = gdf_id
    )
  }
  
  # Join agg results back to gdf objects
  out <- left_join(gdf_agg, out, by = setNames(gdf_id, gdf_agg_id))
  return(out)
}
