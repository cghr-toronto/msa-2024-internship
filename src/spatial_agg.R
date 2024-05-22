#' Title: Spatial Aggregation function
#'
#' @param gdf: a sf geodataframe or data.frame containing columns with values to be aggregated 
#' that can be spatially or non-spatially joined to gdf_agg.
#' 
#' @param gdf_agg: a sf geodataframe containing geometries that can be spatially joined to gdf.
#' 
#' @param gdf_join the column name (char) containing values in gdf to be joined non-spatially 
#' to gdf_agg. Only applicable if is_spatial_join = TRUE. Default should be NULL.
#' 
#' @param gdf_agg_join: the column name (char) containing values in gdf_agg to be joined non-spatially 
#' to gdf. Only applicable if is_spatial_join = TRUE. Default should be NULL.
#' 
#' @param gdf_agg_id: the column name (char) containing identifiers for rows in gdf_agg.
#' 
#' @param mapping: a data.frame containing the behaviour of the spatial aggregation pipeline, 
#' where each column is structured as follows:
#'  - column: the name (char) of the column in gdf to be aggregated
#'  - can_aggregate: a comma separated text (char) of aggregation functions to apply to the column 
#'    (e.g. count,sum,mean,max,median,min,sd,mode,var). Empty values mean this column is skipped. 
#' 
#' @param is_spatial_join: Set to TRUE to perform a spatial join using gdf_geom and gdf_agg_geom, 
#' and FALSE to perform a non-spatial join using gdf_join and gdf_agg_join columns. Default is TRUE.
#'
#' @param ...: additional arguments passed to the st_join (if is_spatial_join is TRUE) or 
#' join (otherwise) function that you will use for the aggregation.
#'
#'
#' @return out: gdf_agg with the processed aggregated results of gdf. 
#' @export
#'
#' @examples

spatial_agg <- function(gdf, gdf_agg, gdf_join = NULL, gdf_agg_join = NULL, 
                        gdf_agg_id, gdf_geom = geometry, gdf_agg_geom = geometry, 
                        mapping, mapping_col = column, mapping_agg = can_aggregate, is_spatial_join, ...){
  
  # Perform joins
  if (is_spatial_join == TRUE){
    
    # Spatial join
    join_gdf <- gdf %>% 
      st_join(gdf_geom, gdf_agg_geom, ...)
    
  } else {
    
    # Non spatial join
    join_gdf <-
      left_join(gdf, gdf_agg, by = setNames(gdf_agg_join, gdf_join), ...)
  }
  
   
  # Group the joined dataframe
  group_gdf <- join_gdf %>% group_by(.data[[gdf_agg_id]])
  
  # List of aggregation functions available to use
  agg_funcs <- c("mean", "sum", "mode", "median", "min", "max", "sd", "var")
 
  # List of mappings functions that match with the joined gdf
  mappings_funcs <- list()

  #List for aggregation results
  agg_list <- list()
  
  
  # Performing aggregation for columns in mappings
  for (func_name in agg_funcs) {
    
    if (func_name %in% c("mode", "mean", "sum", "median", "min", "max", "sd", "var")){
      
      # Retrieve aggregation function connected to corresponding column
      mappings_funcs[[func_name]] <- mappings %>%
        filter(str_detect(mapping_agg, func_name)) %>%
        pull(mapping_col)
      
      # Convert function names to functions
      func <- get(func_name)
      
      # Apply functions
      if (func_name == "mode") {
        
        # Mode does not remove NA's
        agg_list[[func_name]] <- group_gdf %>%
          summarise_at(
            mappings_funcs[[func_name]],
            func
          )
        
      } else {
        
        # Other functions remove NA's
        agg_list[[func_name]] <- group_gdf %>%
          summarise_at(
            mappings_funcs[[func_name]],
            func,
            na.rm = TRUE
          )
      }
    }
   
    # Rename aggregation results columns
    agg_list[[func_name]] <- agg_list[[func_name]] %>%
      rename_with(
        .fn = ~ paste0( ., "_", func_name),
        .cols = -all_of(gdf_agg_id)
      )
  }
  
  # Combine aggregation results and renamed columns into singular vector
  agg_results <- agg_list %>% reduce(left_join, by = gdf_agg_id)   
  
  # Joining function results back to district boundaries
  out <- left_join(gdf_agg, agg_results, by = setNames(gdf_join, gdf_agg_join))
  
  return(out)
  
}