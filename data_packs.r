##########################################################################################################################
#                                                Produce Data Packs                                                      #
##########################################################################################################################
library(sf)
library(data.table)
library(whisker)

##########################################################################################################################
#                                 Get data and subset columns, optionally rename columns                                 #
##########################################################################################################################
# import data, subset columns by name or index (optional), rename columns (optional)
get_data <- function(path_and_file_name,  select_cols_vec =  NULL, new_col_names_vec = NULL,  ...){
  
  Df <- fread(path_and_file_name, ...)
  if (!is.null(select_cols_vec)){
    Df <- Df[, select_cols_vec, with = F]
  }
  if (!is.null(new_col_names_vec)){
    if (length(new_col_names_vec) != ncol(Df)){
      stop("the number of names in col_names should be the same as the number of (the selected) columns")
    }
    setnames(Df, 1:ncol(Df), new_col_names_vec)
  }
  return(Df)
  
}

##########################################################################################################################
#                                                 Join two data tables                                                   #
##########################################################################################################################
# inner join, optionally pass arguments for outer joins (e.g. all.x etc)
join_data <- function(table_x, table_y, by_x, by_y, ...){
  setkeyv(table_x, by_x)
  setkeyv(table_y, by_y)
  return(merge.data.table(table_x, table_y, by.x = by_x, by.y = by_y, ...))
}

##########################################################################################################################
#                                   Produce aggregate stats for a number of columns                                      #
##########################################################################################################################
##### object to hold the names of the attribute columns (vector of characters),
##### the summary statistic (character) and how to treat NA values (boolean)
map_attr_stats <- setClass('map_attr_stats',
                           slots = c(
                             attr_col_name = "character",
                             stat_name = "character",
                             na_behaviour = "logical"),
                           validity = function(object){
                             if (length(object@stat_name) > 1 || length(object@na_behaviour) > 1){
                               return("stat_name and na_behaviour have to be of length 1")
                             }
                             return(T)
                           }
                          )

##### calculate summary statistic in a data.table, the group by can be one or more columns
aggregate_stats <- function(Dt, group_cols_names, attr_cols_names, stat_to_calc,  na.rm = T, sort_by_key = T){
  if (sort_by_key){
    Dt <- setkeyv(Dt, group_cols_names)
  }
  
  if (na.rm){
    out <- eval(parse(text = paste0("Dt[,lapply(.SD, ", stat_to_calc,", na.rm = na.rm), by=group_cols_names, .SDcols=attr_cols_names]")))
   } else {
    out <- eval(parse(text = paste0("Dt[,lapply(.SD, ", stat_to_calc,"), by=group_cols_names, .SDcols=attr_cols_names]")))
  }
  setnames(out, names(out), c(group_cols_names, paste(attr_cols_names, stat_to_calc, sep = "_")))
  return(out)
}

##### calculate multiple summary statistics, return the result in a data.table
aggregate_multiple_stats <- function(Dt, group_cols_names, map_attr_stats_list){
  Dt <- setkeyv(Dt, group_cols_names)
  out_stat <- aggregate_stats(Dt, group_cols_names, attr_cols_names = map_attr_stats_list[[1]]@attr_col_name,
                              stat_to_calc = map_attr_stats_list[[1]]@stat_name, na.rm = map_attr_stats_list[[1]]@na_behaviour,
                              sort_by_key = F)
                              
  if (length(map_attr_stats_list) > 1){
    for (i in 2:length(map_attr_stats_list)){
      temp_stat <- aggregate_stats(Dt, group_cols_names, attr_cols_names = map_attr_stats_list[[i]]@attr_col_name,
                                   stat_to_calc = map_attr_stats_list[[i]]@stat_name, na.rm = map_attr_stats_list[[i]]@na_behaviour,
                                   sort_by_key = F)
      out_stat <- join_data(out_stat, temp_stat, by_fields_t1 = group_cols_names, by_fields_t2 = group_cols_names)
    }
  }
  return(out_stat)
}


##########################################################################################################################
#                                                    Metadata List                                                       #
##########################################################################################################################
metadata <- list(grantNo = "ES/L011840/1",
                 titl = "",
                 AuthEnty = c("Alex Singleton, University of Liverpool", "Michalis Pavlis, University of Liverpool"),
                 depositr = "Michalis Pavlis, University of Liverpool",
                 fundAg = "Economic and Social Research Council",
                 producer= "ESRC Consumer Data Research Centre",
                 copyright = paste0("The following attribution statements must be used to acknowledge copyright and source in use of these datasets:\n",
                                    "Data provided by the ESRC Consumer Data Research Centre;"),
                 dataCollector = "",
                 dataSrc = "",
                 topcClas = "",
                 keyword = "",
                 abstract = "",
                 timePrd = "",
                 collDate = "",
                 nation = "",
                 geogCover = "",
                 geogUnit = "",
                 anlyUnit = "",
                 dataKind = "",
                 accsPlac = c("Consumer Data Research Centre", "UK Data Service"),
                 contact = "data@cdrc.ac.uk",
                 relPubl = "",
                 serName = ""
)

##########################################################################################################################
#                                                       Write Readme txt                                                 #
##########################################################################################################################

write_readme <- function(metadata_list, out_dir, csv_tables = T){
  readme <- file(paste(out_dir, "readme.txt", sep="/"))
  writeLines(c(metadata_list$titl,
               "\n",
               "+ Abstract",
               metadata_list$abstract,
               "\n",
               "+ Contents",
               "\t - readme.txt: Information about the CDRC Geodata pack",
               "\t - metadata.xml: Metadata",
               if(csv_tables) "\t - tables: Folder containing the csv files",
               "\t - shapefiles: Folder containing the shapefiles",
               "\n",
               "+ Citation and Copyright",
               metadata_list$copyright,
               "\n",
               "+ Funding",
               paste("Funded by: Economic and Social Research Council", metadata_list$grantNo),
               "\n",
               "+ Other Information",
               "Areas that contained no information in the original dataset, are marked with NA in the csv files and NULL in the shapefiles."
  ),
  readme)
  close(readme)
}


##########################################################################################################################
#                                                    Write Metadata xml                                                  #
##########################################################################################################################
write_xml <- function(xml_template, metadata_list, out_dir){
  new_xml <- whisker.render(xml_template, metadata_list)
  writeLines(new_xml, paste(out_dir,"metadata.xml", sep="/"))
}

##########################################################################################################################
#                                                     Write other stuff                                                  #
##########################################################################################################################
# write_other_text <- function(output_text_list, out_dir){
#   for (out_file_name in output_text_list){
#     out_file <- file(paste(out_dir, out_file_name, sep="/"))
#     writeLines(output_text_list[[out_file_name]], out_file)
#     close(out_file)
#   }
# }

##########################################################################################################################
#                                                      Create Metadata                                                   #
##########################################################################################################################
# the argumants: area_code, area_name, area_scale, geography_scale, time_period are always required by add_to_metadata
# the function should be in every (x_data_pack)_functions.r script, other arguments can be passed in add_to_metadata
create_metadata <- function(area_code, area_name, area_scale, geography_scale, time_period, base_dir, xml_template,
                            metadata_list = metadata, csv_tables = T, ...){
  
  metadata_list <- add_to_metadata(metadata_list = metadata_list, area_scale = area_scale, area_code = area_code,
                                   area_name = area_name, geography_scale = geography_scale,time_period =  time_period, ...)
  
  out_dir <- paste(base_dir, area_code, sep = "/")
  write_readme(metadata_list, out_dir, csv_tables = csv_tables)
  write_xml(xml_template, metadata_list, out_dir)
}

###########################################################################################################################
#                                                   Create directories                                                    #
###########################################################################################################################
create_dirs <- function(dir_name, base_dir){
  out_dir <- paste(base_dir, dir_name, sep = "/")
  dir.create(out_dir)
  return(out_dir)
}

###########################################################################################################################
#                                                      Subset data                                                        #
###########################################################################################################################
subset_data <- function(col_value, input_data, col_name, rm_col = F){
  out_data <- subset(input_data, input_data[[col_name]] %in% col_value)
  if (rm_col){
    out_data[[col_name]] <- NULL
  }
  return(out_data)
}

###########################################################################################################################
#                                            Merge shapefile with attribute data                                          #
###########################################################################################################################
# merge_shps_attr <- function(sf_df, dt, by_x, by_y, all_x){
#   if (! all_x){
#     sf_df <- subset_data(dt[[by_y]], sf_df, by_x)
#   }
#   n <- ncol(sf_df)
#   sf_df <- data.frame(sf_df, dt[match(sf_df[[by_x]], dt[[by_y]]), ])
#   sf_df[, n+1] <- NULL
#   return(sf_df)
# }

###########################################################################################################################
#                                           Extract data per geographical unit                                            #
###########################################################################################################################

extract_per_geog <- function(area_value, input_data, area_column, base_dir, geom_column = "geometry",
                             create_dir = T,  name_suffix="", csv_tables = T){
  
  out_data <- subset_data(area_value, input_data, area_column, rm_col = T)
  if (create_dir){
    if (csv_tables){
      out_dir_csv <- create_dirs("tables", base_dir) 
    }
    out_dir_shp <- create_dirs("shapefiles", base_dir)
  } else {
    if (csv_tables){
      out_dir_csv <- paste0(base_dir,"/tables") 
    }
    out_dir_shp <- paste0(base_dir,"/shapefiles")
  }
  if (csv_tables){
    fwrite(out_data[,names(out_data) != geom_column], paste0(out_dir_csv, "/", area_value, "_", name_suffix, ".csv")) 
  }
  st_write(st_as_sf(as.data.frame(out_data)), paste0(out_dir_shp, "/", area_value, if(name_suffix != "") paste0("_", name_suffix), ".shp"), driver = "ESRI Shapefile")
}

##########################################################################################################################
#                                               Create data pack                                                         #
#                              input: a sf object and a lookup data.table object                                         #
#                                 output: csvs and shapefiles per geography                                              #
##########################################################################################################################
create_data_pack <- function(sf_df, dt_lookup, base_dir, sf_geom_column = "geometry", sf_geog_unit_column  = "lsoa11cd",
                             dt_area_column = "lad11cd", dt_geog_unit_column = "lsoa11cd", name_suffix = "", 
                             create_dir = T, csv_tables = T){
  names_sf <- names(sf_df)
  names_dt <- names(dt_lookup)
  
  if (! sf_geom_column %in% names_sf) stop(paste("column", sf_geom_column, "is missing from sf_df"))
  if (! sf_geog_unit_column %in% names_sf) stop(paste("column", sf_geog_unit_column, "is missing from sf_df"))
  if (! dt_area_column %in% names_dt) stop(paste("column", dt_area_column, "is missing from dt_lookup"))
  if (! dt_geog_unit_column %in% names_dt) stop(paste("column", dt_geog_unit_column, "is missing from dt_lookup"))
  if (! all(unique(dt_lookup[[dt_geog_unit_column]]) %in% unique(sf_df[[sf_geog_unit_column]]))){
    stop(paste("not all values in column", dt_geog_unit_column, "of dt_lookup are in column", sf_geog_unit_column, "of sf_df"))
  }
  if (any(is.na(sf_df[[sf_geom_column]]))) stop(paste("NA values were found in column", sf_geom_column, "of sf_df"))
  if (any(is.na(sf_df[[sf_geog_unit_column]]))) stop(paste("NA values were found in column", sf_geog_unit_column, "of sf_df"))
  if (any(is.na(dt_lookup[[dt_area_column]]))) stop(paste("NA values were found in column", dt_area_column, "of dt_lookup"))
  if (any(is.na(dt_lookup[[dt_geog_unit_column]]))) stop(paste("NA values were found in column", dt_geog_unit_column, "of dt_lookup"))
  
  # create folders
  if (create_dir) invisible(sapply(unique(dt_lookup[[dt_area_column]]), create_dirs, base_dir))
  
  setDT(sf_df)
  
  # join sf with lookup
  Dt <- merge(dt_lookup, sf_df, by.x = dt_geog_unit_column, by.y = sf_geog_unit_column, all.x = T)
  
  # extract data from data.table
  invisible(sapply(unique(Dt[[dt_area_column]]), function(x)
    extract_per_geog(area_value = x, input_data = Dt, area_column = dt_area_column, base_dir = paste(base_dir, x, sep = "/"),
                     geom_column = geom_column, create_dir = create_dir, name_suffix = name_suffix, csv_tables = csv_tables)))
  
}
