source("~/Dropbox/git/ckan/data_packs.r")

####################################################################################################################################################
#                                                        Function 1: Add to metadata                                                               #
####################################################################################################################################################

add_to_metadata <- function(metadata_list, area_scale, area_code, area_name, geography_scale = "LSOA", time_period, population, nation, keyword = NULL){
  metadata_list$titl <- paste("CDRC ", time_period, " Mid-Year ", population, " Population Estimates ", area_scale, " Geodata Pack: ", area_name,  " (", area_code, ")", sep="")
  metadata_list$abstract <- paste("This geodata pack provides the ONS mid-", time_period, " ", geography_scale, " ", population, " population estimates for those ",
                                  geography_scale, "s covering the ", area_scale, ": ", area_name, " (", area_code, ")",sep="")
  metadata_list$copyright = paste0(metadata_list$copyright,
                                  "\nContains National Statistics data © Crown copyright and database right ", time_period, ";",
                                  "\nContains Ordnance Survey data © Crown copyright and database right ",time_period, ";")
  metadata_list$dataCollector = c("Office for National Statistics", "Ordnance Survey")
  metadata_list$dataSrc = c(paste0(geography_scale, " Mid-Year Population Estimates, formatted, Mid-", time_period), paste0(geography_scale, " 2011 Boundaries V2 (Full, Clipped)"))
  metadata_list$topcClas = c(paste("Mid-Year", time_period, population, "Population Estimates"))
  metadata_list$geogUnit = c(geography_scale)
  metadata_list$anlyUnit = area_scale
  metadata_list$dataKind = c("Count")
  metadata_list$serName = "CDRC Mid-Year Population Estimates"
  metadata_list$timePrd = time_period
  metadata_list$geogCover = area_name
  metadata_list$nation = nation
  metadata_list$keyword = c(area_code, area_name, "mid-year population estimates", keyword, "boundary", time_period)
  metadata_list$relPubl = ""
  return(metadata_list)
}