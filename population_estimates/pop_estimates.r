library(sf)
source("~/Dropbox/git/ckan/population_estimates/pop_estimates_functions.r")

######################################################################################################################################################
#                                                    Download mid-year population estimates                                                          #
######################################################################################################################################################

out_dir <- "~/Dropbox/liverpool/ckan/northern_region/pop_estimates"
dir.create(out_dir)
setwd(out_dir)
url <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/lowersuperoutputareamidyearpopulationestimates/mid2015sape18dt1/sape18dt1mid2015lsoasyoaestimates.zip"
download.file(url, "lsoa_mid_2015.zip")
unzip("lsoa_mid_2015.zip")

######################################################################################################################################################
#                                                            Data preprocessing                                                                      #
#                                          open xslx, remove first three lines and columns 2:3,                                                      #
#                        then rename column 1 to lsoa11cd, and column 2 to total, save each sheet in a folder as csv                                 #
######################################################################################################################################################

lsoa <- st_read("~/Dropbox/liverpool/ckan/northern_region/gis", "lsoa_en", stringsAsFactors = F)
lsoa$LSOA11NM <- NULL
lsoa$LSOA11NMW <- NULL
names(lsoa)[1] <- "lsoa11cd"

persons <- read.csv("~/Dropbox/liverpool/ckan/northern_region/pop_estimates/persons/persons_2015.csv", stringsAsFactors = F)
persons[, 2:93] <- apply(persons[, 2:93], 2, function(x) gsub(",", "", x))
persons[, 2:93] <- apply(persons[, 2:93], 2, as.integer)
persons <- persons[persons$lsoa11cd %in% lsoa$lsoa11cd, ]
names(persons)[3:93] <- paste0("age_", 0:90)
write.csv(persons, "~/Dropbox/liverpool/ckan/northern_region/pop_estimates/persons/persons_2015_en.csv", row.names = F)

males <- read.csv("~/Dropbox/liverpool/ckan/northern_region/pop_estimates/males/males_2015.csv", stringsAsFactors = F)
males[, 2:93] <- apply(males[, 2:93], 2, function(x) gsub(",", "", x))
males[, 2:93] <- apply(males[, 2:93], 2, as.integer)
males <- males[males$lsoa11cd %in% lsoa$lsoa11cd, ]
names(males)[3:93] <- paste0("age_", 0:90)
write.csv(males, "~/Dropbox/liverpool/ckan/northern_region/pop_estimates/males/males_2015_en.csv", row.names = F)

females <- read.csv("~/Dropbox/liverpool/ckan/northern_region/pop_estimates/females/females_2015.csv", stringsAsFactors = F)
females[, 2:93] <- apply(females[, 2:93], 2, function(x) gsub(",", "", x))
females[, 2:93] <- apply(females[, 2:93], 2, as.integer)
females <- females[females$lsoa11cd %in% lsoa$lsoa11cd, ]
names(females)[3:93] <- paste0("age_", 0:90)
write.csv(females, "~/Dropbox/liverpool/ckan/northern_region/pop_estimates/females/females_2015_en.csv", row.names = F)

######################################################################################################################################################
#                                                               Data Input                                                                           #
######################################################################################################################################################

##### lookup tables ##################################################################################################################################
setwd("~/Dropbox/liverpool/ckan/northern_region/lsoa_lookup")
cauth <- fread("lsoa11_cauth.csv")
leps_nop <- fread("lsoa11_lepsnop.csv")
leps_op <- fread("lsoa11_lepsop.csv")
nps <- fread("lsoa11_np.csv")
stps <- fread("lsoa11_stp_lookup.csv")

##### shapefile ######################################################################################################################################
lsoa <- st_read("~/Dropbox/liverpool/ckan/northern_region/gis/lsoa_en.shp", stringsAsFactors = F)
lsoa$LSOA11NM <- NULL
lsoa$LSOA11NMW <- NULL
names(lsoa)[1] <- "lsoa11cd"

##### metadata #######################################################################################################################################
xml_template <- readLines("~/Dropbox/liverpool/ckan/xml_doc.xml")

##### attribute data #################################################################################################################################
persons <-  fread("~/Dropbox/liverpool/ckan/northern_region/pop_estimates/persons/persons_2015_en.csv")
males <- fread("~/Dropbox/liverpool/ckan/northern_region/pop_estimates/males/males_2015_en.csv")
females <- fread("~/Dropbox/liverpool/ckan/northern_region/pop_estimates/females/females_2015_en.csv")

#####################################################################################################################################################
#                                                             Create Data                                                                           #
#####################################################################################################################################################

############################################################# For Persons (total) ###################################################################

sf_df_persons <- merge(lsoa, persons, by = "lsoa11cd")

##### Create NPs for persons ########################################################################################################################

base_dir <- "~/Dropbox/liverpool/ckan/northern_region/pop_estimates/persons/Northern_Powerhouse"
dir.create(base_dir)
setwd(base_dir)

create_data_pack(sf_df = sf_df_persons, dt_lookup = nps[,-"np",with=F], sf_geom_column = "geometry", sf_geog_unit_column = "lsoa11cd",
                 dt_area_column = "npid", dt_geog_unit_column = "lsoa11cd", base_dir = getwd(), csv_tables = F ,create_dir = T)

nps_unique <- unique(nps[,c("npid", "np"), with = F])
invisible(sapply(1:nrow(nps_unique), function(x)
  do.call(create_metadata, c(list(area_code = nps_unique$npid[x], area_name = nps_unique$np[x], area_scale = "Northern Powerhouse", geography_scale = "LSOA",
                  time_period = "2015", base_dir = base_dir, xml_template = xml_template, metadata_list = metadata, population = "Persons",
                  nation = "England", keyword = c("northern powerhouse", "persons"))))))

##### Create STPs for persons #######################################################################################################################

base_dir <- "~/Dropbox/liverpool/ckan/northern_region/pop_estimates/persons/STPs"
dir.create(base_dir)
setwd(base_dir)

create_data_pack(sf_df = sf_df_persons, dt_lookup = stps[,-"stp16nm",with=F], sf_geom_column = "geometry", sf_geog_unit_column = "lsoa11cd",
                 dt_area_column = "stp16cd", dt_geog_unit_column = "lsoa11cd", base_dir = getwd(), csv_tables = F, create_dir = T)

stps_unique <- unique(stps[,c("stp16cd", "stp16nm"), with = F])
invisible(sapply(1:nrow(stps_unique), function(x)
  do.call(create_metadata, c(list(area_code = stps_unique$stp16cd[x], area_name = stps_unique$stp16nm[x], area_scale = "Sustainability and Transformation Plans", geography_scale = "LSOA",
                                  time_period = "2015", base_dir = base_dir, xml_template = xml_template, metadata_list = metadata, population = "Persons",
                                  nation = "England", keyword = c("sustainability and transformation plans", "persons"))))))

##### Create CAUTH for persons #######################################################################################################################

base_dir <- "~/Dropbox/liverpool/ckan/northern_region/pop_estimates/persons/CAUTH"
dir.create(base_dir)
setwd(base_dir)

create_data_pack(sf_df = sf_df_persons, dt_lookup = cauth[,-"cauth16nm",with=F], sf_geom_column = "geometry", sf_geog_unit_column = "lsoa11cd",
                 dt_area_column = "cauth16cd", dt_geog_unit_column = "lsoa11cd", base_dir = getwd(), csv_tables = F, create_dir = T)

cauth_unique <- unique(cauth[,c("cauth16cd", "cauth16nm"), with = F])
invisible(sapply(1:nrow(cauth_unique), function(x)
  do.call(create_metadata, c(list(area_code = cauth_unique$cauth16cd[x], area_name = cauth_unique$cauth16nm[x], area_scale = "Combined Authorities", geography_scale = "LSOA",
                                  time_period = "2015", base_dir = base_dir, xml_template = xml_template, metadata_list = metadata, population = "Persons",
                                  nation = "England", keyword = c("combined authorities", "persons"))))))

##### Create LEPs NOP for persons ####################################################################################################################

base_dir <- "~/Dropbox/liverpool/ckan/northern_region/pop_estimates/persons/LEPs_NOP"
dir.create(base_dir)
setwd(base_dir)

create_data_pack(sf_df = sf_df_persons, dt_lookup = leps_nop[,-"lepnop14nm",with=F], sf_geom_column = "geometry", sf_geog_unit_column = "lsoa11cd",
                 dt_area_column = "lepnop14cd", dt_geog_unit_column = "lsoa11cd", base_dir = getwd(), csv_tables = F, create_dir = T)

lepsnop_unique <- unique(leps_nop[,c("lepnop14cd", "lepnop14nm"), with = F])
invisible(sapply(1:nrow(lepsnop_unique), function(x)
  do.call(create_metadata, c(list(area_code = lepsnop_unique$lepnop14cd[x], area_name = lepsnop_unique$lepnop14nm[x], area_scale = "Local Enterprise Partnerships (Non Overlapping Parts)", geography_scale = "LSOA",
                                  time_period = "2015", base_dir = base_dir, xml_template = xml_template, metadata_list = metadata, population = "Persons",
                                  nation = "England", keyword = c("local enterprise partnerships (non overlapping parts)", "persons"))))))

##### Create LEPs OP for persons #####################################################################################################################

base_dir <- "~/Dropbox/liverpool/ckan/northern_region/pop_estimates/persons/LEPs_OP"
dir.create(base_dir)
setwd(base_dir)

create_data_pack(sf_df = sf_df_persons, dt_lookup = leps_op[,-"lepop14nm",with=F], sf_geom_column = "geometry", sf_geog_unit_column = "lsoa11cd",
                 dt_area_column = "lepop14cd", dt_geog_unit_column = "lsoa11cd", base_dir = getwd(), csv_tables = F, create_dir = T)

lepsop_unique <- unique(leps_op[,c("lepop14cd", "lepop14nm"), with = F])
invisible(sapply(1:nrow(lepsop_unique), function(x)
  do.call(create_metadata, c(list(area_code = lepsop_unique$lepop14cd[x], area_name = lepsop_unique$lepop14nm[x], area_scale = "Local Enterprise Partnerships (Overlapping Parts)", geography_scale = "LSOA",
                                  time_period = "2015", base_dir = base_dir, xml_template = xml_template, metadata_list = metadata, population = "Persons",
                                  nation = "England", keyword = c("local enterprise partnerships (overlapping parts)", "persons"))))))

################################################################### For Males ########################################################################

sf_df_males <- merge(lsoa, males, by = "lsoa11cd")

##### Create NPs for males ###########################################################################################################################

base_dir <- "~/Dropbox/liverpool/ckan/northern_region/pop_estimates/males/Northern_Powerhouse"
dir.create(base_dir)
setwd(base_dir)

create_data_pack(sf_df = sf_df_males, dt_lookup = nps[,-"np",with=F], sf_geom_column = "geometry", sf_geog_unit_column = "lsoa11cd",
                 dt_area_column = "npid", dt_geog_unit_column = "lsoa11cd", base_dir = getwd(), csv_tables = F ,create_dir = T)

invisible(sapply(1:nrow(nps_unique), function(x)
  do.call(create_metadata, c(list(area_code = nps_unique$npid[x], area_name = nps_unique$np[x], area_scale = "Northern Powerhouse", geography_scale = "LSOA",
                                  time_period = "2015", base_dir = base_dir, xml_template = xml_template, metadata_list = metadata, population = "Males",
                                  nation = "England", keyword = c("northern powerhouse", "males"))))))

##### Create STPs for males ###########################################################################################################################

base_dir <- "~/Dropbox/liverpool/ckan/northern_region/pop_estimates/males/STPs"
dir.create(base_dir)
setwd(base_dir)

create_data_pack(sf_df = sf_df_males, dt_lookup = stps[,-"stp16nm",with=F], sf_geom_column = "geometry", sf_geog_unit_column = "lsoa11cd",
                 dt_area_column = "stp16cd", dt_geog_unit_column = "lsoa11cd", base_dir = getwd(), csv_tables = F, create_dir = T)

invisible(sapply(1:nrow(stps_unique), function(x)
  do.call(create_metadata, c(list(area_code = stps_unique$stp16cd[x], area_name = stps_unique$stp16nm[x], area_scale = "Sustainability and Transformation Plans", geography_scale = "LSOA",
                                  time_period = "2015", base_dir = base_dir, xml_template = xml_template, metadata_list = metadata, population = "Males",
                                  nation = "England", keyword = c("sustainability and transformation plans", "males"))))))

##### Create CAUTH for males ###########################################################################################################################

base_dir <- "~/Dropbox/liverpool/ckan/northern_region/pop_estimates/males/CAUTH"
dir.create(base_dir)
setwd(base_dir)

create_data_pack(sf_df = sf_df_males, dt_lookup = cauth[,-"cauth16nm",with=F], sf_geom_column = "geometry", sf_geog_unit_column = "lsoa11cd",
                 dt_area_column = "cauth16cd", dt_geog_unit_column = "lsoa11cd", base_dir = getwd(), csv_tables = F, create_dir = T)

invisible(sapply(1:nrow(cauth_unique), function(x)
  do.call(create_metadata, c(list(area_code = cauth_unique$cauth16cd[x], area_name = cauth_unique$cauth16nm[x], area_scale = "Combined Authorities", geography_scale = "LSOA",
                                  time_period = "2015", base_dir = base_dir, xml_template = xml_template, metadata_list = metadata, population = "Males",
                                  nation = "England", keyword = c("combined authorities", "males"))))))

##### Create LEPs NOP for males ####################################################################################################################

base_dir <- "~/Dropbox/liverpool/ckan/northern_region/pop_estimates/males/LEPs_NOP"
dir.create(base_dir)
setwd(base_dir)

create_data_pack(sf_df = sf_df_males, dt_lookup = leps_nop[,-"lepnop14nm",with=F], sf_geom_column = "geometry", sf_geog_unit_column = "lsoa11cd",
                 dt_area_column = "lepnop14cd", dt_geog_unit_column = "lsoa11cd", base_dir = getwd(), csv_tables = F, create_dir = T)

invisible(sapply(1:nrow(lepsnop_unique), function(x)
  do.call(create_metadata, c(list(area_code = lepsnop_unique$lepnop14cd[x], area_name = lepsnop_unique$lepnop14nm[x], area_scale = "Local Enterprise Partnerships (Non Overlapping Parts)", geography_scale = "LSOA",
                                  time_period = "2015", base_dir = base_dir, xml_template = xml_template, metadata_list = metadata, population = "Males",
                                  nation = "England", keyword = c("local enterprise partnerships (non overlapping parts)", "males"))))))

##### Create LEPs OP for males #####################################################################################################################

base_dir <- "~/Dropbox/liverpool/ckan/northern_region/pop_estimates/males/LEPs_OP"
dir.create(base_dir)
setwd(base_dir)

create_data_pack(sf_df = sf_df_males, dt_lookup = leps_op[,-"lepop14nm",with=F], sf_geom_column = "geometry", sf_geog_unit_column = "lsoa11cd",
                 dt_area_column = "lepop14cd", dt_geog_unit_column = "lsoa11cd", base_dir = getwd(), csv_tables = F, create_dir = T)

invisible(sapply(1:nrow(lepsop_unique), function(x)
  do.call(create_metadata, c(list(area_code = lepsop_unique$lepop14cd[x], area_name = lepsop_unique$lepop14nm[x], area_scale = "Local Enterprise Partnerships (Overlapping Parts)", geography_scale = "LSOA",
                                  time_period = "2015", base_dir = base_dir, xml_template = xml_template, metadata_list = metadata, population = "Males",
                                  nation = "England", keyword = c("local enterprise partnerships (overlapping parts)", "males"))))))

################################################################## For Females #######################################################################

sf_df_females <- merge(lsoa, females, by = "lsoa11cd")

##### Create NPs for females #########################################################################################################################

base_dir <- "~/Dropbox/liverpool/ckan/northern_region/pop_estimates/females/Northern_Powerhouse"
dir.create(base_dir)
setwd(base_dir)

create_data_pack(sf_df = sf_df_females, dt_lookup = nps[,-"np",with=F], sf_geom_column = "geometry", sf_geog_unit_column = "lsoa11cd",
                 dt_area_column = "npid", dt_geog_unit_column = "lsoa11cd", base_dir = getwd(), csv_tables = F ,create_dir = T)

invisible(sapply(1:nrow(nps_unique), function(x)
  do.call(create_metadata, c(list(area_code = nps_unique$npid[x], area_name = nps_unique$np[x], area_scale = "Northern Powerhouse", geography_scale = "LSOA",
                                  time_period = "2015", base_dir = base_dir, xml_template = xml_template, metadata_list = metadata, population = "Females",
                                  nation = "England", keyword = c("northern powerhouse", "females"))))))

##### Create STPs for females #########################################################################################################################

base_dir <- "~/Dropbox/liverpool/ckan/northern_region/pop_estimates/females/STPs"
dir.create(base_dir)
setwd(base_dir)

create_data_pack(sf_df = sf_df_females, dt_lookup = stps[,-"stp16nm",with=F], sf_geom_column = "geometry", sf_geog_unit_column = "lsoa11cd",
                 dt_area_column = "stp16cd", dt_geog_unit_column = "lsoa11cd", base_dir = getwd(), csv_tables = F, create_dir = T)

invisible(sapply(1:nrow(stps_unique), function(x)
  do.call(create_metadata, c(list(area_code = stps_unique$stp16cd[x], area_name = stps_unique$stp16nm[x], area_scale = "Sustainability and Transformation Plans", geography_scale = "LSOA",
                                  time_period = "2015", base_dir = base_dir, xml_template = xml_template, metadata_list = metadata, population = "Females",
                                  nation = "England", keyword = c("sustainability and transformation plans", "females"))))))

##### Create CAUTH for females #########################################################################################################################

base_dir <- "~/Dropbox/liverpool/ckan/northern_region/pop_estimates/females/CAUTH"
dir.create(base_dir)
setwd(base_dir)

create_data_pack(sf_df = sf_df_females, dt_lookup = cauth[,-"cauth16nm",with=F], sf_geom_column = "geometry", sf_geog_unit_column = "lsoa11cd",
                 dt_area_column = "cauth16cd", dt_geog_unit_column = "lsoa11cd", base_dir = getwd(), csv_tables = F, create_dir = T)

invisible(sapply(1:nrow(cauth_unique), function(x)
  do.call(create_metadata, c(list(area_code = cauth_unique$cauth16cd[x], area_name = cauth_unique$cauth16nm[x], area_scale = "Combined Authorities", geography_scale = "LSOA",
                                  time_period = "2015", base_dir = base_dir, xml_template = xml_template, metadata_list = metadata, population = "Females",
                                  nation = "England", keyword = c("combined authorities", "females"))))))

##### Create LEPs NOP for females ##################################################################################################################

base_dir <- "~/Dropbox/liverpool/ckan/northern_region/pop_estimates/females/LEPs_NOP"
dir.create(base_dir)
setwd(base_dir)

create_data_pack(sf_df = sf_df_females, dt_lookup = leps_nop[,-"lepnop14nm",with=F], sf_geom_column = "geometry", sf_geog_unit_column = "lsoa11cd",
                 dt_area_column = "lepnop14cd", dt_geog_unit_column = "lsoa11cd", base_dir = getwd(), csv_tables = F, create_dir = T)

invisible(sapply(1:nrow(lepsnop_unique), function(x)
  do.call(create_metadata, c(list(area_code = lepsnop_unique$lepnop14cd[x], area_name = lepsnop_unique$lepnop14nm[x], area_scale = "Local Enterprise Partnerships (Non Overlapping Parts)", geography_scale = "LSOA",
                                  time_period = "2015", base_dir = base_dir, xml_template = xml_template, metadata_list = metadata, population = "Females",
                                  nation = "England", keyword = c("local enterprise partnerships (non overlapping parts)", "females"))))))

##### Create LEPs OP for females ###################################################################################################################

base_dir <- "~/Dropbox/liverpool/ckan/northern_region/pop_estimates/females/LEPs_OP"
dir.create(base_dir)
setwd(base_dir)

create_data_pack(sf_df = sf_df_females, dt_lookup = leps_op[,-"lepop14nm",with=F], sf_geom_column = "geometry", sf_geog_unit_column = "lsoa11cd",
                 dt_area_column = "lepop14cd", dt_geog_unit_column = "lsoa11cd", base_dir = getwd(), csv_tables = F, create_dir = T)

invisible(sapply(1:nrow(lepsop_unique), function(x)
  do.call(create_metadata, c(list(area_code = lepsop_unique$lepop14cd[x], area_name = lepsop_unique$lepop14nm[x], area_scale = "Local Enterprise Partnerships (Overlapping Parts)", geography_scale = "LSOA",
                                  time_period = "2015", base_dir = base_dir, xml_template = xml_template, metadata_list = metadata, population = "Females",
                                  nation = "England", keyword = c("local enterprise partnerships (overlapping parts)", "females"))))))

