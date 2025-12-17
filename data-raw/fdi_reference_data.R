require(readr)

#regional codelists for multi-reporting purpose
#ICCAT
cl_iccat_species = readr::read_csv("../fdi-codelists/regional/iccat/cl_species.csv")
usethis::use_data(cl_iccat_species, overwrite = TRUE)
cl_iccat_gear_type = readr::read_csv("../fdi-codelists/regional/iccat/cl_gear_type.csv")
usethis::use_data(cl_iccat_gear_type, overwrite = TRUE)
mapping_iccat_sampling_areas__x__stocks = readr::read_csv("../fdi-mappings/regional-to-regional/iccat/codelist_mapping_iccat_sampling_areas_iccat_stocks.csv")
usethis::use_data(mapping_iccat_sampling_areas__x__stocks, overwrite = TRUE)

#WECAFC
cl_wecafc_species = readr::read_csv("../fdi-codelists/regional/wecafc/cl_species.csv")
usethis::use_data(cl_wecafc_species, overwrite = TRUE)
cl_wecafc_gear_type = readr::read_csv("../fdi-codelists/regional/wecafc/cl_gear_type.csv")
usethis::use_data(cl_wecafc_gear_type, overwrite = TRUE)

#RECOFI
cl_recofi_species = readr::read_csv("../fdi-codelists/regional/recofi/cl_species.csv")
usethis::use_data(cl_recofi_species, overwrite = TRUE)
cl_recofi_gear_type = readr::read_csv("../fdi-codelists/regional/recofi/cl_gear_type.csv")
usethis::use_data(cl_recofi_gear_type, overwrite = TRUE)