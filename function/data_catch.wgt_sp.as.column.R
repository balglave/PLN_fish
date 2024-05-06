##############################################################
## Modify catch data so that catch per species are in columns
##############################################################
#' @title data_catch.wgt_sp.as.column
#' @param data_survey.wgt : outputs of the load_data_datras()
#' 
#' @return tmp
#' 
#' @example
#' data_catch.wgt_sp.as.column(data_survey.wgt)
#' 

data_catch.wgt_sp.as.column <- function(data_survey.wgt){

  
  ## delete duplicated data
  data_survey.wgt <- data_survey.wgt[!duplicated(data_survey.wgt),]
  data_survey.wgt %>% 
    dplyr::select("Survey",
                  "StNo",
                  "HaulNo",
                  "Year",
                  "Month",
                  "Stratum",
                  "Distance",
                  "HaulNo",
                  "scientificname",
                  "Sex",
                  "CatIdentifier",
                  "CatCatchWgt",
                  "lati",
                  "long") %>%
    mutate(scientificname = paste("CatchWgt_",scientificname,sep='')) -> tmp
  
  # create toto to save caracteristics of the hauls
  tmp %>% dplyr::select(-c(CatCatchWgt,scientificname,CatIdentifier,Sex)) -> toto
  
  # take only 1st data row for each haul : others are duplicates
  toto %>% group_by(HaulNo,Year) %>% slice(1) -> toto

  # species as columns
  tmp %>%
    group_by(scientificname,HaulNo,Year) %>%
    dplyr :: summarise(CatchWgt = sum(as.numeric(CatCatchWgt))) %>%
    spread(key= scientificname, value = CatchWgt) -> tmp
  tmp <- full_join(toto,tmp, by = c("HaulNo","Year"))
  tmp[is.na(tmp)] <- 0
  return(tmp)
}
