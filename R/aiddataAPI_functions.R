#' Generate tables with AidData destination/receiver and funding organization IDs
#'
#' Used internally 
lookuptable.orgID <- function(x) {
  # get data & convert
  res <- httr::GET("http://api.aiddata.org/data/destination/organizations")
  res <- httr::content(res)
  # get result in data.frame shape
  res <- do.call(rbind, lapply(res$hits, rbind))
  res <- data.frame(res)
  # convert column classes
  res[, c("name", "iso3", "iso2", "type_name")] <- apply(res[, c("name", "iso3", "iso2", "type_name")],2,as.character)
  res[, c("id", "oecdCode", "type_id")] <- apply(res[, c("id", "oecdCode", "type_id")], 2, function(x) as.numeric(lapply(x, as.numeric)))  
  # sort results
  lookup.table <- dplyr::arrange(res, name)
  
  # Get Donor IDs
  donor.ids <- GET("http://api.aiddata.org/flows/origin")
  donor.ids <- content(donor.ids)
  donor.ids <- lapply(donor.ids$items, unlist)
  
  donor.table <- do.call(rbind, lapply(donor.ids, "[", c("source._id", "source.name", "source.iso2")))
  donor.table <- data.frame(donor.table)
  
  # Get unique IDs (some appear twice in donor.ids)
  donor.table <- plyr::ddply(donor.table, 
                             .(source._id), 
                             function(x) 
                               data.frame(source.name = unique(x$source.name), 
                                          source.iso2 = ifelse(is.null(unique(x$source.iso2)), 
                                                               NA, 
                                                               unique(as.character(x$source.iso2)))))
  donor.table <- dplyr::arrange(donor.table, source.name)
  

  
  save(lookup.table, file = "./data/lookup-table.rda")
  save(donor.table, file = "./data/donor-lookup-table.rda")
  # return(lookup.table)
}

#' Extract variable information from parsed JSON / list input
#' 
#' Used internally by the \code{get_aid} function
extract_aid <- function(x) {
  # Not all projects have info on all variables, so 'ifelse' captures 
  # missing variables and replace them with NA
  data.frame("project_id" = ifelse(is.null(x$project_id), NA, x$project_id), 
             "recipient" = ifelse(is.null(x$transactions[[1]]$tr_receiver_country$name), NA, x$transactions[[1]]$tr_receiver_country$name),
             "recipient_iso3" = ifelse(is.null(x$transactions[[1]]$tr_receiver_country$iso3), NA, x$transactions[[1]]$tr_receiver_country$iso3),
             "donor" = ifelse(is.null(x$transactions[[1]]$tr_funding_org$name), NA, x$transactions[[1]]$tr_funding_org$name),
             "donor_iso3" = ifelse(is.null(x$transactions[[1]]$tr_funding_org$iso3), NA, x$transactions[[1]]$tr_funding_org$iso3),
             "year" = ifelse(is.null(x$transactions[[1]]$tr_year), NA, x$transactions[[1]]$tr_year),
             
             # Project name information
             "title" = ifelse(is.null(x$title), NA, x$title),
             "name" = ifelse(is.null(x$sectors3[[1]]$name), NA,x$sectors3[[1]]$name), 
             
             # Financial information
             "tr_constant_value" = ifelse(is.null(x$transactions[[1]]$tr_constant_value), NA, x$transactions[[1]]$tr_constant_value),
             "tr_nominal_value" = ifelse(is.null(x$transactions[[1]]$tr_nominal_value), NA, x$transactions[[1]]$tr_nominal_value), 
             
             # Sector information
             "sector3_code" = ifelse(is.null(x$transactions[[1]]$tr_sector3$code), NA, x$transactions[[1]]$tr_sector3$code), 
             "sector5_code" = ifelse(is.null(x$transactions[[1]]$tr_sector5$code), NA, x$transactions[[1]]$tr_sector5$code),
             
             
             stringsAsFactors = FALSE)
}

#' Download AidData Project Information
#' 
#' Downloads all aid projects in the AidData 3.0 database for one or several 
#' countries, parses resulting JSON files, and returns a data frame with the aid
#' projects.
#' 
#' @param rec Character vector of recipient countries (ISO-2 country codes, e.g.
#'   "AO" for Angola). Donor organizations not yet implemented.
#' @param donor Character vector of donor countries (ISO-2 country codes, e.g. 
#'   "US"). Donor organizations not yet implemented.
#' @param start First year of data.
#' @param end Last year of data.
#' @param progressbar Default TRUE. Show a progress bar?
#'   
#' @details This function downloads information on all aid projects between 
#'   \code{start} and \code{end} years, either to the recipient country 
#'   specified in \code{rec} or from the country \code{donor}. Downloading data 
#'   for a large number of countries over a long period of time can result in a 
#'   large number of projects and may take long; the use of the 
#'   \code{progressbar} option is recommended.
#'   
#'   Not all information available through AidData is provided; due to 
#'   performance reasons the functions is restricted to the following variables
#'   
#'   \itemize{ \item{\code{project_id:} }{Unique AidData project ID} 
#'   \item{\code{recipient: } }{Recipient name, character} 
#'   \item{\code{recipient_iso3:} }{Recipient ISO-3 character string} 
#'   \item{\code{donor:} }{Donor name, character string} 
#'   \item{\code{donor_iso3:} }{Donor ISO-3 character string. \code{NA} if IGO, 
#'   NGO, or no official counry} \item{\code{year:} }{Year of commitment} 
#'   \item{\code{title: }}{Project title} \item{\code{name: }}{Sector name} 
#'   \item{\code{tr_constant_value: }}{Value of transaction in constant USD2009}
#'   \item{\code{tr_nominal_value: }}{Value of transaction, as reported by the 
#'   donor in the reported currency} \item{\code{sector3_code: }}{AidData's 
#'   3-digit sector code} \item{\code{sector5_code: }}{AidData's 5-digit sector 
#'   code} }
#'   
#' @return Data frame in which one row represents one project.
#'   
#' @examples
#' \dontrun{
#'  # Download and store all aid projects in Liberia by all donors 
#'  # between 2006 and 2007 and don't show a progress bar.
#'  aid_projects <- get_aid(rec = "LR", start = 2006, end = 2007, progressbar = FALSE)
#' }
#' 
#' @references
#' 
#' Tierney, Michael J., Daniel L. Nielson, Darren G. Hawkins, J. Timmons 
#' Roberts, Michael G. Findley, Ryan M. Powers, Bradley Parks, Sven E. Wilson, 
#' and Robert L. Hicks. 2011. More Dollars than Sense: Refining Our Knowledge of
#' Development Finance Using AidData. \emph{World Development} 39 (11): 
#' 1891-1906.
#' 
#' \url{http://aiddata.org} 
#'  
#' \url{http://aiddata.org/user-guide}
#' @seealso \code{\link{get_gis}}
#'   
get_aid <- function(rec = NULL, donor = NULL, start = NULL, end = NULL, progressbar = TRUE) {
  if(is.null(rec) & is.null(donor)) stop("No donor or recipient country given. Please provide at least one recipient or donor country.")
  
  # lookup recipients organization 'ro' id
  ro <- subset(lookup.table, iso2 %in% rec, select = c("id", "name"))
  name <- paste(ro[, 2], collapse = ", ")
  ro <- paste(ro[, 1], collapse = ",")
  
  # lookup donor organization 'fo' id
  fo <- subset(donor.table, source.iso2 %in% donor, select = c("source._id", "source.name"))
  donor.name <- paste(fo[, 2], collapse = ", ")
  fo <- paste(unique(fo[, 1]), collapse = ",")
  
  if(fo == "" & ro == "") {
    stop("Donor/Recipient ISO not found")
  }
  
  
  # create years lookup
  years <- start:end
  years <- paste(years, collapse = ",")
  
  # put together query options, based on user input
  query.opts <- list(ro = ro,
                     fo = fo,
                     src = "1,2,3,4,5,6,7,8,9,3249668", # all sources
                     t = "1", # commitments only
                     y = years, 
                     from = 0,
                     size = 50) 
  
  # obtain project count with given options
  count <- httr::content(httr::GET("http://api.aiddata.org/aid/project", query = query.opts))$project_count
  
  if(count == 0){
    stop(paste("No aid projects by", donor.name, "found."))
  }
  
  # AidData API limits queries to 50 projects per page. We need to determine the number of 
  # pages we must loop through.
  targetNumOfPages <- floor(count / 50)
  currentPage <- 0 
  from <- 0
  
  # initialize empty project data frame
  project_list <- data.frame(matrix(nrow=count, ncol=length(extract_aid(NULL))))
  
  if(progressbar) {
    writeLines(paste0("Downloading AidData for ", 
                     count, 
                     " aid projects", 
                     ifelse(!is.null(rec), paste0(" in ", name), ""),
                     ifelse(!is.null(donor), paste0(" from ", donor), "")))
    pb <- txtProgressBar(min = 0, max = ifelse(targetNumOfPages > 1, targetNumOfPages, 1), 
                         style = 3, width = 60)
  }
  
  for(i in currentPage:targetNumOfPages) {
    
    if(progressbar) setTxtProgressBar(pb, i)
    
    from <- i * 50
    query.opts$from <- from
    result <- httr::GET("http://api.aiddata.org/aid/project", query = query.opts)
    result <- httr::content(result)
    
    # build output data frame
    res.df <- lapply(result[["items"]], extract_aid)
    
    res.df <- data.table::rbindlist(res.df)
      
    if(i != targetNumOfPages) { 
      project_list[(from+1):(from+50), ] <- res.df
    } else {
      project_list[(from+1):(count), ] <- res.df # last page
    }

  }
  
  names(project_list) <- names(res.df)
  if(progressbar) close(pb)
  return(project_list)
}

#' Download GIS Information for AidData Projects
#' 
#' Downloads geographic information for aid projects in the AidData 3.0 
#' database, parses resulting JSON files, and returns a data frame with the 
#' point information for the projects.
#' 
#' @param rec Character vector of recipient countries (ISO-2 country codes, e.g.
#'   "AO" for Angola). Donor organizations not yet implemented.
#' @param donor Character vector of donor countries (ISO-2 country codes, e.g. 
#'   "US"). Donor organizations not yet implemented.
#' @param start First year of data.
#' @param end Last year of data.
#' @param proj.info Logical. Defaults to \code{FALSE}. Should complete project 
#'   information be downloaded?
#'   
#' @details This function downloads all point information for aid projects in 
#'   country \code{rec} between \code{start} and \code{end} years. Since many 
#'   projects are implemented in different locations, the number of point 
#'   locations typically exceeds the number of projects.
#'   
#'   The API GIS returns a set of basic information:
#'   
#'   \itemize{ \item{\code{project_id: }}{Unique AidData project ID} 
#'   \item{\code{score: }}{Not entirely clear (Precision Code?)} 
#'   \item{\code{loc_geo_name: }}{Name of the geo-referenced point, e.g.
#'   province name, town, village} \item{\code{lat: }}{Latitude} 
#'   \item{\code{long: }}{Longitude} }
#'   
#'   If \code{proj.info = TRUE}, \code{get_gis} calls \code{get_aid} to match 
#'   point information with project information. See \code{\link{get_aid}} 
#'   documentation for list of variables returned by \code{get_aid}. Since this 
#'   procedure downloads a lot of unnecessary information (only a fraction of 
#'   all aid projects are geocoded), this process may take a while.
#'   
#' @return Data frame in which one row represents one point information of an 
#'   aid project.
#'   
#' @references
#' 
#' Tierney, Michael J., Daniel L. Nielson, Darren G. Hawkins, J. Timmons 
#' Roberts, Michael G. Findley, Ryan M. Powers, Bradley Parks, Sven E. Wilson, 
#' and Robert L. Hicks. 2011. More Dollars than Sense: Refining Our Knowledge of
#' Development Finance Using AidData. \emph{World Development} 39 (11): 
#' 1891-1906.
#' 
#' Strandow, Daniel, Michael Findley, Daniel Nielson, and Joshua Powell. 2011. 
#' \emph{The UCDP-AidData codebook on Geo-referencing Foreign Aid.} Version 1.1.
#' Uppsala Conflict Data Program. Uppsala, Sweden: Uppsala University.
#' 
#' \url{http://aiddata.org}
#' 
#' \url{http://aiddata.org/user-guide}
#' 
#' @examples
#' 
#' \dontrun{
#'  # Download and store GIS information for aid projects in Angola between
#'  # 2005 and 2007; get all project information for these projects
#'  aidprojects_gis <- get_gis(rec = "AO", start = 2005, end = 2007, proj.info = TRUE)
#' }
#' 
#' @seealso \code{\link{get_aid}}

get_gis <- function(rec = NULL, donor = NULL, start = NULL, end = NULL, proj.info = FALSE) {
  
  # lookup recipients organization 'ro' id
  ro <- subset(lookup.table, iso2 %in% rec, select = id)
  ro <- paste(ro[, 1], collapse = ",")
  
  # create years lookup
  years <- start:end
  years <- paste(years, collapse = ",")
  
  # put together query options, based on user input
  query.opts <- list(ro = ro,
                     src = "1,2,3,4,5,6,7,3249668", # all sources
                     t = "1", # commitments only
                     y = years, 
                     from = 0, 
                     size = 50)
  result <- httr::GET("http://api.aiddata.org/gis/aid", query = query.opts)
  result <- httr::content(result)
  
  res.df <- lapply(result$items, function(x) data.frame(project_id = x$fields$loc_project_id,
                                                        score = x$`_score`,
                                                        loc_geo_name = x$fields$loc_geo_name,
                                                        lat = x$fields$loc_point$lat,
                                                        long = x$fields$loc_point$lon,
                                                        stringsAsFactors = FALSE))
  res.df <- data.table::rbindlist(res.df)
  
  if(proj.info) {
    proj_info_df <- get_aid(rec = rec, donor = donor, start = start, end = end)
    res.df <- merge(as.data.frame(res.df), proj_info_df, by = "project_id", all.x = TRUE)
  }
  
  return(res.df)
}

#' Lookup table for countries and organizations in AidData
#' 
#' A data frame with AidData's IDs for all 234 countries and organizations. Used 
#' internally by \code{aiddata} functions.
#' 
#' @format A dataset with 234 observations on 8 variables:
#' \itemize{
#'    \item{\code{project_id:} }{Unique AidData project ID}
#'    \item{\code{name: }}{Organization or country name (character string)}
#'    \item{\code{iso3: }}{ISO-3 character code (if available)}
#'    \item{\code{iso2: }}{ISO-2 character code (if available)}
#'    \item{\code{oecdCode: }}{OECD organizational code}
#'    \item{\code{type_id: }}{Unclear (not provided in the documentation)}
#'    \item{\code{type_name: }}{Unclear (not provided in the documentation)}
#'    \item{\code{parent_agency_id: }}{Unclear (not provided in the documentation)}
#' }
#'
#'@source
#'
#'\url{http://aiddata.org}
"lookup.table"

#' Browse AidData Project Details
#' 
#' Opens the AidData web page with information on the given project. For
#' interactive use only.
#' 
#' @param id The unique AidData project ID (given in the \code{project_id} field
#'   of other \code{aiddata} functions)
#'   
#' @details Quick lookup function to browse individual project IDs. Opens a
#'   browser window with detail informationed from on the given project on the
#'   AidData website. This can be useful if more information about the project
#'   is required, e.g. the long description which isn't downloaded by
#'   \code{get_aid}. Wrapper around \code{httr}'s \code{\link{BROWSE}}
#'   function. Only works interactively.
#'   
#' @examples 
#' 
#' \dontrun{
#' 
#' browse_aid(50225207) # Opens AidData project page an Irish education project in Uganda.
#' }   
#'   
#' @seealso \code{\link{BROWSE}}
browse_aid <- function(id = NULL) {
  
  if(!is.null(id)) {
    BROWSE(paste0("http://aiddata.org/dashboard#/project/", id))
  } else {
    stop("Please provide AidData project ID")
  }
  
}


