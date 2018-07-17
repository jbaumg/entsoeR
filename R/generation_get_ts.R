#' time series version of generation_get
#' @description function to download generation data in a "time series" format (dates, generation)
#' @param documentType
#' @param processType
#' @param businessType
#' @param psrType
#' @param type_MarketAgreement.Type
#' @param auction.Type
#' @param auction.Category
#' @param classificationSequence_AttributeInstanceComponent.Position
#' @param outBiddingZone_Domain
#' @param biddingZone_Domain
#' @param controlArea_Domain
#' @param in_Domain
#' @param out_Domain
#' @param acquiring_Domain
#' @param timeInterval
#' @param periodStart
#' @param periodEnd
#' @param securityToken
#' @export

generation_get_ts <- function(documentType = NULL,
                        processType = NULL,
                        businessType = NULL,
                        psrType = NULL,
                        type_MarketAgreement.Type = NULL,
                        contract_MarketAgreement.Type = NULL,
                        auction.Type = NULL,
                        auction.Category = NULL,
                        classificationSequence_AttributeInstanceComponent.Position = NULL,
                        outBiddingZone_Domain = NULL,
                        biddingZone_Domain = NULL,
                        controlArea_Domain = NULL,
                        in_Domain = NULL,
                        out_Domain = NULL,
                        acquiring_Domain = NULL,
                        timeInterval = NULL,
                        periodStart = NULL,
                        periodEnd = NULL,
                        securityToken = NULL){
  
  final_url <- entsoe_create_url(documentType = documentType,
                                 processType = processType,
                                 businessType = businessType,
                                 psrType = psrType,
                                 type_MarketAgreement.Type = type_MarketAgreement.Type,
                                 contract_MarketAgreement.Type = contract_MarketAgreement.Type,
                                 auction.Type = auction.Type,
                                 auction.Category = auction.Category,
                                 classificationSequence_AttributeInstanceComponent.Position = classificationSequence_AttributeInstanceComponent.Position,
                                 outBiddingZone_Domain = outBiddingZone_Domain,
                                 biddingZone_Domain = biddingZone_Domain,
                                 controlArea_Domain = controlArea_Domain,
                                 in_Domain = in_Domain,
                                 out_Domain = out_Domain,
                                 acquiring_Domain = acquiring_Domain,
                                 timeInterval = timeInterval,
                                 periodStart = periodStart,
                                 periodEnd = periodEnd,
                                 securityToken = securityToken)
  
  # make GET request
  e_request <- httr::GET(url = final_url)
  
  # check status
  if(httr::status_code(e_request) == 400){
    stop(paste0(httr::http_status(e_request)$category, ". ",
                httr::http_status(e_request)$reason, ". ",
                httr::http_status(e_request)$message, ". ",
                e_request %>% httr::content(., encoding = "UTF-8") %>% 
                  xml2::xml_child(., 8) %>% 
                  xml2::xml_child(., 2) %>% 
                  xml2::xml_text()), call. = FALSE)
  } else if(httr::status_code(e_request) == 500){
    stop(paste0(httr::http_status(e_request)$category, ". ",
                httr::http_status(e_request)$reason, ". ",
                httr::http_status(e_request)$message, ". ",
                e_request %>% httr::content(., encoding = "UTF-8") %>% 
                  rvest::html_node("body") %>% 
                  rvest::html_text()), call. = FALSE)
  }
  
  # Check if the get request returns application/zip
  # if yes, save to folder and unzip
  # else, parse the file.
  if(httr::http_type(e_request) == "application/zip"){
    
    stop("application/zip not supported", call. = FALSE)
    
  } else if(httr::http_type(e_request) == "text/xml"){
    
    # not tested yet.
    html_doc <- httr::content(e_request, as = "text", encoding = "UTF-8")
    
    e_content <- generation_helper(html_doc = html_doc)
    
  } else {
    
    stop("Http type not supported.", call. = FALSE)
  }
  
  e_content
}



generation_helper <- function(html_doc){
  
  html_doc <- html_doc %>% 
    xml2::read_html(encoding = "UTF-8") %>% 
    rvest::html_node("body") %>% 
    rvest::html_node("gl_marketdocument")
  
  # data for time series
  html_ts <- 
    html_doc %>% 
    rvest::html_nodes("timeseries")
  
  # nested xml docs. period and interval belong together
  html_period <- 
    html_ts %>% 
    purrr::map(~rvest::html_nodes(.x, "period"))
  
  html_point <- 
    html_period %>% 
    purrr::map(~rvest::html_nodes(.x, "point"))
    
  
  # nested xml docs. mktpsrtype and mktgeneratingunit belong together
  html_mktpsrtype <- 
    html_ts %>% 
    purrr::map(~rvest::html_nodes(.x, "mktpsrtype"))
  
  html_mktgeneratingunit <- 
    html_mktpsrtype %>%
    purrr::map(~rvest::html_nodes(.x, "MktGeneratingUnit"))
  
  ###########################################
  # extract doc info
  #############################################
  
  ids <- c("mRID", 
           "revisionNumber", 
           "type", 
           "process.processType", 
           "sender_MarketParticipant.mRID", 
           "sender_MarketParticipant.marketRole.type", 
           "receiver_MarketParticipant.mRID", 
           "receiver_MarketParticipant.marketRole.type", 
           "createdDateTime",
           "time_Period.timeInterval")
  ids <- tolower(ids)
  
  doc_result <-
    id_extractor(html_doc, ids) %>%
    {suppressMessages(readr::type_convert(.))}
  
  ####################################
  # extract timeseries
  ######################################
  
  ids <- c("mRID",
           "businessType",
           "objectAggregation",
           "inBiddingZone_Domain.mRID",
           "outBiddingZone_Domain.mRID",
           "registeredResource.mRID",
           "registeredResource.name",
           "quantity_Measure_Unit.name",
           "curveType",
           "cancelledTS")
  ids <- tolower(ids)
  
  time_series <- 
    suppressMessages(html_ts %>%
                       purrr::map(~id_extractor(.x, ids)) %>%
                       dplyr::bind_rows() %>%
                       readr::type_convert())
  
  ##################################################
  # get resolution
  ##################################################
  if(length(html_period[[1]]) > 0){
    ids <- c("resolution")
    ids <- tolower(ids)
    
    time_series$period <- 
      html_period %>%
      purrr::map(~id_extractor(.x, ids))
    
    ids <- c("start", "end")
    ids <- tolower(ids)
    
    doc_result_ts_timeinterval <- 
      suppressMessages(html_ts %>% 
                         rvest::html_nodes("timeinterval") %>%
                         purrr::map(~id_extractor(.x, ids)) %>%
                         purrr::map(~readr::type_convert(.x)))
    
    time_series$period <- purrr::map2(time_series$period, 
                                      doc_result_ts_timeinterval, 
                                      ~dplyr::bind_cols(.x, .y))
    
  }
  
  ###############################################
  # Points
  ##############################################
  if(length(html_point[[1]]) > 0){
    ids <- c("position", 
             "quantity",
             "secondaryQuantity")
    ids <- tolower(ids)
    
    point_get <- 
      . %>% 
      purrr::map(~id_extractor(.x, ids)) %>% 
      dplyr::bind_rows(.)
    
    doc_result_ts_ps_p <- 
      suppressMessages(html_point %>%
                         purrr::map(point_get) %>%
                         purrr::map(~dplyr::bind_rows(.x) %>% readr::type_convert()))
    
    time_series$period <- lapply(seq_along(time_series$period), 
                                 function(x){
                                   point_data <- time_series$period[[x]] 
                                   point_data$point <- doc_result_ts_ps_p[x]
                                   
                                   point_data})
    
  }
  
  ##########################################
  # extract mktpsrtype
  #############################################
  
  if(length(html_mktpsrtype[[1]]) > 0){
    #### not working on 4.4.3
    ids <- c("psrType", 
             "voltage_PowerSystemResources.highVoltageLimit")
    ids <- tolower(ids)
    
    time_series$mkt_psr_type <- 
      suppressMessages(html_ts %>% 
                         purrr::map(~rvest::html_nodes(.x, "mktpsrtype")) %>%
                         purrr::map(~id_extractor(.x, ids) %>% readr::type_convert(.)))
    
  }
  
  ##########################################
  # extract powersystem resources
  #############################################
  if(length(html_mktgeneratingunit[[1]]) > 0){
    
    ids <- c("mRID", "name", "nominalIP")
    ids <- tolower(ids)
    
    mkt_generating_unit <- 
      suppressMessages(html_ts %>% 
                         purrr::map(~rvest::html_nodes(.x, "mktpsrtype")) %>%
                         purrr::map(~rvest::html_nodes(.x, "MktGeneratingUnit")) %>%
                         purrr::map(~id_extractor(.x, ids) %>% readr::type_convert()))
    
    time_series$mkt_psr_type <- lapply(seq_along(time_series$mkt_psr_type), 
                                       function(x){
                                         psr_data <- time_series$mkt_psr_type[[x]]
                                         
                                         psr_data$mkt_generating_unit <- mkt_generating_unit[x]
                                         
                                         psr_data
                                       })
  }
  
  # assign the time series to the doc result to keep the document structure.
  doc_result$time_series <- list(time_series)
  tms<-time_series
  
   doc_result
  
  if(substr(time_series$period[[1]]$resolution,3,4)=="15"){
   tm1<-sapply(seq(1,length(time_series$period)/2,1),function(i){
     tres<-paste(substr(time_series$period[[i]]$resolution,3,4),"mins")
     times<-seq.POSIXt(as.POSIXct(time_series$period[[i]]$start),as.POSIXct(time_series$period[[i]]$end),by=tres)[-1]
     vals<-c(time_series$period[[i]]$point)
     #dat<-as_tibble(data.frame(time,vals))

   })

   dates<-as.vector(unlist(sapply(seq(1,length(time_series$period)/2,1),function(i){
     tres<-paste(substr(time_series$period[[i]]$resolution,3,4),"mins")
     times<-seq.POSIXt(as.POSIXct(time_series$period[[i]]$start),as.POSIXct(time_series$period[[i]]$end),by=tres)[-1]
   })))}else{

   tm1<-sapply(seq(1,length(time_series$period),1),function(i){
     tres<-paste(substr(time_series$period[[i]]$resolution,3,4),"mins")
     times<-seq.POSIXt(as.POSIXct(time_series$period[[i]]$start),as.POSIXct(time_series$period[[i]]$end),by=tres)[-1]
     vals<-c(time_series$period[[i]]$point)
     })

   dates<-as.vector(unlist(sapply(seq(1,length(time_series$period),1),function(i){
     tres<-paste(substr(time_series$period[[i]]$resolution,3,4),"mins")
     times<-seq.POSIXt(as.POSIXct(time_series$period[[i]]$start),as.POSIXct(time_series$period[[i]]$end),by=tres)[-1]
   })))}



   tm2<-unlist(sapply(1:length(tm1),function(i){
     c(tm1[[i]]$quantity)
   }))

   tm<-data.frame(dates,tm2)
   return(tm)
}
