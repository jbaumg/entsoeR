#' This wraps a GET request to the API
#'
#' @description function to download generation data in a "time series" format
#' @param documentType The coded type of the document being sent. The 
#'     document type identifies the principal characteristic of the 
#'     status request. Refer to ENTSO-E 
#'     Core Component Code list document for valid codes. The document 
#'     type value must be exactly 3 alphanumeric characters (no blanks).
#'     This information is mandatory. There's no dependency 
#'     requirements.
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