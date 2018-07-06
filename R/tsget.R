#' Splits request in one-year chunks for automated download 
#' @description Wrapper function for load_get_ts and generation_get_ts
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
#' @param dateseq
#' @param type data type (generation, load)
#' @param securityToken
#' @export


tsget<-function(type,dateseq,documentType, processType, psrType, in_Domain, outBiddingZone_Domain = "10YCZ-CEPS-----N", securityToken)
{

if(is.null(securityToken)){stop("needs security token")}
  
if(type=="load"){
  if(is.null(documentType)||is.null(processType)||is.null(dateseq)||is.null(outBiddingZone_Domain)){ stop("needs documentType, processType, outBiddingZone_Domain and time period information")}
  else{
      uniy<-unique(year(dateseq))  
      datelist<-data.frame(matrix(sapply(1:length(uniy),function(i){
      dtsq<-dateseq[year(dateseq)==uniy[i]]
      c(substr(gsub("-|:| ","",as.character(dtsq[1]-(1*3600))),1,12),substr(gsub("-|:| ","",as.character(dtsq[length(dtsq)]+(23*3600))),1,12))
      }),ncol=2,byrow=TRUE))

      names(datelist)<-c("s","e")

      loadts<-data.frame(matrix(unlist(sapply(1:length(datelist$s),function(i){
         
         load_get_ts(documentType = documentType, 
         processType = processType, 
         periodStart = datelist$s[i], 
         periodEnd = datelist$e[i], 
         outBiddingZone_Domain = outBiddingZone_Domain,
         securityToken = ENTSOE_PAT)
      })),ncol=2))
      names(loadts)<-c("dates","load")
      loadts$dates<-as.POSIXct(loadts$dates,origin = origin)
  }
 return(as_tibble(loadts))
} 

if(type=="generation"){
  
  if(is.null(documentType)||is.null(processType)||is.null(psrType)||is.null(in_Domain)||is.null(dateseq)){ stop("needs documentType, processType, psrType, in_Domain and time period information")}
  else{
    
    uniy<-unique(year(dateseq))  
    datelist<-data.frame(matrix(sapply(1:length(uniy),function(i){
    dtsq<-dateseq[year(dateseq)==uniy[i]]
    c(substr(gsub("-|:| ","",as.character(dtsq[1]-3600)),1,12),substr(gsub("-|:| ","",as.character(dtsq[length(dtsq)]+(23*3600))),1,12))
    }),ncol=2,byrow=TRUE))

    names(datelist)<-c("s","e")  
    generationts<- ldply(lapply(1:length(datelist$s),function(i){
      
               wg<-generation_get_ts(documentType = "A74",
               processType = processType,
               psrType = psrType,
               in_Domain = in_Domain,
               periodStart = datelist$s[i],
               periodEnd = datelist$e[i],
               securityToken = ENTSOE_PAT)

}),data.frame)
   names(generationts)<-c("dates","generation")
   generationts$dates<-as.POSIXct(generationts$dates,origin = origin)
  }
  return(as_tibble(generationts))
} 
  
    
}

