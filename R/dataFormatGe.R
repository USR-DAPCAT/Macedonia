#' @title              Data Random entre dues dates 
#' @description        Data Random entre dues dates (dataini = 20120101, datafi = 20121231) 
#' @param  dataini     Data inicial
#' @param  datafi      Data final
#' @return             Una data random tiupus "2012-10-23 20:57:28 CEST"
#' @export             data.random
#' @examples
#' data<-data.random(20120101,20121231)
#' data
data.random <- function(dataini=20120101, datafi=20121231) {
  
  # dataini=20120101
  # datafi=20161231
  
  dataini <- as.POSIXct(lubridate::ymd(dataini))
  datafi <- as.POSIXct(lubridate::ymd(datafi))
  temps <- as.numeric(difftime(datafi,dataini,units ="secs"))
  
  # Genera Data sumant temps random a dataini
  rt <- dataini + stats::runif(1, 0, temps)
}


#' @title                Retorna una data a STRING
#' @description          Retorna una data: "27-09-2022"  a STRING 
#' @param  data          una data   tipus  "27-09-2022" 
#' @return               Data a STRING 
#' @export               data.to.string
#' @examples
#' A<-"27-09-2022"
#' B<-data.to.string(A)
#' B
data.to.string<-function(data) {
  
  data.string=paste0(lubridate::year(data),
                     stringr::str_pad(lubridate::month(data),2,"left","0"),
                     stringr::str_pad(lubridate::day(data),2,"left","0"))
  
}





#' @title               Data R Lubridate a partir de data UTC 
#' @description         Data R Lubridate a partir de data UTC 
#' @param  x            Variablesde data
#' @param  dt           Base de dades
#' @return              Base de dades amb Data R Lubridate
#' @export              dataUTC_to_Rdata
#' @importFrom          dplyr "%>%"
#' @examples
#'
#'dt_plana
#'x=c("dtindex","dnaix","entrada","sortida", "INCLUSIO.DM2")
#'dataUTC_to_Rdata(x,dt_plana)
dataUTC_to_Rdata<-function(x,dt) {
  
  #dt<-dt_plana
  #x=c("dtindex","dnaix","entrada","sortida", "INCLUSIO.DM2")
  # dt<-dades
  # x=c("data_inici_HD","ANT1_ARTER_PERI","ANT2_ARTE_PERI","ANT1_CI")
  
  # Seleccionar nom del camp si es tipo caracter 
  vector_caracter<-dt %>% dplyr::select_if(~!any(class(.)!="character",na.rm=F)) %>% names()
  
  # Vectors de variables UTC (data POSIXct)
  x_UTC<-x [!x %in% vector_caracter]
  
  # Vector de variables caracter ("37712")
  x_text<-x [x %in% vector_caracter]
  
  # Funcio que converteix UTC data a date ymd
  data_convert_UTC<-function(x){
    x<-format(as.POSIXct(x, origin='1970-01-01'), format='%Y/%m/%d')
    x<-lubridate::ymd(x)}
  
  # Funcio que converteix data caracter ("37712) a date ymd () "2003-04-01"
  data_convert_text<-function(x){
    x<-as.Date(as.numeric(x), origin = "1899-12-30") %>% 
      lubridate::ymd()}
  
  # Aplicar conversions als dos tipos de dates
  dt<-dt %>% purrr::modify_at(x_UTC,~data_convert_UTC(.x))   # UTC ->date
  dt<-dt %>% purrr::modify_at(x_text,~data_convert_text(.x))   # text->date
  
  dt
  
}


#' @title                 Converteix data caracter () a una data ymd
#' @description           Converteix data caracter ("37712") a date ymd () "2003-04-01"
#' @param  x              Data caracter
#' @return                Data ymd
#' @export                data_convert_text
#' @importFrom            dplyr "%>%"
#' @examples
#' k<-data_convert_text("37712")
#' k
data_convert_text<-function(x){
  x<-as.Date(as.numeric(x), origin = "1899-12-30") %>% 
    lubridate::ymd()}

 

#' @title                Funcio que converteix de una data numeric (15784) a Date
#' @description          Funcio que converteix de numeric (15784) a Date "2013-03-20" 
#' @param  x             Data numeric
#' @return               Data "2013-03-20" 
#' @export               data_convert_numeric
#' @importFrom           dplyr "%>%"
#' @examples
#'k<-data_convert_numeric(15784)
#'k
data_convert_numeric<-function(x){ x<-as.Date(x, origin = "1970-01-01")}




#' @title               Funcio que converteix UTC data a date ymd
#' @description         Funcio que converteix UTC data a date ymd 
#' @param  x            UTC data
#' @return              Date ymd
#' @export              data_convert_UTC
#' @examples
#'k<-data_convert_UTC("1970-01-01")
#'k
data_convert_UTC<-function(x){
  x<-format(as.POSIXct(x, origin='1970-01-01'), format='%Y/%m/%d')
  x<-lubridate::ymd(x)}




#' @title                  Converteix format TEXT A DATA 
#' @description            Converteix format TEXT A DATA ("19722207"->"1972-02-22")   
#' @param  d               Base de dades
#' @param  taulavariables  Variables que volem canviar
#' @param  campdata        dates
#' @return                 Data
#' @export                 convertir_dates
#' @importFrom             dplyr "%>%"
#' @examples
#'camp=c("idp","dtindex","sexe","dnaix","situacio","entrada",
#'       "sortida", "INCLUSIO.DM2","DG.HTA","DG.IC",
#'       "cHDL.valor","cLDL.valor","cT.valor","GLICADA.valor","IMC.valor","data2")
#'factor=c("","","","","","","",1,1,1,"","","","","",1)
#'dates=c("","","","","","","","","","","","","","","",1)
#'conductor<-data.frame(camp,factor,dates)
#'dt_plana$data2<-"19722207"
#'K<-convertir_dates(d=dt_plana,taulavariables=conductor,campdata="dates")
#'K
convertir_dates<-function(d="dt_plana",taulavariables="conductor",campdata="dates")

  
{
  ####  Llegir etiquetes i variables a analitzar  ##
  #variables <- readxl::read_excel(taulavariables) %>% tidyr::as_tibble() 
  variables <- read_conductor(taulavariables) %>% tidyr::as_tibble()
  
  # variables[is.na(variables)]<- 0
  campdata_sym<-dplyr::sym(campdata)
  variables<-variables %>% dplyr::filter(!is.na(!!campdata_sym))
  
  # etiquetar variables         
  seleccio<-variables
  camp<- as.vector(seleccio$camp) #
  
  for (i in 1:length(camp)){if (seleccio$dates[i]==1) { 
    
    pepito<-paste0("as.Date(d[[camp[",i,"]]], '%Y%d%m')")
    
    d[[camp[i]]]<-eval(parse(text=pepito))
    
  } }
  
  d
  
}





#' @title               Passa data de SPSS a Rdata
#' @description         Passa data de SPSS a Rdata 
#' @param  x            Data de SPSS
#' @return              Data Rdata
#' @export              dataSPSS_to_Rdata
#' @importFrom          dplyr "%>%"
#' @examples
#'k<-dataSPSS_to_Rdata(666)
#'k
dataSPSS_to_Rdata <- function(x) {
  y<-as.Date(x/86400, origin = "1582-10-14") %>% 
    lubridate::ymd() }

