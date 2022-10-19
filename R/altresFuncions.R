#' @title                    Seleccionem les variables
#' @description              Seleccionem les variables a partir d'un conductor
#' @param  taula             Camp a on hi han les variables
#' @param  taulavariables    Conductor
#' @param  dt                Base de dades
#' @return                   Base de dades amb les variables seleccionades
#' @export                   selectorvariables
#' @importFrom dplyr "%>%"
#' @examples

#'k<-selectorvariables(taula="taula1",taulavariables=conductor1,dt=dt_plana)
#'dt_plana
#'k
selectorvariables<-function(taula="table1",
                            taulavariables="variables_R.xls",
                            dt="dadestotal") {

  # taula = "dades_imputacio2"
  # taulavariables="variables_v2.xls"
  # dt=dades_test

  vector_variables<-extreure.variables(taula=taula,taulavariables = taulavariables)

  # Selecciono les que no existeixen en DT
  variables.no.existeixen<-vector_variables[!is.element(vector_variables,names(dt))]

  # Elimino les que no existeixen
  vector_variables<-vector_variables[is.element(vector_variables,names(dt))]
  moco<-dt %>% dplyr::select_at(vector_variables)

  message(paste0("Llista de variables que no existeixen en el dataset:",paste0(variables.no.existeixen ,collapse = ", ")))

  moco


}


#' @title                       Extreure variables seleccionades
#' @description                 Extreure variables seleccionades a partir d un conductor
#' @param  taula                Camp a on hi han les variables
#' @param  taulavariables       Conductor
#' @param  variable_camp        Variable camp del Conductor
#' @param  dt                   Base de dades
#' @param                       ... altres funcions
#' @return                      un vectora amb les variables seleccionades
#' @export                      extreure.variables
#' @importFrom                  dplyr "%>%"
#' @examples
#'
#'k1<-extreure.variables(taula="taula1",taulavariables=conductor1,variable_camp="camp",dt=NA)
#'k2<-extreure.variables(taula="taula1",taulavariables=conductor1,variable_camp="camp",dt=dt_plana)
#'k1
#'k2
#'
#' @title                       Extreure variables seleccionades
#' @description                 Extreure variables seleccionades a partir d un conductor
#' @param  taula                Camp a on hi han les variables
#' @param  taulavariables       Conductor
#' @param  variable_camp        Variable camp del Conductor
#' @param  dt                   Base de dades
#' @param                       ... altres funcions
#' @return                      un vectora amb les variables seleccionades
#' @export                      extreure.variables
#' @importFrom                  dplyr "%>%"
#' @examples
#'
#'k1<-extreure.variables(taula="taula1",taulavariables=conductor1,variable_camp="camp",dt=NA)
#'k2<-extreure.variables(taula="taula1",taulavariables=conductor1,variable_camp="camp",dt=dt_plana)
#'k1
#'k2
#'
extreure.variables<-function(taula="table1",
                             taulavariables="variables_R.xls",
                             variable_camp="camp",
                             dt=NA,...) {

  # taula="dates_excel"
  # taulavariables = conductor_variables
  # variable_camp="camp"
  # dt=dades

  ####  Llegir etiquetes i variables a analitzar ####
  # variables <- readxl::read_excel(taulavariables) %>% tidyr::as_tibble() %>% dplyr::select(!!variable_camp,!!taula)
  variables <- read_conductor(taulavariables,...) %>% dplyr::select(!!variable_camp,!!taula)
  taula_sym<-rlang::sym(taula)
  variables<-variables %>% dplyr::filter(!is.na(!!taula_sym))

  # Verificar si columnes del conductor estan en dt
  if (is.data.frame(dt)) {
    vars_not_dt<-variables %>% dplyr::anti_join(names(dt) %>% tibble::as_tibble(camp=value),by=c("camp"="value")) %>% dplyr::pull("camp")
    variables<-variables %>%dplyr:: semi_join(names(dt) %>%tibble:: as_tibble(camp=value),by=c("camp"="value"))
    paste0("Variables not in data: ",paste0(vars_not_dt,collapse = ", "), ". So, it is not included in formula") %>% warning()
  }

  # filtratge
  kk<-variables %>% dplyr::arrange(!!taula_sym) %>% dplyr::filter(!!taula_sym>0) %>% dplyr::select(!!variable_camp) %>% as.vector()
  kk<-as.vector(kk[[1]])
  purrr::set_names(kk,kk)

}
