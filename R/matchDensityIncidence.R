#' @title                     buscar parelles dtindex
#' @description               buscar parelles dtindex
#' @param dt                  dt
#' @param eventcontrol        eventcontrol
#' @param reemplacement       reemplacement
#' @param Ncontrols           Ncontrols
#' @return                    base de dades
#' @export                    buscar_parelles_dtindex
buscar_parelles_dtindex<-function(dt,
                                  eventcontrol,
                                  reemplacement,
                                  Ncontrols) {

  ##### Funcio aparemmanenta match_density_incidence()

  ### Algoritme buscar parelles

  # dt=dt_temporal
  # eventcontrol=F
  # reemplacement=F
  # Ncontrols<-2
  # Events potencials i potencials controls
  dt_events<-dt %>%dplyr::filter(event==1) %>% dplyr::select(idp,dtindex_case)

  if (eventcontrol==F) {dt_controls<-dt %>%dplyr:: filter(event==0) %>%dplyr:: transmute(idp,dtindex_control=as.numeric(dtindex_control))
  } else {dt_controls<-dt %>% dplyr::transmute(idp,dtindex_control=as.numeric(dtindex_control))}

  # Generar data frame amb dates dels casos on ha de buscar controls
  dt_dates<-dt_events %>% dplyr::group_by(dtindex_case) %>% dplyr::summarise(nCIPS=dplyr::n(),.groups = 'drop') %>%dplyr:: ungroup() %>% dplyr::mutate(nCIPS=nCIPS*Ncontrols)

  ## Inici bucle
  nvoltes<-dplyr::n_distinct(dt_dates$dtindex_case)

  datalist = list()

  for (i in 1:nvoltes) {

    # i<-1
    # per cada data selecciono N controls i actualitzo data frame
    # Si es sense reemplaçament s'extreu i s'actualitza data frame

    # Filtre: Tots els potencials controls han de ser posteriors a data cas
    dt_controls<-dt_controls %>% dplyr::filter(dtindex_control>dt_dates$dtindex_case[i])

    # si hi ha suficients potencials controls a mostrejar --> n controls per data de cas / sinó el mumero màxim
    # mostreig
    num_controls_disponibles<-dplyr::n_distinct(dt_controls$idp)
    # n controls per cas
    n<-(dt_dates$nCIPS[i] %>% as.numeric())   # Num de controls en cada data que ha de mostrejar

    n_sample<-base::min(c(n,num_controls_disponibles),na.rm = T) # Si no hi ha suficients controls, la n de controls a buscar serà el màxim disponible

    # Selecció de controls
    if (num_controls_disponibles>=n) {idp <-sample(dt_controls$idp,n)
    } else if (num_controls_disponibles>=1) {
      idp <-sample(dt_controls$idp,n_sample)  # Si n'hi com a minim 1 mostreja els que tinguis
      idp<-c(idp,rep("",n-n_sample))          # i reemplena amb blancs/0 la resta
    } else {idp<-c(rep("",n-n_sample))}       # Sino genera vector amb espais en blanc/0's inventats

    # Actualitzo sobre tibble de controls
    dt_control_mostra<-tibble::tibble(dtindex=dt_dates$dtindex_case[i],idp=idp)

    # dt_parells_controls<-dplyr::bind_rows(dt_parells_controls,dt_control_mostra)
    datalist[[i]]<-dt_control_mostra

    # si es sense reemplaçament s'ha eliminar control de dt_controls (pull de controls disponibles)
    if (reemplacement==F) {dt_controls<-dt_controls %>%dplyr:: anti_join(dt_control_mostra, by="idp")}

    # print(i)

  }

  # Genero case.id i elimino blancs

  # close(pb)

  dt_parells_controls <- dplyr::bind_rows(datalist)

  # generar caseid vinculat a data index i nombre de controls per cas

  # dt_parells_controls %>% dplyr::mutate(.caseid=rep(seq(1,dplyr::n()), each=Ncontrols))
  # %>% dplyr::filter(idp!="")

}



#' @title                     selecciona parells
#' @description               selecciona parells
#' @param dt                  dt
#' @param eventcontrol        eventcontrol
#' @param reemplacement       reemplacement
#' @param Ncontrols           Ncontrols
#' @return                    parelles
#' @export                    selecciona_parells
selecciona_parells<-function(dt="dt_poblacio",
                             eventcontrol=T,
                             reemplacement=F,
                             Ncontrols=1) {

  # porca<-buscar_parelles_dtindex(dt_poblacio,F,F)
  # porca
  # buscar_parelles_dtindex(dt_temporal,F,F)

  # dt=lola[[5]]
  # dt=dt_match[[1]]
  # eventcontrol=F
  # reemplacement=F

  # lola[[5]] %>%dplyr:: transmute(event,dtindex_case,as.numeric(dtindex_control))

  # selecciona_parells(dt,eventcontrol,reemplacement,Ncontrols)

  # previ: Si no hi ha controls suficients
  datamincas<-dt %>% dplyr::filter(event==1) %>% dplyr::pull(dtindex_case) %>% base::min()
  dt <- dt %>% dplyr::filter(event==1 | (event==0 & as.numeric(dtindex_control)>datamincas))
  datamaxcontrol<-dt %>% dplyr::filter(event==0) %>% dplyr::pull(dtindex_control) %>% as.numeric() %>% base::max() %>%
    suppressWarnings()
  Exist_controls<- (datamaxcontrol>datamincas)


  if (Exist_controls) {


    dt_parells_controls<-buscar_parelles_dtindex(dt,eventcontrol,reemplacement,Ncontrols)

    # Ara preparar i fusionar amb events
    dt_parells_controls<-dt_parells_controls %>%
      dplyr::transmute(idp,dtindex_case=dtindex,.event=0)

    # Genero .caseid controls
    dt_parells_controls<-dt_parells_controls %>%
      dplyr::arrange(dtindex_case) %>%dplyr::mutate(.caseid=rep(seq(1,dplyr::n()/Ncontrols), each=Ncontrols)) %>%
      dplyr::filter(idp!="") # elimino idps controls no trobats

    # Genero .caseid events (num correlatiu)
    dt_events<-dt %>% dplyr::filter(event==1) %>%
      dplyr::select(idp,dtindex_case,.event=event) %>% dplyr::arrange(dtindex_case) %>%dplyr:: mutate(.caseid=1:dplyr::n())

    # fusió i generar .caseid
    dt_events %>%dplyr:: bind_rows(dt_parells_controls)


  } else NULL


}


#' @title                     match density incidence
#' @description               match density incidence
#' @param dt                  dt
#' @param id                  id
#' @param llistaPS            llistaPS
#' @param eventcontrol        eventcontrol
#' @param reemplacement       reemplacement
#' @param numcores            numcores
#' @param Ncontrols           Ncontrols
#' @param seed                seed
#' @return                    match
#' @export                    match_density_incidence
#' @examples
#'
#'
#'dat
#'
#'dat2<-match_density_incidence(dt=dat,
#'id="idp",llistaPS=c("sex"),
#'eventcontrol=FALSE,
#'reemplacement=FALSE,
#'numcores=NA,
#'Ncontrols=1,
#'seed=123)
#'
#'dat2
match_density_incidence<-function(dt="dt_poblacio",
                                  id="idp",
                                  llistaPS=c("sexe"),
                                  eventcontrol=F,
                                  reemplacement=F,
                                  numcores=NA,
                                  Ncontrols=1,
                                  seed=123) {

  # ############################         Test
  # dt_temporal<-dt_poblacio %>% sample_n(500)
  # llistaPS<-c("sexe")
  # llistaPS<-c("sexe","year_DM2")
  # llistaPS<-c("year_DM2")
  # llistaPS<-c("sexe")
  # llistaPS<-c("sexe","year_DM2")
  # llistaPS<-c("sexe","year_DM2","year_naix")
  # llistaPS<-c("sexe","year_naix")
  # llistaPS<-c("year_DM2","year_naix")
  # llistaPS<-c("year_naix","year_DM2")
  # llistaPS<-c("sexe","year_naix")
  # dt_temporal<-dt_poblacio %>%  sample_n(500)
  # llistaPS<-c("sexe")
  # llistaPS<-c("sexe","year_DM2","year_naix")
  # # dt_poblacio %>% match_density_incidence(llistaPS,eventcontrol = F,reemplacement=F)
  #
  # ####  aparellar
  # start_time <- Sys.time()
  # dt_aparellada2<-match_density_incidence(dt_poblacio,llistaPS,eventcontrol = F,reemplacement=F,Ncontrols = 1)
  # end_time <- Sys.time()
  # end_time - start_time
  #
  # ####
  # table(dt_aparellada2$.event)
  # table(dt_aparellada2$event,dt_aparellada2$.event)
  # dplyr::n_distinct(dt_aparellada2$idp)
  # dplyr::n_distinct(dt_aparellada2$.caseid)
  #
  #
  # # verificar coses (eventcontrol=F)
  # dt_aparellada2$idp %>% dplyr::n_distinct()
  # dt_aparellada2$sexe %>% dplyr::n_distinct()
  #
  # # genero formula per taula
  # formu<-paste0("~") %>% paste0(llistaPS %>% paste0(collapse = "+")) %>% paste0("| .event") %>% as.formula()
  #
  # # Descriptiu
  # table1::table1(formu,data=dt_aparellada2)
  #
  # dt_aparellada2 %>% dplyr::arrange(.caseid) %>% dplyr::filter(.n<4)
  #
  # dt_temp<-dt_aparellada2 %>% dplyr::filter(.n==4)
  #
  # # Descriptiu
  # table1::table1(formu,data=dt_temp)
  #


  #
  # df <- data.frame(Date=as.Date(character()),
  #                  File=character(),
  #                  User=character(),
  #                  stringsAsFactors=FALSE)
  #



  set.seed(seed)

  # dt=dt_poblacio %>% sample_n(1000) %>% dplyr::mutate(CIP=idp)
  # dt=dt_temporal
  # llistaPS=c("sexe")
  # eventcontrol=F
  # reemplacement=F
  # llistaPS<-c("year_naix","year_DM2")
  # numcores=NA
  # Ncontrols=4
  # Copia de dades original

  dt_origen<-dt %>% dplyr::mutate(idp=idp)

  # seleccionar dades minimes
  dt<-
    dt %>% dplyr::select(idp=idp,event,dtindex_case,dtindex_control,llistaPS) %>%
    dplyr::mutate(dtindex_case=as.numeric(dtindex_case),dtindex_control=as.numeric(dtindex_control))

  # suppressWarnings(suppressMessages(library(dplyr)))

  #library(furrr)
  if (is.na(numcores)) {numcores<-(parallel::detectCores()) - 1}
  future::plan(future::multisession, workers = numcores)

  # 1. Generar variable split
  dt<-dt %>% tidyr::unite(vargrup,llistaPS,sep = "_",remove = FALSE)

  # 2. Eliminar combinacions que no tenen possibilitats de casos ni controls
  eliminar_grups<-dt %>% dplyr::group_by(vargrup) %>% dplyr::summarise(Nevents=sum(event==1),NControls=sum(event==0),.groups = 'drop') %>%
    dplyr::mutate(eliminar=dplyr::if_else(Nevents<1 | NControls<1,1,0)) %>%dplyr:: transmute(vargrup,eliminar) %>%dplyr:: ungroup() %>%
    dplyr::filter(eliminar==1)

  dt<-dt %>% dplyr::anti_join(eliminar_grups,by="vargrup")

  if(nrow(dt) == 0) {stop("There are not enough observations")}

  # 3. Inici d'aparellament

  # Split dades per variables d'aparellament
  dt_match<-dt %>% split(.$vargrup,drop = T) %>%
    furrr::future_map_dfr(~selecciona_parells(.x,eventcontrol,reemplacement,Ncontrols),.options=furrr::furrr_options(scheduling=4,seed=T),.id="grup",.progress = T)

  # 4. Reenumerar  caseid en funció del grup
  dt_temp<-dt_match %>% dplyr::distinct(.caseid,grup) %>%
    dplyr::mutate(.caseidnew=1:dplyr::n())
  dt_match<-dt_match %>% dplyr::left_join(dt_temp,by = c(".caseid","grup")) %>%
    dplyr::select(-c(grup,.caseid)) %>%
    dplyr::rename(.caseid=.caseidnew)

  # 5. fusionar amb dades originals
  dt_match<-dt_match %>% dplyr::left_join(dplyr::select(dt,-c(dtindex_case,vargrup)),by="idp")

  # 6. Elimino els que no troba cap parella
  dt_match<-
    dt_match %>%  dplyr::filter(idp!="") %>%
    dplyr::group_by(.caseid) %>% dplyr::mutate(.n=dplyr::n()-1) %>% dplyr::ungroup() %>%
    dplyr::filter(.n>=1) %>% dplyr::arrange(.caseid)

  # 7. fusiono amb dades origen
  dt_match %>% dplyr::select(idp,.caseid,.dtindex=dtindex_case,.event,.n) %>% dplyr::left_join(dt_origen,by="idp")


}




#' @title                     riskSetMatch
#' @description               riskSetMatch
#' @param ptid                ptid
#' @param event               event
#' @param terms               terms
#' @param dat                 dat
#' @param Ncontrols           Ncontrols
#' @param oldevent            oldevent
#' @param caseid              caseid
#' @param reuseCases          reuseCases
#' @param reuseControls       reuseControls
#' @param caseIndex           caseIndex
#' @param controlIndex        controlIndex
#' @param NoIndex             NoIndex
#' @param cores               cores
#' @param dateterms           dateterms
#' @return                    match
#' @export                    riskSetMatch
#' @examples
#'
#'case <- c(rep(0,40),rep(1,15))
#'ptid <- paste0("P",1:55)
#'sex <- c(rep("fem",20),rep("mal",20),rep("fem",8),rep("mal",7))
#'byear <- c(rep(c(2020,2030),20),rep(2020,7),rep(2030,8))
#'case.Index <- c(seq(1,40,1),seq(5,47,3))
#'control.Index <- case.Index
#'diabetes <- seq(2,110,2)
#'heartdis <- seq(110,2,-2)
#'diabetes <- c(rep(1,55))
#'heartdis <- c(rep(100,55))
#'library(data.table)
#'
#'dat <- data.table::data.table(case,ptid,sex,byear,diabetes,heartdis,case.Index,control.Index)
#'# Very simple match without reuse - no dates to control for
#'
#'out <- riskSetMatch("ptid",event="case",c("byear","sex"),dat,2,NoIndex=TRUE)
#'
#'dat
#'out
#'
#'# Risk set matching without reusing cases/controls -
#'# Some cases have no controls
#'out2 <- riskSetMatch("ptid",event="case",c("byear","sex"),dat,2,caseIndex="case.Index",
#'controlIndex="control.Index")
#'
#'dat
#'out2
#'
riskSetMatch<-
function (ptid,
          event,
          terms,
          dat,
          Ncontrols,
          oldevent = "oldevent",
          caseid = "caseid",
          reuseCases = FALSE,
          reuseControls = FALSE,
          caseIndex = NULL,
          controlIndex = NULL,
          NoIndex = FALSE,
          cores = 2,
          dateterms = NULL)
{

  #ptid=ptid
  #event=case
  #terms=c("byear","sex")
  #dat
  #Ncontrols=2
  #oldevent = "oldevent"
  #caseid = "caseid"
  #reuseCases = FALSE
  #reuseControls = FALSE
  #caseIndex = NULL
  #controlIndex = NULL
  #NoIndex = TRUE
  #cores = 1
  #dateterms = NULL

    #githubinstall::githubinstall("heaven",ref = "964bbbd",force = T)

  .SD = Internal.ptid = pnrnum = cterms = Internal.event = Internal.cterms = label = Internal.event = pnrnum = random = .N = Internal.controlIndex = Internal.caseIndex = random = Internal.controlIndex = Internal.caseIndex = NULL

  if (!is.character(ptid) | !is.character(event) | (!is.null(caseIndex) &
                                                    !is.character(caseIndex)) | (!is.null(dateterms) & !is.character(dateterms)) |
      (!is.null(controlIndex) & !is.character(controlIndex)))
    stop(" Variables names must be character")

  data.table::setDT(dat)

  if (!is.integer(cores) & !(is.numeric(cores))) stop("cores must be integer, default 1")

  cores <- as.integer(cores)
  datt <- data.table::copy(dat)
  datt[, `:=`("oldevent", .SD), .SDcols = event]

  if (NoIndex)
    noindex <- 1L
  else noindex <- 0L
  data.table::setnames(datt, ptid, "Internal.ptid")
  repetitians <- length(datt[, Internal.ptid]) - length(unique(datt[,Internal.ptid]))

  if (repetitians > 0)
    stop(paste(" Error, participant ID not unique. Number of repeats:",repetitians))

  datt[, `:=`(pnrnum, 1:.N)]
  datt[, `:=`(cterms, do.call(paste0, .SD)), .SDcols = terms]

  cols <- c("pnrnum", event, "cterms")
  if (!NoIndex) cols <- c("pnrnum", caseIndex, controlIndex, event,"cterms")
  if (!NoIndex & !is.null(dateterms))cols <- c("pnrnum", caseIndex, controlIndex, event,"cterms", dateterms)

  alldata <- datt[, .SD, .SDcols = cols]

  if (!NoIndex) RcaseIndex <- caseIndex
  if (NoIndex)  data.table::setnames(alldata, cols, c("pnrnum", "Internal.event","Internal.cterms"))
  else if (!NoIndex & is.null(dateterms))
    data.table::setnames(alldata, c("pnrnum", "Internal.caseIndex","Internal.controlIndex", "Internal.event","Internal.cterms"))
  else if (!NoIndex & !is.null(dateterms)) {
    Internal.dateterms <- paste0("V", seq(1, length(dateterms)))

    data.table::setnames(alldata, c("pnrnum", "Internal.caseIndex","InternalInternal.controlIndex", "Internal.event","Internal.cterms", Internal.dateterms))

    alldata[, `:=`((Internal.dateterms), lapply(.SD,as.integer)), .SDcols = Internal.dateterms]
  }

  data.table::setkey(alldata, Internal.cterms)

  split.alldata <- split(alldata, by = "Internal.cterms")

  if (cores < 2) {
    totalprogress <- as.numeric(length(split.alldata)/1000)
    pb <- txtProgressBar(min = 0, max = 1000, initial = 0,
                         char = "=", width = NA, graphics::title, label, style = 1,
                         file = "")
    progress <- 0
    selected.controls <- do.call("rbind", lapply(split.alldata,
                                                 function(controls) {
                                                   if (!NoIndex & is.null(dateterms))
                                                     data.table::setnames(controls, c("pnrnum",
                                                                                      "Internal.caseIndex",
                                                                                      "Internal.controlIndex",
                                                                                      "Internal.event",
                                                                                      "Internal.cterms"))

                                                   else if (!NoIndex & !is.null(dateterms))
                                                     data.table::setnames(controls, c("pnrnum",
                                                                                      "Internal.caseIndex",
                                                                                      "Internal.controlIndex",
                                                                                      "Internal.event",
                                                                                      "Internal.cterms", Internal.dateterms))
                                                   else if (NoIndex)
                                                     data.table::setnames(controls,
                                                                          c("pnrnum",
                                                                            "Internal.event",
                                                                            "Internal.cterms"))

                                                   data.table::setkey(controls, Internal.event, pnrnum)
                                                   cases <- controls[Internal.event == 1]

                                                   data.table::setkey(cases, pnrnum)
                                                   if (!reuseCases)
                                                     controls <- subset(controls, Internal.event ==0)

                                                   Tcontrols <- dim(controls)[1]
                                                   Ncases <- dim(cases)[1]
                                                   set.seed(17)

                                                   controls[, `:=`(random, stats::runif(.N, 1, Ncontrols * 10))]

                                                   data.table::setkey(controls, random)
                                                   NreuseControls <- as.numeric(reuseControls)
                                                   if (!is.null(dateterms))
                                                     Ndateterms = length(dateterms)
                                                   else Ndateterms <- 0
                                                   if (!NoIndex) {
                                                     control.date <- controls[, Internal.controlIndex]
                                                     case.date <- cases[, Internal.caseIndex]
                                                   }
                                                   else {
                                                     control.date <- 0L
                                                     case.date <- 0L
                                                   }
                                                   if (!is.null(dateterms)) {
                                                     dates.cases <- as.matrix(cases[, .SD, .SDcols = Internal.dateterms])
                                                     dates.controls <- as.matrix(controls[, .SD,
                                                                                          .SDcols = Internal.dateterms])
                                                   }
                                                   else {
                                                     dates.cases <- as.matrix(0)
                                                     dates.controls <- as.matrix(0)
                                                   }
                                                   Output <- .Call("_heaven_Matcher", PACKAGE = "heaven",
                                                                   Ncontrols, Tcontrols, Ncases, NreuseControls,
                                                                     control.date, case.date, controls[, pnrnum],
                                                                     cases[, pnrnum], Ndateterms, dates.cases, dates.controls,
                                                                     noindex)

                                                   data.table::setDT(Output)

                                                   progress <<- progress + 1/totalprogress
                                                   setTxtProgressBar(pb, progress)
                                                   flush(stdout())
                                                   Output
                                                 }))
  }
  else {
    CLUST <- parallel::makeCluster(min(parallel::detectCores(),
                                       cores))
    selected.controls <- do.call(rbind, foreach::foreach(controls = split.alldata,
                                                         .packages = c("heaven"),
                                                         .export = c("reuseControls")) %dopar%
                                   {
                                     if (!NoIndex & is.null(dateterms))
                                       data.table::setnames(controls, c("pnrnum", "Internal.caseIndex",
                                                            "Internal.controlIndex", "Internal.event",
                                                            "Internal.cterms"))
                                     else if (!NoIndex & !is.null(dateterms))
                                       data.table::setnames(controls, c("pnrnum", "Internal.caseIndex",
                                                            "Internal.controlIndex", "Internal.event",
                                                            "Internal.cterms", Internal.dateterms))
                                     else if (NoIndex)
                                       data.table::setnames(controls, c("pnrnum", "Internal.event",
                                                            "Internal.cterms"))
                                     data.table::setkey(controls, Internal.event, pnrnum)
                                     cases <- controls[Internal.event == 1]
                                     data.table::setkey(cases, pnrnum)
                                     if (!reuseCases)
                                       controls <- subset(controls, Internal.event ==
                                                            0)
                                     Tcontrols <- dim(controls)[1]
                                     Ncases <- dim(cases)[1]
                                     set.seed(17)
                                     controls[, `:=`(random, stats::runif(.N, 1, Ncontrols *
                                                                     10))]
                                     data.table::setkey(controls, random)
                                     NreuseControls <- as.numeric(reuseControls)
                                     if (!is.null(dateterms))
                                       Ndateterms = length(dateterms)
                                     else Ndateterms <- 0
                                     if (!NoIndex) {
                                       control.date <- controls[, Internal.controlIndex]
                                       case.date <- cases[, Internal.caseIndex]
                                     }
                                     else {
                                       control.date <- 0L
                                       case.date <- 0L
                                     }
                                     if (!is.null(dateterms)) {
                                       dates.cases <- as.matrix(cases[, .SD, .SDcols = Internal.dateterms])
                                       dates.controls <- as.matrix(controls[, .SD,
                                                                            .SDcols = Internal.dateterms])
                                     }
                                     else {
                                       dates.cases <- as.matrix(0)
                                       dates.controls <- as.matrix(0)
                                     }
                                     Output <- .Call("_heaven_Matcher", PACKAGE = "heaven",
                                                       Ncontrols, Tcontrols, Ncases, NreuseControls,
                                                       control.date, case.date, controls[, pnrnum],
                                                       cases[, pnrnum], Ndateterms, dates.cases, dates.controls,
                                                      noindex)
                                     data.table::setDT(Output)
                                     Output
                                   })
    parallel::stopCluster(CLUST)
    data.table::setDT(selected.controls)
  }
  data.table::setnames(selected.controls, c(caseid, "pnrnum"))
  selected.controls[, `:=`(Internal.event, 0)]
  data.table::setkey(alldata, Internal.event)
  cases <- alldata[Internal.event == 1]
  cases[, `:=`(caseid, pnrnum)]
  FINAL <- rbind(cases[, list(pnrnum, caseid, Internal.event)],
                 selected.controls[, data.table::data.table(pnrnum, caseid,
                                                            Internal.event)])
  data.table::setkey(FINAL)
  datt[, `:=`((event), NULL)]
  FINAL <- merge(FINAL, datt, by = "pnrnum")
  FINAL[, `:=`(c("cterms", "pnrnum"), NULL)]
  data.table::setnames(FINAL, "Internal.ptid", ptid)
  data.table::setkeyv(FINAL, c(caseid, "Internal.event"))
  if (!NoIndex)
    FINAL[, `:=`(eval(caseIndex), .SD[.N]), .SDcols = c(caseIndex),
          by = caseid]
  data.table::setnames(FINAL, "Internal.event", event)
  FINAL
}


