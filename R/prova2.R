##### Funcio aparemmanenta match_density_incidence()

### Algoritme buscar parelles

buscar_parelles_dtindex<-function(dt, eventcontrol,reemplacement,Ncontrols) {

  # dt=dt_temporal
  # eventcontrol=F
  # reemplacement=F
  # Ncontrols<-2
  # Events potencials i potencials controls
  dt_events<-dt %>% filter(event==1) %>% select(idp,dtindex_case)

  if (eventcontrol==F) {dt_controls<-dt %>% filter(event==0) %>% transmute(idp,dtindex_control=as.numeric(dtindex_control))
  } else {dt_controls<-dt %>% transmute(idp,dtindex_control=as.numeric(dtindex_control))}

  # Generar data frame amb dates dels casos on ha de buscar controls
  dt_dates<-dt_events %>% group_by(dtindex_case) %>% summarise(nCIPS=n(),.groups = 'drop') %>% ungroup() %>% mutate(nCIPS=nCIPS*Ncontrols)

  ## Inici bucle
  nvoltes<-n_distinct(dt_dates$dtindex_case)

  datalist = list()

  for (i in 1:nvoltes) {

    # i<-1
    # per cada data selecciono N controls i actualitzo data frame
    # Si es sense reemplaçament s'extreu i s'actualitza data frame

    # Filtre: Tots els potencials controls han de ser posteriors a data cas
    dt_controls<-dt_controls %>% filter(dtindex_control>dt_dates$dtindex_case[i])

    # si hi ha suficients potencials controls a mostrejar --> n controls per data de cas / sinó el mumero màxim
    # mostreig
    num_controls_disponibles<-n_distinct(dt_controls$idp)
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
    dt_control_mostra<-tibble(dtindex=dt_dates$dtindex_case[i],idp=idp)

    # dt_parells_controls<-bind_rows(dt_parells_controls,dt_control_mostra)
    datalist[[i]]<-dt_control_mostra

    # si es sense reemplaçament s'ha eliminar control de dt_controls (pull de controls disponibles)
    if (reemplacement==F) {dt_controls<-dt_controls %>% anti_join(dt_control_mostra, by="idp")}

    # print(i)

  }

  # Genero case.id i elimino blancs

  # close(pb)

  dt_parells_controls <- dplyr::bind_rows(datalist)

  # generar caseid vinculat a data index i nombre de controls per cas

  # dt_parells_controls %>% mutate(.caseid=rep(seq(1,n()), each=Ncontrols))
  # %>% filter(idp!="")

}

# porca<-buscar_parelles_dtindex(dt_poblacio,F,F)
# porca
# buscar_parelles_dtindex(dt_temporal,F,F)


selecciona_parells<-function(dt=dt_poblacio,eventcontrol=T,reemplacement=F,Ncontrols=1) {

  # dt=lola[[5]]
  # dt=dt_match[[1]]
  # eventcontrol=F
  # reemplacement=F

  # lola[[5]] %>% transmute(event,dtindex_case,as.numeric(dtindex_control))

  # selecciona_parells(dt,eventcontrol,reemplacement,Ncontrols)

  # previ: Si no hi ha controls suficients
  datamincas<-dt %>% filter(event==1) %>% pull(dtindex_case) %>% base::min()
  dt <- dt %>% filter(event==1 | (event==0 & as.numeric(dtindex_control)>datamincas))
  datamaxcontrol<-dt %>% filter(event==0) %>% pull(dtindex_control) %>% as.numeric() %>% base::max() %>%
    suppressWarnings()
  Exist_controls<- (datamaxcontrol>datamincas)


  if (Exist_controls) {


    dt_parells_controls<-buscar_parelles_dtindex(dt,eventcontrol,reemplacement,Ncontrols)

    # Ara preparar i fusionar amb events
    dt_parells_controls<-dt_parells_controls %>%
      transmute(idp,dtindex_case=dtindex,.event=0)

    # Genero .caseid controls
    dt_parells_controls<-dt_parells_controls %>%
      arrange(dtindex_case) %>% mutate(.caseid=rep(seq(1,n()/Ncontrols), each=Ncontrols)) %>%
      filter(idp!="") # elimino idps controls no trobats

    # Genero .caseid events (num correlatiu)
    dt_events<-dt %>% filter(event==1) %>%
      select(idp,dtindex_case,.event=event) %>% arrange(dtindex_case) %>% mutate(.caseid=1:n())

    # fusió i generar .caseid
    dt_events %>% bind_rows(dt_parells_controls)


  } else NULL


}



match_density_incidence<-function(dt=dt_poblacio,id="idp",llistaPS=c("sexe"),eventcontrol=F,reemplacement=F,numcores=NA,Ncontrols=1, seed=123) {

  set.seed(seed)

  # dt=dt_poblacio %>% sample_n(1000) %>% mutate(CIP=idp)
  # dt=dt_temporal
  # llistaPS=c("sexe")
  # eventcontrol=F
  # reemplacement=F
  # llistaPS<-c("year_naix","year_DM2")
  # numcores=NA
  # Ncontrols=4
  # Copia de dades original

  dt_origen<-dt %>% mutate(idp=idp)

  # seleccionar dades minimes
  dt<-
    dt %>% select(idp=idp,event,dtindex_case,dtindex_control,llistaPS) %>%
    dplyr::mutate(dtindex_case=as.numeric(dtindex_case),dtindex_control=as.numeric(dtindex_control))

  # suppressWarnings(suppressMessages(library(dplyr)))

  library(furrr)
  if (is.na(numcores)) {numcores<-(parallel::detectCores()) - 1}
  plan(multisession, workers = numcores)

  # 1. Generar variable split
  dt<-dt %>% tidyr::unite(vargrup,llistaPS,sep = "_",remove = FALSE)

  # 2. Eliminar combinacions que no tenen possibilitats de casos ni controls
  eliminar_grups<-dt %>% group_by(vargrup) %>% summarise(Nevents=sum(event==1),NControls=sum(event==0),.groups = 'drop') %>%
    mutate(eliminar=if_else(Nevents<1 | NControls<1,1,0)) %>% transmute(vargrup,eliminar) %>% ungroup() %>%
    filter(eliminar==1)

  dt<-dt %>% anti_join(eliminar_grups,by="vargrup")

  if(nrow(dt) == 0) {stop("There are not enough observations")}

  # 3. Inici d'aparellament

  # Split dades per variables d'aparellament
  dt_match<-dt %>% split(.$vargrup,drop = T) %>%
    furrr::future_map_dfr(~selecciona_parells(.x,eventcontrol,reemplacement,Ncontrols),.options=furrr_options(scheduling=4,seed=T),.id="grup",.progress = T)

  # 4. Reenumerar  caseid en funció del grup
  dt_temp<-dt_match %>% distinct(.caseid,grup) %>%
    mutate(.caseidnew=1:n())
  dt_match<-dt_match %>% left_join(dt_temp,by = c(".caseid","grup")) %>%
    select(-c(grup,.caseid)) %>%
    rename(.caseid=.caseidnew)

  # 5. fusionar amb dades originals
  dt_match<-dt_match %>% left_join(select(dt,-c(dtindex_case,vargrup)),by="idp")

  # 6. Elimino els que no troba cap parella
  dt_match<-
    dt_match %>%  filter(idp!="") %>%
    group_by(.caseid) %>% mutate(.n=n()-1) %>% ungroup() %>%
    filter(.n>=1) %>% arrange(.caseid)

  # 7. fusiono amb dades origen
  dt_match %>% select(idp,.caseid,.dtindex=dtindex_case,.event,.n) %>% left_join(dt_origen,by="idp")


}


# ############################         Test
 dt_temporal<-dt_poblacio %>% sample_n(500)
 llistaPS<-c("sexe")
 llistaPS<-c("sexe","year_DM2")
 llistaPS<-c("year_DM2")
 llistaPS<-c("sexe")
 llistaPS<-c("sexe","year_DM2")
 llistaPS<-c("sexe","year_DM2","year_naix")
 llistaPS<-c("sexe","year_naix")
 llistaPS<-c("year_DM2","year_naix")
 llistaPS<-c("year_naix","year_DM2")
 llistaPS<-c("sexe","year_naix")
 dt_temporal<-dt_poblacio %>%  sample_n(500)
 llistaPS<-c("sexe")
 llistaPS<-c("sexe","year_DM2","year_naix")
 dt_poblacio %>% match_density_incidence(llistaPS,eventcontrol = F,reemplacement=F)
#
# ####  aparellar
 start_time <- Sys.time()
 dt_aparellada2<-match_density_incidence(dt_poblacio,llistaPS,eventcontrol = F,reemplacement=F,Ncontrols = 1)
 end_time <- Sys.time()
 end_time - start_time
#
# ####
 table(dt_aparellada2$.event)
 table(dt_aparellada2$event,dt_aparellada2$.event)
 n_distinct(dt_aparellada2$idp)
 n_distinct(dt_aparellada2$.caseid)
#
#
# # verificar coses (eventcontrol=F)
 dt_aparellada2$idp %>% n_distinct()
 dt_aparellada2$sexe %>% n_distinct()
#
# # genero formula per taula
 formu<-paste0("~") %>% paste0(llistaPS %>% paste0(collapse = "+")) %>% paste0("| .event") %>% as.formula()
#
# # Descriptiu
 table1::table1(formu,data=dt_aparellada2)
#
 dt_aparellada2 %>% arrange(.caseid) %>% filter(.n<4)
#
 dt_temp<-dt_aparellada2 %>% filter(.n==4)
#
# # Descriptiu
 table1::table1(formu,data=dt_temp)
#


#
 df <- data.frame(Date=as.Date(character()),
                  File=character(),
                  User=character(),
                  stringsAsFactors=FALSE)


