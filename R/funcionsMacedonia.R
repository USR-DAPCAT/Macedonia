#' @title                     Etiquetar model
#' @description               Etiquetar model
#' @param model               Model
#' @param taulavariables      Conductor
#' @param camp                Camp
#' @param camp_descripcio     Camp DESCRIPCIO
#' @param ...                 Altres parametres
#' @return                    Etiquetar model
#' @export                    etiquetar_model
#' @importFrom                dplyr "%>%"
etiquetar_model<-function(model="kkk",
                          taulavariables="variables_R.xls",
                          camp="camp",
                          camp_descripcio="descripcio",...) {


  #ull mireu aquesta funcio dels conflictes!!!

  #library(conflicted)
  #
  #conflict_scout()
  #Cars93 %>% dplyr::select(Origin)
  #
  #conflict_prefer("select", "dplyr")
  #conflict_scout()
  #Cars93 %>% select(Origin)


  # NUMERO_1)

  # model=model1
  # taulavariables=conductor_variables
  # camp="camp"
  # camp_descripcio="ETIQUETA"

  ####  Llegir etiquetes i variables a analitzar ####
  # variables <- read_conductor(taulavariables)
  variables <- read_conductor(taulavariables,...)
  camp_sym<-dplyr::sym(camp)
  variables<-variables %>% dplyr::filter(!is.na(!!camp_sym)) %>%dplyr:: select(!!camp_sym,!!camp_descripcio)

  # Factoritzar els que son caracter i posar labels
  # covars<-extreure.variables("ajust5_art2",conductor)
  covars<-attr(model$terms, "term.labels")
  factoritzar_funcio<-function(x) {if (is.character(x)) as.factor(x) else {x}}
  dt<-eval(model$call$data)
  dt<-dt %>% dplyr::mutate_at(dplyr::vars(covars), ~factoritzar_funcio(.))

  # construir etiquetes
  taula_etiquetes<-
    attr(model$terms , "term.labels") %>% rlang::set_names() %>%
    purrr::map(~levels(dt[[.x]])) %>%
    tibble::enframe() %>%
    tidyr::unnest(cols = c(value),keep_empty = T) %>%
    dplyr::mutate(value=ifelse(is.na(value),"",value)) %>%
    dplyr::mutate(categoria=paste0(name,value)) %>%
    etiquetar_taula(camp = "name",taulavariables = taulavariables,camp_descripcio = camp_descripcio,...) %>%
    dplyr::mutate(descripcio_nova=ifelse(value!="",paste0(name,": ",value),name)) %>%
    dplyr::select(Parameter=categoria,descripcio_nova) %>%
    dplyr::filter(Parameter%in%names(model$coefficients)) # filtrar pels coefficients que tenim en el model

  # Canviar noms en model
  # names(model$coefficients)<-taula_etiquetes$descripcio_nova
  names(model$coefficients)[names(model$coefficients)%in%taula_etiquetes$Parameter] <-taula_etiquetes$descripcio_nova

  model

}



#' @title                     Formula a partir de variables
#' @description               Formula a partir de variables
#' @param x                   x
#' @param y                   y
#' @param eliminar            eliminar
#' @return                    FORMULA
#' @export                    formula
#' @importFrom                dplyr "%>%"
formula<-function(x="taula1",
                 y="grup",
                 eliminar=c("idp","y")) {
  # NUMERO_2)

  pepito<-paste("as.vector(variables[variables$",x,"==1,]$camp)[!as.vector(variables[variables$",x,"==1,]$camp)%in%eliminar]",sep="")
  llistataula<-eval(parse(text=pepito))
  y<-stats::as.formula(paste(y, paste(llistataula, collapse=" + "), sep=" ~ "))
}




#' @title                     Formula a partir de variables
#' @description               Formula a partir de variables:compte l'Ordre que està posada en el conductor taulavariables
#' @param x                   x
#' @param y                   y
#' @param elimina             elimina
#' @param taulavariables      taulavariables
#' @param dt                  dt
#' @return                    FORMULA
#' @export                    formula_compare
#' @importFrom                dplyr "%>%"
#' @param ...                 Altres parametres
formula_compare<-function(x="taula1",
                          y="grup",
                          elimina=c("IDP"),
                          taulavariables="variables_R.xls",
                          dt="No",...) {
  # NUMERO_3)

  # x="table5"
  # y="grup"
  # taulavariables =conductor_variables
  # elimina=c("IDP")
  # dt=dades

  # 1. Llegir conductor analisis
  # variables <- readxl::read_excel(taulavariables) %>% tidyr::as_tibble()
  variables <- read_conductor(taulavariables,...) %>% tidyr::as_tibble()


  # 2. DATA table filtrar ordenar llista de camps
  polio<-data.table::data.table(variables)

  x<-dplyr::sym(x)

  mua<-polio[camp!=elimina] %>%
    dplyr::filter(!!x>0) %>%
    dplyr::arrange(!!x) %>%
    dplyr::select(camp) %>% as.vector()

  # 1.2. Filtrar per variables que realment existeixen en la base de dades

  if (is.data.frame(dt)) {mua<-mua %>% dplyr::semi_join(data.frame(camp=names(dades)),by="camp")}

  # 3. Generar formula

  # y<-stats::as.formula(paste(y, paste(llista$camp, collapse=" + "), sep=" ~ "))

  y<-stats::as.formula(paste(y, paste(mua$camp, collapse=" + "), sep=" ~ "))

  y

}



#' @title                     Llistat de taules a partir de Llista de factors de Y's
#' @description               Llistat de taules a partir de Llista de factors de Y's i em retorna una llista de taules
#' @param dt                  dt
#' @param llista.y            llista.y
#' @param llista.x            llista.x
#' @param show.ratio          show.ratio
#' @param byrow               byrow
#' @param show.n              show.n
#' @param show.all            show.all
#' @param show.descr          show.descr
#' @param digits              digits
#' @param digits.ratio        digits.ratio
#' @param hide.no             hide.no
#' @param ref.no              ref.no
#' @return                    llista
#' @export                    llista.compare.Ys
#' @importFrom                dplyr "%>%"
llista.compare.Ys<-function(dt="dades",
                            llista.y=c("CODGLP1","CKDEPI_cat2"),
                            llista.x=c("canvi612.pes.perc","canvi612M.pes"),
                            show.ratio=F,
                            byrow=T,
                            show.n=T,
                            show.all=T,
                            show.descr=T,
                            digits=NA,
                            digits.ratio=NA,
                            hide.no = c('NA','No'),
                            ref.no=NA){
  # NUMERO_4)

  # dt=dt.matched
  # llista.y = c("event")
  # llista.x=llistaPS
  # show.ratio=F
  # byrow=T

  restab.llista<-list()

  # 3. Generar formula

  for (i in 1:length(llista.y)) {

    # i<-1

    restab.llista[[i]]<-stats::as.formula(paste(llista.y[[i]], paste(llista.x, collapse=" + "), sep=" ~ ")) %>%
      compareGroups::compareGroups(data=dt,include.miss = F,include.label=T,byrow = byrow,ref.no=ref.no) %>%
      compareGroups::createTable(show.ratio = show.ratio , hide.no = hide.no, show.p.overall=T,show.n=show.n,show.all=show.all,show.descr=show.descr,digits=digits,digits.ratio=digits.ratio)

  }

  restab.llista

}






#' @title                     Generar Surv
#' @description               Generar Surv
#' @param dt                  dt
#' @param event               event
#' @param dtindex             dtindex
#' @param dtsortida           dtsortida
#' @export                    generar_Surv
generar_Surv<-function(dt,
                       event,
                       dtindex="dtindex",
                       dtsortida="sortida"){

  # NUMERO_5)

  # Retorna objecte Surv en dt a partir de dades (dt), event("20150531" / NA), dtindex(Date), dtsortida(20171231),


  # dt=dades_dt
  # event="DG.MCV"
  # dtindex="dtindex"
  # dtsortida="data_sortida"

  x<-dplyr::sym(event)
  dtindex<-dplyr::sym(dtindex)
  sortida<-dplyr::sym(dtsortida)

  if(class(dt[[x]])!="Date" & class(dt[[sortida]])!="Date") {

    temp<-dt %>% dplyr::select(!!dtindex,!!x,!!sortida) %>%
      dplyr::mutate(
        event=dplyr::case_when(as.Date(as.character(!!x),"%Y%m%d")>0~1,
                        is.na(!!x)~0),
        data_final=dplyr::case_when(as.Date(as.character(!!x),"%Y%m%d")>0~as.Date(as.character(!!x),"%Y%m%d"),
                             is.na(!!x)~as.Date(as.character(!!sortida),"%Y%m%d")))
  }

  if(class(dt[[x]])=="Date" & class(dt[[sortida]])=="numeric") {

    temp<-dt %>% dplyr::select(!!dtindex,!!x,!!sortida) %>%
      dplyr::mutate(
        event=dplyr::case_when(!!x>0~1,
                        is.na(!!x)~0),
        data_final=dplyr::case_when(!!x>0~!!x,
                             is.na(!!x)~as.Date(as.character(!!sortida),"%Y%m%d")))
  }

  temp<- temp %>% dplyr::mutate(temps=(data_final-dtindex) %>% as.numeric())

  # Genero l'objecte Surv
  temp$event_surv<-survival::Surv(temp$temps,temp$event)

  # Selecciono i renombro
  nom_surv=paste0(event,".surv")
  temp<-temp %>% dplyr::select(event_surv)
  colnames(temp)=nom_surv

  temp
}

#' @title                     Generar Surv to column
#' @description               Generar Surv to column
#' @param dt                  dt
#' @param event               event
#' @param temps               temps
#' @param codievent           codievent
#' @export                    generar_Surv_to_column
generar_Surv_to_column<-function(dt="dadestotal",
                                 event="EV.AVC_DIC",
                                 temps="EV.AVC_temps",
                                 codievent=1) {

  # NUMERO_6)

  # dt=dadestotal
  # event="EV.AVC_DIC"
  # temps="EV.AVC_temps"
  # codievent=1

  # selecciono dades
  dt_temp<-dt %>% dplyr::select(!!dplyr::sym(event),!!dplyr::sym(temps))

  # Nom del columna nova
  nom_surv=paste0(event,".surv")

  # Genero l'objecte Surv i el coloco en un tibble el renombro
  dt_temp<-
    survival::Surv(dt[[temps]],as.numeric(dt[[event]]==codievent)) %>% dplyr::tibble() %>%
    dplyr::rename(!!nom_surv:=1)
}



#' @title                     Formula COX
#' @description               Formula COX
#' @param x                   x
#' @param event               event
#' @param temps               temps
#' @param elimina             elimina
#' @param cluster             cluster
#' @param a                   a
#' @param taulavariables      taulavariables
#' @param codievent           codievent
#' @export                    formulaCOX
formulaCOX<-function(x="",
                    event="event",
                    temps="temps",
                    elimina="",
                    cluster="",
                    a="",
                    taulavariables="variables.xls",
                    codievent='1') {

  # NUMERO_7)

  #  formula COX ajustat per event="Yes" -------------
  #
  ###       incorpora efecte cluster

  ###       incorpora variables a evaluar a=Llista de variables a avaluar     ###


  # taulavariables<-conductor_variables
  # x=""
  # a="grup"

  # variables <- data.frame(readxl::read_excel(taulavariables) %>% tidyr::as_tibble())
  variables <- read_conductor(taulavariables)
  # variables[is.na(variables)]<- 0
  x_sym<-rlang::sym(x)
  if (x!="") {variables<-variables %>% dplyr::filter(!is.na(!!x_sym)) %>% dplyr::arrange(!!x_sym)
  pepito<-paste("as.vector(variables[variables$",x,">0,]$camp)[!as.vector(variables[variables$",x,">0,]$camp)%in%c('idp')]",sep="")
  llistataula<-eval(parse(text=pepito))
  if (a!="") llistataula<-c(a,llistataula)

  } else {if (a!="") llistataula<-a}


  # resposta<-paste("Surv(",temps,", as.integer(",event,"=='Si'))")
  # resposta<-paste("Surv(",temps,", as.integer(",event,"=='Yes'))")
  resposta<-paste0("Surv(",temps,", as.integer(",event,"=='",codievent,"'))")

  #
  if (cluster!="") kk<-paste(paste(llistataula,collapse=" + "),paste("cluster(",cluster,")",sep=""),sep="+")
  if (cluster=="") kk<-paste(llistataula,collapse=" + ")
  #
  # y<-stats::as.formula(paste(resposta, paste(llistataula, collapse=" + "), sep=" ~ "))
  if (sum(elimina==llistataula)>0) y<-stats::as.formula(paste(paste(resposta, kk , sep=" ~ "),elimina,sep=" - "))
  if (sum(elimina==llistataula)==0) y<-stats::as.formula(paste(resposta, kk , sep=" ~ "))
  #

  y

}

#' @title                     HRadj
#' @description               HRadj
#' @param x                   x
#' @param event               event
#' @param t                   t
#' @param e                   e
#' @param c                   c
#' @param d                   d
#' @param taulavariables      taulavariables
#' @param codievent           codievent
#' @export                    HRadj
HRadj<-function(x="v.ajust",
               event="EV.INSUF_CARD",
               t="tmp_insuf_card",
               e="",
               c="",
               d="dadesDF",
               taulavariables="variables.xls",
               codievent='1') {
  # NUMERO_8)


  #  Retorna Ngran, Events, coef, HR, IC95, IC95, se.coef, p ---------

  # x="v.ajust"
  # event="exitusCV"
  # t="temps_seguiment"
  # d=dades
  # taulavariables = conductor_variables
  # e=""
  # c=""
  # codievent='Si'

  pepito<-paste("sum(d$",t,")",sep="")
  PT<-eval(parse(text=pepito))

  if (c=="") posicio_p=5
  if (c!="") posicio_p=6

  result=tryCatch({
    pp<-survival::coxph(formulaCOX(x=x,event=event,temps=t,elimina=e,cluster=c,taulavariables = taulavariables,codievent=codievent),data=d)

    cbind(PT.Year=PT/365.25,
          N=pp$n,
          EVENTS=pp$nevent,
          coef=summary(pp)$coef[1,1],
          HR=summary(pp)$coef[1,2],
          IC951=summary(pp)$conf.int[1,3],
          IC952=summary(pp)$conf.int[1,4],
          se.coef=summary(pp)$coef[1,3],
          p=summary(pp)$coef[1,posicio_p])}

    ,error = function(e)  {
      cbind(PT.Year=PT/365.25,
            N=0,
            EVENTS=0,
            coef=NA,
            HR=NA,
            IC951=NA,
            IC952=NA,
            se.coef=NA,
            p=NA)})
  result
}

#' @title                    HRestratificats
#' @description              HRestratificats
#' @param event              event
#' @param t                  t
#' @param tipo               tipo
#' @param c                  c
#' @param taulavariables     taulavariables
#' @export                   HRestratificats
HRestratificats<-function(event="exitus",
                          t="temps",
                          tipo="v.ajust",
                          c="",
                          taulavariables='variables.xls') {
  # NUMERO_9)


  #  HRestratificats  ----------------------
  ###   FUNCIio QUE LLANÇO event, temps adjusted i em retorna un data frame amb tot global+ estratificat  ###
  ###     ENVIO exitus, temps i dades i em retorna data frame amb estratificats
  ####    camp estratificat conte variables estratificades tipo="v.ajust" / "crude"

  HRestratificats=data.frame()
  outDf<-data.frame(Subgroup="Total",HRadj(x=tipo,event=event,t=t,d=dades,c=c))

  variables2 <- data.frame(readxl::read_excel(taulavariables) %>% tidyr::as_tibble())
  variables2[is.na(variables2)]<- 0

  # row.names(outDf)<-label(dades$exitus)
  row.names(outDf)<-eval(parse(text=paste("Hmisc::label(dades$",event,")",sep="")))

  HRestratificats <-rbind(HRestratificats,outDf)

  N<-length(variables2[variables2$estrat==1,]$camp)

  for (i in 1:N) {
    outDf <-plyr::ddply(dades, variables2[variables2$estrat==1,]$camp[i], function(df)  HRadj(x=tipo,event=event,t=t,d=df,c=c))

    row.names(outDf)<-c(paste(Hmisc::label(eval(parse(text=paste("dades$",names(outDf)[1],sep="")))),"Adj1",sep=""),
                        paste(Hmisc::label(eval(parse(text=paste("dades$",names(outDf)[1],sep="")))),"Adj2",sep=""))
    names(outDf)[1]<-paste("Subgroup")

    HRestratificats <-rbind(HRestratificats,outDf)

  }
  #   retorna
  return(HRestratificats)
}




#' @title                    OR ajustats
#' @description              OR ajustats
#' @param x                  x
#' @param ajust              ajust
#' @param y                  y
#' @param d                  d
#' @param taulavariables     taulavariables
#' @export                   OR.ajustats
OR.ajustats<-function(x="lipos",
                     ajust="V.ajust",
                     y="prediabetis",
                     d="dadestotal",
                     taulavariables='variables.xls') {
  # NUMERO_10)


  #  OR.ajustats(x,ajust,y)         ###########
  #


  #
  # d=dades
  # taulavariables = "VARIABLES.xls"
  # x="lipos"
  # ajust="v.ajust"
  # y="Prediabetes"

  # d=dades
  # taulavariables="VARIABLES.xls"
  # x="lipos2"
  # ajust="v.ajust"
  # y="Prediabetes"

  #
  variables <- data.frame(readxl::read_excel(taulavariables))
  # variables[is.na(variables)]<- 0
  x_sym<-rlang::sym(x)
  ajust_sym<-rlang::sym(ajust)
  variables<-variables %>% dplyr::filter(!is.na(!!x_sym) | !is.na(ajust_sym) )


  # inicialitzar
  num<-paste("length(variables[variables$",x,">0,]$camp)",sep="")
  num<-eval(parse(text=num))
  ORadj<-matrix(data=NA,ncol=4,nrow = num)
  # noms de columnes en matriu ORadj
  listvariables<-paste("list(variables[variables$",x,">0,]$camp[1:",num,"],c('OR','Linf','Lsup','p valor'))",sep="")
  dimnames(ORadj)<-eval(parse(text=listvariables))
  #
  #### extrec la variable que vull ajustar
  xtext<-paste("variables[variables$",x,">0,]",sep="")
  #

  ##  inicio bucle amb totes les variables que vull ajustar
  for (i in 1:num) {
    # i=1
    xeval<-eval(parse(text=xtext))$camp[i]
    # genero la forumla del model
    # myFormula<-paste(y,"~",xeval,"+",variables.ajust(x=ajust),sep="")

    myFormula<-formula.LOGIT(x=ajust,y=y,eliminar="",a=xeval)

    # ajusto models
    model<-stats::glm(formula= myFormula, family =stats:: binomial, data=d)
    model

    # extrec Coeficients dels models i IC i coloco dins de ORadj
    lolo<-cbind(OR=exp(stats::summary.glm(model)$coef[,1]),Linf=exp(stats::summary.glm(model)$coef[,1]-1.96*stats::summary.glm(model)$coef[,2]),Lsup=exp(stats::summary.glm(model)$coef[,1]+1.96*stats::summary.glm(model)$coef[,2]),p_value=stats::summary.glm(model)$coef[,4])
    ORadj[i,]<-cbind(OR=exp(stats::summary.glm(model)$coef[2,1]),Linf=exp(stats::summary.glm(model)$coef[2,1]-1.96*stats::summary.glm(model)$coef[2,2]),Lsup=exp(stats::summary.glm(model)$coef[2,1]+1.96*stats::summary.glm(model)$coef[2,2]),p_value=stats::summary.glm(model)$coef[2,4])

  }

  ORadj<-rownames(ORadj) %>% cbind(ORadj)
  ORadj<-dplyr::as_tibble(ORadj)
  nomscol<-c("Variable","OR","Linf","Lsup","pvalor")
  ORadj<-ORadj %>% stats::setNames(nomscol)

  ORadj
}


#' @title                    Variables ajust
#' @description              Variables ajust
#' @param x                  x
#' @param variables          variables
#' @export                   variables.ajust
variables.ajust<-function(x="taula1",
                          variables=variables) {
  # NUMERO_11)

  #  Variables.ajust   -----------------
  #####       hi envio la columna de variables amb que vull generar la formula pel compare
  #             FUNCIO variables.ajust

  pepito<-paste("as.vector(variables[variables$",x,"==1,]$camp)[!as.vector(variables[variables$",x,"==1,]$camp)%in%c('idp','grup')]",sep="")
  llistataula<-eval(parse(text=pepito))
  z<-paste(llistataula, collapse=" + ")
}


#' @title                    Extreure coef glm
#' @description              Extreure coef glm
#' @param dt                 dt
#' @param outcomes           outcomes
#' @param x                  x
#' @param z                  z
#' @param taulavariables     taulavariables
#' @export                   extreure_coef_glm
extreure_coef_glm<-function(dt="dades",
                            outcomes="OFT_WORST",
                            x="DM",
                            z="",
                            taulavariables="variables_R.xls"){

  # NUMERO_12)

  #  GLM  COEFICIENTS      ###########################################################
  #################   EXTREU COEFICIENTS glm, IC95 , p valors  GLM a partir de llista d'outcomes, X, i llista de v.ajust

  # dt=dades
  # outcomes="lipos"
  # x="MCD"
  # z="variables_ajust"
  # taulavariables=conductor_variables

  # Número de categories de X
  Ncat.x<-sum(table(dt[x])!=0)
  if (is.numeric(dt[[x]])) Ncat.x=1

  ### Si hi ha variables d'ajust genero llista
  if (z!="") mam<-names(selectorvariables(z,dt=dt,taulavariables=taulavariables))  ### Genero llista de variables
  if (z!="") x<-paste0(paste0(mam,collapse = "+"),"+",x)

  models1_oft<-names(selectorvariables(outcomes,dt=dt,taulavariables=taulavariables))%>%
    paste('~',x) %>%
    purrr::map(~stats::glm(stats::as.formula(.x), data= dt))%>%
    purrr::map(summary) %>%
    purrr::map(coefficients)

  if (Ncat.x>1) noms_var_X<-models1_oft[[1]] %>%
    rownames %>%        #
    tail(Ncat.x-1)      # Capturo nom categories de X

  if (Ncat.x==1) noms_var_X<-models1_oft[[1]] %>%
    rownames %>% tail(1)

  # names(table(dt[x]))[2:Ncat.x]
  if (Ncat.x==1) models1_oft<-models1_oft %>%                       # Si es continua nomes un coef de X
    purrr::map(tail,Ncat.x) %>%
    purrr::map_dfr(data.table::data.table)

  if (Ncat.x>1) models1_oft<-models1_oft %>%                          ## Select nomes num de coeficients necessaris de X
    purrr::map(tail,Ncat.x-1) %>%
    purrr::map_dfr(data.table::data.table)

  if (Ncat.x>1) variables<-names(selectorvariables(outcomes,taulavariables,dt=dt)) %>%  ##  Noms dels outcomes
    rep(each=Ncat.x-1) %>%                                                ##  Cada Num de coeficients
    data.table::data.table()      # Outcomes

  if (Ncat.x==1) variables<-names(selectorvariables(outcomes,taulavariables,dt=dt)) %>%  ##  Noms dels outcomes
    data.table::data.table()      # Outcomes

  colnames(variables)<-"Outcome"


  models_taula<-cbind(variables,Cat.X=noms_var_X,models1_oft)

  models_taula<-models_taula %>% dplyr::select(-c("t value"))    ## Elimino t value

  list(coef=models_taula,caption=paste("Coeficient ajustat per:", x))


}

#' @title                    Extreure coef glm v2
#' @description              Extreure coef glm v2
#' @param dt                 dt
#' @param outcome            outcome
#' @param x                  x
#' @param v.ajust            v.ajust
#' @param level_conf         level_conf
#' @export                   extreure_coef_glm_v2
extreure_coef_glm_v2<-function(dt="dades",
                               outcome="OFT_WORST",
                               x="DM",
                               v.ajust="",
                               level_conf=0.95){

  # NUMERO_13)

  #  EXTREU COEFICIENTS glm, IC95 , p valors  GLM a outcome, X, i llista de v.ajust

  # dt=dades_long %>% filter(.imp==0),outcome=outcome,x=grups,v.ajust=""
  # dt=dades_long %>% filter(.imp==0)
  # outcome=outcome
  # x=grups
  # v.ajust=v.ajust
  # level_conf=level_conf

  # dt=dades_long %>% filter(.imp==0)
  # outcome="MPR.TX.cat"
  # outcome="HBA1C.dif324m"
  # x="grup"
  # v.ajust=""
  # level_conf=0.95

  Zalfa=stats::qnorm((1-level_conf)/2,lower.tail=FALSE)

  outcome_sym<-rlang::sym(outcome)

  # Número de categories de X
  Ncat.x<-sum(table(dt[x])!=0)
  if (is.numeric(dt[[x]])) Ncat.x=1

  ### Hi ha variables d'ajust genero formula llista
  if (any(v.ajust!="")) pepe<-paste0(outcome,"~",paste0(c(x,v.ajust),collapse = " + "))
  if (any(v.ajust=="")) pepe<-paste0(outcome,"~",x)

  # Outcome es factor?
  outcome_es_factor<-any(dt[[outcome]] %>% class() %in% c("character","factor"))

  # Si Outcome (Y) es factor --> glm-Logistica
  if (outcome_es_factor) {

    fit<-stats::glm(eval(parse(text=pepe)),family = stats::binomial(link="logit"),data=dt)
    resum<-fit %>% summary %>% stats::coef()

    # Estandarditzar
    resumStd<-parameters::model_parameters(fit,standardize="basic",ci=level_conf) %>% dplyr::select(c(1:5),-SE) %>%
      dplyr::rename(term=Parameter, CIStd_low=CI_low,CIStd_high=CI_high) %>%
      dplyr::mutate(term=as.factor(term))
    resum<-resum %>% cbind(resumStd)

    resum_model<-dplyr::tibble(categoria=row.names(resum)) %>%
      cbind(resum) %>% dplyr::as_tibble() %>%
      dplyr::mutate(OR=Estimate %>% exp(),
             Linf=(Estimate-(Zalfa*`Std. Error`)) %>% exp(),
             Lsup=(Estimate+(Zalfa*`Std. Error`)) %>% exp())

  }

  # Si Outcome (Y) es numerica --> glm-lineal
  if (!outcome_es_factor) {

    fit<-stats::glm(stats::as.formula(pepe),family = "gaussian", data= dt)
    resum<-fit %>% summary %>% stats::coef()

    # Estandarditzar
    resumStd<-parameters::model_parameters(fit,standardize="basic",ci=level_conf) %>% dplyr::select(c(1:5),-SE) %>%
      dplyr::rename(term=Parameter, CIStd_low=CI_low,CIStd_high=CI_high) %>%
      dplyr::mutate(term=as.factor(term))
    resum<-resum %>% cbind(resumStd)

    resum_model<-dplyr::tibble(categoria=row.names(resum)) %>%
      cbind(resum) %>% dplyr::as_tibble() %>% dplyr::select(-term) %>%
      dplyr::mutate(Beta=Estimate,
             Linf=(Estimate-(Zalfa*`Std. Error`)) ,
             Lsup=(Estimate+(Zalfa*`Std. Error`)))
  }

  # Si X es factor afegir cat de ref + mean
  es_factor<- any(dt[[x]] %>% class() %in% c("character","factor"))
  if (es_factor) {
    resumtotal<-dplyr::tibble(categoria=row.names(resum)[1:Ncat.x],outcome=outcome) %>%
      dplyr::add_row (categoria=paste0(x,".Ref"),outcome=outcome)
  }
  # Si no es factor
  if (!es_factor) {resumtotal<-dplyr::tibble(categoria=row.names(resum),outcome=outcome) }

  # Afegir categoria
  resumtotal<-resumtotal %>% dplyr::left_join(resum_model,by="categoria")

  # Nomes en GLM afegir mitjana estimada per categoria
  if (outcome_es_factor==F) {
    resumtotal<-resumtotal %>%
      dplyr::mutate (beta0=resumtotal$Estimate[1],estimate=ifelse(is.na(Estimate),0,Estimate)) %>%
      dplyr::mutate(mean=ifelse(categoria!="(Intercept)", beta0+estimate,NA))
  }

  resumtotal %>% head(Ncat.x+1)
}

#' @title                    Extreure coef glm mi
#' @description              Extreure coef glm mi
#' @param dt                 dt
#' @param outcome            outcome
#' @param x                  x
#' @param v.ajust            v.ajust
#' @param level_conf         level_conf
#' @export                   extreure_coef_glm_mi
extreure_coef_glm_mi<-function(dt="tempData",
                               outcome="valor612M.GLICADA",
                               x="SEXE",
                               v.ajust="",
                               level_conf=0.95) {

  # NUMERO_14)


  #  GLM (Logistic o Lineal) dades imputades --------------------
  ## Retorn de coeficients glm() amb dades imputades d'una variable independent X ~ Y

  # dt=mice::as.mids(dades_long)
  # outcome=outcome
  # x=grups
  # v.ajust=v.ajust
  # v.ajust=c("sexe","edat","qmedea")
  # level_conf=0.95

  # Funcio que extreu parametres estandaritzats Overall (mitjana Cutre)
  Standarditzar_mice_fits<-function(fits,level_conf=0.95) {
    pars_Std<-fits$analyses %>%
      purrr::map(~parameters::model_parameters(.x,standardize="basic",ci=level_conf)) %>%
      dplyr::bind_rows() %>% data.frame() %>%
      dplyr::group_by(Parameter) %>%
      dplyr::summarise_all(base::mean) %>%
      dplyr::select(c(1:5),-SE) %>%
      dplyr::rename(term=Parameter, CIStd_low=CI_low,CIStd_high=CI_high) %>% dplyr::mutate(term=as.factor(term))}

  # Z per confidence interval
  Zalfa=stats::qnorm((1-level_conf)/2,lower.tail=FALSE)

  ### Hi ha variables d'ajust genero formula llista
  if (any(v.ajust!="")) pepe<-paste0(outcome,"~",paste0(c(x,v.ajust),collapse = " + "))
  if (any(v.ajust=="")) pepe<-paste0(outcome,"~",x)

  # Outcome es factor?
  outcome_es_factor<-any(dt$data[[outcome]] %>% class() %in% c("character","factor"))

  # Si Outcome (Y) es factor --> glm-Logistica
  if (outcome_es_factor) {

    fits<-with(dt,stats::glm(eval(parse(text=pepe)),family = stats::binomial(link="logit")))

    resum<-fits %>% mice::pool() %>% summary()

    # Resum estandarditzat i ho fusiono
    resum_Std<-Standarditzar_mice_fits(fits,level_conf)
    resum<-resum %>% dplyr::left_join(resum_Std,by="term")

    resum_model<-dplyr::tibble(categoria=resum$term) %>%
      cbind(resum) %>%
      dplyr::mutate(OR=estimate %>% exp,
             Linf=(estimate-(Zalfa*std.error)) %>% exp,
             Lsup=(estimate+(Zalfa*std.error)) %>% exp)
  }

  # Si outcome (Y) es numeric --> GLM lineal
  if (!outcome_es_factor) {

    # pepe<-paste0(outcome,"~",x)
    fits<-with(dt,stats::lm(eval(parse(text=pepe))))

    resum<-base::summary(mice::pool(fits))

    # Resum estandarditzat i fusiono
    resum_Std<-Standarditzar_mice_fits(fits,level_conf)
    resum<-resum %>% dplyr::left_join(resum_Std,by="term")

    resum_model<-dplyr::tibble(categoria=resum$term) %>% cbind(resum)

  }

  # Si X  es cat afegir categoria de referencia
  es_factor<- any(dt$data[[x]] %>% class() %in% c("character","factor"))
  # Número de categories de X i selecciono files de X
  if (is.numeric(dt$data[[x]])) Ncat.x=1 else Ncat.x<-sum(table(dt$data[x])!=0)

  if (es_factor) {
    resumtotal<-
      dplyr::tibble(categoria=resum$term[1:Ncat.x],outcome=outcome) %>%
      dplyr::add_row (categoria=paste0(x,".Ref"),outcome=outcome)
  }

  # Si no es factor
  if (!es_factor) {resumtotal<-dplyr::tibble(categoria=resum$term,outcome=outcome) }

  # Afegir categoria
  resumtotal<-resumtotal %>% dplyr::left_join(resum_model,by="categoria")  %>% dplyr::select(-term)

  # Nomes en GLM calcular la mitjana estimada per categoria
  if (outcome_es_factor==F) {
    resumtotal<-resumtotal %>%
      dplyr::mutate (beta0=resumtotal$estimate[1],
              estimate=ifelse(is.na(estimate),0,estimate)) %>%
      dplyr::mutate(mean=ifelse(categoria!="(Intercept)", beta0+estimate,NA)) }

  # Seleccionar columnes
  resumtotal %>% head(Ncat.x+1)

}


#' @title                    Extreure coef mice estrats
#' @description              Extreure coef mice estrats
#' @param tempData           tempData
#' @param data_list          data_list
#' @param X                  X
#' @param Y                  Y
#' @param grups              grups
#' @param logit              logit
#' @export                   extreure_coef_mice_estrats
extreure_coef_mice_estrats<-function(tempData,
                                     data_list,
                                     X=c("bmi","hyp"),
                                     Y="chl",
                                     grups="age",
                                     logit=F) {

  # NUMERO_15)


  #  Coeficients GLM(lineal/logistica) MICE estratificats  ---------------------
  # Arguments: Objecte MICE i data_list imputats, vector de X , Y , logit=T/F

  # tempData
  # data_list
  # X=X
  # Y="bmi"
  # grups="age"
  # logit=F
  # .data=data_list[[1]]

  fitting=function(.data,frm,logit=F) {
    if(logit) {model=stats::glm(frm,data=.data,family=stats::binomial(link="logit"))}
    if(!logit){model=stats::lm(frm, data =.data)}
    model
  }

  # Cada llista de datasets separat per grups
  data_list_splitted<-data_list %>% purrr::map(~base::split(.x,.x[[grups]]))

  # numero de grups
  num_grups<-tempData$data[[grups]] %>% table %>% length

  # Aplica models n una llista
  models_list<-lapply(1:num_grups, function(NSPLIT) data_list_splitted %>%
                        lapply(dplyr::nth, NSPLIT) %>%
                        lapply(fitting, formula_vector(X,Y,logit),logit=logit) %>%
                        mice::as.mira() %>%
                        mice::pool() %>%
                        summary() %>%
                        tibble::rownames_to_column("variable"))

  # Posar noms als grups
  names(models_list)<-names(data_list_splitted[[1]])

  # Ho posa en un data set
  models_dt<-dplyr::bind_rows(models_list, .id = "Grup") %>% dplyr::as_tibble

  models_dt
}

#' @title                    Extreure dif proporcions
#' @description              Extreure dif proporcions
#' @param dades              dades
#' @param outcome            outcome
#' @param ref_cat            ref_cat
#' @param grups              grups
#' @export                   extreure.dif.proporcions
extreure.dif.proporcions<-function(dades,
                                   outcome="Prediabetes",
                                   ref_cat=NA,
                                   grups="Sex") {

  # NUMERO_16)

  # extreure.dif.proporcions() : Diferencia de % respecte una categoria ref + interval de confiança
  # Extreu : Diferencia de % respecte una categoria ref + interval de confiança

  # dades=dades
  # outcome="Prediabetes"
  # ref_cat=NA
  # grups="Sex"

  # Canviar arguments per ser evaluats
  outcome_eval<-dplyr::sym(outcome)
  grups_eval<-dplyr::sym(grups)

  # refCat
  if (is.na(ref_cat)) ref_cat=levels(dades[[grups]])[1]

  # N per grups
  dades_N<-dades %>%
    dplyr::group_by(!!outcome_eval) %>% dplyr::count(!!grups_eval) %>% dplyr::ungroup() %>%
    tidyr::spread(key=!!outcome_eval,value=dplyr::n)

  levels_outcome=names(dades_N)[names(dades_N)!=grups]

  # Proporcions per grups
  dades_P<-dades %>% dplyr::group_by(!!outcome_eval) %>% dplyr::count(!!grups_eval) %>% dplyr::ungroup() %>%
    tidyr::spread(key=!!outcome_eval,value=dplyr::n) %>%
    dplyr::mutate(sum=rowSums(.[2:ncol(.)])) %>%
    dplyr::mutate_if(is.numeric,dplyr::funs(./sum)) %>%
    dplyr::select(-sum)

  # Parts de errors estandards: p, variancia (p*q/n), n
  prop<-dades_P %>% dplyr::select(c(2:ncol(.)))
  enes<-dades_N %>% dplyr::select(c(2:ncol(.)))
  variancia<-(prop*(1-prop))/rowSums(enes)
  colnames(enes)<-names(enes) %>% paste0(".n")

  variancia <- dplyr::select(dades_N,1) %>% cbind(variancia)

  # Rotate
  var2<-variancia %>%
    tidyr::gather(temp, value,-grups) %>%
    tidyr::spread(!!grups_eval, value) %>% dplyr::right_join(dplyr::tibble(temp=levels_outcome),by="temp") %>%
    dplyr::select(-temp)

  # Error standard de la diferencia [p1*(1-p1)]n1 + [(p2*1-p2)/n2] respecte catRef
  SE_dif_prop<-var2 %>%
    dplyr::mutate_all(function(x) sqrt (x + .[[ref_cat]])) %>%
    dplyr::select(-ref_cat)

  # Rotate (Diferencia respecte una de cat ref_cat ("No"))
  prop<-dades_P %>%
    tidyr::gather(temp,prop,-grups) %>%
    tidyr::spread(!!grups_eval,prop) %>%dplyr:: right_join(dplyr::tibble(temp=levels_outcome),by="temp") %>%
    dplyr::select(-temp)

  # Diferencia de proporcions
  dif_prop<-prop %>% dplyr::mutate_all(function(x) x - .[[ref_cat]])
  dif_prop<-dif_prop %>% dplyr::select(-ref_cat)
  colnames(dif_prop)<-names(dif_prop) %>% paste0(".dif")

  # Calculo IC95%
  IC1<-dif_prop - (1.96*SE_dif_prop) %>%dplyr:: as_tibble()
  colnames(IC1)<-names(dif_prop)[names(dif_prop)!=ref_cat] %>% paste0(".IC195")
  IC2<-dif_prop + (1.96*SE_dif_prop)
  colnames(IC2)<-names(dif_prop)[names(dif_prop)!=ref_cat] %>% paste0(".IC295")

  # Ho junto tot
  dades_T<-data.frame(group=levels_outcome) %>% cbind(dif_prop,IC1,IC2) %>%dplyr::as_tibble()

  # Transformar en %
  dades_T<-dades_T %>% dplyr::mutate_if(is.numeric,~ .*100)

  # Selecciono per printar en l'ordre
  categories<-levels(dades[[grups]])[levels(dades[[grups]])!=ref_cat] %>% as.vector()
  ncat<-length(categories)
  # Genero taula ordenada per categories i en funcio
  dades_select<-dades_T %>% dplyr::select(1)
  for (i in 1:ncat) {
    dades_temp<-dades_T %>% dplyr::select(dplyr::contains(categories[i]))
    dades_select<-dades_select %>% cbind(dades_temp) }


  # Retorno dades

  dplyr::as_tibble(dades_select)

}

#' @title                    Extreure resum outcomes imputation
#' @description              Extreure resum outcomes imputation
#' @param dades_long         dades_long
#' @param outcome            outcome
#' @param v.ajust            v.ajust
#' @param grups              grups
#' @param level_conf         level_conf
#' @export                   extreure.dif.proporcions
extreure_resum_outcomes_imputation<-function(dades_long=dades,
                                             outcome="HBA1C.dif324m",
                                             grups="grup",
                                             v.ajust=c("sexe","edat"),
                                             level_conf=0.95) {
  # NUMERO_17)

  # Funcio que retorna summari (Beta/OR , IC95%, mean) amb dades imputades i completes crudes i ajustades d'un outcome en relacio a un grup
  # Objecte dades_long es fitxer de dades amb dades completes (.imp==0) + imputades (.imp>0)


  # dades_long=dades_temp
  # outcome="HBA1C.dif324m.cat"
  # grups="grup"
  # level_conf=0.95
  # v.ajust=c("sexe","edat")


  Zalfa=stats::qnorm((1-level_conf)/2,lower.tail=FALSE)

  # Outcome es factor?
  outcome_es_factor<-any(dades_long[[outcome]] %>% class() %in% c("character","factor"))

  # Outcome numerica -> Retorna Btes
  if (!outcome_es_factor) {

    # Proves per extreure coeficients (Dades imputades, completes, estimacions crues i ajustades)
    dt_estimaciones1<-extreure_coef_glm_mi(dt=mice::as.mids(dades_long),outcome=outcome,x=grups,v.ajust=v.ajust) %>%
      dplyr::transmute(datos="Imputados",type="Adjusted",categoria,outcome,estimate,std.error,Linf=estimate - (Zalfa*`std.error`),Lsup=estimate + (Zalfa*`std.error`),p.value, mean,
                Std_Coefficient,CIStd_low,CIStd_high)

    dt_estimaciones2<-extreure_coef_glm_mi(dt=mice::as.mids(dades_long),outcome=outcome,x=grups,v.ajust="") %>%
      dplyr::transmute(datos="Imputados",type="Crudas",categoria,outcome,estimate,std.error,Linf=estimate - (Zalfa*`std.error`),Lsup=estimate + (Zalfa*`std.error`),p.value, mean,
                Std_Coefficient,CIStd_low,CIStd_high)

    dt_estimaciones3<-extreure_coef_glm_v2(dt=dades_long %>% dplyr::filter(.imp==0),outcome=outcome,x=grups,v.ajust=v.ajust,level_conf=level_conf) %>%
      dplyr::transmute (datos="Completos",type="Adjusted",categoria,outcome,estimate,Linf,Lsup,p.value=`Pr(>|t|)`,mean,estimate=Estimate,std.error=`Std. Error`,
                 Std_Coefficient,CIStd_low,CIStd_high)

    dt_estimaciones4<-extreure_coef_glm_v2(dt=dades_long %>% dplyr::filter(.imp==0),outcome=outcome,x=grups,v.ajust="",level_conf=level_conf) %>%
      dplyr::transmute (datos="Completos",type="Crudas",categoria,outcome,estimate,Linf,Lsup,p.value=`Pr(>|t|)`,mean,estimate=Estimate,std.error=`Std. Error`,
                 Std_Coefficient,CIStd_low,CIStd_high)

  }

  # Outcome factor -> Retornar OR's
  if (outcome_es_factor) {
    # Outcome categoric
    dt_estimaciones1<-extreure_coef_glm_mi(dt=mice::as.mids(dades_long),outcome=outcome,x=grups,v.ajust=v.ajust,level_conf=level_conf) %>%
      dplyr::transmute(datos="Imputados",type="Adjusted",categoria,outcome,OR,Linf,Lsup,p.value,estimate,std.error,Std_Coefficient,CIStd_low,CIStd_high)

    dt_estimaciones2<-extreure_coef_glm_mi(dt=mice::as.mids(dades_long),outcome=outcome,x=grups,v.ajust="",level_conf=level_conf) %>%
      dplyr::transmute(datos="Imputados",type="Crudas",categoria,outcome,OR,Linf,Lsup,p.value,estimate,std.error,Std_Coefficient,CIStd_low,CIStd_high)

    dt_estimaciones3<-extreure_coef_glm_v2(dt=dades_long %>% dplyr::filter(.imp==0),outcome=outcome,x=grups,v.ajust=v.ajust,level_conf=level_conf) %>%
      dplyr::transmute (datos="Completos",type="Adjusted",categoria,outcome,OR,Linf,Lsup,p.value=`Pr(>|z|)`,estimate=Estimate,std.error=`Std. Error`,
                 Std_Coefficient,CIStd_low,CIStd_high)

    dt_estimaciones4<-extreure_coef_glm_v2(dt=dades_long %>% dplyr::filter(.imp==0),outcome=outcome,x=grups,v.ajust="",level_conf=level_conf) %>%
      dplyr::transmute (datos="Completos",type="Crudas",categoria,outcome,OR,Linf,Lsup,p.value=`Pr(>|z|)`,estimate=Estimate,std.error=`Std. Error`,
                 Std_Coefficient,CIStd_low,CIStd_high)

  }

  # Juntar-ho tot
  dt_estimaciones_resumen<-dt_estimaciones1 %>%dplyr:: bind_rows(dt_estimaciones2) %>% dplyr:: bind_rows(dt_estimaciones3) %>% dplyr:: bind_rows(dt_estimaciones4)
  # Descriptivo datos completos

  dt_estimaciones_resumen

}


#' @title                    Plot KM
#' @description              Plot KM
#' @param y                  y
#' @param grup               grup
#' @param d                  d
#' @param caption            caption
#' @param llegenda           legenda
#' @export                   plotKM
plotKM<-function(y="exitus.surv",
                grup="grup",
                d="dades",
                caption="",
                llegenda=c("No","Yes")) {

  # NUMERO_18)

  #  K-M   plot #####

  # y=dadesDF$exitus_surv
  # grup=dadesDF$hta
  # d=dadesDF
  # caption=Hmisc::label(dadesDF$hta)
  # llegenda=c("No","Yes")

  # y=dadesDF$exitus_surv
  # grup=dadesDF$edad_cat6
  # d=dadesDF
  # llegenda=c("<45", "[45-55)", "[55-65)", "[65-75)", "[75-85)","85+")

  # Basic survival curves
  p <- survminer::ggsurvplot(survival::survfit(y ~ grup, data = d), data = d,
                             main = "Survival curve",
                             title= caption,
                             size = 0.5,
                             ylim = c(0,1),
                             xlim = c(0,60),
                             break.x.by=12,
                             xlab = "Time in months",
                             risk.table = F,
                             censor.shape="|", censor.size = 1
                             ,legend.labs=llegenda)
  p
}



#' @title                    Plot KM_Incidence
#' @description              Plot KM_Incidence
#' @param y                  y
#' @param grup               grup
#' @param d                  d
#' @param caption            caption
#' @param llegenda           legenda
#' @export                   plotKM_Incidence
plotKM_Incidence<-function(y="exitus.surv",
                          grup="grup",
                          d="dades",
                          caption="",
                          llegenda=c("No","Yes")) {
  # NUMERO_19)

  #  K-M   plot #####

  # caption=""
  # llegenda=c("No","Yes")
  # y=dadesDF$exitus_surv
  # grup=dadesDF$edad_cat6
  # d=dadesDF
  # llegenda=c("85+","[75-85)", "[65-75)", "[55-65)", "[45-55)","<45")

  # Basic survival curves
  p <- survminer::ggsurvplot(survival::survfit(y ~ grup, data = d), data = d,
                             main = "Survival curve",
                             title= caption,
                             size = 0.5,
                             ylim = c(0,1),
                             xlim = c(0,60),
                             break.x.by=12,
                             linesize="strata",
                             xlab = "Time in months",
                             risk.table = F,
                             censor.shape=".",
                             censor.size = 0.5,
                             legend.labs=llegenda,
                             legend="right",
                             fun="event",
                             ggtheme = ggplot2::theme_bw(),
                             palette = c("black","black","black","black","black","black"))
  p
}




#' @title                    Boxplot variables grup
#' @description              Boxplot variables grup
#' @param dt                 dt
#' @param variables          variables
#' @param grup               grup
#' @param taulavariables     taulavariables
#' @export                   boxplot_variables_grup
boxplot_variables_grup<-function(dt=dades,
                                 variables="OFT_WORST",
                                 grup="DM",
                                 taulavariables="variables_R.xls") {

  # NUMERO_20)

  #  Box-plot -----------------
  # dt=dades
  # variables="OFT_WORST"
  # grup="DM"
  # taulavariables="variables_R.xls"

  ###   extrect variables
  paco<-extreure.variables(variables,taulavariables=taulavariables)


  ###   Genero taula llarga
  popes<-dt %>%
    dplyr::select(c(paco,grup)) %>%
    tidyr::gather_(key=variables,value="valor",setdiff(paco, grup))


  ###   FAi ggplot
  figura1<-popes %>% ggplot2::ggplot(ggplot2::aes_string(x=variables, y="valor",fill=grup))+ggplot2::geom_boxplot()

  figura1


  ###   FAi ggplot
  #figura1<-popes %>% ggplot2::ggplot(aes_string(x=variables, y="valor",fill=grup))+geom_boxplot()
  #figura1

}

#' @title                    GGPLOT grups
#' @description              GGPLOT grups
#' @param Y                  Y
#' @param dt                 dt
#' @param X                  X
#' @param grup               grup
#' @export                   ggplot_grups
ggplot_grups<-function(Y="DIS_estatina",
                       dt=dades,
                       X="edat",
                       grup="sexe") {

  # NUMERO_21)

  #  Figura Spline Y~x per grups  --------------------
  #  Spline Y ~ x (continua) estratificat per grups)
  #  Requereix Y, X, grup y dades



  figuragamX<-ggplot2::ggplot(dt, ggplot2::aes_string(x=X, y=Y,group=grup,shape=grup, color=grup))+
    ggplot2::geom_smooth(method = stats::lm, formula = y ~ splines::bs(x, 3), se = T)+
    ggplot2::xlab(Hmisc::label(dades[X]))+
    ggplot2::ylab(Hmisc::label(dades[Y]))+
    ggplot2::theme_bw()+
    ggplot2::labs(colour =grup)+
    ggplot2::theme(legend.position="none")
  figuragamX
}


#' @title                    MAP ggplot
#' @description              MAP ggplot
#' @param dades              dades
#' @param datainicial        datainicial
#' @param datafinal          datafinal
#' @param id                 id
#' @param grup_color         grup_color
#' @param grup_linea         grup_linea
#' @param lim_inf            lim_inf
#' @param lim_sup            lim_sup
#' @param add_point          add_point
#' @export                   MAP_ggplot
MAP_ggplot<-function(dades=dt,
                     datainicial="data",
                     datafinal="datafi",
                     id="idp_temp",
                     grup_color=NA,
                     grup_linea=NA,
                     lim_inf=-Inf,
                     lim_sup=Inf,
                     add_point=NA) {
  # NUMERO_22)

  # Retorna un mapa temporal (datainicial-datafinal per grups) Individus a partir de:
  # dades, datainicial, data final, id, grup color, grup linea, finestra (porca1,porca2)


  # dades=mostra_dt
  # datainicial="datainicial"
  # datafinal="datafinal"
  # id="idp"
  # grup_color=NA
  # grup_linea=NA
  # lim_inf=-Inf
  # lim_sup=+Inf
  # add_point=NA

  if (is.na(grup_linea)) dades<- dades %>% dplyr::mutate(Overall="Overall")
  if (is.na(grup_linea)) grup_linea<- "Overall"

  if (is.na(grup_color)) dades<- dades %>% dplyr::mutate(Overall2="Overall2")
  if (is.na(grup_color)) grup_color<- "Overall2"

  # # Configuro limits finestra
  if (lim_inf==-Inf) porca1<-min(dades %>%  dplyr::pull(datainicial) %>% lubridate::ymd())
  if (lim_sup==+Inf) porca2<-max(dades %>% dplyr:: pull(datafinal) %>% lubridate::ymd())
  # #
  if (lim_inf!=-Inf) porca1<-lim_inf
  if (lim_sup!=+Inf) porca2<-lim_sup

  porca1=lubridate::ymd(porca1)
  porca2=lubridate::ymd(porca2)

  # Conversio a Sym per evaluacio
  datainicial<-rlang::sym(datainicial)
  datafinal<-rlang::sym(datafinal)
  id<-rlang::sym(id)
  grup_color<-rlang::sym(grup_color)
  grup_linea<-rlang::sym(grup_linea)

  # Calculo dies de duracio
  dades<-dades %>%
    dplyr::mutate(
      dia0=lubridate::ymd(!!datainicial),
      diaf=lubridate::ymd(!!datafinal),
      days_duration=lubridate::interval(dia0,diaf) %>% lubridate::as.duration()/lubridate::ddays()
    )

  # Gráfico el tema
  figura<-ggplot2::ggplot(dades,ggplot2::aes(x =dia0,y =!!id, color=!!grup_color,group=!!grup_linea,linetype=!!grup_linea))+
    ggplot2::geom_segment(ggplot2::aes(x =dia0, xend=diaf, y =!!id, yend = !!id),arrow = ggplot2::arrow(length = ggplot2::unit(0.03, "npc"))) +
    ggplot2::geom_point(ggplot2::aes(dia0, !!id)) +
    ggplot2::geom_text(vjust = -0.5, hjust=0, size = 3,ggplot2::aes(x =dia0, y = !!id,label = paste(round(days_duration, 2), "days")))+
    ggplot2::scale_colour_brewer(palette = "Set1")+
    ggplot2::xlim(porca1,porca2)+
    ggplot2::theme(legend.position="top",legend.background = ggplot2::element_rect(fill="gray80",size=1, linetype="solid", colour ="black"))

  if (!is.na(add_point)) {
    figura<-figura+
      ggplot2::geom_point(ggplot2::aes(!!rlang::sym(add_point),!!id),size=3,shape=8) +
      ggplot2::geom_text(vjust = -0.5, hjust=0, size = 2,ggplot2::aes(x =!!rlang::sym(add_point), y = !!id,label = add_point))
  }

  figura


}


#' @title                    Gaps
#' @description              Gaps
#' @param dt                 dt
#' @param K                  K
#' @param Nmostra            Nmostra
#' @param finestraX          finestraX
#' @param llavor             llavor
#' @export                   Gaps
Gaps<-function(dt=dades,
               K=14,
               Nmostra=10,
               finestraX=c(NA,NA),
               llavor=123){

  # NUMERO_23)

  # Retorna llista amb dos data_frames de farmacs i dos plots pre i post

  # dt=temp_dades
  # K=14
  # Nmostra=5
  # finestraX=c(NA,NA)
  # llavor=123

  # if (Nmostra==Inf) Nmostra=10

  # Si Nmostra es infinit o mes gran que la mostra agafo el màxim
  Nmostra_maxim<- dt %>% dplyr::distinct(idp) %>% nrow()
  if (Nmostra==Inf | Nmostra>Nmostra_maxim) Nmostra<- Nmostra_maxim


  farmacs_list<-dt %>%dplyr::distinct(agr)%>%dplyr::pull()

  dt<-dt%>% dplyr::mutate(agr=factor(agr))
  set.seed(llavor) # S'ha d'actualitzar
  id_sample<-dt %>% dplyr::distinct(idp) %>% dplyr::sample_n(size=Nmostra)
  dt<-id_sample %>% dplyr::left_join(dt,by="idp")
  dt<-dt%>%dplyr::select(idp,agr,data=dat,datafi,FACTPRESC=tipus)

  # Calculo dies de duracio
  dt<-dt %>%
    dplyr::mutate(
      data=lubridate::ymd(data),
      datafi=lubridate::ymd(datafi),
      days_duration=lubridate::interval(data,datafi) %>%lubridate:: as.duration()/lubridate::ddays())

  dt<-dt %>% dplyr::mutate (idp2=idp, idp=paste0(idp,agr,".",stringr::str_sub(FACTPRESC,1,1)))

  dt<-dt%>%dplyr::select(idp,agr,data,datafi,days_duration,idp2,FACTPRESC)
  # Genera mapa origen (n)

  dt<-dt %>% dplyr::mutate (idp_temp=paste0(stringr::str_sub(dt$idp,1,6),agr,".",stringr::str_sub(FACTPRESC,1,1)))


  if (is.na(finestraX[1]))  porca1<-lubridate::ymd(min(dt$data))
  if (is.na(finestraX[2]))  porca2<-lubridate::ymd(max(dt$datafi))
  if (!is.na(finestraX[1])) porca1<-lubridate::ymd(finestraX[1])
  if (!is.na(finestraX[2])) porca2<-lubridate::ymd(finestraX[2])

  dt<-dt %>% dplyr::mutate(datafi =dplyr::case_when(porca2<=datafi ~ porca2,TRUE ~ datafi))

  # Recalcular intervals en dies a partir de les finetres!
  dt<-dt%>%dplyr::mutate(days_duration=lubridate::interval(data,datafi)%>%lubridate::as.duration()/lubridate::ddays())

  MAP<-MAP_ggplot(dades=dt,datainicial="data",datafinal="datafi",id="idp_temp",grup_color="agr",grup_linea="FACTPRESC",lim_inf=porca1,lim_sup=porca2)


  dt<-dt%>%dplyr::arrange(idp,data,datafi)
  dt<-dplyr::mutate(dt,data=lubridate::ymd(data),datafi=lubridate::ymd(datafi))
  dt<-dt%>%dplyr::group_by(idp)%>% dplyr::mutate(gap=(data-dplyr::lag(datafi)))
  dt<-dt%>%dplyr::mutate(gap2=dplyr::case_when(gap>K ~1, TRUE ~0))
  dt<-dt%>%dplyr::group_by(idp)%>%dplyr::mutate(gap3=(cumsum(gap2)))%>%dplyr::ungroup()

  # Agregate
  dt2<-dt %>%
    dplyr::select(idp,data,datafi,gap3,agr,idp2, FACTPRESC) %>%
    dplyr::group_by(idp,agr,gap3)%>%
    dplyr::summarise(data= min(data), datafi= max(datafi),idp2=min(idp2),FACTPRESC=min(FACTPRESC))%>%
    dplyr::ungroup
  #

  # Tornem a Recalcular intervals en dies a partir dels Gaps i Fienstra!.
  dt2<-dt2%>%dplyr::mutate(days_duration=lubridate::interval(data,datafi)%>%lubridate::as.duration()/lubridate::ddays())


  dt2<-dt2 %>% dplyr::mutate(idp_temp=paste0(stringr::str_sub(dt2$idp,1,6),agr,".",stringr::str_sub(FACTPRESC,1,1)))


  MAP2<-MAP_ggplot(dades=dt2,datainicial="data",datafinal="datafi",id="idp_temp",grup_color="agr",grup_linea="FACTPRESC",lim_inf=porca1,lim_sup=porca2)



  #MAP2

  dt2<-dt2 %>% dplyr::select(idp2,idp,agr,data,datafi,FACTPRESC)

  #dt2

  list(dades1=dt,dades2=dt2,Mapa_pre=MAP,Mapa_post=MAP2)


}


#' @title                    MAP ggplot univariant
#' @description              MAP ggplot univariant
#' @param dades              dades
#' @param datainicial        dat
#' @param datafinal          dat
#' @param id                 id
#' @param Nmostra            Nmostra
#' @param add_point          add_point
#' @param add_final          add_final
#' @param set_seed           set_seed
#' @export                   MAP_ggplot_univariant
MAP_ggplot_univariant<-function(dades=dt,
                                datainicial="data",
                                datafinal="datafi",
                                id="idp_temp",
                                Nmostra=10,
                                add_point=NA,
                                add_final=NA,
                                set_seed=123) {

  # NUMERO_25)

  # Dibuixa mapa temporal univariant per verificar solapaments

  # dades=dades %>% dplyr::filter(situacio=="T" | situacio=="D")
  # datainicial="dtindex"
  # datafinal="datafi_seguiment"
  # id="idp"
  # Nmostra=10
  # add_point=NA
  # add_final="situacio"

  # Conversio a Sym per evaluacio
  datainicial<-rlang::sym(datainicial)
  datafinal<-rlang::sym(datafinal)
  id_sym<-rlang::sym(id)

  # mostrejo
  dades<-mostreig_ids(dt=dades,id=id,n_mostra = Nmostra,set_seed=set_seed)

  # if (Nmostra!=Inf) id_sample<-dades %>% dplyr::distinct(!!id) %>% dplyr:: sample_n(size=Nmostra)
  # dt<-id_sample %>% dplyr::left_join(dt,by=dplyr::quo_name(id)) #

  # Calculo dies de duracio
  dades<-dades %>%  dplyr::mutate(dia0=!!datainicial,diaf=!!datafinal,days_duration=diaf-dia0)

  # Gráfico el tema
  figura<- ggplot2::ggplot(dades,ggplot2::aes(x =dia0,y =!!id_sym))+
    ggplot2::geom_segment(ggplot2::aes(x =dia0, xend=diaf, y =!!id_sym, yend = !!id_sym),arrow =  ggplot2::arrow(length = ggplot2::unit(0.01, "npc"))) +
    ggplot2::geom_point(ggplot2::aes(dia0, !!id_sym)) +
    ggplot2::geom_text(vjust = -0.5, hjust=0, size = 3, ggplot2::aes(x =dia0, y = !!id_sym,label = paste(round(days_duration, 2), "days")))+
    ggplot2::scale_colour_brewer(palette = "Set1")+
    ggplot2::theme(legend.position="top",legend.background =  ggplot2::element_rect(fill="gray80",size=1, linetype="solid", colour ="black"))

  if (!is.na(add_point)) {
    figura<-figura+
      ggplot2::geom_point(ggplot2::aes(!!rlang::sym(add_point),!!id_sym),size=3,shape=8,colour="red") +
      ggplot2::geom_text(vjust = -0.5, hjust=0, size = 2,ggplot2::aes(x =!!rlang::sym(add_point), y = !!id_sym,label = add_point))
  }

  if (!is.na(add_final)) {
    figura<- figura + ggplot2::geom_point(ggplot2::aes(diaf, !!id_sym,colour=!!rlang::sym(add_final) %>% as.factor()))
  }

  figura

}



#' @title                    MAP punts ggplot
#' @description              MAP punts ggplot
#' @param dt                 dt
#' @param id                 id
#' @param datainicial        datainicial
#' @param val                val
#' @param grup_color         grup_color
#' @param Nmostra            Nmostra
#' @param llavor             llavor
#' @param finestraX          finestraX
#' @param id_AGG             id_AGG
#' @export                   MAP_punts_ggplot
MAP_punts_ggplot<-function(
  dt="mostra50",
  id="idp",
  datainicial="dat",
  val="val",
  grup_color="agr",
  Nmostra=Inf,
  llavor=123,
  finestraX=c(-Inf,+Inf),
  id_AGG=F
)
{
  # NUMERO_26)

  #  Analitiques (Y=Individu, X=data, Tamany=Valor, Color=tipus analitica) -----------------
  #

  # dt=VARIABLES
  # id="idp"
  # datainicial ="dat"
  # val="val"
  # grup_color = "cod"
  # Nmostra = 2
  # finestraX=c(-Inf,+Inf)
  # llavor=126
  # id_AGG=T


  if (finestraX[1]==-Inf) porca1<-min(dt %>%  dplyr::pull(datainicial))  %>% lubridate::ymd()
  if (finestraX[2]==+Inf) porca2<-max(dt %>%  dplyr::pull(datainicial))  %>% lubridate::ymd()

  if (finestraX[1]!=-Inf) porca1<-finestraX[1] %>% lubridate::ymd()
  if (finestraX[2]!=+Inf) porca2<-finestraX[2] %>% lubridate::ymd()


  # Interpretacio com a parametre
  grup_color<-rlang::sym(grup_color)
  datainicial<-rlang::sym(datainicial)
  id<-rlang::sym(id)
  val<-rlang::sym(val)

  # Converteix data a data inicial
  dt<-dt %>% dplyr::mutate(dat=lubridate::ymd(!!datainicial))

  # Cal estandarditzar valor

  # Llista de nombre d'analitiques
  analitiques_list<-dt%>%dplyr::distinct(!!grup_color)%>%dplyr::pull()

  set.seed(llavor) # S'ha d'actualitzar
  #
  id_sample<-dt %>% dplyr::distinct(!!id) %>% dplyr:: sample_n(size=Nmostra)
  dt<-id_sample %>% dplyr::left_join(dt,by=dplyr::quo_name(id)) #


  # Construccio del identificador id-grup

  if (id_AGG){
    dt<-dt%>%dplyr::mutate(id_plot=paste0(stringr::str_sub(!!id,1,6),!!grup_color),id_num=as.numeric(factor(!!id)))
  }
  if (id_AGG ==F) {
    dt<-dt%>%dplyr::mutate(id_plot=paste0(stringr::str_sub(!!id,1,6)),id_num=as.numeric(factor(!!id))) }

  ggplot2::ggplot(dt,ggplot2::aes(x =!!datainicial,y =id_plot,color=!!grup_color))+
    ggplot2::geom_point(ggplot2::aes(!!datainicial, id_plot)) +
    ggplot2::geom_point(ggplot2::aes(size = !!val))+
    ggplot2::labs(title = "Historic de determinacions")+ggplot2::theme(plot.title = ggplot2::element_text(size=30,hjust = 0.5))+

    ggplot2::theme(axis.text = ggplot2::element_text(colour = "black",size = 10))+
    ggplot2::theme(panel.grid.major = ggplot2::element_line(colour = "grey80",size=0.001))+
    ggplot2::theme(axis.line = ggplot2::element_line(colour = "black",size = 0.9))+

    ggplot2::scale_colour_brewer(palette = "Set1")+
    ggplot2::xlim(porca1,porca2)+
    ggplot2::geom_text(vjust = -0.5, hjust=0, size = 3,ggplot2::aes(x =!!datainicial, y =id_plot,label = paste(round(!!val, 2),""))) +
    ggplot2::theme(legend.position="top",legend.background = ggplot2::element_rect(fill="gray80",size=1, linetype="solid", colour ="black"))+
    ggplot2::scale_y_discrete(breaks= dt %>%  dplyr::pull(id_plot),labels=dt %>% dplyr:: pull(id_num))
  #

}


#' @title                    MAP valor ggplot
#' @description              MAP valor ggplot
#' @param dt                 dt
#' @param id                 id
#' @param datainicial        datainicial
#' @param val                val
#' @param grup_color         grup_color
#' @param Nmostra            Nmostra
#' @param finestraX          finestraX
#' @param llavor             llavor
#' @param title              title
#' @export                   MAP_valor_ggplot
MAP_valor_ggplot<-function(
  dt="mostra50",
  id="idp",
  datainicial="dat",
  val="val",
  grup_color="agr",
  Nmostra=1,
  finestraX=c(-Inf,Inf),
  llavor=123,
  title="Evolucio de valors"
)
{
  # NUMERO_27)

  #  Analitiques (Y=Individu, X=data, Tamany=Valor, Color=tipus analitica) -----------------

  # dt=VARIABLES %>% dplyr::filter(cod %in% c("HBA1C"))
  # datainicial ="dat"
  # id="idp"
  # val="val"
  # grup_color = "cod"
  # Nmostra = 4
  # finestraX=c(-Inf,Inf)
  # llavor=126


  if (finestraX[1]==-Inf) {porca1<-min(dt %>%  dplyr::pull(datainicial)) %>% lubridate::ymd()}
  if (finestraX[2]==+Inf) {porca2<-max(dt %>% dplyr:: pull(datainicial)) %>% lubridate::ymd()}
  if (finestraX[1]!=-Inf) {porca1<-finestraX[1] %>% lubridate::ymd()}
  if (finestraX[2]!=+Inf) {porca2<-finestraX[2] %>% lubridate::ymd()}


  # Interpretacio com a parametre
  grup_color<-rlang::sym(grup_color)
  datainicial<-rlang::sym(datainicial)
  id<-rlang::sym(id)
  val<-rlang::sym(val)

  # Formatejo a data
  dt<-dt %>% dplyr::mutate(dat=lubridate::ymd(!!datainicial))

  # Llistat de codis d'analitiques
  analitiques_list<-dt%>%dplyr::distinct(!!grup_color)%>%dplyr::pull()

  set.seed(llavor) # S'ha d'actualitzar

  # Seleccionar sample
  id_sample<-dt%>% dplyr::distinct(!!id) %>% dplyr::sample_n(size=Nmostra)
  dt<-id_sample %>% dplyr::left_join(dt,by=dplyr::quo_name(id)) #
  #

  # Construccio del identificador id-grup
  dt<-dt%>%dplyr::mutate(id_plot=paste0(stringr::str_sub(!!id,1,6),!!grup_color))

  # Grafica plot de la variable
  ggplot2::ggplot(dt,ggplot2::aes(x =!!datainicial,y =id_plot,color=id_plot))+

    ggplot2::geom_line(ggplot2::aes(!!datainicial, !!val))+

    ggplot2::geom_point(ggplot2::aes(!!datainicial, !!val),color="black")+

    ggplot2::labs(title = title)+ ggplot2::theme(plot.title = ggplot2::element_text(size=25,hjust = 0.5))+

    ggplot2::theme(axis.text = ggplot2::element_text(colour = "black",size = 10))+
    ggplot2::theme(panel.grid.major = ggplot2::element_line(colour = "grey80",size=0.001))+
    ggplot2::theme(axis.line = ggplot2::element_line(colour = "black",size = 0.9))+

    ggplot2::scale_colour_brewer(palette = "Set1")+
    ggplot2::xlim(porca1,porca2)+

    ggplot2::geom_text(vjust = -0.5, hjust=0, size = 3,ggplot2::aes(x =!!datainicial, y =!!val,label = paste(round(!!val, 2),""))) +
    ggplot2::theme(legend.position="top",legend.background = ggplot2::element_rect(fill="gray80",size=1, linetype="solid", colour ="black"))
}



#' @title                    HR.COX
#' @description              HR.COX
#' @param x                  x
#' @param event              event
#' @param t                  t
#' @param e                  e
#' @param d                  d
#' @param taulavariables     taulavariables
#' @param c                  c
#' @param ...                 Altres parametres
#' @export                   HR.COX
HR.COX<-function(x="",
                 event="EV.INSUF_CARD",
                 t="tmp_insuf_card",
                 e="",
                 d="dadesDF",
                 taulavariables="variables.xls",
                 c="",...) {
  # NUMERO_28)

  #  HR.COX  --------------------
  ####      funcio que retorna MATRIU-->Ngran, Events, HR, IC951, IC952, p

  # x=""
  # event = "event_tbc"
  # t="temps_tbc"
  # d=dades
  # taulavariables ="conductor_variables"
  # e=""
  # c="case.id"

  if (c=="") posicio_p=5
  if (c!="") posicio_p=6

  pepito<-paste("sum(d$",t,")",sep="")
  PT<-eval(parse(text=pepito))

  result=tryCatch({
    # pp<-survival::coxph(formulaCOX(x=x,event=event,temps=t,elimina=e,taulavariables = taulavariables),data=d)
    pp<-survival::coxph(formulaCOX(x=x,event=event,temps=t,elimina=e,cluster=c,taulavariables = taulavariables,...),data=d)

    cbind(N=pp$n,
          EVENTS=pp$nevent,
          HRadjusted=summary(pp)$coef[,2],
          IC951=summary(pp)$conf.int[,3],
          IC952=summary(pp)$conf.int[,4],
          p=summary(pp)$coef[,posicio_p])}

    ,error = function(e)  {
      cbind(N=0,
            EVENTS=0,
            HRadjusted=NA,
            IC951=NA,
            IC952=NA,
            p=NA)}

  )
  if  (is.null(rownames(result))) rownames(result)<-"grup"
  result
}



#' @title                    HR.COX.CRU
#' @description              HR.COX.CRU
#' @param x                  x
#' @param event              event
#' @param t                  t
#' @param e                  e
#' @param d                  d
#' @param variables          variables
#' @param evento             evento
#' @export                   HR.COX.CRU
HR.COX.CRU<-function(x="lipos",
                    event="EVENT_MCV",
                    t="temps_exitus",
                    e="",
                    d="dadesDF",
                    variables="variables_R.xls",
                    evento="Si") {
  # NUMERO_29)

  #  HR CRUS ------------------

  # x="Baseline"
  # event="RD"
  # t="TEMPS_RD2"
  # d=dadestotal
  # variables=conductor_variables
  # evento="1"

  bd.camps<-selectorvariables(x,dt=d,taulavariables=variables)
  camps<-names(bd.camps)
  num_camps<-length(names(bd.camps))

  poco<-cbind()

  for (i in 1:num_camps) {

    # i<-1

    xx<-camps[i]

    rr<-paste("Surv(",t,", as.integer(",event," == ",evento,"))~",xx,sep="")
    pp<-survival::coxph(eval(parse(text=rr)),data=d)


    mama<-cbind(N=pp$n,
                EVENTS=pp$nevent,
                HRcrude=summary(pp)$coef[,2],
                IC951=summary(pp)$conf.int[,3],
                IC952=summary(pp)$conf.int[,4],
                p=summary(pp)$coef[,5])

    rownames(mama)<-names(pp$coefficients)
    poco<-rbind(poco,mama)
  }

  poco

}


#' @title                    Extreure HRFG
#' @description              Extreure HRFG
#' @param event              event
#' @param temps              temps
#' @param grup               grup
#' @param eventcompetitiu    eventcompetitiu
#' @param dt                 dt
#' @param covariables        covariables
#' @param codievent          codievent
#' @param refcat             refcat
#' @param cluster            cluster
#' @export                   extreure_HRFG
extreure_HRFG<-function(event="exitusCV",
                       temps="temps_seguiment",
                       grup="diabetis",
                       eventcompetitiu="exitus",
                       dt="dades",
                       covariables=NA,
                       codievent="Si",
                       refcat=NA,
                       cluster=""){
  # NUMERO_30)

  # HR RISCOS COMPETITIUS  -------------
  # Funcio Riscos competitius Fine & Grey
  # Donat un event, temps de seguiment, grup, eventcompetitiu retorna tibble:
  # Beta, SE, p-value, HR, Li95%CI, Ls95%CI
  # Afegit cluster

  # dt=dades
  # event="event_tbc"
  # temps="temps_tbc"
  # eventcompetitiu="exitus"
  # codievent="1"
  # grup="grup"
  # cluster="case.id"
  # # strata=NULL
  # refcat="Control"
  # # # covariables=c("sexe","edat")
  # covariables=NA
  # # # covariables=variablesajust
  #
  event<-dplyr::sym(event)
  temps<-dplyr::sym(temps)
  grup<-dplyr::sym(grup)
  eventcompetitiu<-dplyr::sym(eventcompetitiu)
  # Selecciono variables necessaries ()
  if (cluster=="") {
    if (any(is.na(covariables)))   dt<-dt %>% dplyr::select(grup=!!grup,exitus=!!eventcompetitiu,temps=!!temps,event=!!event)
    if (!any(is.na(covariables)))  dt<-dt %>% dplyr::select(grup=!!grup,exitus=!!eventcompetitiu,temps=!!temps,event=!!event,dplyr::all_of(covariables)) }

  if (cluster!="") {
    if (any(is.na(covariables)))   dt<-dt %>% dplyr::select(grup=!!grup,exitus=!!eventcompetitiu,temps=!!temps,event=!!event,cluster=!!cluster)
    if (!any(is.na(covariables)))  dt<-dt %>% dplyr::select(grup=!!grup,exitus=!!eventcompetitiu,temps=!!temps,event=!!event,dplyr::all_of(covariables),cluster=!!cluster) }

  # Generar variable status (tipo de censuras) ----
  dt<-dt %>% dplyr::mutate(status=dplyr::case_when(event==codievent ~"event",
                                     event!=codievent & exitus==codievent~"Mortality",
                                     event!=codievent & exitus!=codievent~"Censored"))

  # Generar matriu de covariables
  # Cambiar categoria de referencia de grup a No
  dt[[grup]]<-as.factor(dt[[grup]])
  if (!is.na(refcat)) dt[[grup]] = stats::relevel(dt[[grup]], refcat)

  # Afegir variable grup a covariables
  covariables<-c("grup",covariables)
  cov1 <- stats::model.matrix(formula_vector(covariables,""),data = dt)[, -1]

  # Codificar riscos competitius / competitius per clusters
  if (cluster=="")
    model<-cmprsk::crr(ftime=dt$temps,
                       fstatus=dt$status,
                       cov1=cov1 , #  matrix (nobs x ncovs) of fixed covariates
                       failcode = "event", # code of fstatus that denotes the failure type of interest
                       cencode = "Censored") else # code of fstatus that denotes censored observations
                         model<-crrSC::crrc(ftime=dt$temps,fstatus=dt$status,cov1=cov1,cluster=dt$cluster, failcode = "event", cencode = "Censored")

  tab <- summary(model)$coef[1,]
  x <- round(cbind("beta" = tab[1],
                   "SE" = tab[3],
                   "p-value" = tab[5],
                   "HR" = tab[2],
                   "LI" = exp(tab[1] - stats::qnorm(1 - (1-0.95)/2)*tab[3]),
                   "LS" = exp(tab[1] + stats::qnorm(1 - (1-0.95)/2)*tab[3])), 4)
  colnames(x) <- c("Beta", "SE", "p-value", "HR", "Li95%CI", "Ls95%CI")
  rownames(x) <- rownames(tab)


  dplyr::as_tibble(x)

}

#' @title                    Extreure model cmprisk
#' @description              Extreure model cmprisk
#' @param dt                 dt
#' @param event              event
#' @param temps              temps
#' @param competitiu         competitiu
#' @param codievent          codievent
#' @param covariables        covariables
#' @export                   extreure_model_cmprisk
extreure_model_cmprisk<-function(dt=dades,
                                 event="amputacio_cat",
                                 temps="t_lliure_amputa",
                                 competitiu="Ha_muerto",
                                 codievent="Yes",
                                 covariables=c("Sexo","Edad","Tabaquismo")) {
  # NUMERO_31)

  # dt=dades
  # event="amputacio_cat"
  # temps="t_lliure_amputa"
  # competitiu="Ha_muerto"
  # codievent="Yes"
  # covariables=c("Sexo","Edad","Tabaquismo")

  event<-dplyr::sym(event)
  temps<-dplyr::sym(temps)
  competitiu<-dplyr::sym(competitiu)

  # Extreure variables i formatar
  dt <- dt %>% dplyr::select(temps=!!temps,event=!!event,exitus=!!competitiu,dplyr::all_of(covariables)) %>% stats::na.omit()

  dt<-dt %>% dplyr::mutate(event=as.numeric(event==codievent),
                    exitus=as.numeric(exitus==codievent))

  codi_event=as.numeric(codievent==codievent)
  codi_Noevent=as.numeric(codievent!=codievent)

  # Generar variable status (tipo de censuras) ----
  dt<-dt %>% dplyr::mutate(status=dplyr::case_when(event==codi_event ~"Event",
                                     event==codi_Noevent & exitus==codi_event~"Mortality",
                                     event==codi_Noevent & exitus==codi_Noevent~"Censored"))
  # Converir matriu de covariables
  cov1 <- stats::model.matrix(formula_vector(covariables,""),data = dt)[, -1]

  # Ajust de model
  model<-cmprsk::crr(ftime=dt$temps,
                     fstatus=dt$status,
                     cov1=cov1 , #  matrix (nobs x ncovs) of fixed covariates
                     failcode = "Event", # code of fstatus that denotes the failure type of interest
                     cencode = "Censored") # code of fstatus that denotes censored observations
  model
}


#' @title                    Extreure cuminc cmprisk
#' @description              Extreure cuminc cmprisk
#' @param dt                 dt
#' @param event              event
#' @param temps              temps
#' @param competitiu         competitiu
#' @param codievent          codievent
#' @param group              group
#' @param strata             strata
#' @export                   extreure_cuminc_cmprisk
extreure_cuminc_cmprisk<-function(dt=dades,
                                  event="amputacio_cat",
                                  temps="t_lliure_amputa",
                                  competitiu="Ha_muerto",
                                  codievent="Yes",
                                  group=NULL,
                                  strata=NULL) {
  # NUMERO_32)

  ## Extreure cuminc competitive risk

  # dt=dades
  # event="amputacio_cat"
  # temps="t_lliure_amputa"
  # competitiu="Ha_muerto"
  # codievent="Yes"
  # group=NULL
  # strata=NULL

  event<-dplyr::sym(event)
  temps<-dplyr::sym(temps)
  competitiu<-dplyr::sym(competitiu)

  # Extreure variables i formatar
  dt <- dt %>% dplyr::select(temps=!!temps,event=!!event,exitus=!!competitiu,group,strata) %>% stats::na.omit()

  dt<-dt %>% dplyr::mutate(event=as.numeric(event==codievent),
                    exitus=as.numeric(exitus==codievent))

  codi_event=as.numeric(codievent==codievent)
  codi_Noevent=as.numeric(codievent!=codievent)

  # Generar variable status (tipo de censuras) ----
  dt<-dt %>% dplyr::mutate(status=dplyr::case_when(event==codi_event ~"Event",
                                     event==codi_Noevent & exitus==codi_event~"Mortality",
                                     event==codi_Noevent & exitus==codi_Noevent~"Censored"))
  # cuminc
  cmprsk::cuminc(ftime=dt$temps,
                 fstatus=dt$status,
                 cencode = "Censored") # code of fstatus that denotes censored observations

}

#' @title                    Extreure coef cmprisk
#' @description              Extreure coef cmprisk
#' @param model_cmrisk       model_cmrisk
#' @export                   extreure_coef_cmprisk
extreure_coef_cmprisk<-function(model_cmrisk){

  # NUMERO_33)


  # D'un model cmprisk extrec coeficients

  summary(model_cmrisk)$coef %>%
    as.data.frame() %>%
    dplyr::transmute(Variable=row.names(.),
              HR=`exp(coef)`,
              IC95_Linf=exp(coef-1.97*`se(coef)`),
              IC95_Lsup=exp(coef+1.97*`se(coef)`),
              `p-value`) %>% dplyr::as_tibble()
}

#' @title                    Extreure cor
#' @description              Extreure cor
#' @param var1               var1
#' @param var                var
#' @param d                  d
#' @param taulavariables     taulavariables
#' @param ...                Altres parametres
#' @export                   extreure_cor
extreure_cor<-function(var1="CD36",
                       var="quantis",
                       d="dades",
                       taulavariables="VARIABLES.xls",...) {

  # NUMERO_34)

  # CORRELACIONS, P VALORS ENTRE var1 i llista de quantis de dades  --------------

  # var1="HbA1c"
  # var="lipos2"
  # d="dades"
  # taulavariables="VARIABLES.xls"
  # var1="alb24hurine_value"
  # var="lipos_corr"
  # d="dades"
  # taulavariables = "conductor_variables"

  ##  Llegeix criteris de variables
  variables <- readxl::read_excel(taulavariables) %>% tidyr::as_tibble()
  # variables[is.na(variables)]<- 0
  var_sym<-rlang::sym(var)
  variables<-variables %>% dplyr::filter(!is.na(!!var_sym))

  llistavariables<-eval(parse(text=paste("variables$camp[variables$",var,">0]",sep="")))

  # llistavariables<-variables$camp[variables$var==1]
  x<-eval(parse(text=paste(d,"$",var1,sep="")))

  ppp<-cbind()
  for (i in 1:length(llistavariables)) {

    var2<-paste(d,llistavariables[i],sep="$")
    y<-eval(parse(text=var2))
    stats::cor.test(x,y)$estimate
    correlacio<-stats::cor.test(x,y)$estimate
    pvalor<-stats::cor.test(x,y)$p.value

    pp<-cbind(correlacio,pvalor)
    row.names(pp)<-llistavariables[i]
    ppp<-rbind(ppp,pp)
  }

  ppp
}



#' @title                     Extreure cor multi
#' @description               Extreure cor multi
#' @param dades               dades
#' @param llistavar1          llistavar1
#' @param llistavar2          llistavar2
#' @param etiquetar           etiquetar
#' @param coductor_variables  coductor_variables
#' @param method              method
#' @param sheet               sheet
#' @param ...                 Altres parametres
#' @export                    extreure_cor_multi
extreure_cor_multi<-function(dades=dt,
                             llistavar1=c("Age","BMI"),
                             llistavar2=c("Large_PER_HDL","Medium_HDL_P_molL"),
                             etiquetar=F,
                             conductor_variables="conductor_variables",
                             method = "circle",sheet=NULL,...){
  # NUMERO_35)

  # Correlacions , matriu i plot de quantis de dades  ----------------------

  # Retorna matriu de correlacions, i plot bivariant (Correlograma) de ggcorrplot
  # Requereix dades, llista1, llista2

  # dades=dt_temp
  # llistavar1 = llistat_var
  # llistavar2 = llistat_var
  # conductor_variables=path_conductor
  # method = "square"
  # etiquetar = T
  # sheet="VarQV"

  # dt=dt
  # llistavar1=vars1
  # llistavar2=vars2
  # conductor_variables="conductor_variables"
  # etiquetar=T
  # method = "square"

  # Seleccio de variables
  dt<-dades %>% dplyr::select(all_of(llistavar1),all_of(llistavar2))

  # Genero matriu
  corr_temp<-stats::cor(dt,use="pairwise",method="pearson")

  # Convertir matriu a tibble
  corr_temp<-dplyr::as_tibble(row.names(corr_temp)) %>% cbind(corr_temp) %>% dplyr::as_tibble()

  # Filtrar matriu
  # En cas de llistavar2 llavors filtrar dades a matriciar
  corr_temp<-corr_temp %>% dplyr::filter (value%in%llistavar1) %>% dplyr::select("value",llistavar2)

  # Generar plot
  # 1. Ho converteixo en matriu i capturo noms de files
  M<-as.matrix(dplyr::select(corr_temp,-1))
  rownames(M) <- llistavar1
  # colnames(M) <- llistavar2

  # Etiquetar variable
  if (etiquetar) {
    # Si etiquetar llavors capturar etiquetes de conductor

    dt_conductor_variables<-read_conductor(conductor_variables,sheet=sheet)
    rownames(M)<-etiquetar_taula(dplyr::as_tibble(llistavar1),camp="value",taulavariables=dt_conductor_variables,camp_descripcio= "descripcio") %>%  dplyr::pull(value)
    colnames(M)<-etiquetar_taula(dplyr::as_tibble(llistavar2),camp="value",taulavariables=dt_conductor_variables,camp_descripcio= "descripcio") %>% dplyr:: pull(value)

  }

  # Ploto el tema
  # corrplot<-ggcorrplot::ggcorrplot(M,method = "circle",type=c("full"),lab_col = "black",colors = c("red", "white", "black"),outline.color = "grey")
  corrplot<-ggcorrplot::ggcorrplot(M,method = method,...)

  # Retorno llista d'objectes (MAtriu i plot)
  list(matriu=corr_temp,plot=corrplot)
}





#' @title                     Extreure OR
#' @description               Extreure OR
#' @param formu               formu
#' @param dades               dades
#' @param conditional         conditional
#' @param strata              strata
#' @export                    extreure_OR
extreure_OR<- function (formu="AnyPlaqueBasal~CD5L",
                        dades="dt",
                        conditional=F,
                        strata="caseid") {

  # NUMERO_36)

  #  Extreure OR (segons formula, i dades)  --------------------
  #       LLANÇO UNA FORMULA les dades per executar un model i retorno OR , CI95% i p-valor en una tibble()

  # formu<-formula.LOGIT(x="article.model",y="canvi312M.GLICADA.inputCAT2",taulavariables='variables_v2.xls')
  # dades=tempData

  # formu=formula
  # dades=dades
  # conditional=F
  # strata="caseid"
  modelcomplet=T

  dades_resum<-dplyr::as_tibble()

  # Si dades NO son dades imputadesl
  if (class(dades)[1]!="mids") {


    # Model logistic / logistic condicional
    if (conditional==F) {
      fit<-stats::glm(formu, family = stats::binomial, data=dades)
    } else {

      formu<- paste0(formu,"+ strata(",strata,")")
      fit<-survival::clogit(stats::as.formula(formu),data=dades)}

    # Extrec info total del model
    my_coefficients <- fit %>% stats::coef
    ci<-fit %>% stats::confint
    OR<-my_coefficients %>% exp()
    OR_linf<-ci %>% exp()
    pvalors<-stats::coef(summary(fit))[,'Pr(>|z|)']
    coeficients<-cbind(OR,OR_linf,pvalors) %>% dplyr::as_tibble
    ret_val <- tibble::enframe(row.names(ci)) %>% dplyr::bind_cols(coeficients)
    colnames(ret_val) <- c("id","Categoria","OR","Linf", "Lsup", "p.value")
    dades_resum<-ret_val %>% dplyr::as_tibble

  }

  dades_resum

  # Si son dades imputades tipo mids de MICE
  if (class(dades)[1]=="mids"){

    pepe<-paste(formu[2],formu[3],sep='~')

    resum<-with(tempData,stats::glm(eval(parse(text=pepe)),family = stats::binomial(link="logit"))) %>% mice::pool() %>% summary ()

    ret_val<-cbind(categoria=row.names(resum)) %>% cbind(resum) %>%dplyr:: as_tibble

    # Capturar OR, etc...
    dades_resum<-ret_val %>% dplyr::mutate(OR=estimate %>% exp,
                                    Linf=(estimate-std.error) %>% exp,
                                    Lsup=(estimate+std.error) %>% exp) %>%
      dplyr::select(categoria,OR,Linf,Lsup,p.value)
  }

  dades_resum

}


#' @title                     Generar taula variables formula
#' @description               Generar taula variables formula
#' @param formu               formu
#' @param dades               dades
#' @export                    generar_taula_variables_formula
generar_taula_variables_formula<-function(formu="AnyPlaqueBasal~CD5L",
                                          dades=dt) {

  # NUMERO_37)

  # Taula variables segons formula i dades genera la taula de coeficients

  # formu=formu
  # dt=dades

  taula_editada<-
    all.vars(formu)[-1] %>%
    purrr::map(~paste0(.x,levels(dades[[.x]]),"/",.x)) %>%
    unlist() %>%
    dplyr::tibble() %>% dplyr::rename("var"=".") %>%
    tidyr::separate(col=var, into=c("Categoria","Variable"), sep = "/") %>%
    dplyr::mutate(nivell=stringr::str_remove(Categoria,Variable),
           tipo=dplyr::if_else(nivell=="","Continua","Cat"))
}


#' @title                     Extreure model logistic
#' @description               Extreure model logistic
#' @param x                   x
#' @param y                   y
#' @param taulavariables      taulavariables
#' @param dades               dades
#' @param elimina             elimina
#' @param a                   a
#' @param valor_outcome       valor_outcome
#' @param conditional         conditional
#' @param strata              strata
#' @export                    extreure_model_logistic
extreure_model_logistic<-function(x="OS4_GSK",
                                  y="canvi6M.glipesCAT2",
                                  taulavariables="conductorvariables",
                                  dades=dades,
                                  elimina=c("IDP"),
                                  a="",
                                  valor_outcome="Yes",
                                  conditional=F,
                                  strata="caseid") {
  # NUMERO_38)

  # Retorno model amb ORs, curva ROC , auc IC95% etc... a partir de formula glm , i dades

  # x="regicor_alone_continu"
  # y="event"
  # taulavariables=conductor_variables
  # dades=dades_temp
  # valor_outcome=cat_event
  # conditional = T
  # strata = "caseid"


  # a=""
  # valor_outcome="Caso"
  # conditional = T
  # strata = "caseid"
  # x="regicor_vars2"
  # y="event"
  # taulavariables=conductor_variables
  # dades=dades_temp
  # elimina=c("IDP")

  # Factoritzar character a factor
  covariables<-extreure.variables(x,taulavariables)
  covariables_character<-dades %>% dplyr::select_at(covariables) %>% dplyr::select_if(is.character) %>% names()
  dades<-dades %>% dplyr::mutate_at(covariables_character,as.factor)

  # Eliminar variable que no hi ha com a minim 2 nivells
  var_eliminar<-dades %>% dplyr::select_at(covariables) %>% dplyr::select_if(is.factor) %>% purrr::map(~length(unique(.x)))
  var_eliminar<-var_eliminar[var_eliminar==1] %>% names()
  print(paste0("Eliminada del model: ", var_eliminar))

  # Ojo que variables no factoritzades --> error
  formu=formula.LOGIT(x=x,y=y,taulavariables=taulavariables,eliminar = var_eliminar)
  #formu_text<-formula.text(x=x,y=y,taulavariables=taulavariables,eliminar = var_eliminar)
  formu_text<-formula_text(x=x,y=y,taulavariables=taulavariables,eliminar = var_eliminar)

  # Subselecciono dades completes amb nomes variables utilitzades i elimino nivells sense utilitzar (Sino peta en ROC curve)
  if (conditional) {dades<-dades %>% dplyr::select(c(all.vars(formu),strata)) %>% stats::na.omit()}
  if (conditional==F) {dades<-dades %>% dplyr::select(c(all.vars(formu))) %>% stats::na.omit()}
  # Eliminar nivells que no tenim dades de variables factor
  dades<-dades %>% dplyr::mutate_if(is.factor, droplevels)

  resposta<-all.vars(formu)[1]
  fit<-stats::glm(formu, family = stats::binomial, data=dades)

  # Customitzo el valor del outcome
  #formu_text<-formula.text(x=x,y=paste0(y,"=='",valor_outcome,"'"),taulavariables=taulavariables,eliminar = var_eliminar)
  formu_text<-formula_text(x=x,y=paste0(y,"=='",valor_outcome,"'"),taulavariables=taulavariables,eliminar = var_eliminar)

  if (conditional==F) {
    taula_OR<-extreure_OR(formu=formu,dades=dades,conditional=conditional,strata=strata)
  } else {
    taula_OR<-extreure_OR(formu=formu_text,dades=dades,conditional=conditional,strata=strata)
    fit_c<-survival::clogit(stats::as.formula(paste0(formu_text,"+ strata(",strata,")")),data=dades)
  }

  taula_editada<-generar_taula_variables_formula(formu,dades)

  # juntar taula_OR + taula editada --> etiquetar i editar
  taula_editada<-taula_editada %>%
    dplyr::left_join(taula_OR,by="Categoria") %>%
    dplyr::mutate(nivell=dplyr::if_else(is.na(OR),paste0(" Ref:",nivell),nivell),
           OR=dplyr::if_else(is.na(OR),1,OR),
           Linf=dplyr::if_else(is.na(Linf),1,Linf),
           Lsup=dplyr::if_else(is.na(Lsup),1,Lsup),
           nivell=stringr::str_trim(nivell)) %>%
    dplyr::filter (!is.na(id)) %>% # Eliminar cat de referencia
    etiquetar_taula("Variable",taulavariables,"descripcio",...) %>%
    dplyr::mutate(Variable=dplyr::if_else(tipo=="Cat",paste0(Variable,":",nivell),Variable)) %>%
    dplyr::select(Categoria=Variable,OR,Linf,Lsup,p.value)

  forest_plot<-forest.plot(taula_editada)

  dades_prediccio<-
    data.frame(prediccio=predict(fit,dades, type=c("response")),known.truth=dades %>% dplyr:: pull(resposta)) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(event=as.numeric(known.truth==valor_outcome)) %>%
    dplyr::filter(!is.na(event) & !is.na(prediccio))

  if (conditional) {
    predict_clogit<-data.frame(logit_pred=predict(fit_c,type = "lp")) %>%
      dplyr::mutate(prob_pred=boot::inv.logit(logit_pred))

    dades_prediccio<-dades_prediccio %>%
      cbind(predict_clogit) %>% dplyr::select(-prediccio) %>% dplyr::rename(prediccio=prob_pred)
  }

  g <- pROC::roc(event ~ prediccio, data = dades_prediccio)

  auc=pROC::auc(g)
  auc_ci=pROC::ci(g)

  plot_curve<-
    ggplot2::ggplot(dades_prediccio, ggplot2::aes(d = event, m = prediccio)) +
    plotROC::geom_roc(n.cuts = 0)

  plot_curve<- plot_curve +
    # ggplot::annotate("text", x = .75, y = .25, label = paste("AUC =", round(plotROC::calc_auc(plot_curve)["AUC"], 2))) +
    ggplot2::annotate("text", x = .75, y = .25, label = paste("95 CI%:",round(auc_ci[1],2),"-",round(auc_ci[3],2)))

  HL_test<-ResourceSelection::hoslem.test(dades_prediccio$event, dades_prediccio$prediccio, g = 10)


  popes<-list(taula_OR=taula_editada,forest_plot=forest_plot,ggplot_ROC=plot_curve,auc=auc,auc_ci=auc_ci,HL_test=HL_test)

}
#



#' @title                     Resum3
#' @description               Resum3
#' @param dt                  dt
#' @param x                   x
#' @param estrat              estrat
#' @export                    resum3
resum3<-function(dt=dades,
                 x="val_last.HBA1C",
                 estrat="constant"){
  # NUMERO_39)


  #  Resum d'un data.table (Mitjana, DT, N etc...)  --------------------

  ######         RESUM D'UN DATA.TABLE

  ###   LLANÇO UN DT, VARIABLE I UNA ESTRATIFICACIo I EM TORNA UN DT AMB un resum

  ### mitjana, DT, N etc... per cada ESTRAT


  dt$constant<-1

  e<-parse(text=x)

  resum3<-dt[, .(
    Mean=mean(eval(e),na.rm=T),
    SD=stats::sd(eval(e),na.rm=T),
    Nmenor7=sum(eval(e)<7,na.rm=T),
    Perc_menor7=(sum(eval(e)<7,na.rm=T)/length(which(eval(e) != "NA")))*100,
    N=length(eval(e))
  )
  ,by=estrat]

  resum3
}


#' @title                   Resum quanti
#' @description             Resum quanti
#' @param dt                dt
#' @param y                 y
#' @param grup              grup
#' @export                  resum_quanti
resum_quanti<-function(dt=dades,
                       y="valor_basal.GLICADA",
                       grup="constant") {
  # NUMERO_40)

  #  Resum quanti  -------------------------
  #####     funcio que retorna un summary (mean, sd) de y en funcio d'un grup

  dt$constant=1

  # dt=data_long
  # y="valor_basal.GLICADA"
  # grup="SEXE"

  ### extrect p valor
  pepito=paste0("summary(aov(",y,"~",grup,",data=dt))[[1]][['Pr(>F)']]",sep="")
  pvalor<-eval(parse(text=pepito))[1]

  summ1 <- paste0('mean(', y, ',na.rm=T)')
  summ2<-paste0('sd(',y,',na.rm=T)')

  dt %>% dplyr::group_by_(grup) %>%
    dplyr::summarise_(mean=summ1,
                      sd=summ2,
                      n="n()") %>%
    dplyr::mutate(p=pvalor) %>%
    dplyr::rename("group"=grup)

}


#' @title                   resum_quanti_estrat
#' @description             resum_quanti_estrat
#' @param dt                dt
#' @param y                 y
#' @param grup              grup
#' @param estrat            estrat
#' @export                  resum_quanti_estrat
resum_quanti_estrat<-function(dt=dades,
                              y="valor_basal.GLICADA",
                              grup="CODGLP1",
                              estrat="HBA1C_cat4"){
  # NUMERO_41)

  #  ESTADISTICS RESUMS x grup x estrat ----------------------
  # RETORNA ESTADISTICS RESUMS (mean, sd, p-valor --> ANOVA/t-test) X GRUP  X ESTRAT

  # dt=dades
  # y="valor_basal.GLICADA"
  # grup="CODGLP1"
  # estrat="HBA1C_cat4"

  # dt<-dt %>% dplyr::select_if(names(.)%in%c(y,grup,estrat)) select_if no funciona

  dt<-dt %>% dplyr::select(c(y,grup,estrat))


  if (!"estrat" %in% colnames(dt)) {
    dt<-dt %>% dplyr::mutate (overall="Overall")
    estrat="overall"}

  dt %>%
    tidyr::drop_na(y) %>%
    dplyr::group_by_(estrat) %>%
    dplyr::do(resum_quanti(dt=.,y=y,grup=grup))

}




#' @title                   Resum events
#' @description             Resum events
#' @param dades             dades
#' @param evento            evento
#' @param temps             temps
#' @param valorevent        valorevent
#' @export                  resum_events
resum_events<-function(dades="dadestotal",
                       evento="RD",
                       temps="temps",
                       valorevent="Si") {
  # NUMERO_42)

  #  Resum events  ----------------------
  ###################         Llan?o dades, event i temps i me fa un resum

  # dades="dadesDF"
  # evento="EVENT_MORT2014"
  # temps="temps_mortalitat"
  # valorevent="1"
  # dades=dades
  # evento="RD"
  # temps="TEMPS_RD2"

  Patients=length(dades[[evento]])
  PYears=sum(dades[[temps]])
  temps_seguiment=mean(dades[[temps]])
  N.Events=sum(dades[[evento]]==valorevent)
  Event.rate=((N.Events/PYears)*100)
  IA=(N.Events/Patients)
  resum<-cbind(Patients,PYears,temps_seguiment,N.Events,Event.rate,IA)
  resum
}


#' @title                   Resum events v2
#' @description             Resum events v2
#' @param dades             dades
#' @param evento            evento
#' @param temps             temps
#' @export                  resum_events_v2
resum_events_v2<-function(dades=dades,
                          evento="RD",
                          temps="temps") {
  # NUMERO_43)

  #  Resum events  ----------------------

  # dades=dadestotal
  # evento="RD"
  # temps="TEMPS_RD2"

  Patients=length(dades[[evento]])
  PYears=sum(dades[[temps]])
  temps_seguiment=mean(dades[[temps]])
  N=mean(dades[["N_BREAK"]])
  min=min(dades[["N_BREAK"]])
  max=max(dades[["N_BREAK"]])
  N.Events=sum(dades[[evento]])
  Event.rate=(N.Events/PYears)*100
  IA=(N.Events/Patients)*100

  ### Fusionar tot
  resum<-cbind(Patients,PYears,temps_seguiment,N,min,max,N.Events,Event.rate,IA)
  resum
}



#' @title                   Resum events v3
#' @description             Resum events v3
#' @param dt                dt
#' @param evento            evento
#' @param temps             temps
#' @param valorevent        valorevent
#' @export                  resum_events_v3
resum_events_v3<-function(dt="dadestotal",
                          evento="RD",
                          temps="temps",
                          valorevent="Si") {
  # NUMERO_44)

  # Versio millorada, retorna tibble

  # evento="EV.CVD"
  # temps="EV.CVD_temps"
  # valorevent=1
  # dt=dades

  dt %>% dplyr::summarise(Patients=dplyr::n(),
                   P_Years=sum(!!dplyr::sym(temps)),
                   Years_free_event_mean=mean(!!dplyr::sym(temps)),
                   Years_free_event_median=stats::median(!!dplyr::sym(temps)),
                   N_events=sum(!!dplyr::sym(evento)==valorevent),
                   Event_rate_1000=((N_events/P_Years)*1000),
                   IA_100=(N_events/Patients)*100
  )
}



#' @title                   Resum events grups
#' @description             Resum events grups
#' @param d                 d
#' @param evento            evento
#' @param temps             temps
#' @param grup              grup
#' @export                  resum_events_grup
resum_events_grup<-function(d="dadestotal",
                           evento="RD",
                           temps="TEMPS_RD2",
                           grup="sexe") {

  # NUMERO_45)

  #  Resum events per grup  ------------------
  ##########              Llanço dades, event, temps , grup i retorno un resum d'events per grups

  # d=dadestotal
  # evento="RD"
  # temps="TEMPS_RD2"
  # grup="sexe"
  # valorevent="1"

  pepito=paste0("as.factor(d$",grup,")")
  dadesgrups<-d %>% split(eval(parse(text=pepito)))

  temp<- dadesgrups %>%
    purrr::map(~resum_events_v2(dades=.x,evento=evento,temps=temps)) %>%
    purrr::map(as.data.frame) %>%
    purrr::map_df(dplyr::bind_rows,.id = "Group") %>%
    dplyr::as_tibble()

}


#' @title                     Resum taxa incidencia
#' @description               Resum taxa incidencia
#' @param dt                  dt
#' @param evento              evento
#' @param temps               temps
#' @param valorevent          valorevent
#' @param ...                 Altres parametres
#' @export                    Resum_taxa_incidencia
Resum_taxa_incidencia<-function(dt=dades,
                                evento="event_tbc",
                                temps="anys_lliure_tbc",
                                valorevent="1",...) {
  # NUMERO_46)

  ## Retorna taxa d'incidencia + corresponent IC95

  # dt=dades_long
  # evento="event_tbc"
  # temps="anys_lliure_tbc"
  # valorevent="1"
  Patients=length(dt[[evento]])
  PYears=sum(dt[[temps]])
  N.Events=sum(dt[[evento]]==valorevent)

  pp<-epiR::epi.conf(as.matrix(cbind(N.Events,PYears)),ctype = "inc.rate",method = "exact",N = 1000, design = 1,...)*100000
  cbind(Patients,PYears,N.Events,rate=pp$est,IC95_Linf=pp$lower,IC95_Lsup=pp$upper)%>%dplyr:: as_tibble()
}


#' @title                     Resum taxa incidencia idp
#' @description               Resum taxa incidencia idp
#' @param dt                  dt
#' @param evento              evento
#' @param temps               temps
#' @param valorevent          valorevent
#' @param ...                 Altres parametres
#' @export                    Resum_taxa_incidencia_idp
Resum_taxa_incidencia_idp<-function(dt=dades,
                                    evento="event_tbc",
                                    temps="anys_lliure_tbc",
                                    valorevent="1",...) {
  # NUMERO_47)

  # dades_long %>% Resum_taxa_incidencia_idp(evento="event_tbc_long",temps="tmp_seguiment",valorevent=1)
  # dt=dades_long
  # evento="event_tbc_long"
  # temps="tmp_seguiment"
  # valorevent=1

  Patients_reals=dt %>% dplyr::distinct(idp) %>% dplyr::count() %>% as.numeric()
  N.Events_reals=dt %>% dplyr::filter(!!dplyr::sym(evento)==valorevent) %>% dplyr::distinct(idp) %>% dplyr::count() %>% as.numeric()

  Patients=length(dt[[evento]])
  PYears=sum(dt[[temps]])
  N.Events=dt %>% dplyr::filter(!!dplyr::sym(evento)==valorevent) %>% dplyr::count() %>% as.numeric()

  pp<-epiR::epi.conf(as.matrix(cbind(N.Events,PYears)),ctype = "inc.rate",method = "exact",N = 1000, design = 1,...)*100000
  cbind(Patients_reals,PYears,N.Events_reals,rate=pp$est,IC95_Linf=pp$lower,IC95_Lsup=pp$upper)%>% dplyr::as_tibble()

}


#' @title                     Llistadetaules.compare
#' @description               Llistadetaules.compare
#' @param tablero             tablero
#' @param y                   y
#' @param variables           variables
#' @param dt                  dt
#' @export                    llistadetaules.compare
llistadetaules.compare<-function(tablero=c("taula1","taula2","taula3","taula4","taula5"),
                                 y="sexe",
                                 variables = "variables.xls",
                                 dt=dades){
  # NUMERO_48)


  #  Llistat de Taules compare ------------------
  #   LLISTA DE noms de taules i retorna llista de taules comparatives

  #    Llanço una LLISTA de noms de taules que estan en el Conductor Variables i em retorna una llista de taules ###

  restab.llista<-list()
  for (i in 1:length(tablero)) {
    restab.llista[[i]]<-tablero[i] %>%
      formula_compare(y=y,taulavariables = variables) %>%
      compareGroups::compareGroups(data=dt,include.miss = F,include.label=T) %>%
      compareGroups::createTable(show.ratio = F, hide.no = c('NA','No'), show.p.overall=T,show.n=T,show.all=T)
  }

  restab.llista

}

#' @title                     Pvalors ajustats compare
#' @description               Pvalors ajustats compare
#' @param objecte_compare     objecte_compare
#' @param metodo              metodo
#' @param p                   p
#' @param Sig                 Sig
#' @export                    Pvalors_ajustats_compare
Pvalors_ajustats_compare<-function(objecte_compare="T1.1.2",
                                   metodo="BH",
                                   p="p.overall",
                                   Sig="No") {
  # NUMERO_49)


  #  P-valors ajustats segons multiple test Comparations desde un objecte Compare groups  ------------------

  ### Llanço un objecte compare groups i em retorna els p-valors + els ajustats en una taula

  # p.adjust.methods
  # c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY",
  #   "fdr", "none")
  ##    Ajust BH
  # The "BH" (aka "fdr") and "BY" method of Benjamini, Hochberg, and Yekutieli control the false discovery rate,
  # the expected proportion of false discoveries amongst the rejected hypotheses.
  # The false discovery rate is a less stringent condition than the family-wise error rate, so these methods are more powerful than the others.


  # objecte_compare=T2_Lipos
  # metodo = "bonferroni"
  # metodo = "BH"
  # p="p.overall"
  # p="p.mul"
  # Sig="No"

  # 1. Extrect els p-valors
  pvalors <- compareGroups::getResults(objecte_compare, p)

  # 2. Taula de p vals
  pvals<-data.table::data.table(pvalors)

  # 4. Ajusta p- valors
  # pvals$Adjpvalor<-stats::p.adjust(pvalors, method = metodo)

  pvals<-pvals[,1:ncol(pvals)] %>% purrr::map_df(stats::p.adjust,method = metodo)

  # # 5. Punt de tall
  # pvals<-pvals %>% dplyr::mutate_all(sigBH=ifelse(Adjpvalor<0.05,"Sig","NS"))

  # 5. Canviar a punts de tall si argument Sig="Yes"
  if (any(Sig==c("Yes","Si",1))) pvals<-pvals %>%
    dplyr::mutate_all(dplyr::funs(ifelse(.<0.05,"Sig","NS")))

  # 3. Posa noms
  pvals$variable<-rownames(pvalors)
  if (is.null(rownames(pvalors))) pvals$variable<-names(pvalors)

  pvals %>% dplyr::select(variable,dplyr::starts_with('p'))

  # # 6. Canviar noms
  # pvals<-pvals %>% stats::setNames(c("P.crude","Variable",paste0("Padj.",substr(metodo, 1,3)), paste0("Sig.",substr(metodo, 1,3))))

}


#' @title                     Pvalors ajustats Update Compare
#' @description               Pvalors ajustats Update Compare
#' @param objecte_compare     objecte_compare
#' @param p                   p
#' @param method              method
#' @export                    Pvalors_ajustats_Update_Compare
Pvalors_ajustats_Update_Compare<-function(objecte_compare="res",
                                          p="p.overall",
                                          method ="BH") {
  # NUMERO_50)

  ## Actualitzar p-valors d'un objecte CompareGroups

  # objecte_compare=res
  # p="p.trend"
  # method="bonferroni"

  # 1. Extrect els p-valors
  pvalors <- compareGroups::getResults(objecte_compare, p)

  # 2. Recalculo p-valors segons metode
  pvalors<-stats::p.adjust(pvalors, method = method)

  # 3. Actualitzo Objecte compare
  vars_names<-names(objecte_compare)

  for(i in unique(vars_names)){                                # achieved here
    # objecte_compare[[i]][p]$p.overall <- pvalors[i]
    objecte_compare[[i]][p][p]<- pvalors[i] }
  # 4. Resultat
  objecte_compare
}


#' @title                     Pvalors_ajustats_taula
#' @description               Pvalors_ajustats_taula
#' @param objecte_taula       objecte_taula
#' @param p.valors            p.valors
#' @param metodo              metodo
#' @export                    Pvalors_ajustats_taula
Pvalors_ajustats_taula<-function(objecte_taula="OR.ajust",
                                 p.valors='p valor',
                                 metodo="BH") {
  # NUMERO_51)

  # objecte_taula=taulacoef
  # p.valors='P_adj'
  # metodo="bonferroni"

  # objecte_taula=pvals
  # p.valors="p.No vs Yes"
  # metodo="bonferroni"

  # 0 Genero noms de l'objecte a crear
  nomsnous<-c(names(objecte_taula),paste0(p.valors,".",substr(metodo, 1,3)),paste0(p.valors,".Sig.",substr(metodo, 1,3)))

  # 1. Extrec p-valors
  pvalors <-objecte_taula[[p.valors]]
  p.num<-pvalors %>% as.numeric()

  # 2. Calculo els p valors ajustats
  pvals_adj<-stats::p.adjust(p.num, method = metodo)

  # 3. Ho fusiono amb la taula
  objecte_taula<-objecte_taula %>% cbind(pvals_adj)

  # 4. Punt de tall
  objecte_taula<-objecte_taula %>% dplyr::mutate (sigBH=ifelse(pvals_adj<0.05,"Sig","NS"))

  # 6. Canviar noms
  objecte_taula<-objecte_taula %>% stats::setNames(nomsnous)

  objecte_taula %>% dplyr::as_tibble()


}


#' @title                     Calcular proporcio
#' @description               Calcular proporcio
#' @param dt                  dt
#' @param factor              factor
#' @export                    calcular_proporcio
calcular_proporcio<-function(dt=dades,
                             factor="canvi612M.glicadaCAT2"){
  # NUMERO_54)

  #  CALCULA LA PROPORCIo -- RETORNA N I % fila ----------------

  # dt=dades
  # factor="canvi612M.glicadaCAT2"
  # cat="Yes"

  moco<-dt %>%
    tidyr::drop_na(factor) %>%
    dplyr::group_by_(factor) %>%
    dplyr::summarise_(n="n()") %>%
    dplyr::mutate_(freq="n/sum(n)*100")

  moco

}


#' @title                     Proporcions grups
#' @description               Proporcions grups
#' @param dt                  dt
#' @param factor              factor
#' @param estrat              estrat
#' @export                    proporcions_grups
proporcions_grups<-function(dt=dades,
                            factor="canvi612M.glicadaCAT2",
                            estrat="SEXE"){
  # NUMERO_55)

  #  CALCULA PROPORCIO PER GRUPS I RETORNA P VALOR    --------------


  # dt=dades
  # factor="canvi612M.glicadaCAT2"
  # estrat="CODGLP1"

  ##  extrec p-valor
  pepito=paste0("chisq.test(dt$",factor,",dt$",estrat,")$p.value",sep="")
  pvalor<-eval(parse(text=pepito))

  resultat<-
    dt %>%
    tidyr::drop_na(factor) %>%
    dplyr::group_by_(estrat) %>%
    dplyr::do(calcular_proporcio(dt=.,factor=factor)) %>%
    dplyr::mutate(p=pvalor)

  resultat

}


#' @title                     Proporcio grups estratificat
#' @description               Proporcio grups estratificat
#' @param dt                  dt
#' @param factor.Y            factor.Y
#' @param grup                grup
#' @param estratificat        estratificat
#' @export                    proporcio_grups_estratificat
proporcio_grups_estratificat<-function(dt=dades,
                                       factor.Y="canvi612M.glicadaCAT2",
                                       grup=c("SEXE","CODGLP1","anys_DMcat4"),
                                       estratificat="HBA1C_cat4") {
  # NUMERO_56)

  #  RETORNA UNA LLISTA DE TAULES DE PROPORCIONS PER GRUPS ESTRATIFICAT PER estratificat ----------


  # dt=dades
  # factor.Y="canvi612M.glicadaCAT2"
  # grup=c("SEXE","anys_DMcat4")
  # estratificat="HBA1C_cat4"

  pepe<-list()

  for (i in 1:length(grup))  {
    pepe[[i]]<-
      dt %>%
      tidyr::drop_na(estratificat) %>%
      dplyr::group_by_(estratificat) %>%
      dplyr::do(proporcions_grups(dt=.,factor=factor.Y,estrat=grup[i]))

  }

  pepe


}



#' @title                     Reduccio ajustada
#' @description               Reduccio ajustada
#' @param dt                  dt
#' @param v.basal             v.basal
#' @param v.final             v.final
#' @param mean.basal          mean.basal
#' @export                    reduccio_ajustada
reduccio_ajustada<-function(dt=dades,
                            v.basal,
                            v.final,
                            mean.basal=NA) {
  # NUMERO_57)

  #  REDUCCIo AJUSTADA DIFERENTS METODES D'AJUST-----------------

  ##    BASAL , POST I RETORNA LA DIFERENCIA AJUSTA SEGONS EL BASAL I ERROR ESTANDARD


  #library(mgcv)

  # #  parametres

  # dt=dades
  # v.basal="HBpreADD"
  # v.final="HBpostADD"
  # mean.basal=9.02

  ##  Si no poso la mitjana basal poso la mitjana de la base de dades
  if (is.na(mean.basal)) mean.basal=mean(dt[,v.basal],na.rm=T)

  #   Calculo la variable canvi
  dt<-dt %>%
    dplyr::mutate(canvi=dt[,v.basal]-dt[,v.final])
  # Genero quintils que no els fare servir de moment
  dt<-dt %>%
    dplyr::mutate(basal_cat5=Hmisc::cut2(dt[,v.basal], g=5))

  ## Elimino missings de taula i selecciono variables
  dt<-dt %>%
    tidyr::drop_na(canvi) %>%
    dplyr::select_(v.basal,v.final,"canvi","basal_cat5")

  ## canvio noms que tampoc caldria
  names(dt)<-c("pre","post","dif","basal_cat")

  ## model cru (descriptiu bàsic,+ mean, se )
  taula<-dt %>% dplyr::summarise(
    n=dplyr::n(),
    mean.basal=mean(pre),
    mean.canvi=mean(dif),
    se=stats::sd(dif)/sqrt(dplyr::n())
  )

  ### arguments de funcions dels models amb les dades, junt amb la mean.basal
  pre<-dt$pre
  dif<-dt$dif

  # funcions dels models
  model.lineal.w<-function(y=y,x=x)stats::glm(y~x,weights =x,family = stats::gaussian)
  model.lineal<-function(y=y,x=x)stats::glm(y~x,family = stats::gaussian)
  model.nolineal<-function(y=y,x=x)stats::glm(y~x+I(x^2)+I(x^3),family = stats::gaussian)
  model.gam1<-function(y=y,x=x) mgcv::gam(y~s(x),family = stats::gaussian)
  model.gam2<-function(y=y,x=x) mgcv::gam(y~s(x,bs="cc",k=12),family = stats::gaussian)

  # Genero els models que els poso en una llista
  llista.models<-list(
    lineal.w=model.lineal.w(x=pre,y=dif),
    # lineal=model.lineal(x=pre,y=dif),
    nolineal=model.nolineal(x=pre,y=dif),
    gam1=model.gam1(x=pre,y=dif)
    # , gam2=model.gam2(x=pre,y=dif)
  )

  predict(llista.models[[1]],data.frame(x=mean.basal))

  ## Genero les prediccions () en el punt basal mitg  i guardo el la SE
  maquina<-llista.models %>%
    purrr::map_df(predict,data.frame(x=mean.basal),se.fit=T) %>%
    as.data.frame()

  ## Calculo Rquadrat per cada model
  Rquadrat<-llista.models %>%
    purrr::map_dbl(function(modelaco) (1-(modelaco$deviance/modelaco$null.deviance))) %>%
    as.data.frame()

  ## Combino informacio Rquadrat + prediccions de cada model
  taula.models<-cbind(Rquadrat,maquina)

  ## poso els noms dels models com una columna
  taula.models$model<-row.names(taula.models)

  ## enganxo la taula dels valors mitjans  i la N
  taula<-cbind(taula.models,taula,v.basal)

  ## Hauria de canviar el nom de les variables una mica i eliminar coses que no serveixen i tal pascual

  names(taula)[1] <- "R.Square"

  # Drop variables with -
  # taula<-select(taula, -("residual.scale"))

  taula

}

#' @title                     Glance.prediction
#' @description               Glance.prediction
#' @param x                   x
#' @export                    glance.prediction
glance.prediction <-function(x) {

  # NUMERO_58)

  #  Prediccio ajustada amb dades imputades   -----------------

  #  Envio un dades generades amb MICE , X Y i retorna les prediccions amb ES     ###



  data.frame(term = 'prediction',
             estimate = x[['fit']],
             std.error = x[['se.fit']],
             df.residual = x[['df']]) }

tidy.prediction = function(x, effects = "fixed", exponentiate = FALSE)
{glance.prediction(x)}



#' @title                     Retorn prediccio MI
#' @description               Retorn prediccio MI
#' @param data_imp            data_imp
#' @param x                   x
#' @param y                   y
#' @param dades_origen        dades_origen
#' @export                    retorn_prediccio_MI
retorn_prediccio_MI<-function(data_imp=tempData,
                              x="HBpreADD",
                              y="canvi_ADD",
                              dades_origen=dades) {
  # NUMERO_59)

  # data_imp=tempData
  # x="1"
  # y="canvi_ADD"
  # dades_origen=dades

  imp<-tempData
  nimp<-imp$m

  if (x!="1") mean.basal=mean(dades_origen[,x],na.rm=T)
  if (x=="1") mean.basal=0

  df.pred<-as.data.frame(mean.basal) %>% stats::setNames(x)

  texto=paste0(y,"~",x)

  mods.imp = lapply(1:nimp, function(.nimp){
    # m = stats::lm(canvi_ADD~HBpreADD, data = complete(imp, .nimp))
    m=with(tidyr::complete(imp,.nimp),stats::lm(eval(parse(text=texto))))

    mm = predict(m, newdata=df.pred, se.fit = TRUE)
    structure(
      mm,
      class = 'prediction')
  })



  pp<-summary(mice::pool(mice::as.mira(mods.imp)))

  pp

}

#' @title                     Retorn prediccio MI STR
#' @description               Retorn prediccio MI STR
#' @param data_imp            data_imp
#' @param x                   x
#' @param y                   y
#' @param dades_origen        dades_origen
#' @param valor_subset        valor_subset
#' @param var_subset          var_subset
#' @export                    retorn_prediccio_MI_STR
retorn_prediccio_MI_STR<-function(data_imp=tempData,
                                  x="HBpreADD",
                                  y="canvi_ADD",
                                  dades_origen=dades,
                                  valor_subset="<8",
                                  var_subset="HBpreADD") {
  # NUMERO_60)

  # data_imp=tempData
  # x="1"
  # y="canvi_ADD"
  # dades_origen=dades
  # valor_subset="<8"
  # var_subset="HBpreADD"

  subset<-paste0(var_subset,valor_subset)

  imp<-tempData
  nimp<-imp$m

  if (x!="1") mean.basal=mean(dades_origen[,x],na.rm=T)
  if (x=="1") mean.basal=0

  df.pred<-as.data.frame(mean.basal) %>% stats::setNames(x)

  texto=paste0(y,"~",x) # texte model
  texte_subset<-paste0("subset(complete(imp,.nimp),",subset,")")

  mods.imp = lapply(1:nimp, function(.nimp){
    # m = stats::lm(canvi_ADD~HBpreADD, data = complete(imp, .nimp))
    # m=with(complete(imp,.nimp),stats::lm(eval(parse(text=texto))))

    m=with(eval(parse(text=texte_subset)),stats::lm(eval(parse(text=texto))))

    mm = predict(m, newdata=df.pred, se.fit = TRUE)
    structure(
      mm,
      class = 'prediction')
  })



  pp<-summary(mice::pool(mice::as.mira(mods.imp)))

  pp

}


#' @title                     Retorn prediccio MI STR2
#' @description               Retorn prediccio MI STR2
#' @param data_imp            data_imp
#' @param x                   x
#' @param y                   y
#' @param dades_origen        dades_origen
#' @param valor_subset1       valor_subset1
#' @param valor_subset2       valor_subset2
#' @param var_subset          var_subset
#' @export                    retorn_prediccio_MI_STR2
retorn_prediccio_MI_STR2<-function(data_imp=tempData,
                                   x="HBpreADD",
                                   y="canvi_ADD",
                                   dades_origen=dades,
                                   valor_subset1=">=8",
                                   valor_subset2="<=10",
                                   var_subset="HBpreADD") {
  # NUMERO_61)

  # data_imp=tempData
  # x="1"
  # y="canvi_ADD"
  # dades_origen=dades
  # valor_subset="<8"
  # var_subset="HBpreADD"
  # valor_subset1=">=8"
  # valor_subset2="<=10"

  subset<-paste0(var_subset,valor_subset1," & ",var_subset,valor_subset2)

  imp<-tempData
  nimp<-imp$m

  if (x!="1") mean.basal=mean(dades_origen[,x],na.rm=T)
  if (x=="1") mean.basal=0

  df.pred<-as.data.frame(mean.basal) %>% stats::setNames(x)

  texto=paste0(y,"~",x) # texte model
  texte_subset<-paste0("subset(complete(imp,.nimp),",subset,")")

  mods.imp = lapply(1:nimp, function(.nimp){
    # m = stats::lm(canvi_ADD~HBpreADD, data = complete(imp, .nimp))
    # m=with(complete(imp,.nimp),stats::lm(eval(parse(text=texto))))

    m=with(eval(parse(text=texte_subset)),stats::lm(eval(parse(text=texto))))

    mm = predict(m, newdata=df.pred, se.fit = TRUE)
    structure(
      mm,
      class = 'prediction')
  })



  pp<-summary(mice::pool(mice::as.mira(mods.imp)))

  pp

}



#' @title                     Plot.dispersio.reduccio
#' @description               Plot.dispersio.reduccio
#' @param dt                  dt
#' @param v.basal             v.basal
#' @param v.final             v.final

plot.dispersio.reduccio <-function(dt="dades",
                                   v.basal="HBpreADD",
                                     v.final="HBpostADD") {
  # NUMERO_62)

  # no hem posta export.
  #See section 'Generic functions and methods' in the 'Writing R
  #Extensions' manual.

  #Found the following apparent S3 methods exported but not registered:
  #  formula.LOGIT plot.dispersio.reduccio
  #See section 'Registering S3 methods' in the 'Writing R Extensions'
  #manual.


  #  PLOT dispersio segons PRE-POST , FA DISPERSIo DE PRE VS CANVI I SOBREPOSA AJUST------
  # library(mgcv)

  # #  parametres

  # dt=dades
  # v.basal="HBpreADD"
  # v.final="HBpostADD"

  dt <-dt %>% dplyr::mutate(canvi=dt[,v.basal]-dt[,v.final])
  # Genero quintils
  dt<-dt %>% dplyr::mutate(basal_cat5=Hmisc::cut2(dt[,v.basal], g=5))

  ## Elimino missings de taula i selecciono variables

  dt<-dt %>%
    tidyr::drop_na(canvi) %>%
    dplyr::select_(v.basal,v.final,"canvi","basal_cat5")

  ## poso noms
  names(dt)<-c("pre","post","dif","basal_cat")

  lineal<-stats::glm(dif~pre,weights = pre,family = stats::gaussian, data=dt) %>% predict()
  lineal2<-stats::glm(dif~pre,family = stats::gaussian, data=dt) %>% predict()
  gam1<-mgcv::gam(dif~s(pre),family = stats::gaussian, data=dt) %>% predict()
  gam2<-mgcv::gam(dif~s(pre,bs="cc",k=12),family = stats::gaussian, data=dt) %>% predict()
  model.nolineal<-stats::glm(dif~pre+I(pre^2)+I(pre^3),family = stats::gaussian,data=dt) %>% predict()


  figuraZ<-dt %>%
    ggplot2::ggplot(ggplot2::aes(x=pre, y=dif)) +
    ggplot2::geom_point() +
    ggplot2::ylab("Change at 6-12 months:HbA1c (%)") +
    ggplot2::xlab("HbA1c (%) Baseline") +
    ggplot2::geom_point(ggplot2::aes(y=lineal),color="red")+
    ggplot2::geom_point(ggplot2::aes(y=gam1),color="blue") +
    ggplot2::geom_point(ggplot2::aes(y=model.nolineal),color="green")+
    ggplot2::ggtitle(paste0(v.basal," Versus ",v.final)) # for the main title

  figuraZ


}


#' @title                     Forest.plot
#' @description               Forest.plot
#' @param dadesmodel          dadesmodel
#' @param label               label
#' @param mean                mean
#' @param lower               lower
#' @param upper               upper
#' @param label_X             label_X
#' @param intercept           intercept
#' @export                    forest.plot
forest.plot<-function(dadesmodel="ramo",
                      label=dadesmodel$Categoria,
                      mean=dadesmodel$OR,
                      lower=dadesmodel$Linf,
                      upper=dadesmodel$Lsup,
                      label_X="OR (95% CI)",
                      intercept=1) {
  # NUMERO_63)

  #  Forest.plot --------------------

  # A partir de taula amb OR's / Betas genera Forest Plot
  # La taula ha de contenir els seguents camps:Categoria,OR,Linf,Lsup


  # dadesmodel=taula_coefs
  # label=taula_editada$Categoria
  # mean=taula_editada$OR
  # lower=taula_editada$Linf
  # upper=taula_editada$Lsup
  # label_X="OR (95% CI)"
  # intercept=1

  dadesmodel<-dadesmodel %>% dplyr::mutate(id=seq(length(label),1))

  fp <- ggplot2::ggplot(data=dadesmodel,ggplot2::aes(x=dadesmodel$id, y=dadesmodel$OR, ymin=dadesmodel$Linf, ymax=dadesmodel$Lsup)) +
    ggplot2::geom_pointrange() +
    ggplot2::geom_hline(yintercept=intercept, lty=2) +  # add a dotted line at x=1 after flip
    ggplot2::coord_flip() +  # flip coordinates (puts labels on y axis)
    ggplot2::xlab("Label") + ggplot2::ylab(label_X) +
    ggplot2::scale_x_continuous(breaks=dadesmodel %>%  dplyr::pull(id) ,labels=dadesmodel %>% dplyr:: pull(Categoria))

  fp
}


#' @title                     Forest.plot.v2
#' @description               Forest.plot.v2
#' @param dadesmodel          dadesmodel
#' @param label               label
#' @param mean                mean
#' @param lower               lower
#' @param upper               upper
#' @param label_X             label_X
#' @param intercept           intercept
#' @export                    forest.plot.v2
forest.plot.v2<-function(dadesmodel="ramo",
                         label="Categoria",
                         mean="OR",
                         lower="Linf",
                         upper="Lsup",
                         label_X="OR (95% CI)",
                         intercept=1) {
  # NUMERO_64)


  # Forest plot versio 2 millorada per tal que funcioni

  # dadesmodel=dt_dif
  # label="lipo"
  # mean="dif_st"
  # lower ="ci1"
  # upper="ci2"
  # label_X="Differences standardized (95% CI)"
  # intercept=0

  dadesmodel<-dadesmodel %>% dplyr::mutate(id=seq(length(dadesmodel[[label]])))

  # Generar data set
  dadestemp <- dadesmodel %>% dplyr::select(etiqueta=!!label,valor=!!mean,Linf=!!lower,Lsup=!!upper,id)

  fp <- ggplot2::ggplot(data=dadestemp,ggplot2::aes(x=id, y=valor, ymin=Linf, ymax=Lsup)) +
    ggplot2::geom_pointrange() +
    ggplot2::geom_hline(yintercept=intercept, lty=2) +  # add a dotted line at x=1 after flip
    ggplot2::coord_flip() +  # flip coordinates (puts labels on y axis)
    ggplot2::xlab("Label") + ggplot2::ylab(label_X) +
    ggplot2::scale_x_continuous(breaks=dadestemp %>% dplyr:: pull(id),labels=dadestemp %>% dplyr:: pull(etiqueta))

  fp

}


#' @title                     Forest.plot.v3
#' @description               Forest.plot.v3
#' @param dadesmodel          dadesmodel
#' @param label               label
#' @param mean                mean
#' @param lower               lower
#' @param upper               upper
#' @param label_X             label_X
#' @param intercept           intercept
#' @param nivell              nivell
#' @param factor1             factor1
#' @param factor2             factor2
#' @param color               color
#' @export                    forest.plot.v3
forest.plot.v3<-function(dadesmodel="dt_estimacions",
                         label="Categoria",
                         mean="estimate",
                         lower="Linf",
                         upper="Lsup",
                         label_X="OR (95% CI)",
                         intercept=0,
                         nivell="outcome",
                         factor1="type",
                         factor2="datos",
                         color=TRUE) {
  # NUMERO_65)

  # Forest plot versio 3

  # dadesmodel=dt_estimacions
  # label="labels"
  # mean="estimate"
  # lower = "Linf"
  # upper="Lsup"
  # label_X="Differences standardized (95% CI)"
  # intercept = 0
  # nivell="outcome"
  # factor1="type"
  # factor2="datos"
  # color=TRUE


  # Generar data set
  dadesmodel <- dadesmodel %>% dplyr::select(valor=!!mean,Linf=!!lower,Lsup=!!upper,nivell=!!nivell, factor1=!!factor1,factor2=!!factor2)

  ## Preparar taula (Genero etiqueta)
  taula_betas<-dadesmodel %>% dplyr::mutate(etiqueta=paste0("     ",factor2," ",factor1),
                                     Method = paste0(factor2," ",factor1))

  # Afegir fila com un punt nivell per outcome i genero label de group
  taula_betas<-taula_betas %>% split(.$nivell) %>%
    purrr::map_dfr(~dplyr::add_row(.x,.before = 0),.id = "outcome" ) %>%
    dplyr::mutate (etiqueta2=dplyr::if_else(is.na(etiqueta),outcome,"")) %>%
    dplyr::mutate (etiqueta=dplyr::if_else(is.na(etiqueta),outcome,etiqueta))

  # AFegir etiqueta 3 mes centrada
  taula_betas<-taula_betas %>% dplyr::mutate(etiqueta3=dplyr::lag(etiqueta2),
                                      etiqueta3=dplyr::if_else(is.na(etiqueta3),"",etiqueta3))

  # Generar id
  taula_betas<-taula_betas %>% dplyr::mutate(id=seq(dplyr::n())) %>% dplyr::mutate(id=dplyr::n()-id+1)

  # REomplir missings en factor1 i factor2
  taula_betas<-taula_betas %>% fill(c(factor1,factor2,Method),.direction="updown")

  # Relevel mateix ordre tal com surt taula
  ordre_levels<-taula_betas %>%  dplyr::pull(Method) %>% unique()
  taula_betas$Method<-factor(taula_betas$Method, levels = ordre_levels)

  fp <- ggplot2::ggplot(data=taula_betas,ggplot2::aes(x=id, y=valor, ymin=Linf, ymax=Lsup)) +
    ggplot2::geom_pointrange(size=0.2) +
    ggplot2::geom_hline(yintercept=intercept, lty=1) +  # add a dotted line at x=1 after flip
    ggplot2::coord_flip() +  # flip coordinates (puts labels on y axis)
    ggplot2::xlab("Outcome") + ggplot2::ylab(label_X) +
    ggplot2::scale_x_continuous(breaks=taula_betas %>% dplyr:: pull(id),labels=taula_betas %>% dplyr:: pull(etiqueta3))

  fp<-fp + ggplot2::theme_minimal() + ggplot2::theme(axis.text.y = ggplot2::element_text(hjust = 0,vjust=0,size=10))

  if (color) {fp<-fp + ggplot2::geom_point(ggplot2::aes(color=Method),size=3)} else
  {fp<-fp + ggplot2::geom_point(ggplot2::aes(shape=Method),size=3)}

  # Add banda d'error
  fp<-fp + ggplot2::geom_hline(yintercept = c(intercept+0.1,intercept-0.1),linetype=2)

  fp

}


#' @title                     Forest.plot.HR
#' @description               Forest.plot.HR
#' @param dadesmodel          dadesmodel
#' @param label               label
#' @param mean                mean
#' @param lower               lower
#' @param upper               upper
#' @param label_X             label_X
#' @param intercept           intercept
#' @param nivell              nivell
#' @param factor1             factor1
#' @param color               color
#' @param label_Xvertical     label_Xvertical
#' @param nolabels            nolabels
#' @param title               title
#' @param label_Favors        label_Favors
#' @export                    forest.plot.HR
forest.plot.HR<-function(dadesmodel,
                         label="Categoria",
                         mean="estimate",
                         lower="Linf",
                         upper="Lsup",
                         label_X="OR (95% CI)",
                         intercept=1,
                         nivell="outcome",
                         factor1="type",
                         color=F,
                         label_Xvertical="Cardiovascular event",
                         nolabels=TRUE,
                         title = "Forest plot of hazard hatios and confidence interval (95%CI)",
                         label_Favors="Favors SGLT-2        Favors oGLD-2") {
  # NUMERO_66)


  # Forest plot estratificar, label de categoria, mean , nivell per fer un salt, i un factor per posar llegenda


  # dadesmodel=dt_fig
  # label="label"
  # mean="HR"
  # lower="IC951"
  # upper="IC952"
  # label_X="Hazard ratio (95% CI)"
  # intercept=1
  # nivell="outcome"
  # factor1="grups"
  # label_Xvertical = "Subgroups"
  # color=F
  # nolabels=TRUE

  # Generar data set
  dadesmodel <- dadesmodel %>% dplyr::select(valor=!!mean,Linf=!!lower,Lsup=!!upper,nivell=!!nivell, factor1=!!factor1)

  ## Preparar taula (Genero etiqueta)
  taula_betas<-dadesmodel %>% dplyr::mutate(etiqueta=paste0("   ",factor1),
                                     Group = paste0(factor1))

  # Afegir fila com un punt nivell per outcome i genero label de group
  taula_betas<-taula_betas %>% split(.$nivell) %>%
    purrr::map_dfr(~dplyr::add_row(.x,.before = 0),.id = "outcome" ) %>%
    dplyr::mutate (etiqueta2=dplyr::if_else(is.na(etiqueta),outcome,"")) %>%
    dplyr::mutate (etiqueta=dplyr::if_else(is.na(etiqueta),outcome,etiqueta))

  # AFegir etiqueta 3 mes centrada
  taula_betas<-taula_betas %>% dplyr::mutate(etiqueta3=dplyr::lag(etiqueta2),
                                      etiqueta3=dplyr::if_else(is.na(etiqueta3),"",etiqueta3))

  # Reordenar outcomes segons origen de taula inicial
  dt_ordre<-dadesmodel %>% dplyr::distinct(outcome=nivell) %>% dplyr::mutate(seq=seq(1:dplyr::n()))
  taula_betas<-taula_betas %>% dplyr::left_join(dt_ordre,by="outcome") %>%dplyr:: arrange(seq)

  # Generar id
  taula_betas<-taula_betas %>% dplyr::mutate(id=seq(dplyr::n())) %>% dplyr::mutate(id=dplyr::n()-id+1)

  # REomplir missings en factor1 i factor2
  taula_betas<-taula_betas %>% fill(c(factor1,Group),.direction="updown")

  # Relevel mateix ordre tal com surt taula
  ordre_levels<-taula_betas %>% dplyr:: pull(Group) %>% unique()
  taula_betas$Group<-factor(taula_betas$Group, levels = ordre_levels)

  # per defecte agafo etiqueta 3 (Si no agafo etiqueta buida)
  if (nolabels) labels_scaleX=taula_betas %>% dplyr:: pull(etiqueta3) else labels_scaleX=taula_betas %>% dplyr:: pull(etiqueta)

  #limits màxims d'eixos
  xmax=max(taula_betas$Lsup,na.rm = T) %>% max(2)
  xmin=min(taula_betas$Linf,na.rm = T) %>% min(0.4)
  ymaxim=taula_betas %>% dplyr::count() %>% as.numeric()

  fp <- ggplot2::ggplot(data=taula_betas,ggplot2::aes(x=id, y=valor, ymin=Linf, ymax=Lsup)) +
    # geom_pointrange(size=0.6) +
    ggplot2::geom_pointrange(size=0.2) +
    ggplot2::geom_hline(yintercept=intercept, lty=1,colour="grey") +  # add a dotted line at x=1 after flip
    ggplot2::coord_flip() +  # flip coordinates (puts labels on y axis)
    ggplot2::scale_x_continuous(breaks=taula_betas %>% dplyr:: pull(id),labels=labels_scaleX)  +
    ggplot2::ylim(xmin,xmax)

  fp<-fp + ggplot2::theme_minimal(base_size = 12) + ggplot2::theme(axis.text.y = ggplot2::element_text(hjust = 0,vjust=0,size=11)) +
    ggplot2::labs(title = title, x=label_Xvertical,y=label_X, col="Method \n") +
    ggplot2::theme(legend.position="top") +
    ggplot2::annotate("text", x=ymaxim+1,y=1,label=label_Favors, colour = "black",size=2.5)

  # caption = "SGLT-2: sodium-glucose co-transporter-2 inhibitors | oGLD-2 \n created by Jordi Real & Rai Puig ")

  if (color) {fp<-fp + ggplot2::geom_point(ggplot2::aes(color=Group),size=3)}

  # Add banda d'error
  # fp<-fp + geom_hline(yintercept = c(intercept+0.1,intercept-0.1),linetype=2)

  fp

  # plotly::ggplotly(fp)


}


#' @title                     Forest.plot.v4
#' @description               Forest.plot.v4
#' @param dadesmodel          dadesmodel
#' @param label               label
#' @param mean                mean
#' @param lower               lower
#' @param upper               upper
#' @param label_X             label_X
#' @param intercept           intercept
#' @param nivell              nivell
#' @param factor              factor
#' @param label_Xvertical     label_Xvertical
#' @param title               title
#' @param subtitle            subtitle
#' @param caption             caption
#' @param label_Favors        label_Favors
#' @export                    fores.plot.v4
fores.plot.v4<-function(dadesmodel="dt_outHR",
                        label="etiqueta",
                        mean="estimate",
                        lower="Linf",
                        upper="Lsup",
                        label_X="OR (95% CI)",
                        intercept=1,
                        nivell="outcome",
                        factor="Event",
                        label_Xvertical="Cardiovascular event",
                        title = "Forest plot of hazard hatios and confidence interval (95%CI)",
                        subtitle = "Sensitivity analysis",
                        caption="",
                        label_Favors="Favors SGLT-2        Favors oGLD-2") {

  # NUMERO_67)

  # dadesmodel=dt_outHR
  # label="Adjusted"
  # mean="Coefficient"
  # lower="CI_low"
  # upper="CI_high"
  #
  # nivell="Event"
  # # nivell=""
  # factor="method"
  # # factor<-""
  #
  # intercept=1
  # color=T
  # label_X="HR (95% CI)"
  # title = "Hazard ratios for Parkinson disease according retinophaty status"
  # subtitle = "Sensitivity analysis"
  # caption=""
  # label_Favors="Favors without DR               Favors Retinophaty"


  # Selecciono camps de dades necessaris
  dt<-dadesmodel %>%
    dplyr::select(label=!!dplyr::sym(label),mean=!!dplyr::sym(mean),lower=!!dplyr::sym(lower),upper=!!dplyr::sym(upper),nivell=!!dplyr::sym(nivell),factor=!!dplyr::sym(factor))

  # Indexo ordre
  dt_temp<-dt %>%dplyr:: mutate (id=dplyr::n():1)

  # Si existeix nivell preparar dades per nivell
  # Afegir nivell amb etiqueta si cal
  if (nivell!="") {
    dt_temp<- dt_temp %>%
      dplyr::left_join(dt_temp %>% dplyr::distinct(nivell) %>% dplyr::mutate(id_nivell=1:dplyr::n()),.id="nivell",by = "nivell") %>%
      dplyr::mutate(label=paste0("           ",label)) %>%
      split(.$nivell) %>%
      purrr::map_dfr(~dplyr::add_row(.x,.before = 0),.id = "nivell" ) %>%
      dplyr::mutate(label=dplyr::if_else(is.na(label),nivell,label)) %>%
      dplyr::group_by(nivell) %>%
      dplyr::mutate(id_nivell=max(id_nivell,na.rm = T)) %>% dplyr::ungroup() %>%
      dplyr::arrange(id_nivell) %>%
      dplyr::mutate(id=dplyr::n():1)
    # reemplenar categoria missing de factor (si existeix)
    dt_temp<-dt_temp %>% dplyr::group_by(id_nivell) %>% dplyr::mutate(factor=dplyr::if_else(is.na(factor),max(factor,na.rm = T),factor))
  }

  # Plot
  ## Forest Plot
  pplot<-
    dt_temp %>%
    ggplot2::ggplot(ggplot2::aes(x=id, y=mean, ymin=lower, ymax=upper)) +
    ggplot2::geom_errorbar(size=0.2, width=0.5)+
    ggplot2::geom_hline(yintercept=intercept, lty=2) +  # add a dotted line at x=1 after flip
    ggplot2::coord_flip() +  # flip coordinates (puts labels on y axis)
    ggplot2::xlab("") + ggplot2::ylab(label_X) +
    ggplot2::scale_x_continuous(breaks=dt_temp %>%  dplyr::pull(id),labels=dt_temp %>% dplyr:: pull(label)) +
    # ylim(-0.5,10) +
    ggplot2::theme_minimal()

  if (nivell!="") {pplot <- pplot + ggplot2::geom_point(ggplot2::aes(color=nivell),size=4,shape=15) + ggplot2::labs(color=nivell)}

  if (factor!="") { pplot <- pplot + ggplot2::geom_point(ggplot2::aes(shape=factor)) + ggplot2::labs(shape=factor) }

  # tituls + labs
  pplot +
    ggplot2::labs(title = title, subtitle = subtitle,caption = caption) +
    ggplot2::annotate("text", x=-0.8,y=1,label=label_Favors, colour = "black",size=4)


}



#' @title                     Dt_index_data_random
#' @description               Dt_index_data_random
#' @param dt                  dt
#' @export                    dt_index_data_random
dt_index_data_random<-function(dt="PACIENTS") {

  # NUMERO_68)



  #
  #  Random dates i marcar potencials CONTROLS-----------
  #
  # Genero N dates random entre 2010-2016 el mateix nombre que

  # dt=PACIENTS
  # Necessito camp dtsortida (ymd)

  ####        Genero una data random entre 01/01/2010 i 31/12/2016

  set.seed(123)
  data_index_data<-dt %>%
    nrow() %>% stats::runif(as.Date("10/01/01", "%y/%m/%d"), as.Date("16/12/31", "%y/%m/%d")) %>%
    data.table::data.table() %>%
    stats::setNames(.,c("dtindex.random")) %>%
    dplyr::mutate (
      dtindex.random=as.Date(dtindex.random, origin = "1970-01-01")
    )

  # Fusiono amb idp i selecciono POTENCIALS CONTROLS dins de periode de seguiment

  BD_PAC_DINDEX<-dt %>%
    dplyr::select(idp,dtsortida) %>%
    cbind(data_index_data) %>%                                # Fusiono dates random
    dplyr::filter(dtindex.random<=lubridate::ymd(dtsortida)) %>%     # Filtro nomes aquells que dins de la data de seguiment
    dplyr::select (idp,dtindex.random) %>%
    dplyr::as_tibble()

}


#' @title                     Dt_index_data_semirandom
#' @description               Dt_index_data_semirandom
#' @param dt                  dt
#' @param dt.variables        dt.variables
#' @param codi                codi
#' @export                   dt_index_data_semirandom
dt_index_data_semirandom<-function(dt="PACIENTS",
                                   dt.variables="VARIABLES",
                                   codi="EK201"){

  # NUMERO_69)

  #

  #  GENERA UNA DATA INDEX SEGONS UNA DETERMINACIo ----------------------
  ## RETORNA DADES AMB idp + dtindex.semirandom

  # dt=PACIENTS
  # dt.variables=VARIABLES
  # codi="EK201"

  # b) SEMI.RANDOM (amb un màxim de data a data sortida)

  # Una data entre tots Colesterol total (previes a data sortida)
  # Si no hi ha cap Colesterol alguna V clinica periode (Random)
  set.seed(123)
  ### Per cada pacient selecciono una dat random de entre tots els COLESTEROLS  (2010-2016)
  UN.COLESTEROL<-dt.variables %>%
    dplyr::filter(cod==codi) %>%                       # selecciono colesterols (Validar que es EK201)
    dplyr::left_join(dt,by="idp") %>%           # Junto pacients
    dplyr::select(idp,cod,dat,dtsortida) %>%           # Selecciono camps necessaris
    dplyr::filter(!is.na(dtsortida)) %>%               # Filtro nomes pacients (amb dtsortida)
    dplyr::filter (dat>=20100101 & dat<=dtsortida) %>%  # filtro Dates dins periode de seguiment
    dplyr::group_by(idp) %>%                           # Agafo un colesterol per cada idp
    dplyr::sample_n(size = 1) %>%                      # Random
    dplyr::ungroup %>%
    dplyr::select(idp, dat) %>%
    dplyr::rename(dat_col=dat)

  ### Per cada pacient selecciono una dat random entre totes les VARIABLES
  UNA.VARIABLE<-dt.variables %>%                # totes les variables
    dplyr::left_join(dt,by="idp") %>%           # Junto pacients
    dplyr::select(idp,dat,dtsortida) %>%               # Selecciono camps necessaris
    dplyr::filter(!is.na(dtsortida)) %>%               # Filtro nomes pacients amb dtsortida
    dplyr::filter (dat>=20100101 & dat<=dtsortida) %>% # Dates possibles dins el seguiment
    dplyr::group_by(idp) %>%                           # Agafo unA fila per cada idp
    dplyr::sample_n(size = 1) %>%                      # RAndom
    dplyr::ungroup() %>%
    dplyr::select(idp, dat) %>%
    dplyr::rename(dat_var=dat)

  ### Fusio d'ambdos fitxers i selecciono una d'elles preferentment colesterol

  BDADES_DT_INDEX<-UNA.VARIABLE %>%
    dplyr::left_join(UN.COLESTEROL,by="idp") %>%
    dplyr::mutate(dtindex.semirandom=ifelse(is.na(dat_col),dat_var,dat_col)) %>%
    dplyr::select(idp,dtindex.semirandom)

}

#' @title                     Matching 4grups
#' @description               Matching 4grups
#' @param dt                  dt
#' @param grups               grups
#' @param vars_match          vars_match
#' @param conductor           conductor
#' @param caliper             caliper
#' @export                    matching_4grups
matching_4grups<-function(dt=dadesini,
                          grups="grup",
                          vars_match="matching",
                          conductor="vars_ilerbus.xls",
                          caliper=0.01) {

  # NUMERO_70)

  # Funcio que retorna 4 grups aparellats per 4 grups (2 x 2) de 2 variables
  # Entra una base de dades (dades) i una variable factor amb 4 nivells
  # Retorna dades aparellades en dos fases a) 1vs3 2vs4  Fusio --> b) 1vs2 3vs4



  # dt=dadesini
  # grups="grup"
  # caliper=0.01
  # vars_match="matching"
  # conductor="vars_ilerbus.xls"

  set.seed(123)

  # Formula matching
  #formulaPS<-formula.text("matching",y="grup_dic",taulavariables=conductor) %>% stats::as.formula()
  formulaPS<-formula_text("matching",y="grup_dic",taulavariables=conductor) %>% stats::as.formula()

  # Genero index de grup
  dt_temp<-dt %>% dplyr::select(!!grups) %>% dplyr::distinct() %>%dplyr:: mutate(id_grup=dplyr::row_number())
  # Fusiono id_grup
  dt<-dt %>% dplyr::left_join(dt_temp)  # + id_grup

  #  -----------------  1 vs 3 ---------------------- dades_match13
  dades<-dt %>% dplyr::filter(id_grup==1 | id_grup==3 ) # Filtro dos grups

  # Dicotomitzar id_grup
  dades<-make_dummies(dades,"id_grup","gr_")
  names(dades)[length(names(dades))]<-"grup_dic"

  # MATCHING 1VS3
  m.out<-MatchIt::matchit(formulaPS,method="nearest",data=dades,caliper=caliper,ratio=1,exact=c("gender"))
  # Filtro per ps
  dades_match_13<-dades %>% dplyr::bind_cols(ps=m.out$weights) %>% dplyr::filter(ps==1) %>% dplyr::select(-ps)

  #  -----------------  2 vs 4 ---------------------- dades_match13
  dades<-dt %>% dplyr::filter(id_grup==2 | id_grup==4 ) # Filtro dos grups
  # Validacio prematch

  # Dicotomitzar id_grup
  dades<-make_dummies(dades,"id_grup","gr_")
  names(dades)[length(names(dades))]<-"grup_dic"

  # Matching 2VS4
  m.out<-MatchIt::matchit(formulaPS,method="nearest",data=dades,caliper=caliper,ratio=1,exact=c("gender"))

  # Filtro per ps
  dades_match_24<-dades %>%dplyr:: bind_cols(ps=m.out$weights) %>% dplyr::filter(ps==1) %>% dplyr::select(-ps)

  # -------------------  Actualitzar dt amb dades nomes matxejades ---
  dt<-dades_match_13 %>% dplyr::bind_rows(dades_match_24) %>%dplyr:: select(-c(gr_2,gr_1,grup_dic))

  #  -----------------  1 vs 2 ---------------------- dades_match12
  dades<-dt %>% dplyr::filter(id_grup==1 | id_grup==2 ) # Filtro dos grups
  # Validacio prematch
  #formu<-formula.text("match_desc","grup",taulavariables = conductor)
  formu<-formula_text("match_desc","grup",taulavariables = conductor)

  # Dicotomitzar id_grup
  dades<-make_dummies(dades,"id_grup","gr_")
  names(dades)[length(names(dades))]<-"grup_dic"

  # MATCHING 1VS2
  m.out<-MatchIt::matchit(formulaPS,method="nearest",data=dades,caliper=caliper,ratio=1,exact=c("gender"))

  # Filtro per ps
  dades_match_12<-dades %>% dplyr::bind_cols(ps=m.out$weights) %>% dplyr::filter(ps==1) %>% dplyr::select(-ps)

  #  -----------------  3 vs 4 ---------------------- dades_match12
  dades<-dt %>% dplyr::filter(id_grup==3 | id_grup==4 ) # Filtro dos grups

  # Dicotomitzar id_grup
  dades<-make_dummies(dades,"id_grup","gr_")
  names(dades)[length(names(dades))]<-"grup_dic"

  # MATCHING 3VS4
  m.out<-MatchIt::matchit(formulaPS,method="nearest",data=dades,caliper=caliper,ratio=1,exact=c("gender"))
  # Filtro per ps
  dades_match_34<-dades %>%dplyr:: bind_cols(ps=m.out$weights) %>% dplyr::filter(ps==1) %>% dplyr::select(-ps)

  # Fusionar dades
  # Juntar tot

  # -------------------  Actualitzar dt amb dades nomes matxejades ---
  dt<-dades_match_12 %>% dplyr::bind_rows(dades_match_34) %>% dplyr::select(-c(gr_1,gr_3,grup_dic))

}




#' @title                     Matching case control
#' @description               Matching case control
#' @param dt                  dt
#' @param variables.ps        variables.ps
#' @param dt_pacients_dindex  dt_pacients_dindex
#' @export                    matching_case_control
matching_case_control<-function(dt="PACIENTS",
                                variables.ps="llistaPS",
                                dt_pacients_dindex="BD_PAC_DINDEX") {

  # NUMERO_71)



  #  MATCHING CAS-CONTROL SEGONS MeTODE DENSITY-INCIDENCE ------------------

  ##  Retorna Subset matxejat per grup (event) en data index (dtindex.random, control) DE dt_pacients_dindex
  ##  Llista de variables variables.ps


  # dt=PACIENTS
  # variables.ps=c("edat","dtindex","sexe") # covaribles
  # dt_pacients_dindex=BD_PAC_DINDEX

  # Es neceseciten camps com <dtsortida idp event> + llista de variables a matxejar
  # <idp, dtindex.random, control> en BD_PAC_DINDEX

  # 2 Fusionar events i controls en una sola taula

  dt <-dt %>%
    dplyr::left_join(dt_pacients_dindex,by="idp")              # dt + dtindex.random (data random generada + de control

  # Selecciono events i mutar dataindex (event=1) en data d'event (dtsortida)

  dtevents<-dt %>% dplyr::filter(event==1) %>% dplyr::mutate(dtindex=lubridate::ymd(dtsortida), event=1)         ## Els events data de sortida

  # Seleccionar controls i mutar dataindex en data index random

  dtcontrols<-dt %>% dplyr::filter(control==1) %>% dplyr::mutate(dtindex=dtindex.random, event=0)     ## Els controls data random

  # Fusionar events + controls
  dt.total<-dtevents %>% rbind(dtcontrols)


  # 3 Agregar en data index (Edat)


  # Agrego en dtindex

  dt.total<-dt.total %>%
    dplyr::mutate (edat=as.numeric((dtindex-lubridate::ymd(dnaix))/365.25))               # Calculo edat en dataindex


  # 4 Fer matching

  # preparar dades per matching (idp + Llista matching)
  dadesmatching<-dt.total %>% dplyr::select(idp,edat,dtindex,event,sexe)

  # Genero llista de covaraibles
  formulaPS<-stats::as.formula(paste("event", paste(variables.ps, collapse=" + "), sep=" ~ "))

  dt.matched<-formulaPS %>%
    MatchIt::matchit(method="nearest",data=dadesmatching,ratio=4,caliper=0.01,distance = "logit") %>%    # FAig el matching 4 a 1
    stats::weights() %>%                                                            # Guardo els pesos
    data.table::data.table() %>%
    'colnames<-'(c("PS")) %>%
    dplyr::bind_cols(dt.total) %>%                                                 # Ho junto al dt.total
    dplyr::filter(PS==1) %>%
    dplyr::as_tibble()


}

#' @title                     Covariate plot
#' @description               Covariate plot
#' @param dt                  dt
#' @param vars_remove         vars_remove
#' @param etiquetar           etiquetar
#' @param subtitle            subtitle
#' @param ...                 Altres parametres
#' @export                    covariate_plot
covariate_plot<-function(dt="m.out",
                         vars_remove=NULL,
                         etiquetar=F,
                         subtitle="oGLD vs SGLT-2i group",
                         ...) {

  # NUMERO_72)


  # Retorna a Covariate_plot d'un objecte matchit()  -------------------------
  # Llances un objecte m-out, variables que vols eliminar i si vols etiquetar segons conductor

  # vars_remove<-c("age", "sexe","tempsdm_cat4", "iyearsem","qmedea")
  # m.out,vars_remove = c("qmedea","age"),etiquetar = T
  # dt=m.out
  # vars_remove=NULL
  # etiquetar = F
  # taulavariables=conductor_variables

  # Preparar dades a plotejar
  dt_pre<-summary(dt,standardize = T)$sum.all %>% tibble::as_tibble(rownames = "var") %>% dplyr::mutate(Sample="Unmatched",id=dplyr::row_number())
  dt_post<-summary(dt,standardize = T)$sum.matched %>% tibble::as_tibble(rownames = "var") %>% dplyr::mutate(Sample="Matched",id=dplyr::row_number())
  #
  # Preparar i ordenar per id
  dt_total<-
    dt_pre %>% dplyr::bind_rows(dt_post) %>%
    dplyr::mutate (stat=`Std. Mean Diff.`) %>%
    dplyr::filter(var!="distance") %>%
    dplyr::filter(!is.na(stat)) %>%
    dplyr::mutate(var=factor(var,levels=rev(dt_pre$var)))   # Convertir a factor per que surti ordenat

  # He generar variables+nivells indexat
  llista_vars<-names(dt$X)

  # les variables exact s'han de factoritzar
  dt$X<-dt$X %>% dplyr::mutate_at(all.vars(dt$exact),as.factor)

  vars_df<-
    llista_vars %>% rlang::set_names(llista_vars) %>%
    purrr::map(~levels(dt$X[[.x]])) %>%
    tibble::enframe() %>%
    tidyr::unnest(cols = c(value))

  vars_df<-
    tibble::as_tibble(llista_vars) %>% dplyr::select(name=value) %>%
    dplyr::left_join(vars_df,by="name") %>%
    dplyr::mutate(
      value=ifelse(is.na(value) | value=="NA","",value),
      var=paste0(name,value))

  # Juntar noms de variables + levels
  dt_total<-dt_total %>% dplyr::left_join(vars_df,by="var")

  # Eliminar vars a eliminar
  dt_total<-dt_total %>% dplyr::filter(!name%in%vars_remove)

  # Etiquetar variables
  if (etiquetar) dt_total<-dt_total %>% etiquetar_taula(camp = "name", ...)

  # Afegir nivells exepte Yes / Si i eliminar cat No
  dt_total<-
    dt_total %>%
    dplyr::mutate(name=dplyr::if_else(value=="" | value=="Yes" | value=="Si",
                        name,paste0(name,":",value))) %>%
    dplyr::filter(value!="No") %>%
    dplyr::filter(value!="0")

  # Preque mantingui l'ordre
  dt_total$name<- factor(dt_total$name, levels=rev(unique(dt_total$name)),ordered=T)

  ggplot2::ggplot(ggplot2::aes(y = name, x = stat, group = Sample), data = dt_total) +
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white"),
                   axis.text.x = ggplot2::element_text(color = "black"),
                   axis.text.y = ggplot2::element_text(color = "black"),
                   panel.border = ggplot2::element_rect(fill = NA, color = "black"),
                   plot.background = ggplot2::element_blank(),
                   legend.background = ggplot2::element_blank(),
                   legend.key = ggplot2::element_blank()) +
    ggplot2::geom_point(ggplot2::aes(colour=Sample),size=3) +
    ggplot2::geom_vline(xintercept = c(-0.1,0,0.1) , linetype = 2, color = "gray8")+
    ggplot2::theme(legend.position = "top")+
    ggplot2::labs(y = NULL, x = "Standardized mean difference",
                  title=paste0("Covariate plot \n ", subtitle))+
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))


}

#' @title                     Covariate_plot_dades
#' @description               Covariate_plot_dades
#' @param dt                  dt
#' @param var                 var
#' @param stat                stat
#' @param title               title
#' @param labx                labx
#' @export                    covariate_plot_dades
covariate_plot_dades<-function(dt="dt_total",
                               var="name",
                               stat="stat",
                               title="Covariate plot \n oGLD vs SGLT-2i group",
                               labx="Standardized mean difference") {
  # NUMERO_73)


  # dt=dt_total
  # var="name"
  # stat="stat"
  # title="Covariate plot \n oGLD vs SGLT-2i group"
  # labx="Standardized mean difference"

  var=dplyr::sym(var)
  stat=dplyr::sym(stat)

  ggplot2::ggplot(ggplot2::aes(y = !!var, x = !!stat, group = Sample), data = dt) +
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white"),
                   axis.text.x = ggplot2::element_text(color = "black"),
                   axis.text.y = ggplot2::element_text(color = "black"),
                   panel.border = ggplot2::element_rect(fill = NA, color = "black"),
                   plot.background = ggplot2::element_blank(),
                   legend.background = ggplot2::element_blank(),
                   legend.key = ggplot2::element_blank()) +
    ggplot2::geom_point(ggplot2::aes(colour=Sample),size=3) +
    ggplot2::geom_vline(xintercept = c(-0.1,0,0.1) , linetype = 2, color = "gray8")+
    ggplot2::theme(legend.position = "top")+
    ggplot2::labs(y = NULL, x = labx, title=title)+
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))


}


#' @title                     Mostreig ids
#' @description               Mostreig ids
#' @param dt                  dt
#' @param id                  id
#' @param n_mostra            n_mostra
#' @param set_seed            set_seed
#' @export                    mostreig_ids
mostreig_ids<-function(dt,
                       id="idp",
                       n_mostra=100,
                       set_seed=123) {
  # NUMERO_74)

  # mostreig_ids () Mostreja ids d'una base de dades  ---------------------

  # n_mostra<-100
  # dt<-dades
  # id="idp"

  set.seed(set_seed)

  if (n_mostra!=Inf) {

    id_sym<-dplyr::sym(id)
    id_sample<-dt %>% dplyr::distinct(!!id_sym) %>% dplyr::sample_n(size=n_mostra)
    dt<-id_sample %>% dplyr::left_join(dt,by=id)

  } else { dt<-dt}

  dt

}



#' @title                     Regicor
#' @description               Regicor
#' @param age                 age
#' @param sex                 sex
#' @param smoker              smoker
#' @param diabetes            diabetes
#' @param coltot              coltot
#' @param colhdl              colhdl
#' @param sbp                 sbp
#' @param dbp                 dbp
#' @param divide              divide
#' @export                    regicor
regicor <- function(age,
                    sex,
                    smoker,
                    diabetes,
                    coltot,
                    colhdl,
                    sbp,
                    dbp,
                    divide = 1){
  # NUMERO_75)

  #
  # Funcio per calcular el risc REGICOR (regicor)  -----------------
  #
  # age: númerica (anys)
  # sex: text, 'H'  homes i 'D' dones
  # smoker, diabetes: binària (0 no i 1 si)
  # coltot i colhdl: en mg/dL
  # sbp i dbp: númeric (mmHg)


  n <- length(age)
  diabetes <- as.numeric(diabetes)
  bp_opti <- ifelse(sbp <  120 & dbp < 80, 1, 0)
  bp_high <- ifelse((130 <= sbp & sbp < 140) | (85 <= dbp & dbp < 90), 1, 0)
  bp_i <- ifelse((140 <= sbp & sbp < 160) | (90 <= dbp & dbp < 100), 1, 0)
  bp_ii <- ifelse(160 <= sbp | 100 <= dbp, 1, 0)
  i_bp_ii <- (bp_ii == 1)
  bp_opti[i_bp_ii] <- bp_high[i_bp_ii] <- bp_i[i_bp_ii] <- 0
  i_bp_i <- (bp_i == 1)
  bp_opti[i_bp_i] <- bp_high[i_bp_i] <- 0
  i_bp_high <- (bp_high == 1)
  bp_opti[i_bp_high] <- 0

  c_160 <- ifelse(coltot < 160, 1, 0)
  c200_239 <- ifelse(200 <= coltot & coltot < 240, 1, 0)
  c240_279 <- ifelse(240 <= coltot & coltot < 280, 1, 0)
  c280_ <- ifelse(280 <= coltot, 1, 0)
  h_35 <- ifelse(colhdl < 35, 1, 0)
  h35_44 <- ifelse(35 <= colhdl & colhdl < 45, 1, 0)
  h45_49 <- ifelse(45 <= colhdl & colhdl < 50, 1, 0)
  h50_59 <- ifelse(50 <= colhdl & colhdl < 60, 1, 0)
  h60_ <- ifelse(60 <= colhdl, 1, 0)

  men <- (sex == 'H')
  l_chol = rep(0, n)
  l_chol[men] <- (0.04826*age - 0.65945*c_160 + 0.17692*c200_239 + 0.50539*c240_279 +
                    0.65713*c280_ + 0.49744*h_35 + 0.24310*h35_44 - 0.05107*h50_59 - 0.48660*h60_ -
                    0.00226*bp_opti + 0.28320*bp_high + 0.52168*bp_i + 0.61859*bp_ii +
                    0.42839*diabetes + 0.52337*smoker)[men]
  l_chol[!men] <- (0.33766*age - 0.00268*(age^2) - 0.26138*c_160 + 0.20771*c200_239 +
                     0.24385*c240_279 + 0.53513*c280_ + 0.84312*h_35 + 0.377096*h35_44 +
                     0.19785*h45_49 - 0.42951*h60_ - 0.53363*bp_opti - 0.06773*bp_high +
                     0.26288*bp_i + 0.46573*bp_ii + 0.59626*diabetes + 0.29246*smoker)[!men]
  g_chol = rep(0, n)
  g_chol[men] <- 3.489
  g_chol[!men] = 10.279
  b_chol <- exp(l_chol - g_chol)
  result <- rep(0,n)
  result[men] <- (1 - (1 -(1 - 0.951)/divide)^b_chol[men])*100
  result[!men] <- (1 - (1 - (1 - 0.978)/divide)^b_chol[!men])*100
  result
}

#' @title                     Extreure Pglobal SigTest
#' @description               Extreure Pglobal SigTest
#' @param dt                  dt
#' @param vars_pre            vars_pre
#' @param vars_post           vars_post
#' @export                    extreure_Pglobal_SigTest
extreure_Pglobal_SigTest<-function(dt=dades,
                                   vars_pre=vars_pre,
                                   vars_post=vars_post) {
  # NUMERO_76)

  ## Llista 2 parells de llistes de variables tipus pre-post i retorna un únic p-valor test del signes (canvi) de la binomial
  ## S'espera que tots els canvis van cap al mateix sentit (o tots baixen o tots pujen)

  # vars_pre<-c("VLDL_C","IDL_C")
  # vars_post<-c("VLDL_C_FU","IDL_C_FU")
  # dt<-dades

  # vars_pre<-vars_pre
  # vars_post<-vars_post
  # dt<-dades

  dt<-dt %>% dplyr::mutate(id=1:n()) %>%
    dplyr::select(id, vars_pre,vars_post) %>%
    dplyr::rename_at(vars_pre,~paste0("var",c(1:length(vars_post)),"_pre")) %>%
    dplyr::rename_at(vars_post,~paste0("var",c(1:length(vars_post)),"_pos")) %>%
    dplyr::mutate_all(as.numeric)

  longer<-dt %>%
    tidyr::pivot_longer(cols=-1, names_pattern = "(.*)(....)$", names_to = c("var", "temps")) %>%
    dplyr::mutate(temps=dplyr::if_else(temps=="_pre","0","1")) %>%
    tidyr::pivot_wider(id_cols = c(id,temps), names_from = var, values_from = value, names_repair = "check_unique") %>%
    stats::na.omit()

  vars<-paste0("var",c(1:length(vars_post)))

  dt_fi<-longer %>% dplyr::group_by(id) %>% dplyr::summarise_at(vars,list(dif=~.-dplyr::lag(.))) %>%
    dplyr::slice(2) %>% dplyr::ungroup() %>%
    tidyr::pivot_longer(cols=-1) %>%
    dplyr::filter (value!=0) # Elimino els empats

  x<-sum(as.numeric(dt_fi$value>0))
  n<-length(dt_fi$value)

  test_bin<-stats::binom.test(x,n,p=0.5)
  if (test_bin$p.value<0.001) result="<0.001" else result=test_bin$p.value

  return(result)

}


#' @title                    Formula LOGIT
#' @description              Formula LOGIT
#' @param x                  x
#' @param y                  y
#' @param eliminar           eliminar
#' @param a                  a
#' @param taulavariables     taulavariables

formula.LOGIT<-function(x="taula1",
                        y="resposta",
                        eliminar=c("IDP"),
                        a="",
                        taulavariables="variables.xls") {

  # NUMERO_77)

  # no hem posta export.
  #S3 methods shown with full name in documentation object 'formula.LOGIT':
  #  'formula.LOGIT'
  #
  #The \usage entries for S3 methods should use the \method markup and not
  #their full name.
  #See chapter 'Writing R documentation files' in the 'Writing R
  #Extensions' manual.


  #  Formula.LOGIT segons LLISTA DE VARIABLES  D'AJUST     #######################
  #      hi envio la columna de variables amb que vull generar la formula pel compare

  #####     x= variables d'ajust / y = resposta / eliminar /  a = Avaluar

  # x="regicor_alone"
  # y="event"
  # taulavariables = conductor_variables
  # eliminar=c("IDP")
  # a=""

  # Llegir variables
  variables<-read_conductor(taulavariables)
  # variables[is.na(variables)]<- 0
  x_sym<-dplyr::sym(x)

  variables<-variables %>% dplyr::filter(!is.na(!!x_sym))

  llistataula<-variables %>%
    dplyr::filter(!!x_sym>0) %>%
    dplyr::arrange(!!x_sym) %>%
    dplyr::pull(camp)

  llistataula<-llistataula[!llistataula%in%eliminar]

  if (a!="") llistataula<-c(a,llistataula)

  y<-stats::as.formula(paste(y, paste(llistataula, collapse=" + "), sep=" ~ "))

}






