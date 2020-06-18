
sql_patients_referidos_not_in_sync <- " select pat.id, pat.patientid,pat.dateofbirth::TIMESTAMP::DATE as dateofbirth,pat.firstnames as firstnames , 
        pat.sex, pat.lastname as lastname ,pat.uuid, ep.startreason, ep.startdate, ep.clinic,cl.clinicname, at.value
        from patient pat inner join
        (
         select patient, max(startdate), startreason, startdate, clinic
           from episode
            group by patient, startreason ,startdate, clinic

        )  ep on ep.patient = pat.id

 left join sync_temp_patients sync on sync.patientid =  pat.patientid 
 left join patientattribute at on at.patient = pat.id
 left join clinic cl on cl.id=ep.clinic
 
 where  ep.startreason='Referido para outra Farmacia' and   sync.patientid is null and  ep.clinic <>2 ;
"

#' getLocalServerCon  ->  Estabelece uma conexao com o PostgreSQL 
#' 
#' @param postgres.user username do postgres
#' @param postgres.password passwd
#' @param postgres.db.name nome da db no postgres
#' @param postgres.host localhost
#' @return FALSE/Con -  (con) retorna um conexao valida  (FALSE) - erro de conexao   
#' @examples 
#' con_local<- getLocalServerCon()
#' 
getLocalServerCon <- function(){
  
  
  status <- tryCatch({
    
    
    # imprimme uma msg na consola
    message(paste0( "Postgres - conectando-se ao servidor Local : ",
                    local.postgres.host, ' - db:',local.postgres.db.name, "...") )
    
    
    
    # Objecto de connexao com a bd openmrs postgreSQL
    con_postgres <-  dbConnect(PostgreSQL(),user = local.postgres.user,
                               password = local.postgres.password, 
                               dbname = local.postgres.db.name,
                               host = local.postgres.host,
                               port = local.postgres.port )
    
    return(con_postgres)
    
  },
  error = function(cond) {
    
    ## Coisas a fazer se occorre um erro 
    
    # imprimir msg na consola
    
    message(paste0( "PosgreSQL - Nao foi possivel connectar-se ao host: ",
                    local.postgres.host, '  db:',local.postgres.db.name,
                    "...",'user:',local.postgres.user,
                    ' passwd: ', local.postgres.password)) 
    # guardar o log 
    # if(is.farmac){
    #   saveLogError(us.name = farmac_name,
    #                event.date = as.character(Sys.time()),
    #                action = 'getLocalServerCon  ->  Estabelece uma conexao com o PostgreSQL Local ',
    #                error = as.character(cond$message) )  
    #   
    # } else {
    #   
    #   saveLogError(us.name = main_clinic_name ,
    #                event.date = as.character(Sys.time()),
    #                action = 'getLocalServerCon  ->  Estabelece uma conexao com o PostgreSQL Local',
    #                error = as.character(cond$message) )  
    #}
    
    
    
    return(FALSE)
  },
  finally = {
    # NOTE:
    # Here goes everything that should be executed at the end,
    # Do nothing
  })
  
  status
}

  
#' Calcula o peso da diferenca de dois strings atraves do algoritmo de leveingstein / stringdist no R
#' Funcao baseada no algoritmo de simetria de nomes
#' @param string1 do primeiro nome  
#' @param string2 o segudo nome
#' @return numeric distance
#' #' @examples
#' dist  = getStringDistance('agnaldo','agnaldo s')
getStringDistance <- function (string1,string2) {
  
  dist <- stringdist(string1,string2,method = 'jw')
  return(dist)
}