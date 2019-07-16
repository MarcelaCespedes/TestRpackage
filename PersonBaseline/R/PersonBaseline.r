#' For AIBL longitudinal data, create columns 
#' person: numerical person ID, max(person) returns the number of participants, 
#' baseline: value 1 for baseline observation of participant
#' NumberReps: number of replicates for each person
#' NOTE: data must be in AIBL.ID and Collection order.
#' @param ids: processed IDS data frame with AIBL.ID and other columns
#' @keywords process IDS
#' @export
#' @examples 
#' ids<- data.frame(AIBL.ID = rep(c(3, 54, 79), each=4), Collection = rep(c(1:4), 3), CSF = runif(12, min = 500, max = 600))
#' ids<- PersonBaseline(ids)
#' ids
PersonBaseline<- function(ids){
  
  no.ppl<- length(unique(ids$AIBL.ID))
  pplc<- 1:no.ppl
  person<-c(); baseline<- rep(NA, dim(ids)[1])
  reps<- rep(NA, dim(ids)[1])
  counter <- 1
  r.counter = 2
  
  for(i in 2:dim(ids)[1]){
    
    if(ids$AIBL.ID[i]== ids$AIBL.ID[i-1]){     
      person[i]<-pplc[counter]
      reps[i]<- r.counter
      r.counter = r.counter + 1
      
    }
    else{
      if(ids$AIBL.ID[i] != ids$AIBL.ID[i-1]) {
        counter<- counter + 1 
        person[i]<- pplc[counter]
        baseline[i]<- 1
        
        r.counter = 1
        reps[i]<- r.counter
        r.counter = r.counter + 1
      }  
    }    
  }  
  person[1]<-1; baseline[1]<- reps[1]<- 1 
  ids$person<- person
  ids$NumberReps<- reps
  ids$baseline<- baseline
  
  # this underestimates some individuals (don't get assigned a baseline value)
  # for mci.mildAD, these individuals below were not assigned a baseline
  # setdiff(unique.ids, bl.dat)
  #[1]  345  373  385  591  619  779  800  902  924  963  964 1175 1240 1254 1276 1277 1352 1393 1473
  #[20] 1608
  
  ######################
  return(ids)
}