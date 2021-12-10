#' qol
#'
#' Calculates the domain-based scale scores using the data from
#' Quality of Life questionnaire
#'
#' @description Creates a dataset containing the domain-based scale scores using
#' the data from Quality of Life questionnaire
#'
#' @details qol function inputs either a dataset containing missing information, represented as,
#' 9 or 99 or NA or a data not containing any missing information. It extracts only the columns
#' named 'Q1','Q2',...,'Q30' and replaces the missing data with the minimum value of the particular question.
#'
#' Using each of the 30 columns, the Raw Score is computed, and one column is obtained containing
#' the Raw Score for each patient.
#'
#' Further, using each of the Raw Scores, three domain-based Scale Scores are computed,
#' they are, Global Scales Score, Functional Scales Score and Symptoms Scales Score.
#'
#' Thus, the columns 'Q1','Q2',...,'Q30' are replaced by the domain-based scale scores,
#' which is obtained as the output.
#'
#' qol(x)
#'
#' 1) Subject ID column should be named as 'ID'.
#'
#' 2) Each question column should be named as 'Q1' for data from question 1,
#' 'Q2' for data from question 2, and so on until 'Q30' for data from question 30.
#'
#' 3) Data may contain more variables, such as, Age, Gender, etc.
#'
#' x - A data frame with ID, Q1, Q2,..., Q30 columns along with other columns if data
#' is available.
#'
#' rs - A matrix containing the Raw Score computed using all Q1 to Q30 data for each
#' patient. The RS(a) function is used in this case.
#'
#' fs - A matrix containing the Functional Scale Scores computed using all Q1 to Q30
#' data for each patient. The FS(a,b) function is used in this case.
#'
#' ss_gs - A matrix containing the Global Scale Scores computed using all Q1 to Q30
#' data for each patient. The SS_GS(a,b) function is used in this case.
#'
#' final_data - A data frame formed by replacing the columns 'Q1','Q2',...,'Q30' by
#' the domain-based scale scores.
#'
#' @param x A data frame with ID, Q1, Q2,..., Q30 columns along with other columns if data is available.
#'
#' @import dplyr
#'
#' @return A data frame by replacing the columns 'Q1','Q2',...,'Q30' by the domain-based scale scores.
#'
#' @references
#'
#' @examples
#' ##
#' data("sim_x")
#' qol(sim_x)
#' ##
#'
#' @export
#' @author Atanu Bhattacharjee and Ankita Pal
#' @seealso

qol <- function(y){
  d <- as.matrix(select(y,'Q1':'Q30'))

  # Imputing missing values with minimum value of respective question
  for(j in 1:ncol(d)){
    for(i in 1:nrow(d)){
      if(is.na(d[i,j])==TRUE || d[i,j]==9 || d[i,j]==99){
        d[i,j] <- min(d[,j],na.rm = TRUE)
      }
    }
  }

  # Raw Score
  RS <- function(a){
    nr <- nrow(a)
    rs <- rep(0, nr)
    for(i in 1:nr){
      rs[i] <- mean(a[i,])
    }
    return(rs)
  }

  # Functional Scales Score
  FS <- function(a,b){
    nr <- length(a)
    fs <- rep(0, nr)
    for(i in 1:nr){
      z <- (a[i]-1)/diff(range(b))
      fs[i] <- (1-z)*100
    }
    return(fs)
  }

  # Symptoms Scales Score
  SS_GS <- function(a,b){
    nr <- length(a)
    ss_gs <- rep(0, nr)
    for(i in 1:nr){
      ss_gs[i] <- ((a[i]-1)/diff(range(b)))*100
    }
    return(ss_gs)
  }

  # Dataset with Raw Scores
  RS_data <- data.frame(RS_QL = RS(d[,c(29,30)]),
                        RS_PF = RS(d[,1:5]),
                        RS_RF = RS(d[,c(6,7)]),
                        RS_EF = RS(d[,21:24]),
                        RS_CF = RS(d[,c(20,25)]),
                        RS_SF = RS(d[,c(26,27)]),
                        RS_FA = RS(d[,c(10,12,18)]),
                        RS_NV = RS(d[,c(14,15)]),
                        RS_PA = RS(d[,c(9,19)]),
                        RS_DY = d[,8],
                        RS_SL = d[,11],
                        RS_AP = d[,13],
                        RS_CO = d[,16],
                        RS_DI = d[,17],
                        RS_FI = d[,28])

  # Dataset with Score Values
  score_data <- data.frame(QL = SS_GS(RS_data$RS_QL,d[,c(29,30)]),
                           PF = FS(RS_data$RS_PF,d[,1:5]),
                           RF = FS(RS_data$RS_RF,d[,c(6,7)]),
                           EF = FS(RS_data$RS_EF,d[,21:24]),
                           CF = FS(RS_data$RS_CF,d[,c(20,25)]),
                           SF = FS(RS_data$RS_SF,d[,c(26,27)]),
                           FA = SS_GS(RS_data$RS_FA,d[,c(10,12,18)]),
                           NV = SS_GS(RS_data$RS_NV,d[,c(14,15)]),
                           PA = SS_GS(RS_data$RS_PA,d[,c(9,19)]),
                           DY = SS_GS(RS_data$RS_DY,d[,8]),
                           SL = SS_GS(RS_data$RS_SL,d[,11]),
                           AP = SS_GS(RS_data$RS_AP,d[,13]),
                           CO = SS_GS(RS_data$RS_CO,d[,16]),
                           DI = SS_GS(RS_data$RS_DI,d[,17]),
                           FI = SS_GS(RS_data$RS_FI,d[,28]))
  new_data <- select(y,-('Q1':'Q30'))
  final_data <- data.frame(new_data,score_data)
  return(final_data)
}
#' miss_patient
#'
#' Calculates the domain-based scale scores using the data from
#' Quality of Life questionnaire
#'
#' @description Creates a dataset containing the domain-based scale scores using
#' the data from Quality of Life questionnaire
#'
#' @details miss_patient function inputs a dataset in which the information of some patients
#' are completely missing. The information of these patients are omitted from the data
#' and only the columns named 'Q1','Q2',...,'Q30' are extracted.
#'
#' Using each of the 30 columns, the Raw Score is computed, and one column is obtained containing
#' the Raw Score for each patient.
#'
#' Further, using each of the Raw Scores, three domain-based Scale Scores are computed,
#' they are, Global Scales Score, Functional Scales Score and Symptoms Scales Score.
#'
#' Thus, the columns 'Q1','Q2',...,'Q30' are replaced by the domain-based scale scores,
#' which is obtained as the output.
#'
#' miss_patient(x)
#'
#' 1) Subject ID column should be named as 'ID'.
#'
#' 2) Each question column should be named as 'Q1' for data from question 1,
#' 'Q2' for data from question 2, and so on until 'Q30' for data from question 30.
#'
#' 3) Data may contain more variables, such as, Age, Gender, etc.
#'
#' x - A data frame with ID, Q1, Q2,..., Q30 columns along with other columns if data
#' is available.
#'
#' rs - A matrix containing the Raw Score computed using all Q1 to Q30 data for each
#' patient. The RS(a) function is used in this case.
#'
#' fs - A matrix containing the Functional Scale Scores computed using all Q1 to Q30
#' data for each patient. The FS(a,b) function is used in this case.
#'
#' ss_gs - A matrix containing the Global Scale Scores computed using all Q1 to Q30
#' data for each patient. The SS_GS(a,b) function is used in this case.
#'
#' final_data - A data frame formed by replacing the columns 'Q1','Q2',...,'Q30' by
#' the domain-based scale scores.
#'
#' @param x A data frame with ID, Q1, Q2,..., Q30 columns along with other columns if data is available.
#'
#' @import dplyr
#'
#' @return A data frame by replacing the columns 'Q1','Q2',...,'Q30' by the domain-based scale scores.
#'
#' @references
#'
#' @examples
#' ##
#' data("sim_x")
#' miss_patient(sim_x)
#' ##
#'
#' @export
#' @author Atanu Bhattacharjee and Ankita Pal
#' @seealso

miss_patient <- function(y){
  qdata <- as.matrix(select(y,'Q1':'Q30'))

  # Remove patient details
  d <- qdata[-c(which(apply(qdata, 1, function(x) ifelse(((sum(x)<=0 | is.na(sum(x)))), TRUE, FALSE)) == TRUE)),]

  # Raw Score
  RS <- function(a){
    nr <- nrow(a)
    rs <- rep(0, nr)
    for(i in 1:nr){
      rs[i] <- mean(a[i,])
    }
    return(rs)
  }

  # Functional Scales Score
  FS <- function(a,b){
    nr <- length(a)
    fs <- rep(0, nr)
    for(i in 1:nr){
      z <- (a[i]-1)/diff(range(b))
      fs[i] <- (1-z)*100
    }
    return(fs)
  }

  # Symptoms Scales Score
  SS_GS <- function(a,b){
    nr <- length(a)
    ss_gs <- rep(0, nr)
    for(i in 1:nr){
      ss_gs[i] <- ((a[i]-1)/diff(range(b)))*100
    }
    return(ss_gs)
  }

  # Dataset with Raw Scores
  RS_data <- data.frame(RS_QL = RS(d[,c(29,30)]),
                        RS_PF = RS(d[,1:5]),
                        RS_RF = RS(d[,c(6,7)]),
                        RS_EF = RS(d[,21:24]),
                        RS_CF = RS(d[,c(20,25)]),
                        RS_SF = RS(d[,c(26,27)]),
                        RS_FA = RS(d[,c(10,12,18)]),
                        RS_NV = RS(d[,c(14,15)]),
                        RS_PA = RS(d[,c(9,19)]),
                        RS_DY = d[,8],
                        RS_SL = d[,11],
                        RS_AP = d[,13],
                        RS_CO = d[,16],
                        RS_DI = d[,17],
                        RS_FI = d[,28])

  # Dataset with Score Values
  score_data <- data.frame(QL = SS_GS(RS_data$RS_QL,d[,c(29,30)]),
                           PF = FS(RS_data$RS_PF,d[,1:5]),
                           RF = FS(RS_data$RS_RF,d[,c(6,7)]),
                           EF = FS(RS_data$RS_EF,d[,21:24]),
                           CF = FS(RS_data$RS_CF,d[,c(20,25)]),
                           SF = FS(RS_data$RS_SF,d[,c(26,27)]),
                           FA = SS_GS(RS_data$RS_FA,d[,c(10,12,18)]),
                           NV = SS_GS(RS_data$RS_NV,d[,c(14,15)]),
                           PA = SS_GS(RS_data$RS_PA,d[,c(9,19)]),
                           DY = SS_GS(RS_data$RS_DY,d[,8]),
                           SL = SS_GS(RS_data$RS_SL,d[,11]),
                           AP = SS_GS(RS_data$RS_AP,d[,13]),
                           CO = SS_GS(RS_data$RS_CO,d[,16]),
                           DI = SS_GS(RS_data$RS_DI,d[,17]),
                           FI = SS_GS(RS_data$RS_FI,d[,28]))
  new_data <- select(data.frame(d),-('Q1':'Q30'))
  final_data <- data.frame(new_data,score_data)
  return(final_data)
}

#' lc_miss
#'
#' Calculates the domain-based scale scores using the data of QLQ-LC13
#'
#' @description Creates a dataset containing the domain-based scale scores using
#' the data from QLQ-LC13
#'
#' @details lc_miss function inputs either a dataset containing missing information, represented as,
#' 9 or 99 or NA or a data not containing any missing information. It extracts only the columns
#' named 'LC_Q31','LC_Q32',...,'LC_Q42' and replaces the missing data with the minimum value of the particular question.
#'
#' Using each of the 30 columns, the Raw Score is computed, and one column is obtained containing
#' the Raw Score for each patient.
#'
#' Further, using each of the Raw Scores, three domain-based Scale Scores are computed,
#' they are, Global Scales Score, Functional Scales Score and Symptoms Scales Score.
#'
#' Thus, the columns 'LC_Q31','LC_Q32',...,'LC_Q42' are replaced by the domain-based scale scores,
#' which is obtained as the output.
#'
#' lc_miss(x)
#'
#' 1) Subject ID column should be named as 'ID'.
#'
#' 2) Each question column should be named as 'LC_Q31' for data from question 31,
#' 'LC_Q32' for data from question 32, and so on until 'LC_Q42' for data from question 42.
#'
#' 3) Data may contain more variables, such as, Age, Gender, etc.
#'
#' x - A data frame with ID, LC_Q31,LC_Q32,...,LC_Q42 columns along with other columns if data
#' is available.
#'
#' rs - A matrix containing the Raw Score computed using all LC_Q31 to LC_Q42 data for each
#' patient. The RS(a) function is used in this case.
#'
#' ss - A matrix containing the Global Scale Scores computed using all LC_Q31 to LC_Q42
#' data for each patient. The SS(a,b) function is used in this case.
#'
#' final_data - A data frame formed by replacing the columns 'LC_Q31','LC_Q32',...,'LC_Q42' by
#' the domain-based scale scores.
#'
#' @param x A data frame with ID, LC_Q31,LC_Q32,...,LC_Q42 columns along with other columns if data is available.
#'
#' @import dplyr
#'
#' @return A data frame by replacing the columns 'LC_Q31','LC_Q32',...,'LC_Q42' by the domain-based scale scores.
#'
#' @references
#'
#' @examples
#' ##
#' data("lc_df")
#' lc_miss(lc_df)
#' ##
#'
#' @export
#' @author Atanu Bhattacharjee and Ankita Pal
#' @seealso

lc_miss <- function(y){
  d <- as.matrix(select(y,'LC_Q31':'LC_Q42'))

  # Imputing missing values with minimum value of respective question
  for(j in 1:ncol(d)){
    for(i in 1:nrow(d)){
      if(is.na(d[i,j])==TRUE || d[i,j]==9 || d[i,j]==99){
        d[i,j] <- min(d[,j],na.rm = TRUE)
      }
    }
  }

  # Raw Score
  RS <- function(a){
    nr <- nrow(a)
    rs <- rep(0, nr)
    for(i in 1:nr){
      rs[i] <- mean(a[i,])
    }
    return(rs)
  }

  # Symptoms Scales Score
  SS <- function(a,b){
    nr <- length(a)
    ss <- rep(0, nr)
    for(i in 1:nr){
      ss[i] <- ((a[i]-1)/diff(range(b)))*100
    }
    return(ss)
  }

  # Dataset with Raw Scores
  RS_data <- data.frame(RS_LCDY = RS(d[,3:5]),
                        RS_LCCO = d[,1],
                        RS_LCHA = d[,2],
                        RS_LCSM = d[,6],
                        RS_LCDS = d[,7],
                        RS_LCPN = d[,8],
                        RS_LCHR = d[,9],
                        RS_LCPC = d[,10],
                        RS_LCPA = d[,11],
                        RS_LCPO = d[,12])

  # Dataset with Score Values
  score_data <- data.frame(LCDY = SS(RS_data$RS_LCDY,d[,3:5]),
                           LCCO = SS(RS_data$RS_LCCO,d[,1]),
                           LCHA = SS(RS_data$RS_LCHA,d[,2]),
                           LCSM = SS(RS_data$RS_LCSM,d[,6]),
                           LCDS = SS(RS_data$RS_LCDS,d[,7]),
                           LCPN = SS(RS_data$RS_LCPN,d[,8]),
                           LCHR = SS(RS_data$RS_LCHR,d[,9]),
                           LCPC = SS(RS_data$RS_LCPC,d[,10]),
                           LCPA = SS(RS_data$RS_LCPA,d[,11]),
                           LCPO = SS(RS_data$RS_LCPO,d[,12]))
  new_data <- select(y,-('LC_Q31':'LC_Q42'))
  final_data <- data.frame(new_data,score_data)
  return(final_data)
}

#' hnc_miss
#'
#' Calculates the domain-based scale scores using the data of QLQ-HN35
#'
#' @description Creates a dataset containing the domain-based scale scores using
#' the data from QLQ-HN35
#'
#' @details hn_miss function inputs either a dataset containing missing information, represented as,
#' 9 or 99 or NA or a data not containing any missing information. It extracts only the columns
#' named 'HN_Q31','HN_Q32',...,'HN_Q65' and replaces the missing data with the minimum value of the particular question.
#'
#' Using each of the 30 columns, the Raw Score is computed, and one column is obtained containing
#' the Raw Score for each patient.
#'
#' Further, using each of the Raw Scores, three domain-based Scale Scores are computed,
#' they are, Global Scales Score, Functional Scales Score and Symptoms Scales Score.
#'
#' Thus, the columns 'HN_Q31','HN_Q32',...,'HN_Q65' are replaced by the domain-based scale scores,
#' which is obtained as the output.
#'
#' hnc_miss(x)
#'
#' 1) Subject ID column should be named as 'ID'.
#'
#' 2) Each question column should be named as 'HN_Q31' for data from question 31,
#' 'HN_Q32' for data from question 32, and so on until 'HN_Q65' for data from question 65.
#'
#' 3) Data may contain more variables, such as, Age, Gender, etc.
#'
#' x - A data frame with ID, HN_Q31,HN_Q32,...,HN_Q65 columns along with other columns if data
#' is available.
#'
#' rs - A matrix containing the Raw Score computed using all HN_Q31 to HN_Q65 data for each
#' patient. The RS(a) function is used in this case.
#'
#' ss - A matrix containing the Global Scale Scores computed using all HN_Q31 to HN_Q65
#' data for each patient. The SS(a,b) function is used in this case.
#'
#' final_data - A data frame formed by replacing the columns 'HN_Q31','HN_Q32',...,'HN_Q65' by
#' the domain-based scale scores.
#'
#' @param x A data frame with ID, HN_Q31,HN_Q32,...,HN_Q65 columns along with other columns if data is available.
#'
#' @import dplyr
#'
#' @return A data frame by replacing the columns 'HN_Q31','HN_Q32',...,'HN_Q65' by the domain-based scale scores.
#'
#' @references
#'
#' @examples
#' ##
#' data("hnc_df")
#' hnc_miss(hnc_df)
#' ##
#'
#' @export
#' @author Atanu Bhattacharjee and Ankita Pal
#' @seealso

hnc_miss <- function(y){
  d <- as.matrix(select(y,'HN_Q31':'HN_Q65'))

  # Imputing missing values with minimum value of respective question
  for(j in 1:ncol(d)){
    for(i in 1:nrow(d)){
      if(is.na(d[i,j])==TRUE || d[i,j]==9 || d[i,j]==99){
        d[i,j] <- min(d[,j],na.rm = TRUE)
      }
    }
  }

  # Raw Score
  RS <- function(a){
    nr <- nrow(a)
    rs <- rep(0, nr)
    for(i in 1:nr){
      rs[i] <- mean(a[i,])
    }
    return(rs)
  }

  # Symptoms Scales Score
  SS <- function(a,b){
    nr <- length(a)
    ss <- rep(0, nr)
    for(i in 1:nr){
      ss[i] <- ((a[i]-1)/diff(range(b)))*100
    }
    return(ss)
  }

  # Dataset with Raw Scores
  RS_data <- data.frame(RS_HNPA = RS(d[,1:4]),
                        RS_HNSW = RS(d[,5:8]),
                        RS_HNSE = RS(d[,c(13,14)]),
                        RS_HNSP = RS(d[,c(16,23,24)]),
                        RS_HNSO = RS(d[,19:22]),
                        RS_HNSC = RS(d[,c(18,25,26,27,28)]),
                        RS_HNSX = RS(d[,c(29,30)]),
                        RS_HNTE = d[,9],
                        RS_HNOM = d[,10],
                        RS_HNDR = d[,11],
                        RS_HNSS = d[,12],
                        RS_HNCO = d[,15],
                        RS_HNFI = d[,17],
                        RS_HNPK = d[,31],
                        RS_HNNU = d[,32],
                        RS_HNFE = d[,33],
                        RS_HNWL = d[,34],
                        RS_HNWG = d[,35])

  # Dataset with Score Values
  score_data <- data.frame(HNPA = SS(RS_data$RS_HNPA,d[,1:4]),
                           HNSW = SS(RS_data$RS_HNSW,d[,5:8]),
                           HNSE = SS(RS_data$RS_HNSE,d[,c(13,14)]),
                           HNSP = SS(RS_data$RS_HNSP,d[,c(16,23,24)]),
                           HNSO = SS(RS_data$RS_HNSO,d[,19:22]),
                           HNSC = SS(RS_data$RS_HNSC,d[,c(18,25,26,27,28)]),
                           HNSX = SS(RS_data$RS_HNSX,d[,c(29,30)]),
                           HNTE = SS(RS_data$RS_HNTE,d[,9]),
                           HNOM = SS(RS_data$RS_HNOM,d[,10]),
                           HNDR = SS(RS_data$RS_HNDR,d[,11]),
                           HNSS = SS(RS_data$RS_HNSS,d[,12]),
                           HNCO = SS(RS_data$RS_HNCO,d[,15]),
                           HNFI = SS(RS_data$RS_HNFI,d[,17]),
                           HNPK = SS(RS_data$RS_HNPK,d[,31]),
                           HNNU = SS(RS_data$RS_HNNU,d[,32]),
                           HNFE = SS(RS_data$RS_HNFE,d[,33]),
                           HNWL = SS(RS_data$RS_HNWL,d[,34]),
                           HNWG = SS(RS_data$RS_HNWG,d[,35]))
  new_data <- select(y,-('HN_Q31':'HN_Q65'))
  final_data <- data.frame(new_data,score_data)
  return(final_data)
}

#' brc_miss
#'
#' Calculates the domain-based scale scores using the data of QLQ-BR23
#'
#' @description Creates a dataset containing the domain-based scale scores using
#' the data from QLQ-BR23
#'
#' @details brc_miss function inputs either a dataset containing missing information, represented as,
#' 9 or 99 or NA or a data not containing any missing information. It extracts only the columns
#' named 'BR_Q31','BR_Q32',...,'BR_Q53' and replaces the missing data with the minimum value of the particular question.
#'
#' Using each of the 30 columns, the Raw Score is computed, and one column is obtained containing
#' the Raw Score for each patient.
#'
#' Further, using each of the Raw Scores, three domain-based Scale Scores are computed,
#' they are, Global Scales Score, Functional Scales Score and Symptoms Scales Score.
#'
#' Thus, the columns 'BR_Q31','BR_Q32',...,'BR_Q53' are replaced by the domain-based scale scores,
#' which is obtained as the output.
#'
#' brc_miss(x)
#'
#' 1) Subject ID column should be named as 'ID'.
#'
#' 2) Each question column should be named as 'BR_Q31' for data from question 31,
#' 'BR_Q32' for data from question 32, and so on until 'BR_Q53' for data from question 53
#'
#' 3) Data may contain more variables, such as, Age, Gender, etc.
#'
#' x - A data frame with ID, BR_Q31,BR_Q32,...,BR_Q53 columns along with other columns if data
#' is available.
#'
#' rs - A matrix containing the Raw Score computed using all BR_Q31 to BR_Q53 data for each
#' patient. The RS(a) function is used in this case.
#'
#' fs - A matrix containing the Functional Scale Scores computed using all BR_Q31 to BR_Q53
#' data for each patient. The FS(a,b) function is used in this case.
#'
#' ss - A matrix containing the Global Scale Scores computed using all BR_Q31 to BR_Q53
#' data for each patient. The SS(a,b) function is used in this case.
#'
#' final_data - A data frame formed by replacing the columns 'BR_Q31','BR_Q32',...,'BR_Q53' by
#' the domain-based scale scores.
#'
#' @param x A data frame with ID, BR_Q31,BR_Q32,...,BR_Q53 columns along with other columns if data is available.
#'
#' @import dplyr
#'
#' @return A data frame by replacing the columns 'BR_Q31','BR_Q32',...,'BR_Q53' by the domain-based scale scores.
#'
#' @references
#'
#' @examples
#' ##
#' data("brc_df")
#' brc_miss(brc_df)
#' ##
#'
#' @export
#' @author Atanu Bhattacharjee and Ankita Pal
#' @seealso

brc_miss <- function(y){
  d <- as.matrix(select(y,'BR_Q31':'BR_Q53'))

  # Imputing missing values with minimum value of respective question
  for(j in 1:ncol(d)){
    for(i in 1:nrow(d)){
      if(is.na(d[i,j])==TRUE || d[i,j]==9 || d[i,j]==99){
        d[i,j] <- min(d[,j],na.rm = TRUE)
      }
    }
  }

  # Raw Score
  RS <- function(a){
    nr <- nrow(a)
    rs <- rep(0, nr)
    for(i in 1:nr){
      rs[i] <- mean(a[i,])
    }
    return(rs)
  }

  # Functional Scales Score
  FS <- function(a,b){
    nr <- length(a)
    fs <- rep(0, nr)
    for(i in 1:nr){
      z <- (a[i]-1)/diff(range(b))
      fs[i] <- (1-z)*100
    }
    return(fs)
  }

  # Symptoms Scales Score
  SS <- function(a,b){
    nr <- length(a)
    ss <- rep(0, nr)
    for(i in 1:nr){
      ss[i] <- ((a[i]-1)/diff(range(b)))*100
    }
    return(ss)
  }

  # Dataset with Raw Scores
  RS_data <- data.frame(RS_BRBI = RS(d[,9:12]),
                        RS_BRSEF = RS(d[,c(14,15)]),
                        RS_BRSEE = d[,16],
                        RS_BRFU = d[,13],
                        RS_BRST = RS(d[,c(1,2,3,4,6,7,8)]),
                        RS_BRBS = RS(d[,20:23]),
                        RS_BRAS = RS(d[,17:19]),
                        RS_BRHL = d[,5])

  # Dataset with Score Values
  score_data <- data.frame(BRBI = FS(RS_data$RS_BRBI,d[,9:12]),
                           BRSEF = SS(RS_data$RS_BRSEF,d[,c(14,15)]),
                           BRSEE = SS(RS_data$RS_BRSEE,d[,16]),
                           BRFU = FS(RS_data$RS_BRFU,d[,13]),
                           BRST = SS(RS_data$RS_BRST,d[,c(1,2,3,4,6,7,8)]),
                           BRBS = SS(RS_data$RS_BRBS,d[,20:23]),
                           BRAS = SS(RS_data$RS_BRAS,d[,17:19]),
                           BRHL = SS(RS_data$RS_BRHL,d[,5]))
  new_data <- select(y,-('BR_Q31':'BR_Q53'))
  final_data <- data.frame(new_data,score_data)
  return(final_data)
}

#' ovc_miss
#'
#' Calculates the domain-based scale scores using the data of QLQ-OV28
#'
#' @description Creates a dataset containing the domain-based scale scores using
#' the data from QLQ-OV28
#'
#' @details brc_miss function inputs either a dataset containing missing information, represented as,
#' 9 or 99 or NA or a data not containing any missing information. It extracts only the columns
#' named 'OV_Q31','OV_Q32',...,'OV_Q58' and replaces the missing data with the minimum value of the particular question.
#'
#' Using each of the 30 columns, the Raw Score is computed, and one column is obtained containing
#' the Raw Score for each patient.
#'
#' Further, using each of the Raw Scores, three domain-based Scale Scores are computed,
#' they are, Global Scales Score, Functional Scales Score and Symptoms Scales Score.
#'
#' Thus, the columns 'OV_Q31','OV_Q32',...,'OV_Q58' are replaced by the domain-based scale scores,
#' which is obtained as the output.
#'
#' ovc_miss(x)
#'
#' 1) Subject ID column should be named as 'ID'.
#'
#' 2) Each question column should be named as 'OV_Q31' for data from question 31,
#' 'OV_Q32' for data from question 32, and so on until 'OV_Q58' for data from question 58
#'
#' 3) Data may contain more variables, such as, Age, Gender, etc.
#'
#' x - A data frame with ID, OV_Q31,OV_Q32,...,OV_Q58 columns along with other columns if data
#' is available.
#'
#' rs - A matrix containing the Raw Score computed using all OV_Q31 to OV_Q58 data for each
#' patient. The RS(a) function is used in this case.
#'
#' ss - A matrix containing the Global Scale Scores computed using all OV_Q31 to OV_Q58
#' data for each patient. The SS(a,b) function is used in this case.
#'
#' final_data - A data frame formed by replacing the columns 'OV_Q31','OV_Q32',...,'OV_Q58' by
#' the domain-based scale scores.
#'
#' @param x A data frame with ID, OV_Q31,OV_Q32,...,OV_Q58 columns along with other columns if data is available.
#'
#' @import dplyr
#'
#' @return A data frame by replacing the columns 'OV_Q31','OV_Q32',...,'OV_Q58' by the domain-based scale scores.
#'
#' @references
#'
#' @examples
#' ##
#' data("ovc_df")
#' ovc_miss(ovc_df)
#' ##
#'
#' @export
#' @author Atanu Bhattacharjee and Ankita Pal
#' @seealso

ovc_miss <- function(y){
  d <- as.matrix(select(y,'OV_Q31':'OV_Q58'))

  # Imputing missing values with minimum value of respective question
  for(j in 1:ncol(d)){
    for(i in 1:nrow(d)){
      if(is.na(d[i,j])==TRUE || d[i,j]==9 || d[i,j]==99){
        d[i,j] <- min(d[,j],na.rm = TRUE)
      }
    }
  }

  # Raw Score
  RS <- function(a){
    nr <- nrow(a)
    rs <- rep(0, nr)
    for(i in 1:nr){
      rs[i] <- mean(a[i,])
    }
    return(rs)
  }

  # Symptoms Scales Score
  SS <- function(a,b){
    nr <- length(a)
    ss <- rep(0, nr)
    for(i in 1:nr){
      ss[i] <- ((a[i]-1)/diff(range(b)))*100
    }
    return(ss)
  }

  # Dataset with Raw Scores
  RS_data <- data.frame(RS_GI = RS(d[,1:6]),
                        RS_PN = RS(d[,c(11,12)]),
                        RS_HOR = RS(d[,c(18,19)]),
                        RS_BI = RS(d[,c(20,21)]),
                        RS_AD = RS(d[,c(22,23,24)]),
                        RS_CSE = RS(d[,13:17]),
                        RS_SI = RS(d[,7:10]),
                        RS_SX = RS(d[,25:28]))

  # Dataset with Score Values
  score_data <- data.frame(Abdominal_GI = SS(RS_data$RS_GI,d[,1:6]),
                           Peripheral_Neuropathy = SS(RS_data$RS_PN,d[,c(11,12)]),
                           Hormonal = SS(RS_data$RS_HOR,d[,c(18,19)]),
                           Body_Image = SS(RS_data$RS_BI,d[,c(20,21)]),
                           Attitude_to_Disease = SS(RS_data$RS_AD,d[,c(22,23,24)]),
                           Chemotherapy_side_effects = SS(RS_data$RS_CSE,d[,13:17]),
                           Other_Single_Items = SS(RS_data$RS_SI,d[,7:10]),
                           Sexuality = SS(RS_data$RS_SX,d[,25:28]))
  new_data <- select(y,-('OV_Q31':'OV_Q58'))
  final_data <- data.frame(new_data,score_data)
  return(final_data)
}
