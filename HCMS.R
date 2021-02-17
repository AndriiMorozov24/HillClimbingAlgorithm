(
  WD <- getwd()
)
if (!is.null(WD))
  setwd(WD)

main <- function(.name,.lms,.lbp){
  
  df <- read.csv(.name, sep = ";", row.names = "Zadanie")
  
  N <- nrow(df) # number of tasks
  NM <- ncol(df) # number of machines
  LiczbaBezPolepszen <- .lbp #ustawiamy jaka chcemy
  LiczbaMultiStart <- .lms #ustawiamy jaka chcemy
  .Min <- NULL
  
  Fitness <- function(.vector) {
    temp <- rep(0, NM) # how much time each machine will work with given schedule
    for (i in 1:N) {
      ind <- .vector[i] # number of current task
      for (j in 1:NM) {
        if (j != 1) {
          # for other machines
          if (temp[j] < temp[j - 1]) {
            # calculating "makespan"
            temp[j] <-
              temp[j - 1] + df[[j]][[ind]] # starting time of the next task on the machine [j]
          } else {
            temp[j] <-
              temp[j] + df[[j]][[ind]] # starting time of the next task on the machine [j]
          }
        } else{
          # for the first machine
          temp[j] <-
            temp[j] + df[[j]][[ind]] # starting time of the next task on the first machine
        }
      }
    }
    return(max(temp)) # we are interested to accomplish all tasks, so we choose max of time
  }
  
  Swap <- function(vectorM.){
    vector. <- sample(1:N, 2, replace = F)
    for (j in 1:N){
      if (vectorM.[j] == vector.[1]){
        Find <- j
      }
      if (vectorM.[j] == vector.[2]){
        Sind <- j
      }
    }
    temp <- vectorM.[Find]
    vectorM.[Find] <- vectorM.[Sind]
    vectorM.[Sind] <- temp
    return(vectorM.)
  } # function that swap 2 random genes
  
  cat("Current file = ",
      .name,
      "\n",
      file = "output.txt",
      append = TRUE)
  
  
  for(i in 1:LiczbaMultiStart) {
    Schedule <- sample(1:N, N, replace = F) # random generated scheduling
    curMin <- Fitness(Schedule) # current scheduling time
    cat("\n", file = "output.txt", append = TRUE)
    cat("Start schedule for the start number = ",i,"\n", file = "output.txt", append = TRUE)
    capture.output(Schedule, file = "output.txt", append = TRUE)
    cat("\n", file = "output.txt", append = TRUE)
    count <- 0
    repeat {
      Kandydat <- Swap(Schedule) # swap
      Kandydat.Min <- Fitness(Kandydat) # checking new scheduling time
      dE <- Kandydat.Min - curMin
      if (dE < 0) {
        # if we found better scheduling time
        Schedule <- Kandydat # new scheduling
        curMin <- Kandydat.Min # override with the new optimal scheduling time
        count <- 0
      } else{
        # if not
        count <- count + 1
      }
      if (count == LiczbaBezPolepszen) {
        # stop condition
        cat("Current best for start number = ",i,"\n",file = "output.txt",append = TRUE)
        cat("Current best local max = ",curMin,"\n",file = "output.txt",append = TRUE)
        .Min[i] <- curMin
        cat("Optimal schedule", "\n", file = "output.txt", append = TRUE)
        capture.output(Schedule, file = "output.txt", append = TRUE)
        break
      }
    }
  }
  cat("Best schedule of the probe = ",min(.Min),"\n",file = "output.txt",append = TRUE)
}
Names <- c("Dane_S2_50_10.csv")
nProb <- 30 #liczba prob
LBP <- c(50,100,150,200,250) #wpisuj ile chcesz liczb, tu sa ile bedzie iteracji bez powtorzen
LMS <- c(20) # to samo dla liczby Multi-startow

for (i in 1:length(Names)) {
  for (j in 1:length(LMS)){
    for(k in 1:length(LBP)){
      for(x in 1:nProb){
        cat("Current number of probes = ",x,"\n",file = "output.txt",append = TRUE)
        cat("Current number of Multi-starts = ",LMS[j],"\n",file = "output.txt",append = TRUE)
        cat("Liczba iteracji bez polepszenia (warunek zatrzymania) = ",LBP[k],"\n",file = "output.txt",append = TRUE)
        main(Names[i],LMS[j],LBP[k])
        print("end")
      }
    }
  }
}
