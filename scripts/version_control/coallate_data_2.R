
# These read in the various colonies and configures trips
{
  source("scripts/init_fun.R")
  source("scripts/colony_inputs/lsl_in.R")
  source("scripts/colony_inputs/ann_in.R")
  source("scripts/colony_inputs/bob_in.R")
  source("scripts/colony_inputs/cop_in.R")
  source("scripts/colony_inputs/fai_in.R")
  source("scripts/colony_inputs/mks_in.R")
  source("scripts/colony_inputs/stm_in.R")
  source("scripts/colony_inputs/swo_in.R")
  source("scripts/colony_inputs/win_in.R")
}

# Assign a colony variable 
{
  fulmar_lsl$colony <- rep("lsl", nrow(fulmar_lsl))
  fulmar_ann$colony <- rep("ann", nrow(fulmar_ann))
  fulmar_bob$colony <- rep("bob", nrow(fulmar_bob))
  fulmar_cop$colony <- rep("cop", nrow(fulmar_cop))
  fulmar_fai$colony <- rep("fai", nrow(fulmar_fai))
  fulmar_mks$colony <- rep("mks", nrow(fulmar_mks))
  fulmar_stm$colony <- rep("stm", nrow(fulmar_stm))
  fulmar_swo$colony <- rep("swo", nrow(fulmar_swo))
  fulmar_win$colony <- rep("win", nrow(fulmar_win))
}

# var list for stripping data
var <- c("Latitude", "Longitude", "ID", "date_time",
         "PointDist", "trip_id", "returns", "colony", 
         "CloudLow", "CloudMedium", "CloudHigh", "CloudTotal",
         "WindU", "WindV", "ChlA", "SST", "Vapour", "Wave", "Charnock")

# Strip 
fulmar_trip <- rbind(fulmar_lsl[,var],
                     fulmar_ann[,var],
                     fulmar_bob[,var],
                     fulmar_cop[,var],
                     fulmar_fai[,var],
                     fulmar_mks[,var],
                     fulmar_stm[,var],
                     fulmar_swo[,var],
                     fulmar_win[,var])

rm(fulmar_lsl,
   fulmar_ann,
   fulmar_bob,
   fulmar_cop,
   fulmar_fai,
   fulmar_mks,
   fulmar_stm,
   fulmar_swo,
   fulmar_win,
   var)

fulmar_trip <-
  WGS2UTM30(fulmar_trip)

ls1 <- split(fulmar_trip,
             fulmar_trip$trip_id)[
               sapply(
                 split(fulmar_trip,
                       fulmar_trip$trip_id),
                 function(x) dim(x)[1]) > 0]

ls2 <- lapply(ls1, SCRStepR, t = 60)

step <- (do.call(rbind, ls2))$step

ls3 <- lapply(ls2, PointWeight)

hmm_data <- do.call(rbind, ls3) %>%
  rename(id = ID,
         ID = trip_id,
         date = date_time)

var <- c("x","y","ID", "date", "point_weight")

hmm_prep <-
  momentuHMM::prepData(hmm_data[var], type="UTM")

rm(ls1, ls2, ls3, var)

hmm_prep$step <- step

hmm_prep$step[which(hmm_prep$step == 0)] <- 1

ls <- split(hmm_prep, hmm_prep$ID)

ls <- lapply(ls, function(x) {
  for(i in 2:nrow(x)){
    if(x$point_weight[i] < 0.5)
    {
      x$angle[i] <- x$angle[i - 1]
      x$step[i] <- x$step[i - 1]
    }
  }
  x
})

kvar <- c("step","angle")

movevar <- data.frame(hmm_prep[kvar])

kmean <- kmeans(na.omit(movevar),3)

aggregate(na.omit(movevar),
          by = list(kmean$cluster),
          FUN = mean)

aggregate(na.omit(movevar),
          by = list(kmean$cluster),
          FUN = sd)

step_par <-
  c(45, 445, 713, 
    50, 93, 107)

angle_par <- 
  c(0.005, 0.5, 0.005,
    0.68, 1.16, 0.25)

rm(kmean, kvar, movevar)

hmm_prep <- do.call(rbind, ls)

system.time(
  fulmar_state_mod <- 
    momentuHMM::fitHMM(
      data = hmm_prep,
      nbStates = 3,
      estAngleMean = list(angle = T),
      dist = list(step = "gamma", angle = "vm"),
      Par0 = list(step = step_par, angle = angle_par),
      weights = point_weight,
      formula = ~ 1)
)

rm(step_par, angle_par, ls)

fulmar_state <- fulmar_trip

fulmar_state$state <- 
  as.character(momentuHMM::viterbi(fulmar_state_mod))

fulmar_state$angle <- hmm_prep$angle

fulmar_state$step <- hmm_prep$step

fulmar_state$tod <- format(
  fulmar_state$date_time, format="%H%M%S")

fulmar_state$hod <- format(
  fulmar_state$date_time, format="%H")

ls1 <- split(fulmar_state,
             fulmar_state$trip_id)

ls1 <- ls1[sapply(ls1,
                  function(x) dim(x)[1] > 0)]

ls2 <- mclapply(ls1,
                EnvAppend,
                mc.cores = numCores)

fulmar_state <- do.call(rbind, ls2)

rm(ls1, ls2, hmm_data, hmm_prep, step)


# vector of IDs of male birds
male <-
  c("FP40378",
    "FP40481",
    "FH15622",
    "FH78319",
    "FP40378",
    "FP40481",
    "FH78363",
    "FV46512",
    "NOFU_FAI0400",
    "NOFU_FAI0404",
    "NOFU_ORK0898",
    "NOFU_FAI0325",
    "NOFU_FAI0326",
    "NOFU_FAI0324",
    "NOFU_FAI0327",
    "NOFU_ORK0733",
    "NOFU_FAI0323",
    "NOFU_BOB0022",
    "NOFU_BOB0027",
    "NOFU_ORK0614",
    "NOFU_ORK0661",
    "NOFU_ORK0182",
    "NOFU_ORK0204",
    "NOFU_ORK0177",
    "NOFU_SCI0116"
  )

# vector of IDs of female birds
female <- 
  c("FH78373",
    "FH78320",
    "FB25141",
    "FH15572",
    "FH78370",
    "FH78373",
    "FP64381",
    "FH15572",
    "FH78374",
    "FH78375",
    "FH78377",
    "FP14811",
    "NOFU_FAI0398",
    "NOFU_FAI0399",
    "NOFU_FAI0330",
    "NOFU_ORK0732",
    "NOFU_BOB0024",
    "NOFU_FAI0272",
    "NOFU_FAI0275",
    "NOFU_FAI0278",
    "NOFU_WIN0074",
    "NOFU_ORK0617",
    "NOFU_ORK0639",
    "NOFU_ORK0664",
    "NOFU_ORK0181",
    "NOFU_ORK0183",
    "NOFU_FAI0146",
    "NOFU_FAI0144",
    "NOFU_ORK0899",
    "NOFU_BOB0031",
    "NOFU_ORK0185",
    "NOFU_SCI0062",
    "NOFU_ORK0176")

# Append sex to fulmar df
fulmar_state$Sex <- rep("U", nrow(fulmar_state))
fulmar_state$Sex[which(fulmar_state$ID %in% male)] <- "M"
fulmar_state$Sex[which(fulmar_state$ID %in% female)] <- "F"

# blow away sex ID vectors
rm(male, female)

save(fulmar_state,
     file = paste("data/", Sys.Date(), "_fulmar_state_df.RData", sep = ""))


