#' @importFrom dplyr mutate if_else case_when rename select
#' @export
read_behav_neighborhood <- function(file) {
  fields <- c("face", #face stimulus
              "pgo", #probability of outcome(-1 or +1 point), see next column
              "outcome", #1, 0 or -1. get point, no point, or lose point for SIS.
              "block_loop.thisN", #block number of experiment, 0-3
              #"block_loop.thisIndex", #block number in terms sample drawn from the set of blocks... basically denotes order of conditions, not super useful
              "Cavb_trials.thisN", #within-block trial number, 0-39, repeated 4 times
              "obtained", #combo of pgo and outcome. What they actually scored on the trial
              "key_resp_10.keys", #go or no-go. space=go, none=no-go
              "key_resp_10.rt", #rt of keyresp
              "participant", #participant Number
              "participant.initials" #participant initials
  )

  dat <- read.csv(file, stringsAsFactors = FALSE)
  dat <- dat[,fields] #retain only columns of interest
  dat <- dat %>% mutate_if(is.character, list(~if_else(. == "", NA_character_, .))) %>% #convert "" to NA
    mutate(id=if_else(is.na(participant), NA_character_, paste(participant, participant.initials, sep="_"))) %>%
    mutate(condition=case_when(
      is.na(pgo) ~ NA_character_,
      pgo == 0.3 && outcome == 1 ~ "no_t_w",
      pgo == 0.3 && outcome == -1 ~ "no_t_a",
      pgo == 0.7 && outcome == 1 ~ "go_t_w",
      pgo == 0.7 && outcome == -1 ~ "go_t_a",
      TRUE ~ NA_character_
    )) %>%
    rename(
      block = block_loop.thisN,
      block_trial = Cavb_trials.thisN,
      key_pressed = key_resp_10.keys,
      rt = key_resp_10.rt
    ) %>%
    mutate(block_trial=block_trial + 1, block = block + 1, trial=1:n()) %>% #use 1-based indexing
    select(-participant, -participant.initials)

  #Some files contain a final all-missing row. Remove this and, more generally, remove any row that is all NA
  na_rows <- apply(dat, 1, function(r) { all(is.na(r)) })
  dat <- dat[!na_rows,] #drop any all-na rows

  return(dat)
}

read_behav_vending <- function(file_ins, file_pav, file_pit) {
  fields <- list(
    ins = c("repA.thisN",              # Response A - Button A for reward
            "repB.thisN",              # Response B - Button B for reward
            "vending_machine.started", # Trial start time
            "vending_machine.stopped"  # Trial stop time
    ),
    pav = c("ï..testCS",               # Correct image
            "correctAns",              # Correct response
            "CS",                      # Shown image
            "blocks.thisTrialN",       # Block
            "blocks.thisN",            # Trial
            "vend.started",            # Trial start time
            "vend.stopped"             # Trial end time
    ),
    pit = c("Condition",               # Image
            "trials.thisN",            # Trial Number
            "pressL",                  # Number of times pressed left
            "pressR",                  # Number of times pressed right
            "CStest.started",          # Trial start time
            "CStest.stopped"           # Trial end time
    )
  )

  dat <- list(
    ins = read.csv(file_ins, stringsAsFactors = FALSE),
    pav = read.csv(file_pav, stringsAsFactors = FALSE),
    pit = read.csv(file_pit, stringsAsFactors = FALSE)
  )

  mutator = list(
    ins = function(df) {
      df <- df[,fields$ins] %>%
        mutate_if(is.character, list(~if_else(. == "" | . == "None", NA_character_, .))) %>%
        rename(
          responseA = repA.thisN,
          responseB = repB.thisN,
          start = vending_machine.started,
          stop = vending_machine.stopped
        ) %>%
        filter(!is.na(responseA) | !is.na(responseB)) %>%
        mutate(trial = row_number()) %>%
        mutate_at(c("responseA"), ~if_else(. == "0", "A", NA_character_)) %>%
        mutate_at(c("responseB"), ~if_else(. == "0", "B", NA_character_)) %>%
        unite("response", responseA:responseB, na.rm = TRUE) %>%
        mutate_at(c("start", "stop"), list(~ as.numeric(.))) %>%
        rowwise() %>%
        mutate(time = sum(stop, -start, na.rm = FALSE)) %>%
        select(-one_of("start", "stop"))
      return(df)
    },
    pav = function(df) {
      df = df[,fields$pav] %>%
        mutate_if(is.character, list(~if_else(. == "" | . == "None", NA_character_, .))) %>%
        rename(
          correct_image = ï..testCS,
          correct_response = correctAns,
          shown_image = CS,
          block = blocks.thisTrialN,
          trial = blocks.thisN,
          start = vend.started,
          stop = vend.stopped
        )%>%
        filter(!is.na(shown_image)) %>%
        mutate_at(c("start", "stop"), list(~ as.numeric(.))) %>%
        rowwise() %>%
        mutate(time = sum(stop, -start, na.rm = FALSE)) %>%
        select(-one_of("start", "stop")) %>%
        mutate(block = block + 1, trial = trial + 1)
      return(df)
    },
    pit = function(df) {
      df = df[,fields$pit]
      return(df)
    }
  )

  dat <- Map(function(block_data, block_name) { block_data = mutator[[block_name]](block_data) }, dat, names(dat))
  return(dat)
}

#' general wrapper for reading behavioral files into the package
#' @export
read_behav <- function(file, parser=NULL, ...) {
  if (is.null(parser)) { stop("Need to pass parser function to read_behav") }
  stopifnot(file.exists(file))
  behav <- parser(file, ...)
  class(behav) <- c(class(behav), "ep.behav") #tag with ep.behav class

  #other general post-processing for behavior data
  return(behav)
}
