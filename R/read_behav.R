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

  return(list(default=dat))
}

#' @title Import Quail, Morris, Balleine Vending Machine task data
#' @param files A names character vector of three CSV files. Elements should be named 'pav', 'ins', 'pit'
#' @importFrom dplyr mutate_all select filter mutate rowwise
#' @importFrom tidyr unite
#' @importFrom utils read.table
#' @importFrom checkmate assert_atomic_vector assert_character assert_file_exists
#' @examples
#' \dontrun{
#'   b <- read_behav_vending(c(
#'      pav="070_Pavlovian_2020_Feb_18_1421.csv",
#'      ins="070_Instrumental_2020_Feb_18_1412.csv",
#'      pit="070_transfer_2020_Feb_18_1438.csv")
#'   )
#' }
#' @export
read_behav_vending <- function(files=NULL) {
  # Validates Parameters
  assert_atomic_vector(files)
  assert_character(files)
  sapply(files, assert_file_exists)
  assert_set_equal(names(files), c("pav", "ins", "pit"))

  stopifnot(length(files) == 3)

  # Sets files based on index
  file_ins = files["ins"]  # Instrumental
  file_pav = files["pav"]  # Index 2: Pavlovian
  file_pit = files["pit"]  # Index 3: PIT

  # Fields to keep from original dataframe
  fields <- list(
    ins = c("repA.thisN",              # Response A - Button A for reward
            "repB.thisN",              # Response B - Button B for reward
            "vending_machine.started", # Trial start time
            "vending_machine.stopped"  # Trial stop time
    ),
    pav = c("testCS",               # Correct image
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

  # Import original dataframes from CSVs
  dat <- list(
    ins = read.csv(file_ins, stringsAsFactors = FALSE),
    pav = read.csv(file_pav, stringsAsFactors = FALSE),
    pit = read.csv(file_pit, stringsAsFactors = FALSE)
  )

  # List of curried upper-level functions to pass each phase data through
  mutator = list(
    ins = function(df) {
      assert_set_equal(unique(df$phase), c("VIns", "")) #validate that the file is correct (i.e., right phase)
      assert_set_equal(unique(df$expName), c("Instrumental", "")) #validate that the file is correct (i.e., right phase)

      df <- df[,fields$ins] %>%
        # Set NA to missing values
        mutate_if(is.character,
                  list(~if_else(. == "" | . == "None", NA_character_, .))) %>%

        # Renames columns
        rename(
          responseA = repA.thisN,
          responseB = repB.thisN,
          start = vending_machine.started,
          stop = vending_machine.stopped
        ) %>%

        # Removes rows with missing responses
        filter(!is.na(responseA) | !is.na(responseB)) %>%

        # Add trial number
        mutate(trial = row_number()) %>%

        # Combine response columns
        mutate_at(c("responseA"), ~if_else(. == "0", "A", NA_character_)) %>%
        mutate_at(c("responseB"), ~if_else(. == "0", "B", NA_character_)) %>%
        unite("response", responseA:responseB, na.rm = TRUE) %>%

        # Combine times
        mutate_at(c("start", "stop"), list(~ as.numeric(.))) %>%
        rowwise() %>%
        mutate(time = sum(stop, -start, na.rm = FALSE)) %>%
        select(-one_of("start", "stop")) %>% ungroup()
      return(df)
    },
    pav = function(df) {
      assert_set_equal(unique(df$phase), c("Vpav", "")) #validate that the file is correct (i.e., right phase)
      assert_set_equal(unique(df$expName), c("Pavlovian", "")) #validate that the file is correct (i.e., right phase)

      df = df[,fields$pav] %>%
        # Set NA to missing values
        mutate_if(is.character, list(~if_else(. == "" | . == "None", NA_character_, .))) %>%

        # Renames columns
        rename(
          correct_image = testCS,
          correct_response = correctAns,
          shown_image = CS,
          block = blocks.thisTrialN,
          trial = blocks.thisN,
          start = vend.started,
          stop = vend.stopped #Appears to be "None" (NA) in general
        )%>%

        # Remove if no image
        filter(!is.na(shown_image)) %>%

        # Combine times
        mutate_at(c("start", "stop"), list(~ as.numeric(.))) %>%
        rowwise() %>%
        mutate(time = sum(stop, -start, na.rm = FALSE)) %>%
        select(-one_of("start", "stop")) %>%

        # Change trial and block to 1-based indexing
        mutate(block = block + 1, trial = trial + 1) %>% ungroup()
      return(df)
    },
    pit = function(df) {
      assert_set_equal(unique(df$phase), c("Vpit", "")) #validate that the file is correct (i.e., right phase)
      assert_set_equal(unique(df$expName), c("transfer", "")) #validate that the file is correct (i.e., right phase)

      df = df[,fields$pit] %>%
        #Set NA to missing values
        mutate_if(is.character, list(~if_else(. == "" | . == "None", NA_character_, .))) %>%

        # Renames columns
        dplyr::rename(
          image = Condition,
          trial = trials.thisN,
          start = CStest.started,
          stop = CStest.stopped
        ) %>%

        # Remove missing trials or vending image
        filter(!is.na(trial)) %>%
        filter(image != 'vend.png') %>%

        # Combine times
        mutate_at(c("start", "stop"), list(~ as.numeric(.))) %>%
        rowwise() %>%
        mutate(time = sum(stop, -start, na.rm = FALSE)) %>%
        select(-one_of("start", "stop")) %>%

        # Make trial 1-based index
        mutate(trial = trial / 2 + 1) %>% ungroup()
      return(df)
    }
  )


  # Maps each block to correct function
  dat <- Map(function(block_data, block_name) {
    block_data = tryCatch({
      mutator[[block_name]](block_data)
    }, error = function(e) {
      stop(paste("Error with", block_name, "block mutator.",
                 "Possibly wrong file passed."))
    }
    )}, dat, names(dat))
  return(dat)
}

# setwd("/Users/mnh5174/Downloads/s3_ALL_data_prototype")
# b <- read_behav_vending(c(
#   pav="070_Pavlovian_2020_Feb_18_1421.csv",
#   ins="070_Instrumental_2020_Feb_18_1412.csv",
#   pit="070_transfer_2020_Feb_18_1438.csv")
# )

#' general wrapper for reading behavioral files into the package
#' @importFrom checkmate assert_file_exists
#' @export
read_behav <- function(file, parser=NULL, ...) {
  if (is.null(parser)) { stop("Need to pass parser function to read_behav") }
  assert_file_exists(file)

  behav <- parser(file, ...)
  class(behav) <- c(class(behav), "ep.behav") #tag with ep.behav class

  #other general post-processing for behavior data
  return(behav)
}
