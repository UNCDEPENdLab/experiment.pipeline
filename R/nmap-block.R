# NeruoMap Block
#
# S4 Object Definition for NeruoMap Block.
# Blocks are used to define callbacks to function
# to define sequential/parallel events in function calls
#
# Example:
# func <- function() {
#   f1 <- function(l) {print("f1");return(c(1,2,3));}
#   f2 <- function(list) {print(list);return(list());}
#
#   foo <- create.block("A", f1)
#   bar <- create.block("B", f2)
#
#   foobar <- connect.blocks(foo, bar)
#   foo <- foobar$input.block
#   bar <- foobar$output.block
#
#   run.block(foo)
# }

#' Checks validity of block object
#'
#' Checks that the input block is a block object or null.
#' Checks that the output block is a block object or null.
#'
#' @param object Block object
#' @return true boolean or list of errors
block.validity <- function(object) {
  errors <- character()

  for(input.block in object@input.blocks) {
    if(!(class(input.block) == "Block") && !(is.null(object@input.block))) {
      msg <- paste("Invalid input block")
      errors <- c(errors, msg)
    }
  }

  for(output.block in object@output.blocks) {
    if(!(class(output.block) == "Block") && !(is.null(object@output.block))) {
      msg <- paste("Invalid output block")
      errors <- c(errors, msg)
    }
  }

  if(length(errors) == 0) TRUE else errors
}

#' Class Block
#'
#' @param input.blocks {character} Previously attached blocks
#' @param output.blocks {character} Block to move to end of event
setClass("Block",
         representation(block.name = "character",
                        input.blocks = "ANY",
                        output.blocks = "ANY",
                        callback = "ANY"),
         prototype(block.name = NA_character_,
                   input.blocks = list(),
                   output.blocks = list(),
                   callback = NULL),
         validity = block.validity
        )

#' Get name of block object
#'
#' @param block Block object
#' @return name of block
get.block.name <- function(block) {
  return(block@block.name)
}

#' Set name of block object
#'
#' @param block Block object
#' @param block.name name of block
set.block.name <- function(block, block.name) {
  block@block.name <- block.name
  return(block)
}

#' Get input of block object
#'
#' @param block Block object
#' @return input of block
get.input.blocks <- function(block) {
  return(block@input.block)
}

#' Set input of block object
#'
#' @param block Block object
#' @param input.blocks inputs of block
set.input.block <- function(block, input.blocks) {
  block@input.blocks <- input.blocks
  return(block)
}

#' Get output of block object
#'
#' @param block Block object
#' @return output of block
get.output.blocks <- function(block) {
  return(block@output.blocks)
}

#' Set output of block object
#'
#' @export
#'
#' @param block Block object
#' @param output.blocks outputs of block
set.output.blocks <- function(block, output.blocks) {
  block@output.blocks <- output.blocks
  return(block)
}

#' Get callback function of block object
#'
#' @param block Block object
#' @return callback function of block
get.callback <- function(block) {
  return(block@callback)
}

#' Set callback function of block object
#'
#' @param block Block object
#' @param callback callback function of block
set.callback <- function(block, callback) {
  block@callback <- callback
  return(block)
}

#' Create a block object
#'
#' Block objects allow for easy maniputlation of dataflow.
#' Each block object is given an input, output, and function.
#' The input represents where the block's inputs come from (NULL if none).
#' The output represents where the block's outputs go to (NULL if none).
#' The callback function is the actual manipulation of data.
#'
#' @param block.name Name of block
#' @param callback callback function (Feed-forward pass-through if none)
#' @return block object
#'
#' @export
create.block <- function(block.name, callback = function(l) {return(l)}) {
  block <- methods::new("Block",
                         block.name = block.name,
                         input.blocks = NULL,
                         output.blocks = NULL,
                         callback = callback
                        )

  return(block)
}

#' Connects blocks
#'
#' Connects two blocks together as a directed graph style
#'
#' @param input.block Input block
#' @param output.block Output block
#' @return list
connect.blocks <- function(input.block, output.block) {
  input.block@output.blocks <- append(input.block@output.blocks, output.block)
  output.block@input.blocks <- append(output.block@input.blocks, input.block)

  res <- list("input.block" = input.block, "output.block" = output.block)

  return(res)
}

#' Runs a block and follows the output until completion
#'
#' Recursive function to follow blocks until no more outputs.
#'
#' @param block Block to run
#' @param input Input of block (this should only be used during recursion)
#' @return 0 if succesful, 1 if error
#'
#' @export
run.block <- function(block, input = list()) {
  log.info(paste("Running Block", block@block.name, sep = " "))

  tryCatch({
    res <- block@callback(input)
    if(length(block@output.blocks) == 0) {
      return(0)
    } else {
      s <- parallel::parSapplyLB(NeuroMap$SYSTEM$cluster, block@output.blocks, run.block, res)
    }
  }, error = function(err) {
    log.error(err)
    return(1)
  })
}
