
#' Update running TODO.md for display on github using \code{todor}
#'
#' @description Pull all relevant todoR fields and render a TODO.md file that is viewable on github.
#'
#' @importFrom todor todor
#' @importFrom rprojroot find_package_root_file
#'
#' @export
github_todo <- function(...){
  todo <- todor(output = "markdown")
  items <-str_split(todo, "\n\n") %>% unlist() %>% data.frame(str = .) %>%
    filter(str != "") %>% mutate(id = rep(seq(1, nrow(.)/2), each = 2),
                                 type = rep(c("location", "item"), nrow(.)/2)) %>%
    pivot_wider(names_from = type, values_from = str) %>% mutate(item_type = str_extract(str_extract(item, "- \\[ [[:upper:]]+ ]"), "[[:upper:]]+"),
                                                                 string = sub("- \\[ [[:upper:]]+ ]  ", "", item)) %>% select(-item, -id)


  md_text <- c()
  for(i in unique(items$item_type)){
    it <- items %>% filter(item_type == i)
    md_text <- c(md_text, paste0("# ", i))
    for(j in 1:nrow(it)){
      j <- 1
      this.item <- it[j,]
      md_checkbox <- paste0("- [ ] ", this.item$string, ". (", sub("\\*\\* ", "",sub("\\*\\*", "", this.item$location)) ,")")
      md_text <- c(md_text, md_checkbox)
    }
  }

  writeLines(md_text, file.path(rprojroot::find_package_root_file(), "TODO.md"))
}
