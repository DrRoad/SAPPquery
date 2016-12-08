## Shorthand for linking to ontologies at identifiers.org.

library(data.table)

o <- function(s, format=c("markdown", "url"), verbose=FALSE, do.link=names(s), dont.link=NULL, prefix="http://identifiers.org") {
  # s : One of the following,
  #   string of the form "ontology/identifier"
  #   data.frame with ontologies as column names and identifiers as elements
  #   data.table with list-valued columns, i.e. vector-valued elements.
  #     If vector-valued elements have names, these will be used as identifiers 
  #     while the vector elements will be used for display text.
  # format : if markdown, return "[identifier](http://identifiers.org/ontology/identifier)"
  #   if url, return "http://identifiers.org/ontology/identifier"
  # verbose : if markdown, return "[ontology/identifier](http://identifiers.org/ontology/identifier)"
  # do.link : vector specifying which columns to linkify (default all)
  # dont.link : vector specifying columns not to linkify
  # prefix : 
  
  # Escape colon to work around rmarkdown bug
  # https://github.com/rstudio/rmarkdown/issues/521
  e <- function(s) gsub(":", "\\:", s, fixed=TRUE)
  
  if (inherits(s, "data.frame")) {
    
    if (is.null(names(do.link)))
      names(do.link) <- do.link
    named <- !(names(do.link) %in% c(NA, ""))
    names(do.link)[!named] <- do.link[!named]
    do.link <- do.link[!do.link %in% dont.link]
    
    # Linkify each column, and also the column names
    d <- data.frame(matrix(nrow=nrow(s), ncol=0))
    for (i in 1:ncol(s)) {
      nam <- names(s)[i]
      if (nam %in% do.link) {  # Linkify
        
        # Get the ontology's "identifiers.org name" and "display name"
        if (names(do.link)[do.link==nam] == nam) {  # Column name directly identifies an ontology
          onto <- nam
          onto.link <- o(onto, prefix=prefix)
        } else {  # Map column name to ontology via names(do.link)
          onto <- names(do.link)[do.link == nam]
          onto.link <- sprintf("[%s](%s)", e(nam), ou(onto))
        }
        
        # Linkify the terms, i.e. the values in the current column
        terms <- s[[i]]
        if (is.list(terms)) {  # list-valued column in data.table
          vec.link <- function(term.vec) {
            if (is.null(names(term.vec)))
              identifiers <- term.vec
            else
              identifiers <- names(term.vec)
            return(paste(collapse=" &bull; ", sprintf("[%s](%s/%s/%s)", e(term.vec), prefix, onto, identifiers)))
          }
          d[[onto.link]] <- sapply(terms, vec.link)
        } else {
          d[[onto.link]] <- o(paste(onto, terms, sep="/"), prefix=prefix)
        }
        
      } else {  # Just copy
        d[[nam]] <- s[[i]]
      }
    }
    return(d)
  }
  
  s <- as.character(s)
  format <- match.arg(format)
  L <- strsplit(s, split="/")
  out <- NULL
  for (Li in L) {
    # parse ontology/identifier
    if (length(Li)==0 || is.na(Li[[1]])) {
      ontology <- NA
      identifier <- NA
    } else if (length(Li)==1) {
      ontology <- Li[[1]]
      identifier <- NA
    } else {
      ontology <- Li[[1]]
      identifier <- Li[[2]]
    }
    # make url
    if (is.na(ontology))
      url <- NA_character_
    else if (is.na(identifier))
      url <- sprintf("%s/%s", prefix, ontology)
    else
      url <- sprintf("%s/%s/%s", prefix, ontology, identifier)
    # convert to markdown if requested
    if (format=="url")
      item <- url
    else {
      if (is.na(url))
        item <- ""
      else if (is.na(identifier))
        item <- sprintf("[%s](%s)", ontology, url)
      else if (verbose)
        item <- sprintf("[%s/%s](%s)", ontology, e(identifier), url)
      else
        item <- sprintf("[%s](%s)", e(identifier), url)
    }
    # grow output
    out <- c(out, item)
  }
  return(out)
}

if (!require(testthat)) {
  message("Install testthat to run tests in ontology_links.R")
} else {
  # Ontology url
  ou <- function(s, format="url", prefix="http://identifiers.org") o(s, format, prefix=prefix)
  
  # Helper function to save typing
  df <- function(...) data.frame(..., check.names=FALSE, stringsAsFactors=FALSE)
  
  test_that("ou() produces correct urls", {
    expect_that(ou("test/that"), equals("http://identifiers.org/test/that"))
    expect_that(ou("test"),      equals("http://identifiers.org/test"))
  })
  
  test_that("o() produces correct markdown links", {
    expect_equal(o("test/that"), "[that](http://identifiers.org/test/that)")
    expect_equal(o("test"), "[test](http://identifiers.org/test)")
    expect_equal(o("test/that", verbose=TRUE), "[test/that](http://identifiers.org/test/that)")
  })
  
  test_that("o() handles data frame input", {
    expect_equal(o(df(onto="term")),
                 df(`[onto](http://identifiers.org/onto)`="[term](http://identifiers.org/onto/term)"))
  })
  
  test_that("o() handles data table input with list-valued columns", {
    expect_equal(o(data.table(onto=list(c("term1", "term2"), "term3"))),
                 df(`[onto](http://identifiers.org/onto)`=c("[term1](http://identifiers.org/onto/term1) &bull; [term2](http://identifiers.org/onto/term2)", "[term3](http://identifiers.org/onto/term3)")))
  })
  
  test_that("o() can be told which columns to process", {
    expect_equal(o(data.table(onto="term", data=123), do.link="onto"),
                 df(`[onto](http://identifiers.org/onto)`="[term](http://identifiers.org/onto/term)", data=123))
    expect_equal(o(data.table(onto="term", data=123), dont.link="data"),
                 df(`[onto](http://identifiers.org/onto)`="[term](http://identifiers.org/onto/term)", data=123))
  })
  test_that("names(do.link) can remap variable names to other ontologies", {
    expect_equal(o(data.table(onto="term", data=123), do.link=c(ontology="onto")),
                 df(`[onto](http://identifiers.org/ontology)`="[term](http://identifiers.org/ontology/term)", data=123))
    expect_equal(o(data.table(onto="term", data=123), do.link=c(ontology="onto", "data")),
                 df(`[onto](http://identifiers.org/ontology)`="[term](http://identifiers.org/ontology/term)", 
                    `[data](http://identifiers.org/data)`="[123](http://identifiers.org/data/123)"))
  })
  test_that("names(do.link) can remap multiple variable names to the same other ontology", {
    expect_equal(o(data.table(onto="term", data=123), do.link=c(ontology="onto", ontology="data")),
                 df(`[onto](http://identifiers.org/ontology)`="[term](http://identifiers.org/ontology/term)", 
                    `[data](http://identifiers.org/ontology)`="[123](http://identifiers.org/ontology/123)"))
    expect_equal(o(data.table(name="test", onto="term", data=123), do.link=c(ontology="onto", ontology="data")),
                 df(name="test",
                    `[onto](http://identifiers.org/ontology)`="[term](http://identifiers.org/ontology/term)", 
                    `[data](http://identifiers.org/ontology)`="[123](http://identifiers.org/ontology/123)"))
  })
  test_that("Names in list-valued columns can be used as labels", {
    expect_equal(o(data.table(onto=list(c(term1="DISPLAY1", term2="term2"), "term3"))),
                 df(`[onto](http://identifiers.org/onto)`=c("[DISPLAY1](http://identifiers.org/onto/term1) &bull; [term2](http://identifiers.org/onto/term2)", "[term3](http://identifiers.org/onto/term3)")))
  })
  test_that("We escape URI scheme prefixes to work around an rmarkdown bug", {
    # https://github.com/rstudio/rmarkdown/issues/521
    
    expect_equal(o("test/im:that"), "[im\\:that](http://identifiers.org/test/im:that)")
    expect_equal(o("test/im:that", verbose=TRUE), "[test/im\\:that](http://identifiers.org/test/im:that)")
    
    expect_equal(o(df(onto="im:term")),
                 df(`[onto](http://identifiers.org/onto)`="[im\\:term](http://identifiers.org/onto/im:term)"))
    
    expect_equal(o(data.table(onto=list(c("term1", "im:term2"), "term3"))),
                 df(`[onto](http://identifiers.org/onto)`=c("[term1](http://identifiers.org/onto/term1) &bull; [im\\:term2](http://identifiers.org/onto/im:term2)", "[term3](http://identifiers.org/onto/term3)")))
    
  })
  
  test_that("List-valued columns don't cause 'arguments imply differing number of rows' error", {
    d <- data.frame(a=1:2)
    d$b <- list(3:4, 5:7)
    expect_equal(d, o(d, do.link=NA))
  })
  
  test_that("We can change prefix to link elsewhere than http://identifiers.org", {
    warning("Skipping test_that: We can change prefix to link elsewhere than http://identifiers.org")
    return(TRUE)
    d <- data.frame(a=1:2)
    d$b <- list(3:4, 5:7)
    expect_equal(d, o(d, prefix="PREFIX"))
    # Nearly works.
    # Headers still link to identifiers.org, and the scalar column does not get customized.
    # Also, we might want to customize the prefix per column, so might allow a named vector as for do.link and dont.link.
  })
  
  
  ### Demo (generate notebook in RStudio with Control-Shift-K):
  # kable(o(iris))
}