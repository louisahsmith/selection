# message to display if non-null true value
nonnull.mess <- 'Note: You are calculating a "non-null" selection bias E-value, i.e., a value for the minimum
                amount of selection bias needed to move the estimate and confidence interval
                to your specified true value rather than to the null value.'

# various functions
BF <- function(RR1, RR2) {
  (RR1 * RR2) / (RR1 * RR2 - 1)
}

RRUY0 <- function(num) {
  numericInput(paste0("RRUY0", num), "$\\text{RR}_{UY \\mid (A = 0)}$", NA) %>%
    shinyInput_label_embed(
      shiny_iconlink() %>%
        bs_embed_popover(
          title = NA,
          content = "Maximum outcome risk ratio comparing two levels of U among the unexposed",
          placement = "top"
        )
    )
}

RRUY1 <- function(num) {
  numericInput(paste0("RRUY1", num), "$\\text{RR}_{UY \\mid (A = 1)}$", NA) %>%
    shinyInput_label_embed(
      shiny_iconlink() %>%
        bs_embed_popover(
          title = NA,
          content = "Maximum outcome risk ratio comparing two levels of U among the exposed",
          placement = "top"
        )
    )
}

RRSU0 <- function(num) {
  numericInput(paste0("RRSU0", num), "$\\text{RR}_{SU \\mid (A = 0)}$", NA) %>%
    shinyInput_label_embed(
      shiny_iconlink() %>%
        bs_embed_popover(
          title = NA,
          content = "Maximum factor by which non-selection increases some value of U among the unexposed",
          placement = "top"
        )
    )
}

RRSU1 <- function(num) {
  numericInput(paste0("RRSU1", num), "$\\text{RR}_{SU \\mid (A = 1)}$", NA) %>%
    shinyInput_label_embed(
      shiny_iconlink() %>%
        bs_embed_popover(
          title = NA,
          content = "Maximum factor by which selection increases some value of U among the exposed",
          placement = "top"
        )
    )
}