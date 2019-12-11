library(shiny)
library(shinythemes)
library(EValue)
library(bsplus)

options(shiny.sanitize.errors = FALSE)
source("setup.R")

#### UI component --------------------------------------------------
ui <- navbarPage(
  title = "Simple sensitivity analysis for selection bias",
  id = "navbar",
  theme = shinytheme("yeti"),
  
  #### intro tab -----------------------------------------------------
  tabPanel(
    title = "Introduction",
    
    # prevent page from greying out after 10 seconds
    tags$script(src = "keep_alive.js"),
    tags$head(
      # workaround to get math in there (can't get inline mathjax to work)
      tags$link(
        rel = "stylesheet",
        href = "https://cdn.jsdelivr.net/npm/katex@0.10.1/dist/katex.min.css",
        integrity = "sha384-dbVIfZGuN1Yq7/1Ocstc1lUEm+AT+/rCkibIcC/OmWo5f0EA48Vf8CytHzGrSwbQ",
        crossorigin = "anonymous"
      ),
      HTML('<script defer src="https://cdn.jsdelivr.net/npm/katex@0.10.1/dist/katex.min.js" 
           integrity="sha384-2BKqo+exmr9su6dir+qCw08N2ZKRucY4PrGQPPWU1A7FtlCGjmEGFqXCv5nyM5Ij" 
           crossorigin="anonymous"></script>'),
      HTML('<script defer src="https://cdn.jsdelivr.net/npm/katex@0.10.1/dist/contrib/auto-render.min.js" 
           integrity="sha384-kWPLUVMOks5AQFrykwIup5lo0m3iMkkHrD0uJ4H5cjeGihAutqP0yW0J6dpFiVkI" 
           crossorigin="anonymous" onload="renderMathInElement(document.body);"></script>'),
      HTML('<script> document.addEventListener("DOMContentLoaded", function(){
        renderMathInElement(document.body, { delimiters: [{left: "$", right: "$", display: false}]
        }); })</script>'),
      # Google analytics 
      HTML('<script async src="https://www.googletagmanager.com/gtag/js?id=UA-112795291-2"></script>
      <script> 
        window.dataLayer = window.dataLayer || [];
        function gtag(){dataLayer.push(arguments);}
        gtag("js", new Date());
        gtag("config", "UA-112795291-2");
     </script>'),
      # my CSS additions
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    
    # content to open on
    mainPanel(
      # info about selection bias
      includeMarkdown("content/intro.md"),
      width = 8
    ),
    
    # info about citation
    sidebarPanel(
      includeMarkdown("content/intro_side.md"),
      width = 4
    )
  ), # end opening panel
  
  #### compute bound tab ------------------------------------------------
  tabPanel(
    title = "Compute bound",
    mainPanel(
      bs_modal(
        id = "modal_assumptions_B",
        title = "Additional assumptions",
        body = includeMarkdown("content/assumptions.md"),
        size = "medium"
      ),
      selectInput(
        "outcomeType_B",
        label = "Outcome type",
        choices = c(
          "Risk ratio" = "RR",
          "Risk difference" = "RD",
          "Odds ratio" = "OR"
        )
      ),
      conditionalPanel(
        condition = "input.outcomeType_B == 'OR'",
        p("Bound only available for odds ratio when bias is due to control selection in a case-control study. For other types of selection bias, choose risk ratio or difference."),
        checkboxGroupInput(
          "control_sel_assump",
          label = HTML('<label class="control-label">Additional assumptions</label>
        <a href="#" data-toggle="modal" data-target="#modal_control_sel_assump">
             <i class="fa fa-info-circle"></i>
             </a>'),
          choices = c(
            "Selection of cases (but not controls) independent of exposure" = "non_indep"
          )
        ),
      ),
      conditionalPanel(
        condition = "input.outcomeType_B == 'RR' | input.outcomeType_B == 'RD'",
        checkboxGroupInput(
          "assump_B",
          label = HTML('<label class="control-label">Additional assumptions</label>
        <a href="#" data-toggle="modal" data-target="#modal_assumptions_B">
             <i class="fa fa-info-circle"></i>
             </a>'),
          choices = c(
            "Unmeasured factor a defining characteristic of selection" = "S_eq_U",
            "Selection always associated with increased risk of outcome in both exposure groups" = "risk_inc",
            "Selection always associated with decreased risk of outcome in both exposure groups" = "risk_dec",
            "Inference only in selected population" = "sel_pop"
          )
        )),
      # extra inputs for risk difference
      conditionalPanel(
        condition = "input.outcomeType_B == 'RD'",
        splitLayout(
          numericInput("pY_S1_A1", HTML("Risk in selected exposed:<br>$P(Y = 1 \\mid A = 1, S = 1)$"),
                       value = NA, min = 0, max = 1, step = 0.1
          ),
          numericInput("pY_S1_A0", HTML("Risk in selected unexposed:<br>$P(Y = 1 \\mid A = 0, S = 1)$"),
                       value = NA, min = 0, max = 1, step = 0.1
          )
        )),
      
      # conditional panels for all of the various possible assumptions
      HTML("<label class='control-label'>Estimated/hypothesized values for parameters</label>"),
      conditionalPanel(
        condition = "output.sel_pop && !output.S_eq_U && !output.risk_inc && !output.risk_dec",
        splitLayout(
          numericInput("RRUYS1", "$\\text{RR}_{UY|S=1}$", NA) %>%
            shinyInput_label_embed(
              shiny_iconlink() %>%
                bs_embed_popover(
                  title = NA,
                  content = "Maximum outcome risk ratio comparing two levels of U among the selected",
                  placement = "top"
                )
            ),
          numericInput("RRAUS1", "$\\text{RR}_{AU|S=1}$", NA) %>%
            shinyInput_label_embed(
              shiny_iconlink() %>%
                bs_embed_popover(
                  title = NA,
                  content = "Maximum factor by which exposure increases some value of U among the selected",
                  placement = "top"
                )
            )
        )
      ),
      conditionalPanel( # TODO: the other ones need to disappear when this happens
        condition = "output.non_indep",
        splitLayout(
          # TODO: Fix content in popovers
          numericInput("RRUA1", "$\\text{RR}_{UA_1}$", NA) %>%
            shinyInput_label_embed(
              shiny_iconlink() %>%
                bs_embed_popover(
                  title = NA,
                  content = "Maximum outcome risk ratio comparing two levels of U among the selected",
                  placement = "top"
                )
            ),
          numericInput("RRS1U", "$\\text{RR}_{S_1U}$", NA) %>%
            shinyInput_label_embed(
              shiny_iconlink() %>%
                bs_embed_popover(
                  title = NA,
                  content = "Maximum factor by which exposure increases some value of U among the selected",
                  placement = "top"
                )
            ),
          numericInput("RRUA0", "$\\text{RR}_{UA_0}$", NA) %>%
            shinyInput_label_embed(
              shiny_iconlink() %>%
                bs_embed_popover(
                  title = NA,
                  content = "Maximum factor by which exposure increases some value of U among the selected",
                  placement = "top"
                )
            ),
          numericInput("RRS0U", "$\\text{RR}_{S_0U}$", NA) %>%
            shinyInput_label_embed(
              shiny_iconlink() %>%
                bs_embed_popover(
                  title = NA,
                  content = "Maximum factor by which exposure increases some value of U among the selected",
                  placement = "top"
                )
            )
        )
      ), # end conditional panel for control selection
      conditionalPanel(
        condition = "!output.sel_pop && !output.risk_inc && !output.risk_dec && !output.S_eq_U",
        splitLayout(
          RRUY0(1),
          RRUY1(1),
          RRSU0(1),
          RRSU1(1)
        )
      ),
      conditionalPanel(
        condition = "output.S_eq_U && !output.risk_inc && !output.risk_dec && !output.sel_pop",
        splitLayout(
          RRUY0(2),
          RRUY1(2)
        )
      ),
      conditionalPanel(
        condition = "output.S_eq_U && output.risk_inc && !output.sel_pop && !output.risk_dec",
        RRUY1(3)
      ),
      conditionalPanel(
        condition = "output.S_eq_U && output.risk_dec && !output.sel_pop && !output.risk_inc",
        RRUY0(3)
      ),
      conditionalPanel(
        condition = "output.risk_inc && !output.S_eq_U && !output.sel_pop && !output.risk_dec",
        splitLayout(
          RRUY1(4),
          RRSU1(4)
        )
      ),
      conditionalPanel(
        condition = "output.risk_dec && !output.S_eq_U && !output.sel_pop && !output.risk_inc",
        splitLayout(
          RRUY0(4),
          RRSU0(4)
        )
      ),
      
      # display results
      wellPanel(span(uiOutput("result.text_B"))),
      width = 6
    ), # ends computation input/output panel
    
    # panel for explaining how to calculate a bound 
    sidebarPanel(
      includeMarkdown("content/bound_side.md"),
      width = 6
    ) # end explanation sidebar
  ), # end direct computation of bounds tab
  
  #### selection e-value tab ------------------------------------------
  tabPanel(
    title = "E-values for selection bias",
    mainPanel(
      bs_modal(
        id = "modal_control_sel_assump",
        title = "Additional assumptions",
        body = includeMarkdown("content/control_sel.md"), #TODO: write
        size = "medium"
      ),
      bs_modal(
        id = "modal_assumptions_S",
        title = "Additional assumptions",
        body = includeMarkdown("content/assumptions.md"),
        size = "medium"
      ),
      bs_modal(
        id = "modal_parameters_S",
        title = "Parameter definitions",
        body = includeMarkdown("content/parameters.md"), #TODO: ADD control selection params
        size = "medium"
      ),
      splitLayout(
        selectInput("outcomeType_S",
                    label = "Outcome type",
                    choices = c(
                      "Risk ratio" = "RR",
                      "Odds ratio" = "OR",
                      "Hazard ratio" = "HR"
                    )
        ),
        numericInput("est_S", "Point estimate", NA, min = 0)
      ),
      conditionalPanel(
        condition = "input.outcomeType_S != 'RR' ",
        checkboxInput("rare", "Outcome prevalence <15%", TRUE)
      ),
      splitLayout(
        numericInput("lo_S", "CI lower limit (optional)", NA, min = 0),
        numericInput("hi_S", "CI upper limit (optional)", NA, min = 0)
      ),
      numericInput("true_S", "True causal effect to which to shift estimate", 1, min = 0),
      
      checkboxGroupInput(
        "assump_S",
        label = HTML('<label class="control-label">Additional assumptions</label>
        <a href="#" data-toggle="modal" data-target="#modal_assumptions_S">
             <i class="fa fa-info-circle"></i>
             </a>'),
        choices = c(
          "Unmeasured factor a defining characteristic of selection" = "S_eq_U",
          "Selection always associated with increased risk of outcome in both exposure groups" = "risk_inc",
          "Selection always associated with decreased risk of outcome in both exposure groups" = "risk_dec",
          "Inference only in selected population" = "sel_pop"
        )
      ),
      
      # display results
      wellPanel(span(textOutput(("result.text_S")))),
      uiOutput("message.text_S"),
      width = 6
    ), # ends panel for input/output of e-value
    
    # panel for info
    sidebarPanel(
      includeMarkdown("content/evalue_side.md"),
      width = 6
    ) # end e-value explanation sidebar
  ), # end tab for computing e-values
  
  #### additional resources tab ----------------------------------------
  tabPanel(
    "More resources",
    mainPanel(
      includeMarkdown("content/resources.md"),
      
      # hide the text for keeping page from greying out (so doesn't create extra blank space)
      textOutput("keep_alive")
    )
  ),
  use_bs_popover()
)

#### server component ------------------------------------------------
server <- function(input, output, session) {
  # what will be blank text to prevent page grey-out
  output$keep_alive <- renderText({
    req(input$alive_count)
    input$alive_count
  })
  
  #### compute bound tab -----------------------------------------------
  # need these to choose conditionally which parameter boxes to show
  output$non_indep <- reactive({
    "non_indep" %in% input$control_sel_assump
  })
  output$sel_pop <- reactive({
    "sel_pop" %in% input$assump_B
  })
  output$S_eq_U <- reactive({
    "S_eq_U" %in% input$assump_B
  })
  output$risk_inc <- reactive({
    "risk_inc" %in% input$assump_B
  })
  output$risk_dec <- reactive({
    "risk_dec" %in% input$assump_B
  })
  
  outputOptions(output, "non_indep", suspendWhenHidden = FALSE)
  outputOptions(output, "sel_pop", suspendWhenHidden = FALSE)
  outputOptions(output, "S_eq_U", suspendWhenHidden = FALSE)
  outputOptions(output, "risk_inc", suspendWhenHidden = FALSE)
  outputOptions(output, "risk_dec", suspendWhenHidden = FALSE)
  
  # compute bound computation
  bounds <- reactive({
    non_indep <- "non_indep" %in% input$non_indep
    sel_pop <- "sel_pop" %in% input$assump_B
    S_eq_U <- "S_eq_U" %in% input$assump_B
    risk_inc <- "risk_inc" %in% input$assump_B
    risk_dec <- "risk_dec" %in% input$assump_B
    
    validate(
      need(
        !all(c("risk_inc", "risk_dec") %in% input$assump_B) &
          !all(c("sel_pop", "risk_inc") %in% input$assump_B) &
          !all(c("sel_pop", "risk_dec") %in% input$assump_B) &
          !all(c("sel_pop", "S_eq_U") %in% input$assump_B),
        "You have unnecessary and/or incompatible assumptions"
      )
    )
    
    validate(
      need(
        !anyNA(c(input$RRUA1, input$RRS0U, input$RRUA0, input$RRS1U)) |
          !anyNA(c(input$RRAUS1, input$RRUYS1)) |
          !anyNA(c(input$RRUY01, input$RRSU01, input$RRUY11, input$RRSU11)) |
          !anyNA(c(input$RRUY02, input$RRUY12)) |
          !is.na(input$RRUY13) | !is.na(input$RRUY03) |
          !anyNA(c(input$RRUY14, input$RRSU14)) |
          !anyNA(c(input$RRUY04, input$RRSU04)),
        "Please enter values for the parameters above"
      )
    )
    if (input$outcomeType_B == "OR") {
      return(BF(input$RRUA1, input$RRS0U) * BF(input$RRUA0, input$RRS1U))
    }
    if (input$outcomeType_B == "RR") {
      if (sel_pop) {
        return(BF(input$RRAUS1, input$RRUYS1))
      }
      if (!S_eq_U & !risk_inc & !risk_dec) {
        return(BF(input$RRUY01, input$RRSU01) * BF(input$RRUY11, input$RRSU11))
      }
      if (S_eq_U & !risk_inc & !risk_dec) {
        return(input$RRUY02 * input$RRUY12)
      }
      if (S_eq_U & risk_inc) {
        return(input$RRUY13)
      }
      if (S_eq_U & risk_dec) {
        return(input$RRUY03)
      }
      if (risk_inc) {
        return(BF(input$RRUY14, input$RRSU14))
      }
      if (risk_dec) {
        return(BF(input$RRUY04, input$RRSU04))
      }
    }
    if (input$outcomeType_B == "RD") {
      if (sel_pop) {
        bf <- BF(input$RRAUS1, input$RRUYS1)
        return(max(c(
          input$pY_S1_A0 * bf,
          input$pY_S1_A1 * (1 - 1 / bf)
        )))
      }
      if (!S_eq_U & !risk_inc & !risk_dec) {
        bf0 <- BF(input$RRUY01, input$RRSU01)
        bf1 <- BF(input$RRUY11, input$RRSU11)
        return(bf1 - input$pY_S1_A1 / bf1 + input$pY_S1_A0 * bf0)
      }
      if (S_eq_U & !risk_inc & !risk_dec) {
        return(input$RRUY12 -
                 input$pY_S1_A1 / input$RRUY12 +
                 input$pY_S1_A0 * input$RRUY02)
      }
      if (S_eq_U & risk_inc) {
        return(input$RRUY13 - input$pY_S1_A1 / input$RRUY13)
      }
      # CHECK THESE
      if (S_eq_U & risk_dec) {
        return(input$RRUY03 - input$pY_S1_A0 / input$RRUY03)
      }
      if (risk_inc) {
        bf <- BF(input$RRUY14, input$RRSU14)
        return(bf - input$pY_S1_A1 / bf)
      }
      if (risk_dec) {
        bf <- BF(input$RRUY04, input$RRSU04)
        return(bf - input$pY_S1_A0 / bf)
      }
    }
  })
  
  # compute bound result
  output$result.text_B <- renderUI({
    sepr <- ifelse(input$outcomeType_B %in% c("RR", "OR"), "/", "-")
    b <- paste0(
      "$", input$outcomeType_B, "_{true} \\geq ",
      input$outcomeType_B, "_{obs} ", sepr,
      round(bounds(), 2), "$"
    )
    tagList(
      paste0("Selection bias bound: ", b),
      # make sure math is printed
      tags$script('renderMathInElement(document.getElementById("result.text_B"), 
                  {delimiters: [{left: "$", right: "$", display: false}]});')
    )
  })
  
  #### selection e-value tab -------------------------------------------
  svals <- reactive({
    validate(
      need(!is.na(input$est_S), "Please enter a point estimate")
    )
    validate(
      need(!is.na(input$true_S), "Please enter a value to shift the estimate to")
    )
    
    sval_args <- list(
      est = input$est_S,
      lo = input$lo_S,
      hi = input$hi_S,
      true = input$true_S,
      sel_pop = ("sel_pop" %in% input$assump_S),
      S_eq_U = ("S_eq_U" %in% input$assump_S),
      risk_inc = ("risk_inc" %in% input$assump_S),
      risk_dec = ("risk_dec" %in% input$assump_S)
    )
    
    if (input$outcomeType_S != "RR") sval_args$rare <- input$rare
    
    func <- switch(input$outcomeType_S,
                   RR = svalues.RR,
                   OR = svalues.OR,
                   HR = svalues.HR
    )
    
    svals <- round(do.call(func, sval_args)[2, ], 2)
    
    return(svals)
  })
  
  # message that explains what parameters the selection bias e-value refers to
  mess_S <- reactive({
    
    sel_pop <- "sel_pop" %in% input$assump_S
    S_eq_U <- "S_eq_U" %in% input$assump_S
    risk_inc <- "risk_inc" %in% input$assump_S
    risk_dec <- "risk_dec" %in% input$assump_S
    
    m1 <- "This value refers to the minimum value of each of"
    m3 <- "that would explain away your point estimate."
    
    if (sel_pop) {
      m2 <- "$\\text{RR}_{UY|S=1}$ and $\\text{RR}_{AU|S=1}$"
      return(paste(m1, m2, m3))
    }
    if (!S_eq_U & !risk_inc & !risk_dec) {
      m2 <- "$\\text{RR}_{UY \\mid (A = 0)}$, $\\text{RR}_{UY \\mid (A = 1)}$, $\\text{RR}_{SU \\mid (A = 0)}$, $\\text{RR}_{SU \\mid (A = 1)}$"
      return(paste(m1, m2, m3))
    }
    if (S_eq_U & !risk_inc & !risk_dec) {
      m2 <- "$\\text{RR}_{UY \\mid (A = 0)}$ and $\\text{RR}_{UY \\mid (A = 1)}$"
      return(paste(m1, m2, m3))
    }
    if (S_eq_U & risk_inc) {
      m2 <- "$\\text{RR}_{UY \\mid (A = 1)}$"
      return(paste(m1, m2, m3))
    }
    if (S_eq_U & risk_dec) {
      m2 <- "$\\text{RR}_{UY \\mid (A = 0)}$"
      return(paste(m1, m2, m3))
    }
    if (risk_inc) {
      m2 <- "$\\text{RR}_{UY \\mid (A = 1)}$ and $\\text{RR}_{SU \\mid (A = 1)}$"
      return(paste(m1, m2, m3))
    }
    if (risk_dec) {
      m2 <- "$\\text{RR}_{UY \\mid (A = 0)}$ and $\\text{RR}_{SU \\mid (A = 0)}$"
      return(paste(m1, m2, m3))
    }
  })
  
  # print the selection bias e-value
  output$result.text_S <- renderText({
    s <- svals()
    # if there is input for the CI (either lower or upper)
    if (!is.na(svals()[2]) | !is.na(svals()[3])) {
      sval.CI <- min(svals(), na.rm = TRUE)
      result.string_S <- paste("Selection bias E-value for point estimate: ", svals()[1],
                               ", and for confidence interval: ", sval.CI,
                               sep = ""
      )
      # if user only gave point estimate
    } else {
      result.string_S <- paste("Selection bias E-value for point estimate: ",
                               s[1],
                               sep = ""
      )
    }
    return(result.string_S)
  })
  
  # print message about what parameters e-value refers to
  output$message.text_S <- renderUI({
    m <- mess_S()
    
    if (!is.na(input$true_S) & input$true_S != 1) m <- paste(m, nonnull.mess, sep = " ")
    tagList(
      helpText(m),
      # make sure math is printed
      tags$script('renderMathInElement(document.getElementById("message.text_S"),
                  {delimiters: [{left: "$", right: "$", display: false}]});')
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)