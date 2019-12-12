library(shiny)
library(shinythemes)
library(EValue)
library(bsplus)

options(shiny.sanitize.errors = FALSE)
source("setup.R")

# TODO: ADD warning about risk difference scale

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
      # this can go anywhere, just proving modal for assumptions
      # TODO: add other modals??
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
      
      # if they choose OR there's only one option
      conditionalPanel(
        condition = "input.outcomeType_B == 'OR'",
        p("Bound only available for odds ratio when bias is due to control selection in a case-control study. 
          For other types of selection bias, choose risk ratio or difference."),
        checkboxGroupInput(
          "control_sel_assump",
          label = HTML('<label class="control-label">Necessary assumptions</label>
        <a href="#" data-toggle="modal" data-target="#modal_control_sel_assump">
             <i class="fa fa-info-circle"></i>
             </a>'),
          choices = c(
            "No unmeasured confounding \\({(Y^a \\amalg A \\mid C)}\\)" = "no_confound",
            "Selection of cases independent of exposure \\({(S \\amalg A \\mid Y = 1, C )}\\)" = "case_indep",
            "Selection of controls independent of exposure, conditional on some possibly unmeasured factor(s) 
            \\(U\\) 
            \\({(S \\amalg A \\mid  Y = 0, U, C)}\\)" = "control_indep"
          ),
          selected = c("case_indep", "control_indep", "no_confound")
        ),
        HTML("<label class='control-label'>Estimated/hypothesized values for parameters</label>"),
        
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
      
      # if not OR, what is the target of inference
      conditionalPanel(
        condition = "input.outcomeType_B != 'OR'",
        radioButtons(
          "pop_group",
          label = HTML('<label class="control-label">Target population</label>
        <a href="#" data-toggle="modal" data-target="#modal_pop_group">
             <i class="fa fa-info-circle"></i>
             </a>'),
          choices = c(
            "Entire population" = "whole_pop",
            "Selected population" = "sel_pop"
          ),
          selected = "whole_pop"
        )
      ),
      
      conditionalPanel( # "Inference only in selected population"
        condition = "input.outcomeType_B != 'OR' && input.pop_group == 'sel_pop'",
        
        checkboxGroupInput( # TODO: needs modal?
          "sel_pop_assump",
          label = HTML('<label class="control-label">Necessary assumptions</label>
        <a href="#" data-toggle="modal" data-target="#modal_sel_pop_assump">
             <i class="fa fa-info-circle"></i>
             </a>'),
          choices = c(
            "No unmeasured confounding \\({(Y_a \\amalg A \\mid C)}\\)" = "no_confound",
            "All selection bias is captured by possibly unmeasured factor(s) \\(U\\) \\({(Y_a \\amalg A \\mid S = 1, U, C)}\\)" = 
            "U_indep"
          ),
          selected = c("U_indep", "no_confound")
        ),
        
        HTML("<label class='control-label'>Estimated/hypothesized values for parameters</label>"),
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
        ),
        # extra inputs for risk difference
        conditionalPanel(
          condition = "input.pop_group == 'sel_pop' && input.outcomeType_B == 'RD'",
          splitLayout(
            numericInput("pY_S1_A1_sel", HTML("Risk in selected exposed:<br>$P(Y = 1 \\mid A = 1, S = 1)$"),
                         value = NA, min = 0, max = 1, step = 0.1
            ),
            numericInput("pY_S1_A0_sel", HTML("Risk in selected unexposed:<br>$P(Y = 1 \\mid A = 0, S = 1)$"),
                         value = NA, min = 0, max = 1, step = 0.1
            )
          ))
      ), # end selected population option
      
      conditionalPanel(
        condition = "input.outcomeType_B != 'OR' && input.pop_group != 'sel_pop'",
        # TODO: does this need modal?
        checkboxGroupInput(
          "whole_pop_assump",
          label = HTML('<label class="control-label">Necessary assumptions</label>
        <a href="#" data-toggle="modal" data-target="#modal_whole_pop_assump">
             <i class="fa fa-info-circle"></i>
             </a>'),
          choices = c(
            "No unmeasured confounding \\({(Y_a \\amalg A \\mid C)}\\)" = "no_confound",
            "Selection is only related to outcome via unmeasured factor(s) \\(U\\) \\({(Y \\amalg S \\mid A, U, C)}\\)" = 
              "U_indep"
          ),
          selected = c("U_indep", "no_confound")
        ),
        
        checkboxGroupInput(
          "assump_B",
          label = HTML('<label class="control-label">Additional assumptions</label>
        <a href="#" data-toggle="modal" data-target="#modal_assumptions_B">
             <i class="fa fa-info-circle"></i>
             </a>'),
          choices = c(
            "Unmeasured factor a defining characteristic of selection" = "S_eq_U",
            "Selection always associated with increased risk of outcome in both exposure groups" = "risk_inc",
            "Selection always associated with decreased risk of outcome in both exposure groups" = "risk_dec"
          )
        ),
      
      HTML("<label class='control-label'>Estimated/hypothesized values for parameters</label>"),
      
      # extra inputs for risk difference
      conditionalPanel(
        condition = "input.pop_group != 'sel_pop' && input.outcomeType_B == 'RD'",
        splitLayout(
          numericInput("pY_S1_A1_whole", HTML("Risk in selected exposed:<br>$P(Y = 1 \\mid A = 1, S = 1)$"),
                       value = NA, min = 0, max = 1, step = 0.1
          ),
          numericInput("pY_S1_A0_whole", HTML("Risk in selected unexposed:<br>$P(Y = 1 \\mid A = 0, S = 1)$"),
                       value = NA, min = 0, max = 1, step = 0.1
          )
        )),
      
      # conditional panels for all of the various possible assumptions
      conditionalPanel(
        condition = "input.outcomeType_B != 'OR' && input.pop_group != 'sel_pop' && !output.risk_inc && !output.risk_dec && !output.S_eq_U",
        splitLayout(
          RRUY0(1),
          RRUY1(1),
          RRSU0(1),
          RRSU1(1)
        )
      ),
      conditionalPanel(
        condition = "input.outcomeType_B != 'OR' && input.pop_group != 'sel_pop' && output.S_eq_U && !output.risk_inc && !output.risk_dec",
        splitLayout(
          RRUY0(2),
          RRUY1(2)
        )
      ),
      conditionalPanel(
        condition = "input.outcomeType_B != 'OR' && input.pop_group != 'sel_pop' && output.S_eq_U && output.risk_inc && !output.risk_dec",
        RRUY1(3)
      ),
      conditionalPanel(
        condition = "input.outcomeType_B != 'OR' && input.pop_group != 'sel_pop' && output.S_eq_U && output.risk_dec && !output.risk_inc",
        RRUY0(3)
      ),
      conditionalPanel(
        condition = "input.outcomeType_B != 'OR' && input.pop_group != 'sel_pop' && output.risk_inc && !output.S_eq_U && !output.risk_dec",
        splitLayout(
          RRUY1(4),
          RRSU1(4)
        )
      ),
      conditionalPanel(
        condition = "input.outcomeType_B != 'OR' && input.pop_group != 'sel_pop' && output.risk_dec && !output.S_eq_U && !output.risk_inc",
        splitLayout(
          RRUY0(4),
          RRSU0(4)
        )
      )),
      
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
      
      selectInput(
        "outcomeType_S",
        label = "Outcome type",
        choices = c(
          "Risk ratio" = "RR",
          "Odds ratio" = "OR"
        )
      ), 
      
      # if they choose OR there's only one option
      conditionalPanel(
        condition = "input.outcomeType_S == 'OR'",
        p("Bound only available for odds ratio when bias is due to control selection in a case-control study. 
          For other types of selection bias, choose risk ratio or difference."),
        checkboxGroupInput(
          "control_sel_assump_S",
          label = HTML('<label class="control-label">Necessary assumptions</label>
        <a href="#" data-toggle="modal" data-target="#modal_control_sel_assump">
             <i class="fa fa-info-circle"></i>
             </a>'),
          choices = c(
            "No unmeasured confounding \\({(Y^a \\amalg A \\mid C)}\\)" = "no_confound",
            "Selection of cases independent of exposure \\({(S \\amalg A \\mid Y = 1, C )}\\)" = "case_indep",
            "Selection of controls independent of exposure, conditional on some possibly unmeasured factor(s) 
            \\(U\\) 
            \\({(S \\amalg A \\mid  Y = 0, U, C)}\\)" = "control_indep"
          ),
          selected = c("case_indep", "control_indep", "no_confound")
        )
      ), # end conditional panel for control selection
      
      # if not OR, what is the target of inference
      conditionalPanel(
        condition = "input.outcomeType_S != 'OR'",
        radioButtons(
          "pop_group_S",
          label = HTML('<label class="control-label">Target population</label>
        <a href="#" data-toggle="modal" data-target="#modal_pop_group">
             <i class="fa fa-info-circle"></i>
             </a>'),
          choices = c(
            "Entire population" = "whole_pop",
            "Selected population" = "sel_pop"
          ),
          selected = "whole_pop"
        )
      ),
      
      conditionalPanel( # "Inference only in selected population"
        condition = "input.outcomeType_S != 'OR' && input.pop_group_S == 'sel_pop'",
        
        checkboxGroupInput( # TODO: needs modal?
          "sel_pop_assump_S",
          label = HTML('<label class="control-label">Necessary assumptions</label>
        <a href="#" data-toggle="modal" data-target="#modal_sel_pop_assump">
             <i class="fa fa-info-circle"></i>
             </a>'),
          choices = c(
            "No unmeasured confounding \\({(Y_a \\amalg A \\mid C)}\\)" = "no_confound",
            "All selection bias is captured by possibly unmeasured factor(s) \\(U\\) \\({(Y_a \\amalg A \\mid S = 1, U, C)}\\)" = 
              "U_indep"
          ),
          selected = c("U_indep", "no_confound")
        )
       ), # end selected population option
      
      conditionalPanel(
        condition = "input.outcomeType_S != 'OR' && input.pop_group_S != 'sel_pop'",
        # TODO: does this need modal?
        checkboxGroupInput(
          "whole_pop_assump_S",
          label = HTML('<label class="control-label">Necessary assumptions</label>
        <a href="#" data-toggle="modal" data-target="#modal_whole_pop_assump">
             <i class="fa fa-info-circle"></i>
             </a>'),
          choices = c(
            "No unmeasured confounding \\({(Y_a \\amalg A \\mid C)}\\)" = "no_confound",
            "Selection is only related to outcome via unmeasured factor(s) \\(U\\) \\({(Y \\amalg S \\mid A, U, C)}\\)" = 
              "U_indep"
          ),
          selected = c("U_indep", "no_confound")
        ),
        
        checkboxGroupInput(
          "assump_S",
          label = HTML('<label class="control-label">Additional assumptions</label>
        <a href="#" data-toggle="modal" data-target="#modal_assumptions_B">
             <i class="fa fa-info-circle"></i>
             </a>'),
          choices = c(
            "Unmeasured factor a defining characteristic of selection" = "S_eq_U",
            "Selection always associated with increased risk of outcome in both exposure groups" = "risk_inc",
            "Selection always associated with decreased risk of outcome in both exposure groups" = "risk_dec"
          )
        )
      ),
      
      numericInput("est_S", "Point estimate", NA, min = 0),
      splitLayout(
        numericInput("lo_S", "CI lower limit (optional)", NA, min = 0),
        numericInput("hi_S", "CI upper limit (optional)", NA, min = 0)
      ),
      numericInput("true_S", "True causal effect to which to shift estimate", 1, min = 0),
      
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
  output$S_eq_U <- reactive({
    "S_eq_U" %in% input$assump_B
  })
  output$risk_inc <- reactive({
    "risk_inc" %in% input$assump_B
  })
  output$risk_dec <- reactive({
    "risk_dec" %in% input$assump_B
  })
  
  outputOptions(output, "S_eq_U", suspendWhenHidden = FALSE)
  outputOptions(output, "risk_inc", suspendWhenHidden = FALSE)
  outputOptions(output, "risk_dec", suspendWhenHidden = FALSE)
  
  # compute bound computation
  bounds <- reactive({

    S_eq_U <- "S_eq_U" %in% input$assump_B
    risk_inc <- "risk_inc" %in% input$assump_B
    risk_dec <- "risk_dec" %in% input$assump_B
    
    if (input$outcomeType_B == "OR") {
      return(BF(input$RRUA1, input$RRS0U) * BF(input$RRUA0, input$RRS1U))
    }
    if (input$outcomeType_B == "RR") {
      if (input$pop_group == "sel_pop") {
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
      if (input$pop_group == "sel_pop") {
        bf <- BF(input$RRAUS1, input$RRUYS1)
        return(max(c(
          input$pY_S1_A0_sel * bf,
          input$pY_S1_A1_sel * (1 - 1 / bf)
        )))
      }
      if (!S_eq_U & !risk_inc & !risk_dec) {
        bf0 <- BF(input$RRUY01, input$RRSU01)
        bf1 <- BF(input$RRUY11, input$RRSU11)
        return(bf1 - input$pY_S1_A1_whole / bf1 + input$pY_S1_A0_whole * bf0)
      }
      if (S_eq_U & !risk_inc & !risk_dec) {
        return(input$RRUY12 -
                 input$pY_S1_A1_whole / input$RRUY12 +
                 input$pY_S1_A0_whole * input$RRUY02)
      }
      if (S_eq_U & risk_inc) {
        return(input$RRUY13 - input$pY_S1_A1_whole / input$RRUY13)
      }
      # CHECK THESE
      if (S_eq_U & risk_dec) {
        return(input$RRUY03 - input$pY_S1_A0_whole / input$RRUY03)
      }
      if (risk_inc) {
        bf <- BF(input$RRUY14, input$RRSU14)
        return(bf - input$pY_S1_A1_whole / bf)
      }
      if (risk_dec) {
        bf <- BF(input$RRUY04, input$RRSU04)
        return(bf - input$pY_S1_A0_whole / bf)
      }
    }
  })
  
  # compute bound result
  output$result.text_B <- renderUI({
    validate(
      need(
        (!all(c("risk_inc", "risk_dec") %in% input$assump_B) & 
           all(c("U_indep", "no_confound") %in% input$whole_pop_assump)) &
          all(c("U_indep", "no_confound") %in% input$sel_pop_assump) & 
          all(c("case_indep", "control_indep", "no_confound") %in% input$control_sel_assump),
        "You have missing or incompatible assumptions"
      ),
      need(
        (!anyNA(c(input$RRUA1, input$RRS0U, input$RRUA0, input$RRS1U)) & input$outcomeType_B == "OR") |
          (!anyNA(c(input$RRAUS1, input$RRUYS1)) & input$pop_group == "sel_pop") |
          (!anyNA(c(input$RRUY01, input$RRSU01, input$RRUY11, input$RRSU11)) & !"risk_inc" %in% input$assump_B & 
             !"risk_dec" %in% input$assump_B & !"S_eq_U" %in% input$assump_B) |
          (!anyNA(c(input$RRUY02, input$RRUY12)) & "S_eq_U" %in% input$assump_B & !"risk_dec" %in% input$assump_B & 
             !"risk_dec" %in% input$assump_B) |
          (!is.na(input$RRUY13) & "S_eq_U" %in% input$assump_B & "risk_dec" %in% input$assump_B & 
             !"risk_dec" %in% input$assump_B) | 
          (!is.na(input$RRUY03) & "S_eq_U" %in% input$assump_B & !"risk_dec" %in% input$assump_B & 
             "risk_dec" %in% input$assump_B) |
          (!anyNA(c(input$RRUY14, input$RRSU14)) & !"S_eq_U" %in% input$assump_B & 
             "risk_dec" %in% input$assump_B & !"risk_dec" %in% input$assump_B) |
          (!anyNA(c(input$RRUY04, input$RRSU04)) & !"S_eq_U" %in% input$assump_B & 
             !"risk_dec" %in% input$assump_B & "risk_dec" %in% input$assump_B),
        "Please enter values for the parameters above"
      )
    )
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
      need(!is.na(input$est_S), "Please enter a point estimate"),
      need(
        (!all(c("risk_inc", "risk_dec") %in% input$assump_S) & 
           all(c("U_indep", "no_confound") %in% input$whole_pop_assump_S)) &
          all(c("U_indep", "no_confound") %in% input$sel_pop_assump_S) & 
          all(c("case_indep", "control_indep", "no_confound") %in% input$control_sel_assump_S),
        "You have missing or incompatible assumptions"
      ),
      need(!is.na(input$true_S), "Please enter a value to shift the estimate to")
    )
    
    sval_args <- list(
      est = input$est_S,
      lo = input$lo_S,
      hi = input$hi_S,
      true = input$true_S,
      sel_pop = (input$pop_group_S == 'sel_pop' & 
        input$outcomeType_S == "RR"),
      S_eq_U = ("S_eq_U" %in% input$assump_S & 
                  input$outcomeType_S == "RR"),
      risk_inc = ("risk_inc" %in% input$assump_S & 
                    input$outcomeType_S == "RR"),
      risk_dec = ("risk_dec" %in% input$assump_S & 
                    input$outcomeType_S == "RR")
    )
    
    svals <- round(do.call(svalues, sval_args)[2, ], 2)
    
    return(svals)
  })
  
  # message that explains what parameters the selection bias e-value refers to
  mess_S <- reactive({
    
    sel_pop <- input$pop_group_S == 'sel_pop'
    S_eq_U <- "S_eq_U" %in% input$assump_S
    risk_inc <- "risk_inc" %in% input$assump_S
    risk_dec <- "risk_dec" %in% input$assump_S
    cont_sel <- input$outcomeType_S == "OR"
    
    m1 <- "This value refers to the minimum value of each of"
    m3 <- "that would explain away your point estimate."
    
    if (sel_pop) {
      m2 <- "$\\text{RR}_{UY|S=1}$ and $\\text{RR}_{AU|S=1}$"
      return(paste(m1, m2, m3))
    }
    if (!S_eq_U & !risk_inc & !risk_dec & !cont_sel) {
      m2 <- "$\\text{RR}_{UY \\mid (A = 0)}$, $\\text{RR}_{UY \\mid (A = 1)}$, $\\text{RR}_{SU \\mid (A = 0)}$, $\\text{RR}_{SU \\mid (A = 1)}$"
      return(paste(m1, m2, m3))
    } # TODO: confirm notation
    if (!S_eq_U & !risk_inc & !risk_dec) {
      m2 <- "$\\text{RR}_{U_0Y}$, $\\text{RR}_{U_1Y}$, $\\text{RR}_{S_0U}$, $\\text{RR}_{S_1U}$"
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