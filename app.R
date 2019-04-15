library(shiny)
library(shinythemes)
library(EValue)
# todo: make "this value refers to" text only show up if something is inputted
# make sure right-side panels are the same width and why are they in a frame???
options(shiny.sanitize.errors = FALSE)

# message to display if non-null true value
nonnull.mess <- 'Note: You are calculating a "non-null" selection bias value, i.e., a value for the minimum
                amount of selection bias needed to move the estimate and confidence interval
                to your specified true value rather than to the null value.'


ui <- navbarPage(
  title = "Bounding bias due to selection",
  id = "navbar",
  theme = shinytheme("yeti"),

  tabPanel(
    title = "Selection Bias",

    # prevent page from greying out after 10 seconds
    tags$script(src = "keep_alive.js"),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),

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
           crossorigin="anonymous"></script>'),
      HTML('<script> document.addEventListener("DOMContentLoaded", function(){
        renderMathInElement(document.body, { delimiters: [{left: "$", right: "$", display: false}]
        }); })</script>')
    ),

    # tab to open on
    mainPanel(
      # Google Analytics
      tags$head(HTML('<script async src="https://www.googletagmanager.com/gtag/js?id=UA-112795291-2"></script>
      <script> 
        window.dataLayer = window.dataLayer || [];
        function gtag(){dataLayer.push(arguments);}
        gtag("js", new Date());
        gtag("config", "UA-112795291-2");
     </script>')),

      # info about selection bias
      wellPanel(HTML(paste("<b>What is selection bias?</b>",
        "Here is information about selection bias.",
        "Here is more information about selection bias.",
        sep = "<br/><br/>"
      ))),
      width = 8
    ),
    
    # info about citation
    sidebarPanel(
      HTML(paste("<b>This website is based on the following article:</b>",
        "Smith LH & VanderWeele TJ. (2019). Bounding bias due to selection. 
        <i>Epidemiology</i>, forthcoming. <a href='https://arxiv.org/abs/1810.13402'>(Pre-print available).</a>",
        "<b>Bug reports</b>",
        "Submit any bug reports to: <i>louisa_h_smith [AT] g.harvard [DOT] edu</i> or open
        an issue on <a href='https://github.com/louisahsmith/selection/issues'>Github</a>.",
        sep = "<br/><br/>"
      ))
    )
  ), # end opening panel
  
  # tab for directly computing bounds
  tabPanel(
    title = "Compute bound",
    mainPanel(
      selectInput(
        "outcomeType_B",
        label = "Outcome type",
        choices = c(
          "Risk / rate ratio" = "RR",
          "Risk / rate difference" = "RD"
        )
      ),
      checkboxGroupInput(
        "assump_B", 
        label = "Additional assumptions (see linked article for details):",
        choices = c(
          "Inference only in selected population" = "sel_pop",
          "Unmeasured factor a defining characteristic of selection" = "S_eq_U",
          "Selection associated with increased risk of outcome" = "risk_inc",
          "Selection associated with decreased risk of outcome" = "risk_dec"
        )
      ),
      # conditionalPanel(
      #     condition = "input.outcomeType_S != 'RR' ",
      #     checkboxInput("rare", "Outcome prevalence <15%", TRUE)
      # ),
      conditionalPanel(
        condition = "input.outcomeType_B == 'RD'",
        numericInput("pY_S1_A1", "Risk in selected exposed: P(Y = 1 | A = 1, S = 1)", 
                     value = NA, min = 0, max = 1, step = 0.1)
      ),
      conditionalPanel(
        condition = "input.outcomeType_B == 'RD'",
        numericInput("pY_S1_A0", "Risk in selected unexposed: P(Y = 1 | A = 0, S = 1)", 
                     value = NA, min = 0, max = 1, step = 0.1)
      ),
      
      # display results
      wellPanel(span(textOutput(("result.text_B")))),
      uiOutput("message.text_B"),
      width = 6
    ), # ends computation input/output panel
    
    # panel for explaining how to calculate a bound
    sidebarPanel(
      wellPanel(HTML(paste("<b>Computing an E-value for selection bias</b>",
        "Like the E-value for unmeasured confounding, the selection bias E-value describes 
        the minimum strength of association between several (possibly unmeasured) factors that would be 
        sufficient to have created enough selection bias to explain away an observed exposure-outcome association.
        The parameters that the E-value refers to depends on what assumptions an investigator is willing to make,
        and are printed with the results. See the cited article for exact definitions and for more details.",
        "<b>Please use the following citation:</b>",
        "Smith LH & VanderWeele TJ. (2019). Bounding bias due to selection. 
        <i>Epidemiology</i>, forthcoming. <a href='https://arxiv.org/abs/1810.13402'>(Pre-print available).</a>",
        sep = "<br/><br/>"
      ))),
      width = 6
    ) # end explanation sidebar
  ), # end direct computation of bounds tab
  
  # tab for computing "e-value"
  tabPanel(
    title = "E-values for selection bias",
    mainPanel(
      selectInput("outcomeType_S",
        label = "Outcome type",
        choices = c(
          "Risk / rate ratio" = "RR",
          "Odds ratio" = "OR",
          "Hazard ratio" = "HR"
        )
      ),
      conditionalPanel(
        condition = "input.outcomeType_S != 'RR' ",
        checkboxInput("rare", "Outcome prevalence <15%", TRUE)
      ),
      numericInput("est_S", "Point estimate", NA, min = 0),
      numericInput("lo_S", "Confidence interval lower limit", NA, min = 0),
      numericInput("hi_S", "Confidence interval upper limit", NA, min = 0),
      numericInput("true_S", "True causal effect to which to shift estimate", 1, min = 0),

      checkboxGroupInput(
        "assump_S", 
        label = "Additional assumptions (see linked article for details):",
        choices = c(
          "Inference only in selected population" = "sel_pop",
          "Unmeasured factor a defining characteristic of selection" = "S_eq_U",
          "Selection associated with increased risk of outcome" = "risk_inc",
          "Selection associated with decreased risk of outcome" = "risk_dec"
        )
      ),
      
      # display results
      wellPanel(span(textOutput(("result.text_S")))),
      uiOutput("message.text_S"),
      
      # warnings if computing non-null E-value
      # note: because the condition is in Javascript, have to use period instead of dollar sign to
      #  access arguments, so CANNOT have period in the variable names (e.g., "true.RR" doesn't work!)
      conditionalPanel(condition = "input.true_S != 1", helpText(nonnull.mess)),
      width = 6
    ), # ends panel for input/output of e-value
    
    # panel for info
    sidebarPanel(
      wellPanel(HTML(paste("<b>Computing an E-value for selection bias</b>",
        "Like the E-value for unmeasured confounding, the selection bias E-value describes 
        the minimum strength of association between several (possibly unmeasured) factors that would be 
        sufficient to have created enough selection bias to explain away an observed exposure-outcome association.
        The parameters that the E-value refers to depends on what assumptions an investigator is willing to make,
        and are printed with the results. See the cited article for exact definitions and for more details.",
        "<b>Please use the following citation:</b>",
        "Smith LH & VanderWeele TJ. (2019). Bounding bias due to selection. 
        <i>Epidemiology</i>, forthcoming. <a href='https://arxiv.org/abs/1810.13402'>(Pre-print available).</a>",
        sep = "<br/><br/>"
      ))),
      width = 6
    ) # end e-value explanation sidebar
  ), # end tab for computing e-values
  
  # tab for additional resources
  tabPanel(
    "More resources",
    mainPanel(
      HTML(paste(
        "<b>Developers</b>",
        "<br><br>This website was created by <a href='https://www.louisahsmith.com'>Louisa H. Smith</a> and
        inspired by the <a href = 'https://evalue.hmdc.harvard.edu'>E-values website</a> by
        <a href='https://profiles.stanford.edu/maya-mathur'>Maya Mathur</a>,
        <a href='https://sites.google.com/site/pengdingpku/'>Peng Ding</a>, 
        <a href='https://sph.berkeley.edu/corinne-riddell-phd'>Corinne Riddell</a>, 
        and <a href='https://www.hsph.harvard.edu/tyler-vanderweele/tools-and-tutorials/'>Tyler VanderWeele</a>.",
        
        "<br><br><b>Other software</b>",
        "<br><br>You can alternatively compute E-values
        using the R package <a href='https://cran.r-project.org/web/packages/EValue/index.html'>EValue</a>.",
        
        "<br><br><b>Additional references</b>",
        "<br><br>Evalues also available here"
      )),
      
      # hide the text for keeping page from greying out (so doesn't create extra blank space)
      textOutput("keep_alive")
    )
  )
)

server <- function(input, output, session) {
  # what will be blank text to prevent page grey-out
  output$keep_alive <- renderText({
    req(input$alive_count)
    input$alive_count
  })

  # unput/output for calculating e-values
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

  output$result.text_S <- renderText({
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
        svals()[1],
        sep = ""
      )
    }
    return(result.string_S)
  })
  
  # print message about what parameters e-value refers to
  output$message.text_S <- renderUI({
    m <- mess_S()
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