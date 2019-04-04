.libPaths("/usr/local/lib/R/site-library/00LOCK-packrat")
.libPaths()
library(shiny)
library(EValue)
library(shinythemes)

options(shiny.sanitize.errors = FALSE)

# message to display if non-null true value
nonnull.mess <- 'Note: You are calculating a "non-null" selection bias value, i.e., a value for the minimum
                amount of selection bias needed to move the estimate and confidence interval
                to your specified true value rather than to the null value.'

# Define UI for application that draws a histogram
ui <- navbarPage(
    tags$head(
        tags$link(rel="stylesheet", 
                  href="https://cdn.jsdelivr.net/npm/katex@0.10.1/dist/katex.min.css", 
                  integrity="sha384-dbVIfZGuN1Yq7/1Ocstc1lUEm+AT+/rCkibIcC/OmWo5f0EA48Vf8CytHzGrSwbQ",
                  crossorigin="anonymous"),
        HTML('<script defer src="https://cdn.jsdelivr.net/npm/katex@0.10.1/dist/katex.min.js" integrity="sha384-2BKqo+exmr9su6dir+qCw08N2ZKRucY4PrGQPPWU1A7FtlCGjmEGFqXCv5nyM5Ij" crossorigin="anonymous"></script>'),
        HTML('<script defer src="https://cdn.jsdelivr.net/npm/katex@0.10.1/dist/contrib/auto-render.min.js" integrity="sha384-kWPLUVMOks5AQFrykwIup5lo0m3iMkkHrD0uJ4H5cjeGihAutqP0yW0J6dpFiVkI" crossorigin="anonymous"></script>'),
        HTML('
    <script>
      document.addEventListener("DOMContentLoaded", function(){
        renderMathInElement(document.body, {
          delimiters: [{left: "$", right: "$", display: false}]
        });
      })
    </script>')
    ),
    title ="Bounding bias due to selection",          
  id = "navbar",
  theme = shinytheme("yeti"),
  tabPanel(
    "Selection Bias",
    mainPanel(

      # TO FIX
      # Google Analytics
      # this JS is from the "tracking code" available on the Google Analytics website
      #         tags$head( HTML( '<script async src="https://www.googletagmanager.com/gtag/js?id=UA-125815848-1"></script>
      # <script>
      #                                window.dataLayer = window.dataLayer || [];
      #                                function gtag(){dataLayer.push(arguments);}
      #                                gtag("js", new Date());
      #
      #                                gtag("config", "UA-125815848-1");
      #                                </script>' ) ),
      # END Google Analytics

      wellPanel(HTML(paste("<b>What is selection bias?</b>",
        "Here is information about selection bias.",
        "Here is more information about selection bias.",
        sep = "<br/><br/>"
      ))),
      width = 8
    ),
    sidebarPanel(
      HTML(paste("<b>This website is based on the following article:</b>",
        "Smith LH & VanderWeele TJ. (2019). Bounding bias due to selection. 
                                             <i>Epidemiology</i>, forthcoming. <a href='https://arxiv.org/abs/1810.13402'>(Pre-print available).</a>",
        "<b>Bug reports</b>",
        "Submit any bug reports to: <i>louisa_h_smith [AT] g [DOT] harvard [DOT] edu</i> or open
        an issue on <a href='https://github.com/louisahsmith/selection/issues'>Github</a>.",
        sep = "<br/><br/>"
      ))
    )
  ),
  tabPanel(
    "E-values for selection bias",
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
        "assump_S", "Additional assumptions (see linked article for details):",
        c(
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
    ), # ends mainPanel
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
    ) # end explanation sidebar
  ), # end selection bias panel
  tabPanel(
    "More resources",
    mainPanel(HTML(paste(
      "<b>Developers</b>",
      "<br><br>This website was created by <a href='https://www.louisahsmith.com'>Louisa H. Smith</a> and
      inspired by the <a href = 'https://evalue.hmdc.harvard.edu'>E-values website</a> by
      <a href='https://profiles.stanford.edu/maya-mathur'>Maya Mathur</a>,
                                            <a href='https://sites.google.com/site/pengdingpku/'>Peng Ding</a>, <a href='https://sph.berkeley.edu/corinne-riddell-phd'>Corinne Riddell</a>, 
      and <a href='https://www.hsph.harvard.edu/tyler-vanderweele/tools-and-tutorials/'>Tyler VanderWeele</a>.",
      "<br><br><b>Other software</b>",
      "<br><br>You can alternatively compute E-values
                                        using the R package <a href='https://cran.r-project.org/web/packages/EValue/index.html'>EValue</a>.",
      "<br><br><b>Additional references</b>",
      "<br><br>Evalues also available here"
    )))
  )
)

server <- function(input, output, session) {
    
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
                     HR = svalues.HR)
      svals <- round(do.call(func, sval_args)[2, ], 2)
    
    return(svals)
  })
  
  # message for selection bias
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
      if(S_eq_U & risk_inc) {
          m2 <- "$\\text{RR}_{UY \\mid (A = 1)}$"
          return(paste(m1, m2, m3))
      }
      if(S_eq_U & risk_dec) {
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
    ##### Create String for UI #####
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
  # change message slightly from package
  output$message.text_S <- renderUI({
      m <- mess_S()
      tagList(
          helpText(m),
          tags$script('renderMathInElement(document.getElementById("message.text_S"), {delimiters: [{left: "$", right: "$", display: false}]});')
      )
  })  
}

# Run the application
shinyApp(ui = ui, server = server)