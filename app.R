library(shiny)
library(shinyWidgets)

ui <- fluidPage(
  tags$head(
    tags$style(type='text/css', 
               ".nav-tabs {font-size: 20px} "),
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
  titlePanel("Sample Size Calculator for MKT 6300"),
  tabsetPanel(
    tabPanel(title="$n=\\left(\\frac{Z\u03c3}{D}\\right)^2$",
             tableOutput("z.table1"),
             numericInput("m.stdev", "Standard Deviation, (\u03c3)", 
                          value=1, min=0, width="200px"),
             numericInput("m.moe", "Margin of Error, (D)", 
                          value=1, min = 0, width="200px"),
             numericInput("m.zscore", "Z-score, (Z)", 
                          value=1, min = 0, width="200px"),
             autonumericInput("m.rr", "Response Rate", value=100, 
                              currencySymbol="%", 
                              currencySymbolPlacement = "s", 
                              decimalPlaces = 0, align="left", 
                              maximumValue=100, minimumValue = 0, 
                              width="200px"),
             actionButton("m.click", "Calculate", class="btn-lg btn-success"),
             htmlOutput("m.result")
    ),
    tabPanel(title="$n=\\frac{Z^2\u03c0(1-\u03c0)}{D^2}$",
             tableOutput("z.table2"),
             numericInput("p.prop", "Proportion, (\u03c0)", 
                          value=1, min = 0, width="200px"),
             numericInput("p.moe", "Margin of Error, (D)", 
                          value=1, min = 0, width="200px"),
             numericInput("p.zscore", "Z-score, (Z)", 
                          value=1, min = 0, width="200px"),
             autonumericInput("p.rr", "Response Rate", value="100", currencySymbol="%",
                              currencySymbolPlacement = "s", decimalPlaces = 0,
                              align="left", maximumValue=100, minimumValue = 0,
                              width="200px"),
             actionButton("p.click", "Calculate", class="btn-lg btn-success"),
             htmlOutput("p.result")
    )
  )
)

server <- function(input, output, session) {
  Confidence.Level <- c("90%","95%","97.5%", "99%")
  Z.Score <- c(1.64, 1.96, 2.24, 2.58)
  Z.Table <- data.frame(Confidence.Level,Z.Score)
  
  m.n <- eventReactive(input$m.click, {
    (input$m.zscore*input$m.stdev/input$m.moe)^2
  })
  m.N <- eventReactive(input$m.click, {
    ceiling(m.n())/(input$m.rr/100)
  })
  output$m.result <- renderUI({
    str1 <- paste("n =",round(m.n(),2),"=",ceiling(m.n()),"completed surveys")
    str2 <- paste("N =",round(m.N(),2),"=",ceiling(m.N()),"surveys sent")
    HTML("<br/>",paste(str1,str2,sep="<br/>"))
  })
  output$z.table1 <- renderTable({
    Z.Table
  })
  output$z.table2 <- renderTable({
    Z.Table
  })
  p.n <- eventReactive(input$p.click, {
    (input$p.zscore^2*input$p.prop*(1-input$p.prop))/input$p.moe^2
  })
  p.N <- eventReactive(input$p.click, {
    ceiling(p.n())/(input$p.rr/100)
  })
  output$p.result <- renderUI({
    str3 <- paste("n =",round(p.n(),2),"=",ceiling(p.n()),"completed surveys")
    str4 <- paste("N =",round(p.N(),2),"=",ceiling(p.N()),"surveys sent")
    HTML("<br/>",paste(str3,str4,sep="<br/>"))
  })
}

shinyApp(ui, server)