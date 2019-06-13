library(shiny)
library(rhandsontable)
library(ggplot2)
library(plotly)
library(shinythemes)

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("cosmo"),
  navbarPage("Ludica",
             tabPanel("Reglas del juego",
                      includeMarkdown("include.md")
             ),
             tabPanel("Comparación de los dos grupos",
                      plotlyOutput("plot")
                      ),
             
             tabPanel("Equipo Verde",
                        fluidRow(column(6, plotlyOutput("plot1.1"), offset = 0.5),
                                 column(6,plotlyOutput("plot1.2") , offset = 0.5)),
                      fluidRow(column(6, rHandsontableOutput('table'), offset = 0.5),
                               column(6,plotlyOutput("plot1.3") , offset = 0.5))
                      ),
             tabPanel("Equipo Azul",
                        fluidRow(column(6, plotlyOutput("plot2.1"), offset = 0.5),
                                 column(6,plotlyOutput("plot2.2") , offset = 0.5)),
                      fluidRow(column(6, rHandsontableOutput('tab'), offset = 0.5),
                               column(6,plotlyOutput("plot2.3") , offset = 0.5))
                      ),
             tabPanel("Equipo Rojo",
                        fluidRow(column(6, plotlyOutput("plot3.1"), offset = 0.5),
                                 column(6,plotlyOutput("plot3.2") , offset = 0.5)),
                        
                        fluidRow(column(6, rHandsontableOutput('tbl'), offset = 0.5),
                                 column(6,plotlyOutput("plot3.3") , offset = 0.5))
                      )
  ))

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  ventanas=rep(0, 3)
  puertas=rep(0, 3)
  cuerpo=rep(0, 3)
  techos=rep(0, 3)
  empleados=rep(8, 3)
  casas=rep(0, 3)
  df = data.frame(ventanas=ventanas, puertas=puertas, cuerpo=cuerpo,
                  techos=techos, empleados=empleados, casas=casas)
  
  cv=-400
  cp=-700
  ct=-1500
  cc=-2000
  ce=-1000
  ch=10000
  
  ## Defining a reactivevalues object so that whenever dataset value changes it affects everywhere in the scope of every reactive function
  datavalues <- reactiveValues(data=df)
  dat <- reactiveValues(data=df)
  dv <- reactiveValues(data=df)
  
  # Display the data using renderRHandsontable() function
  # It will display the data frame that was initialized in the reactiveValues()
  # This will display data in a excel like editable cells
  
  output$table <- renderRHandsontable({
    rhandsontable(datavalues$data)
  })
  
  output$tab <- renderRHandsontable({
    rhandsontable(dat$data)
  })
  
  output$tbl <- renderRHandsontable({
    rhandsontable(dv$data)
  })
  # Watching any changes made to table cells in column variables a or b and then update column c based on formula
  
  observeEvent(
    input$table$changes$changes, # observe if any changes to the cells of the rhandontable
    {
      
      xi=input$table$changes$changes[[1]][[1]] # capture the row which is changed
      datavalues$data <- hot_to_r(input$table) # convert the rhandontable to R data frame object so manupilation / calculations could be done
      
      # Calculating the cell value of column C using cell values in column a and b
      # 1 is added to row index because change event row and column indices starts with zero vs R index which starts with 1
    }
  )
  
  observeEvent(
    input$tab$changes$changes, # observe if any changes to the cells of the rhandontable
    {
      
      xi=input$tab$changes$changes[[1]][[1]] # capture the row which is changed
      dat$data <- hot_to_r(input$tab) # convert the rhandontable to R data frame object so manupilation / calculations could be done
      
      # Calculating the cell value of column C using cell values in column a and b
      # 1 is added to row index because change event row and column indices starts with zero vs R index which starts with 1
    }
  )
  
  observeEvent(
    input$tbl$changes$changes, # observe if any changes to the cells of the rhandontable
    {
      
      xi=input$tbl$changes$changes[[1]][[1]] # capture the row which is changed
      dv$data <- hot_to_r(input$tbl) # convert the rhandontable to R data frame object so manupilation / calculations could be done
      
      # Calculating the cell value of column C using cell values in column a and b
      # 1 is added to row index because change event row and column indices starts with zero vs R index which starts with 1
    }
  )
  ###########################################################################
  #                                 GRAFICOS                                #
  ########################################################################### 
  ## plot a histogram using plotly
  ## plotly graph also changes as the table cell values changes
  ggpareto <- function(df, grupo, ronda) {
    
    #poner el titulo
    title <- grupo
    modality <- as.factor(names(df))
    Df <- as.data.frame(modality)
    Df$frecuency <- as.vector(t(df[ronda,]))
    Df <- Df[1:4,]
    
    Df = Df [ rev(order(c(Df$frecuency))),]
    
    nr <- nrow(Df)
    N <- sum(row(Df))
    
    Df$modality <- ordered(Df$modality, levels = unlist(Df$modality, use.names = F))
    Df$modality_int <- as.integer(c(1,2,3,4))
    Df$cumfreq <- cumsum(Df$frecuency)
    Df$cumperc <- Df$cumfreq/sum(Df$frecuency) * 100
    
    library(ggplot2)
    
    g <- ggplot(data=Df, aes(x=modality, y=frecuency)) +
      geom_bar(stat="identity", aes(fill = modality_int))+
      geom_line(aes(x=modality, y = cumfreq, color = cumperc))+
      geom_point(aes(x=modality, y = cumfreq, color = cumperc), pch = 19) +
      scale_x_discrete(breaks = Df$modality) +
      guides(fill = FALSE, color = FALSE) +
      labs(title = paste0("Ronda ",ronda," grupo ", title), y = "absolute frequency", X="Depto") +
      theme_bw()
    
    library(plotly)
    ggplotly(g)
  }
  
  cost <- c(cv, cp, ct, cc, ce, ch)
  
  costos <- function(df1, df2, df3, cost){
    
    v1 <- sum(df1[1,]*cost)
    v2 <- sum(df1[2,]*cost)
    
    a1 <- sum(df2[1,]*cost)
    a2 <- sum(df2[2,]*cost)
    
    r1 <- sum(df3[1,]*cost)
    r2 <- sum(df3[2,]*cost)
    
    v <- cbind(c("Ronda 1", "Ronda 2"), rep("Verde",2))
    a <- cbind(c("Ronda 1", "Ronda 2"), rep("Azul",2))
    r <- cbind(c("Ronda 1", "Ronda 2"), rep("Rojo",2))
    
    m<- rbind(v, a, r)
    colnames(m) <- c("Ronda", "Equipo")
    m <- as.data.frame(m)
    m$Ganancia <- c(v1, v2, a1, a2, r1, r2)
    cbp1 <- c("#4494D3", "#C03729", "#449865")
    
    g <- ggplot(m, aes(x = Ronda, y = Ganancia, group = Equipo)) + 
      geom_line(aes(color=Equipo)) + 
      scale_colour_manual(values=cbp1)+
      labs(x="Ronda", y="Ganancia", title="Desempeño por Equipo") +
      theme(legend.position = "top")+
      theme_bw()
    
    library(plotly)
    ggplotly(g)
  }
  
  
  geqp <- function(df, cost){
    
    v1 <- sum(df[1,]*cost)
    v2 <- sum(df[2,]*cost)
    
    m <- as.data.frame(c("Ronda 1", "Ronda 2"))
    colnames(m) <- c("Ronda")
    m$Ganancia <- c(v1, v2)
    
    g <- ggplot(m, aes(x = Ronda, y = Ganancia, color = Ganancia)) + 
      geom_point(aes(x = Ronda, y = Ganancia)) + 
      labs(x="Ronda", y="Ganancia", title="Situación Financiera") +
      theme_bw()
    
    library(plotly)
    ggplotly(g)
  }
  # applying the function to the factor variable:
  
  output$plot <- renderPlotly({ costos(datavalues$data, dat$data,dv$data, cost) })
  output$plot1.1 <- renderPlotly({ ggpareto(datavalues$data, "Verde", 1) })
  output$plot1.2 <- renderPlotly({ ggpareto(datavalues$data, "Verde", 2) })
  output$plot2.1 <- renderPlotly({ ggpareto(dat$data, "Azul", 1) })
  output$plot2.2 <- renderPlotly({ ggpareto(dat$data, "Azul", 2) })
  output$plot3.1 <- renderPlotly({ ggpareto(dv$data, "Rojo", 1) })
  output$plot3.2 <- renderPlotly({ ggpareto(dv$data, "Rojo", 2) })
  output$plot1.3 <- renderPlotly({ geqp(datavalues$data, cost) })
  output$plot2.3 <- renderPlotly({ geqp(dat$data, cost) })
  output$plot3.3 <- renderPlotly({ geqp(dv$data, cost) })
}

# Run the application 
shinyApp(ui = ui, server = server)

