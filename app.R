# Load packages
library(shiny)
library(bslib)
library(plotly)
library(plyr)
library(dplyr)
library(shinyalert)
library(viridis)
library(readr)

# ui.R
ui <- page_navbar(
  title = "Personal Spending Dashboard",
  sidebar = sidebar(
    open = FALSE, # Closed in mobile browser only
    fileInput(
      inputId = 'file',
      label = 'Choose CSV File',
      accept = c('text/csv',
                 'text/comma-separated-values,text/plain',
                 '.csv'),
      multiple = FALSE
    ) |>
      tagAppendAttributes(
        onInput = "
                  const id = $(this).find('input[type=\"file\"]').attr('id');
                  Shiny.setInputValue(id + '_click', Math.random());
        "
      ),
  ),
  
  theme = bs_theme(
    version = 5, # Reduce the chance of breaking the dashboard
    bootswatch = "darkly",
    base_font = font_google("Inter"),
    navbar_bg = "#45AD82"
  ),
  
  nav_spacer(),
  
  # Info tab
  nav_panel(title = "Home", 
            h3("Welcome!"),
            p("In this personal spending dashboard, you can see how your current month spending has been in various categories."),
            p("This dashboard also provides you valuable information about your spending, such as:"),
            tags$ul(
              tags$li("How does your current month's spending look like?"),
              tags$li("How does your yearly spending trend look like?"),
              tags$li("Does your spending pattern in current month differ from that of average of last 12 months'?"),
              tags$li("For each spending category, how does your current month's improve from average of last 12 months'?"),
            ),
            p('Rest assured that your data will not be stored remotely! If you refresh the dashboard, all results are gone and you need to upload the same csv file for results again.'),
            h3("Remarks"),
            p("This Shiny app is for illustration purpose only."),
            p("There is a specific format for uploaded csv file. Category's name is listed on the first column (with a header as 'category'), with spending for each month in the following columns. Each month's spending should have a relevant header. For example, it is '2024.09' if it is spending during Semptember 2024. See the example screenshot below:"),
            img(src='example.png'),
            p("Please have spending of the past 12 months and current month."),
            p('Head over to Dashboard tab, then open the sidebar (with an arrow sign; top left of your screen) and upload your spending dataset! Then see a brief analysis of your spending in Analysis tab!'),
            br(),
            p("© 2024", a(href = "https://github.com/fenditsim", "Fendi Tsim")," | Built with Shiny")
  ),
  
  # Dashboard
  nav_panel(
    title = "Dashboard",
    layout_columns(
      uiOutput(outputId = "current.month.spending"),
      uiOutput(outputId = "average.months.spending"),
      uiOutput(outputId = "diff")
    ),
    
    layout_columns(
      uiOutput(outputId = "donut.chart"),
      uiOutput(outputId = "stacked.bar.chart")
    ),
    
    layout_columns(
      uiOutput(outputId = "radar.chart"),
      uiOutput(outputId = "diff.chart")
    ),
  ),
  
  # Analysis tab
  nav_panel(title = "Analysis",
            h3("Analysis"),
            p("The following information is based on the spending dataset you've uploaded:"),
            layout_columns(uiOutput(outputId = "overspent")),
            layout_columns(uiOutput(outputId = "underspent")),
            layout_columns(uiOutput(outputId = "same.spending"))
  ),
  
  # Log tab
  nav_panel(title = "Log", 
            h3("Version Log"),
            h4("0.1.4 - 2024.10.14"),
            tags$div(
              tags$ul(
                tags$li("Restructuring the dashboard"),
                tags$li("Adding the notification message (waiting the data to be uploaded) in Dashboard tab"),
              )
            ),
            h4("0.1.3 - 2024.10.10"),
            tags$div(
              tags$ul(
                tags$li("Adding Analysis"),
              )
            ),
            h4("0.1.2 - 2024.10.09"),
            tags$div(
              tags$ul(
                tags$li("Alpha testing"),
              )
            ),
            h4("0.1 - 2024.10.01"),
            tags$div(
              tags$ul(
                tags$li("v0.1"),
              )
            )
  ),
  
  # Github repo link
  nav_item(tags$a(icon("github"), href = "https://github.com/fenditsim/personal-spending-dashboard")) # Link to relevant github repo
)

# server
server <- function(input, output) {
  
  # Notification message (waiting the data to be uploaded)
  observeEvent(input$file_click,
               {
                 shinyalert(
                   title = "Wait",
                   text = "Waiting for your data to be uploaded",
                   size = "xs",
                   closeOnEsc = TRUE,
                   closeOnClickOutside = TRUE,
                   html = TRUE,
                   type = "info",
                   showConfirmButton = TRUE,
                   confirmButtonText = "OK",
                   confirmButtonCol = "#004192",
                   showCancelButton = FALSE,
                   imageUrl = "",
                   animation = TRUE
                 )
               },
               ignoreNULL = FALSE,
               ignoreInit = TRUE
  )
  
  # Reactive value to store the uploaded data
  data <- reactive({
    req(input$file)
    df <- read_csv(input$file$datapath)
    colnames(df)[-1] <- gsub("X", "", colnames(df)[-1]) # Remove X in the month colnames
    df <- df%>%replace(is.na(.), 0) # Replace NA with 0
    return(df)
  })
  
  # Create a reactive df for category (a color per category)
  category <- reactive({
    req(data())
    df <- data.frame('category'=data()$category, 'color'=viridis_pal(option = "C")(nrow(data()))) # Need to find a better way of assigning colours
    return(df)
  })
  
  # Visualize the current month spending as a reactive value_box
  output$current.month.spending <- renderUI({
    req(data())
    bslib::value_box(
      title = paste0("Spending of Current Month (", colnames(data())[ncol(data())], ")"),
      value = paste0("£", colSums(data()[ncol(data())])),
      theme = "orange"
    )
  })
  
  # Visualize the average spending of the past 12 months as a reactive value_box
  output$average.months.spending <- renderUI({
    req(data())
    bslib::value_box(
      title = "Average Spending of Past 12 Months",
      value = paste0("£", round(mean(colSums(data()[(ncol(data())-12):(ncol(data())-1)])), 2)), 
      theme = "purple"
    )
  })
  
  # Visualize the spending difference as a reactive object
  output$diff <- renderUI({
    req(data())
    bslib::value_box(
      title = "Spending Difference between Current Month and Average of Past 12 Months",
      value = paste0("£", abs(round(colSums(data()[ncol(data())]) - mean(colSums(data()[(ncol(data())-12):(ncol(data())-1)])), 2))),
      theme = if (round(colSums(data()[ncol(data())]) - mean(colSums(data()[(ncol(data())-12):(ncol(data())-1)])), 2) > 0) "red" else "green"
    )
  })
  
  # Donut Chart
  output$donut.chart <- renderUI({
    req(data())
    
    fig <- plot_ly(data = data(), labels = ~data()$category, values = ~unlist(data()[,ncol(data())]), textinfo='label+percent', name = colnames(data())[ncol(data())], hovertemplate = "%{label}: <br>£ %{value} </br>", textposition = 'inside', marker = list(colors = category()$color)) %>%
      add_pie(hole = 0.6) %>%
      layout(showlegend = F, plot_bgcolor = "rgba(0,0,0,0)", paper_bgcolor = "rgba(0,0,0,0)")
    
    card <- card(full_screen = TRUE,
                 card_header("How does you spend in current month?"),
                 fig
    )
    
    return(card)
  })
  
  # Stacked Bar Chart
  output$stacked.bar.chart <- renderUI({
    req(data())
    
    yearly <- data.frame(t(data()[,(ncol(data())-12):ncol(data())]))
    colnames(yearly) <- data()$category
    
    fig <- plot_ly(x=rownames(yearly), y=as.numeric(unlist(yearly[,1])), type = "bar", name = colnames(yearly)[1], marker = list(color = category()$color[1]), hovertemplate = "%{x}: <br>£ %{y} </br>")
    for(i in 2:ncol(yearly)) fig <- add_trace(fig, y=as.numeric(unlist(yearly[,i])), type = 'bar', name = colnames(yearly)[i], marker = list(color = category()$color[i]), hovertemplate = "%{x}: <br>£ %{y} </br>")%>%layout( barmode = 'stack')
    fig <- fig%>%layout(xaxis = list(tickangle = 45), yaxis = list(title = 'Spending in £'), showlegend = F, plot_bgcolor = "rgba(0,0,0,0)", paper_bgcolor = "rgba(0,0,0,0)", font=list(color = "white"))
    
    card <- card(full_screen = TRUE,
                 card_header("How does your spending in the past year look like?"),
                 fig
    )
    
    return(card)
  })
  
  # Radar Chart
  output$radar.chart <- renderUI({
    req(data())
    
    average <- data.frame(data()$category, round(rowMeans(data()[(ncol(data())-12):(ncol(data())-1)]), 2), data()[,ncol(data())])
    colnames(average) <- c("category", "avg", colnames(data())[ncol(data())])
    
    fig <- plot_ly(data = average, type = 'scatterpolar', fill = 'toself', hovertemplate = "%{theta}: <br>£ %{r} </br>")%>%
      add_trace(r = ~average$avg, theta = average$category, name = "Average of past 12 months", fillcolor=rgb(147, 112, 219, maxColorValue = 255), marker=list(color=rgb(147, 112, 219, maxColorValue = 255)))%>%
      add_trace(r = ~average[,ncol(average)], theta = average$category, name = colnames(average)[ncol(average)], fillcolor=rgb(255, 143, 0, maxColorValue = 255), marker=list(color=rgb(255, 143, 0, maxColorValue = 255)))%>%
      layout(
        polar = list(
          radialaxis = list(
            visible = T,
            range = c(0, round_any(max(as.numeric(unlist(average))), 100, f = ceiling))
          ),
          bgcolor = "rgba(0,0,0,0)"
        ),
        showlegend = F,
        plot_bgcolor = "rgba(0,0,0,0)", paper_bgcolor = "rgba(0,0,0,0)",
        font=list(color = "white")
      )
    
    card <- card(full_screen = TRUE,
                 card_header("How does your current month spending pattern differ from the average of last 12 months'?"),
                 fig
    )
    
    return(card)
  })
  
  # Bar Chart (Difference in each category)
  difference <- reactive({
    req(data())
    df <- data.frame(data()$category, rowMeans(data()[(ncol(data())-12):(ncol(data())-1)]), data()[,ncol(data())])
    colnames(df) <- c("category", "average", colnames(data())[ncol(data())])
    df$diff <- round(df[,ncol(df)] - df$average, 2)
    return(df)
  })
  
  output$diff.chart <- renderUI({
    req(difference())
    
    fig <- plot_ly(data = difference(), x=~difference()$category, y=~difference()$diff, type = "bar", name = ~ifelse(difference()$diff > 0, yes = "Increase", no = "Decrease"),
                   hovertemplate = "%{x}: <br>£ %{y} </br>", 
                   color = ~difference()$diff > 0, colors = c("green", "red"),
                   yaxis = 'y') %>%
      layout(
        showlegend = F,
        xaxis = list(title="", tickangle = 45),
        yaxis = list(title="Difference in £"),
        plot_bgcolor = "rgba(0,0,0,0)", paper_bgcolor = "rgba(0,0,0,0)",
        font=list(color = "white")
      )
    
    card <- card(full_screen = TRUE,
                 card_header("How does each item of spending in this month spending differ from the average of last 12 months'?"),
                 fig
    )
    
    return(card)
  })
  
  # Analysis: Overspending
  output$overspent <- renderUI({
    req(difference())
    
    df <- difference()%>%filter(diff > 0) # Filter overspending category
    df <- df%>%select(category, diff)
    df <- df[order(df$diff, decreasing = TRUE),]
    
    value <- c()
    
    for (i in 1:nrow(df)) value[i] <- paste0("You've overspent £", df$diff[i], " in ", df$category[i], " this month than the average of last 12 months.")
    
    bslib::value_box(
      title = "",
      value = "Categories that you've been overspending",
      lapply(value, function(x) p(x)),
      theme = "red"
    )
  })
  
  # Analysis: Underspending
  output$underspent <- renderUI({
    req(difference())
    
    df <- difference()%>%filter(diff < 0) # Filter overspending category
    df <- df%>%select(category, diff)
    df <- df[order(abs(df$diff), decreasing = TRUE),]
    
    value <- c()
    
    for (i in 1:nrow(df)) value[i] <- paste0("You've saved up £", abs(df$diff[i]), " in ", df$category[i], " this month than the average of last 12 months.")
    
    bslib::value_box(
      title = "",
      value = "Categories that you've been saving up",
      lapply(value, function(x) p(x)),
      theme = "green"
    )
  })
  
  # Analysis: Neither overspending nor underspending
  output$same.spending <- renderUI({
    req(difference())
    
    df <- difference()%>%filter(diff == 0) # Filter the same spending category
    df <- df%>%select(category, diff)
    
    value <- c()
    
    for (i in 1:nrow(df)) value[i] <- paste0(" - ", df$category[i])
    
    bslib::value_box(
      title = "",
      value = "Categories you've not been overspending or underspending",
      lapply(value, function(x) p(x)),
      theme = "black"
    )
  })
  
}

shinyApp(ui, server)