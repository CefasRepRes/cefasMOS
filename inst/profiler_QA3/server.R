
library(shiny)
library(cefasMOS)
library(rhandsontable)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

  prdata = reactiveValues(cruiseList = NULL)

  observeEvent(input$select_year, {
    # prdata$cruiseList = data.table(profiler.cruiselist(yr = input$select_year))
    print(input$select_year)
      load("cruiseList.rdata")
      prdata$cruiseList = data.table(cruiseList)
      #
    prdata$cruiseList$name = paste(cruiseList$CruiseId, cruiseList$InstId)
    updateSelectInput(session, "select_cruiseID", choices = c("Select cruise" = "", prdata$cruiseList$name))
  })

  observe({
    if(input$select_cruiseID != ""){
      selected = prdata$cruiseList[name == input$select_cruiseID]
      print("fetching")
      dat = profiler.fetch(cruiseID = selected$CruiseId,
                           profiler = selected$InstId,
                           min_QA_reached = F, parameters = c("SAL", "O2CONC"))
      prdata$data = dat
    }
    updateSelectInput(session, "select_profile", choices = unique(prdata$data$startTime))
    updateSelectInput(session, "select_variable", choices = unique(prdata$data$par))
  })

  observe(
    if(input$select_profile != ""){
      prdata$subset = prdata$data[startTime == as.POSIXct(input$select_profile, tz = "UTC") & par == input$select_variable]
    }
  )

  observeEvent(input$mark_btn, {
    prdata$marks[[input$select_variable]] = rbind(prdata$marks[[input$select_variable]], prdata$newmark, fill = T)
  })

  output$bottle_plot = renderPlot({
    ggplot(prdata$subset) +
      geom_line(aes(dateTime, depth, color = value)) +
      geom_point(aes(dateTime, depth, color = value)) +
      viridis::scale_color_viridis(input$select_variable) +
      theme_bw() +
      theme(legend.position = "bottom", legend.key.width = unit(10, "lines"))

  })

  output$mark_text = renderText({
    pts = brushedPoints(prdata$subset, input$scan_brush)
    newmark = data.table(
      dateTime = as.character(median(pts$dateTime)),
      stn = median(pts$station),
      var = unique(pts$par),
      mean_depth = round(mean(pts$depth), 1),
      sd_depth = round(sd(pts$depth), 1),
      mean_val = round(mean(pts$value), 3),
      sd_val = round(sd(pts$value), 3),
      niskin = 0)
    prdata$newmark = newmark
    paste("Depth = ", newmark$mean_depth, "+/-", newmark$sd_depth,
          "Value = ", newmark$mean_val, "+/-", newmark$sd_val)
  })

  output$marks = renderRHandsontable({
    mrk = data.table(prdata$marks[[input$select_variable]])
    rhandsontable(mrk, readOnly = T, highlightRow = T, digits = 6) %>%
      hot_col(c("niskin"), readOnly = F, format = "0.0000") %>%
      hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
  })

  output$regression = renderPlot({
    pts = subset(hot_to_r(input$marks), niskin != 0)
    m = lm(data = pts, mean_val ~ niskin)
    ggplot(pts, aes(niskin, mean_val)) +
      geom_point() +
      geom_abline(intercept = coef(m)[1], slope = coef(m)[2]) +
      labs(title = paste(input$select_cruiseID, input$select_variable),
        caption = paste0("y = ", round(coef(m)[1], 2), " + ", "x", round(coef(m)[2], 2) )
        ) +
      theme_bw()
  })

})