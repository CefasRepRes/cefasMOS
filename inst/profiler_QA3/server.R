
library(shiny)
library(cefasMOS)
library(rhandsontable)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

  prdata = reactiveValues(data = list(), cruiseList = NULL, marks = list(), subset = NULL)

  observeEvent(input$select_year, {
    prdata$cruiseList = data.table(profiler.cruiselist(yr = input$select_year))
    prdata$cruiseList$name = paste(prdata$cruiseList$CruiseId, prdata$cruiseList$InstId)
    updateSelectInput(session, "select_cruiseID", choices = c("Select cruise" = "", prdata$cruiseList$name))
  })

  observeEvent(input$select_cruiseID, {
    if(input$select_cruiseID != ""){
      selected = prdata$cruiseList[name == input$select_cruiseID]
      withProgress(message = 'Fetching data', value = 0, {
        dat = profiler.fetch(cruiseID = selected$CruiseId,
                             profiler = selected$InstId,
                             min_QA_reached = F, parameters = c("SAL", "O2CONC"))
        incProgress(1)
      })
      prdata$data = dat
      updateSelectInput(session, "select_profile", choices = unique(prdata$data$startTime))
      updateSelectInput(session, "select_variable", choices = unique(prdata$data$par))
    }
  })

  observe(
    if(input$select_profile != ""){
      prdata$subset = prdata$data[startTime == as.POSIXct(input$select_profile, tz = "UTC") & par == input$select_variable]
    }
  )

  output$mark_text = renderText({
    pts = data.table()
    if(!is.null(prdata$subset)){
      pts = brushedPoints(prdata$subset, input$scan_brush)
    }
    if(nrow(pts) > 1){
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
    }
  })

  observeEvent(input$mark_btn, {
    marks = prdata$marks[[input$select_cruiseID]][[input$select_variable]]
    if(!is.null(marks)){
      newdata = rbind(hot_to_r(input$marks), prdata$newmark, fill = T)
    }else{
      newdata = prdata$newmark
    }
    newdata[, offset := niskin - mean_val]
    prdata$marks[[input$select_cruiseID]][[input$select_variable]] = newdata
  })

  output$marks = renderRHandsontable({
    mrk = prdata$marks[[input$select_cruiseID]][[input$select_variable]]
    if(!is.null(mrk)){
      rhandsontable(mrk, readOnly = T, highlightRow = T, digits = 6) %>%
        hot_col(c("niskin", "mean_val"), readOnly = F, format = "0.000") %>%
        hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
    }
  })

  output$scan_plot = renderPlot({
    if(!is.null(prdata$subset)){
      ggplot(prdata$subset) +
        geom_line(aes(dateTime, depth, color = value)) +
        geom_point(aes(dateTime, depth, color = value)) +
        viridis::scale_color_viridis(input$select_variable) +
        theme_bw() +
        theme(legend.position = "bottom", legend.key.width = unit(10, "lines"))
    }
  })

  output$regression = renderPlot({
    if(!is.null(input$marks) ){
      pts = subset(hot_to_r(input$marks), niskin != 0)
      m = lm(data = pts, mean_val ~ niskin)
      ggplot(pts, aes(niskin, mean_val)) +
        geom_point() +
        geom_abline(intercept = coef(m)[1], slope = coef(m)[2]) +
        labs(title = paste(input$select_cruiseID, input$select_variable),
          caption = paste0("y = ", round(coef(m)[1], 2), " + ", "x", round(coef(m)[2], 2) )
          ) +
        theme_bw()
    }
  })
})