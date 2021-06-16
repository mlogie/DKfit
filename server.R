shinyServer(function(input, output, session) {
  allrecipes <- reactiveValues(recipes = allrecipes)
  output$txt <- renderUI({
    'Select meals and click Create Recipes in Meals tab'
  })
  
  observe({
    ree <- (10*input$weight) + (6.25*input$height) - (5*input$age)
    if(input$malefemale=='Male'){
      ree <- ree + 5
    } else {
      ree <- ree - 161
    }
    
    ree <- ree *
      switch(input$exerciseDaily,
             'Sedentary (little or no exercise)' = 1.2,
             'Lightly active (light exercise/sports 1-3 days/week)' = 1.375,
             'Moderately active (moderate exercise/sports 3-5 days/week)' = 1.55,
             'Very active (hard exercise/sports 6-7 days a week)' = 1.725,
             'Extremely active (very hard exercise/sports & a physical job)' = 1.9)
    
    ree <- round(ree,0)
    updateSliderInput(session,
                      inputId = 'totalCalories',
                      label = 'Total Daily Calories',
                      min = ifelse(ree<1000,ree,1000),
                      max = ifelse(ree>4000,ree,4000),
                      value = ree)
    
    output$calCalc <- renderUI({
      paste0('Calculated calories = ',ree)
    })
  })
  
  observeEvent(input$clearFoods, {
    tables <- readRDS('tables.rds')
    tables$extrafoodtable <- NULL
    saveRDS(tables, 'tables.rds')
    calBalances <- getCalBalance(input$totalCalories, input$eatenMeals)
    outputtable <- makerecipetable(i = 1:numberMeals,
                                   meals = c(input$meal1, input$meal2, input$meal3),
                                   allrecipes = allrecipes,
                                   calBalance = calBalances$calBalance)
    output$filetable <- renderText({outputtable})
  })
  
  observeEvent(input$addFood, {
    if(!is.numeric(input$selectorQuantity)){
      output$recipeMessage <- renderUI({'Please input a value for ingredient'})
      return()
    } else if(input$selectorQuantity<=0){
      output$recipeMessage <- renderUI({'Please input a positive value for ingredient'})
      return()
    }
    if(!nchar(input$selector)>0){
      output$recipeMessage <- renderUI({'Please select an ingredient'})
      return()
    }
    macroIng <- macros[macros$`Food Name` == input$selector,]
    output$recipeMessage  <- renderUI({'Ingredient Added'})
    macroIng$quantity     <- input$selectorQuantity
    macroIng$Protein      <- macroIng$Protein*macroIng$quantity/100
    macroIng$Fat          <- macroIng$Fat*macroIng$quantity/100
    macroIng$Carbohydrate <- macroIng$Carbohydrate*macroIng$quantity/100
    macroIng$kcal         <- macroIng$kcal*macroIng$quantity/100
    tables <- readRDS('tables.rds')
    tables$extrafoodtable <- bind_rows(tables$extrafoodtable,
                                       macroIng)
    saveRDS(tables, 'tables.rds')
    calBalances <- getCalBalance(input$totalCalories, input$eatenMeals)
    outputtable <- makerecipetable(i = 1:numberMeals,
                                   meals = c(input$meal1, input$meal2, input$meal3),
                                   allrecipes = allrecipes,
                                   calBalance = calBalances$calBalance)
    output$filetable <- renderText({outputtable})
  })

  observeEvent(input$goButton, {
    output$txt <- renderUI({''})
    # Calculate the daily calorie balance
    calBalances <- getCalBalance(input$totalCalories, input$eatenMeals)
    
    meal_options <- c('Breakfast','Mid-Morning','Lunch','Dinner','Supper')
    possibleMeals <- switch(numberMeals,
                            c(3), c(1,4), c(1,3,4),
                            c(1,3,4,5), c(1:5))
    possibleMeals <- meal_options[possibleMeals]
    #calBalance[which(possibleMeals %in% input$calorieBalance)] <-
    #  calBalance[1]*1.2
    #calBalance <- round(calBalance*input$totalCalories/(sum(calBalance)),0)
    meals <- c(input$meal1, input$meal2, input$meal3)
    outputtable <- makerecipetable(i = 1:numberMeals,
                                   meals = c(input$meal1, input$meal2, input$meal3),
                                   allrecipes = allrecipes,
                                   calBalance = calBalances$calBalance)
    output$filetable <- renderText({outputtable})
  })
})