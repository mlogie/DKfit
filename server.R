shinyServer(function(input, output, session) {
  allrecipesreact <- reactiveValues(recipes = allrecipes,
                                    numMeals = 3)
  output$txt <- renderUI({
    ''
  })
  
  updateSelectizeInput(session,
                       inputId = 'selector',
                       label = 'Select Ingredient',
                       choices = macros %>% pull(`Food Name`),
                       server = TRUE,
                       options = list(
                         placeholder = 'Please search here',
                         onInitialize = I('function() { this.setValue(""); }')
                       ))
  
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
    tables$extrafoodtable <- tables$extrafoodtable %>% filter(source != 'Man')
    if(nrow(tables$extrafoodtable)==0) tables$extrafoodtable <- NULL
    saveRDS(tables, 'tables.rds')
    calBalances <- getCalBalance(input$totalCalories, input$eatenMeals,
                                 input$skippedMeals, as.numeric(input$numMeals))
    outputtable <- makerecipetable(meals = c(input$meal1, input$meal2, input$meal3, input$meal4, input$meal5),
                                   allrecipes = allrecipes,
                                   calBalances = calBalances,
                                   numMeals = as.numeric(input$numMeals))
    output$filetable <- renderText({outputtable})
  })
  
  observeEvent(input$clearIngs, {
    tables <- readRDS('tables.rds')
    tables$extrafoodtable <- tables$extrafoodtable %>% filter(source != 'Ing')
    if(nrow(tables$extrafoodtable)==0) tables$extrafoodtable <- NULL
    saveRDS(tables, 'tables.rds')
    calBalances <- getCalBalance(input$totalCalories, input$eatenMeals,
                                 input$skippedMeals, as.numeric(input$numMeals))
    outputtable <- makerecipetable(meals = c(input$meal1, input$meal2, input$meal3, input$meal4, input$meal5),
                                   allrecipes = allrecipesreact,
                                   calBalances = calBalances,
                                   numMeals = as.numeric(input$numMeals))
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
    macroIng$Protein      <- round(macroIng$Protein*macroIng$quantity/100,0)
    macroIng$Fat          <- round(macroIng$Fat*macroIng$quantity/100,0)
    macroIng$Carbohydrate <- round(macroIng$Carbohydrate*macroIng$quantity/100,0)
    macroIng$kcal         <- round(macroIng$kcal*macroIng$quantity/100,0)
    macroIng$source       <- 'Ing'
    names(macroIng) <-
      c('Food_','Protein_','Fat_','Carbs_','kCals_','Quantity','source')
    tables <- readRDS('tables.rds')
    tables$extrafoodtable <- bind_rows(tables$extrafoodtable,
                                       macroIng)
    saveRDS(tables, 'tables.rds')
    calBalances <- getCalBalance(input$totalCalories, input$eatenMeals,
                                 input$skippedMeals, as.numeric(input$numMeals))
    outputtable <- makerecipetable(meals = c(input$meal1, input$meal2, input$meal3, input$meal4, input$meal5),
                                   allrecipes = allrecipesreact,
                                   calBalances = calBalances,
                                   numMeals = as.numeric(input$numMeals))
    output$filetable <- renderText({outputtable})
  })
  
  observeEvent(input$addFoodstuff, {
    if(!is.numeric(input$foodstuffCal)){
      output$foodstuffMessage <- renderUI({'Please input calories'})
      return()
    } else if(input$foodstuffCal<=0){
      output$foodstuffMessage <- renderUI({'Please input a positive value for calories'})
      return()
    }
    if(!nchar(input$newFoodstuff)>0){
      output$foodstuffMessage <- renderUI({'Please enter a name for the food'})
      return()
    }
    macroFood <- data.frame(Food_ = input$newFoodstuff,
                            Protein_ = NA, Fat_ = NA, Carbs_ = NA,
                            kCals_ = input$foodstuffCal,
                            Quantity = 1, source = 'Man')
    tables <- readRDS('tables.rds')
    tables$extrafoodtable <- bind_rows(tables$extrafoodtable,
                                       macroFood)
    saveRDS(tables, 'tables.rds')
    calBalances <- getCalBalance(input$totalCalories, input$eatenMeals,
                                 input$skippedMeals, as.numeric(input$numMeals))
    outputtable <- makerecipetable(meals = c(input$meal1, input$meal2, input$meal3, input$meal4, input$meal5),
                                   allrecipes = allrecipesreact,
                                   calBalances = calBalances,
                                   numMeals = as.numeric(input$numMeals))
    output$filetable <- renderText({outputtable})
  })
  
  observe({
    calBalances <- getCalBalance(input$totalCalories, input$eatenMeals,
                                 input$skippedMeals, as.numeric(input$numMeals))
    outputtable <- makerecipetable(meals = c(input$meal1, input$meal2, input$meal3, input$meal4, input$meal5),
                                   allrecipes = allrecipesreact,
                                   calBalances = calBalances,
                                   numMeals = as.numeric(input$numMeals))
    output$filetable <- renderText({outputtable})
    
    if(input$numMeals<5){
      updateSelectizeInput(session, inputId = 'meal5', selected = "",
                           choices = '', 
                           options = list(
                             placeholder = 'Select 5 meals to choose a meal'))
    }
    
    if(input$numMeals<4){
      updateSelectizeInput(session, inputId = 'meal4', selected = "",
                           choices = '', 
                           options = list(
                             placeholder = 'Select 4+ meals to choose a meal'))
    }
  })
  
  observeEvent(input$numMeals, {
    if(input$numMeals==5&allrecipesreact$numMeals<5){
      updateSelectizeInput(session, inputId = 'meal5', selected = "",
                           choices = names(allrecipesreact$recipes),
                           options = list(
                             placeholder = 'Select a meal'))
    }
    if(input$numMeals>=4&allrecipesreact$numMeals<4){
      updateSelectizeInput(session, inputId = 'meal4', selected = "",
                           choices = names(allrecipesreact$recipes),
                           options = list(
                             placeholder = 'Select a meal'))
    }
    allrecipesreact$numMeals <- input$numMeals
  })
})