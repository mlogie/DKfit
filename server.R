shinyServer(function(input, output, session) {
  allrecipes <- reactiveValues(recipes = allrecipes)
  output$txt <- renderUI({
    'Select calories and meals then click Create Recipes in Menu Options tab'
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
  
  #numericInput(inputId = 'exerciseDaily
  
  observe({
    meal_options <- c('Breakfast','Mid-Morning','Lunch','Dinner','Supper')
    meals <- switch(input$numberMeals,
                    c(3), c(1,4), c(1,3,4),
                    c(1,3,4,5), c(1:5))
    updateCheckboxGroupInput(session, inputId = 'calorieBalance',
                             label = 'Big Meal',
                             choices = meal_options[meals],
                             selected = NULL)

    updateSelectizeInput(session,
                         inputId = 'meal1',
                         label = meal_options[meals][1],
                         choices = names(allrecipes$recipes),
                         options = list(
                           placeholder = 'Please search here',
                           onInitialize = I('function() { this.setValue(""); }')))
    updateSelectizeInput(session,
                         inputId = 'meal2',
                         label = meal_options[meals][2],
                         choices = names(allrecipes$recipes),
                         options = list(
                           placeholder = 'Please search here',
                           onInitialize = I('function() { this.setValue(""); }')))
    
    if(input$numberMeals>2){
      updateSelectizeInput(session,
                           inputId = 'meal3',
                           label = meal_options[meals][3],
                           choices = names(allrecipes$recipes),
                           options = list(
                             placeholder = 'Please search here',
                             onInitialize = I('function() { this.setValue(""); }')))
    } else {
      updateSelectizeInput(session,
                           inputId = 'meal3',
                           label = '',
                           choices = c('Select 3+ meals...'),
                           selected = 'Select 3+ meals...')
    }
    if(input$numberMeals>3){
      updateSelectizeInput(session,
                           inputId = 'meal4',
                           label = meal_options[meals][4],
                           choices = names(allrecipes$recipes),
                           options = list(
                             placeholder = 'Please search here',
                             onInitialize = I('function() { this.setValue(""); }')))
    } else {
      updateSelectizeInput(session,
                           inputId = 'meal4',
                           label = '',
                           choices = c('Select 4+ meals...'),
                           selected = 'Select 4+ meals...')
    }
    if(input$numberMeals>4){
      updateSelectizeInput(session,
                           inputId = 'meal5',
                           label = meal_options[meals][5],
                           choices = names(allrecipes$recipes),
                           options = list(
                             placeholder = 'Please search here',
                             onInitialize = I('function() { this.setValue(""); }')))
    } else {
      updateSelectizeInput(session,
                           inputId = 'meal5',
                           label = '',
                           choices = c('Select 5 meals...'),
                           selected = 'Select 5 meals...')
    }
  })
  
  observeEvent(input$addIngredient, {
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
    output$recipeMessage <- renderUI({'Ingredient Added'})
    macroIng$quantity <- input$selectorQuantity
    macroIng$Protein      <- macroIng$Protein*macroIng$quantity/100
    macroIng$Fat          <- macroIng$Fat*macroIng$quantity/100
    macroIng$Carbohydrate <- macroIng$Carbohydrate*macroIng$quantity/100
    macroIng$kcal         <- macroIng$kcal*macroIng$quantity/100
    files <- list.files(file.path(tmpdir),full.names = TRUE)
    if(any(grepl('unfinished',basename(files)))){
      macroIng <-
        bind_rows(readRDS(file.path(tmpdir,'unfinished.rds')),macroIng)
    }
    saveRDS(macroIng,file.path(tmpdir,'unfinished.rds'))
    output$filetable <- renderUI({
      HTML(
        htmlTable(x = macroIng)
      )
    })
  })
  
  observeEvent(input$clearRecipe, {
    files <- list.files(file.path(tmpdir),full.names = TRUE)
    if(any(grepl('unfinished',basename(files)))){
      unlink(file.path(tmpdir,'unfinished.rds'))
    }
    output$buildMessage <- renderUI({'Recipe cleared'})
    output$recipeMessage <- renderUI({''})
    output$filetable <- renderUI({''})
  })
  
  observeEvent(input$clearAllRecipes, {
    files <- list.files(file.path(tmpdir),full.names = TRUE)
    if(any(grepl('unfinished',basename(files)))){
      unlink(file.path(tmpdir,'unfinished.rds'))
    }
    recipefiles <- files[grepl('done_recipes',basename(files))]
    if(length(recipefiles)>0){
      unlink(recipefiles)
    }
    output$buildMessage <- renderUI({'All recipes deleted'})
    source('recipes.R')
  })
  
  observeEvent(input$buildRecipe, {
    files <- list.files(file.path(tmpdir),full.names = TRUE)
    if(!any(grepl('unfinished',basename(files)))){
      output$recipeMessage <- renderUI({''})
      output$buildMessage <- renderUI({'No recipe to build'})
      return()
    } else if(input$recipeName==''){
      output$recipeMessage <- renderUI({''})
      output$buildMessage <- renderUI({'Please enter a name'})
      return()
    } else if(input$recipeName %in% names(allrecipes$recipes)){
      output$recipeMessage <- renderUI({''})
      output$buildMessage <- renderUI({'Name already taken'})
      return()
    } else {
      recipefiles <- files[grepl('done_recipes',basename(files))]
      if(length(recipefiles)>0){
        numbers <- as.numeric(str_extract(basename(recipefiles),'[0-9]+'))
        file_num <- max(numbers)+1
      } else {
        file_num <- 1
      }
      recipe_built <- readRDS(file.path(tmpdir,'unfinished.rds'))
      recipe_built <- recipe_built %>%
        bind_rows(summarise_all(., funs(if(is.numeric(.)) sum(.) else "Total")))
      output$filetable <- renderUI({
        HTML(
          htmlTable(x = recipe_built)
        )
      })
      saveRDS(recipe_built,
              file.path(tmpdir,paste0('done_recipes_',file_num,'.rds')))
      unlink(file.path(tmpdir,'unfinished.rds'))
      output$recipeMessage <- renderUI({''})
      output$buildMessage <- renderUI({'Recipe saved'})
      recipe_built$ingredient <- recipe_built$`Food Name`
      recipe_built$unit <- 'g'
      calories <- as.numeric(recipe_built[nrow(recipe_built),'kcal'])
      recipe_built <- recipe_built %>% select(ingredient,quantity,unit)
      recipe_built <- recipe_built[-nrow(recipe_built),] %>% data.frame()
      allrecipes$recipes[[length(allrecipes$recipes)+1]] <-
        list(ingredients = recipe_built,
             recipe = '',
             calories = calories,
             user = TRUE)
      names(allrecipes$recipes)[[length(allrecipes$recipes)]] <- input$recipeName
    }
  })
  
  observeEvent(input$goButton, {
    output$txt <- renderUI({''})
    # Calculate the daily calorie balance
    calBalance <- rep(input$totalCalories/input$numberMeals,
                      input$numberMeals)
    meal_options <- c('Breakfast','Mid-Morning','Lunch','Dinner','Supper')
    possibleMeals <- switch(input$numberMeals,
                            c(3), c(1,4), c(1,3,4),
                            c(1,3,4,5), c(1:5))
    possibleMeals <- meal_options[possibleMeals]
    calBalance[which(possibleMeals %in% input$calorieBalance)] <-
      calBalance[1]*1.2
    calBalance <- round(calBalance*input$totalCalories/(sum(calBalance)),0)
    meals <- c(input$meal1, input$meal2, input$meal3,
               input$meal4, input$meal5)
    tables <- lapply(1:input$numberMeals, FUN = function(i){
      if(!grepl(pattern = 'Select [0-9]{1}[\\+]? meals',x = meals[i])&
         (nchar(meals[i])>0)){
        recipe <- allrecipes$recipes[[meals[i]]]
        recipe$ingredients$quantity <-
          round(recipe$ingredients$quantity*calBalance[i]/500,0)
        recipe$ingredients$quantity[recipe$ingredients$quantity>100] <-
          round(recipe$ingredients$quantity[recipe$ingredients$quantity>100]*calBalance[i]/500,-1)
        recipe$ingredients$amount <-
          paste0(recipe$ingredients$quantity,
                 recipe$ingredients$unit)
        recipe$ingredients <- recipe$ingredients %>%
          select(ingredient, amount)
        paste0(
          HTML(
            htmlTable(x = recipe$ingredients,
                      header = c("Ingredient","Quantity"),
                      rnames = rep('',nrow(recipe$ingredients)),
                      caption= possibleMeals[i],
                      css.cell = rbind(rep("background: black; font-size: 1em; text-align: left; padding-left: .5em; padding-right: .5em;", 
                                           times = ncol(recipe$ingredients)),
                                       matrix("", 
                                              ncol = ncol(recipe$ingredients), 
                                              nrow = nrow(recipe$ingredients))))),
          HTML(recipe$recipe),
          HTML('<br>')
        )
      } else {
        HTML(paste0('<br>No recipe selected for meal ',i,'<br>'))
      }
    }) %>% unlist() %>% paste0(collapse = '')
    # Create the table (using table from htmlTables doc as example)
    
    output$filetable <- renderText({tables})
  })
})