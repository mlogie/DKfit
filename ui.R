#packrat::on()
package.list <- c('shiny','shinyBS','htmlTable','shinyjs','shinythemes','stringr','dplyr')
new.packages <- package.list[!(package.list %in% installed.packages()[,"Package"])]
if(length(new.packages)) lapply(new.packages, install.packages)
success <- lapply(package.list, require, character.only = TRUE) %>%
  unlist() %>% all()
source('recipes.R')
macros <- readRDS('macros.rds')
tmpdir <- tempdir()

shinyUI(
  navbarPage(theme = shinythemes::shinytheme('superhero'),
             'DK Fitness Menu App', id='nav',
    tabPanel('Menus',
      sidebarLayout(
        mainPanel(
          h4('Your Personal Recipes'),
          uiOutput(HTML('txt')),
          uiOutput(HTML("filetable"))
        ),
        sidebarPanel(id = 'controls', class = 'panel panel-default',
                     fixed = TRUE,
          tabsetPanel(
            tabPanel(id = 'personalInputs', title = 'Calculator',
              h4('Calorie Calculator'),
              uiOutput(HTML('calCalc')),
              HTML('<br>'),
              numericInput(inputId = 'weight', label = 'Weight (kg)',
                           value = 80),
              numericInput(inputId = 'height', label = 'Height (cm)',
                           value = 180),
              numericInput(inputId = 'age', label = 'Age',
                           value = 35),
              radioButtons(inputId = 'malefemale', label = 'Sex',
                           choices = c('Female','Male'),
                           selected = 'Male'),
              radioButtons(inputId = 'exerciseDaily',
                           label = 'Activity Level',
                          choices = c('Sedentary (little or no exercise)',
                                      'Lightly active (light exercise/sports 1-3 days/week)',
                                      'Moderately active (moderate exercise/sports 3-5 days/week)',
                                      'Very active (hard exercise/sports 6-7 days a week)',
                                      'Extremely active (very hard exercise/sports & a physical job)'))
            ),
            tabPanel(id = 'mealChoices', title = 'Options',
              HTML('<br>'),
              sliderInput(inputId = 'totalCalories', label = 'Total Daily Calories',
                          min = 1000, max = 4000, value = 2500),
              sliderInput(inputId = 'numberMeals', label = 'Number of Meals',
                          min = 2, max = 5, value = 3, ticks = FALSE),
              checkboxGroupInput(inputId = 'calorieBalance',
                                 label = 'Big Meal',
                                 choices = c('Breakfast','Lunch','Dinner'))
            ),
            tabPanel(id = 'menuChoices', title = 'Meals',
              HTML('<br>'),
              actionButton("goButton", "Create Recipes"),
              HTML('<br><br>'),
              selectizeInput(inputId = 'meal1',label = 'Breakfast',
                            choices = names(allrecipes),
                            options = list(
                              placeholder = 'Please search here',
                              onInitialize = I('function() { this.setValue(""); }')
                            )),
              selectizeInput(inputId = 'meal2',label = 'Lunch',
                            choices = names(allrecipes),
                            options = list(
                              placeholder = 'Please search here',
                              onInitialize = I('function() { this.setValue(""); }')
                            )),
              selectizeInput(inputId = 'meal3',label = 'Dinner',
                            choices = names(allrecipes),
                            options = list(
                              placeholder = 'Please search here',
                              onInitialize = I('function() { this.setValue(""); }')
                            )),
              selectizeInput(inputId = 'meal4',label = '',
                            choices = c('Select 4+ meals...'),
                            selected = 'Select 4+ meals...'),
              selectizeInput(inputId = 'meal5',label = '',
                            choices = c('Select 5 meals...'),
                            selected = 'Select 5 meals...')
            ),
            tabPanel(id = 'addFoods', title = 'Add Foods',
              HTML('<br>'),
              h4('Build Recipe'),
              selectizeInput(inputId = 'selector',label = 'Select Ingredient',
                             choices = macros %>% pull(`Food Name`),
                             options = list(
                               placeholder = 'Please search here',
                               onInitialize = I('function() { this.setValue(""); }')
                             )),
              numericInput(inputId = 'selectorQuantity',
                           label = 'Quantity, in grams',
                           value = 0),
              actionButton(inputId = 'addIngredient', label = 'Add Ingredient'),
              uiOutput(HTML('recipeMessage')),
              actionButton(inputId = 'clearRecipe', label = 'Clear Recipe'),
              textInput(inputId = 'recipeName', label = 'Recipe Name'),
              actionButton(inputId = 'buildRecipe', label = 'Build Recipe'),
              uiOutput(HTML('buildMessage')),
              actionButton(inputId = 'clearAllRecipes', label = 'Clear All Recipes'))
          )
        ),
        position = "right"
      )
    )
  )
)