#packrat::on()
package.list <- c('shiny','shinyBS','htmlTable','shinyjs','shinythemes','stringr','dplyr')
new.packages <- package.list[!(package.list %in% installed.packages()[,"Package"])]
if(length(new.packages)) lapply(new.packages, install.packages)
success <- all(unlist(lapply(package.list, require, character.only = TRUE)))
source('recipes.R')

shinyUI(
  navbarPage(theme = shinythemes::shinytheme('sandstone'),
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
                                      'Extremely active (very hard exercise/sports & a physical job)')),
              HTML('<br>'),
              sliderInput(inputId = 'totalCalories', label = 'Total Daily Calories',
                          min = 1000, max = 4000, value = 2106)
            ),
            tabPanel(id = 'menuChoices', title = 'Meals',
              HTML('<br>'),
              selectizeInput(inputId = 'meal1',label = 'Breakfast'
                            ,choices = names(allrecipes)
                            ,selected = names(allrecipes)[1]
                            #,options = list(
                            #  placeholder = 'Please search here',
                            #  onInitialize = I('function() { this.setValue(""); }'))
                            ),
              selectizeInput(inputId = 'meal2',label = 'Lunch'
                            ,choices = names(allrecipes)
                            ,selected = names(allrecipes)[2]
                            #,options = list(
                            #  placeholder = 'Please search here',
                            #  onInitialize = I('function() { this.setValue(""); }'))
                            ),
              selectizeInput(inputId = 'meal3',label = 'Dinner'
                            ,choices = names(allrecipes)
                            ,selected = names(allrecipes)[3]
                            #,options = list(
                            #  placeholder = 'Please search here',
                            #  onInitialize = I('function() { this.setValue(""); }'))
                            )
            ),
            tabPanel(id = 'addFoods', title = 'Adjust My Day',
              HTML('<br>'),
              h4('Meals Eaten or Skipped'),
              fluidRow(column(6,
              checkboxGroupInput(inputId = 'eatenMeals',
                                 label = 'Eaten Meals',
                                 choices = c('Breakfast','Lunch','Dinner'))),
                       column(6,
              checkboxGroupInput(inputId = 'skippedMeals',
                                 label = 'Skipped Meals',
                                 choices = c('Breakfast','Lunch','Dinner')))),
              h4('Extra Food Eaten'),
              selectizeInput(inputId = 'selector',label = 'Select Ingredient',
                             choices = macros %>% pull(`Food Name`),
                             options = list(
                               placeholder = 'Please search here',
                               onInitialize = I('function() { this.setValue(""); }')
                             )),
              numericInput(inputId = 'selectorQuantity',
                           label = 'Quantity, in grams',
                           value = 0),
              fluidRow(column(6,
              actionButton(inputId = 'addFood', label = 'Add Item')),
                       column(6,
              actionButton(inputId = 'clearIngs', label = 'Clear Added Items'))),
              uiOutput(HTML('recipeMessage')),
              textInput(inputId = 'newFoodstuff', label = 'Manually Enter Cals',
                        value = '', placeholder = 'Enter Food'),
              numericInput(inputId = 'foodstuffCal',
                           label = 'Calories (in kcal)',
                           value = 0),
              fluidRow(column(6,
              actionButton(inputId = 'addFoodstuff', label = 'Add Food')),
                       column(6,
              actionButton(inputId = 'clearFoods', label = 'Clear Added Foods'))),
              uiOutput(HTML('foodstuffMessage'))
            )
          )
        ),
        position = "right"
      )
    )
  )
)