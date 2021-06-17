numberMeals <- 3
saveRDS(list(recipetable = '',
             extrafoodtable = NULL),
        'tables.rds')

getCalBalance <- function(totalCalories, eatenMeals){
  calBalance <- rep(totalCalories/numberMeals,
                    numberMeals)
  tables <- readRDS('tables.rds')
  if(!is.null(tables$extrafoodtable)){
    extraCals <- sum(tables$extrafoodtable$kCals_)
    eaten <- c('Breakfast','Lunch','Dinner') %in% eatenMeals
    eatenCals <- extraCals + sum(as.numeric(calBalance[eaten]))
    calBalance[!eaten] <- as.numeric(calBalance[!eaten])-extraCals/sum(!eaten)
    calBalance[calBalance<200] <- 200
  } else {
    extraCals <- 0
  }
  data.frame(calBalance = calBalance, extraCals = extraCals)
}

makerecipetable <- function(i, meals, allrecipes, calBalance){
  recipetable <- lapply(1:numberMeals, FUN = function(i){
    if(nchar(meals[i])>0){
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
      recipe$recipe <- paste0(recipe$recipe,'<br>Calories = ',
                              10*round(calBalance[i]/10,0))
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
  tables <- readRDS('tables.rds')
  tables$recipetable <- recipetable
  saveRDS(tables, 'tables.rds')
  if(is.null(tables$extrafoodtable)){
    extrafood <- ''
  } else {
    extrafood <- paste0('<br>Extra Food<br>',
                        HTML(htmlTable(x = tables$extrafoodtable)))
  }
  outputtable <- paste0(tables$recipetable,
                        extrafood)
  outputtable
}

allrecipes <- list(
  `Steak and Eggs` = list(
    ingredients =
      data.frame(ingredient = c('Sirloin Steak','Olive Oil','Spinach','Eggs'),
                 quantity = c(170,2,80,3),
                 unit = c('g','tsp','g',' eggs')),
    recipe =
      'Fry steak in the oil, serve with poached eggs and steamed spinach',
    calories = 500, user = FALSE),
  `Steak Salad` = list(
    ingredients =
      data.frame(ingredient = c('Sirloin Steak','Avocado','Mixed Salad'),
                 quantity = c(230,70,200),
                 unit = c('g','g','g')),
    recipe =
      'Fry steak in a pan, and serve with sliced avocado and salad.',
    calories = 500, user = FALSE),
  `Chicken Kebab` = list(
    ingredients =
      data.frame(ingredient = c('Chicken Breast','Olive Oil','Sour Cream','Mixed Salad'),
                 quantity = c(170,2,2,180),
                 unit = c('g','tsp','tsp','g')),
    recipe =
      'Cut chicken into pieces and skewer.  Coat in sour cream and grill until cooked through, approximately 10-15 minutes. Serve with mixed salad dressed with oil.',
    calories = 500, user = FALSE),
  `Avocado Shake` = list(
    ingredients =
      data.frame(ingredient = c('Avocado','Protein Powder'),
                 quantity = c(140,60),
                 unit = c('g','g')),
    recipe =
      'Blend avocado with protein powder.',
    calories = 500, user = FALSE),
  `Salmon and Eggs` = list(
    ingredients =
      data.frame(ingredient = c('Salmon steaks','Avocado','Eggs'),
                 quantity = c(110,70,3),
                 unit = c('g','g',' eggs')),
    recipe =
      'Fry steaks in a pan, poach the eggs and serve with sliced avocado.',
    calories = 500, user = FALSE),
  'Chicken, veg and pinenuts' = list(
    ingredients =
      data.frame(ingredient = c('Pine Nuts','Broccoli','Asparagus','Olive Oil','Chicken Breast'),
                 quantity = c(22,90,85,2,170),
                 unit = c('g','g','g','tsp','g')),
    recipe =
      paste0('Thinly slice chicken and fry in olive oil on medium heat until cooked, approximately 10 minutes.  Add pinenuts for the last minute to lightly toast. ',
             'Meanwhile, steam the broccoli and asparagus and serve with chicken and pine nuts.'),
    calories = 500, user = FALSE)
  )

