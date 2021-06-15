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

