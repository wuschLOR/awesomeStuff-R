## Startup #####################################################################
# clear workspace
rm(list = ls())

# Libraries!
library("ggplot2")
library('scales')



## functions ###################################################################
# validation
decode_and_validate<- function(vector_of_values, vector_of_possible_values, vector_of_possible_lables ){
  
  # save the original data
  output_df <- data.frame(values_original= vector_of_values)
  
  # convert it to a factor with the right order as the vector_of_possible_lables
  # indicates. With this transformation all values not specified in the 
  # vector_of_possible_values are turned NA
  output_df$values_factor   <- factor(x = output_df$values_original
                                      , levels = vector_of_possible_values
                                      , labels = vector_of_possible_lables)
                                      # , ordered = TRUE)
  
  # convert to character
  output_df$values_for_math <- as.character(x = output_df$values_factor)
  # convert to numeric - everything that is not a number is turned to NA so that
  # there is only a clean vector for doing math things
  suppressWarnings(
    output_df$values_for_math <- as.numeric(x = output_df$values_for_math)
    )
  return(output_df)
}


create_color_pallet <- function(vector_of_possible_values){
# bob ross wants to create a bar plot with lots of happy little bars.
  
  # herefore he needs to know how many different happy bars should be there
  how_many_numbers = length(vector_of_possible_values)
  
  # one of the little bars doesn't want to be there all the time he checks if 
  # the grumpy missing value wants to be in this picture
  if (99 %in% vector_of_possible_values){
    how_many_numbers = how_many_numbers - 1
  }
  
  # every happy little bar wants to be painted in a different color because 
  # every bar is different but equally important. Here the happy little bars want 
  # to be drawn either green or red.
  happy_little_bars_colors <- scales::seq_gradient_pal("blue", "green", "Lab")(seq(0,1,length.out=how_many_numbers))
  
  # oh dear lets check again if grumpy missing value bar wants to be in our 
  # picture. If he wants to be here lets assign the grumpy missing value a 
  # different color because he's very special
  if (99 %in% vector_of_possible_values)
    happy_little_bars_colors[how_many_numbers+1] <-  "#000000"
  
  
  # now bob is taking his small detail brush dips it into some color and is 
  # ready to create all those little happy bars. He even thinks about the the 
  # NA bar that always gets sad because it only there when somebody didn't want
  # to share his thoughts and rather kept his hopes and dreams by himself.
  # Also if a happy little bar doesn't want to be there, he just paints the 
  # happy little bars with the color he promised to paint them
  twleve_inch_brush <- scale_fill_manual(values = happy_little_bars_colors , na.value="#75747E" ,drop=F)
  
  return(twleve_inch_brush)
}


seq_containing_min_and_max <- function(min, max, breaks){
# creates a seqence like this:
# seq_containing_min_and_max(min = 0,max = 10, breaks = 2)
# [1] 0  3  7 10
# breaking points will be rounded to generate full numbers
# 
# careful when using small values:
#     seq_containing_min_and_max(min = 0,max = 1, breaks = 2)
#     [1] 0 0 1 1
#     seq_containing_min_and_max(min = 0,max = 2, breaks = 2)
#     [1] 0 1 1 2
#     seq_containing_min_and_max(min = 0,max = 3, breaks = 2)
#     [1] 0 1 2 3
#     seq_containing_min_and_max(min = 0,max = 4, breaks = 2)
#     [1] 0 1 3 4
#     seq_containing_min_and_max(min = 0,max = 5, breaks = 2)
#     [1] 0 2 3 5
    
    # create normal sequence (min is allways there) wit one break more because 
    # min counts as breakingpoint
    SEQENCE <- seq(min, max,max/(breaks+1))

    # add  the max point to the sequence
    SEQENCE[breaks+2] <-max
    
    # round everything to get rid of decimal
    SEQENCE <- round(SEQENCE)
    
    return(SEQENCE)
}


# for (i in 1:9){
#     print(seq_containing_min_and_max(min = 0,max = i, breaks = 2))
# }

## promo code ##################################################################
value <- c(0,1,2,3,4,5,6,7,8,9,10,99)
label <- c('0','1','2','3','4','5','6','7','8','9','10','missing')



values = round(runif(10, min=0, max=11))
values[1] = NA
values[2] = 99
values[3] = 11

data <- decode_and_validate(vector_of_values = values, vector_of_possible_values = value, vector_of_possible_lables = label)
head(data)



# data$values_for_math <- as.character(data$values_factor)
# data$values_for_math <- as.numeric(x = data$values_for_math)
# head(data)

decode_and_validate_df = data
vector_of_possible_values = value
vector_of_possible_lables = label


do_plot <- function(decode_and_validate_df, vector_of_possible_values, vector_of_possible_lables){
  # bob ross wants to create a beautiful little picture with lots of 
  # happy little bars  
  
  # first he needs to know how big his canvas has to be. 
  # The width of the painting will be determined by the possible happy bars
  # he can use length(vector_of_possible_values) to create his guides
  # bob also compensates for the NA bar +1.5 
  scale_x <- c(0.5, length(vector_of_possible_values) + 1.5)

  xintercept_line_position <- length(vector_of_possible_values) + .5
  if (99 %in% vector_of_possible_values){
      xintercept_line_position = xintercept_line_position - 1
  }
  
  # getting to know the height of the painting is a little harder. 
  scale_y_datarows <- nrow(decode_and_validate_df)
  scale_y_max  <- scale_y_datarows *1.2 # to get space for tiangle stuff
  
  
  
  
  # also bob has to get to know all the bars who want to be on this picture and
  # how they want to be drawn 
  scale_fill_right = create_color_pallet(vector_of_possible_values)

  
  

  
  
  gg <- ggplot(data = decode_and_validate_df) + 
    geom_vline(xintercept = xintercept_line_position
             , color    = "#FFFFFF"
             , size     = 3
             , linetype = "solid")+
    geom_bar(aes(x = values_factor, fill = values_factor))+
    coord_cartesian(xlim = scale_x) + 
    scale_x_discrete(drop = FALSE) + 
    scale_y_discrete(limits = c(1:scale_y_max)
                     , breaks = seq_containing_min_and_max(min = 0,max = scale_y_datarows, breaks = 1)
    ) + 
    scale_fill_right

  
  
  print(gg)
  return(gg)
}





  plot <- do_plot(decode_and_validate_df = data, vector_of_possible_values = value, vector_of_possible_lables = label)
  print(plot)


decode_and_validate_df$values_factor

