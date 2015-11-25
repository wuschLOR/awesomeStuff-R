library("ggplot2")


# validation
decode_and_validate<- function(vector_of_values, vector_of_possible_values, vector_of_possible_lables ){
  
  # save the origional data
  output_df <- data.frame(values_original= vector_of_values)
  
  # convert it to a factor with the right order as the vector_of_possible_lables
  # indicates. With this transformation all values not speciefied in the 
  # vector_of_possible_values are truned NA
  output_df$values_factor   <- factor(x = output_df$values_original
                                      , levels = vector_of_possible_values
                                      , labels = vector_of_possible_lables)
                                      # , ordered = TRUE)
  
  # convert to character
  output_df$values_for_math <- as.character(x = output_df$values_factor)
  # convert to numeric - everything that is not a number is turned to na so that
  # there is only a clean vector for doing math things
  suppressWarnings(
    output_df$values_for_math <- as.numeric(x = output_df$values_for_math)
    )
  return(output_df)
}


create_color_pallet <- function(vector_of_possible_values){
  # check if something says missing
  
  how_many_numbers = length(vector_of_possible_values)
  
  if (99 %in% vector_of_possible_values)
    how_many_numbers = how_many_numbers - 1
  
  plate <- scales::seq_gradient_pal("red", "green", "Lab")(seq(0,1,length.out=how_many_numbers))
  
  plate[how_many_numbers+1] <-  "#000000"
  return(plate)
}

value <- c(0,1,2,3,4,5,6,7,8,9,10,99)
label <- c('0','1','2','3','4','5','6','7','8','9','10','missing')



values = round(runif(15, min=1, max=10))
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
  
  # make the right color pallet
  color_pallet = create_color_pallet(vector_of_possible_values)
  
  
  # calculate the scale for the y axis
  scale_y_data <- nrow(decode_and_validate_df)
  scale_y_max  <- scale_y_data + 2
  
  #calculate the limits for the xaxis
  # you can use length(vector_of_possible_values) and check again for missing or not missing
  # or you can just use the length of the color_pallet (create_color_pallet did that check before)
  # +1.5 to compensate for NA
  scale_x <- c(.5, length(color_pallet)+1.5)
  
  
  gg <- ggplot(data = decode_and_validate_df) + 
    geom_bar(aes(x = values_factor, fill = values_factor))+
    coord_cartesian(xlim = scale_x) + 
    scale_x_discrete(drop = FALSE) + 
    scale_y_discrete(limits = c(1:scale_y_max)
                     , breaks = seq(0, scale_y_data, ceiling(scale_y_data/3))
    ) + 
    scale_fill_manual(values = color_pallet , na.value="#75747E")+
    geom_vline(xintercept = 11.5
               , color    = "#FFFFFF"
               , size     = 3, linetype = "solid")
  
  
  print(gg)
  return(gg)
}





  plot <- do_plot(decode_and_validate_df = data, vector_of_possible_values = value, vector_of_possible_lables = label)
  print(plot)


decode_and_validate_df$values_factor
