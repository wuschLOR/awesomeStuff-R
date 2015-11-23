library("ggplot2")


# validation
decode_and_validate_ <- function(vector_of_values, vector_of_possible_values, vector_of_possible_lables ){
  
  # save the origional data
  output_df <- data.frame(values_original= vector_of_values)
  
  # convert it to a factor with the right order as the vector_of_possible_lables
  # indicates. With this transformation all values not speciefied in the 
  # vector_of_possible_values are truned NA
  output_df$values_factor   <- factor(x = output_df$values_original
                                      , levels = vector_of_possible_values
                                      , labels = vector_of_possible_lables
                                      , ordered = TRUE)
  
  # convert to character
  output_df$values_for_math <- as.character(x = output_df$values_factor)
  # convert to numeric - everything that is not a number is turned to na so that
  # there is only a clean vector for doing math things
  output_df$values_for_math <- as.numeric(x = output_df$values_for_math)

  return(output_df)
}

value <- c(0,1,2,3,4,5,6,7,8,9,10,99)
label <- c('0','1','2','3','4','5','6','7','8','9','10','missing')



values = round(runif(100, min=1, max=10))
values[1] = NA
values[2] = 99
values[3] = 11

data <- validation(vector_of_values = values, vector_of_possible_values = value, vector_of_possible_lables = label)
head(data)



data$values_for_math <- as.character(data$values_factor)
data$values_for_math <- as.numeric(x = data$values_for_math )
head(data)



scale_y_data <- nrow(data)
scale_y_max  <- scale_y_data + 2

create_colorplate <- function(list_of_factors){
  how_many_factors=length(unique(list_of_factors))
  how_many_numbers=length(unique(as.integer(list_of_factors)))
  plate <- scales::seq_gradient_pal("red", "green", "Lab")(seq(0,1,length.out=how_many_numbers))
  plate[how_many_numbers+1] <-  "#FF00FF"
  return(plate)
}

plate = create_colorplate(list_of_factors = data$values_factor)

gg <- ggplot(data = data) + 
  geom_bar(aes(x = values_factor, fill = values_factor))+
  coord_cartesian(xlim = c(0.5,12.5)) + 
  scale_x_discrete(drop = FALSE) + 
  scale_y_discrete(limits = c(1:scale_y_max)
                   , breaks = seq(0, scale_y_data, ceiling(scale_y_data/3))
  ) + 
  scale_fill_manual(values = plate , na.value="#75747E")+
  geom_vline(xintercept = 11.5
             , color    = "#FFFFFF"
             , size     = 3, linetype = "solid")




print(gg)


