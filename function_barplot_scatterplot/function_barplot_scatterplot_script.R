function_barplot_scatterplot <-
    function(data = read_csv(file = here::here("csv_dataframe2.csv"),
                             #dataframe name or csv name to pull in
                             show_col_types = FALSE),
             create_barplot = yes,
             #whether you want to plot a barplot. Default is yes
             nsat_barplot = yes,
             #should the barplot be of nsat scores
             create_scatterplot = no,
             #whether you want to plot a barplot. Default is no
             plotting_column_x = values,
             #column to plot on the x-axis. Column must be numeric data type (i.e. numbers only)
             plotting_column_y = xaxis,
             #column to plot on the y-axis (if applicable). Column must be numeric data type (i.e. numbers only)
             column_x_have_na = no,
             #does the x-axis columns have any variables representing an NA answer that you want removed
             column_y_have_na = no,
             #does the y-axis columns have any variables representing an NA answer that you want removed (if applicable)
             compute_nsat = no,
             #whether you want to compute the nsat for your data. Default is no. Using this will assume that you have a 1 to 5 nsat scale
             nsat_in_title = no,
             #if you chose to compute the nsat, do you want to insert it in the plot's title. Default is no. Won't do anything if you select no for the above compute_nsat option
             product_name = "Tech Product",
             rotation = 45,
             bar_text_size = 1.1
             ){
        
        #deparse(substitute) functions turn input into a character so that users don't need to surround argument with quotation marks like yes vs "yes"
        create_barplot <- deparse(substitute(create_barplot))
        nsat_barplot <- deparse(substitute(nsat_barplot))
        create_scatterplot <- deparse(substitute(create_scatterplot))
        plotting_column_x <- deparse(substitute(plotting_column_x))
        plotting_column_y <- deparse(substitute(plotting_column_y))
        column_x_have_na <- deparse(substitute(column_x_have_na))
        column_y_have_na <- deparse(substitute(column_y_have_na))
        compute_nsat <- deparse(substitute(compute_nsat))
        nsat_in_title <- deparse(substitute(nsat_in_title))
        
        
        #Code to create a barplot
        if(create_barplot == "yes" & nsat_barplot == "yes"){
            
            #Creating dataframe for the barplot
            data %<>%
                dplyr::select(contains(plotting_column_x)) %>%
                #selecting only the column with the data the we want to plot
                rename_with(.cols = contains(plotting_column_x), .fn = ~ as.character("xaxis_col_num")) %>%
                #renaming the numeric column to plot so that it's easier to reference it down the line
                filter_all(any_vars(. < 6 & . > 0)) %>%
                #filtering so that we only have values between 1 and 5 in our column
                mutate(xaxis_col_char = case_when(
                    xaxis_col_num == 1 ~ "1) Very Dissatisfied",
                    xaxis_col_num == 2 ~ "2) Somewhat Dissatisfied",
                    xaxis_col_num == 3 ~ "3) Neither Satisfied Nor Dissatisfied",
                    xaxis_col_num == 4 ~ "4) Somewhat Satisfied",
                    xaxis_col_num == 5 ~ "5) Very Satisfied"
                )) %>%
                group_by(xaxis_col_char) %>%
                mutate(score_count = n()) %>%
                ungroup() %>%
                mutate(score_percent = round((score_count/n())*100, 2)) 
                
            
            n_satisfied <- sum(data$xaxis_col_num == 4) + sum(data$xaxis_col_num == 5)
            
            n_dissatisfied <- sum(data$xaxis_col_num == 1) + sum(data$xaxis_col_num == 2)
            
            n_other <- sum(data$xaxis_col_num == 3)
            
            n_total <- n_satisfied + n_dissatisfied + n_other
            
            #NSAT measuremeant based on the calculations in https://measuringu.com/microsoft-nsat/
            #I'm assuming a 5 point scale because I think that an odd number of Likert points is almost always the way to go
            #If you have a 4 point scale just make sure that your dissatisfied answers are labeled a 1 or 2
            #and your satisfied labels are 4 or 5, and skip 3 and you'll be fine
            
            p_satisfied <- round(n_satisfied / n_total, 2)
            
            p_dissatisfied <- round(n_dissatisfied / n_total, 2)
            
            p_other <- round(n_other / n_total, 2)
            
            #because we're adding 100 to the score the final nsat score will be between 100 and 200
            nsat <- round(p_satisfied - p_dissatisfied + 100, 0)
            
            data %>%
                ggplot(aes(x = xaxis_col_char)) +
                geom_bar(stat="identity",
                         position = "dodge",
                         fill = "white",
                         size = 0.9,
                         aes(y = score_percent,
                             color = xaxis_col_char)) + 
                scale_y_continuous(labels = scales::number_format(suffix = "%")) +
                scale_color_viridis(discrete = TRUE) + 
                #appending a percent sign to the end of the numbers on the y-axis
                theme(axis.text.x = element_text(angle = rotation, 
                                                 hjust = 1.0, 
                                                 vjust = 1.0,
                                                 face = "bold"), 
                      legend.position="none") +
                geom_text(aes(y = score_percent,
                              color = xaxis_col_char, 
                              vjust = bar_text_size,
                              label= paste(glue::glue("{score_count}\nparticipants\n{score_percent}%\nof\nsample"))),
                          size = 3) + 
                labs(title = paste(glue::glue("{product_name} NSAT Plot
                                         {nsat} NSAT Score")),
                     subtitle = "",
                     x = "NSAT Rating",
                     y = "Percent Respondents")
        }
                
        }
  