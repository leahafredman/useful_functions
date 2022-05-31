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
             rotation = 45
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
                         color = "darkblue",
                         fill = "white",
                         size = 0.9,
                         aes(y = score_percent)) + 
                scale_y_continuous(labels = scales::number_format(suffix = "%")) + 
                #appending a percent sign to the end of the numbers on the y-axis
                theme(axis.text.x = element_text(angle = rotation, 
                                                 hjust = 1.0, 
                                                 vjust = 1.0)) +
                geom_text(aes(y = score_percent, label= paste(glue::glue("{score_count}\nparticipants\n{score_percent}%\nof\nsample"))),
                          color = "darkblue",
                          size = 3) + 
                labs(title = paste(glue::glue("{product_name} NSAT Plot
                                         {nsat} NSAT Score")),
                     subtitle = "",
                     x = "NSAT Rating",
                     y = "Percent Respondents")
        }
                
        }
        
        





        
        
        #searching for any column name in the dataframe that contains the text bootstrapped_column,
        #which should be numberic,
        #but just in case R messed up in the import converting it to numeric datatype
        if (any(grepl("bootstrapped_column", colnames(data)))) {
            data %<>%
                mutate(across(.col = everything(), .fns = ~ as.numeric(.x)))
        } else {
            #If I don't have the default boostrapped column then I have a more complex dataframe that should contain test_number and group_number columns
            #and so I need to filter the dataset to include only the combination of groups and tests that I'm interested in
            #as indicated by the test and group arguments changed from the default no
            data %<>%
          
                {
                    if (group1 == 'no')
                        filter(., group_number != 'group1')
                    else
                        .
                } %>%
                {
                    if (group2 == 'no')
                        filter(., group_number != 'group2')
                    else
                        .
                } %>%
                {
                    if (group3 == 'no')
                        filter(., group_number != 'group3')
                    else
                        .
                } %>%
                {
                    if (group4 == 'no')
                        filter(., group_number != 'group4')
                    else
                        .
                } %>%
                unite('group_names_combo', c(group_number, test_number)) %>% #aggregating the group number and test number columns into a single column named group_names_combo
                mutate(groups_column = unclass(as.factor(group_names_combo)), #retaining only the numeric representation of the names by converting the names into a factor datatype and then retaining only the numbers and dropping the class levels
                       {
                           {
                               bootstrapping_column
                           }
                       } := as.numeric({
                           {
                               bootstrapping_column
                           }
                       })) #ensuring that the column to bootstrap will be numeric
            
            #Creating a dataframe that's a mapping table for the group_names_combo column
            #so I later display results with the correct names and groups
            data_group_names <-
                data %>% #start with the original dataframe that we just modified in the logic above
                dplyr::select(groups_column, group_names_combo) %>% #select the column representing the groups in number and names
                distinct() %>% #keep only the distinct combo
                arrange(group_names_combo)
        }
        
        #Saving each group's character name
        group1_name <- {
            gsub('_', ', ', tolower(pull(
                data_group_names %>%
                    filter(groups_column == 1) %>%
                    dplyr::select(group_names_combo)
            )))
        }
        
        group2_name <- {
            gsub('_', ', ', tolower(pull(
                data_group_names %>%
                    filter(groups_column == 2) %>%
                    dplyr::select(group_names_combo)
            )))
        }
        
        if (any(grepl("3", data_group_names$groups_column))) {
            group3_name <- {
                gsub('_', ', ', tolower(
                    pull(
                        data_group_names %>%
                            filter(groups_column == 3) %>%
                            dplyr::select(group_names_combo)
                    )
                ))
            }
        }
        
        if (any(grepl("4", data_group_names$groups_column))) {
            group4_name <- {
                gsub('_', ', ', tolower(
                    pull(
                        data_group_names %>%
                            filter(groups_column == 4) %>%
                            dplyr::select(group_names_combo)
                    )
                ))
            }
        }
        
        #A function to bootstrap a statistic which is the same for all group
        #the group filtering will happen in the next step where we set up the unit to bootstrap
        function_boot_group_mean <-
            function(data = data, random) {
                d = data[random, ] #subsetting all columns but random rows
                group_mean = mean(d %>% dplyr::select({
                    {
                        bootstrapping_column
                    }
                }) %>% pull(),
                na.rm = TRUE)
                
                return(group_mean)
            }
