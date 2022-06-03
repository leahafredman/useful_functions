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
             plotting_column_y = c(),
             #column to plot on the y-axis (if applicable). Column must be numeric data type (i.e. numbers only)
             column_x_have_na = no,
             #does the x-axis columns have any variables representing an NA answer that you want removed
             column_y_have_na = no,
             #does the y-axis columns have any variables representing an NA answer that you want removed (if applicable)
             value_1 = 1,
             value_2 = 2,
             value_3 = 3,
             value_4 = 4,
             value_5 = 5,
             value_6 = 999,
             value_7 = 9999,
             value_1_char = none,
             value_2_char = none,
             value_3_char = none,
             value_4_char = none,
             value_5_char = none,
             value_6_char = none,
             value_7_char = none,
             product_name = "Tech Product",
             y_axis_percent = yes,
             rotation = 45,
             bar_text_size = 1.1,
             main_title_text = "Main Title Goes Here",
             subtitle_text = "Subtitle Goes Here",
             x_axis_variable_name = "NSAT Score",
             y_axis_variable_name = "NSAT Module Scores"
             ){
        
        #deparse(substitute) functions turn input into a character so that users don't need to surround argument with quotation marks like yes vs "yes"
        create_barplot <- deparse(substitute(create_barplot))
        nsat_barplot <- deparse(substitute(nsat_barplot))
        create_scatterplot <- deparse(substitute(create_scatterplot))
        column_x_have_na <- deparse(substitute(column_x_have_na))
        column_y_have_na <- deparse(substitute(column_y_have_na))
        compute_nsat <- deparse(substitute(compute_nsat))
        nsat_in_title <- deparse(substitute(nsat_in_title))
        value_1_char <- deparse(substitute(value_1_char))
        value_2_char <- deparse(substitute(value_2_char))
        value_3_char <- deparse(substitute(value_3_char))
        value_4_char <- deparse(substitute(value_4_char))
        value_5_char <- deparse(substitute(value_5_char))
        value_6_char <- deparse(substitute(value_6_char))
        value_7_char <- deparse(substitute(value_7_char))
        y_axis_percent <- deparse(substitute(y_axis_percent))
        
        
        #Code to create a barplot
        if(create_barplot == "yes"){
            
            #Creating dataframe for the barplot
            data %<>%
                dplyr::select({{plotting_column_x}}) %>%
                #selecting only the column with the data the we want to plot
                rename_with(.cols = {{plotting_column_x}}, .fn = ~ as.character("xaxis_col_num")) %>%
                #renaming the numeric column to plot so that it's easier to reference it down the line
                {if(nsat_barplot == "yes") filter_all(., any_vars(. < 6 & . > 0)) else .} %>%
                #filtering if this is an nsat plot so that we only have values between 1 and 5 in our column
                mutate(xaxis_col_char = case_when(
                    xaxis_col_num == value_1 ~ gsub('"', '', glue::glue("{value_1_char}")),
                    xaxis_col_num == value_2 ~ gsub('"', '', glue::glue("{value_2_char}")),
                    xaxis_col_num == value_3 ~ gsub('"', '', glue::glue("{value_3_char}")),
                    xaxis_col_num == value_4 ~ gsub('"', '', glue::glue("{value_4_char}")),
                    xaxis_col_num == value_5 ~ gsub('"', '', glue::glue("{value_5_char}")),
                    xaxis_col_num == value_6 ~ gsub('"', '', glue::glue("{value_6_char}")),
                    xaxis_col_num == value_7 ~ gsub('"', '', glue::glue("{value_7_char}"))
                )) %>%
                group_by(xaxis_col_num) %>%
                mutate(score_count = n()) %>%
                ungroup() %>%
                mutate(score_percent = round((score_count/n())*100, 2)) %>%
                drop_na()
                
            
            if(nsat_barplot == "yes"){ #calculating the nsat
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
            }
            
            data %>% #building the bar chart
                ggplot(aes(x = xaxis_col_char)) +
                geom_bar(stat="identity",
                         position = "dodge",
                         fill = "white",
                         size = 0.9,
                         aes(y = {if((nsat_barplot == "yes") | (y_axis_percent == "yes")) score_percent
                             else score_count},
                             color = xaxis_col_char)) + 
                {if((nsat_barplot == "yes") | (y_axis_percent == "yes")) scale_y_continuous(labels = scales::number_format(suffix = "%", accuracy = 1))} +
                #scale_color_viridis(discrete = TRUE) + 
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
                {if(nsat_barplot == "yes") labs(title = paste(glue::glue("{product_name} NSAT Plot
                                         {nsat} NSAT Score")),
                     subtitle = "",
                     x = "NSAT Rating",
                     y = "Percent Respondents")} + 
                {if(nsat_barplot != "yes") labs(title = glue::glue("{main_title_text}"),
                    subtitle = glue::glue("{subtitle_text}"),
                    x = "Rating",
                    y = {if((nsat_barplot == "yes") | (y_axis_percent == "yes")) "Percent Respondents" else "Number of Respondents"})}
        }
        
        
        #Code to create a scatterplot
        if(create_scatterplot != "no"){
            
            #Creating dataframe for the scatterplot
            data %<>%
                dplyr::select({{plotting_column_x}}, any_of(plotting_column_y)) %>%
                pivot_longer(cols = plotting_column_y) %>%
                drop_na()
            
            data %>% #building the bar chart
                ggplot(aes(x = {{plotting_column_x}},
                           y = value,
                           groups = name)) +
                geom_jitter(aes(color = name, shape = name),
                            stat = "identity",
                    position = "jitter",
                    alpha = .5) +
                facet_grid(cols = vars(name), scales = "free") +
                theme(axis.text.x = element_text(angle = rotation,
                                                 face = "bold"), 
                      legend.position="none") +
                labs(title = glue::glue("{main_title_text}"),
                                                subtitle = glue::glue("{subtitle_text}"),
                                                x = glue::glue("{x_axis_variable_name}"),
                                                y = glue::glue("{y_axis_variable_name}"))
        }
            
        }
                
    


function_barplot_scatterplot(
    value_1_char = "1) Very Dissatisfied",
                             value_2_char = "2) Somewhat Dissatisfied",
                             value_3_char = "3) Neither Satisfied Nor Dissatisfied",
                             value_4_char = "4) Somewhat Satisfied",
                             value_5_char = "5) Very Satisfied",
    nsat_barplot = no,
    y_axis_percent == "yes"
    )

function_barplot_scatterplot(

    create_scatterplot = yes,
    create_barplot = no,
    plotting_column_y = c('values1', 'values2', 'values3', 'values4')
)

  