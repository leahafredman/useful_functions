function_means_bootstrap <-
    function(data = read_csv(file = here::here("csv_dataframe.csv"),
                             #dataframe name or csv name to pull in
                             show_col_types = FALSE),
             R = 10000,
             #number of bootstraps
             bootstrapping_column = bootstrapped_column,
             #column to bootstrap means from
             groups = yes,
             #is there a column titled groups_column to pull group membership from
             test1 = no,
             #if there is a test_number column, and we want to include data from the first test
             test2 = no,
             #if there is a test_number column, and we want to include data from the second test
             test3 = no,
             #if there is a test_number column, and we want to include data from the third test
             test4 = no,
             #if there is a test_number column, and we want to include data from the fourth test
             group1 = no,
             #if there is a group_number column, and we want to include data from the first group
             group2 = no,
             #if there is a group_number column, and we want to include data from the second group
             group3 = no,
             #if there is a group_number column, and we want to include data from the third group
             group4 = no,
             #if there is a group_number column, and we want to include data from the fourth group
             simple_output = yes) {
        #Simplified output or one with more details
        
        #deparse(substitute) functions turn input into a character so that users don't need to surround argument with quotation marks like yes vs "yes"
        group <- deparse(substitute(group))
        test1 <- deparse(substitute(test1))
        test2 <- deparse(substitute(test2))
        test3 <- deparse(substitute(test3))
        test4 <- deparse(substitute(test4))
        group1 <- deparse(substitute(group1))
        group2 <- deparse(substitute(group2))
        group3 <- deparse(substitute(group3))
        group4 <- deparse(substitute(group4))
        simple_output <- deparse(substitute(simple_output))
        
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
                    if (test1 == 'no')
                        filter(., test_number != 'test1')
                    else
                        .
                } %>%
                {
                    if (test2 == 'no')
                        filter(., test_number != 'test2')
                    else
                        .
                } %>%
                {
                    if (test3 == 'no')
                        filter(., test_number != 'test3')
                    else
                        .
                } %>%
                {
                    if (test4 == 'no')
                        filter(., test_number != 'test4')
                    else
                        .
                } %>%
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
        
        #A function for setting up the unit that we want bootstrapped
        #creating the boot object class that contains the output of a bootstrap calculation
        function_boot_results <-
            function(group = 1) {
                boot(
                    data = data %>% filter(groups_column == group),
                    statistic = function_boot_group_mean,
                    R = R
                )
            }
        
        #A function to to run the boostrapping function and retaining the means and their confidence intervals
        function_boot_means_ci <-
            function(boot_object = boot_results_group1) {
                boot.ci(
                    boot.out = boot_object,
                    index = 1,
                    conf = .95,
                    type = "bca"
                )
            }
        
        boot_results_group1 <- function_boot_results(group = 1)
        boot_means_ci_group1 <-
            function_boot_means_ci(boot_object = boot_results_group1)
        
        boot_results_group2 <- function_boot_results(group = 2)
        boot_means_ci_group2 <-
            function_boot_means_ci(boot_object = boot_results_group2)
        
        if (any(grepl("3", data_group_names$groups_column))) {
            boot_results_group3 <- function_boot_results(group = 3)
            boot_means_ci_group3 <-
                function_boot_means_ci(boot_object = boot_results_group3)
        }
        
        if (any(grepl("4", data_group_names$groups_column))) {
            boot_results_group4 <- function_boot_results(group = 4)
            boot_means_ci_group4 <-
                function_boot_means_ci(boot_object = boot_results_group4)
        }
        
        print(
            glue::glue(
                "
                {group1_name} group's confidence interval: {round(boot_means_ci_group1$bca[4],3)} - {round(boot_means_ci_group1$bca[5],3)}
                {group2_name} group's confidence interval: {round(boot_means_ci_group2$bca[4],3)} - {round(boot_means_ci_group2$bca[5],3)}
                "
            )
        )
        
        
        if (any(grepl("3", data_group_names$groups_column))) {
            print(
                glue::glue(
                    "
                {group3_name} group's confidence interval: {round(boot_means_ci_group3$bca[4],3)} - {round(boot_means_ci_group3$bca[5],3)}
                "
                )
            )
            
        }
        
        if (any(grepl("4", data_group_names$groups_column))) {
            print(
                glue::glue(
                    "
                {group4_name} group's confidence interval: {round(boot_means_ci_group4$bca[4],3)} - {round(boot_means_ci_group4$bca[5],3)}
                "
                )
            )
        }
        
        if ((((
            round(boot_means_ci_group1$bca[4], 3) < round(boot_means_ci_group2$bca[4], 3)
        )
        &
        (
            round(boot_means_ci_group1$bca[4], 3) < round(boot_means_ci_group2$bca[5], 3)
        ))
        &
        ((
            round(boot_means_ci_group1$bca[5], 3) < round(boot_means_ci_group2$bca[4], 3)
        )
        &
        (
            round(boot_means_ci_group1$bca[5], 3) < round(boot_means_ci_group2$bca[5], 3)
        ))) == TRUE |
        (((
            round(boot_means_ci_group1$bca[4], 3) > round(boot_means_ci_group2$bca[4], 3)
        )
        &
        (
            round(boot_means_ci_group1$bca[4], 3) > round(boot_means_ci_group2$bca[5], 3)
        ))
        &
        ((
            round(boot_means_ci_group1$bca[5], 3) > round(boot_means_ci_group2$bca[4], 3)
        )
        &
        (
            round(boot_means_ci_group1$bca[5], 3) > round(boot_means_ci_group2$bca[5], 3)
        ))) == TRUE) {
            print(
                glue::glue(
                    "The confidence intervals of {group1_name} and {group2_name} do not overlap, indicating that the groups' means differ from each other."
                )
            )
        } else{
            print(
                glue::glue(
                    "The confidence intervals of {group1_name} and {group2_name} overlap, indicating that the groups' means do not differ from each other."
                )
            )
        }
        
        if (any(grepl("3", data_group_names$groups_column))) {
            if ((((
                round(boot_means_ci_group1$bca[4], 3) < round(boot_means_ci_group3$bca[4], 3)
            )
            &
            (
                round(boot_means_ci_group1$bca[4], 3) < round(boot_means_ci_group3$bca[5], 3)
            ))
            &
            ((
                round(boot_means_ci_group1$bca[5], 3) < round(boot_means_ci_group3$bca[4], 3)
            )
            &
            (
                round(boot_means_ci_group1$bca[5], 3) < round(boot_means_ci_group3$bca[5], 3)
            ))) == TRUE |
            (((
                round(boot_means_ci_group1$bca[4], 3) > round(boot_means_ci_group3$bca[4], 3)
            )
            &
            (
                round(boot_means_ci_group1$bca[4], 3) > round(boot_means_ci_group3$bca[5], 3)
            ))
            &
            ((
                round(boot_means_ci_group1$bca[5], 3) > round(boot_means_ci_group3$bca[4], 3)
            )
            &
            (
                round(boot_means_ci_group1$bca[5], 3) > round(boot_means_ci_group3$bca[5], 3)
            ))) == TRUE) {
                print(
                    glue::glue(
                        "The confidence intervals of {group1_name} and {group3_name} do not overlap, indicating that the groups' means differ from each other."
                    )
                )
            } else{
                print(
                    glue::glue(
                        "The confidence intervals of {group1_name} and {group3_name} overlap, indicating that the groups' means do not differ from each other."
                    )
                )
            }
            
            
            if ((((
                round(boot_means_ci_group2$bca[4], 3) < round(boot_means_ci_group3$bca[4], 3)
            )
            &
            (
                round(boot_means_ci_group2$bca[4], 3) < round(boot_means_ci_group3$bca[5], 3)
            ))
            &
            ((
                round(boot_means_ci_group2$bca[5], 3) < round(boot_means_ci_group3$bca[4], 3)
            )
            &
            (
                round(boot_means_ci_group2$bca[5], 3) < round(boot_means_ci_group3$bca[5], 3)
            ))) == TRUE |
            (((
                round(boot_means_ci_group2$bca[4], 3) > round(boot_means_ci_group3$bca[4], 3)
            )
            &
            (
                round(boot_means_ci_group2$bca[4], 3) > round(boot_means_ci_group3$bca[5], 3)
            ))
            &
            ((
                round(boot_means_ci_group2$bca[5], 3) > round(boot_means_ci_group3$bca[4], 3)
            )
            &
            (
                round(boot_means_ci_group2$bca[5], 3) > round(boot_means_ci_group3$bca[5], 3)
            ))) == TRUE) {
                print(
                    glue::glue(
                        "The confidence intervals of {group2_name} and {group3_name} do not overlap, indicating that the groups' means differ from each other."
                    )
                )
            } else{
                print(
                    glue::glue(
                        "The confidence intervals of {group2_name} and {group3_name} overlap, indicating that the groups' means do not differ from each other."
                    )
                )
            }
        }
        
        
        if (any(grepl("4", data_group_names$groups_column))) {
            if ((((
                round(boot_means_ci_group1$bca[4], 3) < round(boot_means_ci_group4$bca[4], 3)
            )
            &
            (
                round(boot_means_ci_group1$bca[4], 3) < round(boot_means_ci_group4$bca[5], 3)
            ))
            &
            ((
                round(boot_means_ci_group1$bca[5], 3) < round(boot_means_ci_group4$bca[4], 3)
            )
            &
            (
                round(boot_means_ci_group1$bca[5], 3) < round(boot_means_ci_group4$bca[5], 3)
            ))) == TRUE |
            (((
                round(boot_means_ci_group1$bca[4], 3) > round(boot_means_ci_group4$bca[4], 3)
            )
            &
            (
                round(boot_means_ci_group1$bca[4], 3) > round(boot_means_ci_group4$bca[5], 3)
            ))
            &
            ((
                round(boot_means_ci_group1$bca[5], 3) > round(boot_means_ci_group4$bca[4], 3)
            )
            &
            (
                round(boot_means_ci_group1$bca[5], 3) > round(boot_means_ci_group4$bca[5], 3)
            ))) == TRUE) {
                print(
                    glue::glue(
                        "The confidence intervals of {group1_name} and {group4_name} do not overlap, indicating that the groups' means differ from each other."
                    )
                )
            } else{
                print(
                    glue::glue(
                        "The confidence intervals of {group1_name} and {group4_name} overlap, indicating that the groups' means do not differ from each other."
                    )
                )
            }
            
            
            
            if ((((
                round(boot_means_ci_group2$bca[4], 3) < round(boot_means_ci_group4$bca[4], 3)
            )
            &
            (
                round(boot_means_ci_group2$bca[4], 3) < round(boot_means_ci_group4$bca[5], 3)
            ))
            &
            ((
                round(boot_means_ci_group2$bca[5], 3) < round(boot_means_ci_group4$bca[4], 3)
            )
            &
            (
                round(boot_means_ci_group2$bca[5], 3) < round(boot_means_ci_group4$bca[5], 3)
            ))) == TRUE |
            (((
                round(boot_means_ci_group2$bca[4], 3) > round(boot_means_ci_group4$bca[4], 3)
            )
            &
            (
                round(boot_means_ci_group2$bca[4], 3) > round(boot_means_ci_group4$bca[5], 3)
            ))
            &
            ((
                round(boot_means_ci_group2$bca[5], 3) > round(boot_means_ci_group4$bca[4], 3)
            )
            &
            (
                round(boot_means_ci_group2$bca[5], 3) > round(boot_means_ci_group4$bca[5], 3)
            ))) == TRUE) {
                print(
                    glue::glue(
                        "The confidence intervals of {group2_name} and {group4_name} do not overlap, indicating that the groups' means differ from each other."
                    )
                )
            } else{
                print(
                    glue::glue(
                        "The confidence intervals of {group2_name} and {group4_name} overlap, indicating that the groups' means do not differ from each other."
                    )
                )
            }
            
            
            
            if ((((
                round(boot_means_ci_group3$bca[4], 3) < round(boot_means_ci_group4$bca[4], 3)
            )
            &
            (
                round(boot_means_ci_group3$bca[4], 3) < round(boot_means_ci_group4$bca[5], 3)
            ))
            &
            ((
                round(boot_means_ci_group3$bca[5], 3) < round(boot_means_ci_group4$bca[4], 3)
            )
            &
            (
                round(boot_means_ci_group3$bca[5], 3) < round(boot_means_ci_group4$bca[5], 3)
            ))) == TRUE |
            (((
                round(boot_means_ci_group3$bca[4], 3) > round(boot_means_ci_group4$bca[4], 3)
            )
            &
            (
                round(boot_means_ci_group3$bca[4], 3) > round(boot_means_ci_group4$bca[5], 3)
            ))
            &
            ((
                round(boot_means_ci_group3$bca[5], 3) > round(boot_means_ci_group4$bca[4], 3)
            )
            &
            (
                round(boot_means_ci_group3$bca[5], 3) > round(boot_means_ci_group4$bca[5], 3)
            ))) == TRUE) {
                print(
                    glue::glue(
                        "The confidence intervals of {group3_name} and {group4_name} do not overlap, indicating that the groups' means differ from each other."
                    )
                )
            } else{
                print(
                    glue::glue(
                        "The confidence intervals of {group3_name} and {group4_name} overlap, indicating that the groups' means do not differ from each other."
                    )
                )
            }
            
        }
        
        
        
        
    }
