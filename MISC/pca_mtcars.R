#splitting ds
ds = mtcars %>% rownames_to_column()
ds %>% View()

#create recipe
(pca.recipe = ds %>% recipe( ~ . ) %>%
    update_role(mpg, rowname, new_role = 'id') %>%  #metadata (including the response var, mpg) that won't be used in the preprocesing steps below
    #center and scale numeric data
    step_normalize(all_predictors()) %>% 
    step_pca(all_predictors())
)

#examine interaction of recipe with train ds
(pca.prep =  pca.recipe %>% prep())

names(pca.prep)

#std deviation
sdev <- pca.prep$steps[[2]]$res$sdev # number corresponds to step index # where pca was carried out

#pct variation
percent_variation <- sdev^2 / sum(sdev^2) #needs to be expressed in variances so squaring

var_df <- data.frame(PC=paste0("PC",1:length(sdev)),
                     var_explained=percent_variation,
                     stringsAsFactors = FALSE)

var_df %>%
  mutate(PC = fct_inorder(PC)) %>%
  ggplot(aes(x=PC,y=var_explained, fill = var_explained))+geom_col()


#in tidy form
tidied.pca.prep = tidy(pca.prep, 2) # number corresponds to step index # where pca was carried out

tidied.pca.prep %>% View()

tidied.pca.prep %>%
  filter(component %in% paste0("PC", 1:2)) %>% #filter to top PCs  
  mutate(component = fct_inorder(component)) %>% #convert 'component' from char to factor
  ggplot(aes(value, terms, fill = terms)) + #set fill to unique char var to get diff colors for each bar
  geom_col(show.legend = FALSE) +
  facet_wrap(~component, nrow = 1) +
  labs(y = 'feature')


library(tidytext)

tidied.pca.prep %>%
  filter(component %in% paste0("PC", 1:2)) %>%
  group_by(component) %>%
  top_n(7, abs(value)) %>%
  ungroup() %>%
  mutate(terms = reorder_within(terms, abs(value), component)) %>%
  ggplot(aes(abs(value), terms, fill = value > 0)) + # by using a comparison operator in the fill arg, value is converted to a logical!  brilliant!
  geom_col() +
  facet_wrap(~component, scales = "free_y") +
  scale_y_reordered() +
  labs(
    x = "Absolute value of contribution",
    y = NULL, fill = "Positive?"
  ) + scale_fill_manual(values = c('darkred','darkgreen'))


juice(pca.prep) %>%
  ggplot(aes(PC1, PC2, label = paste0(rowname,': ',round(mpg,2)))) +
  geom_point(aes(color = mpg, size = mpg), alpha = 0.8) +
  geom_text(check_overlap = TRUE, hjust = "inward", family = "Calibri") +
  labs(color = NULL) + scale_color_viridis_c()
