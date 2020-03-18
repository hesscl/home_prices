#look at proportion NA among the features
train %>%
  gather(key = "variable", value = "value", -Id) %>%
  group_by(variable) %>%
  summarize(n_na = sum(is.na(value)),
            n = n()) %>%
  mutate(prop_na = n_na/n) %>%
  arrange(desc(prop_na))

#look at table of each character variable
for(col in train %>% select_if(is.character) %>% colnames){
  cat(col)
  print(table(train[[col]]))
  cat("\n")
}

#look at table of each factor variable
for(col in train %>% select_if(is.factor) %>% colnames){
  cat(col)
  print(table(train[[col]]))
  cat("\n")
}

#look at the distribution of numeric features
train %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()
