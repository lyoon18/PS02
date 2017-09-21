
```{r}
# Taking out random sample to test our spline model
test1 <- train[sample(1:nrow(train), nrow(train)*.2,
  	replace=FALSE),]
# Removing testing part from training part
train1 <- anti_join(train, test1, by = "id")
```

Now I want to create a function that will take in all continuous variables, then run a spline on each.  The variable that works the best will be used in the model.  

```{r, warning=FALSE}
clas <- data_frame(class = sapply(train, class), name = names(train), min = sapply(train, min, na.rm=TRUE), max = as.numeric(sapply(train, max, na.rm=TRUE))) %>% filter(class == "integer", name!="id", max>50)


spleen <- function(var){
  ### Take training data and filter out missing values
  data <- train1%>%select_(var, "price_doc")
  colnames(data) <- c("var", "price_doc")
  data <- data %>% filter(!is.na(var))
  mspline <- smooth.spline(data$var, data$price_doc, cv=TRUE) # create spline model

  ### Predict test price with spline model
  newX <- test1%>%select_("id", var, "price_doc")
  colnames(newX) <- c("id","var", "price_doc")
  newX <- newX %>% filter(!is.na(var))
  output <- predict(mspline, newX$var) %>% 
    tibble::as.tibble()

  ### Compare predicted price with actual price.  Sum total RMS error
  combined <- data_frame(id = newX$id, price_new = output$y, price_old = newX$price_doc)
  combined <- combined %>% mutate(SE = (log(price_new+1)-log(price_old+1))^2) %>% summarise(RMSE = sqrt(mean(SE)), n = n(), variable = var, df = mspline$df)

  ### Add this to scored dataset
  return(combined)
}

scores <- sapply(clas$name[c(1:22, 38:47, 51:55,58:71, 73:76, 78:82, 84:122)], spleen)

df <- data.frame(matrix(unlist(scores), nrow=99, byrow=T),stringsAsFactors=FALSE) %>% arrange(X1)
head(df)
```


# Create Submission File

```{r}
mspline <- with(train1, smooth.spline(full_sq, price_doc, cv=TRUE))
newX <- test%>%filter(!is.na(full_sq))%>%select(full_sq)
output <- predict(mspline, newX$full_sq) %>% 
  tibble::as.tibble()
submission <- data_frame(id = test$id, price_doc = output$y)
write_csv(submission, "submission.csv")
```

