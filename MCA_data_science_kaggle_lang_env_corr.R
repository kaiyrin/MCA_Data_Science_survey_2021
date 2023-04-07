#LIBRARY
library(dplyr)
library(tidytext)
library(readr)
library(cld2)
library(textcat)
library(stringr)
library(SnowballC)
library(ClusterR)
library(cluster)
library(tm)   # For text preprocessing
library(ggplot2)  # For data visualization
library(lubridate)
library(FactoMineR)
library(ggplot2)
library(factoextra)
library(tidyr)
library(tidyverse)
library(FactoMineR) #for mca
library(factoextra) #for mca

#DATASET
test <- kaggle_survey_2021_responses_csv

#CLEANING DATA
head(test[2:25974, 2:50], 3)
test<-test[2:25974, c(2:52,72)]
t_q7<-unite(test, Q7, c(Q7_Part_1, Q7_Part_2,Q7_Part_3,Q7_Part_4,Q7_Part_5,
                        Q7_Part_6,Q7_Part_7,Q7_Part_8,Q7_Part_9,Q7_Part_10,Q7_Part_11,Q7_Part_12,
                        Q7_OTHER), sep = ", ",remove = TRUE,na.rm = TRUE)
t_q9<-unite(test, Q9, c(Q9_Part_1, Q9_Part_2,Q9_Part_3,Q9_Part_4,Q9_Part_5,
                        Q9_Part_6,Q9_Part_7,Q9_Part_8,Q9_Part_9,Q9_Part_10,Q9_Part_11,Q9_Part_12,
                        Q9_OTHER), sep = ", ",remove = TRUE,na.rm = TRUE)
t_q10<-unite(test, Q10, c(Q10_Part_1, Q10_Part_2,Q10_Part_3,Q10_Part_4,Q10_Part_5,
                          Q10_Part_6,Q10_Part_7,Q10_Part_8,Q10_Part_9,Q10_Part_10,Q10_Part_11,Q10_Part_12,
                          Q10_Part_13,Q10_Part_14,Q10_Part_15,Q10_Part_16,Q10_OTHER), 
             sep = ", ",remove = TRUE,na.rm = TRUE)

df <- data.frame(test$Q1,test$Q2,test$Q3,test$Q4,test$Q5,test$Q6,t_q7$Q7,test$Q8,
                 t_q9$Q9,t_q10$Q10)
df1 <- setNames(df, c("age","gender","country","education","role","experience",
                      "main_lang","rec_lang","ide","notebook"))

#FACTORIZING THE DATA TO CATEGORIES
test$Q1 <- factor(test$Q1)
test$Q2 <- factor(test$Q2)
test$Q3 <- factor(test$Q3)
test$Q4 <- factor(test$Q4)
test$Q5 <- factor(test$Q5)
test$Q6 <- factor(test$Q6)
test$l_python <- grepl("Python", test$Q7_Part_1)
test$l_r <- grepl("R", test$Q7_Part_2)
test$l_sql <- grepl("SQL", test$Q7_Part_3)
test$l_c <- grepl("C", test$Q7_Part_4)
test$l_cpp <- grepl("C", test$Q7_Part_5)
test$l_java <- grepl("Java", test$Q7_Part_6)
test$l_js <- grepl("Javascript", test$Q7_Part_7)
test$l_jul <- grepl("Julia", test$Q7_Part_8)
test$l_swift <- grepl("Swift", test$Q7_Part_9)
test$l_bash <- grepl("Bash", test$Q7_Part_10)
test$l_matlab <- grepl("MATLAB", test$Q7_Part_11)
test$l_none <- grepl("None", test$Q7_Part_12)
test$l_other <- grepl("Other", test$Q7_OTHER)
test$Q8 <- factor(test$Q8)
test$n_1 <- grepl("Jupyter (JupyterLab, Jupyter Notebooks, etc)", test$Q9_Part_1)
test$n_2 <- grepl("RStudio", test$Q9_Part_2)
test$n_3 <- grepl("Visual Studio", test$Q9_Part_3)
test$n_4 <- grepl("Visual Studio Code (VSCode)", test$Q9_Part_4)
test$n_5 <- grepl("PyCharm", test$Q9_Part_5)
test$n_6 <- grepl("Spyder", test$Q9_Part_6)
test$n_7 <- grepl("Notepad%/", test$Q9_Part_7)
test$n_8 <- grepl("Sublime Text", test$Q9_Part_8)
test$n_9 <- grepl("Vim / Emacs", test$Q9_Part_9)
test$n_10 <- grepl("MATLAB", test$Q9_Part_10)
test$n_11 <- grepl("Jupyter Notebook", test$Q9_Part_11)
test$n_12 <- grepl("None", test$Q9_Part_12)
test$n_13 <- grepl("Other", test$Q9_OTHER)
test$Q11 <- factor(test$Q11)
test$Q15 <- factor(test$Q15)
test<-test[,c(1:6,8,53:78,51,52)]
test$l_python <- factor(test$l_python )
test$l_r <- factor(test$l_r)
test$l_sql <- factor(test$l_sql)
test$l_c <- factor(test$l_c )
test$l_cpp <- factor(test$l_cpp)
test$l_java <-factor(test$l_java)
test$l_js <- factor(test$l_js)
test$l_jul <- factor(test$l_jul)
test$l_swift <- factor(test$l_swift)
test$l_bash <- factor(test$l_bash)
test$l_matlab <-factor(test$l_matlab)
test$l_none <- factor(test$l_none)
test$l_other <- factor(test$l_other)
test$n_1 <-  factor(test$n_1)
test$n_2 <-  factor(test$n_2)
test$n_3 <-  factor(test$n_3)
test$n_4 <-  factor(test$n_4)
test$n_5 <-  factor(test$n_5)
test$n_6 <-  factor(test$n_6)
test$n_7 <-  factor(test$n_7)
test$n_8 <-  factor(test$n_8)
test$n_9 <-  factor(test$n_9)
test$n_10 <-  factor(test$n_10)
test$n_11 <-  factor(test$n_11)
test$n_12 <-  factor(test$n_12)
test$n_13 <-  factor(test$n_13)
class(test$l_other)
summary(test)
#frequency
for (i in 23) {
  plot(test[,i], main=colnames(test)[i],
       ylab = "Count", col="steelblue", las = 2)
}
#MCA
cats = apply(test, 2, function(x) nlevels(as.factor(x)))
cats
rmNArows<-function(d){
  goodRows<-apply(d,1,function(x) sum(is.na(x))!=ncol(d))
  d[goodRows,]
}
rmNArows(test)
dataframe<-na.omit(test)
#SPECIFIC DATAFRAMES TO WORK ON
languages<-test[,c(8:20)]
notebook<-test[,c(21:33)]
general<-test[,c(1:6,34,35)]

#MCA OVERALL
MCA(test, ncp = 4, graph = TRUE)

#MCA for general CATEGORICAL DUMMY VARIABLES (Multi-way contingency table)
res<-MCA(test, graph = FALSE)
res #circle of coorelation
head(res$coord, 4)
fviz_pca_var(res, col.var = "black")
head(res$cos2, 4)

MCA(general, ncp = 4, graph = TRUE)
res_gen<-MCA(general, graph = FALSE)
fviz_mca_var(res_gen, 
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())
fviz_cos2(res_gen, choice = "var", axes = 1:2)
gen_dim_desc <- dimdesc(res_gen, axes = c(1,2))
gen_dim_desc[[1]]
gen_dim_desc[[2]]

#MCA FOR PROGRAMMING LANGUAGES
MCA(languages, ncp = 4, graph = TRUE)
res_lang<-MCA(languages, graph = FALSE)
fviz_mca_var(res_lang, 
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())
fviz_cos2(res_lang, choice = "var", axes = 1:2)
lan_dim_desc <- dimdesc(res_lang, axes = c(1,2))
lan_dim_desc[[1]]
lan_dim_desc[[2]]

fviz_pca_ind(res_lang, col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = FALSE )
#Mosaic plot
summary(languages)
mosaic(languages)

#MCA FOR PREFARABLE ENVIRONMENT
MCA(notebook, ncp = 4, graph = TRUE)
res_note<-MCA(notebook, graph = FALSE)
fviz_mca_var(res_note, 
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())
fviz_cos2(res_note, choice = "var", axes = 1:2)
not_dim_desc <- dimdesc(res_note, axes = c(1))
not_dim_desc[[1]]
not_dim_desc[[2]]
fviz_pca_ind(res_note, col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = FALSE )

#CHOOSING DIMENSIONS
fviz_screeplot(res_lang, addlabels = TRUE, ylim = c(0, 45))
fviz_screeplot(res_note, addlabels = TRUE, ylim = c(0, 45))

write.csv(test, "ds_kaggle_df_lang_env.csv", row.names=FALSE)
