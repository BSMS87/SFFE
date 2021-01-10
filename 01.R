library(foreign)
library(mosaic)
library(haven)

G <- read_sav("ZA6702_v1-0-0.sav")
colnames(G) <- tolower(colnames(G))


ds = read.spss("ZA6702_v1-0-0.sav",use.value.labels = TRUE)

dataset <- read.spss("ZA6702_v1-0-0.sav",use.value.labels = TRUE, max.value.labels = Inf, to.data.frame = TRUE)
colnames(dataset) <- tolower(colnames(dataset))

# To have an overview of the  dataset structure 
str(dataset)

# To check the size of you data
dim(dataset)
dim(G)

# to visualize the dataset, using head functions allows you to inspect the first 6 rows of the dataset 
head(dataset)

# to define the length of the variable 
length(dataset)
       
# To inspect the last six rows 
tail(dataset)

# to view the dataset
View(dataset)

# to manually edit a dataset entry 
fix(dataset)

# to visulaize the variable: 

tally(G$q1_1)
tally(G$q1_2)

G$ImportanceOfFamily <- G$q1_1
G$ImportanceOfFriends <- G$q1_2

gf_boxplot(ImportanceOfFamily~ImportanceOfFriends, data = G)
mosaicplot(ImportanceOfFamily~ImportanceOfFriends, data = G)
gf_jitter(ImportanceOfFamily~ImportanceOfFriends, data = G)

summary(G)

Summ <- summary(G)
rownames(G)

X <- mtcars

G

dataset

