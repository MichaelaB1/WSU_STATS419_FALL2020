### HOMEWORK 2

## NUMBER 1
# Create the "rotate matrix" functions described in lectures from the sample matrix.
# rotating myMatrix by 90, 180 and 270 degrees! 

myMatrix = matrix( c(
  1, 0, 2,
  0, 3, 0,
  4, 0, 5
), nrow=3, byrow=T);



transposeMatrix = function(mat)
{
  t(mat);
}

# clockwise
rotateMatrix90 = function(mat)
{
  t(mat[nrow(mat):1,,drop=FALSE]);
}

rotateMatrix180 = function(mat)
{
  rotateMatrix90(rotateMatrix90(mat));
}   

rotateMatrix270 = function(mat)
{
  rotateMatrix90(rotateMatrix90(rotateMatrix90(mat)));
}   

rotateMatrix90(myMatrix);
rotateMatrix180(myMatrix);
rotateMatrix270(myMatrix);


# counter clockwise
rotateMatrix90_cc = function(mat)
{
  apply(t(mat), 2, rev);
}

rotateMatrix180_cc = function(mat)
{
  rotateMatrix90_cc(rotateMatrix90_cc(mat));
}   

rotateMatrix270_cc = function(mat)
{
  rotateMatrix90_cc(rotateMatrix90_cc(rotateMatrix90_cc(mat)));
}   

rotateMatrix90_cc(myMatrix);
rotateMatrix180_cc(myMatrix);
rotateMatrix270_cc(myMatrix);






## NUMBER 2
# Recreate the graphic for the IRIS Data Set using R.  Same titles, same scales, same colors.  
# See: https://en.wikipedia.org/wiki/Iris_flower_data_set#/media/File:Iris_dataset_scatterplot.svg
# Iris Data Set
IRIS_Data = iris 
class(iris)
# Scatterplot
pairs(IRIS_Data[1:4], main = "Iris Data (red=setosa,green=versicolor,blue=virginica)", 
      pch = 21, bg = c("red", "green", "blue")[unclass(IRIS_Data$Species)])



## NUMBER 3
# Right 2-3 sentences concisely defining the IRIS Data Set.  Maybe search KAGGLE for a nice template.  
# Be certain the final writeup are your own sentences 
# (make certain you modify what you find, make it your own, but also cite where you got your ideas from).  
# NOTE:  Watch the video, Figure 8 has a +5 EASTER EGG.

# The Iris flower data set is a multivariate data set. 
# The data set contains four measurements (sepals length and width, petals length and width) for 150 records of flowers. 
# Each is represented in the three species of iris: Iris setosa, Iris versicolor and Iris virginica.
# The Iris setosa is from a wide range across the Arctic sea.
# The Iris versicolor is found in North America, like Eastern United States and Eastern Canada.
# The Iris virginica is native to eastern North America.



## NUMBER 4

# my_data = read.delim("personality-raw.txt", sep = "|", header = TRUE);
my_data = read.delim("C:\\Users\\michaela.bayerlova\\Documents\\STATS 419\\reading assignments\\personality-raw.txt", sep = "|", header = TRUE);
head(my_data);
nrow_raw_data <- nrow(my_data);
dim(my_data);

# remove column V00
my_data$V00 <- NULL;

# Create two new columns from the current column "date_test":  year and week
d = strptime(my_data$date_test, format="%m/%d/%Y %H:%M");
d.year = as.numeric(strftime(d, format='%Y'));
d.week = as.numeric(strftime(d, format='%W'));
my_data$year = d.year;
my_data$week = d.week;
date_index = which(names(my_data)=="date_test");

# Remove date_test
my_data$date_test <- NULL;

# Sort the new data frame by YEAR, WEEK so the newest tests are first 
# my_data <- arrange(my_data, c("year","week"),);
df.dim = dim(my_data); 
df.pos = df.dim[2]; 
which( names(my_data)=="year" );
which( names(my_data)=="week" );
my_data = my_data[, c(1,62,63, 2:61)];

if(date_index == 1)
{
  reorder = c( (df.pos-1):df.pos, 2:(df.pos-2) );
}
else
{
  reorder = c( 1:(date_index-1), (df.pos-1):df.pos, 2:(df.pos-2) );
}
my_data = my_data[, reorder];
my_data_sorted = my_data[ order(-my_data[,date_index],-my_data[,(1+date_index)]), ];
head(my_data_sorted[,1:5]);

# Remove duplicates using the unique function based on the column "md5_email", only leave most recent
#my_data <- my_data[!duplicated(my_data$md5_email),];
u = unique( my_data_sorted["md5_email"] );
my_data_clean = my_data_sorted[!duplicated(my_data_sorted["md5_email"]), ];

# In the homework, for this tasks, report how many records your raw dataset had and
# how many records your clean dataset has
nrow_clean_data <- nrow(my_data_clean)
dim(my_data_clean);
fwrite(my_data_clean,"personality-clean.txt", sep="|",col.names = TRUE, row.names = FALSE);

## NUMBER 5
# doSummary and sampleVariance and doMode 
doSummary = function(x)
{
  # length
  l = length(x);
  # number of NAs
  n_NAs = colSums(is.na(x));
  # mean
  m = mean(x, na.rm = TRUE);
  # median
  med = median(x, na.rim = TRUE);
  # mode #custom function
  mode_c = doMode(x);
  # variance #custom function
  variance = doSampleVariance(x, "naive");
  # sd... built in fct but compare it to custom fct...
  sd_a = sd(x);
  sd_b = sd(x)*(sqrt((length(x)-1)/length(x)));
  
}


doSampleVariance = function(x, method)
{
  if( method=="naive")
  {
    sum((x-mean(x))^2)/(length(x)-1);
  }
  else
  {
    # two-pass algorithm
    n = sum1 = sum2 = 0;
    for (i in 1:x)
    {
      n += 1;
      sum1 += x;
    }
    m = sum1/n
    
    for (i in 1:x)
    {
      sum2 += (x-m)*(x-m);
    }
    variance = sum2/(n-1);
    return variance;
  }
  
}



doMode = function(x)
{
  result = c();
  # freq... #high frequencies
  # ties... store all of ties
  score = 0;
  # find highest score
  for (i in 1:x)
  {
    if (score < x[i])
    {
      score = x[i];
    }
  }
  # go through x again to get all ties and store them
  for (i in 1:x)
  {
    if (score == x[i])
    {
      result = c(result,x[i]);
    }
  }
  
  result;	  
}





## NUMBER 6



