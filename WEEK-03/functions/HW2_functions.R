### HOMEWORK 2 FUNCTIONS

#Matrix

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



# NUMBER 5
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



doSampleVariance = function(x, method="two-pass")
{
  x = stats::na.omit(x);
  if(method=="naive")
  {
    n = 0;
    sum = 0;
    sum2 = 0;
    
    for(i in 1:length(x))  ## stats::na.omit(x)
    {
      n = n + 1;
      sum = sum + x[i];
      sum2 = sum2 + x[i]*x[i];
    }
    
    if(n < 2) { return(NULL);} #
    x.bar = sum/n;
    s.var = (sum2 - (sum*sum)/n)/(n-1);
    
  } else	{
    # two-pass algorithm # testing
    n = sum = sum2 = 0;
    ## first pass
    for(i in 1:length(x))  ## stats::na.omit(x)
    {
      n = n + 1;
      sum = sum + x[i];
    }
    if(n < 2) { return(NULL);} #
    x.bar = sum/n;
    ## second pass
    for(i in 1:length(x))  ## stats::na.omit(x)
    {
      deviation = x[i] - x.bar;
      sum2 = sum2 + deviation * deviation;
    }
    s.var = sum2/(n-1);
  }
  
  s.sd = sqrt(s.var);
  list("x.bar"=x.bar,"s.var"=s.var,"s.sd"=s.sd);
}



doMode = function(x) # alias ?
{
  whichMaxFreq(x);
}


whichMaxFreq = function(x)  # doMode
{
  x.table = as.data.frame( table(x) );
  freq.max = max( x.table$Freq );
  x.list = x.table[x.table$Freq==freq.max,];
  xs = as.numeric( as.vector (x.list$x) );
  xs;
}




'''
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

'''




