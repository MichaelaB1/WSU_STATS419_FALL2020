# Michaela Bayerlova
# functions for measurement project


# Instructor function
removeDuplicatesFromDataFrameAllColumns	= function(df)
{
  nrows = dim(df)[1];
  ncols = dim(df)[2];
  
  df.str = c();
  for(i in 1:nrows)
  {
    row = df[i,];
    row.str = paste(as.character( unlist(row) ) ,collapse="-");
    df.str = c(df.str,row.str);
  }
  
  #duplicated(df.str);
  ndf = df[!duplicated(df.str), ];
}




# Average values
# For data points that have a right and left measurement, the average will be taken. When there is only one measurement, that one will be taken. This enables to get a uniform set of data without missing values for those entries.
# take one given value of left or right OR take the average of them both when given
combineLeftAndRightMeasurements = function(measure.clean)
{
  getOne = c("hand.length", "hand.width", "hand.elbow", "elbow.armpit", "arm.reach", "foot.length", "floor.kneepit", "floor.hip", "floor.armpit");
  n.rows = dim(measure.clean)[1];
  
  for(one in getOne)
    {
    measure.clean[one] = NA;
    }
  
  for(i in 1:n.rows)
    {  
    measure.row = measure.clean[i,];
    for(one in getOne)
      {
      nidx = getIndexOfDataFrameColumns(measure.clean, one);
    
      myleft = paste0(one,".left");
      lidx = getIndexOfDataFrameColumns(measure.row, myleft);
      myright = paste0(one,".right");
      ridx = getIndexOfDataFrameColumns(measure.row, myleft);
    
    # print(paste0(
    #             "left: ",myleft," --> ",lidx,
    #             " right: ",myright," --> ",ridx
    #             )
    #       );
    
      row.m = mean(
        c(as.numeric(unlist(measure.row[lidx])),
          as.numeric(unlist(measure.row[ridx]))),
        na.rm=TRUE);
    
      measure.clean[i,nidx] =  row.m;
    }
  }

  #str(measure); # lot's of columns ...
  measure.clean;
}





# make same gender notation
sameGenderNotation = function(measure.clean)
{ 
  measure.clean$gender <- tolower(measure.clean$gender);

  measure.clean$gender[measure.clean$gender == 'f'] <- 'female';
  measure.clean$gender[measure.clean$gender == 'm'] <- 'male';

  measure.clean;
}





# same units - convert to cm - 1 inch is 2.54 cm
convertUnitsFromInchesTocm = function(measure.clean)
{
  for (row in 1:nrow(measure.clean)) { # Iterate through each row
    if (measure.clean$units[row] == 'inches' || measure.clean$units[row] == 'in' || measure.clean$units[row] == 'Inch') { # When units is in cm
      measure.clean[row, 4:17] <- measure.clean[row, 4:17]*2.54; # Replace measurements with cm equivalent
      measure.clean$units[row] <- 'cm';
    }
  }
  measure.clean;
}



## Remove wrong data
removeWrongData = function(measure.clean)
{
  comparison = measure.clean$arm.reach > measure.clean$height.NA;
  comparison[is.na(comparison)];
  measure.clean = measure.clean[comparison,];
  measure.clean;
}





## Remove Outliers
removeOutliers = function(measure.clean)
{
measure.clean <- na.omit(measure.clean);
measure.clean;

for (columns in 4:17) {
   Q1 <- quantile(measure.clean[[columns]], 0.2);
   Q3 <- quantile(measure.clean[[columns]], 0.8);
   IQR <- IQR(measure.clean[[columns]]);
# boxplot(measure.clean$hand.length);
   no_outliers <- subset(measure.clean, measure.clean[[columns]]>(Q1 - 1.5*IQR) & measure.clean[[columns]]<(Q3 +1.5*IQR));
# boxplot(no_outliers$hand.length);
}

#colnames(measure.clean)
measure.clean;
}





prepareMeasureData = function(file.measure)
{ 
  # create data frame
  measure.raw = read.csv(file.measure, header=TRUE, sep="|");
  measure.raw = as.data.frame(measure.raw);
  
  measure.clean = measure.raw;
  measure.clean = as.data.frame(measure.clean);
  
  dim(measure.clean);
  summary(measure.clean);
  
  measure.clean;
  
  # Instructor function
  measure.clean = removeDuplicatesFromDataFrameAllColumns(measure.clean);
  measure.clean;
  
  
  # Average values
  measure.clean = combineLeftAndRightMeasurements(measure.clean);
  
  
  ## Leave only average values
  # determine if this separation above will give the same as separating by head proportions
  measure.clean = measure.clean[,-(7:16), drop=FALSE];
  measure.clean = measure.clean[,-(8:13), drop=FALSE];
  measure.clean = measure.clean[,-(9:10), drop=FALSE];
  
  # re-arrange columns
  measure.clean = measure.clean[,c(1,2,3,4,5,6,20,21,22,23,7,24,25,26,27,8,28,9,10,11,12,13,14,15,16,17,18,19)];
  measure.clean;
  
  # remove notes column
  measure.clean <- measure.clean[,1:27];
  
  # make same gender notation
  measure.clean = sameGenderNotation(measure.clean);
  
  # same units - convert to cm - 1 inch is 2.54 cm
  measure.clean = convertUnitsFromInchesTocm(measure.clean);
  
  # Remove wrong data
  measure.clean = removeWrongData(measure.clean);
  
  outfile = gsub(".txt", ".rds", file.measure);
  saveRDS(measure.clean, outfile);
  
  # Remove outliers
  measure.clean = removeOutliers(measure.clean);
  measure.clean;
  }



## Read in Instructors cleaned data
readDataIntoDataFrame = function(file.measure)
{
  measure.clean2 = read.csv(file.measure, header=TRUE, sep="|");
  measure.clean2 = as.data.frame(measure.clean2);
  measure.clean2;
}






# DATA ANALYSIS
## create subset with data variables
createSubset = function(measure.clean)
{
  measure.subset <- as.data.frame(measure.clean);
  measure.subset = measure.subset[,-(1:3), drop=FALSE];
  measure.subset = measure.subset[,-(3:8), drop=FALSE];
  measure.subset = measure.subset[,-(4:13), drop=FALSE];
  measure.subset = measure.subset[,-(6:8), drop=FALSE];
  measure.subset;
}

## create subset with data variables
createSubsetInstructor = function(measure.clean)
{
  measure.subset <- as.data.frame(measure.clean);
  measure.subset = measure.subset[,-(1:2), drop=FALSE];
  measure.subset = measure.subset[,-(3), drop=FALSE];
  measure.subset = measure.subset[,-(4:6), drop=FALSE];
  measure.subset = measure.subset[,-(4:6), drop=FALSE];
  measure.subset = measure.subset[,-(6:17), drop=FALSE];
  
  # rename columns
  measure.subset;
  colnames(measure.subset) <-c("height.NA", "head.height.NA", "arm.span.NA", "age", "gender"); 
  measure.subset;
}



## Create new data frame 'measure.proportions' to work on analysis
measureProportionsData = function(measure.subset)
{
  measure.proportions <- as.data.frame(measure.subset);
  measure.proportions <- na.omit(measure.proportions);
  measure.proportions;
}



## create group based on proportions of head.height vs height
proportionsGroup = function(measure.proportions)
{
  # create proportion: how many times is the head in the body
  measure.proportions$proportions.head.and.height = measure.proportions$height.NA/measure.proportions$head.height.NA;
  
  # group based on proportions
  measure.proportions$proportions.group[measure.proportions$gender == 'female' & measure.proportions$proportions.head.and.height > 6.5] <- 'F'; 
  measure.proportions$proportions.group[measure.proportions$gender == 'male' & measure.proportions$proportions.head.and.height > 6.5] <- 'M'; 
  measure.proportions$proportions.group[measure.proportions$proportions.head.and.height <= 6.5] <- 'K';
  
  measure.proportions;
}



# what is the mean of male or female proportions
meanMaleProportions = function(measure.proportions)
{
  measure.proportions.male <- as.data.frame((measure.proportions));
                               
  comparison = (measure.proportions.male$gender == 'male');
  comparison[is.na(comparison)];
  measure.proportions.male = measure.proportions.male[comparison,];
                                            
  summary(measure.proportions.male);
}

# female
meanFemaleProportions = function(measure.proportions)
{
  measure.proportions.female <- as.data.frame((measure.proportions));
  
  comparison2 = measure.proportions.female$gender == 'female';
  comparison[is.na(comparison2)];
  measure.proportions.female = measure.proportions.female[comparison2,];
  
  summary(measure.proportions.female);
}



## create group based on age
ageGroup = function(measure.proportions)
{
  # group into adolescent female, male and child
  measure.proportions$age.group[measure.proportions$gender == 'female' & measure.proportions$age > 10] <- 'F';
  measure.proportions$age.group[measure.proportions$gender == 'male' & measure.proportions$age > 10] <- 'M';
  measure.proportions$age.group[measure.proportions$age < 11] <- 'K';
  
  measure.proportions;
}




## create data frame with only the 3 new columns
dataframeGroup = function(measure.proportions)
{
  # new data frame
  measure.proportions.new <- measure.proportions[6:8];
  
  measure.proportions.new <- na.omit(measure.proportions.new);
  measure.proportions.new;
}



## Prepare for analysis of proportions
prepareProportionsAnalysis = function(measure.clean)
{
  # Remove wrong data
  #measure.clean = removeWrongData(measure.clean);
  
  # Create subset with data variables
  measure.subset = createSubset(measure.clean);
  
  # Create new data frame 'measure.proportions' to work on analysis
  measure.proportions = measureProportionsData(measure.subset);
  
  # Create group based on proportions of head.height vs height
  measure.proportions = proportionsGroup(measure.proportions);
  
  # Create group based on age
  measure.proportions = ageGroup(measure.proportions);
  
  measure.proportions;
}


## Prepare for analysis of proportions
prepareProportionsAnalysisInstructor = function(measure.clean)
{
  # Remove wrong data
  #measure.clean = removeWrongData(measure.clean);
  
  # Create subset with data variables
  measure.subset = createSubsetInstructor(measure.clean);
  
  # Create new data frame 'measure.proportions' to work on analysis
  measure.proportions = measureProportionsData(measure.subset);
  
  # Create group based on proportions of head.height vs height
  measure.proportions = proportionsGroup(measure.proportions);
  
  # Create group based on age
  measure.proportions = ageGroup(measure.proportions);
  
  measure.proportions;
}






## Do the 2 groups assigned match
matchingGroups = function(measure.proportions)
{
  # Create data frame with only the 3 new columns
  measure.proportions.new = dataframeGroup(measure.proportions);
  
  # now let's see if the 2 groupings give the same answer
  match = 0;
  
  
  for (row in 1:nrow(measure.proportions.new)) { # Iterate through each row
    if (measure.proportions.new$proportions.group[row] == measure.proportions.new$age.group[row]) { # When grouping is the same
      match = match + 1; # Add a match
    }
  }
  
  rows = nrow(measure.proportions.new);
  match.ratio = match/rows;
  
  match.percentage = match.ratio*100;
  match.percentage;
}




## What percentage of people is considered heroic?
heroicPeople = function(measure.proportions)
{
  # group based on proportions
  measure.proportions$proportions.group.2[measure.proportions$gender == 'female' & measure.proportions$proportions.head.and.height > 6.5 & measure.proportions$proportions.head.and.height <= 8.5] <- 'F';
  measure.proportions$proportions.group.2[measure.proportions$gender == 'male' & measure.proportions$proportions.head.and.height > 6.5 & measure.proportions$proportions.head.and.height <= 8.5] <- 'M';
  measure.proportions$proportions.group.2[measure.proportions$proportions.head.and.height <= 6.5] <- 'K';
  measure.proportions$proportions.group.2[measure.proportions$proportions.head.and.height > 8.5] <- 'H';
  
  measure.proportions;
  
  
  # new data frame
  measure.proportions.h <- measure.proportions[9];
  measure.proportions.h <- na.omit(measure.proportions.h);
  measure.proportions.h;
  
  
  # now let's see what proportion of people is considered heroic
  match.h = 0;
  
  for (row in 1:nrow(measure.proportions.h)) { # Iterate through each row
    if (measure.proportions.h$proportions.group.2[row] == 'H') { # When grouping is heroic
      match.h = match.h + 1; # Add a match
    }
  }
  
  rows = nrow(measure.proportions.h);
  h.ratio = match.h/rows;
  
  h.ratio;
  h.percentage = h.ratio*100;
  h.percentage;
}


# What percentage of heroic people are male
percentageOfHeroicPeopleBeingMale = function(measure.proportions)
{
  # new data frame
  measure.proportions.new2 <- measure.proportions[,c(5,7)];
  measure.proportions.new2 <- na.omit(measure.proportions.new2);
  measure.proportions.new2;
  
  
  # now let's see what proportion of people is considered heroic
  match.h = 0;
  
  for (row in 1:nrow(measure.proportions.new2)) { # Iterate through each row
    if (measure.proportions.new2$proportions.group[row] == 'H') { # When grouping is heroic
      match.h = match.h + 1; # Add a match
    }
  }
  
  rows = nrow(measure.proportions.new2);
  h.ratio = match.h/rows;
  
  h.ratio;
  h.percentage = h.ratio*100;
  h.percentage;
  
  male.heroic = (16/26)*100;
  female.heroic = 100 - male.heroic;
  
  male.heroic;
  
  # heroic.male = 0;
  # heroic.female = 0;
  # 
  # 
  # for (row in 1:nrow(measure.proportions.new2)) { # Iterate through each row
  #  if (measure.proportions.new2$proportions.group.2[row] == 'H'&& measure.proportions.new2$gender == 'male')
  #    { # When grouping is heroic and male
  #     heroic.male = heroic.male + 1; # Add a match
  #  }
  #  if (measure.proportions.new2$proportions.group.2[row] == 'H'&& measure.proportions.new2$gender == 'female') { # When grouping is heroic and female
  #     heroic.female = heroic.female + 1; # Add a match
  #   }
  # }
  # 
  # heroic.female;
  #   
  # # what percentage of the heroic are male and female
  # heroic.male.p = heroic.male/match.h;
  # heroic.female.p = heroic.female/match.h;
  # 
  # heroic.male.p;
  # heroic.female.p;
  # 
  # heroic.male.of.all.data = (heroic.male/rows)*100;
  # heroic.female.of.all.data = (heroic.female/rows)*100;
  # heroic.male.of.all.data;
  # heroic.female.of.all.data;
}




## Assign people heroic based on height
assignHeroicByHeightMalePercentage = function(measure.proportions)
{
  # group based on average height from website
  measure.proportions$proportions.group.heroic.height[measure.proportions$height.NA > 171 & measure.proportions$gender == 'male'] <- 'H and male';
  measure.proportions$proportions.group.heroic.height[measure.proportions$height.NA > 159 & measure.proportions$gender == 'female'] <- 'H and female';
  
  measure.proportions;
  
  
  # new data frame
  measure.proportions.new2 <- measure.proportions['proportions.group.heroic.height'];
  measure.proportions.new2 <- na.omit(measure.proportions.new2);
  measure.proportions.new2;
  
  
  # now let's see what proportion of people is considered heroic
  heroic.m = 0;
  
  for (row in 1:nrow(measure.proportions.new2)) { # Iterate through each row
    if (measure.proportions.new2$proportions.group.heroic.height[row] == 'H and male') { # When grouping is heroic and male
      heroic.m = heroic.m + 1; # Add a match
    }
  }
  
  rows = nrow(measure.proportions.new2);
  heroic.m.p = (heroic.m/rows)*100;
  
  heroic.m.p
}




## Proportions of all measurements vs head.height
proportionsAllColumnsVSHeadHeight = function(measure.clean)
{
  measure.head.proportions = as.data.frame(measure.clean[4:17]);
  measure.head.proportions;
  
  for (row in 1:nrow(measure.head.proportions)) 
  { # Iterate through each row
    measure.head.proportions$proportions.height = measure.head.proportions$height.NA/measure.head.proportions$head.height.NA;
    measure.head.proportions$proportions.head.circumference = measure.head.proportions$head.circumference.NA/measure.head.proportions$head.height.NA;
    measure.head.proportions$proportions.hand.length = measure.head.proportions$hand.length/measure.head.proportions$head.height.NA;
    measure.head.proportions$proportions.hand.width = measure.head.proportions$hand.width/measure.head.proportions$head.height.NA;
    measure.head.proportions$proportions.hand.elbow = measure.head.proportions$hand.elbow/measure.head.proportions$head.height.NA;
    measure.head.proportions$proportions.elbow.armpit = measure.head.proportions$elbow.armpit/measure.head.proportions$head.height.NA;
    measure.head.proportions$proportions.arm.reach = measure.head.proportions$arm.reach/measure.head.proportions$head.height.NA;
    measure.head.proportions$proportions.arm.span = measure.head.proportions$arm.span.NA/measure.head.proportions$head.height.NA;
    measure.head.proportions$proportions.foot.length = measure.head.proportions$foot.length/measure.head.proportions$head.height.NA;
    measure.head.proportions$proportions.floor.kneepit = measure.head.proportions$floor.kneepit/measure.head.proportions$head.height.NA;
    measure.head.proportions$proportions.floor.hip = measure.head.proportions$floor.hip/measure.head.proportions$head.height.NA;
    measure.head.proportions$proportions.floor.navel = measure.head.proportions$floor.navel.NA/measure.head.proportions$head.height.NA;
    measure.head.proportions$proportions.floor.armpit = measure.head.proportions$floor.armpit/measure.head.proportions$head.height.NA;
    
    row = row+1;
  }
  
  measure.head.proportions;
  
  measure.head.proportions = na.omit(measure.head.proportions);
  measure.head.proportions;

}



# How does arm span relate to height?
relateArmSpanAndHeight = function(measure.proportions)
{
  measure.proportions$proportions.arm.span.and.height = measure.proportions$arm.span.NA/measure.proportions$head.height.NA;
  
  match90 = 0;
  
  for (row in 1:nrow(measure.proportions)) { # Iterate through each row
    if (measure.proportions$proportions.arm.span.and.height[row] > 0.9 || measure.proportions$proportions.arm.span.and.height[row] < 1.1) { # When % difference between arm.span and height is less than 10%
      match.90 = match.90 + 1; # Add a match
    }
  }
  
  match.ratio.90 = match/nrow(measure.proportions);
  
  match.percentage.90 = match.ratio.90*100;
  match.percentage.90;
}







