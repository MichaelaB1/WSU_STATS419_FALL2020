







# Average values
For data points that have a right and left measurement, the average will be taken. When there is only one measurement, that one will be taken. This enables to get a uniform set of data without missing values for those entries.

# take one given value of left or right OR take the average of them both when given

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


