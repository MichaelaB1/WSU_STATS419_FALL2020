## functions

education = function(one)
   {
   result = list();
      result$who = one;
	  result$think = c("intensively", "critically");
	  result$goal = "intelligence + character");
   result;
   }
   
me = education("Misa");




myMatrix = matrix( c(
                        1, 0, 2,
						0, 3, 0,
						4, 0, 5
						), nrow=3, byrow=T);



transposeMatrix = function(mat)
   {
   t(mat);
   }


# in Homework, write functions -> point of rotations is 3
# rotateMatrix90(mat)
# rotateMatrix180(mat)
# rotateMatrix270(mat)
# 3x3 matrix...##matrix multiplication -> transformative T matrix that you multiply by


install.packages("stringr"), dependencies=T);
library(stringr);
install.packages("rvest"), dependencies=T);
librariy(rvest);



# Research question: who is a better actor? Will Smith or Denzel Washington?
## actor has a person_id, movie_id, details name, count movies

# https://rvest.tidyverse.org/index.html
## Denzel Washington[nm0000243] vs Will Smith [nm0000226]
## https://www.imdb.com/filmosearch/?explore=title_type&role=nm0000243&ref_=filmo_ref_typ&sort=num_votes,desc&mode=detail&page=l&title_type=movie

grabNameFromFilmsPage = function(page)
   {
    name = page %>%
      html_node(".header") %>%
	  html_text();
	  
	  name = gsub("Most Rated Feature Films With", "", name, fixed=T);
	  name = str_trim(name)
	  
	name;
   
   }

grabFilmCountFromFilmsPage = function(page)
   {
   count = page %>%
      html_node(".desc") %>%
	  html_text();
	  
	  temp = strsplit(count, "of", fixed=T);
	  temp2 = strsplit(temp[[1]][2],"titles", fixed=T);
	  
	  count = str_trim(temp2[[1]][1]);
	  
	as.numeric(count);
   
   }

 
grabFilmInfoFromFilmsPage = function(page)
   {
   totalcount = page %>%
      html_nodes(".desc") %>% # need nodes because there is 50 of them
	  html_text();
	  
	  temp = strsplit(totalcount, "of", fixed=T);
	  temp2 = strsplit(temp[[1]][2],"titles", fixed=T);
	  
	  totalcount = str_trim(temp2[[1]][1]);
	  totalcount = as.numeric(totalcount);
	  
	  temp2 = strsplit(temp[[1]][1],"to", fixed=T);
	  
	  pagecount = str_trim(temp2[[1]][2]);
	  pagecount = as.numeric(pagecount);
	
   result = list();
   
   result$totalcount = totalcount;
   result$pagecount = pagecount;
   } 
   
   
grabFilmInfoFromFilmsPage = function(page, pagecount)
   {
   # 50 elements
   for(i in 1:pagecount)
      {
	  # title = id = rank = year = rating = minutes = genre = votes = metascore = desc = millions
	  }
    
	result;
   
   }   
   
   


# maybe do library imdb over semester
nmid = "nm0000226";
grabFilmsForPerson = function(nmid)
   {
   url = paste("https://www.imdb.com/filmosearch/?explore=title_type&role=", nmid, "&ref_=filmo_ref_typ&sort=num_votes,desc&mode=detail&page=l&title_type=movie", sep="");
   # get page object
   page1 = read_html(url);
   result = list();
   
   # name of person, function
   results$name = grabNameFromFilmsPage(page1);
   
   results$countfilms = grabFilmCountFromFilmsPage(page1);
   
   result$movies.50 = grabFilmInfoFromFilmsPage(page1, result$pagecount);
   
   # parallel format...
   ranks = page1 %>%
      html_nodes(".lister-item-index") %>%
	  html_text() %>%
	  as.numeric();
	ranks;
	
	years = page1 %>%
	   html_nodes(".lister-item-year") %>%
	   html_text();
	   years = gsub('(','',years, fixed=T);
	   years = gsub(')','',years, fixed=T);
	   years = gsub('I','',years, fixed=T);
	   years = as.numeric(years);
	   
	titles = page1 %>%
	   html_nodes(".lister-items-header a") %>%
	   html_text();
	titles;
   
   }

# bonus: to make table nicer
table = as.data.frame(cbind(ranks, years, titles));
table$ranks = as.numeric(table$ranks);
table$years = as.numeric(table$years);
colnames(table) = c("rank", "year", "title");



# loop

