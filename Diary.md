## Introduction

This is just a note book of Kang, reporting what he is doing everyday. 

## 20221110 

So ... where to start? 

How about a overview of the data? 

Here are some perspectives to explore and potential solutions: 
(1) General description: total number of people of each day, etc. 
(2) What is the impact of some factors: holiday or weekend, weather, etc? 
(3) How do people move to the mountain top? I can extract some people with more data to track their path. 

Let's begin. 

## 20221121 

Have done a very very basic analysis that covers many aspects of the data. 
Had a discussion with the professors. 
The things I need to do next are twofold: to learn discrete choice model (so, go back to logit again ... too difficult for me); to think about the specific question. 

## 20221125 

Read some articles and sections of textbooks about discrete model ... haven't understand the model yet. But maybe I can try to apply the model (which is not a good manner though) or collect the data required for the potential model first. 

The first problem to solve is to speed up the data reading and process. There are 3 ways to do that at least: Rcpp, with C code; parallelism with `parallel` package or `foreach` package; some functions from `dplyr` package. 
I used the second and the third one. 

The second problem is to think about our variables for the potential logit model. So, we need: 
(1) "dependent variable", which is the choice of the trail head (登山口); 
(2) a set of the attributes of the alternatives, like the distance of the trail head to the mountain top, the accessibility (might be related to the distance from the home city of the visitors, but, by the way, I doubt that the distance of different trail head is nothing compared to the travel distance to the prefecture of the visitors come from the city far away from here), travel time and opportunity cost (calculated by travel time and wage rate), how beautiful the trail head is (but I wonder if we can quantify that - maybe there are some scores like the Tabelog for food?); 
(3) a set of the attributes of the visitors, like the income level (how do we get the data?). 
