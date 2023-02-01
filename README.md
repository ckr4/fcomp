This is a shiny app to compare the null and alternative hypotheses when testing multiple means.
It provides a graphic representation of how the distributions overlap according to the number 
of treatments, number of samples per treatment, means of the treatments, and the overall
variance. It also displays the areas corresponding to Type I and II errors and calculates the
Beta and Power of the experiment.

The app is hosted online at http://3ogq5s-chad-kite.shinyapps.io/fcomp-app and can also be run
from the console in Rstudio with the command: runGitHub('ckr4/fcomp').

Parameters:
  - Number of treatments -> Number of treatments or factors. The variable the experiement is attempting to test.
  - Means -> The average result of the replications for each treatment.
  - Number of reps per treatment -> The number of time each treatment or factor is applied/used/replicated.
  - Variance -> The overall variance of the results of the experiment
  - Alpha -> The probability of a Type I error (false positive).

Example:
An experiment on the effects of a power setting on the rate of material removal via plasma etching
gives the following results: 

Power Setting  /  Mean

1  /  651.2

2  /  667.4

3  /  625.4

4  /  627

Replications: 5

Overall Variance: 776.2

Setting alpha at .05 would produce the following result:

![fcomp_ex](https://user-images.githubusercontent.com/118565445/216158080-335b73ba-9d3f-4bb0-bff8-e4c341bc20de.png)

