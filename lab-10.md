Lab 10 - Grading the professor, Pt. 2
================
Steph Bossert
13 April

### Load packages and data

``` r
library(tidyverse) 
library(tidymodels)
library(openintro)
?evals
view(evals)
```

### Exercise 1-2

### m\_bty = 3.88 +.067(bty\_avg)

\#\#\#Rsq = .04, Adj R sq = .03– 40% of the variance in scores is
explained in this model.

``` r
###1 load data.
m_bty = lm(score~bty_avg, data = evals) #Create the linear regression
summary(m_bty)
```

    ## 
    ## Call:
    ## lm(formula = score ~ bty_avg, data = evals)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.9246 -0.3690  0.1420  0.3977  0.9309 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  3.88034    0.07614   50.96  < 2e-16 ***
    ## bty_avg      0.06664    0.01629    4.09 5.08e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5348 on 461 degrees of freedom
    ## Multiple R-squared:  0.03502,    Adjusted R-squared:  0.03293 
    ## F-statistic: 16.73 on 1 and 461 DF,  p-value: 5.083e-05

### m\_bty\_gen = 3.88 +.07(bty\_avg) + (.17)(gender)

\#\#\#Exercise 3: Everything else held constant, men score .17 higher
than females on their scores \#\#\#Ex. 4: 59% of the variance in score
is explained by gender and beauty rating…slightly disturbing results…
\#\#\#Ex 5: m\_bty\_gen = 3.88 +.07(bty\_avg) \#\#\#Ex 6: Men \#\#\#Ex
7: See plot \#\#\#Ex 8: The R square of m\_bty\_gen is almost 20%
percentage points higher, meaning we are accounting for another 20% of
variance in scores with the additional information \#\#\#Ex 9: The
association/slope increases slightly, yes the addition of gender has
changed the slope \#\#\#Ex 10: m\_bty\_rank = 3.98 + .07(bty\_avg)
-.16(tenure track) -.13(tenure); all else held constant, tenure track
professors score .16 points less than tenure professors. All else held
constant a one point increase in beauty average is associated with a .07
increase in score.

``` r
m_bty_gen = lm(score~bty_avg + gender, data = evals)
summary(m_bty_gen)
```

    ## 
    ## Call:
    ## lm(formula = score ~ bty_avg + gender, data = evals)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.8305 -0.3625  0.1055  0.4213  0.9314 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  3.74734    0.08466  44.266  < 2e-16 ***
    ## bty_avg      0.07416    0.01625   4.563 6.48e-06 ***
    ## gendermale   0.17239    0.05022   3.433 0.000652 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5287 on 460 degrees of freedom
    ## Multiple R-squared:  0.05912,    Adjusted R-squared:  0.05503 
    ## F-statistic: 14.45 on 2 and 460 DF,  p-value: 8.177e-07

``` r
### m_bty_gen = 3.88 +.07(bty_avg) + (.17)(gender) 
###Exercise 3: Everything else held constant, men score .17 higher than females on their scores
###Ex. 4: 59% of the variance in score is explained by gender and beauty rating...slightly disturbing results...
###Ex 5: m_bty_gen = 3.88 +.07(bty_avg)
###Ex 6: Men
####Ex 7: 
ggplot(evals, aes(x=score, y=bty_avg, group=gender)) + 
  geom_point(shape=18, color="blue")+
  geom_smooth(method=lm, se=FALSE, linetype="solid",
             color="orange") +
  geom_jitter(aes(colour = gender))
```

    ## `geom_smooth()` using formula 'y ~ x'

![](lab-10_files/figure-gfm/multiple%20regression-1.png)<!-- -->

``` r
m_gen = lm(score~ gender, data = evals)
summary(m_gen)
```

    ## 
    ## Call:
    ## lm(formula = score ~ gender, data = evals)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.83433 -0.36357  0.06567  0.40718  0.90718 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  4.09282    0.03867 105.852  < 2e-16 ***
    ## gendermale   0.14151    0.05082   2.784  0.00558 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5399 on 461 degrees of freedom
    ## Multiple R-squared:  0.01654,    Adjusted R-squared:  0.01441 
    ## F-statistic: 7.753 on 1 and 461 DF,  p-value: 0.005583

``` r
###Ex 8: The R square of m_bty_gen is almost 20% percentage points higher, meaning we are accounting for another 20% of variance in scores with the additional information 
###Ex 9: The association/slope increases slightly, yes the addition of gender has changed the slope
###Ex 10: 
m_bty_rank = lm(score~bty_avg + rank, data = evals)
summary(m_bty_rank)
```

    ## 
    ## Call:
    ## lm(formula = score ~ bty_avg + rank, data = evals)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.8713 -0.3642  0.1489  0.4103  0.9525 
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)       3.98155    0.09078  43.860  < 2e-16 ***
    ## bty_avg           0.06783    0.01655   4.098 4.92e-05 ***
    ## ranktenure track -0.16070    0.07395  -2.173   0.0303 *  
    ## ranktenured      -0.12623    0.06266  -2.014   0.0445 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5328 on 459 degrees of freedom
    ## Multiple R-squared:  0.04652,    Adjusted R-squared:  0.04029 
    ## F-statistic: 7.465 on 3 and 459 DF,  p-value: 6.88e-05

``` r
### m_bty_rank = 3.98 + .07(bty_avg) -.16(tenure track) -.13(tenure); all else held constant, tenure track professors score 
```

\#\#\#Ex 11. cls\_credits- Class credits \#\#\#Ex 12: m\_cls = 4.15 +
(.48)(cls\_credits) \#\#\#Ex 13: Do not include cls\_did\_eval because
it would include redundant info in the model and not affect your
variance explained in the model \#\#\#Ex 14: m\_full2= lm(score\~ gender
+ language + age + cls\_perc\_eval + cls\_did\_eval + cls\_students +
cls\_level + cls\_profs + cls\_credits + bty\_avg, data=evals) \#\#\#Ex
15: m\_full9 = lm(score\~ gender + ethnicity + bty\_avg + rank,
data=evals); score = 3.86 +.18(gender) + .10(notminority) .07(bty\_avg)
-.12(tenure track) - .15(tenure) \#\#\#Ex 16: All else held constant,
men score .18 points higher than women on average. All others held
constant, a 1 point increase in beauty average is associated with an
increase in .07 in their scores. \#\#\#Ex 17: Professor would be a non
minority man, on the tenure track, who is attractive. \#\#\#Ex 18: I
think these results could be generalized to most schools that share
similar demographics to the University of Texas.

``` r
###Ex 11. cls_credits- Class credits
###Ex 12
m_cls = lm(score~cls_credits, data = evals)
summary(m_cls)
```

    ## 
    ## Call:
    ## lm(formula = score ~ cls_credits, data = evals)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.84702 -0.34702  0.05298  0.35298  0.85298 
    ## 
    ## Coefficients:
    ##                       Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)            4.14702    0.02552 162.494  < 2e-16 ***
    ## cls_creditsone credit  0.47520    0.10568   4.496 8.75e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5329 on 461 degrees of freedom
    ## Multiple R-squared:  0.04202,    Adjusted R-squared:  0.03994 
    ## F-statistic: 20.22 on 1 and 461 DF,  p-value: 8.751e-06

``` r
### m_cls = 4.15 + (.48)(cls_credits)
###Ex 13: Do not include cls_did_eval because it would include redundant info in the model and not affect your variance explained in the model
###Ex 14
m_full2= lm(score~ gender + language + age + cls_perc_eval + cls_did_eval + cls_students + cls_level + cls_profs + cls_credits + bty_avg, data=evals)
###Ex 15:
m_full9 = lm(score~ gender + ethnicity + bty_avg + rank, data=evals)
summary(m_full9)
```

    ## 
    ## Call:
    ## lm(formula = score ~ gender + ethnicity + bty_avg + rank, data = evals)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.8329 -0.3694  0.0972  0.4018  0.9670 
    ## 
    ## Coefficients:
    ##                       Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)            3.77599    0.11441  33.004  < 2e-16 ***
    ## gendermale             0.18186    0.05195   3.501  0.00051 ***
    ## ethnicitynot minority  0.10029    0.07231   1.387  0.16612    
    ## bty_avg                0.07284    0.01638   4.447 1.09e-05 ***
    ## ranktenure track      -0.11979    0.07411  -1.616  0.10669    
    ## ranktenured           -0.15887    0.06252  -2.541  0.01138 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5254 on 457 degrees of freedom
    ## Multiple R-squared:  0.07674,    Adjusted R-squared:  0.06664 
    ## F-statistic: 7.597 on 5 and 457 DF,  p-value: 7.172e-07

``` r
### score = 3.86 +.18(gender) + .10(notminority) .07(bty_avg) -.12(tenure track) - .15(tenure)
###Ex 16: All else held constant, men score .18 points higher than women on average. All others held constant, a 1 point increase in beauty average is associated with an increase in .07 in their scores. 
###Ex 17: Professor would be a non minority man, on the tenure track, who is attractive. 
###Ex 18: I think these results could be generalized to most schools that share similar demographics to the University of Texas. 
```
