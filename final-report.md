Global Terrorism
================
Weijia Zhang, Yuanzhu Li, Yijing Lin
12/3/2018

Global Terrorism Dataset

leaflet package or shiny plot \#Context The Global Terrorism Database
(GTD) is an open-source database including information on terrorism
around the world from 1970 through 2017 (with annual updates planned for
the future). Unlike many other event databases, the GTD includes
systematic data on domestic as well as international terrorist incidents
that have occurred during this time period and now includes more than
180,000 cases.

\#Goals: We are going to use the global terrorism dataset to visualize
the occurrence of terrorism across the globe from 1970 to 2017. In
particular, we aim to show: 1)frequency of terrorism in 2017 across the
globe - world map 2)Pick 5-8 countries of interest, explore the changes
across the time span 3)what the distribution of different attack types
(such as suicide, bombing) is. 4)What the distribution of number of
wounded is for each attack type／What the distribution of the use of
different attack types based on different terrorist groups is 5)explore
the correlation between attack type and number of killed

\#Content Geography: Worldwide Time period: 1970-2017, except 1993 Unit
of analysis: Attack Variables: \>100 variables on location, tactics,
perpetrators, targets, and outcomes Sources: Unclassified media articles
(Note: Please interpret changes over time with caution. Global patterns
are driven by diverse trends in particular regions, and data collection
is influenced by fluctuations in access to media coverage over both time
and place.)

\#Data(variables): Iyear: This field contains the year in which the
incident occurred. Country\_txt: This field identifies the country or
location where the incident occurred Region\_txt: This field identifies
the region in which the incident occurred. City: Name of the city,
village, or town in which the incident occurred Success: Success of a
terrorist strike Suicide: 1 = “Yes” The incident was a suicide attack. 0
= “No” There is no indication that the incident was a suicide attack.
Targsubtype1\_txt: The more specific target category Nperps: The total
number of terrorists participating in the incident Nwound: Number of
confirmed non-fatal injuries to both perpetrators and victims.
Attacktype: types of attacks Gname: name of the terrorist group
(potentially be coded based on ideology)

Website:<https://www.kaggle.com/START-UMD/gtd>

Data: Global Terrorism Dataset

``` r
globalterro<-read.csv('data/415Project.csv')
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.3.2     ✓ purrr   0.3.4
    ## ✓ tibble  3.0.4     ✓ dplyr   1.0.2
    ## ✓ tidyr   1.1.2     ✓ stringr 1.4.0
    ## ✓ readr   1.4.0     ✓ forcats 0.5.0

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
problems(globalterro)
```

    ## [1] row      col      expected actual  
    ## <0 rows> (or 0-length row.names)

``` r
globalterro <- globalterro%>%select(iyear,imonth,country_txt, region_txt, city, nkill, latitude, longitude, attacktype1_txt, success, nperps,  gname )%>%filter(!is.na(iyear), !is.na(country_txt),!is.na(nkill))#clean data before each section below too
colnames(globalterro)[colnames(globalterro)=="country_txt"]<-'country'
colnames(globalterro)[colnames(globalterro)=="iyear"]<-'year'
colnames(globalterro)[colnames(globalterro)=="attacktype1_txt"]<-'attack_type'
```

``` r
#HPick 5-8 countries of interest, explore the changes across the time span 

#number of incidents
Num_I_world<-globalterro%>%group_by(year)%>%mutate(world = n())%>%ggplot(aes(year,world))+geom_line()+geom_point(shape=23)+labs(x='year',y='terrorist attacks')+theme_bw()
Num_I_world
```

![](final-report_files/figure-gfm/q2-1.png)<!-- -->

``` r
Num_incident5<-globalterro%>%filter(year>= 1991, country=='Syria'|country=='Iraq'|country== 'Afghanistan'|country== 'United States'|country=='United Kingdom'|country=='Yemen')
#Num_incident5

Num_incident5%>%group_by(country,year)%>%summarise(terrorist_attacks=n())%>%ggplot(aes(year,terrorist_attacks, color = country))+geom_line()+geom_point()+theme_bw()#the plot shows the number of terrorism incidents occured in the five countries from 1970 to 2017
```

    ## `summarise()` regrouping output by 'country' (override with `.groups` argument)

![](final-report_files/figure-gfm/q2-2.png)<!-- -->

``` r
Num_incident_isr_pal<-globalterro%>%filter(country=='Israel'|country=='West Bank and Gaza Strip')
Num_incident_isr_pal%>%group_by(country,year)%>%summarise(terrorist_attacks=n())%>%ggplot(aes(year,terrorist_attacks, color = country))+geom_line()+geom_point()+theme_bw()
```

    ## `summarise()` regrouping output by 'country' (override with `.groups` argument)

![](final-report_files/figure-gfm/q2-3.png)<!-- -->

``` r
gt_incidentworldrank<-globalterro%>%group_by(country)%>%summarize(number_of_incidents=n(),deaths=sum(nkill))%>%arrange(desc(number_of_incidents))
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
#gt_incidentworldrank

gt_incidentworldrank_before2003<-globalterro%>%filter(year<2003)%>%group_by(country)%>%summarize(number_of_incidents=n(),deaths=sum(nkill))%>%arrange(desc(number_of_incidents))
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
#gt_incidentworldrank_before2003
gt_incidentworldrank_after2003<-globalterro%>%filter(year>=2003)%>%group_by(country)%>%summarize(number_of_incidents=n(),deaths=sum(nkill))%>%arrange(desc(number_of_incidents))
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
gt_incidents_top4<-globalterro%>%filter(country=='Iraq'|country=='Pakistan'|country=='Afghanistan'|country=='India')%>%group_by(country,year)%>%summarise(number_of_incidents=n())
```

    ## `summarise()` regrouping output by 'country' (override with `.groups` argument)

``` r
#gt_incidents_top4
gt_incidents_top4%>%ggplot(aes(year,number_of_incidents, color = country))+geom_line()+geom_point()+theme_bw()
```

![](final-report_files/figure-gfm/q2-4.png)<!-- -->

``` r
gt_incidents_top3_after2003<-globalterro%>%filter(year>=2003,country=='Iraq'|country=='Pakistan'|country=='Afghanistan')%>%group_by(country)%>%summarize(terrorist_attack_after2003=n())
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
#gt_incidents_top3_after2003

gt_incidents_top3_before2003<-globalterro%>%filter(year<2003,country=='Iraq'|country=='Pakistan'|country=='Afghanistan')%>%group_by(country)%>%summarize(terrorist_attack_before2003=n())
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
#gt_incidents_top3_before2003

gt_incidents_top3_comparison<-full_join(gt_incidents_top3_after2003, gt_incidents_top3_before2003, by = "country")
gt_incidents_top3_comparison
```

    ## # A tibble: 3 x 3
    ##   country     terrorist_attack_after2003 terrorist_attack_before2003
    ##   <fct>                            <int>                       <int>
    ## 1 Afghanistan                      12175                         187
    ## 2 Iraq                             23751                         160
    ## 3 Pakistan                         12191                        1928

``` r
#number of fatalities 
gt_countries<-globalterro%>%filter(year>= 1991, country=='Syria'|country=='Iraq'|country== 'Afghanistan'|country== 'United States'|country=='United Kingdom'|country=='Yemen')%>%group_by(country, year)%>%summarize(fatalities=sum(nkill))
```

    ## `summarise()` regrouping output by 'country' (override with `.groups` argument)

``` r
#gt_countries
gt_countries%>%ggplot(aes(year, fatalities, color=country))+geom_point()+geom_line()+theme_bw()#the plot shows the fatalities rates resulted from terrorism in six countries from 1970 to 2017
```

![](final-report_files/figure-gfm/q2-5.png)<!-- -->

``` r
gt_911<-globalterro%>%filter(country=='Iraq'|country== 'Afghanistan'|country== 'United States')%>%group_by(country, year)%>%summarize(fatalities=sum(nkill))
```

    ## `summarise()` regrouping output by 'country' (override with `.groups` argument)

``` r
gt_911%>%ggplot(aes(year, fatalities, color=country))+geom_point()+geom_line()+theme_bw()
```

![](final-report_files/figure-gfm/q2-6.png)<!-- -->

``` r
gt_isr_pal<-globalterro%>%filter(country=='Israel'|country=='West Bank and Gaza Strip')%>%group_by(country, year)%>%summarize(fatalities=sum(nkill))
```

    ## `summarise()` regrouping output by 'country' (override with `.groups` argument)

``` r
gt_isr_pal%>%ggplot(aes(year, fatalities, color=country))+geom_point()+geom_line()+labs(x='year')+theme_bw()
```

![](final-report_files/figure-gfm/q2-7.png)<!-- -->

``` r
gt_fatalities_worldrank<-globalterro%>%group_by(country)%>%summarize(fatalities=sum(nkill))%>%arrange(desc(fatalities))
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
head(gt_fatalities_worldrank)#the table that contains the rank of countries based on the fatalities rates resulted from terrorism since 1970 to 2017
```

    ## # A tibble: 6 x 2
    ##   country     fatalities
    ##   <fct>            <int>
    ## 1 Iraq             78589
    ## 2 Afghanistan      39384
    ## 3 Pakistan         23822
    ## 4 Nigeria          22682
    ## 5 India            19341
    ## 6 Sri Lanka        15530

``` r
number_of_fatalities<-sum(globalterro$nkill)
gt_attacktypes<-globalterro%>%filter(attack_type!='unknown')%>%group_by(attack_type)%>%summarize(fatalities=sum(nkill), rates=fatalities/number_of_fatalities,incidents=n())%>%arrange(desc(fatalities))
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
gt_attacktypes
```

    ## # A tibble: 9 x 4
    ##   attack_type                         fatalities   rates incidents
    ##   <fct>                                    <int>   <dbl>     <int>
    ## 1 Armed Assault                           160297 0.389       40353
    ## 2 Bombing/Explosion                       157321 0.382       84322
    ## 3 Unknown                                  32381 0.0786       6567
    ## 4 Assassination                            24920 0.0605      19233
    ## 5 Hostage Taking (Kidnapping)              24231 0.0588       8610
    ## 6 Hostage Taking (Barricade Incident)       4478 0.0109        898
    ## 7 Hijacking                                 3718 0.00903       606
    ## 8 Facility/Infrastructure Attack            3642 0.00884      9788
    ## 9 Unarmed Assault                            880 0.00214      1001

``` r
gt_attacktypes%>%ggplot()+geom_bar(aes(x=rates,fill=attack_type))+coord_polar()+theme_bw()
```

    ## Warning: position_stack requires non-overlapping x intervals

![](final-report_files/figure-gfm/q3%20attack%20types-1.png)<!-- -->

``` r
#6 countries
number_of_fatalities_per_country<-globalterro%>%filter(country=='Iraq'|country== 'United States')%>%group_by(country,attack_type)%>%group_by(country)%>%summarize(fatalities=median(nkill))
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
number_of_fatalities_per_country
```

    ## # A tibble: 2 x 2
    ##   country       fatalities
    ##   <fct>              <int>
    ## 1 Iraq                   1
    ## 2 United States          0

``` r
med_attack<-globalterro%>%group_by(attack_type)%>%summarize(avg=mean(nkill))
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
med_attack
```

    ## # A tibble: 9 x 2
    ##   attack_type                           avg
    ##   <fct>                               <dbl>
    ## 1 Armed Assault                       3.97 
    ## 2 Assassination                       1.30 
    ## 3 Bombing/Explosion                   1.87 
    ## 4 Facility/Infrastructure Attack      0.372
    ## 5 Hijacking                           6.14 
    ## 6 Hostage Taking (Barricade Incident) 4.99 
    ## 7 Hostage Taking (Kidnapping)         2.81 
    ## 8 Unarmed Assault                     0.879
    ## 9 Unknown                             4.93

``` r
gt_attacktypes_6countries<-globalterro%>%filter(country=='Iraq'|country== 'Afghanistan'|country== 'United States'|country=='France'|country=='Syria'|country=='Israel')%>%group_by(country,attack_type) #rates?
gt_attacktypes_6countries%>%ggplot()+geom_bar(aes(attack_type, fill=country),position='dodge')+theme_bw()
```

![](final-report_files/figure-gfm/q3%20attack%20types-2.png)<!-- -->

``` r
globalterro911<-globalterro%>%mutate(idate=year*100+imonth)
#globalterro911
toptenc<-globalterro911%>%mutate(count_total=n())%>%group_by(country)%>%summarise(count=n())%>%arrange(desc(count))%>%head(10)
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
before911<- globalterro911%>%filter(idate <200109)%>%group_by(country)%>%summarise(cot_bf911=n())%>%arrange(desc(cot_bf911))
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
before911<-left_join(before911,toptenc,by="country")%>%mutate(num = count-cot_bf911)%>%mutate(before = cot_bf911/count)
#before911

after911<-globalterro911%>%filter(idate>=200109)%>%group_by(country)%>%summarise(cot_ps911=n())%>%arrange(desc(cot_ps911))
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
after911<-left_join(after911,toptenc,by = "country")%>%mutate(num = count-cot_ps911)%>%mutate(post = cot_ps911/count)
#after911
```

``` r
toptenc<-globalterro911%>%mutate(count_total=n())%>%group_by(country)%>%summarise(count=n())%>%arrange(desc(count))%>%head(10)
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
globalterro911<-globalterro%>%mutate(idate=year*100+imonth)
#globalterro911
before911<- globalterro911%>%filter(idate <200109)%>%group_by(country)%>%summarise(cot_bf911=n())%>%arrange(desc(cot_bf911))
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
before911<-left_join(before911,toptenc,by="country")%>%mutate(num = count-cot_bf911)%>%mutate(before = cot_bf911/count)
before911topten <- before911%>% head(10)
before911topten 
```

    ## # A tibble: 10 x 5
    ##    country        cot_bf911 count   num before
    ##    <fct>              <int> <int> <int>  <dbl>
    ##  1 Colombia            6043  7848  1805  0.770
    ##  2 Peru                5391  5457    66  0.988
    ##  3 United Kingdom      4143  5065   922  0.818
    ##  4 El Salvador         3939  3939     0  1    
    ##  5 India               3350 11740  8390  0.285
    ##  6 Spain               2714    NA    NA NA    
    ##  7 Turkey              2442  4160  1718  0.587
    ##  8 United States       2341    NA    NA NA    
    ##  9 Sri Lanka           2204    NA    NA NA    
    ## 10 France              2110    NA    NA NA

``` r
after911<-globalterro911%>%filter(idate>=200109)%>%group_by(country)%>%summarise(cot_ps911=n())%>%arrange(desc(cot_ps911))
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
after911<-left_join(after911,toptenc,by = "country")%>%mutate(num = count-cot_ps911)%>%mutate(post = cot_ps911/count)
after911topeten <- after911%>% head(10)
after911topeten
```

    ## # A tibble: 10 x 5
    ##    country     cot_ps911 count   num   post
    ##    <fct>           <int> <int> <int>  <dbl>
    ##  1 Iraq            23756 23911   155  0.994
    ##  2 Pakistan        12245 14119  1874  0.867
    ##  3 Afghanistan     12217 12362   145  0.988
    ##  4 India            8390 11740  3350  0.715
    ##  5 Philippines      4657  6694  2037  0.696
    ##  6 Thailand         3607    NA    NA NA    
    ##  7 Nigeria          3527    NA    NA NA    
    ##  8 Somalia          3277    NA    NA NA    
    ##  9 Yemen            2962    NA    NA NA    
    ## 10 Libya            1982    NA    NA NA

``` r
compare <- before911topten %>%
   mutate(country_after = after911topeten$country)%>%mutate(count_post911 = after911topeten$cot_ps911)%>%mutate(total_count = after911topeten$count)%>%mutate(num_post = after911topeten$num)%>%mutate(post = after911topeten$post)
colnames(compare)[colnames(compare)=="country_after"]<-'country'
colnames(compare)[colnames(compare)=="cot_bf911"]<-'count_before'
colnames(compare)[colnames(compare)=="count_post911"] <- 'count_after'
colnames(compare)[colnames(compare)=="count"] <- 'total_count'
colnames(compare)[colnames(compare)=="before"] <- 'before_percentage'
colnames(compare)[colnames(compare)=="post"] <- 'after_percentage'
colnames(compare)[colnames(compare)=="num"] <- 'num_before'
library(knitr)
kable(compare)
```

| country        | count\_before | total\_count | num\_before | before\_percentage | country     | count\_after | total\_count | num\_post | after\_percentage |
| :------------- | ------------: | -----------: | ----------: | -----------------: | :---------- | -----------: | -----------: | --------: | ----------------: |
| Colombia       |          6043 |         7848 |        1805 |          0.7700051 | Iraq        |        23756 |        23911 |       155 |         0.9935176 |
| Peru           |          5391 |         5457 |          66 |          0.9879054 | Pakistan    |        12245 |        14119 |      1874 |         0.8672711 |
| United Kingdom |          4143 |         5065 |         922 |          0.8179664 | Afghanistan |        12217 |        12362 |       145 |         0.9882705 |
| El Salvador    |          3939 |         3939 |           0 |          1.0000000 | India       |         8390 |        11740 |      3350 |         0.7146508 |
| India          |          3350 |        11740 |        8390 |          0.2853492 | Philippines |         4657 |         6694 |      2037 |         0.6956976 |
| Spain          |          2714 |           NA |          NA |                 NA | Thailand    |         3607 |           NA |        NA |                NA |
| Turkey         |          2442 |         4160 |        1718 |          0.5870192 | Nigeria     |         3527 |           NA |        NA |                NA |
| United States  |          2341 |           NA |          NA |                 NA | Somalia     |         3277 |           NA |        NA |                NA |
| Sri Lanka      |          2204 |           NA |          NA |                 NA | Yemen       |         2962 |           NA |        NA |                NA |
| France         |          2110 |           NA |          NA |                 NA | Libya       |         1982 |           NA |        NA |                NA |

``` r
library(tmap)
data("World")

combined1<-left_join(x=World,y=toptenc,by = c("name"="country"))
tm_shape(combined1)+tm_polygons("count", breaks = c(0,50,100,250,500,1000,2500))
```

    ## Warning: Values have found that are higher than the highest break

![](final-report_files/figure-gfm/tmap-1.png)<!-- -->

``` r
combine_bf911 <- left_join(x=World, y=before911, by = c("name" = "country"))
tm_shape(combine_bf911)+tm_polygons("cot_bf911", breaks = c(0,50,100,250,500,1000,2500))
```

    ## Warning: Values have found that are higher than the highest break

![](final-report_files/figure-gfm/tmap-2.png)<!-- -->

``` r
combine_af911 <- left_join(x=World, y=after911, by = c("name" = "country"))
tm_shape(combine_af911)+tm_polygons("cot_ps911", breaks = c(0,50,100,250,500,1000,2500))
```

    ## Warning: Values have found that are higher than the highest break

![](final-report_files/figure-gfm/tmap-3.png)<!-- -->
