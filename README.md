# Optimal Substitution Times in Football (Soccer)

## Overview
Bret Myers (2012) has proposed a decision rule for soccer substitution timings that optimizes a team’s likelihood to improve its goal differential when trailing: use the first, second and third substitutions before the 58th, 73rd and 79th minutes, respectively. A replication of Myers’ analysis with a larger dataset yielded highly similar decision points, 65-72-78. Myers reported that following the rule led to goal differential improvement 42% of times versus 20% for ‘defiers’, but did not account for any other characteristics that could affect a team’s likelihood to follow the rule or improve its situation regardless. A comparison of goal differential improvement between 509 follower-defier pairs matched on relevant pre- and in-game characteristics suggests a more modest, yet robust results: teams who followed the 65-72-78 rule improved their goal differential 32% of times versus only 21% success otherwise, plus-minus 3%. Coaches would thus benefit from following the 65-72-78 decision rule.

## Contents
The repo is divided into two: 
- v1 contains the code and report generated up to April 2019, based on which the manuscript for submission to the Journal of Quantitative Analysis in Sports was created. 
- v2 contains further improvements done from the original version, on which I currently work to prepare for actual submission. 

The main improvement is bootstrapping the derivation of decision points. The second difference is that in v1 half of the data was used to derive the rules (a training set) while the other half was used to test it (test set). In v2, I use all data for both purposes, because in fact, the derivation of the rule in independent from the estimation of its effect, as I shall justify in an updated version of the analysis. 

The bulk of the analysis is well-presented in the [manuscript](https://github.com/tomkreker/Football-Substitutions/blob/master/Manuscript%20Optimal%20substitution%20times%20in%20soccer.pdf), including the following:
1. Introduction and relevant literature
2. Replicating Myers’ decision rule
3. Limitations of the original rule’s evaluation
4. Building comparable followers-defiers groups with genetic matching
5. Estimating the effect of following the revised '65-72-78' decision rule

## Data
I replicate Myer's procedure to derive a decision rule using [data](https://www.kaggle.com/secareanualin/football-events) from matches played between the 2011/2012 and 2016/2017 seasons in five major European leagues: English Premier League, Spanish La Liga, German Bundesliga, Italian Serie A, and French Ligue 1. The main difference from Myers’ procedure was the exclusion of matches with substitutions due to injuries and matches with red cards; both suggest forced substitutions while only those from tactical reasons are of interest. In the original paper, these matches were excluded only when testing the rule but not during its derivation. After these exclusions, substitutions from 4346 matches remained. Substitutions from half of these matches during which the team was behind (n=4546) were used to derive the decision rule while the other 2173 matches were used to estimate its impact. As mentioned above, in v2 I use all matches for both purposes.

## Code Structure
The code in folder v2 is the most well-organized code that supports the analysis presented in the manuscript. It contains the following files:
1. [preprocessing.R](https://github.com/tomkreker/Football-Substitutions/blob/master/v2/preprocessing.R) - a short script that processes the original heavy dataset to generate the *data.csv* file that contains only goals and substitutions as well as the relevant columns.
2. [derive_rule.R](https://github.com/tomkreker/Football-Substitutions/blob/master/v2/derive_rule.R) - the script that performs the first part of the analysis. That is, deriving the optimal minutes for substitutions by fitting decision trees on a processed dataset in which each sub has an associated binary outcome variable of whether the team had a positive goal differential from the moment of the sub until the end of the match. After the points are derived, I use bootstrapping to obtain an estimation of uncertainty around them.
3. [test_rule.R](https://github.com/tomkreker/Football-Substitutions/blob/master/v2/test_rule.R) - the script that performs the second part of the analysis - estimating the effect of following a given decision rule for substitutions, of the form "if behind, perform sub 1/2/3 by minutes T1/T2/T3, respectivey. Conceiving of following the rule as 'treatment', the main function identifies instances of treatment and control units, the latter being teams that could have followed the rule (were behind on the relevant minutes) but did not do so. After all matches are scanned, genetic matching is used to form treatment and control groups that are similar on relevant characteristics - team stength, time behind, score at (potential) subbing point, and the decision point in which they started following the rule. After matching, the effect of following the rule can be estimated by the difference of mean outcomes for both groups, represented by binary variable that equals '1' if the team outscored their opponent from the moment they began following the rule to the end of the game (i.e. they improved their goal differential) or '0' otherwise. The treatment effect is the difference in means between the two groups.

## References
The paper by Brett Myers that served as the basis for this replication and extension:
Myers, B. R. (2012). A proposed decision rule for the timing of soccer substitutions. Journal of
Quantitative Analysis in Sports , 8, Article 9.

An extensive list of referecnces appears in the manuscript.
