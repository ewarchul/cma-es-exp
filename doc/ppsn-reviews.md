# Review 1
## Score = 1 (weak)

This paper investigates variations of the “1/5-rule” for stepsize adaptation in ES algorithms.  Concretely, the paper presents three Procedures (#4-#6) that differ in the way ‘success’ is defined (ratio of the current population that exceed the fitness of a reference point).  In an extensive numerical study on the CEC13 and CEC17 benchmark set the methods are evaluated and compared against the standard cumulative step size adaptation (CSA-ES), which performs best. 1 of the investigated schemes does consistently not work well, the other 2 schemes perform well (similar as CSA-ES) on unimodal functions.  There is nothing wrong with the methodology in this paper, but the conclusions might not be that impactful as advertised: - That the 1/5-rule ‘works well’ on unimodal function was already known, as historically CSA-ES (and many other steps size adaptation rules) have been developed to address the deficiencies of the 1/5 rule on more general problems.  The experimental study allows to draw conclusions about the ‘Procedures’ as indicated in the paper. However, the procedures unfortunately do not only differ in the way success is measured (different reference point), but also in adaptation strength (see below). The effect of these differences might be stronger than the effect of choice of different reference point, so I don’t think the experiments allow to draw conclusions such as ‘we have demonstrated that the information about the population midpoint fitness may improve the efficiency of CMA-ES’.  Parameters for Procedures: The parameters for the procedures where set by referencing prior work. However, it is a bit unclear (for the reader) to which extend these parameter choices impact the performance, and whether they are ‘comparable’ for the different procedures. For instance, consider the situation when one starts optimization at the optimal solution and does not move. Then all Procedures should decrease the stepsize. However, it is not clear whether the decay factor is the same for all methods. Similar for increase, etc. Evaluating (and calibrating) the Procedures in some controlled situations/ablation study would add valuable information for helping to interpret the numerical results. (such that one is sure that only the choice of midpoint is different in these schemes, but no other essential behavior) Essentially, it seems that results in Section 4 mostly depend on ‘how fast’ stepsize is decreased/increased by these schemes, and whether the choice of comparison point has an impact cannot be decided from these experiments.

Minor:
Page 3: “(line 6).Then”
Procedure 3, what is index ‘k \lambda’?
Procedure 4: no damping is used here (compared to Procedure 2), maybe incorporating damping will slightly improve robustness?
Figure 1: the colors are not visible in b/w print

# Review 2
## Score = 0 

The main weakness of the paper is the fact that the experimental results lack statistical validation.  Furthermore, the threshold 1/5 for the success ratio, used to decide if the step ratio has to be increased or decreased, looks quite arbitrary. The fact that this fraction was previously used in reference number [7] does not authorize to use it without justification; in fact, also in reference [7] the choice was arbitrary and that reference is now rather old.  Last but not least, the English needs a general revision. No significant mistakes were found, but in several cases the construction of the sentence is odd, and rather different from the one a native would use.

## Todo

- walidacja statystyczna [x]
- eksperymenty z roznymi wartosciami p_target [x] // uzasadnienie P_target

# Review 3
## Score = 0

This paper introduces different step-size control mechanisms using what is called by the authors the population midpoint fitness. After presenting different step-size mechanisms for Evolution Strategies, the authors introduce several algorithms that they evaluate through single runs on the linear and sphere function and on CEC benchmarks.  Overall I find that the technical realization of the paper is not sufficient. I do not think that the proposed mechanisms work properly in the setting we need them to work properly, i.e. independently of the initial mean and initial step-size, for all population size, on the sphere, linear, ellipsoid, stationary sphere functions. The shown results are not enough to convince the reader that this works properly. My suspicions come from the fact that what they base the rule upon, the probability to sample offspring better that previous mean (or previous mid-point) or current mean is very small and decreasing with dimension. (This is an experiment which is easy to do using CSA which maintains a normalized step-size close to optimal.) Detailed comments: The mid point fitness is the name the authors give to the empirical mean of the population, which is an estimator for the mean from which candidate solutions are sampled. It is known that the mean is a better point that the sampled offspring, that is why it is returned as estimator for the optimum in CMA-ES. The reason why this is better is related to genetic repair (see work by Beyer for instance).  Hence designing a step-size rule based on comparing offspring to the mean seems a bit tricky as typically the probability that offspring are better than the mean is very small. This probability depends on n and on lambda. This is why it is difficult to design proper step-size adaptation based on the ideas of the paper.  In effect, I am not convinced by the algorithm proposed and the results shown fail to convince me that the step-size mechanisms are working properly for all initial set of search points, step-size, for all population size.  On the single runs on linear function and sphere: - you have to plot the step-size evolution in log-scale: I suspect that it is always decreasing which would show that this is not a proper adaptation mechanism.  you should start from different initial mean, step-size. I suspect that you might observe two modes and thus two convergence rates.  You have to investigate single runs on the ellipsoid function, starting from different initial parameters.  Why did you take $\lambda = 4n$ (i.e. change the default population). Does it mean that it fails for other population size ? Is it the only population size for which it is working ? All this needs to be discussed and explain.  The paper “How to Assess Step-Size Adaptation Mechanisms in Randomised Search” by Hansen et al. explains how to evaluate step-size adaptation. The stationary sphere is probably very relevant for the algorithms you design because as far I understand the target step-size is probably much too small.

## Todo

- eksperymenty dla roznych wartosci sigma [x]
- eksperymenty dla roznych wartosci lambda [x]
- eksperymenty dla roznych wartosci x0 [x]

# Review 4
## Score = 2

The paper investigates whether information about the population midpoint fitness can be used to improve the efficiency of CMA-ES. The authors propose three different strategies for controlling the step size and find that their efficacy depends on problem dimension.  The paper is written very clearly. The author tested their strategies on the CEC 2013 & 2017 test suites, but reported the results using ECDF curves citing COCO's performance assessment approach. Therefore I wonder why not simply use COCO and the test suites therein to test their strategies. COCO already contains results for a multitude of CMA-ES-based algorithms which would make it much easier to compare these results to the already existing ones.

## Todo

- BBOB [x]


