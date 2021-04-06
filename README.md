# MultiObjectiveProjects


## Summary

- A repository containing the implementation of a variety of Multi-Objective Problems with the main objective of researching the effects of multiple constraint handling techniques (CHTs) on constrained problems. 
- The main objective of this repository is to present the results published in the following paper:
  - Felipe Lima Vaz, Yuri Lavinas, Claus Aranha, and Marcelo Ladeira, **Exploring Constraint Handling Techniques in Real-world Problems on MOEA/D with Limited Budget of Evaluations**, Evolutionary Multi-Criterion Optimization (EMO), doi: https://doi.org/10.1007/978-3-030-72062-9_44 [Preprint.](https://arxiv.org/pdf/2011.09722.pdf)

- The main algorithm here is MOEA/D which is used with the R package MOEA/Dr.


## Problems
#### MAZDA Car Problem

- Benchmark Problem Based on Real-World Car Structure Design Optimization. The main idea of it is to optimize the structure of 3 cars by minimizing the total weight and maximizing the number of common thickness parts.

- Main Characteristics:
  - Discrete Problem.
  - 2 Objectives.
  - 222 Design Variables.
  - 54 Constraint Functions
- Benchmark webiste:
  - https://ladse.eng.isas.jaxa.jp/benchmark/index.html

#### Moon Landing Problem

- Benchmark problem of Lunar Lander Landing Site Selection. The goal here is to find the moon's latitude and longitude which is better to perform a landing. The objectives are minimize the number of continuous shaded days, minimize the tilt angle and maximize the total communication time.

- Main Characteristics:
  - Continuous Problem.
  - 3 Objectives.
  - 2 Design Variables.
  - 2 Constraint Functions
- Competition webiste:
  - http://www.jpnsec.org/files/competition2018/EC-Symposium-2018-Competition-English.html

#### RE problem suite

- Benchmark problem suite containing 16 unconstrained real world problems and 8 constrained adaptation of the previous 8. Contains a variety of problems with different number of objectives, constraints and design variables. There are also discrete and continuous problems withim them. Although there are 24 total problems in the benchmark, only 6 constrained will be studied with more focus which are:
  -  Two bar truss design (CRE21)
  - Welded beam design (CRE22)
  - Disk brake design (CRE23)
  - Car side impact design (CRE31)
  -  Conceptual marine design (CRE32)
  - Water resource planning (CRE51)
- Paper published:
  - "An easy-to-use real-world multi-objective optimization problem suite" by Ryoji Tanabe and Hisao Ishibuchi, 2020.
- Problem's repository:
  - https://github.com/ryojitanabe/reproblems

## Important notes

- The published paper of this research is missing equation 9 which was corrected in the [version](https://github.com/LordeFelipe/MultiObjectiveProjects/blob/master/paper.pdf) presented in this repository.
