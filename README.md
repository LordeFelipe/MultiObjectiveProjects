# MultiObjectiveProjects


## Summary

- A repository containing the implementation of a variety of Multi-Objective Problems with the main objective of researching the effects of multiple constraint handling techniques (CHTs) on constrained problems. 
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

#### Wind Turbine Design Problem

- Benchmark problem of wind turbine design optimization problem. The idea is to optimize the parts of a wind turbine in order to increase the annual power production, minimize the average annual cost, minimize the tower base load, reduce the blade tip speed and minimize the fatigue damage.

- Main Characteristics:
  - Continuous Problem.
  - 5 Objectives.
  - 32 Design Variables.
  - 22 Constraint Functions
- Competition webiste:
  - http://www.jpnsec.org/files/competition2019/EC-Symposium-2019-Competition-English.html

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

## How to Execute the tests

**Better tutorial coming soon!!**

1. Acess the folder of the desired problem.
2. Set the working directory to the source file location
3. On the top of the code, the number of generations and population size can be changed
4. All the components can be changed further down the code
5. Just run the code

##### Observations

3. For the problems with 3+ objectives, the population is set to a number a lot lower than 300 which is because of the way SLD decomposition method works. For example, in the 3 objective problems 325 subproblems are created with 25 as the input. This is temporary and i will probably switch it to Unifom Design.


