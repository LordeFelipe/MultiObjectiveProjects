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

## How to execute the tests

1. Access the folder of the desired CRE problem.
2. Set the working directory to the source file location.
   1. If necessary, access the repository https://github.com/ryojitanabe/reproblems, clone it, extract the desired source code, compile it and put it in the selected CRE problem with the name "example" (default name)
3. Open the code of the problem
   1. Choose  your execution parameters (number of generations, population size and number of executions)
   2. Configure the parameters of MOEA/D's components including the desired CHT.
4. Execute the script
5. Wait for the execution
6. Check the created output folder with the results
   1. Use the plot functions in the data folder to better visualize your results


