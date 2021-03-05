
## MAZDA Car Problem

- Benchmark Problem Based on Real-World Car Structure Design Optimization. The main idea of it is to optimize the structure of 3 cars by minimizing the total weight and maximizing the number of common thickness parts.
- Main Characteristics:
  - Discrete Problem.
  - 2 Objectives.
  - 222 Design Variables.
  - 54 Constraint Functions
- Benchmark webiste:
  - https://ladse.eng.isas.jaxa.jp/benchmark/index.html

## How to execute the tests

1. Acess the folder of the MAZDA problem.
2. Set the working directory to the source file location.
   1. If necessary, access https://ladse.eng.isas.jaxa.jp/benchmark/index.html, download the benchmark problem, extract the multiobjective problem for 3 cars (not 2!), compile the code and put it in the assets folder with the name "mazda_mop"
3. Open the code of the problem
   1. Choose  your execution parameters (number of generations, population size and number of executions)
   2. Configure the parameters of MOEA/D's components including the desired CHT.
4. Execute the script
5. Wait fot the execution
6. Check the created output folder with the results
   1. Use the plot functions in the data folder to better visualize your results
