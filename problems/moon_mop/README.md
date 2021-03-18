
#### Moon Landing Problem

- Benchmark problem of Lunar Lander Landing Site Selection. The goal here is to find the moon's latitude and longitude which is better to perform a landing. The objectives are minimize the number of continuous shaded days, minimize the tilt angle and maximize the total communication time.

- Main Characteristics:
  - Continuous Problem.
  - 3 Objectives.
  - 2 Design Variables.
  - 2 Constraint Functions
- Competition website:
  - http://www.jpnsec.org/files/competition2018/EC-Symposium-2018-Competition-English.html

## How to execute the tests

1. Access the folder of the MOON problem.
2. Set the working directory to the source file location.
   1. If necessary, access http://www.jpnsec.org/files/competition2018/EC-Symposium-2018-Competition-English.html, download the benchmark problem, extract the source code, compile it and put it in the assets folder with the name "moon_mop"
3. Open the code of the problem
   1. Choose  your execution parameters (number of generations, population size and number of executions)
   2. Configure the parameters of MOEA/D's components including the desired CHT.
4. Execute the script
5. Wait for the execution
6. Check the created output folder with the results
   1. Use the plot functions in the data folder to better visualize your results
