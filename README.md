# PR-Project

This repository contains all code files regardin the Planning and Reasoning Course 24/25 held by Prof. Marrella.

The .pddl files refers to the PDDL implementation and includes:
- **domain.pddl**: domain definition 💻
- **problem_easy.pddl**: easy task definition 🥉
- **problem_mid.pddl**: mid task definition 🥈
- **problem_diff.pddl**: difficult task definition 🥇

To make them run, **ensure** having installed [enhsp-20.jar](https://drive.google.com/file/d/1GfVLQNEgeeNnNeI6HkrCtAUrrSzdSW8g/view?usp=sharing), then run, where the jar is, the following:

```
java -jar enhsp-20.jar -o [code_path]/domain.pddl -f [code_path]/problem_[type].pddl -s WAStar -h h_[h_type]
```
Change ```[code_path]``` with the actual folder location containing the files and ```[h_type]``` with ```h_ff, h_max, h_add```.


The .pl files refers to the Situation Calculus implementation and includes:
- **greenhouse_domain.pl**: domain definition 💻
- **legality_task.pl**: legality tasks definition 
- **projection_tasks.pl**: projection tasks definition 
- **astar_controller.pl**: difficult task definition
- **run_all_tasks.pl**: to run the above

To make them run, **ensure** having installed **Indigolog** in ```"C:/Prolog/indigolog/interpreters"```, and **SWIPL**. So run SWIPL terminal in the folder, and lunch:

```
?- consult(run_all_tasks).
?- run.
```
