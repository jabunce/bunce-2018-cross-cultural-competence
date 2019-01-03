# bunce-2018-cross-cultural-competence
files relating to the 2018 manuscript:

Two paths to cross-cultural competence, and one to cultural sustainability

The original preprint is on SocArXiv [here](https://osf.io/preprints/socarxiv/468ns/)


Steps to reproduce the analysis in this paper:

1) Create a project folder on your machine. Name it whatever you want.

2) Inside this project folder, put the file ``RunAll.R``

3) Also inside the project folder, create three sub-folders named (exactly) ``Code``, ``Plots``, and ``Data``

4) Inside the ``Data`` folder, put the file ``Manu_perceptions_11sep18.csv``

5) Inside the ``Code`` folder, put all the other files.

6) Open the file ``RunAll.R``. Inside it, you can set the path to your project folder. Then run its parts in order in R.

All data-dependent figures in the manuscript and appendix will appear in the ``Plots`` folder.

It can take several days to run all twenty models in this analysis to convergence. However, you can get pretty good estimates by using two mcmc chains of 1000 samples each, which should only take a few hours. Also, you can produce all the figures using just models m1, m4, m11, and m19. ``RunAll.R`` lets you fit only these models, and modify the number of mcmc chains and samples. 
