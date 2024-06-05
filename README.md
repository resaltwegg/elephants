# Sustainable hunting of large-tusked elephants

This model was developed to evaluate the effectiveness of different management strategies to protect large-tusked elephants in populations that are subject to trophy hunting. It is described in Maphisa et al (in prep) "Is the hunting of large tusked bull elephants sustainable?"

The R script "setupandrunsimulations.r" is used to set parameter values (overall quota, length of simulation, initial population size, demographic parameters, etc.). The rest of the script then runs the simulations using two versions of the model, one assuming dominant inheritance of the gene coding for large tusks, and the other one assuming recessive inheritance of the gene coding for large tusks.

The R scripts "dominantmodel.R" and "recessivemodel.R" contain a function that sets up the demographic models for the dominant and recessive inheritance case, respectively, and simulates population trajectories under the different management scenarios. The functions return the total population size (females and males) at the end of the simulation, the total number of large-tusked males, the number of old (>55 years) large-tusked males, the number of large-tusked males hunted over the duration of the simulation, and the number of small-tusked males hunted over the duration of the simulation.

The R script "outputs.r" gathers all the simulation outputs and produces the figures shown in the manuscript (Figure 1 and Figure A1).