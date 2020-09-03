# Eyetracking

**data_simulation.R**
Data simulation script for 2x3 factorial design with the factors "fixation cross" with the levels "bulls eye" / "standard" and "fixation training" with the levels "baseline" / "notraining" / "training".

The experimental design consists of two sessions which is completed by thirty participants. Each participant does an initial "baseline" session, then half of the participants do a "training" session and half of the participants do a "no training" session. In each session, objects are presented with a "standard" fixation cross in half of the blocks, and with a "bulls eye" fixation cross in the other half of the blocks. Dependent variable is the mean number of saccades per trial.  

**data_preprocess.R**
Preprocessing of eye tracking data that is collected in an eye tracking experiment in which all the subjects did two experimental sessions("baseline" and "training"). The script loads eye movement and behavioral data, detect the number of fixations and microsaccades per trial, creates and saves data frame for each subject. Microsaccades and fixations are detected using a velocity based algorithm (Engbert & Kliegl, 2003; Malsburg, 2015). 
