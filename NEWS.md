# Versions History

## Upcoming work

Text encoding for binary input compatible with RLCS for NLP tasks is currently being
studied and is potentially interesting. More RL work is needed, as right now there
is no "actual" Policy being worked out, TD, SARSA, Q or otherwise.
Not included in the code itself in current version, although in the works (I maintain a separate WIP folder), 
are complicated demos and tests for hierarchical setups of multiple connected RLCS models, 
several performance testing setups (profvis, microbenchmark), tests for NLP 
applications (data encoding is not easy, as mentioned above), and an example RL setup 
with RLCS to optimise the RLCS hyperparameters themselves (!).

## v0.1.6

Version v0.1.6 implements new RCpp functions and most importantly also now uses
"environment()" for faster passing of arguments across functions.


## v0.1.5

Version v0.1.5 is a half-rework which in latest tests improves performance by 30 to 50%.
Normal calls do not implement the full set of improvements just yet.
Some of the most complicated demos do call the new version of the code, but it is mostly experimental.
It uses environments passing (instead of variables). Correct set identification is improved
by caching LCS vector of actions. sapply() calls have been mostly replaced by vapply().
And a few other changes.
Next version should be mostly about code clean-up.

## v0.1.4

Version v0.1.4 is supposed to be a bit more efficient than 0.1.3. Moreover, code
is prepared to support a concept of coverage per rule, which might help influence
subsumption in the future (but that new part is rather slow).
Parallel processing overhead still needs to be reviewed.
Not included in the examples per-se, but worked out separately, some RL is now
being used to test and choose RLCS Hyperparameters for some of the SL demos.

## v0.1.3

Version v0.1.3 now adds natively the option to do horizontal splits of the datasets to train sub-LCS agents. A cleaner use of matrices without
parent env overwrites and a corresponding change in RLCS class was added.
RL scenario being re-worked, stable but can be made faster.

## v0.1.2

Version v0.1.2 added support for (where available) foreach/doParallel, using validation sets to compare several agents trained in parallel, and more demos. It also
includes a much (much) faster matching step, which has a true positive impact on speed. The RLCS "rosetta stone" functionality also
makes it much easier to leverage RLCS on numerical data.

## v0.1.1

Essentially included more demos.

## v0.1.0

I guess it was the first "stable" version.
