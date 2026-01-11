# Versions History

## Upcoming work

Text encoding for binary input compatible with RLCS for NLP tasks is currently being
studied and is potentially interesting. More RL work is needed, as right now there
is no "actual" Policy being worked out, TD, SARSA, Q or otherwise.

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
