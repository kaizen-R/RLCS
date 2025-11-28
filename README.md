# What this is

An R package. It implements a simple version of the "Learning Classifier System" of John H. Holland.

# How can I get it?

Great question. Provided you have the "devtools" you should be able to do:

```R
library(devtools)
install_github("kaizen-R/RLCS")
```

And then:

```R
library(RLCS)
demo_env1 <- rlcs_demo_secret1()

## Have a look and try to find the secret rule in the data
sample_of_rows <- sample(1:nrow(demo_env1), 10, replace=F)
print(demo_env1[sample_of_rows,], row.names = F)

## Defaults will work but make things a bit slow...
## Tuning some hyper-parameters makes it faster:
demo_params <- RLCS_hyperparameters(n_epochs = 400, deletion_trigger = 40, deletion_threshold = 0.9)
rlcs_model1 <- rlcs_train_sl(demo_env1, demo_params)

## That's it!
print(rlcs_model1)
plot(rlcs_model1)
```

# DEMOS

To facilitate explanations, I preferred to include a few key examples.
Have a look here: https://github.com/kaizen-R/RLCS/tree/main/demos

# RECOMMENDED: Further information

A "short" presentation (meant for a 15' intro to data scientists) is included here:
https://kaizen-r.github.io/others/RLCS_documentation_15.html

A much more detailed presentation is offered here:
https://kaizen-r.github.io/others/RLCS_documentation.html
