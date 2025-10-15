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
test_params <- RLCS_hyperparameters(n_epochs = 280, deletion_trigger = 40, deletion_threshold = 0.9)
test_env1 <- rlcs_secret1()
# test_env1
rlcs_model1 <- rlcs_train(test_env1, test_params, NULL, F)
print(rlcs_model1)
```

# DEMOS

To facilitate explanations, I preferred to include a few key examples.
Have a look here: https://github.com/kaizen-R/RLCS/tree/main/demos

# RECOMMENDED: Further information

A "short" presentation (meant for a 15' intro to data scientists) is included here:
https://kaizen-r.github.io/others/RLCS_documentation_15.html

A much more detailed presentation is offered here:
https://kaizen-r.github.io/others/RLCS_documentation.html
