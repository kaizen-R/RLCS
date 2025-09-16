\name{rlcs_train}
\alias{rlcs_train}
\title{Train an LCS with the RLCS Package}
\usage{
rlcs_train(environment_df, run_params, pre_trained_lcs = NULL, verbose = F)

environment_df: A data frame containing, specifically, one "state" and one "class" column. The "state" column MUST contain strings made of ONLY 0 and 1, such as: "00110101". This is a requirement for the current RLCS implementation.
run_params: An RLCS_hyperparameters object, for which an object construction is provided.
pre_trained_lcs: Optional. Can be used to EVOLVE a pre-trained LCS.
verbose: Default is FALSE. I strongly suggest you do not use that parameter.
}
\description{
Returns an \R \code{RLCS Model} containing the proposed model, made of several classifiers.
}
\examples{
## Generate running hyperparameters
test_params <- RLCS_hyperparameters(n_epochs = 400, deletion_trigger = 40, deletion_threshold = 0.9)
## One demo dataset for data mining scenario
test_env1 <- rlcs_secret1()
## Try to see for yourself what the dataset hides:
test_env1
## Generate the model with RLCS:
rlcs_model <- rlcs_train(test_env1, test_params)
print(rlcs_model)
plot(rlcs_model)
}
