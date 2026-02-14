context("popgraph.R")

# Theoretical Check (not needed in production but good for tests):
# H <- diag(K) - (1/K) * matrix(1, K, K)
# expect_equal(H %*% H, H) # This is the idempotent property
