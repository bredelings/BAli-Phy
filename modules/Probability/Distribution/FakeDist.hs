module Probability.Distribution.FakeDist where

import Probability.Random

fake_sample_error = error "sampling from fake_dist is not allowed.  You can only observe it."
fake_range_error = error "fake_dist has no range.  You can only observe it."
fake_dist likelihoods = Distribution (\x-> likelihoods) (no_quantile "fake_dist") fake_sample_error fake_range_error
