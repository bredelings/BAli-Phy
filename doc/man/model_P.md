% model_P(1)
% Benjamin Redelings
% October 2026

# NAME

**model_P** - Estimate log marginal likelihood from sampled log likelihoods.

# SYNOPSIS

**model_P** \[OPTIONS\] < _LOG-LIKELIHOODS_

# DESCRIPTION

**model_P** estimates a model's marginal likelihood using a stabilized harmonic-mean estimator.
The marginal likelihood averages the likelihood over the model's prior distribution of
parameters and can be used to compare models. This tool estimates it from likelihood values
evaluated at parameter values sampled from the posterior distribution.

Standard input must contain whitespace-separated natural-log likelihoods, with no header or
other columns. Remove burn-in and select the likelihood values before invoking the tool.
Supply log likelihoods, not log posterior densities or likelihoods on the ordinary scale.
Results go to standard output; diagnostics go to standard error.

# OUTPUT

The output has the form:

    log P(data|M) = ESTIMATE    +- SD

Both numbers are on the natural-log scale. `ESTIMATE` is the estimated log marginal likelihood;
`SD` is the standard deviation of estimates obtained from 100 block-bootstrap resamples.
Resampling contiguous blocks retains some dependence between neighboring samples.

The reported spread describes resampling variability. It is not a posterior credible interval
and does not measure estimator bias or establish that the posterior was adequately sampled.
A nonfinite estimate or bootstrap result causes the program to report an error instead of
printing a completed numerical summary.

# EXAMPLE

Estimate from a file containing only retained log-likelihood values, with a reproducible seed:

    model_P --seed=1 < log-likelihoods.txt

# OPTIONS

**`--seed`** _NUM_
: Set the random seed for bootstrap resampling. If omitted, a seed is generated automatically.

**`--verbose`**
: Print additional diagnostics, including the random seed and input sample count, to standard error.

**`-h`**, **`--help`**
: Display help and exit.

# REPORTING BUGS

BAli-Phy online help: <https://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.
