# Known issues

## Blocked bootstrap and a validation split leak into each other

**Not triggered by any current analysis.** `dnn_ssf` is used either with `bootstrap` (scenario 1,
no validation set) or with `validation` (scenario 3 rerun, no bootstrap), never with both.

Found 2026-09-08 by reading cito 1.2 (`R/dnn.R`), not by a failure.

`dnn_ssf` forwards `bootstrap`, `bootstrap_blocking_variable`, `validation` and `early_stopping` to
`cito::dnn` through `...`. The blocked resampling itself is correct for an SSF: `dnn.R:498-506`
draws whole strata with replacement and keeps each stratum's rows contiguous, so every bootstrap
replicate is a valid stratified dataset and the conditional-logit reshape stays aligned.

The defect is that `dnn.R:516` (serial) and `dnn.R:554` (parallel) forward `validation` unchanged
into each replicate, where it is applied to the **resampled** data. Validation indices are defined
on the original row order, and after resampling those positions hold different strata. Worse, 200
strata drawn from 200 with replacement duplicate roughly 26 % of draws, so one original stratum can
appear in the training and the validation set of the same replicate. The validation loss is then not
out of sample and `early_stopping` selects a later epoch than it should — the direction that inflates
the conditional effects, because the conditional-logit loss has no stopping point of its own.

Consequence: bootstrapped standard errors combined with early stopping would be optimistic, silently.
Nothing errors.

Fix direction: draw the validation strata from the resampled stratum list inside each replicate
rather than passing outer-order indices, i.e. let the blocking variable govern the split as well as
the resample. Until then, do not combine `bootstrap` with `validation` in a `dnn_ssf` call.

Related, same code path: the blocked index vector is built with `sapply(...) |> c()`, which returns
a usable vector only because every stratum has the same number of rows. With unequal strata `sapply`
returns a list and the indexing breaks. Fixed `n_control` makes this safe for simulated data and for
`amt`-built tracks.
