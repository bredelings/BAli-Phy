module PopGen where
{
import Distributions;

readPhaseFile = builtinReadPhaseFile . listToString;

ewensSamplingMixtureProbability (thetas,ps) x = builtinEwensSamplingMixtureProbability (listToVectorDouble thetas) (listToVectorDouble ps) x;

afs args = (ProbDensity (ewensSamplingProbability args) (error "afs has no quantile") () ());

afsGroup args = (ProbDensity (ewensSamplingGroupProbability args) (error "afs has no quantile") () ());

afsMixture args = (ProbDensity (ewensSamplingMixtureProbability args) (error "afs has no quantile") () ());
}