module PopGen where
{
import Distributions;

readPhaseFile = builtinReadPhaseFile . listToString;

ewensSamplingMixtureProbability (thetas,ps) x = builtinEwensSamplingMixtureProbability (listToVectorDouble thetas) (listToVectorDouble ps) x;

afs args = (ProbDensity "afs" ewensSamplingProbability (error "afs has no quantile") () (),args);

afsGroup args = (ProbDensity "afsGroup" ewensSamplingGroupProbability (error "afs has no quantile") () (),args);

afsMixture args = (ProbDensity "afsMixture" ewensSamplingMixtureProbability (error "afs has no quantile") () (),args);
}