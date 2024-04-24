module PopGen where

import Probability
import Range
import Bio.Alignment.Matrix

data VVI -- Vector<Vector<int>>

foreign import bpcall "PopGen:read_phase_file" builtin_read_phase_file :: CPPString -> IO (EVector (EVector Int))
foreign import bpcall "PopGen:read_phase2_file" builtin_read_phase2_file :: CPPString -> IO (EVector (EVector Int))
foreign import bpcall "PopGen:remove_2nd_allele" remove_2nd_allele :: EVector a -> EVector a
foreign import bpcall "PopGen:allele_frequency_spectrum" allele_frequency_spectrum :: EVector Int -> EVector Int

read_phase_file filename = fmap (map list_from_vector . list_from_vector) $ builtin_read_phase_file (list_to_string filename)

read_phase2_file filename = fmap (map list_from_vector . list_from_vector) $ builtin_read_phase2_file (list_to_string filename)

----------------------------------
foreign import bpcall "PopGen:ewens_sampling_probability" ewens_sampling_probability :: Double -> EVector Int -> LogDouble

data AFS = AFS Double

instance Dist AFS where
    type Result AFS = EVector Int
    dist_name _ = "afs"

instance HasAnnotatedPdf AFS where
    annotated_densities (AFS args) = make_densities $  ewens_sampling_probability args

afs args = AFS args

----------------------------------
foreign import bpcall "PopGen:ewens_sampling_group_probability" ewens_sampling_group_probability :: Double -> VVI -> LogDouble
data AFSGroup = AFSGroup Double

instance Dist AFSGroup where
    type Result AFSGroup = VVI
    dist_name _ = "afsGroup"

instance HasAnnotatedPdf AFSGroup where
    annotated_densities (AFSGroup args) = make_densities $ ewens_sampling_group_probability args


afsGroup args = AFSGroup args

---------------------------------
foreign import bpcall "PopGen:ewens_sampling_mixture_probability" builtin_ewens_sampling_mixture_probability :: EVector Double -> EVector Double -> VVI -> LogDouble
ewens_sampling_mixture_probability thetas ps x = builtin_ewens_sampling_mixture_probability (list_to_vector thetas) (list_to_vector ps) x

data AFSMixture = AFSMixture [Double] [Double]

instance Dist AFSMixture where
    type Result AFSMixture = VVI
    dist_name _ = "afsMixture"

instance HasAnnotatedPdf AFSMixture where
    annotated_densities (AFSMixture thetas ps) = make_densities $ ewens_sampling_mixture_probability thetas ps


afsMixture thetas ps = AFSMixture thetas ps

----------------------------------------
data SelfingCoalescence = SelfingCoalescence Int Double

foreign import bpcall "PopGen:selfing_coalescence_probability" builtin_selfing_coalescence_probability :: Int -> Double -> EVector Int -> LogDouble
selfing_coalescence_probability n_loci s i = builtin_selfing_coalescence_probability n_loci s (list_to_vector i)

instance Dist SelfingCoalescence where
    type Result SelfingCoalescence = [Int]
    dist_name _ = "selfing_coalescence"

instance HasAnnotatedPdf SelfingCoalescence where
    annotated_densities (SelfingCoalescence n_loci s) = make_densities $ selfing_coalescence_probability n_loci s

selfing_coalescence n_loci s = SelfingCoalescence n_loci s

----------------------------------------

data LiStephens2003 = LiStephens2003 (EVector Int) [(Double,Double,Double)]

foreign import bpcall "SMC:" li_stephens_2003_composite_likelihood_raw :: EVector Int -> EVector (EVector Double) -> AlignmentMatrix -> LogDouble

li_stephens_2003_composite_likelihood sites rhoFunc alignment = li_stephens_2003_composite_likelihood_raw sites rhoFuncRaw alignment
    where rhoFuncRaw = list_to_vector [list_to_vector [x,start,end] | (x,start,end) <- rhoFunc]

instance Dist LiStephens2003 where
    type Result LiStephens2003 = AlignmentMatrix
    dist_name _ = "li_stephens_2003"

instance HasAnnotatedPdf LiStephens2003 where
    annotated_densities (LiStephens2003 locs rho) = make_densities $ li_stephens_2003_composite_likelihood locs rho

li_stephens_2003 locs rho = LiStephens2003 (list_to_vector locs) rho


----------------------------------------

data WilsonMcVean2006 = WilsonMcVean2006 [(Double,Double,Double)]

foreign import bpcall "SMC:" wilson_mcvean_2006_composite_likelihood_raw :: Matrix -> EVector (EVector Double) -> Double -> AlignmentMatrix -> LogDouble

wilson_mcvean_2006_composite_likelihood q rhos theta alignment = wilson_mcvean_2006_composite_likelihood_raw q rhosRaw theta alignment
    where rhosRaw = list_to_vector [list_to_vector [x,start,end] | (x,start,end) <- rhos]

instance Dist WilsonMcVean2006 where
    type Result WilsonMcVean2006 = AlignmentMatrix
    dist_name _ = "wilson_mcvean_2006"

instance HasAnnotatedPdf WilsonMcVean2006 where
    annotated_densities (WilsonMcVean2006 q rhos theta) = make_densities $ wilson_mcvean_2006_composite_likelihood q rhos theta

wilson_mcvean_2006 q rhos theta = WilsonMcVean2006 q rhos theta


