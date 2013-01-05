module GTRx3 where
{
  import SModel;
  import Distributions;
  import submodel GTR;
  main tripletA = singletToTripletExchange tripletA (GTR.main (getNucleotides tripletA))
}