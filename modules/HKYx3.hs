module HKYx3 where
{
  import SModel;
  import Distributions;
  import submodel HKY;
  main = \tripletA -> singletToTripletExchange (HKY.main (getNucleotides tripletA))
}