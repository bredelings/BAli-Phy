module F3x4 where
{
  import Distributions;
  import SModel;
  import Alphabet;
  import submodel PlusF as Site1;
  import submodel PlusF as Site2;
  import submodel PlusF as Site3;
  main T = ReversibleFrequency T (iotaUnsigned (alphabetSize T)) pi (plus_gwF T 1.0 pi) where 
      {pi1 = listToVectorDouble Site1.main,
       pi2 = listToVectorDouble Site2.main,
       pi3 = listToVectorDouble Site3.main,
       pi  = f3x4_frequencies T pi1 pi2 pi3};

}