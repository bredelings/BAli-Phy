#include "computation/computation.H"
#include "mytypes.H"
#include "sequence/alphabet.H"

extern "C" closure builtin_function_m0(OperationArgs& Args)
{
  object_ptr<const Codons> C = Args.evaluate_as<Codons>(0);
  object_ptr<const SymmetricMatrixObject> S = Args.evaluate_as<SymmetricMatrixObject>(1);
  double omega = *Args.evaluate_as<Double>(2);

  object_ptr<SymmetricMatrixObject> R ( new SymmetricMatrixObject );

  R->t.resize(C->size());

  for(int i=0;i<C->size();i++) 
  {
    for(int j=0;j<i;j++) {
      int nmuts=0;
      int pos=-1;
      for(int p=0;p<3;p++)
	if (C->sub_nuc(i,p) != C->sub_nuc(j,p)) {
	  nmuts++;
	  pos=p;
	}
      assert(nmuts>0);
      assert(pos >= 0 and pos < 3);

      double rate=0.0;

      if (nmuts == 1) 
      {
	int l1 = C->sub_nuc(i,pos);
	int l2 = C->sub_nuc(j,pos);
	assert(l1 != l2);

	rate = S->t(l1,l2);

	if (C->translate(i) != C->translate(j))
	  rate *= omega;	
      }

      R->t(i,j) = R->t(j,i) = rate;
    }
  }

  return R;
}
