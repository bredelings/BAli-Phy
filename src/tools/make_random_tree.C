#include "tree.H"
#include "arguments.H"
#include "util.H"
#include "rng.H"

int main(int argc,char* argv[]) { 
  Arguments args;
  args.read(argc,argv);

  unsigned long seed =0;
  if (args.set("seed")) {
    seed = convertTo<unsigned long>(args["seed"]);
    myrand_init(seed);
  }
  else
    seed = myrand_init();

  assert(args.set("names"));
  vector<string> names = split(args["names"],':');

  double branch_mean = 0.1;
  if (args.set("mean"))
    branch_mean = convertTo<double>(args["mean"]);

  SequenceTree T = RandomTree(names,branch_mean);

  std::cout<<T.write()<<std::endl;
}
