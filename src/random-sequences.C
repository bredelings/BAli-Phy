
int main(int argc,char* argv[]) {


  /* ----- Alphabets to try ------ */
  vector<alphabet> alphabets;
  alphabets.push_back(alphabet("DNA nucleotides","AGTC","N"));
  alphabets.push_back(alphabet("RNA nucleotides","AGUC","N"));
  alphabets.push_back(alphabet("Amino Acids","ARNDCQEGHILKMFPSTWYV","X"));
                                                                                
  /* ----- Try to load template alignment -----*/
  if (not args.set("align"))
    throw myexception("Alignment file not specified! (align=<filename>)");
 
  ifstream ifile(args["align"].c_str());
  A.load_phylip(alphabets,ifile);
                                                                                
  ifile.close();

}
