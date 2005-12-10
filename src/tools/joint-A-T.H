#ifndef JOINT_A_T_H
#define JOINT_A_T_H

#include <string>
#include <vector>
#include <boost/program_options.hpp>

#include "sequencetree.H"
#include "alignment.H"


//--------------------------- Class joint_A_T ----------------------------//
struct joint_A_T 
{
  int size() {return A.size();}

  std::vector<alignment> A;
  std::vector<SequenceTree> T;

  joint_A_T(const std::vector<alignment>& A1,const std::vector<SequenceTree>& T1,bool internal);
};

joint_A_T get_joint_A_T(const boost::program_options::variables_map& args,bool internal);


#endif
