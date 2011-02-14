#include "expression2.H"

int main()
{
  Formula f;
  polymorphic_cow_ptr<Formula> F(f);
  F->add_state_node("X");
  F->add_state_node("Y");

  {
    vector<int> indices1;
    indices1.push_back(0);
    indices1.push_back(1);
    
    F->add_computed_node(Multiply(),indices1);
  }

  {
    vector<int> indices2;
    indices2.push_back(0);
    indices2.push_back(2);
    
    F->add_computed_node(Add(),indices2);
  }

  Context CTX(F);

  CTX.set_value(0,Double(2));
  CTX.set_value(1,Double(3));

  shared_ptr<const Object> result = CTX.evaluate(3);

  
}
