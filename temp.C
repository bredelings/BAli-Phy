#include "clone.H"
#include <iostream>
#include "alphabet.H"

using namespace std;

 

class Pet:public Cloneable { 
public: 
  Pet* clone() const {return new Pet(*this);}

  virtual ~Pet(){}
};

class Dog : public Pet {
};

class Cat : public Pet {
};

 

int main() {
  Pet* b = new Cat; // Upcast

  // Try to cast it to Dog*:

  Dog* d1 = dynamic_cast<Dog*>(b);

  // Try to cast it to Cat*:

  Cat* d2 = dynamic_cast<Cat*>(b);

  cout << "d1 = " << (long)d1 << endl;

  cout << "d2 = " << (long)d2 << endl;


  alphabet* A = new Codons(DNA());

  RNA* a1 = dynamic_cast<RNA*>(A);

  DNA* a2 = dynamic_cast<DNA*>(A);

  const Codons* a3 = dynamic_cast<const Codons*>(A);

  cout << "a1 = " << (long)a1 << endl;

  cout << "a2 = " << (long)a2 << endl;

  cout << "a3 = " << (long)a3 << endl;

} 
