#include "pow2.H"

namespace fp_scale {

  double table[max*2+1];


  void initialize() {
    table[shift] = 1.0;
    for(int i=0;i<max;i++) {
      table[shift+i+1] = table[shift+i] * 2.0;
      table[shift-i-1] = table[shift-i] * 0.5;
    }
  }

}
