#include <iostream>
#include <string>
#include <vector>
#include <cassert>

using namespace std;

string getvalue(const string& line,int pos1) {
  int pos2 = pos1;

  while(pos2<line.size() and line[pos2]!=' ')
    pos2++;

  return line.substr(pos1,pos2-pos1);
}


int main(int argc,char* argv[]) { 
  vector<string> patterns;
  
  for(int i=1;i<argc;i++)
    patterns.push_back(argv[i]);

  for(int i=0;i<patterns.size();i++)
    patterns[i] += " = ";

  assert(patterns.size() >= 0);

  string line;
  vector<int> matches(patterns.size());  
  while(getline(cin,line)) {
    bool linematches=true;
    for(int i=0;i<patterns.size();i++) {
      matches[i] = line.find(patterns[i]);
      //      cout<<"   "<<patterns[i]<<": "<<matches[i]<<endl;
      if (matches[i] == -1) {
	linematches=false;
	break;
      }
    }
    if (not linematches) continue;

    for(int i=0;i<patterns.size();i++) {
      cout<<getvalue(line,matches[i] + patterns[i].size());
      if (i != patterns.size()-1)
	cout<<" ";
    }
    cout<<"\n";
  }

}
