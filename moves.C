#include "moves.H"
#include "myrandom.H"
#include "possibilities.H"

void move_gap(alignment& A,int species,int column1,int column2) {
  assert(A(column1,species) == alphabet::gap);
  if (column1 < column2) {
    for(int i=column1;i<column2;i++)
      A(i,species) = A(i+1,species);
  }
  else {
    for(int i=column1;i>column2;i--)
      A(i,species) = A(i-1,species);
  }
  A(column2,species) = alphabet::gap;
}


void shift_indel(alignment& A,int species,int start,int end,int direction) {
  if (direction == -1) {
    int target = (start + A.length()-1)%A.length();
    if (target > end) 
      move_gap(A,species,end,target);
    else {
      A(end,species) = A(target,species);
      A(target,species) = alphabet::gap;
    }
  }
  else {
    int target = (end + A.length()+1)%A.length();
    if (target < start)
      move_gap(A,species,start,target);
    else {
      A(start,species) = A(target,species);
      A(target,species) = alphabet::gap;
    }
  }
}

void shift_indel(alignment& A,vector<int> mask,int start,int end,int direction) {
  for(int i=0;i<mask.size();i++) 
    if (mask[i] == 1)
      shift_indel(A,i,start,end,direction);
}

int find_nth_gap(const alignment& A,int species,int n) {
  int count=0;
  int column;
  for(column=0;column<A.length();column++) {
    if (A(column,species) != alphabet::gap) continue;
    if (count == n)
      return column;
    assert(column < A.length()-1);
    count++;
  }
  assert(0);
}

int num_non_gaps(const alignment& A,int column) {
  int count=0;
  for(int i=0;i<A.num_sequences();i++) 
    if (A(column,i) != alphabet::gap)
      count++;
  return count;
}

bool valid(const alignment& A) {
  for(int column=0;column<A.length();column++)
    if (num_non_gaps(A,column)==0)
      return false;
  return true;
}

alignment swap_gap(const alignment& old) {
  std::cerr<<"swap_gap"<<endl;
  alignment A = old;

  int total_gaps = A.length()*A.num_sequences();
  for(int i=0;i<A.num_sequences();i++) 
    total_gaps -= A.seq(i).size();
  int gap = myrandom(total_gaps);

  int species = -1;
  int column = -1;
  for(int i=0;i<A.num_sequences();i++) {
    int ngaps = A.length()-A.seq(i).size();
    if (gap >= ngaps) {
      gap -=  ngaps;
      continue;
    }

    species = i;
    column = find_nth_gap(A,species,gap);
    break;
  }
  assert(species >= 0);
  assert(column >= 0);
  
  int non_gap = myrandom(A.seq(species).size());
  int column2=-1;
  int dist=0;
  for(int i=0;i<A.length();i++) {
    if (A(i,species) == alphabet::gap) continue;
    if (dist == non_gap) {
      column2 = i;
      break;
    }
    dist++;
  }
  assert(column2 >= 0);

  move_gap(A,species,column,column2);
  return A;
}

int move_length() {
  int length=1;
  int data = myrandom();
  while(data&1) {
    length++;
    data>>=1;
  }
  return length;
}

alignment swap_gap_nearby(const alignment& old) {
  std::cerr<<"swap_gap_nearby"<<endl;
  alignment A = old;

  int total_gaps = A.length()*A.num_sequences();
  for(int i=0;i<A.num_sequences();i++) 
    total_gaps -= A.seq(i).size();
  int gap = myrandom(total_gaps);

  int species = -1;
  int column = -1;
  for(int i=0;i<A.num_sequences();i++) {
    int ngaps = A.length()-A.seq(i).size();
    if (gap >= ngaps) {
      gap -=  ngaps;
      continue;
    }

    species = i;
    column = find_nth_gap(A,species,gap);
    break;
  }
  assert(species >= 0);
  assert(column >= 0);
  int l = move_length();
  int column2=-1;
  if (myrandomf()<0.5) {
    column2 = column-l;
    if (column2<0) column2 += A.length();
  }
  else {
    column2 = column+l;
    if (column2>=A.length()) column2 -= A.length();
  }

  if (A(column2,species) == alphabet::gap)
    A=swap_gap(old);
  else {
    move_gap(A,species,column,column2);
  }

  return A;
}


alignment move_indel(const alignment& old) {
  std::cerr<<"move_gap"<<endl;
  alignment A = old;

  int total_gaps = A.length()*A.num_sequences();
  for(int i=0;i<A.num_sequences();i++) 
    total_gaps -= A.seq(i).size();
  int gap = myrandom(total_gaps);

  int species = -1;
  int column = -1;
  for(int i=0;i<A.num_sequences();i++) {
    int ngaps = A.length()-A.seq(i).size();
    if (gap >= ngaps) {
      gap -=  ngaps;
      continue;
    }

    species = i;
    column = find_nth_gap(A,species,gap);
    break;
  }
  
  int start = column;
  int end = column;
  while (A((start+A.length()-1)%A.length(),species) == alphabet::gap)
    start = (start+A.length()-1)%A.length();
  while (A((end+A.length()+1)%A.length(),species) == alphabet::gap)
    end = (end+A.length()+1)%A.length();

  int direction = 1;
  if (myrandomf()<0.5) 
    direction = -1;

  shift_indel(A,species,start,end,direction);

  return A;
}

alignment move_indels(const alignment& old,const tree& T) {
  std::cerr<<"move_indels"<<endl;
  alignment A = old;


  // Find the '-' that we want to start from
  int total_gaps = A.length()*A.num_sequences();
  for(int i=0;i<A.num_sequences();i++) 
    total_gaps -= A.seq(i).size();
  int gap = myrandom(total_gaps);

  int species = -1;
  int column = -1;
  for(int i=0;i<A.num_sequences();i++) {
    int ngaps = A.length()-A.seq(i).size();
    if (gap >= ngaps) {
      gap -=  ngaps;
      continue;
    }

    species = i;
    column = find_nth_gap(A,species,gap);
    break;
  }
  
  // Decide which other species to group
  vector<int> present_leaf(A.num_sequences());
  for(int i=0;i<A.num_sequences();i++) 
    present_leaf[i] = (A(column,i) != alphabet::gap);
  TreeFunc<int> present = mark_tree(present_leaf,T);

  vector<edge> indel_edges;
  for(int i=0;i<T.num_nodes()-1;i++) {
    if (present(i) != 1) continue;

    vector<int> neighbors = get_neighbors(T[i]);

    for(int j=0;j<neighbors.size();j++) {
      int neighbor = neighbors[j];
      if (!present(neighbor))
	indel_edges.push_back(edge(i,neighbor)); // can't get added twice
    }
  }
  
  edge parent;
  for(int i=0;i<indel_edges.size();i++) {
    std::valarray<bool> mask = T.partition(indel_edges[i].node1,indel_edges[i].node2);
    if (mask[species]) {
      assert(parent.node1 == -1);
      parent = indel_edges[i];
    }
  }

  while(myrandomf()< 0.5) {
    vector<edge> children = get_children(T,parent,present);
    if (children.size() == 0) break;
    
    parent = children[0];
    if (children.size()==2 && myrandomf()<0.5)
      parent = children[1];
  }

  // Figure out which species are including in the deletion
  std::valarray<bool> d = T.partition(parent.node1,parent.node2);
  vector<int> deleted(T.leaves(),0);
  for(int i=0;i<T.leaves();i++)
    if (d[i])
      deleted[i] = 1;
  
  // Locate the beginning and end of the indel
  int start = column;
  int end = column;

  do {
    int temp = (start+A.length()-1)%A.length();
    bool ok=true;
    for(int i=0;i<deleted.size();i++)
      if (deleted[i] && A(temp,i) != alphabet::gap)
	ok = false;
    if (!ok) break;
    start = temp;
  } while (1);

  do {
    int temp = (end+A.length()+1)%A.length();
    bool ok=true;
    for(int i=0;i<deleted.size();i++)
      if (deleted[i] && A(temp,i) != alphabet::gap)
	ok = false;
    if (!ok) break;
    end = temp;
  } while (1);

  // Decide the direction
  int direction = 1;
  if (myrandomf()<0.5) 
    direction = -1;

  // Actually shift it
  shift_indel(A,deleted,start,end,direction);

  return A;
}



alignment inc_length(const alignment& old) {
  std::cerr<<"inc_length"<<endl;
  alignment A = old;
  A.changelength(A.length()+1);
  for(int i=0;i<A.num_sequences();i++) {
    int insertion_point = myrandom(A.length());
    for(int j=A.length()-1;j>=insertion_point+1;j--)
      A(j,i) = A(j-1,i);
    A(insertion_point,i) = alphabet::gap;
  }
  return A;
}

alignment dec_length(const alignment& old) {
  std::cerr<<"dec_length"<<endl;
  alignment A = old;
  if (!A.changelength(old.length()-1))
    return A;

  for(int i=0;i<A.num_sequences();i++) {
    int ngaps = old.length()-old.seq(i).size();
    int deletion_point = find_nth_gap(old,i,myrandom(ngaps));
    for(int j=deletion_point;j<A.length();j++)
      A(j,i) = old(j+1,i);
  }
  return A;
}

alignment try_move(const alignment& old,const tree& T) {
  double p = myrandomf();
  if (p<0.2)
    return move_indels(old,T);
  if (p<0.4)
    return move_indel(old);
  else if (p<0.8)
    return swap_gap(old);
  else if (p<0.9)
    return inc_length(old);
  else
    return dec_length(old);
}

alignment move(const alignment& old,const tree& T) {
  while(1) {
    alignment A = try_move(old,T);
    if (valid(A))
      return A;
  }
}
