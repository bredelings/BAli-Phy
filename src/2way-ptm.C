#include "2way-ptm.H"

#include <vector>

using std::vector;

// We want to decrease 
// (a) the number of times get_counts( ) is called
// (b) the number of times seqlength( ) is called
// (c) the number of times log( ) is called
// This should give a further 6% speedup.

vector<vector<int> > get_type_sequences(const alignment& A, const TransducerIndelModel& T)
{
  vector<vector<int> > sequences(A.n_sequences());

  const ublas::matrix<int>& type_note = A.note(1);
  
  for(int i=0;i<sequences.size();i++)
  {
    sequences[i].reserve(A.length()+2);
    sequences[i].push_back(0);
    for(int j=0;j<A.length();j++)
      if (A.character(j,i))
	sequences[i].push_back(1+type_note(j,0));
    sequences[i].push_back(T.n_letters()+1);
  }

  return sequences;
}

vector<int> get_type_sequence(const alignment& A,int n,const TransducerIndelModel& T)
{
  vector<int> sequence;

  const ublas::matrix<int>& type_note = A.note(1);
  
  sequence.reserve(A.length()+2);

  // 0 for start (?)
  sequence.push_back(0);

  // 1,2..L for letters
  for(int i=0;i<A.length();i++)
    if (A.character(i,n))
      sequence.push_back(1+type_note(i,0));

  // L+1 for end
  sequence.push_back(T.n_letters() + 1);

  return sequence;
}

ublas::matrix<int> get_root_transition_counts(const alignment& A,int n,const TransducerIndelModel& T)
{
  ublas::matrix<int> counts(T.n_letters()+2,T.n_letters()+2);
  counts.clear();

  const ublas::matrix<int>& type_note = A.note(1);
  
  const int START = 0;
  const int END = T.n_letters()+1;

  // 0 for letters
  int p = START;

  // 1,2..L for letters
  for(int c=0; c<A.length(); c++)
    if (A.character(c,n)) 
    {
      int s = 1+type_note(c,0);
      counts(p,s)++;
      p = s;
    }

  // L+1 for end
  counts(p,END)++;

  return counts;
}

boost::shared_ptr<transducer_state_info> SS;

vector<int> get_FS_state_path(const alignment& A,int n1, int n2, const indel::PairTransducer& PTM)
{
  if (not SS)
    SS = boost::shared_ptr<transducer_state_info>(new transducer_state_info(PTM));

  const ublas::matrix<int>& M = SS->M;
  const ublas::matrix<int>& D = SS->D;
  const ublas::matrix<int>& I = SS->I;

  const ublas::matrix<int>& type_note = A.note(1);

  /******* Construct the type-of-next-residue-in-sequence-n1 array for sequence n1  ************/
  vector<int> next_type(A.length(),-1);

  int last_type = PTM.n_letters(); // the END state.
  for(int c=A.length()-1; c >= 0; c--)
  {
    next_type[c] = last_type;
    if (A.character(c,n1))
      last_type = type_note(c,0);
  }

  
  vector<int> path; path.reserve(A.length());

  // add start state
  path.push_back(PTM.start_state());

  for(int i=0;i<A.length();i++) 
  {
    bool c1 = A.character(i,n1);
    bool c2 = A.character(i,n2);
    if (not c1 and not c2) continue;

    int t1 = type_note(i,0);
    int t2 = next_type[i];
    int s=-1;

    if (c1 and c2)
      s = M(t1,t2);

    if (c1 and not c2)
      s = D(t1,t2);

    if (not c1 and c2)
      s = I(t1,t2);

    assert(s != -1);
    path.push_back(s);
  }

  // add end state
  path.push_back(PTM.end_state());

  return path;
}

ublas::matrix<int> get_FS_counts(const alignment& A,int n1, int n2, const indel::PairTransducer& PTM)
{
  ublas::matrix<int> counts(PTM.n_states(), PTM.n_states());
  counts.clear();

  if (not SS)
    SS = boost::shared_ptr<transducer_state_info>(new transducer_state_info(PTM));

  const ublas::matrix<int>& M = SS->M;
  const ublas::matrix<int>& D = SS->D;
  const ublas::matrix<int>& I = SS->I;

  const ublas::matrix<int>& type_note = A.note(1);

  /******* Construct the type-of-next-residue-in-sequence-n1 array for sequence n1  ************/
  vector<int> next_type(A.length(),-1);

  int last_type = PTM.n_letters(); // the END state.
  for(int c=A.length()-1; c >= 0; c--)
  {
    next_type[c] = last_type;
    if (A.character(c,n1))
      last_type = type_note(c,0);
  }

  
  // add start state
  int last_state = PTM.start_state();
  for(int i=0;i<A.length();i++) 
  {
    bool c1 = A.character(i,n1);
    bool c2 = A.character(i,n2);
    if (not c1 and not c2) continue;

    int t1 = type_note(i,0);
    int t2 = next_type[i];
    int s=-1;

    if (c1 and c2)
      s = M(t1,t2);

    if (c1 and not c2)
      s = D(t1,t2);

    if (not c1 and c2)
      s = I(t1,t2);

    assert(s != -1);

    counts(last_state,s)++;
    last_state = s;
  }

  // add the transition to the end state
  counts(last_state,PTM.end_state())++;

  return counts;
}

