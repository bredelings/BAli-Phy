#include "generalized_tuples.H"
#include "values.H"

using namespace std;

void Values::record_changes_no_deliver(int x, message_list_t& x_changes)
{
  // If x has changed completely, then just record a single "everything has changed" message.
  if (completely_out_of_date(x))
  {
    owned_ptr<out_of_date_message_t> mp = claim(new out_of_date_message_t);
    unprocessed_messages[x].clear();
    unprocessed_messages[x].push_back(mp);
  }
  else
  // Collect the changes from this message.
    unprocessed_messages[x].splice(unprocessed_messages[x].end(), x_changes);
}

void Values::notify_x_of_change_in_slot_y(int x, int slot, const out_of_date_message_t& m)
{
  // if x is already of date, don't bother sending any more message about how its inputs have changed
  if (completely_out_of_date(x)) return;

  // Don't unshare x by calling notify_inputs_out_of_date( ) unless x will change!
  {
    // If we don't cast 'values' to a constant, then values[x] will call the non-const operator[]
    // and unshare values[x], which defeats the whole purpose of checking 'ignored'
    const std::vector<polymorphic_cow_ptr<ValueBase> >& const_values = values;

    // Quit before generating a non-const reference to values[x] if values[x] will not change value.
    if (const_values[x]->ignored(m,slot)) return;
  }

  // Notify x of the changes, and find out how x has changed.
  message_list_t x_changes = values[x]->notify_input_out_of_date(*this, m, slot);

  record_changes_no_deliver(x, x_changes);
}

void Values::process_messages()
{
  // For each index1 ...
  for(int index1=0; index1<size(); index1++)
  {
    const vector<affected_index_t>& Nodes_affected = F->affected_indices(index1);

    // ... with unprocessed messages...
    if (unprocessed_messages[index1].empty()) continue;

    // ... consider each index2 that is directly downstream ...
    for(int j=0;j<Nodes_affected.size();j++)
    {
      affected_index_t index2 = Nodes_affected[j];

      assert(index2.index > index1);

      // ... and isn't already out of date.
      if (completely_out_of_date(index2.index)) continue;
      
      message_list_t& index1_all_changes = unprocessed_messages[index1];

      message_list_t index2_all_changes; // or at least all changes resulting from from index1

      for(message_list_t::const_iterator m = index1_all_changes.begin(); 
	  m != index1_all_changes.end() and not completely_out_of_date(index2.index); m++)
      {
	notify_x_of_change_in_slot_y(index2.index, index2.slot, *(*m));
      }
    }
  }
}

void Values::mark_out_of_date(int i)
{
  if (completely_out_of_date(i))
    return;

  values[i]->mark_self_out_of_date();

  unprocessed_messages[i].clear();
  owned_ptr<out_of_date_message_t> mp = claim(new out_of_date_message_t);
  unprocessed_messages[i].push_back(mp);

  process_messages();
}

void Values::calculate_value(int index2)
{
  vector<int> indices_to_validate(1,index2);

  while(not indices_to_validate.empty())
  {
    int index1 = indices_to_validate.back();

    if (completely_up_to_date(index1)) {
      indices_to_validate.pop_back();
      continue;
    }

    const vector<int>& input_indices = F->input_indices(index1);
    if (input_indices.empty())
      throw myexception()<<"State expression "<<F->expression_for_entry(index1)<<" ["<<index1<<"] not up-to-date during computation of "<<F->expression_for_entry(index2)<<" ["<<index2<<"]!";

    bool inputs_ok = true;
    for(int i=0;i<input_indices.size();i++)
    {
      int j = input_indices[i];
      if (not completely_up_to_date(j))
      {
	indices_to_validate.push_back(j);
	inputs_ok = false;
      }
    }

    if (inputs_ok) {
      values[index1]->update(*this,input_indices);
      indices_to_validate.pop_back();
    }
  }
}

string Values::expression() const
{
  ostringstream o;
  for(int i=0;i<size();i++)
  {
    o<<F->expression_for_entry(i)<<"\n";
    o<<" ["<<i<<"] = "<<values[i]->result_expression()<<"   ";
    if (completely_up_to_date(i))
      o<<"[*]";
    else if (completely_out_of_date(i))
      o<<"[!]";
    else
      o<<"[*!]";
    if (not values[i].unique())
      o<<"  shared";
    o<<"  id = "<<F->get_id_for_index(i);
    o<<"\n";
  }
  return o.str();
}

bool Values::is_term(int i) const
{
  return F->is_term(i);
}

bool Values::is_constant(int i) const
{
  return F->is_constant(i);
}

bool Values::is_computed(int i) const
{
  return F->is_computed(i);
}

Values::Values(const Formula& f)
  :values(f.size()),
   F(f),
   unprocessed_messages(f.size())
{
  for(int i=0;i<F->size();i++)
    values[i] = F->get_new_entry_value(i);
}

