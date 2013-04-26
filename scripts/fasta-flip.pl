#!/usr/bin/perl

$taxa = -1;

while($line = <>)
  {
    if($line =~ />([^ ]*)/ || $line =~ />([^ ]*) .*/)
      {    
	$taxa++;
	$name = $1;
	$name =~ s/[\s+|\(|\)|\,|;]/_/g;
	$name =~ s/,//g;
	$name =~ s/>//g;
	$taxonNames[$taxa] = $line;
      }
    else
      {
	$seq = $line;
	$seq =~ s/\s+//g;
	$sequences[$taxa] = $sequences[$taxa].$seq;
      }      
  }

close(F);

for($i = 0; $i <= $taxa; $i++)
{
    print $taxonNames[$i];
    $sequences[$i] = reverse $sequences[$i];
    print $sequences[$i]."\n";
}
