#!/usr/bin/perl -w

use strict;

use File::Temp qw(tempfile);

my $verbose = 0;

sub exec_show
{
    my $cmd = shift;
#    print LOG "\ncommand:  $cmd\n\n";
    print "*";
    my ($tmp_fh,$tmp_filename) = tempfile();

    my $result = `$cmd 2>$tmp_filename`;
    if ($? != 0)
    {
        my $code = $?>>8;
        my $message = `cat $tmp_filename`; 
        print STDERR "Subcommand failed! (code $code)\n";
#        print LOG    "Subcommand failed! (code $code)\n";

        print STDERR "\n  command:  $cmd\n";

        print STDERR "\n  message:  $message\n";
#        print LOG    "\n  message:  $message\n";
#        exit($code);
	return ($code, $message)
    }
    elsif ($verbose)
    {
        print STDERR "\n\t$cmd\n\n";
    }
    close $tmp_fh;
    return (0, $result)
}

sub exec_result
{
    my $cmd = shift;
#    print LOG "\ncommand:  $cmd\n\n";

    print STDERR "\n\t$cmd\n\n" if ($verbose);

    my $result = `$cmd`;
    return $?;
}

my $failures = 0;

sub show_only
{
    my ($code, $result) = exec_show("bali-phy --test @_");
    $failures++ if ($code != 0);
}

show_only("rna.fasta");

show_only("dna.fasta");
show_only("dna.fasta --smodel=HKY");
show_only("dna.fasta --smodel=HKY+F");
show_only("dna.fasta --smodel=HKY[kappa=2]+F[pi=Freq[A=0.1,C=0.2,T=0.3,G=0.4]]");
show_only("dna.fasta --smodel=HKY+gwF");
show_only("dna.fasta --smodel=HKY+gwF+GammaRates");
show_only("dna.fasta --smodel=HKY+gwF+GammaRates[n=4]");
show_only("dna.fasta --smodel=HKY+gwF+GammaInvRates");
show_only("dna.fasta --smodel=HKY+gwF+GammaInvRates[4]");

show_only("dna.fasta --smodel=TN[kappaPur=2,kappaPyr=2]");

show_only("aa.fasta --smodel WAG");
show_only("aa.fasta --smodel WAG+F");
show_only("aa.fasta --smodel LG+DP");

show_only("codons.fasta --alphabet=Codons");
show_only("codons.fasta --alphabet=Codons --smodel=M0+F1x4");
show_only("codons.fasta --alphabet=Codons --smodel=M0[omega=1]+F1x4");
show_only("codons.fasta --alphabet=Codons --smodel=M0[GTR]+F3x4");
show_only("codons.fasta --alphabet=Codons --smodel=M3[GTR,MG94]");
show_only("codons.fasta --alphabet=Codons --smodel=M3_Test");
show_only("codons.fasta --alphabet=Codons --smodel=M8");
show_only("codons.fasta --alphabet=Codons --smodel=M8[,HKY,F61]");

show_only("codons.fasta --alphabet=Codons --smodel=HKY+fMutSel");
show_only("codons.fasta --alphabet=Codons --smodel=HKY+F+fMutSel");
show_only("codons.fasta --alphabet=Codons --smodel=HKY+gwF+fMutSel");

print "There were ${failures} failures.\n";
