#!/usr/bin/perl
use warnings;
use strict;

# sub parse_dir {
#     my $total_size = 0;
#     return $total_size;
# }

my $filename = 'day7.testinput';

open(FH, '<', $filename) or die $!;

my @dirs=();

while(<FH>){
   push(@dirs, $_);
}

for(@dirs) {
    print $_
}

@dirs[50] = "test"

close(FH);
