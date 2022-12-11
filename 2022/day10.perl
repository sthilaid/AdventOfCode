#!/usr/bin/perl
use warnings;
use strict;

my @pixels=();
for (my $x=0; $x<240; ++$x) {
    $pixels[$x] = ".";
}

my $filename = $ARGV[0];
my $register = 1;
my $cycle = 1;
my @all_cycles = (20, 60, 100, 140, 180, 220);
my @target_cycles = (220, 180, 140, 100, 60, 20);
my @target_registers = ();

sub execute {
    my $prev_reg = $register;
    my $prev_cycle = $cycle;
    my @cmd = split(" ", $_[0]);
    my $cmdsize = scalar @cmd;
    if (($cmd[0] cmp "noop") == 0) {
        register_pixels($cycle, $register);
        $cycle = $cycle + 1;
        register_pixels($cycle, $register);
    } elsif (($cmd[0] cmp "addx") == 0) {
        register_pixels($cycle, $register);
        register_pixels($cycle+1, $register);
        $cycle = $cycle + 2;
        $register = $register + int($cmd[1]);
        register_pixels($cycle, $register);
    }
    if ($#target_cycles >= 0) {
        my $target = $target_cycles[$#target_cycles];
        if ($prev_cycle < $target && $cycle >= $target) {
            push(@target_registers, ($cycle == $target) ? $register : $prev_reg);
            pop(@target_cycles);
        }
    }
}

sub register_pixels {
    my $cycle = $_[0];
    my $register = $_[1];
    my $pixel = ($cycle-1) % 40;
    #print("cycle: $cycle, register: $register, pixel: $pixel\n");
    if ($register == $pixel || $register == $pixel-1 || $register == $pixel+1) {
        my $row = int($cycle/6);
        $pixels[$cycle-1] = "#";
    }
}

sub print_screen {
    for (my $x=0; $x<240; ++$x) {
        if (($x % 40) == 0 && $x != 0) {
            print("\n");
        }
        print("$pixels[$x]");
    }
    print("\n");
}

open(FH, '<', $filename) or die $!;
while(<FH>){
   execute($_);
}
close(FH);

my $part1 = 0;
for (my $i=0; $i<=$#target_registers; ++$i) {
    $part1 += $target_registers[$i] * $all_cycles[$i];
}
print ("part1: $part1\n");
print ("part2:\n");
print_screen();



