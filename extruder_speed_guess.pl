#!/usr/bin/perl -w

use 5.010;
use List::AllUtils qw(:all);

my ($lx,$ly,$lz,$le);

while (<STDIN>) {
  chomp;
  my ($x,$y,$z,$f,$e) = /G1 X([-\.\d]+) Y([-\.\d]+) Z([-\.\d]+) F([-\.\d]+) E([-\.\d]+)/;
  next unless $x;

  #unless ($lx) {
    my $dist = distance($lx - $x, $ly - $y, $lz - $z);
    my $per = ($le - $e) / $dist;

    printf "$x,$y\te:%0.2f\td:%0.2f extruded %0.2f per mm\n",$e,$dist,$per;
  #}

  $lx = $x;
  $ly = $y;
  $lz = $z;
  $le = $e;
}



sub distance {
  return sqrt sum(map { $_ ** 2 } @_);
}
