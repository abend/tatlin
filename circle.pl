#!/usr/bin/perl -w

use GCode;
use List::AllUtils ':all';
#use Math::Trig ':pi';
use Math::Trig;
use Data::Dumper;
use 5.010;

my $file = $ARGV[0] || "circle.gcode";
open(my $fh, '>', $file) or die "opening file '$file': $!";

my $gcode = GCode->new($fh);

my $zbase = .13;
my $h = 8; # circle height
my $r = 20;

my @pts = circle();
#say Dumper(@pts);
$gcode->header($pts[0][0], $pts[0][1], $zbase);
base($gcode, @pts);
fence($gcode, @pts);

sub fence {
  my $gcode = shift;
  my @pts = @_;

  $gcode->comment("fence");

  for (my $i = 0; $i < @pts; ++$i) {
    my $p = $pts[$i];
    my $n = $i < $#pts ? $pts[$i + 1] : $pts[0];

    #printf "i: $i pts: %d p: %0.2f,%0.2f n: %0.2f,%0.2f\n", scalar @pts, $p->[0], $p->[1], $n->[0], $n->[1];

    $gcode->{speed} = 50;
    $gcode->{extrusion_rate} = .1;

    if ($i == 0) {
      $gcode->comment("initial post");
      $gcode->go($p->[0], $p->[1], $h, "Left strut");
      $gcode->dwell(.05);
      #$gcode->pause(2);
    }

    $gcode->comment("diagonal");
    $gcode->move($n->[0], $n->[1], $zbase);
    $gcode->go($p->[0], $p->[1], $h);
    $gcode->dwell(.05);
    #$gcode->pause(2);

    $gcode->move($n->[0], $n->[1], $h);
    $gcode->move($n->[0], $n->[1], $zbase);
    $gcode->go($n->[0], $n->[1], $h);

    $gcode->comment("top");
    $gcode->{speed} = 1500;
    $gcode->go($p->[0], $p->[1], $h);
    $gcode->dwell(.05);
    #$gcode->pause(2);

    $gcode->move($p->[0], $p->[1], $h * 2);
    $gcode->move($n->[0], $n->[1], $h * 2);

    #$gcode->move(10, .25, $zbase, 1000, "BR");
# go(0, .25, 10);
# move(10, .25, 10, 1000, "TR");
# move(10, .25, $zbase, 1000, "BR");
# go(10, .25, 10);
# dwell(.1);
# pause(5);
# $speed = 1500;
# go(0, .25, 10);
# dwell(.5);
  }
}

# header(-20, 0, $zbase);
# comment("base");
# go(0, 0, $zbase);
# go(0, -20, $zbase);
# go(.25, -20, $zbase);
# go(.25, 0, $zbase);
# go(20, 0, $zbase);
# go(20, .25, $zbase);
# go(.25, .25, $zbase);
# go(.25, 20, $zbase);
# go(0, 20, $zbase);
# go(0, .25, $zbase);

# comment("fence");
# $speed = 50;
# $extrusion_rate = .1; # extrusion amount per mm
# go(0, .25, 10, "Left strut");
# dwell(.1);
# pause(5);

# move(10, .25, $zbase, 1000, "BR");
# go(0, .25, 10);
# move(10, .25, 10, 1000, "TR");
# move(10, .25, $zbase, 1000, "BR");
# go(10, .25, 10);
# dwell(.1);
# pause(5);
# $speed = 1500;
# go(0, .25, 10);
# dwell(.5);

# move(20, .25, $zbase, 1000, "BR");
# go(10, .25, 10);
# move(20, .25, 10, 1000, "TR");
# move(20, .25, $zbase, 1000, "BR");
# go(20, .25, 10);
# dwell(.1);
# pause(5);
# $speed = 1500;
# go(10, .25, 10);
# dwell(.5);

$gcode->footer;


sub circle {
  my @p;
  my $n = 12;
  my $a = 360 / $n;
  for my $i (0..$n-1) {
    push @p, [cos(deg2rad($i * $a)) * $r, sin(deg2rad($i * $a)) * $r];
  }
  @p;
}

sub base {
  my $gcode = shift;
  my @pts = @_;

  $gcode->comment("base");

  for my $p (@pts) {
    $gcode->go($p->[0], $p->[1], $zbase);
  }

  # and connect back to the start
  $gcode->go($pts[0]->[0], $pts[0]->[1], $zbase);
}
