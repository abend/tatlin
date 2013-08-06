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

$gcode->{speed} = 1500;

my @pts = circle();
#say Dumper(@pts);
$gcode->header($pts[0][0], $pts[0][1], $zbase);
base($gcode, @pts);
fence($gcode, @pts);
$zbase = $h;
$h = $h * 2;
fence($gcode, @pts);

sub fence {
  my $gcode = shift;
  my @pts = @_;

  $gcode->comment("fence");

  for (my $i = 0; $i < @pts; ++$i) {
    my $p = $pts[$i];  # current point
    my $n = $i < $#pts ? $pts[$i + 1] : $pts[0]; # next point in circle

    #printf "i: $i pts: %d p: %0.2f,%0.2f n: %0.2f,%0.2f\n", scalar @pts, $p->[0], $p->[1], $n->[0], $n->[1];

    $gcode->{speed} = 200;
    #$gcode->{extrusion_rate} = .1;

#    if ($i == 0) {
    #$gcode->comment("post");
      $gcode->go($p->[0], $p->[1], $h, "vert");
    #$gcode->dwell(.05);
      $gcode->pause(2);
#    }

    #$gcode->comment("diagonal");
    $gcode->go($n->[0], $n->[1], $zbase, "diag");
    # $gcode->go($p->[0], $p->[1], $h);
    #$gcode->dwell(.05);
    #$gcode->pause(2);

    # $gcode->move($n->[0], $n->[1], $h);
    # $gcode->move($n->[0], $n->[1], $zbase);
    # $gcode->go($n->[0], $n->[1], $h);

    # $gcode->comment("top");
    # $gcode->{speed} = 1500;
    # $gcode->go($p->[0], $p->[1], $h);
    # $gcode->dwell(.05);
    # #$gcode->pause(2);

    # $gcode->move($p->[0], $p->[1], $h * 2);
    # $gcode->move($n->[0], $n->[1], $h * 2);
  }

  $gcode->comment("layer top");
  for (my $i = 0; $i < @pts; ++$i) {
    my $p = $pts[$i];  # current point
    my $n = $i < $#pts ? $pts[$i + 1] : $pts[0]; # next point in circle

    $gcode->{speed} = 1000;
    #$gcode->{extrusion_rate} = .05;

    if ($i == 0) {
      $gcode->move($p->[0], $p->[1], $h);
    }

    $gcode->go($n->[0], $n->[1], $h);
  }
}

$gcode->footer;


sub circle {
  my @p;
  my $n = 12;
  my $a = 360 / $n;
  for my $i (0..$n-1) {
    my $angle = $i * $a;
    my $x = cos(deg2rad($angle));
    my $y = sin(deg2rad($angle));

    my $amore = $angle + $a * .1;
    my $xmore = cos(deg2rad($amore));
    my $ymore = sin(deg2rad($amore));

    my $amoremore = $angle + $a * .2;
    my $xmoremore = cos(deg2rad($amoremore));
    my $ymoremore = sin(deg2rad($amoremore));

    push @p, [$x * $r, $y * $r,
              $xmore * ($r - 5), $ymore * ($r - 5),
              $xmore * ($r + 5), $ymore * ($r + 5),
              $xmoremore * $r, $ymoremore * $r];
  }
  #say Dumper(@p);
  @p;
}

sub base {
  my $gcode = shift;
  my @pts = @_;

  $gcode->{speed} = 1000;

  $gcode->comment("base");

  for my $p (@pts) {
    my $it = natatime 2, @$p;
    while (my @xy = $it->()) {
      $gcode->go($xy[0], $xy[1], $zbase);
    }
  }

  # and connect back to the start
  $gcode->go($pts[0]->[0], $pts[0]->[1], $zbase);
}
