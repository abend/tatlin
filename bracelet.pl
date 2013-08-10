#!/usr/bin/perl -w

# stretchy bracelet

use GCode;
use List::AllUtils ':all';
use Math::Trig;
use Data::Dumper;
use 5.010;

my $file = $ARGV[0] || "bracelet.gcode";
open(my $fh, '>', $file) or die "opening file '$file': $!";

my $gcode = GCode->new($fh);

my $zbase = .13;
my $ring_height = 2; # circle height
my $r = 15;

$gcode->{speed} = 1500;

my @pts = circle();

my $height = $ring_height;
$gcode->header($pts[0][0], $pts[0][1], $zbase);
layer_top($gcode, $zbase, @pts);

# layers
for (my $i = 0; $i < 8; ++$i) {
  @pts = circle($i);
  fence($gcode, $zbase, $height, @pts);

  $zbase = $height;
  $height += $ring_height;
}

# verts and diagonals, plus a layer on top
sub fence {
  my $gcode = shift;
  my $base = shift;
  my $h = shift;
  my @pts = @_;

  $gcode->comment("fence");

  for (my $i = 0; $i < @pts; ++$i) {
    my $p = $pts[$i];  # current point
    my $n = $i < $#pts ? $pts[$i + 1] : $pts[0]; # next point in circle

    $gcode->{speed} = 200;
    $gcode->go($p->[0], $p->[1], $h, "vert");
    $gcode->pause(2);
    $gcode->go($n->[0], $n->[1], $base, "diag");
  }

  layer_top($gcode, $h, @pts);
}

sub layer_top {
  my $gcode = shift;
  my $h = shift;
  my @pts = @_;

  $gcode->comment("layer top");
  for (my $i = 0; $i < @pts; ++$i) {
    my $p = $pts[$i];  # current point
    my $n = $i < $#pts ? $pts[$i + 1] : $pts[0]; # next point in circle

    $gcode->{speed} = 1000;

    if ($i == 0) {
      $gcode->move($p->[0], $p->[1], $h);
    }

    $gcode->go($n->[0], $n->[1], $h);
  }
}

$gcode->footer;


sub circle {
  my ($rot_offset) = @_;
  $rot_offset ||= 0;

  my @p;
  my $n = 24;
  my $a = 360 / $n;
  my $zig = 2;

  for my $i (0..$n-1) {
    my $angle = $i * $a + $rot_offset * 6;
    my $x = cos(deg2rad($angle));
    my $y = sin(deg2rad($angle));

    my $amore = $angle + $a * .1;
    my $xmore = cos(deg2rad($amore));
    my $ymore = sin(deg2rad($amore));

    my $amoremore = $angle + $a * .2;
    my $xmoremore = cos(deg2rad($amoremore));
    my $ymoremore = sin(deg2rad($amoremore));

    my $radius_offset = 0;# $rot_offset;# $zig * ($i % 2 ? -1 : 1);

    push @p, [$x * ($r + $radius_offset), $y * ($r + $radius_offset), # current point
#              $xmore * ($r - 5), $ymore * ($r - 5), # inner base point
#              $xmore * ($r + 5), $ymore * ($r + 5), # outer base point
#              $xmoremore * $r, $ymoremore * $r, # fwd base point
             ];
  }
  #say Dumper(@p);
  @p;
}
