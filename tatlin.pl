#!/usr/bin/perl -w

use List::AllUtils qw(:all);

my $file = $ARGV[0] || die "usage: $0 file.gcode";
open(my $fh, '>', $file) or  die "opening file '$file': $!";

my $speed = 2500; #1620; # mm/s
my $extrusion_rate = .05; # extrusion amount per mm
my $extruder_pos = 0;
my ($cur_x, $cur_y, $cur_z);


header(-20, 0, .13);
go(0, 0, .13);
go(0, -20, .13);
go(.25, -20, .13);
go(.25, 0, .13);
go(20, 0, .13);
go(20, .25, .13);
go(.25, .25, .13);
go(.25, 20, .13);
go(0, 20, .13);
go(0, .25, .13);

$speed = 50; #1620;
$extrusion_rate = .12; # extrusion amount per mm
go(0, .25, 10);
pause(5);
move(10, .25, .13, 1000, "go to bottom");
go(0, .25, 10);
move(10, .25, .13, 1000, "go to bottom again");
go(10, .25, 10);
pause(5);
go(0, .25, 10);

footer();


sub move {
  my ($x, $y, $z, $spd, $comment) = @_;

  $comment = $comment ? "($comment)" : '';

  #$extruder_pos += distance($cur_x - $x, $cur_y - $y, $cur_z - $z) * $extrusion_rate;

  printf $fh "M103 (Extrude off)\nG1 X%0.2f Y%0.2f Z%0.2f F%0.2f E%0.2f $comment\nM101 (Extrude on)\n", $x, $y, $z, $spd, $extruder_pos;

  ($cur_x, $cur_y, $cur_z) = ($x, $y, $z);
}

sub dwell {
  my ($secs) = @_;
  printf $fh "G4 P%d (Dwell %.1fs)\n", $secs * 1000, $secs;
}

sub pause {
  my ($secs) = @_;
  printf $fh "M103 (Extrude off)\nG4 P%d (Dwell %.1fs)\nM101 (Extrude on)\n", $secs * 1000, $secs;
}

sub go {
  my ($x, $y, $z, $comment) = @_;

  $comment = $comment ? "($comment)" : '';

  $extruder_pos += distance($cur_x - $x, $cur_y - $y, $cur_z - $z) * $extrusion_rate;

  printf $fh "G1 X%0.2f Y%0.2f Z%0.2f F%d E%0.2f $comment\n", $x, $y, $z, $speed, $extruder_pos;

  ($cur_x, $cur_y, $cur_z) = ($x, $y, $z);
}

sub distance {
  return sqrt sum(map { $_ ** 2 } @_);
}

sub header {
  my ($x, $y, $z) = @_;

  my ($anchor_x, $anchor_y) = (-118, -75);

  ($cur_x, $cur_y, $cur_z) = ($anchor_x, $anchor_y, $z);

  print $fh <<EOM;
(**** start.gcode for The Replicator, Dualstrusion! ****)
M73 P0 (enable build progress)
G21 (set units to mm)
G90 (set positioning to absolute)
M108 T0 (set extruder max speed)
M109 S60 T0 (set HBP temperature)
M104 S190 T0 (set extruder temperature)

(**** begin homing ****)
G162 X Y F2500 (home XY axes maximum)
G161 Z F1100 (home Z axis minimum)
G92 Z-5 (set Z to -5)
G1 Z0.0 (move Z to "0")
G161 Z F100 (home Z axis minimum)
M132 X Y Z A B (Recall stored home offsets for XYZAB axis)
(**** end homing ****)

G1 X$anchor_x Y$anchor_y Z150 F3300.0 (move to waiting position)
G130 X20 Y20 Z20 A20 B20 (Lower stepper Vrefs while heating)
M6 T0 (wait for toolhead, and HBP to reach temperature)
G130 X127 Y127 Z40 A127 B127 (Set Stepper motor Vref to defaults)
M108 R3.0 T0 (set extruder max speed)
G0 X$anchor_x Y$anchor_y (Position Nozzle)
G0 Z$z      (Position Height)
M108 R4.0    (Set Extruder Speed)
M101         (Start Extruder Fwd)
G4 P1500     (Dwell 1.5 sec)
(**** end of start.gcode ****)
EOM

  #($cur_x, $cur_y, $cur_z) = ($x, $y, $z);
  go($x, $y, $z, "go to first point");

# (Go to first point)
# G1 X-70 Y-60.60 Z0.9 F3300.0

#   print $fh <<EOM;
# G1 F798.0
# G1 E0.3
# G1 F3300.0
# M101
# EOM
  print $fh "\n\n";
}

sub footer {
  # my ($fh) = @_;

  $extruder_pos = sprintf "%0.2f", $extruder_pos;
  print $fh <<EOM;


(Finish up)
G1 F1200.0
G1 E$extruder_pos
(G1 F$speed prob unnecessary)
M103
G4 P15000 (Dwell - cool off)

(back up extruder .5?)

(******* End.gcode*******)
M73 P100 ( End  build progress )
G0 Z150 ( Send Z axis to bottom of machine )
M18 ( Disable steppers )
M109 S0 T0 ( Cool down the build platform )
M104 S0 T0 ( Cool down the Right Extruder )
G162 X Y F2500 ( Home XY endstops )
M18 ( Disable stepper motors )
M70 P5 ( Done )
M72 P1  ( Play Ta-Da song )
(*********end End.gcode*******)
EOM
}
