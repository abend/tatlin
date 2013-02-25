package GCode;

use 5.010;

require Exporter;
our @ISA = qw(Exporter);
#@EXPORT_OK = qw(header footer move dwell comment pause go);
#%EXPORT_TAGS = (all => [@EXPORT_OK]);

use List::AllUtils ':all';
use Data::Dumper;


sub new {
  my ($class, $filehandle) = @_;

  my $self = {speed => 2500, # mm/s
              move_speed => 2500,
              extrusion_rate => .05, # extrusion amount per mm
              extruder_pos => 0,
              cur_x => undef,
              cur_y => undef,
              cur_z => undef,
              fh => $filehandle,
             };

  bless($self, $class);

  return $self;
}


sub dwell {
  my ($self, $secs) = @_;
  my $fh = $self->{fh};
  printf $fh "G4 P%d (Dwell %.1fs)\n", $secs * 1000, $secs;
}

sub comment {
  my $self = shift;

  my $fh = $self->{fh};

  print $fh "(";
  printf $fh @_;
  print $fh ")\n";
}

sub pause {
  my ($self, $secs) = @_;
  my $fh = $self->{fh};
  printf $fh "M103 (Extrude off)\nG4 P%d (Dwell %.1fs)\nM101 (Extrude on)\n",
    $secs * 1000, $secs;
}

sub go {
  my ($self, $x, $y, $z, $comment) = @_;

  $comment = $comment ? "($comment)" : '';

  $self->{extruder_pos} += distance($self->{cur_x} - $x,
                                    $self->{cur_y} - $y,
                                    $self->{cur_z} - $z) * $self->{extrusion_rate};

  my $fh = $self->{fh};

  printf $fh "G1 X%0.2f Y%0.2f Z%0.2f F%d E%0.2f $comment\n",
    $x, $y, $z, $self->{speed}, $self->{extruder_pos};

  $self->{cur_x} = $x;
  $self->{cur_y} = $y;
  $self->{cur_z} = $z;
}


sub move {
  my ($self, $x, $y, $z, $spd, $comment) = @_;

  $comment = $comment ? "($comment)" : '';

  my $fh = $self->{fh};

  printf $fh "M103 (Extrude off)\nG1 X%0.2f Y%0.2f Z%0.2f F%0.2f E%0.2f $comment\nM101 (Extrude on)\n",
    $x, $y, $z, $spd || $self->{move_speed} || $self->{speed}, $self->{extruder_pos};

  $self->{cur_x} = $x;
  $self->{cur_y} = $y;
  $self->{cur_z} = $z;
}

sub distance {
  return sqrt sum(map { $_ ** 2 } @_);
}

sub header {
  my ($self, $x, $y, $z) = @_;

  my ($anchor_x, $anchor_y) = (118, 75);

  # start the anchor at the proper corner
  $anchor_x *= -1 if $x < 0;
  $anchor_y *= -1 if $y < 0;

  $self->{cur_x} = $anchor_x;
  $self->{cur_y} = $anchor_y;
  $self->{cur_z} = $z;

  my $fh = $self->{fh};

  print $fh <<EOM;
(**** start.gcode for The Replicator, Dualstrusion! ****)
M73 P0 (enable build progress)
G21 (set units to mm)
G90 (set positioning to absolute)
M108 T0 (set extruder max speed)
M109 S70 T0 (set HBP temperature)
M104 S200 T0 (set extruder temperature)

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

  $self->go($x, $y, $z, "go to first point");

#   print $fh <<EOM;
# G1 F798.0
# G1 E0.3
# G1 F3300.0
# M101
# EOM
  print $fh "\n\n";
}

sub footer {
  my ($self) = @_;

  $self->{extruder_pos} = sprintf "%0.2f", $self->{extruder_pos};

  my $fh = $self->{fh};

  print $fh <<EOM;


(Finish up)
G1 F1200.0
G1 E$self->{extruder_pos}
(G1 F$self->{speed} prob unnecessary)
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

1;
