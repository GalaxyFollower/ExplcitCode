#!/usr/bin/perl -w

use strict 'vars';
use strict 'subs';
use Getopt::Long;

sub AcceptOneWater;
sub GenerateOneWater;
sub PrintHelp;
sub PrintSummary;
sub SetDefaults;
sub ShiftOneWater;
sub WriteCONFIG;
sub WriteXYZ;

my ($format, $info, $help, $model, $N, $rho, $smear, $stats);
my ($xfree, $xsize, $yfree, $ysize, $zfree, $zsize);

my ($oh1x, $oh2x, $oh2y);
my ($Mgen, $Mreq, $rhogen, $Vtot);
my ($L, $Lx, $Ly, $Lz, $xsteps, $ysteps, $zsteps);
my (@TYPE, @X, @Y, @Z);
my ($h1x, $h1y, $h1z, $h2x, $h2y, $h2z, $ox, $oy, $oz);
my ($a, $b, $c, $i, $id, $xx, $yy, $zz);
my $accept;

my $NA = 6.022141793e+23;
my $MW = 18.0152833;

SetDefaults();

GetOptions (
   'format=s' => \$format,
   'header!' => \$info,
   'help' => \$help,
   'model=s' => \$model,
   'rho=f' => \$rho,
   'smear=f' => \$smear,
   'start=i' => \$N,
   'stats' => \$stats,
   'xfree=f' => \$xfree,
   'xsize=f' => \$xsize,
   'yfree=f' => \$yfree,
   'ysize=f' => \$ysize,
   'zfree=f' => \$zfree,
   'zsize=f' => \$zsize
);

if ($help) {
   PrintHelp();
   exit;
}

if ($model eq "TIP3P") {
   $oh1x = 0.957200000000000;
   $oh2x = -0.239987208409034;
   $oh2y = 0.926627206485995;
}     else {
   print "The requested water model $model is not supported. See help for correct usage.\n";
   exit;
}

if ($N < 1) {
   print "Cannot start numbering from the specified number. Resetting to default...\n";
   $N = 1;
}

if ($rho<=0.0 || $rho>=2.0) {
   print "The requested density is either too small or too large to be handled by this script.
See help for correct usage.\n";
   exit;
}

if ($xfree<0.0 || $xsize<0.0 || $yfree <0.0 || $ysize<0.0 || $zfree<0.0 || $zsize<0.0 ||
    $xfree>$xsize-3.0 || $yfree>$ysize-3.0 || $zfree>$zsize-3.0) {
   print "Cannot have a negative dimension. Resetting all dimensions to defaults...\n";
   $xfree = 0.0000;     $xsize = 10.0000;
   $yfree = 0.0000;     $ysize = 10.0000;
   $zfree = 0.0000;     $zsize = 10.0000;
}

$Vtot = ($xsize-$xfree) * ($ysize-$yfree) * ($zsize-$zfree);
$Mreq = int (($NA * $Vtot * 1.0e-24 * $rho / $MW) + 1);
$L = ($Vtot/$Mreq) ** (1.0/3.0);
$xsteps = int ((($xsize-$xfree)/$L) + 1);
$ysteps = int ((($ysize-$yfree)/$L) + 1);
$zsteps = int (($zsize-$zfree)/$L);

$Lx = ($xsize-$xfree) / $xsteps;
$Ly = ($ysize-$yfree) / $ysteps;
$Lz = ($zsize-$zfree) / $zsteps;

$id = 0;
for ($xx=0; $xx<$xsteps; $xx++) {
   for ($yy=0; $yy<$ysteps; $yy++) {
      for ($zz=0; $zz<$zsteps; $zz++) {
         GenerateOneWater();
         ShiftOneWater();
         AcceptOneWater();
         if ($accept) {
            $TYPE[$id] = "Ow";     $TYPE[$id+1] = "Hw";     $TYPE[$id+2] = "Hw";
            $id += 3;
         }
      }
   }
}

if ($stats) {
   PrintSummary();
}

if ($format eq "CONFIG") {
   WriteCONFIG();
}     elsif ($format eq "xyz") {
   WriteXYZ();
}     else {
   print "The requested file format $format is not supported. See help for correct usage.\n";
}

sub AcceptOneWater {
   $accept = 1;
   for ($i=$id; $i<$id+3; $i++) {
      if ($X[$i]<$xfree || $Y[$i]<$yfree || $Z[$i]<$zfree ||
          $X[$i]>$xsize || $Y[$i]>$ysize || $Z[$i]>$zsize) {
         $accept = 0;
      }
   }
}

sub GenerateOneWater {
   $a = $smear * rand();
   $b = $smear * rand();
   $c = $smear * rand();

   if ($a <= 0.3334*$smear) {
      $ox = $a;     $oy = $b;     $oz = $c;

      if ($b <= 0.3334*$smear) {
         $h1x = $ox + $oh1x;     $h1y = $oy;     $h1z = $oz;
         $h2x = $ox + $oh2x;     $h2y = $oy + $oh2y;     $h2z = $oz;
      }     elsif ($b >= 0.6667*$smear) {
         $h1x = $ox;     $h1y = $oy;     $h1z = $oz + $oh1x;
         $h2x = $ox + $oh2y;     $h2y = $oy;     $h2z = $oz + $oh2x;
      }     else {
         $h1x = $ox;     $h1y = $oy + $oh1x;     $h1z = $oz;
         $h2x = $ox;     $h2y = $oy + $oh2x;     $h2z = $oz + $oh2y;
      }
   }     else {
      $h1x = $a;     $h1y = $b;   $h1z = $c;

      if ($b <= 0.3334*$smear) {
         $ox = $h1x - $oh1x;     $oy = $h1y;     $oz = $h1z;
         $h2x = $ox + $oh2x;     $h2y = $oy + $oh2y;     $h2z = $oz;
      }     elsif ($b >= 0.6667*$smear) {
         $ox = $h1x;     $oy = $h1y;     $oz = $h1z - $oh1x;
         $h2x = $ox + $oh2y;     $h2y = $oy;     $h2z = $oz + $oh2x;
      }     else {
         $ox = $h1x;     $oy = $h1y - $oh1x;     $oz = $h1z;
         $h2x = $ox;     $h2y = $oy + $oh2x;     $h2z = $oz + $oh2y;
      }
   }
}

sub PrintHelp {
   print "
This script fills a vacuum space with standard 3-point water molecules.

   usage:   fillwaterbox --format OUTPUT FILE FORMAT --help --model STANDARD WATER MODEL
            --noheader --rho REQUIRED DENSITY --smear SHAKING MULTIPLIER
            --start UNIQUE ATOM ID TO START FROM --xfree NO-WATER REGION ALONG X
            --xsize X DIMENSION OF PERIODIC BOX  --yfree NO-WATER REGION ALONG Y
            --ysize Y DIMENSION OF PERIODIC BOX  --zfree NO-WATER REGION ALONG Z
            --zsize Z DIMENSION OF PERIODIC BOX

      --format     format of the output file. by default, CONFIG.
      --help       print this help
      --model      3-point standard water model. by default, TIP3P.
      --noheader   export coordinates' data only. by default, header information is also exported.
      --rho        required density of water in the box (g/cm^3). the script will generally produce
                   slightly higher density. by default, 1.0 g/cm^3.
      --smear      factor for shaking the coordinates of generated water molecules. a higher value
                   will increase randomization to some extent but chances of a water molecule being
                   rejected will also increase. by default, 0.50.
      --start      when writing coordinates in DLPOLY CONFIG format, start numbering atoms from
                   number. by default, numbering starts from 1.
      --stats      print summary. by default, statistics are turned off.
      --xfree      region along X where no water molecule should be present (Angstroms).
                   by default, 0.0 Angstroms.
      --xsize      X dimension of the periodic box (Angstroms). by default, 10.0 Angstroms.
      --yfree      region along Y where no water molecule should be present (Angstroms).
                   by default, 0.0 Angstroms.
      --ysize      Y dimension of the periodic box (Angstroms). by default, 10.0 Angstroms.
      --zfree      region along Z where no water molecule should be present (Angstroms).
                   by default, 0.0 Angstroms.
      --zsize      Z dimension of the periodic box (Angstroms). by default, 10.0 Angstroms.\n";
}

sub PrintSummary {
   $Mgen = $id / 3;
   $rhogen = $rho * $Mgen / $Mreq;
   $i = ($xsteps*$ysteps*$zsteps) - $Mgen;
   print "$Mgen $model water molecules were successfully generated. $i water molecules were
rejected during the process. The actual density achieved is $rhogen g/cm^3.
$Mreq $model water molecules were required to achieve specified density of $rho g/cm^3.\n";
}

sub SetDefaults {
   $format = "CONFIG";
   $info = 1;
   $model = "TIP3P";
   $N = 1;
   $rho = 1.0000;
   $smear = 0.5000;
   $xfree = 0.0000;
   $xsize = 10.0000;
   $yfree = 0.0000;
   $ysize = 10.0000;
   $zfree = 0.0000;
   $zsize = 10.0000;
}

sub ShiftOneWater {
   $X[$id] = $xfree + ($xx+0.5)*$Lx + $ox;
   $Y[$id] = $yfree + ($yy+0.5)*$Ly + $oy;
   $Z[$id] = $zfree + ($zz+0.5)*$Lz + $oz;
   $X[$id+1] = $xfree + ($xx+0.5)*$Lx + $h1x;
   $Y[$id+1] = $yfree + ($yy+0.5)*$Ly + $h1y;
   $Z[$id+1] = $zfree + ($zz+0.5)*$Lz + $h1z;
   $X[$id+2] = $xfree + ($xx+0.5)*$Lx + $h2x;
   $Y[$id+2] = $yfree + ($yy+0.5)*$Ly + $h2y;
   $Z[$id+2] = $zfree + ($zz+0.5)*$Lz + $h2z;
}

sub WriteCONFIG {
   if ($info!=0) {
      $Mgen = $id / 3;
      printf "H2Ox$Mgen\n     0     2     $id\n";
      printf "%20.10f %20.10f %20.10f\n", $xsize, 0.0, 0.0;
      printf "%20.10f %20.10f %20.10f\n", 0.0, $ysize, 0.0;
      printf "%20.10f %20.10f %20.10f\n", 0.0, 0.0, $zsize;
   }
   for ($i=0; $i<$id; $i++) {
      printf "%-10s %-10s\n %20.10f %20.10f %20.10f\n", $TYPE[$i], $N, $X[$i], $Y[$i], $Z[$i];
      $N += 1;
   }
}

sub WriteXYZ {
   if ($info!=0) {
      $Mgen = $id / 3;
      printf "$id\nH2Ox$Mgen\n";
   }
   for ($i=0; $i<$id; $i++) {
      printf "%-8s %20.10f %20.10f %20.10f\n", $TYPE[$i], $X[$i], $Y[$i], $Z[$i];
   }
}
