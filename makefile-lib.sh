#!/bin/bash -x

export COMPILER="ifort -free -O3"

# start afresh
rm -f smss-lib.a
rm -f dlpoly-convert-config-to-geometry-xyz dlpoly-extract-config-coordinates dlpoly-relocate-config-coordinates
rm -f geometry-convert-xyz-to-dlpoly-config
rm -f vasp-convert-poscar-coordinates vasp-extract-outcar-coordinates vasp-move-cluster-to-center
rm -f smss-match-indices
rm -f dlpoly-replace-field-charges
rm -f dlpoly-replay-history
rm -f smss-evaluate-energy smss-evaluate-gradients
rm -f dlpoly-merge-config-coordinates
rm -f geometry-merge-xyz-coordinates
rm -f turbomole-extract-maximum-absolute-force
rm -f vasp-build-supercell-from-poscar
rm -f vasp-extract-maximum-absolute-force
rm -f dlpoly-replay-history-4Cores

# compile modules
$COMPILER -c Parameters.f90 Debugger.f90 Timers.f90
$COMPILER -c MemoryManagement.f90 UtilityProcedures.f90
$COMPILER -c CommandExecutors.f90
$COMPILER -c GenericInputOutput.f90 CoordinateFileFormats.f90
$COMPILER -c SolvationScheme.f90 FileParsers.f90

# create library
ar vr smss-lib.a *.o

# file format conversion
$COMPILER -o dlpoly-convert-config-to-geometry-xyz DlpolyConvertConfigToGeometryXyz.f90 smss-lib.a
$COMPILER -o dlpoly-extract-config-coordinates DlpolyExtractConfigCoordinates.f90 smss-lib.a
$COMPILER -o dlpoly-relocate-config-coordinates DlpolyRelocateConfigCoordinates.f90 smss-lib.a
$COMPILER -o geometry-convert-xyz-to-dlpoly-config GeometryConvertXyzToDlpolyConfig.f90 smss-lib.a
$COMPILER -o vasp-convert-poscar-coordinates VaspConvertPoscarCoordinates.f90 smss-lib.a
$COMPILER -o vasp-extract-outcar-coordinates VaspExtractOutcarCoordinates.f90 smss-lib.a
$COMPILER -o vasp-move-cluster-to-center VaspMoveClusterToCenter.f90 smss-lib.a

# smss input generation
$COMPILER -o smss-match-indices SmssMatchIndices.f90 smss-lib.a
$COMPILER -o dlpoly-replace-field-charges DlpolyReplaceFieldCharges.f90 smss-lib.a

# smss execution
$COMPILER -o dlpoly-replay-history DlpolyReplayHistory.f90 smss-lib.a
$COMPILER -o dlpoly-replay-history-4Cores DlpolyReplayHistory4Cores.f90 smss-lib.a
$COMPILER -o smss-evaluate-energy SmssEvaluateEnergy.f90 smss-lib.a
$COMPILER -o smss-evaluate-gradients SmssEvaluateGradients.f90 smss-lib.a

# miscellaneous
$COMPILER -o dlpoly-merge-config-coordinates DlpolyMergeConfigCoordinates.f90 smss-lib.a
$COMPILER -o geometry-merge-xyz-coordinates GeometryMergeXyzCoordinates.f90 smss-lib.a
$COMPILER -o turbomole-extract-maximum-absolute-force TurbomoleExtractMaximumAbsoluteForce.f90 smss-lib.a
$COMPILER -o vasp-build-supercell-from-poscar VaspBuildSupercellFromPoscar.f90 smss-lib.a
$COMPILER -o vasp-extract-maximum-absolute-force VaspExtractMaximumAbsoluteForce.f90 smss-lib.a
$COMPILER -o moment-of-inertia MomentOfInertia.f90 smss-lib.a

# clean up
rm -f *.mod *.o *.out
