#!/usr/bin/tclsh

# topotools example 2e:
# generate topology data from coordinate data
# build a box of 3x3x2 = 18 cyclohexane molecules.
#                      + 18 hexane molecules
##############################################

# explicitly load topotools and pbctools packages since
# they are not automatically requred in text mode and
# abort if their version numbers are insufficient.
if {[catch {package require topotools 1.1} ver]} {
   vmdcon -error "$ver. This script requires at least TopoTools v1.1. Exiting..."
   quit
}

if {[catch {package require pbctools 2.3} ver]} {
   vmdcon -error "$ver. This script requires at least pbctools v2.3. Exiting..."
   quit
}

set fname cyclohexane.pdb
# check for presence of coordinate file
if {! [file exists $fname]} {
   vmdcon -error "Required file '$fname' not available. Exiting..."
   quit
}
  
# load coordinates and use automatically computed bonds
mol new $fname autobonds yes waitfor all

# create separate selections for hydrogens and carbons
#
# initialize the hydrogens and carbon to the parameters
# for tetrahedral carbons and hydrogens bound to it.
set selh [atomselect top {name H}]
$selh set type HC
$selh set resname CYC
$selh set mass 1.00800
$selh set charge 0.060

set selc [atomselect top {name C}]
$selc set type CT
$selc set resname CYC
$selc set mass 12.01100
$selc set charge -0.120 ; # = 2x -0.060

# remember this molecule
set mol1 [molinfo top]

set fname hexane.pdb
# check for presence of coordinate file
if {! [file exists $fname]} {
   vmdcon -error "Required file '$fname' not available. Exiting..."
   quit
}
  
# load coordinates and use automatically computed bonds
mol new $fname autobonds yes waitfor all

# create new selections for hydrogens and carbons
$selh delete
$selc delete
# initialize the hydrogens and carbon to the parameters
# for tetrahedral carbons and hydrogens bound to it.
set selh [atomselect top {name H}]
$selh set type HC
$selh set resname HEX
$selh set mass 1.00800
$selh set charge 0.060

set selc [atomselect top {name C}]
$selc set type CT
$selc set resname HEX
$selc set mass 12.01100
$selc set charge -0.120 ; # = 2x -0.060

# get a list of all carbon atom indices 
# the terminal carbon atoms are the first and last in the list.
set cidx [$selc get index]
$selc delete
set selc [atomselect top "name C and index [lindex $cidx 0] [lindex $cidx end]"]
$selc set charge -0.180 ; # = 3x -0.060
set mol2 [molinfo top]

# now combine the two molecules into one
::TopoTools::mergemols [list $mol1 $mol2]

# with a proper .pdb file, VMD will have already
# determined the proper element definitions, so
# recomputing the bonds will be hardly necessary.
# we still need to assign bond types, though.
topo retypebonds 
vmdcon -info "assigned [topo numbondtypes] bond types to [topo numbonds] bonds:"
vmdcon -info "bondtypes: [topo bondtypenames]"

# now derive angle and dihedral definitions from bond topology.
# every two bonds that share an atom yield an angle.
# every two bonds that share a bond yield a dihedral.
topo guessangles
vmdcon -info "assigned [topo numangletypes] angle types to [topo numangles] angles:"
vmdcon -info "angletypes: [topo angletypenames]"
topo guessdihedrals
vmdcon -info "assigned [topo numdihedraltypes] dihedral types to [topo numdihedrals] dihedrals:"
vmdcon -info "dihedraltypes: [topo dihedraltypenames]"

# now let VMD reanalyze the molecular structure
# this is needed to detect fragments/molecules
# after we have recomputed the bonds
mol reanalyze top

# now set box dimensions from the min/max corners in order 
# to fit the molecule  considering its vdw atom radii.
set sel [atomselect top all]
set minmax [measure minmax $sel -withradii]
set box [vecscale 1.1 [vecsub [lindex $minmax 1] [lindex $minmax 0]]]
pbc set $box
vmdcon -info "box size: $box"
# and recenter the coordinates around the center of mass
set center [measure center $sel weight none]
$sel moveby [vecscale -1.0 $center]
vmdcon -info "moved center from $center to [measure center $sel weight none]"

# we use a high-level tool from to multiply the system.
TopoTools::replicatemol top 3 3 2

# and write out the result as a lammps data file.
topo writelammpsdata data.step2e full

# for easier testing and visualization, we
# also write out copies in .pdb and .psf format.
animate write pdb 18xmixedhexane.pdb
animate write psf 18xmixedhexane.psf

# final sanity check the whole system has to be neutral.
$sel delete
set sel [atomselect top all]
set totq [measure sumweights $sel weight charge]
if {[expr {abs($totq)}] > 0.0005} {
    vmdcon -warning "Total system not neutral: $totq"
}

# done. now exit vmd
quit
