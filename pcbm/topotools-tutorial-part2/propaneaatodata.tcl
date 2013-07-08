#!/usr/bin/tclsh

# topotools example 2c:
# generate topology data from coordinate data
# build a box of 4x3x3 = 36 propane molecules.
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

set fname propane.pdb
# check for presence of coordinate file
if {! [file exists $fname]} {
   vmdcon -error "Required file '$fname' not available. Exiting..."
   quit
}
  
# load coordinates and use automatically computed bonds
mol new $fname autobonds yes waitfor all

# create separate selections for hydrogens and carbons
# and one for all atoms.
#
# initialize the hydrogens and carbon to the parameters
# for terminal carbons and hydrogens bound to it.
set selh [atomselect top {name H}]
$selh set type HC
$selh set mass 1.00800
$selh set charge 0.060

set selc [atomselect top {name C}]
$selc set type CT
$selc set mass 12.01100
$selc set charge -0.180 ; # = 3x -0.060

# now change the carbon selection to get the
# center carbon, which is the atom with index 4
$selc delete
set selc [atomselect top {index 4}]
# update charge for this atom
$selc set charge -0.120 ; # two hydrogens

set sel [atomselect top all]

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
set minmax [measure minmax $sel -withradii]
set box [vecscale 1.1 [vecsub [lindex $minmax 1] [lindex $minmax 0]]]
pbc set $box
vmdcon -info "box size: $box"
# and recenter the coordinates around the center of mass
set center [measure center $sel weight none]
$sel moveby [vecscale -1.0 $center]
vmdcon -info "moved center from $center to [measure center $sel weight none]"

# we use a high-level tool from to multiply the system.
TopoTools::replicatemol top 4 3 3

# and write out the result as a lammps data file.
topo writelammpsdata data.step2c full

# for easier testing and visualization, we
# also write out copies in .pdb and .psf format.
animate write pdb 36xpropane.pdb
animate write psf 36xpropane.psf

# done. now exit vmd
quit
