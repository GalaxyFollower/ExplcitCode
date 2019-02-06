#!/bin/bash -x

#########################################################################################################
#								   			
#		  Purpose: To make a cluster out of a gas phase species optimized on a surface.		#															
#	           Author: Mohammad Shamsus Saleheen, Department of Chemical Engineering,USC            #
#	       	     Date: 11.03.2014								        #
#   	     Modification: 10.11.16-This is probably the 'crudest' sed script you'll ever see. I used   #
#			   regular expressions and specific co-ordinate ranges cause I knew		#
#			   my cluster atom co-ordinates lie in this range. Obviously I'd try to		#
#			   make it better, when I've time. :)						#
#  Reasons of Modification:										# 
#                                                               					#		
#							                                                #
#########################################################################################################

# There are three parts in this script. The first part deals with creating an expanded surface from the EG optimized on Pt(111) Surface,
# the second part deals with making the cluster from that expanded surface. As I have said previously, it's just a makeshift script to 
# make the cluster depending on the experience of the cluster co-ordinates. The last part deals with making a waterbox, immersing the
# clean Pt surface in the water and replacing the atoms (to be treated quantum mechanically) of the immersed surface with that of cluster.

# This part is just for cleaning up the two CONTCAR files

sed  -i '/^[[:space:]]*$/,$d' CONTCAR



###First Part-Creating an expanded surface###

vasp-build-supercell-from-poscar -f CONTCAR -o XYZ -X 4 -Y 5 > expanded.xyz


###Second Part-Making Cluster###


# Since we don't need the bottom two layers for the cluster, the following lines mark the bottom two layer atoms with 'bottom!' word and then we delete them.
# we could directly go to making the cluster but since this script totally depends on coordinates, it's best to delete bottom two layers.


# This line marks those bottom layer atoms whose x and y coordinates starts with a single digit.
sed '/Pt[ <t>]*\([0-9]\.[0-9]\{14\}[ <t>]*\)\{2\}[0-2]/ s/$/  bottom!/' expanded.xyz > cluster.xyz

# This line marks those bottom layer atoms whose x and y coordinates both starts with double digits.
sed -i '/Pt[ <t>]*\([0-9][0-9]\.[0-9]\{14\}[ <t>]*\)\{2\}[0-2]/ s/$/  bottom!/' cluster.xyz

# This line marks those with double digits of x but single digit of y coordinates.
sed -i '/Pt[ <t>]*\([0-9][0-9]\.[0-9]\{14\}[ <t>]*\)\([0-9]\.[0-9]\{14\}[ <t>]*\)\([0-2]\.[0-9]\{14\}[ <t>]*\)/ s/$/  bottom!/' cluster.xyz

# This line marks those with double digits y but single digit x coordinates.
sed -i '/Pt[ <t>]*\([0-9]\.[0-9]\{14\}[ <t>]*\)\([0-9][0-9]\.[0-9]\{14\}[ <t>]*\)\([0-2]\.[0-9]\{14\}[ <t>]*\)/ s/$/  bottom!/' cluster.xyz 

# Deleting bottom layer atoms
sed -i '/bottom!/d' cluster.xyz

# This part is marking the cluster atoms with 'cluster!'. This is where the code starts getting messy cause it's based on my project and experience of qm atom 
# coordinates. At first when I marked the cluster atoms, I saw my range of coordinates some times include some more atoms and that's why I had to delete those
# atoms later by marking them with 'delete!' keyword.


# Marking Pt cluster atoms.
## Z coordinate 4
# Bottom layer cluster atoms 1
sed -i '/Pt[ <t>]*\([1][7-8]\.[0-9]\{14\}[ <t>]*\)\([1][0]\.[0-9]\{14\}[ <t>]*\)\([4]\.[0-9]\{14\}[ <t>]*\)/ s/$/  cluster!/' cluster.xyz
sed -ne '/cluster!/ p' cluster.xyz >> Letsbegin.xyz
sed -i '/cluster!/d' cluster.xyz
# Bottom layer cluster atoms 2
sed -i '/Pt[ <t>]*\([2][0-1]\.[0-9]\{14\}[ <t>]*\)\([1][0]\.[0-9]\{14\}[ <t>]*\)\([4]\.[0-9]\{14\}[ <t>]*\)/ s/$/  cluster!/' cluster.xyz
sed -ne '/cluster!/ p' cluster.xyz >> Letsbegin.xyz
sed -i '/cluster!/d' cluster.xyz

# Bottom layer cluster atoms 3,4
sed -i '/Pt[ <t>]*\([1][6-9]\.[0-9]\{14\}[ <t>]*\)\([1][2]\.[0-9]\{14\}[ <t>]*\)\([4]\.[0-9]\{14\}[ <t>]*\)/ s/$/  cluster!/' cluster.xyz
sed -ne '/cluster!/ p' cluster.xyz >> Letsbegin.xyz
sed -i '/cluster!/d' cluster.xyz

# Bottom layer cluster atoms 5
sed -i '/Pt[ <t>]*\([2][2]\.[0-9]\{14\}[ <t>]*\)\([1][2]\.[0-9]\{14\}[ <t>]*\)\([4]\.[0-9]\{14\}[ <t>]*\)/ s/$/  cluster!/' cluster.xyz
sed -ne '/cluster!/ p' cluster.xyz >> Letsbegin.xyz
sed -i '/cluster!/d' cluster.xyz


# Bottom layer cluster atoms 6,7

sed -i '/Pt[ <t>]*\([1][5-8]\.[0-9]\{14\}[ <t>]*\)\([1][5]\.[0-9]\{14\}[ <t>]*\)\([4]\.[0-9]\{14\}[ <t>]*\)/ s/$/  cluster!/' cluster.xyz
sed -ne '/cluster!/ p' cluster.xyz >> Letsbegin.xyz
sed -i '/cluster!/d' cluster.xyz

# Bottom layer cluster atoms 8,9

sed -i '/Pt[ <t>]*\([2][1-3]\.[0-9]\{14\}[ <t>]*\)\([1][5]\.[0-9]\{14\}[ <t>]*\)\([4]\.[0-9]\{14\}[ <t>]*\)/ s/$/  cluster!/' cluster.xyz
sed -ne '/cluster!/ p' cluster.xyz >> Letsbegin.xyz
sed -i '/cluster!/d' cluster.xyz


# Bottom layer cluster atoms 10,11,12

sed -i '/Pt[ <t>]*\([1][4-9]\.[0-9]\{14\}[ <t>]*\)\([1][7]\.[0-9]\{14\}[ <t>]*\)\([4]\.[0-9]\{14\}[ <t>]*\)/ s/$/  cluster!/' cluster.xyz
sed -ne '/cluster!/ p' cluster.xyz >> Letsbegin.xyz
sed -i '/cluster!/d' cluster.xyz

# Bottom layer cluster atoms 13,14

sed -i '/Pt[ <t>]*\([2][2-5]\.[0-9]\{14\}[ <t>]*\)\([1][7]\.[0-9]\{14\}[ <t>]*\)\([4]\.[0-9]\{14\}[ <t>]*\)/ s/$/  cluster!/' cluster.xyz
sed -ne '/cluster!/ p' cluster.xyz >> Letsbegin.xyz
sed -i '/cluster!/d' cluster.xyz



# Bottom layer cluster atoms 15,16

sed -i '/Pt[ <t>]*\([1][5-8]\.[0-9]\{14\}[ <t>]*\)\([2][0]\.[0-9]\{14\}[ <t>]*\)\([4]\.[0-9]\{14\}[ <t>]*\)/ s/$/  cluster!/' cluster.xyz
sed -ne '/cluster!/ p' cluster.xyz >> Letsbegin.xyz
sed -i '/cluster!/d' cluster.xyz

# Bottom layer cluster atoms 17,18

sed -i '/Pt[ <t>]*\([2][1-3]\.[0-9]\{14\}[ <t>]*\)\([2][0]\.[0-9]\{14\}[ <t>]*\)\([4]\.[0-9]\{14\}[ <t>]*\)/ s/$/  cluster!/' cluster.xyz
sed -ne '/cluster!/ p' cluster.xyz >> Letsbegin.xyz
sed -i '/cluster!/d' cluster.xyz

# Bottom layer cluster atoms 19,20

sed -i '/Pt[ <t>]*\([1][6-9]\.[0-9]\{14\}[ <t>]*\)\([2][2]\.[0-9]\{14\}[ <t>]*\)\([4]\.[0-9]\{14\}[ <t>]*\)/ s/$/  cluster!/' cluster.xyz
sed -ne '/cluster!/ p' cluster.xyz >> Letsbegin.xyz
sed -i '/cluster!/d' cluster.xyz

# Bottom layer cluster atoms 21

sed -i '/Pt[ <t>]*\([2][2]\.[0-9]\{14\}[ <t>]*\)\([2][2]\.[0-9]\{14\}[ <t>]*\)\([4]\.[0-9]\{14\}[ <t>]*\)/ s/$/  cluster!/' cluster.xyz
sed -ne '/cluster!/ p' cluster.xyz >> Letsbegin.xyz
sed -i '/cluster!/d' cluster.xyz

## Z coordinate 6

# Top layer cluster atom 22,23

sed -i '/Pt[ <t>]*\([1][6-9]\.[0-9]\{14\}[ <t>]*\)\([9]\.[0-9]\{14\}[ <t>]*\)\([6]\.[0-9]\{14\}[ <t>]*\)/ s/$/  cluster!/' cluster.xyz
sed -ne '/cluster!/ p' cluster.xyz >> Letsbegin.xyz
sed -i '/cluster!/d' cluster.xyz


# Top layer cluster atom 24

sed -i '/Pt[ <t>]*\([2][2]\.[0-9]\{14\}[ <t>]*\)\([9]\.[0-9]\{14\}[ <t>]*\)\([6]\.[0-9]\{14\}[ <t>]*\)/ s/$/  cluster!/' cluster.xyz
sed -ne '/cluster!/ p' cluster.xyz >> Letsbegin.xyz
sed -i '/cluster!/d' cluster.xyz

# Top layer cluster atom 25,26

sed -i '/Pt[ <t>]*\([1][5-8]\.[0-9]\{14\}[ <t>]*\)\([1][2]\.[0-9]\{14\}[ <t>]*\)\([6]\.[0-9]\{14\}[ <t>]*\)/ s/$/  cluster!/' cluster.xyz
sed -ne '/cluster!/ p' cluster.xyz >> Letsbegin.xyz
sed -i '/cluster!/d' cluster.xyz

# Top layer cluster atom 27,28

sed -i '/Pt[ <t>]*\([2][1-3]\.[0-9]\{14\}[ <t>]*\)\([1][2]\.[0-9]\{14\}[ <t>]*\)\([6]\.[0-9]\{14\}[ <t>]*\)/ s/$/  cluster!/' cluster.xyz
sed -ne '/cluster!/ p' cluster.xyz >> Letsbegin.xyz
sed -i '/cluster!/d' cluster.xyz


# Top layer cluster atom 29,30,31

sed -i '/Pt[ <t>]*\([1][3-9]\.[0-9]\{14\}[ <t>]*\)\([1][4]\.[0-9]\{14\}[ <t>]*\)\([6]\.[0-9]\{14\}[ <t>]*\)/ s/$/  cluster!/' cluster.xyz
sed -ne '/cluster!/ p' cluster.xyz >> Letsbegin.xyz
sed -i '/cluster!/d' cluster.xyz

# Top layer cluster atom 32,33

sed -i '/Pt[ <t>]*\([2][2-5]\.[0-9]\{14\}[ <t>]*\)\([1][4]\.[0-9]\{14\}[ <t>]*\)\([6]\.[0-9]\{14\}[ <t>]*\)/ s/$/  cluster!/' cluster.xyz
sed -ne '/cluster!/ p' cluster.xyz >> Letsbegin.xyz
sed -i '/cluster!/d' cluster.xyz


# Top layer cluster atom 34,35 

sed -i '/Pt[ <t>]*\([1][2-5]\.[0-9]\{14\}[ <t>]*\)\([1][7]\.[0-9]\{14\}[ <t>]*\)\([6]\.[0-9]\{14\}[ <t>]*\)/ s/$/  cluster!/' cluster.xyz
sed -ne '/cluster!/ p' cluster.xyz >> Letsbegin.xyz
sed -i '/cluster!/d' cluster.xyz

# Top layer cluster atom 36

sed -i '/Pt[ <t>]*\([1][8]\.[0-9]\{14\}[ <t>]*\)\([1][7]\.[0-9]\{14\}[ <t>]*\)\([6-7]\.[0-9]\{14\}[ <t>]*\)/ s/$/  cluster!/' cluster.xyz
sed -ne '/cluster!/ p' cluster.xyz >> Letsbegin.xyz
sed -i '/cluster!/d' cluster.xyz



# Top layer cluster atom 37,38,39

sed -i '/Pt[ <t>]*\([2][1-6]\.[0-9]\{14\}[ <t>]*\)\([1][7]\.[0-9]\{14\}[ <t>]*\)\([6]\.[0-9]\{14\}[ <t>]*\)/ s/$/  cluster!/' cluster.xyz
sed -ne '/cluster!/ p' cluster.xyz >> Letsbegin.xyz
sed -i '/cluster!/d' cluster.xyz


# Top layer cluster atom 40,41,42

sed -i '/Pt[ <t>]*\([1][4-9]\.[0-9]\{14\}[ <t>]*\)\([1][9]\.[0-9]\{14\}[ <t>]*\)\([6]\.[0-9]\{14\}[ <t>]*\)/ s/$/  cluster!/' cluster.xyz
sed -ne '/cluster!/ p' cluster.xyz >> Letsbegin.xyz
sed -i '/cluster!/d' cluster.xyz

# Top layer cluster atom 43,44

sed -i '/Pt[ <t>]*\([2][2-5]\.[0-9]\{14\}[ <t>]*\)\([1][9]\.[0-9]\{14\}[ <t>]*\)\([6]\.[0-9]\{14\}[ <t>]*\)/ s/$/  cluster!/' cluster.xyz
sed -ne '/cluster!/ p' cluster.xyz >> Letsbegin.xyz
sed -i '/cluster!/d' cluster.xyz


# Top layer cluster atom 45,46 

sed -i '/Pt[ <t>]*\([1][5-8]\.[0-9]\{14\}[ <t>]*\)\([2][1]\.[0-9]\{14\}[ <t>]*\)\([6]\.[0-9]\{14\}[ <t>]*\)/ s/$/  cluster!/' cluster.xyz
sed -ne '/cluster!/ p' cluster.xyz >> Letsbegin.xyz
sed -i '/cluster!/d' cluster.xyz

#Top layer cluster atom 47,48 

sed -i '/Pt[ <t>]*\([2][1-3]\.[0-9]\{14\}[ <t>]*\)\([2][1]\.[0-9]\{14\}[ <t>]*\)\([6]\.[0-9]\{14\}[ <t>]*\)/ s/$/  cluster!/' cluster.xyz
sed -ne '/cluster!/ p' cluster.xyz >> Letsbegin.xyz
sed -i '/cluster!/d' cluster.xyz

# Top layer cluster atom 49,50

sed -i '/Pt[ <t>]*\([1][6-9]\.[0-9]\{14\}[ <t>]*\)\([2][4]\.[0-9]\{14\}[ <t>]*\)\([6]\.[0-9]\{14\}[ <t>]*\)/ s/$/  cluster!/' cluster.xyz
sed -ne '/cluster!/ p' cluster.xyz >> Letsbegin.xyz
sed -i '/cluster!/d' cluster.xyz

# Top layer cluster atom 51

sed -i '/Pt[ <t>]*\([2][2]\.[0-9]\{14\}[ <t>]*\)\([2][4]\.[0-9]\{14\}[ <t>]*\)\([6]\.[0-9]\{14\}[ <t>]*\)/ s/$/  cluster!/' cluster.xyz
sed -ne '/cluster!/ p' cluster.xyz >> Letsbegin.xyz
sed -i '/cluster!/d' cluster.xyz



# Marking Adsorbed Species (Ethelyne Glycol) atoms 

# All C atoms
sed -i '/C[ <t>]*\([1][6-9]\.[0-9]\{14\}[ <t>]*\)\([1][4-7]\.[0-9]\{14\}[ <t>]*\)/ s/$/  cluster!/' cluster.xyz
sed -ne '/cluster!/ p' cluster.xyz >> Letsbegin.xyz
sed -i '/cluster!/d' cluster.xyz
   # C atoms, remaining
     sed -i '/C[ <t>]*\([2][0-1]\.[0-9]\{14\}[ <t>]*\)\([1][4-7]\.[0-9]\{14\}[ <t>]*\)/ s/$/  cluster!/' cluster.xyz
     sed -ne '/cluster!/ p' cluster.xyz >> Letsbegin.xyz
     sed -i '/cluster!/d' cluster.xyz

# All O atoms
sed -i '/O[ <t>]*\([1][4-9]\.[0-9]\{14\}[ <t>]*\)\([1][2-7]\.[0-9]\{14\}[ <t>]*\)/ s/$/  cluster!/' cluster.xyz
sed -ne '/cluster!/ p' cluster.xyz >> Letsbegin.xyz
sed -i '/cluster!/d' cluster.xyz
    # O atoms, remaining
    sed -i '/O[ <t>]*\([2][0-1]\.[0-9]\{14\}[ <t>]*\)\([1][2-7]\.[0-9]\{14\}[ <t>]*\)/ s/$/  cluster!/' cluster.xyz
    sed -ne '/cluster!/ p' cluster.xyz >> Letsbegin.xyz
    sed -i '/cluster!/d' cluster.xyz

# All H atoms
sed -i '/H[ <t>]*\([1][5-9]\.[0-9]\{14\}[ <t>]*\)\([1][3-9]\.[0-9]\{14\}[ <t>]*\)/ s/$/  cluster!/' cluster.xyz
sed -ne '/cluster!/ p' cluster.xyz >> Letsbegin.xyz
sed -i '/cluster!/d' cluster.xyz
    #H atoms , remaining
	 sed -i '/H[ <t>]*\([2][0-1]\.[0-9]\{14\}[ <t>]*\)\([1][3-9]\.[0-9]\{14\}[ <t>]*\)/ s/$/  cluster!/' cluster.xyz
	sed -ne '/cluster!/ p' cluster.xyz >> Letsbegin.xyz
	sed -i '/cluster!/d' cluster.xyz


# Making a new file for the cluster atoms 
mv Letsbegin.xyz cluster.xyz


# Removing the 'cluster!' word from cluster atoms.
sed -i 's/cluster!//g' cluster.xyz

# Since xyz file requires number of atoms in the first line and a title in the 2nd line, adding those lines.
sed -i '1 i\61\nFree Energy Perturbation' cluster.xyz

###Third Part-Immersing the clean surface in water and placing the QM atoms###

# Expanding the clean surface and making random water image of specific box size at 500K temperature.(Hence rho 0.8313)
#vasp-build-supercell-from-poscar -f CONTCAR-Pt -o XYZ -X 4 -Y 5 > surface.xyz
#fillwaterbox.perl --format xyz --rho 0.8313 --stats --xsize 43 --ysize 46 --zfree 9 --zsize 49.01 > waterbox.xyz
#fillwaterbox.perl --format xyz --rho 0.8313 --stats --xsize 43 --ysize 46 --zfree 9 --zsize 49.01 > statswaterbox.xyz   #Just for looking at the stats, how many water molecules are needed, how many are present etc.

# Removing the stats data printed in waterbox.xyz and concating it with clean surface.
#sed -i '1,5d' waterbox.xyz
#cat  surface.xyz waterbox.xyz > surface-water.xyz

# Changing the number of atoms line in the immersed surface file.
#sed -i '1 s/1280/8336/g' surface-water.xyz

# Inplacing the cluster atoms into the immersed surface. So at last we have a water configuration to start with.

geometry-merge-xyz-coordinates-last.ext -b SurfaceWater.xyz -m cluster.xyz  > built.xyz


###BYE!###
