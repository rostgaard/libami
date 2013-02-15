###############################################################################
#                                                                             #
#  This is free software;  you can redistribute it  and/or modify it          #
#  under terms of the  GNU General Public License as published  by the        #
#  Free Software  Foundation;  either version 3,  or (at your option) any     #
#  later version.  This software is distributed in the hope  that it will     #
#  be useful, but WITHOUT ANY WARRANTY;  without even the implied warranty    #
#  of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU        #
#  General Public License for  more details.                                  #
#  You should have  received  a copy of the GNU General  Public  License      #
#  distributed  with  this  software;   see  file COPYING3.  If not, go       #
#  to http://www.gnu.org/licenses for a complete copy of the license.         #
#                                                                             #
###############################################################################

GPR_TARGET=lib/gnat/ 

include makefile.setup

all: ami

ami:
	gnatmake -P ami_build && touch ami

debug:
	BUILDTYPE=Debug gnatmake -P ami_build

clean:
	gnatclean -P ami_build
	BUILDTYPE=Debug gnatclean -P ami_build

uninstall:
	rm -rf $(PREFIX)/ami
	rm -rf $(PREFIX)/include/ami
	rm -f $(PREFIX)/$(GPR_TARGET)/ami.gpr

install: all
	mkdir -p $(PREFIX)/lib/gnat
	mkdir -p $(PREFIX)/ami
	mkdir -p $(PREFIX)/include/ami
	cp -pr lib/* $(PREFIX)/ami
	cp -pr src/*.ad[sb] $(PREFIX)/include/ami
	cp -pr ami.gpr.dist $(PREFIX)/lib/gnat/ami.gpr
