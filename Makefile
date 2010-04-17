 #
 #
 #  This file is part of Wikicreole-ocaml
 #
 #  Wikicreole-ocaml is free software: you can redistribute it and/or modify
 #  it under the terms of the GNU General Public License as published by
 #  the Free Software Foundation, either version 3 of the License, or
 #  (at your option) any later 
 #  
 #  Wikicreole-ocaml is distributed in the hope that it will be useful,
 #  but WITHOUT ANY WARRANTY; without even the implied warranty of
 #  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 #  GNU General Public License for more details.
 #  
 #  You should have received a copy of the GNU General Public License
 #  along with Wikicreole-ocaml. If not, see <http://www.gnu.org/licenses/>.
 #
 #  Makefile william le ferrand (william@themipsfactory.com) 17/04/2010 22:50
 #
 #



all:
	ocamlbuild wikicreole.cma

install:
	ocamlfind install aws META _build/shared/aws_shared.cma _build/shared/aws_shared.cmi _build/simpledb/aws_simpledb.cma _build/simpledb/aws_simpledb.cmi _build/ec2/aws_ec2.cma _build/ec2/aws_ec2.cmi

remove: 
	ocamlfind remove wikicreole

clean:
	ocamlbuild -clean

