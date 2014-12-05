###############################################
# Makefile for compiling haskell source code. #
###############################################
OBJS = moses.hs
DOCOBJS = moses ../Common/Data ../Common/Inet/Parse ../Common/Inet/Data ../Common/Db/FileAccess
CC = ghc
CFLAGS = --make
IFLAGS = -i../

moses: $(OBJS)
	$(CC) $(IFLAGS) $(CFLAGS) $(OBJS)

# Call doc to generate source documentation for moses.
# Documentation is generated as html pages.
doc: 
	haddock $(DOCOBJS) -h -o ../../doc/moses

# Cleans up moses from everything but source files.
clean:
	\rm moses *.o *~ *.hi 

# Cleans up common files from everything but source files.
cleancommon:
	\rm ../Common/Inet/*.o ../Common/Inet/*~ ../Common/Inet/*.hi \
../Common/*.o ../Common/*~ ../Common/*.hi \
../Common/Db/*.o ../Common/Db/*~ ../Common/Db/*.hi

# Cleans up all of existing documentation for moses.
cleandoc:
	\rm ../../doc/moses/*























































































