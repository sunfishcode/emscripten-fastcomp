##===- utils/Makefile --------------------------------------*- Makefile -*-===##
# 
#                     The LLVM Compiler Infrastructure
#
# This file is distributed under the University of Illinois Open Source
# License. See LICENSE.TXT for details.
# 
##===----------------------------------------------------------------------===##

LEVEL = ..
PARALLEL_DIRS := FileCheck FileUpdate TableGen PerfectShuffle \
	      count fpcmp llvm-lit not unittest

ifeq ($(NACL_SANDBOX),1)
  # In sandboxed mode, just build the bare minimum
  # Note: TableGen is usually built twice: 
  #       * once with host compiler
  #       * also with the "given" compiler
  #  Here we just disable that second build
  PARALLEL_DIRS := 
endif

EXTRA_DIST := check-each-file codegen-diff countloc.sh \
              DSAclean.py DSAextract.py emacs findsym.pl GenLibDeps.pl \
	      getsrcs.sh llvmdo llvmgrep llvm-native-gcc \
	      llvm-native-gxx makellvm profile.pl vim

include $(LEVEL)/Makefile.common
