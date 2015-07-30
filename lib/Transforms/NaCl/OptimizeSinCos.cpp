//===-- OptimizeSinCos.cpp - sincos optimization --------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===-----------------------------------------------------------------------===//
//
// Optimize sin(x) and cos(x) into sincos(x, &s, &c).
//
//===-----------------------------------------------------------------------===//

#include "llvm/Transforms/NaCl.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/InstIterator.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/raw_ostream.h"
using namespace llvm;

namespace {

struct OptimizeSinCos : public FunctionPass {
  static char ID; // Pass identification, replacement for typeid
  OptimizeSinCos() : FunctionPass(ID) {
    initializeOptimizeSinCosPass(*PassRegistry::getPassRegistry());
  }

  bool runOnFunction(Function &Func) override;

  const char *getPassName() const override { return "OptimizeSinCos"; }
};

}

char OptimizeSinCos::ID = 0;

INITIALIZE_PASS(OptimizeSinCos, "optimize-sincos",
                "Optimize sin and cos into sincos",
                false, false);

// Given a sin or cos call, find a nearby cos or sin call with the same operand
// to be the counterpart.
static CallInst *FindCounterpart(CallInst *Call, const char *Name) {
  BasicBlock *BB = Call->getParent();
  BasicBlock::iterator I = Call;
  ++I;
  int Num = 10;
  for (; I != BB->end() && Num >= 0; ++I, --Num) {
    CallInst *C = dyn_cast<CallInst>(&*I);
    if (!C) continue;
    Function *F = C->getCalledFunction();
    if (!F) continue;
    if (F->getFunctionType() != Call->getFunctionType()) continue;
    if (F->getName() != Name) continue;
    if (C->getArgOperand(0) != Call->getArgOperand(0)) continue;
    return C;
  }
  return nullptr;
}

bool OptimizeSinCos::runOnFunction(Function &Func) {
  bool Changed = false;

  for (inst_iterator I = inst_begin(Func), E = inst_end(Func); I != E; ) {
    Instruction *Inst = &*I++;

    // Look for a call to a floating-point math function.
    CallInst *Call = dyn_cast<CallInst>(Inst);
    if (!Call) continue;
    if (Call->getNumArgOperands() != 1) continue;
    if (Call->getArgOperand(0)->getType() != Call->getType()) continue;
    if (!Call->getType()->isFloatingPointTy()) continue;
    Function *Callee = Call->getCalledFunction();
    if (!Callee) continue;

    // Look for a call that has a counterpart.
    CallInst *Counterpart = nullptr;
    bool StartWithCos = false;
    if (Callee->getName() == "sin") {
      Counterpart = FindCounterpart(Call, "cos");
    } else if (Callee->getName() == "cos") {
      Counterpart = FindCounterpart(Call, "sin");
      StartWithCos = true;
    } else if (Callee->getName() == "sinf") {
      Counterpart = FindCounterpart(Call, "cosf");
    } else if (Callee->getName() == "sinl") {
      Counterpart = FindCounterpart(Call, "cosl");
    } else if (Callee->getName() == "cosf") {
      Counterpart = FindCounterpart(Call, "sinf");
      StartWithCos = true;
    } else if (Callee->getName() == "cosl") {
      Counterpart = FindCounterpart(Call, "sinl");
      StartWithCos = true;
    }
    if (!Counterpart) continue;

    // Create allocas to pass to a sincos call.
    Type *Ty = Call->getType();
    AllocaInst *S = new AllocaInst(Ty, "", Func.getEntryBlock().begin());
    AllocaInst *C = new AllocaInst(Ty, "", Func.getEntryBlock().begin());

    // Get the sincos declaration.
    Module *M = Func.getParent();
    AttributeSet AS[1];
    AS[0] = AttributeSet::get(M->getContext(), AttributeSet::FunctionIndex,
                                               Attribute::NoUnwind);
    const char *SincosName = nullptr;
    switch (Ty->getTypeID()) {
      case Type::FloatTyID: SincosName = "sincosf"; break;
      case Type::DoubleTyID: SincosName = "sincos"; break;
      default: continue;
    }

    // Call the sincos function.
    Value *SinCos = M->getOrInsertFunction(SincosName,
                                           AttributeSet::get(M->getContext(), AS),
                                           Type::getVoidTy(M->getContext()),
                                           Ty,
                                           S->getType(),
                                           C->getType(),
                                           nullptr);
    Value *Args[] = { Call->getArgOperand(0), S, C };
    CallInst::Create(SinCos, Args, "", Call);

    // Load the values stored by the sincos function and swap them in.
    LoadInst *LS = new LoadInst(S, "", Call);
    LoadInst *LC = new LoadInst(C, "", Call);

    Call->replaceAllUsesWith(StartWithCos ? LC : LS);
    Counterpart->replaceAllUsesWith(StartWithCos ? LS : LC);

    // Bump the iterator if needed to avoid deleting out from underneath it.
    if (&*I == Counterpart) ++I;

    Call->eraseFromParent();
    Counterpart->eraseFromParent();

    Changed = true;
  }

  return Changed;
}

FunctionPass *llvm::createOptimizeSinCosPass() {
  return new OptimizeSinCos();
}
