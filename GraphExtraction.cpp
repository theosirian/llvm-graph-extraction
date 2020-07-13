#include "llvm/ADT/GraphTraits.h"
#include "llvm/ADT/SCCIterator.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Analysis/CFG.h"
#include "llvm/Analysis/CFGPrinter.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/CFG.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Operator.h"
#include "llvm/IR/PassManager.h"
#include "llvm/IR/Value.h"
#include "llvm/Pass.h"
#include "llvm/PassAnalysisSupport.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/GraphWriter.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"
#include <math.h>
#include <ostream>
#include <set>

using namespace llvm;

namespace {
struct Features {
	// Count of terminator class instructions, as defined by LLVM code
	unsigned numTerminatorInst;

	// Count of unary class instructions, as defined by LLVM code
	unsigned numUnaryInst;

	// Count of binary class instructions, as defined by LLVM code
	unsigned numBinaryInst;

	// Count of logical class instructions, as defined by LLVM code
	unsigned numLogicalInst;
	// Count of memory class instructions, as defined by LLVM code
	unsigned numMemoryInst;

	// Count of convert class instructions, as defined by LLVM code
	unsigned numConvertInst;

	// Count of other class instructions, as defined by LLVM code
	unsigned numOtherInst;

	//
	// Count of assignments instructions
	unsigned numAssignmentInst;

	// Count of integer operation instructions
	unsigned numIntOpInst;

	// Count of float operation instructions
	unsigned numFloatOpInst;

	// Count of unary operation instructions
	unsigned numUnaryOpInst;

	// Count of switch instructions
	unsigned numSwitchInst;

	// Count of instructions
	unsigned numInst;

	// Count of pointer arithmetics
	unsigned numPtrArithmetic;

	// Count of instructions with one operand constant
	unsigned numOneOperandConst;

	// Count of instructions with left operand constant
	unsigned numLeftOpIntConst;

	//
	// Count of phi-nodes
	unsigned numPhiNodes;

	// Count of phi-node args
	unsigned numPhiArgs;

	// Count of phi-node args
	float avgPhiArgs;

	//
	// Count of pointer return call type
	unsigned numPtrRetCallType;

	// Count of integer return call type
	unsigned numIntRetCallType;

	// Count of argument with pointer types
	unsigned numCallArgPtrType;

	// Count of calls with more than four args
	unsigned numCall4Args;

	// Count of direct function calls
	unsigned numDirectCalls;

	// Count of indirect function calls
	unsigned numIndirectCalls;

	//
	// Count of unconditional branch instructions
	unsigned numUncondBranch;

	// Count of conditional branch instructions
	unsigned numCondBranch;

	//
	// Count of constant integer variables
	unsigned numConstInt0Var;

	// Count of constant 1-bit integer variables
	unsigned numConstInt1Var;

	// Count of constant 32-bit integer variables
	unsigned numConstInt32Var;

	// Count of constant 64-bit integer variables
	unsigned numConstInt64Var;

	// Count of reference to external variables
	unsigned numRefExtVar;

	// Count of reference to local variables
	unsigned numRefLocalVar;

	// Count of local pointer variables
	unsigned numLocalPtrVar;

	// Count of static pointer variables
	unsigned numStaticPtrVar;

	// Count of address variables
	unsigned numAddressVar;

	//
	// Count of indirect references
	unsigned numIndirectRef;

	// Count of address functions
	unsigned numAddressFunc;

	Features()
		: numTerminatorInst(0),
			numUnaryInst(0),
			numBinaryInst(0),
			numLogicalInst(0),
			numMemoryInst(0),
			numConvertInst(0),
			numOtherInst(0),

			numAssignmentInst(0),
			numIntOpInst(0),
			numFloatOpInst(0),
			numUnaryOpInst(0),
			numSwitchInst(0),
			numInst(0),
			numPtrArithmetic(0),
			numOneOperandConst(0),
			numLeftOpIntConst(0),

			numPhiNodes(0),
			numPhiArgs(0),
			avgPhiArgs(0),

			numPtrRetCallType(0),
			numIntRetCallType(0),
			numCallArgPtrType(0),
			numCall4Args(0),
			numDirectCalls(0),
			numIndirectCalls(0),

			numUncondBranch(0),
			numCondBranch(0),

			numConstInt0Var(0),
			numConstInt1Var(0),
			numConstInt32Var(0),
			numConstInt64Var(0),
			numRefExtVar(0),
			numRefLocalVar(0),
			numLocalPtrVar(0),
			numStaticPtrVar(0),
			numAddressVar(0),

			numIndirectRef(0),
			numAddressFunc(0) {}

	const std::vector<int> as_vec() {
		std::vector<int> vec;

		vec.push_back(numTerminatorInst);
		vec.push_back(numUnaryInst);
		vec.push_back(numBinaryInst);
		vec.push_back(numLogicalInst);
		vec.push_back(numMemoryInst);
		vec.push_back(numConvertInst);
		vec.push_back(numOtherInst);
		vec.push_back(numAssignmentInst);
		vec.push_back(numIntOpInst);
		vec.push_back(numFloatOpInst);
		vec.push_back(numUnaryOpInst);
		vec.push_back(numSwitchInst);
		vec.push_back(numInst);
		vec.push_back(numPtrArithmetic);
		vec.push_back(numOneOperandConst);
		vec.push_back(numLeftOpIntConst);
		vec.push_back(numPhiNodes);
		vec.push_back(numPhiArgs);
		vec.push_back(avgPhiArgs);
		vec.push_back(numPtrRetCallType);
		vec.push_back(numIntRetCallType);
		vec.push_back(numCallArgPtrType);
		vec.push_back(numCall4Args);
		vec.push_back(numDirectCalls);
		vec.push_back(numIndirectCalls);
		vec.push_back(numUncondBranch);
		vec.push_back(numCondBranch);
		vec.push_back(numConstInt0Var);
		vec.push_back(numConstInt1Var);
		vec.push_back(numConstInt32Var);
		vec.push_back(numConstInt64Var);
		vec.push_back(numRefExtVar);
		vec.push_back(numRefLocalVar);
		vec.push_back(numLocalPtrVar);
		vec.push_back(numStaticPtrVar);
		vec.push_back(numAddressVar);
		vec.push_back(numIndirectRef);
		vec.push_back(numAddressFunc);

		return vec;
	}
};

struct GraphExtractionPass : public FunctionPass {
	static char ID;
	static char Separator;

	int counter;
	std::map<BasicBlock*, int> blockMap;
	std::map<int, Features> featureMap;
	std::map<int, std::vector<int>> succMap;

	GraphExtractionPass() : FunctionPass(ID) { counter = 0; }

	bool runOnFunction(Function& F) override {
		std::string Filename = "cfg." + F.getName().str() + ".out";

		std::error_code EC;
		raw_fd_ostream File(Filename.c_str(), EC, sys::fs::F_Text);

		if (!EC) {
			for (auto& B : F) {
				auto block = &B;

				int id = getBlockId(block);

				Features features;

				for (auto& I : B) {
					switch (I.getOpcode()) {
						// Terminators
					case llvm::Instruction::Ret:
					case llvm::Instruction::Br:
					case llvm::Instruction::Switch:
					case llvm::Instruction::IndirectBr:
					case llvm::Instruction::Invoke:
					case llvm::Instruction::Resume:
					case llvm::Instruction::Unreachable:
					case llvm::Instruction::CleanupRet:
					case llvm::Instruction::CatchRet:
					case llvm::Instruction::CatchPad:
					case llvm::Instruction::CatchSwitch:
					case llvm::Instruction::CallBr:
						features.numTerminatorInst++;
						break;

					// Standard unary operators...
					case llvm::Instruction::FNeg:
						features.numUnaryInst++;
						break;

					// Standard binary operators...
					case llvm::Instruction::Add:
					case llvm::Instruction::FAdd:
					case llvm::Instruction::Sub:
					case llvm::Instruction::FSub:
					case llvm::Instruction::Mul:
					case llvm::Instruction::FMul:
					case llvm::Instruction::UDiv:
					case llvm::Instruction::SDiv:
					case llvm::Instruction::FDiv:
					case llvm::Instruction::URem:
					case llvm::Instruction::SRem:
					case llvm::Instruction::FRem:
						features.numBinaryInst++;
						break;

					// Logical operators...
					case llvm::Instruction::And:
					case llvm::Instruction::Or:
					case llvm::Instruction::Xor:
						features.numLogicalInst++;
						break;

					// Memory instructions...
					case llvm::Instruction::Alloca:
					case llvm::Instruction::Load:
					case llvm::Instruction::Store:
					case llvm::Instruction::AtomicCmpXchg:
					case llvm::Instruction::AtomicRMW:
					case llvm::Instruction::Fence:
					case llvm::Instruction::GetElementPtr:
						features.numMemoryInst++;
						break;

					// Convert instructions...
					case llvm::Instruction::Trunc:
					case llvm::Instruction::ZExt:
					case llvm::Instruction::SExt:
					case llvm::Instruction::FPTrunc:
					case llvm::Instruction::FPExt:
					case llvm::Instruction::FPToUI:
					case llvm::Instruction::FPToSI:
					case llvm::Instruction::UIToFP:
					case llvm::Instruction::SIToFP:
					case llvm::Instruction::IntToPtr:
					case llvm::Instruction::PtrToInt:
					case llvm::Instruction::BitCast:
					case llvm::Instruction::AddrSpaceCast:
						features.numConvertInst++;
						break;

					// Other instructions...
					case llvm::Instruction::ICmp:
					case llvm::Instruction::FCmp:
					case llvm::Instruction::PHI:
					case llvm::Instruction::Select:
					case llvm::Instruction::Call:
					case llvm::Instruction::Shl:
					case llvm::Instruction::LShr:
					case llvm::Instruction::AShr:
					case llvm::Instruction::VAArg:
					case llvm::Instruction::ExtractElement:
					case llvm::Instruction::InsertElement:
					case llvm::Instruction::ShuffleVector:
					case llvm::Instruction::ExtractValue:
					case llvm::Instruction::InsertValue:
					case llvm::Instruction::LandingPad:
					case llvm::Instruction::CleanupPad:
					case llvm::Instruction::Freeze:
						features.numOtherInst++;
						break;

					default:
						errs() << "invalid operator found!\n";
						break;
					}
				}

				featureMap[id] = features;

				std::vector<int> succs;

				for (auto succ : successors(block)) {
					succs.push_back(getBlockId(succ));
				}

				succMap[id] = succs;

				for (auto& I : B) {
					auto instruction = &I;

					unsigned numOperands = instruction->getNumOperands();
					unsigned tempIndirectCount = 0;
					Type* instructionTy = instruction->getType();

					if (auto* SI = dyn_cast<StoreInst>(&instruction)) {
						/* There are two arguments to the store instruction:
               a value to store and an address at which to store it
            */
						Value* isInstValue = instruction->getOperand(0);
						Type* getTypeBits = isInstValue->getType();

						if (getTypeBits->isPointerTy()) {
							++features.numIndirectRef;
						}

						if (isa<Function>(isInstValue)) {
							++features.numAddressFunc;
						}

						if ((getTypeBits->isPointerTy()) && (isa<Instruction>(isInstValue))) {
							++features.numAddressVar;
						}

					} else if (auto* BI = dyn_cast<BranchInst>(&instruction)) {

						if (BI->isConditional()) {
							++features.numCondBranch;
						} else if (BI->isUnconditional()) {
							++features.numUncondBranch;
						}

					} else if (auto* SI = dyn_cast<SwitchInst>(&instruction)) {

						++features.numSwitchInst;

					} else if (auto* CI = dyn_cast<CallInst>(&instruction)) {

						Function* callFunction = CI->getCalledFunction();
						Type* callType = CI->getType();

						if (callType->isIntegerTy()) {
							++features.numIntRetCallType;
						} else if (callType->isPointerTy()) {
							++features.numPtrRetCallType;
						}

						/*  if calledFunction is nullptr and stripped value is a
                function, then, it's a direct call in the generate assembly.
                (Ref:
                https://lists.llvm.org/pipermail/llvm-dev/2018-August/125098.html)
            */
						if (callFunction == nullptr) {
							Value* calledValue = CI->getCalledValue()->stripPointerCasts();
							if (isa<Function>(calledValue)) {
								++features.numDirectCalls;
							} else {
								++features.numIndirectCalls;
							}
						} else {
							++features.numDirectCalls;
						}

						unsigned argsCount = 0;
						if (callFunction != nullptr) {
							unsigned numArgOp = CI->getNumArgOperands();

							for (int arg = 0; arg < numArgOp; arg++) {
								Type* argTy = CI->getArgOperand(arg)->getType();

								if (argTy->isPointerTy()) {
									++tempIndirectCount;
								}

								++argsCount;
							}
						}

						if (tempIndirectCount != 0) {
							++features.numCallArgPtrType;
						}

						if (argsCount > 4) {
							++features.numCall4Args;
						}
					}

					if (UnaryOperator* UN = dyn_cast<UnaryOperator>(&instruction)) {
						++features.numUnaryOpInst;
					} else if (BinaryOperator* BIO = dyn_cast<BinaryOperator>(&instruction)) {

						Value* firstBinOp = BIO->getOperand(0);
						Value* secondBinOp = BIO->getOperand(1);
						Type* firstBinOpTy = BIO->getOperand(0)->getType();
						Type* secondBinOpTy = BIO->getOperand(1)->getType();

						if ((firstBinOpTy->isIntegerTy()) && (secondBinOpTy->isIntegerTy())) {
							++features.numIntOpInst;
						} else if ((firstBinOpTy->isFloatingPointTy()) && (secondBinOpTy->isFloatingPointTy())) {
							++features.numFloatOpInst;
						} else if ((firstBinOpTy->isPointerTy()) || (secondBinOpTy->isPointerTy())) {
							++features.numPtrArithmetic;
						}

						if ((isa<ConstantInt>(firstBinOp)) || (isa<ConstantInt>(secondBinOp))) {
							++features.numOneOperandConst;
						}
					}

					if (isa<PHINode>(&instruction)) {
						++features.numPhiNodes;
						/* The num of operands is the num of arguments for a phi node*/
						features.numPhiArgs += instruction->getNumOperands();
					}

					if ((instructionTy->isVoidTy()) == 0) {
						++features.numAssignmentInst;
						/* Number of assignment instructions with the left operand
             an integer constant in the method. Here, a left operand is
             being considered as the first operand in instruction with
             two operands. */
						if (numOperands == 2) {
							if (ConstantInt* intConst = dyn_cast<ConstantInt>(instruction->getOperand(0))) {
								++features.numLeftOpIntConst;
							}
						}

						if (instructionTy->isPointerTy())
							++features.numLocalPtrVar;
					}

					if (isa<AllocaInst>(&instruction) == 0) {
						for (Use& U : instruction->operands()) {
							if (isa<Instruction>(U)) {
								++features.numRefLocalVar;
							}

							if (ConstantInt* zeroOne = dyn_cast<ConstantInt>(U)) {
								Type* getTypeBits = zeroOne->getType();

								if (getTypeBits->isIntegerTy(32)) {
									++features.numConstInt32Var;
								} else if (getTypeBits->isIntegerTy(64)) {
									++features.numConstInt64Var;
								}

								if ((zeroOne->getSExtValue()) == 0 && (getTypeBits->isIntegerTy())) {
									++features.numConstInt0Var;
								} else if ((zeroOne->getSExtValue() == 1) && (getTypeBits->isIntegerTy())) {
									++features.numConstInt1Var;
								}
							}

							if (GlobalVariable* GV = dyn_cast<GlobalVariable>(U)) {
								++features.numRefExtVar;
								if (GV->getNumOperands()) {
									Type* getType = GV->getOperand(0)->getType();
									if (getType->isPointerTy()) {
										++features.numStaticPtrVar;
									}
								}
							}
						}
					}

					if (GEPOperator* gepo = dyn_cast<GEPOperator>(&instruction)) {
						for (auto it = gepo->idx_begin(), et = gepo->idx_end(); it != et;
								 it++) {
							if (GlobalVariable* gv = dyn_cast<GlobalVariable>(*it)) {
								//++defUseCounter;
							}
						}
					}
				}

				features.numInst = block->size();
				features.avgPhiArgs = (float)features.numPhiArgs / features.numPhiNodes;
			}

			File << blockMap.size() << "\n";

			for (auto pair : blockMap) {
				auto id = pair.second;

				auto features = featureMap[id].as_vec();
				auto succs = succMap[id];

				File << id << "\n";

				File << features.size() << Separator;
				for (auto& f : features)
					File << f << Separator;
				File << "\n";

				File << succs.size() << Separator;
				for (auto& s : succs)
					File << s << Separator;
				File << "\n";
			}
		} else
			errs() << "  error opening file for writing!";
		errs() << "\n";

		return false;
	}

	int getBlockId(BasicBlock* block) {
		int id;

		if (blockMap.find(block) != blockMap.end()) {
			id = blockMap[block];
		} else {
			id = counter;
			blockMap[block] = counter++;
		}

		return id;
	}
};
} // namespace

char GraphExtractionPass::ID = 0;
char GraphExtractionPass::Separator = ' ';

static RegisterPass<GraphExtractionPass> X("graph", "Graph Extraction Pass",
																					 false, false);
static RegisterStandardPasses Y(PassManagerBuilder::EP_EarlyAsPossible,
																[](const PassManagerBuilder& Builder,
																	 legacy::PassManagerBase& PM) {
																	PM.add(new GraphExtractionPass());
																});
