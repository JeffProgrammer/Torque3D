//-----------------------------------------------------------------------------
// Copyright (c) 2012 GarageGames, LLC
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to
// deal in the Software without restriction, including without limitation the
// rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
// sell copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
// IN THE SOFTWARE.
//-----------------------------------------------------------------------------

#include "platform/platform.h"
#include "console/console.h"
#include "console/telnetDebugger.h"

#include "console/ast.h"
#include "core/tAlgorithm.h"

#include "core/strings/findMatch.h"
#include "console/consoleInternal.h"
#include "core/stream/fileStream.h"
#include "console/compiler.h"

#include "console/simBase.h"

template< typename T >
struct Token
{
   T value;
   S32 lineNumber;
};
#include "console/cmdgram.h"

namespace Compiler
{
   U32 compileBlock(StmtNode *block, CodeStream &codeStream, U32 ip)
   {
      for (StmtNode *walk = block; walk; walk = walk->getNext())
         ip = walk->compileStmt(codeStream, ip);
      return codeStream.tell();
   }

   inline bool isSimpleVarLookup(ExprNode *arrayExpr, StringTableEntry &varName)
   {
      if (arrayExpr == nullptr)
      {
         varName = StringTable->insert("");
         return false;
      }

      // No double arrays allowed for optimization.
      VarNode *var = dynamic_cast<VarNode*>(arrayExpr);
      if (var && !var->arrayIndex)
      {
         StringTableEntry arrayVar = StringTable->insert(var->varName);
         precompileIdent(arrayVar);
         varName = arrayVar;
         return true;
      }
      return false;
   }

   // Do not allow 'recursive' %this optimizations. It can lead to weird bytecode
   // generation since we can only optimize one expression at a time.
   static bool OnlyOneThisOptimization = false;

   inline bool isThisVar(ExprNode *objectExpr)
   {
      // If we are currently optimizing a this var, don't allow extra optimization.
      if (objectExpr == nullptr || OnlyOneThisOptimization)
         return false;

      VarNode *thisVar = dynamic_cast<VarNode*>(objectExpr);
      if (thisVar && thisVar->varName == StringTable->insert("%this"))
         return true;
      return false;
   }

   inline void optimizeThisPointer(CodeStream &codeStream, ExprNode *arrayExpr, U32 &ip, StringTableEntry slotName)
   {
      OnlyOneThisOptimization = true;

      // Is the array a simple variable? If so, we can optimize that.
      StringTableEntry varName = nullptr;
      bool simple = false;

      if (arrayExpr)
      {
         simple = isSimpleVarLookup(arrayExpr, varName);
         if (!simple)
         {
            // Less optimized array setting.
            codeStream.emit(OP_ADVANCE_STR);
            CompileRet result = arrayExpr->compile(codeStream, ip, TypeReqString);
            ip = result.ip;
         }
      }

      codeStream.emit(OP_SETCURFIELD_THIS);
      codeStream.emitSTE(slotName);

      if (arrayExpr)
      {
         if (simple)
         {
            codeStream.emit(OP_SETCURFIELD_ARRAY_VAR);
            codeStream.emitSTE(varName);
         }
         else
         {
            codeStream.emit(OP_SETCURFIELD_ARRAY);
            codeStream.emit(OP_TERMINATE_REWIND_STR);
         }
      }

      OnlyOneThisOptimization = false;
   }
}

using namespace Compiler;

//-----------------------------------------------------------------------------

void StmtNode::addBreakLine(CodeStream &code)
{
   code.addBreakLine(dbgLineNumber, code.tell());
}

//------------------------------------------------------------

StmtNode::StmtNode()
{
   mNext = NULL;
   dbgFileName = CodeBlock::smCurrentParser->getCurrentFile();
   dbgLineNumber = 0;
}

void StmtNode::setPackage(StringTableEntry)
{
}

void StmtNode::append(StmtNode *next)
{
   StmtNode *walk = this;
   while (walk->mNext)
      walk = walk->mNext;
   walk->mNext = next;
}

void FunctionDeclStmtNode::setPackage(StringTableEntry packageName)
{
   package = packageName;
}

//------------------------------------------------------------
//
// Console language compilers
//
//------------------------------------------------------------

static U32 conversionOp(TypeReq src, TypeReq dst)
{
   if (src == TypeReqString)
   {
      switch (dst)
      {
         case TypeReqUInt:
            return OP_STR_TO_UINT;
         case TypeReqFloat:
            return OP_STR_TO_FLT;
         case TypeReqNone:
            return OP_STR_TO_NONE;
         case TypeReqVar:
            return OP_SAVEVAR_STR;
         default:
            break;
      }
   }
   else if (src == TypeReqFloat)
   {
      switch (dst)
      {
         case TypeReqUInt:
            return OP_FLT_TO_UINT;
         case TypeReqString:
            return OP_FLT_TO_STR;
         case TypeReqNone:
            return OP_FLT_TO_NONE;
         case TypeReqVar:
            return OP_SAVEVAR_FLT;
         default:
            break;
      }
   }
   else if (src == TypeReqUInt)
   {
      switch (dst)
      {
         case TypeReqFloat:
            return OP_UINT_TO_FLT;
         case TypeReqString:
            return OP_UINT_TO_STR;
         case TypeReqNone:
            return OP_UINT_TO_NONE;
         case TypeReqVar:
            return OP_SAVEVAR_UINT;
         default:
            break;
      }
   }
   else if (src == TypeReqVar)
   {
      switch (dst)
      {
         case TypeReqUInt:
            return OP_LOADVAR_UINT;
         case TypeReqFloat:
            return OP_LOADVAR_FLT;
         case TypeReqString:
            return OP_LOADVAR_STR;
         case TypeReqNone:
            return OP_COPYVAR_TO_NONE;
         default:
            break;
      }
   }
   return OP_INVALID;
}

//------------------------------------------------------------

U32 BreakStmtNode::compileStmt(CodeStream &codeStream, U32 ip)
{
   if (codeStream.inLoop())
   {
      addBreakLine(codeStream);
      codeStream.emit(OP_JMP);
      codeStream.emitFix(CodeStream::FIXTYPE_BREAK);
   }
   else
   {
      Con::warnf(ConsoleLogEntry::General, "%s (%d): break outside of loop... ignoring.", dbgFileName, dbgLineNumber);
   }
   return codeStream.tell();
}

//------------------------------------------------------------

U32 ContinueStmtNode::compileStmt(CodeStream &codeStream, U32 ip)
{
   if (codeStream.inLoop())
   {
      addBreakLine(codeStream);
      codeStream.emit(OP_JMP);
      codeStream.emitFix(CodeStream::FIXTYPE_CONTINUE);
   }
   else
   {
      Con::warnf(ConsoleLogEntry::General, "%s (%d): continue outside of loop... ignoring.", dbgFileName, dbgLineNumber);
   }
   return codeStream.tell();
}

//------------------------------------------------------------

U32 ExprNode::compileStmt(CodeStream &codeStream, U32 ip)
{
   addBreakLine(codeStream);
   return compile(codeStream, ip, TypeReqNone).ip;
}

//------------------------------------------------------------

U32 ReturnStmtNode::compileStmt(CodeStream &codeStream, U32 ip)
{
   addBreakLine(codeStream);
   if (!expr)
      codeStream.emit(OP_RETURN_VOID);
   else
   {
      TypeReq walkType = expr->getPreferredType();
      if (walkType == TypeReqNone) walkType = TypeReqString;

      CompileRet result = expr->compile(codeStream, ip, walkType);
      ip = result.ip;

      // Return the correct type
      switch (walkType) {
         case TypeReqUInt:
            codeStream.emit(OP_RETURN_UINT);
            break;
         case TypeReqFloat:
            codeStream.emit(OP_RETURN_FLT);
            break;
         default:
            codeStream.emit(OP_RETURN);
            break;
      }
   }
   return codeStream.tell();
}

//------------------------------------------------------------

ExprNode *IfStmtNode::getSwitchOR(ExprNode *left, ExprNode *list, bool string)
{
   ExprNode *nextExpr = (ExprNode *)list->getNext();
   ExprNode *test;
   if (string)
      test = StreqExprNode::alloc(left->dbgLineNumber, left, list, true);
   else
      test = IntBinaryExprNode::alloc(left->dbgLineNumber, opEQ, left, list);
   if (!nextExpr)
      return test;
   return IntBinaryExprNode::alloc(test->dbgLineNumber, opOR, test, getSwitchOR(left, nextExpr, string));
}

void IfStmtNode::propagateSwitchExpr(ExprNode *left, bool string)
{
   testExpr = getSwitchOR(left, testExpr, string);
   if (propagate && elseBlock)
      ((IfStmtNode *)elseBlock)->propagateSwitchExpr(left, string);
}

U32 IfStmtNode::compileStmt(CodeStream &codeStream, U32 ip)
{
   U32 endifIp, elseIp;
   addBreakLine(codeStream);

   if (testExpr->getPreferredType() == TypeReqUInt)
   {
      integer = true;
   }
   else
   {
      integer = false;
   }

   CompileRet result = testExpr->compile(codeStream, ip, integer ? TypeReqUInt : TypeReqFloat);
   ip = result.ip;
   codeStream.emit(integer ? OP_JMPIFNOT : OP_JMPIFFNOT);

   if (elseBlock)
   {
      elseIp = codeStream.emit(0);
      elseOffset = compileBlock(ifBlock, codeStream, ip) + 2;
      codeStream.emit(OP_JMP);
      endifIp = codeStream.emit(0);
      endifOffset = compileBlock(elseBlock, codeStream, ip);

      codeStream.patch(endifIp, endifOffset);
      codeStream.patch(elseIp, elseOffset);
   }
   else
   {
      endifIp = codeStream.emit(0);
      endifOffset = compileBlock(ifBlock, codeStream, ip);

      codeStream.patch(endifIp, endifOffset);
   }

   // Resolve fixes
   return codeStream.tell();
}

//------------------------------------------------------------

U32 LoopStmtNode::compileStmt(CodeStream &codeStream, U32 ip)
{
   if (testExpr->getPreferredType() == TypeReqUInt)
   {
      integer = true;
   }
   else
   {
      integer = false;
   }

   // if it's a for loop or a while loop it goes:
   // initExpr
   // testExpr
   // OP_JMPIFNOT to break point
   // loopStartPoint:
   // loopBlock
   // continuePoint:
   // endLoopExpr
   // testExpr
   // OP_JMPIF loopStartPoint
   // breakPoint:

   // otherwise if it's a do ... while() it goes:
   // initExpr
   // loopStartPoint:
   // loopBlock
   // continuePoint:
   // endLoopExpr
   // testExpr
   // OP_JMPIF loopStartPoint
   // breakPoint:

   // loopBlockStart == start of loop block
   // continue == skip to end
   // break == exit loop


   addBreakLine(codeStream);
   codeStream.pushFixScope(true);

   if (initExpr)
      ip = initExpr->compile(codeStream, ip, TypeReqNone).ip;

   if (!isDoLoop)
   {
      ip = testExpr->compile(codeStream, ip, integer ? TypeReqUInt : TypeReqFloat).ip;
      codeStream.emit(integer ? OP_JMPIFNOT : OP_JMPIFFNOT);
      codeStream.emitFix(CodeStream::FIXTYPE_BREAK);
   }

   // Compile internals of loop.
   loopBlockStartOffset = codeStream.tell();
   continueOffset = compileBlock(loopBlock, codeStream, ip);

   if (endLoopExpr)
      ip = endLoopExpr->compile(codeStream, ip, TypeReqNone).ip;

   ip = testExpr->compile(codeStream, ip, integer ? TypeReqUInt : TypeReqFloat).ip;

   codeStream.emit(integer ? OP_JMPIF : OP_JMPIFF);
   codeStream.emitFix(CodeStream::FIXTYPE_LOOPBLOCKSTART);

   breakOffset = codeStream.tell(); // exit loop

   codeStream.fixLoop(loopBlockStartOffset, breakOffset, continueOffset);
   codeStream.popFixScope();

   return codeStream.tell();
}

//------------------------------------------------------------

U32 IterStmtNode::compileStmt(CodeStream &codeStream, U32 ip)
{
   // Instruction sequence:
   //
   //   containerExpr
   //   OP_ITER_BEGIN varName .fail
   // .continue:
   //   OP_ITER .break
   //   body
   //   OP_JMP .continue
   // .break:
   //   OP_ITER_END
   // .fail:

   addBreakLine(codeStream);

   codeStream.pushFixScope(true);

   const U32 startIp = ip;
   containerExpr->compile(codeStream, startIp, TypeReqString);

   codeStream.emit(isStringIter ? OP_ITER_BEGIN_STR : OP_ITER_BEGIN);
   codeStream.emitSTE(varName);
   const U32 finalFix = codeStream.emit(0);
   const U32 continueIp = codeStream.emit(OP_ITER);
   codeStream.emitFix(CodeStream::FIXTYPE_BREAK);
   const U32 bodyIp = codeStream.tell();

   const U32 jmpIp = compileBlock(body, codeStream, bodyIp);
   const U32 breakIp = jmpIp + 2;
   const U32 finalIp = breakIp + 1;

   codeStream.emit(OP_JMP);
   codeStream.emitFix(CodeStream::FIXTYPE_CONTINUE);
   codeStream.emit(OP_ITER_END);

   codeStream.patch(finalFix, finalIp);
   codeStream.fixLoop(bodyIp, breakIp, continueIp);
   codeStream.popFixScope();

   return codeStream.tell();
}

//------------------------------------------------------------

CompileRet ConditionalExprNode::compile(CodeStream &codeStream, U32 ip, TypeReq type)
{
   // code is testExpr
   // JMPIFNOT falseStart
   // trueExpr
   // JMP end
   // falseExpr
   if (testExpr->getPreferredType() == TypeReqUInt)
   {
      integer = true;
   }
   else
   {
      integer = false;
   }

   ip = testExpr->compile(codeStream, ip, integer ? TypeReqUInt : TypeReqFloat).ip;
   codeStream.emit(integer ? OP_JMPIFNOT : OP_JMPIFFNOT);

   U32 jumpElseIp = codeStream.emit(0);
   ip = trueExpr->compile(codeStream, ip, type).ip;
   codeStream.emit(OP_JMP);
   U32 jumpEndIp = codeStream.emit(0);
   codeStream.patch(jumpElseIp, codeStream.tell());
   ip = falseExpr->compile(codeStream, ip, type).ip;
   codeStream.patch(jumpEndIp, codeStream.tell());

   return { codeStream.tell(), StringTable->insert("bool") };
}

TypeReq ConditionalExprNode::getPreferredType()
{
   return trueExpr->getPreferredType();
}

//------------------------------------------------------------

CompileRet FloatBinaryExprNode::compile(CodeStream &codeStream, U32 ip, TypeReq type)
{
   ip = right->compile(codeStream, ip, TypeReqFloat).ip;
   ip = left->compile(codeStream, ip, TypeReqFloat).ip;

   U32 operand = OP_INVALID;
   switch (op)
   {
      case '+':
         operand = OP_ADD;
         break;
      case '-':
         operand = OP_SUB;
         break;
      case '/':
         operand = OP_DIV;
         break;
      case '*':
         operand = OP_MUL;
         break;
   }
   codeStream.emit(operand);
   if (type != TypeReqFloat)
      codeStream.emit(conversionOp(TypeReqFloat, type));

   return { codeStream.tell(), nullptr };
}

TypeReq FloatBinaryExprNode::getPreferredType()
{
   return TypeReqFloat;
}

//------------------------------------------------------------

void IntBinaryExprNode::getSubTypeOperand()
{
   subType = TypeReqUInt;
   switch (op)
   {
      case '^':
         operand = OP_XOR;
         break;
      case '%':
         operand = OP_MOD;
         break;
      case '&':
         operand = OP_BITAND;
         break;
      case '|':
         operand = OP_BITOR;
         break;
      case '<':
         operand = OP_CMPLT;
         subType = TypeReqFloat;
         break;
      case '>':
         operand = OP_CMPGR;
         subType = TypeReqFloat;
         break;
      case opGE:
         operand = OP_CMPGE;
         subType = TypeReqFloat;
         break;
      case opLE:
         operand = OP_CMPLE;
         subType = TypeReqFloat;
         break;
      case opEQ:
         operand = OP_CMPEQ;
         subType = TypeReqFloat;
         break;
      case opNE:
         operand = OP_CMPNE;
         subType = TypeReqFloat;
         break;
      case opOR:
         operand = OP_OR;
         break;
      case opAND:
         operand = OP_AND;
         break;
      case opSHR:
         operand = OP_SHR;
         break;
      case opSHL:
         operand = OP_SHL;
         break;
   }
}

CompileRet IntBinaryExprNode::compile(CodeStream &codeStream, U32 ip, TypeReq type)
{
   getSubTypeOperand();

   if (operand == OP_OR || operand == OP_AND)
   {
      ip = left->compile(codeStream, ip, subType).ip;
      codeStream.emit(operand == OP_OR ? OP_JMPIF_NP : OP_JMPIFNOT_NP);
      U32 jmpIp = codeStream.emit(0);
      ip = right->compile(codeStream, ip, subType).ip;
      codeStream.patch(jmpIp, ip);
   }
   else
   {
      ip = right->compile(codeStream, ip, subType).ip;
      ip = left->compile(codeStream, ip, subType).ip;
      codeStream.emit(operand);
   }
   if (type != TypeReqUInt)
      codeStream.emit(conversionOp(TypeReqUInt, type));
   return { codeStream.tell(), nullptr };
}

TypeReq IntBinaryExprNode::getPreferredType()
{
   return TypeReqUInt;
}

//------------------------------------------------------------

CompileRet StreqExprNode::compile(CodeStream &codeStream, U32 ip, TypeReq type)
{
   // eval str left
   // OP_ADVANCE_STR_NUL
   // eval str right
   // OP_COMPARE_STR
   // optional conversion

   ip = left->compile(codeStream, ip, TypeReqString).ip;
   codeStream.emit(OP_ADVANCE_STR_NUL);
   ip = right->compile(codeStream, ip, TypeReqString).ip;
   codeStream.emit(OP_COMPARE_STR);
   if (!eq)
      codeStream.emit(OP_NOT);
   if (type != TypeReqUInt)
      codeStream.emit(conversionOp(TypeReqUInt, type));

   return { codeStream.tell(), StringTable->insert("bool") };
}

TypeReq StreqExprNode::getPreferredType()
{
   return TypeReqUInt;
}

//------------------------------------------------------------

CompileRet StrcatExprNode::compile(CodeStream &codeStream, U32 ip, TypeReq type)
{
   ip = left->compile(codeStream, ip, TypeReqString).ip;
   if (!appendChar)
      codeStream.emit(OP_ADVANCE_STR);
   else
   {
      codeStream.emit(OP_ADVANCE_STR_APPENDCHAR);
      codeStream.emit(appendChar);
   }
   ip = right->compile(codeStream, ip, TypeReqString).ip;
   codeStream.emit(OP_REWIND_STR);
   if (type == TypeReqUInt)
      codeStream.emit(OP_STR_TO_UINT);
   else if (type == TypeReqFloat)
      codeStream.emit(OP_STR_TO_FLT);

   return { codeStream.tell(), StringTable->insert("string") };
}

TypeReq StrcatExprNode::getPreferredType()
{
   return TypeReqString;
}

//------------------------------------------------------------

CompileRet CommaCatExprNode::compile(CodeStream &codeStream, U32 ip, TypeReq type)
{
   ip = left->compile(codeStream, ip, TypeReqString).ip;
   codeStream.emit(OP_ADVANCE_STR_COMMA);
   ip = right->compile(codeStream, ip, TypeReqString).ip;
   codeStream.emit(OP_REWIND_STR);

   // At this point the stack has the concatenated string.

   // But we're paranoid, so accept (but whine) if we get an oddity...
   if (type == TypeReqUInt || type == TypeReqFloat)
      Con::warnf(ConsoleLogEntry::General, "%s (%d): converting comma string to a number... probably wrong.", dbgFileName, dbgLineNumber);
   if (type == TypeReqUInt)
      codeStream.emit(OP_STR_TO_UINT);
   else if (type == TypeReqFloat)
      codeStream.emit(OP_STR_TO_FLT);

   return { codeStream.tell(), nullptr };
}

TypeReq CommaCatExprNode::getPreferredType()
{
   return TypeReqString;
}

//------------------------------------------------------------

CompileRet IntUnaryExprNode::compile(CodeStream &codeStream, U32 ip, TypeReq type)
{
   integer = true;
   TypeReq prefType = expr->getPreferredType();
   if (op == '!' && (prefType == TypeReqFloat || prefType == TypeReqString))
      integer = false;

   ip = expr->compile(codeStream, ip, integer ? TypeReqUInt : TypeReqFloat).ip;
   if (op == '!')
      codeStream.emit(integer ? OP_NOT : OP_NOTF);
   else if (op == '~')
      codeStream.emit(OP_ONESCOMPLEMENT);
   if (type != TypeReqUInt)
      codeStream.emit(conversionOp(TypeReqUInt, type));

   if (op == '!')
      return { codeStream.tell(), StringTable->insert("bool") };
   return { codeStream.tell(), StringTable->insert("int") };
}

TypeReq IntUnaryExprNode::getPreferredType()
{
   return TypeReqUInt;
}

//------------------------------------------------------------

CompileRet FloatUnaryExprNode::compile(CodeStream &codeStream, U32 ip, TypeReq type)
{
   ip = expr->compile(codeStream, ip, TypeReqFloat).ip;
   codeStream.emit(OP_NEG);
   if (type != TypeReqFloat)
      codeStream.emit(conversionOp(TypeReqFloat, type));
   return { codeStream.tell(), StringTable->insert("float") };
}

TypeReq FloatUnaryExprNode::getPreferredType()
{
   return TypeReqFloat;
}

//------------------------------------------------------------

CompileRet VarNode::compile(CodeStream &codeStream, U32 ip, TypeReq type)
{
   // if this has an arrayIndex and we are not short circuiting from a constant.
   //    if we are a var node
   //    OP_SETCURVAR_ARRAY_VARLOOKUP
   //    varName
   //    varNodeVarName

   //    else
   //    OP_LOADIMMED_IDENT
   //    varName
   //    OP_ADVANCE_STR
   //    evaluate arrayIndex TypeReqString
   //    OP_REWIND_STR
   //    OP_SETCURVAR_ARRAY
   //    OP_LOADVAR (type)

   // else
   // OP_SETCURVAR
   // varName
   // OP_LOADVAR (type)

   if (type == TypeReqNone)
      return { codeStream.tell(), nullptr };

   bool shortCircuit = false;
   if (arrayIndex)
   {
      // If we have a constant, shortcircuit the array logic.

      IntNode *intNode = dynamic_cast<IntNode*>(arrayIndex);
      StrConstNode *strNode = dynamic_cast<StrConstNode*>(arrayIndex);
      if (intNode)
      {
         varName = StringTable->insert(avar("%s%d", varName, intNode->value));
         shortCircuit = true;
      }
      else if (strNode)
      {
         varName = StringTable->insert(avar("%s%s", varName, strNode->str));
         shortCircuit = true;
      }
   }

   precompileIdent(varName);

   if (arrayIndex && !shortCircuit)
   {
      // Ok, lets try to optimize %var[%someothervar] as this is
      // a common case for array usage.
      StringTableEntry varNodeVarName;
      if (isSimpleVarLookup(arrayIndex, varNodeVarName))
      {
         codeStream.emit(OP_SETCURVAR_ARRAY_VARLOOKUP);
         codeStream.emitSTE(varName);
         codeStream.emitSTE(varNodeVarName);
      }
      else
      {
         codeStream.emit(OP_LOADIMMED_IDENT);
         codeStream.emitSTE(varName);
         codeStream.emit(OP_ADVANCE_STR);
         ip = arrayIndex->compile(codeStream, ip, TypeReqString).ip;
         codeStream.emit(OP_REWIND_STR);
         codeStream.emit(OP_SETCURVAR_ARRAY);
      }
   }
   else
   {
      codeStream.emit(OP_SETCURVAR);
      codeStream.emitSTE(varName);
   }

   switch (type)
   {
      case TypeReqUInt:
         codeStream.emit(OP_LOADVAR_UINT);
         break;
      case TypeReqFloat:
         codeStream.emit(OP_LOADVAR_FLT);
         break;
      case TypeReqString:
         codeStream.emit(OP_LOADVAR_STR);
         break;
      case TypeReqVar:
         codeStream.emit(OP_LOADVAR_VAR);
         break;
      case TypeReqNone:
         break;
      default:
         break;
   }

   return { codeStream.tell(), getCurrentTypeTable()->lookupType(varName) };
}

TypeReq VarNode::getPreferredType()
{
   return TypeReqNone; // no preferred type
}

//------------------------------------------------------------

CompileRet ParamNode::compile(CodeStream& codeStream, U32 ip, TypeReq type)
{
   if (type == TypeReqNone)
      return { codeStream.tell(), nullptr };

   precompileIdent(varName);

   codeStream.emit(OP_SETCURVAR);
   codeStream.emitSTE(varName);

   switch (type)
   {
   case TypeReqUInt:
      codeStream.emit(OP_LOADVAR_UINT);
      break;
   case TypeReqFloat:
      codeStream.emit(OP_LOADVAR_FLT);
      break;
   case TypeReqString:
      codeStream.emit(OP_LOADVAR_STR);
      break;
   case TypeReqVar:
      codeStream.emit(OP_LOADVAR_VAR);
      break;
   default:
      break;
   }

   return { codeStream.tell(), nullptr };
}

TypeReq ParamNode::getPreferredType()
{
   return TypeReqNone;
}

//------------------------------------------------------------

CompileRet IntNode::compile(CodeStream &codeStream, U32 ip, TypeReq type)
{
   if (type == TypeReqString)
      index = getCurrentStringTable()->addIntString(value);
   else if (type == TypeReqFloat)
      index = getCurrentFloatTable()->add(value);

   switch (type)
   {
      case TypeReqUInt:
         codeStream.emit(OP_LOADIMMED_UINT);
         codeStream.emit(value);
         break;
      case TypeReqString:
         codeStream.emit(OP_LOADIMMED_STR);
         codeStream.emit(index);
         break;
      case TypeReqFloat:
         codeStream.emit(OP_LOADIMMED_FLT);
         codeStream.emit(index);
         break;
      case TypeReqNone:
         break;
   }

   return { codeStream.tell(), StringTable->insert("int") };
}

TypeReq IntNode::getPreferredType()
{
   return TypeReqUInt;
}

//------------------------------------------------------------

CompileRet FloatNode::compile(CodeStream &codeStream, U32 ip, TypeReq type)
{
   if (type == TypeReqString)
      index = getCurrentStringTable()->addFloatString(value);
   else if (type == TypeReqFloat)
      index = getCurrentFloatTable()->add(value);

   switch (type)
   {
      case TypeReqUInt:
         codeStream.emit(OP_LOADIMMED_UINT);
         codeStream.emit(U32(value));
         break;
      case TypeReqString:
         codeStream.emit(OP_LOADIMMED_STR);
         codeStream.emit(index);
         break;
      case TypeReqFloat:
         codeStream.emit(OP_LOADIMMED_FLT);
         codeStream.emit(index);
         break;
      case TypeReqNone:
         break;
   }

   return { codeStream.tell(), StringTable->insert("float") };
}

TypeReq FloatNode::getPreferredType()
{
   return TypeReqFloat;
}

//------------------------------------------------------------

CompileRet StrConstNode::compile(CodeStream &codeStream, U32 ip, TypeReq type)
{
   // Early out for documentation block.
   if (doc)
   {
      index = getCurrentStringTable()->add(str, true, tag);
   }
   else if (type == TypeReqString)
   {
      index = getCurrentStringTable()->add(str, true, tag);
   }
   else if (type != TypeReqNone)
   {
      fVal = consoleStringToNumber(str, dbgFileName, dbgLineNumber);
      if (type == TypeReqFloat)
      {
         index = getCurrentFloatTable()->add(fVal);
      }
   }

   // If this is a DOCBLOCK, then process w/ appropriate op...
   if (doc)
   {
      codeStream.emit(OP_DOCBLOCK_STR);
      codeStream.emit(index);
      return { codeStream.tell(), nullptr };
   }

   // Otherwise, deal with it normally as a string literal case.
   switch (type)
   {
      case TypeReqString:
         codeStream.emit(tag ? OP_TAG_TO_STR : OP_LOADIMMED_STR);
         codeStream.emit(index);
         break;
      case TypeReqUInt:
         codeStream.emit(OP_LOADIMMED_UINT);
         codeStream.emit(U32(fVal));
         break;
      case TypeReqFloat:
         codeStream.emit(OP_LOADIMMED_FLT);
         codeStream.emit(index);
         break;
      case TypeReqNone:
         break;
   }

   return { codeStream.tell(), StringTable->insert("string") };
}

TypeReq StrConstNode::getPreferredType()
{
   return TypeReqString;
}

//------------------------------------------------------------

CompileRet ConstantNode::compile(CodeStream &codeStream, U32 ip, TypeReq type)
{
   if (type == TypeReqString)
   {
      precompileIdent(value);
   }
   else if (type != TypeReqNone)
   {
      fVal = consoleStringToNumber(value, dbgFileName, dbgLineNumber);
      if (type == TypeReqFloat)
         index = getCurrentFloatTable()->add(fVal);
   }

   switch (type)
   {
      case TypeReqString:
         codeStream.emit(OP_LOADIMMED_IDENT);
         codeStream.emitSTE(value);
         break;
      case TypeReqUInt:
         codeStream.emit(OP_LOADIMMED_UINT);
         codeStream.emit(U32(fVal));
         break;
      case TypeReqFloat:
         codeStream.emit(OP_LOADIMMED_FLT);
         codeStream.emit(index);
         break;
      case TypeReqNone:
         break;
   }

   return { codeStream.tell(), nullptr };
}

TypeReq ConstantNode::getPreferredType()
{
   return TypeReqString;
}

//------------------------------------------------------------

CompileRet AssignExprNode::compile(CodeStream &codeStream, U32 ip, TypeReq type)
{
   subType = expr->getPreferredType();
   if (subType == TypeReqNone)
      subType = type;
   if (subType == TypeReqNone)
   {
      // What we need to do in this case is turn it into a VarNode reference. 
      // Unfortunately other nodes such as field access (SlotAccessNode) 
      // cannot be optimized in the same manner as all fields are exposed 
      // and set as strings.
      if (dynamic_cast<VarNode*>(expr) != NULL)
      {
         subType = TypeReqVar;
      }
      else
      {
         subType = TypeReqString;
      }
   }

   //if we are an array index and we are gonna short circuit
   // eval expr
   // compute new varName
   // OP_SETCURVAR_CREATE
   // varName
   // OP_SAVEVAR

   //else if it's an array expr and we don't short circuit, the formula is:
   // eval expr
   // (push and pop if it's TypeReqString) OP_ADVANCE_STR
   // if array lookup is varnode
   //    OP_SETCURVAR_ARRAY_CREATE_VARLOOKUP
   //    varName
   //    varNodeVarName
   // else
   //    OP_LOADIMMED_IDENT
   //    varName
   //    OP_ADVANCE_STR
   //    eval array
   //    OP_REWIND_STR
   //    OP_SETCURVAR_ARRAY_CREATE
   // endif
   // OP_TERMINATE_REWIND_STR
   // OP_SAVEVAR

   //else
   // eval expr
   // OP_SETCURVAR_CREATE
   // varname
   // OP_SAVEVAR

   ip = expr->compile(codeStream, ip, subType).ip;

   bool shortCircuit = false;
   if (arrayIndex)
   {
      // If we have a constant, shortcircuit the array logic.

      IntNode *intNode = dynamic_cast<IntNode*>(arrayIndex);
      StrConstNode *strNode = dynamic_cast<StrConstNode*>(arrayIndex);
      if (intNode)
      {
         varName = StringTable->insert(avar("%s%d", varName, intNode->value));
         shortCircuit = true;
      }
      else if (strNode)
      {
         varName = StringTable->insert(avar("%s%s", varName, strNode->str));
         shortCircuit = true;
      }
   }

   precompileIdent(varName);

   if (arrayIndex && !shortCircuit)
   {
      if (subType == TypeReqString)
         codeStream.emit(OP_ADVANCE_STR);

      // Ok, lets try to optimize %var[%someothervar] as this is
      // a common case for array usage.
      StringTableEntry varNodeVarName;
      if (isSimpleVarLookup(arrayIndex, varNodeVarName))
      {
         codeStream.emit(OP_SETCURVAR_ARRAY_CREATE_VARLOOKUP);
         codeStream.emitSTE(varName);
         codeStream.emitSTE(varNodeVarName);
      }
      else
      {
         codeStream.emit(OP_LOADIMMED_IDENT);
         codeStream.emitSTE(varName);

         codeStream.emit(OP_ADVANCE_STR);
         ip = arrayIndex->compile(codeStream, ip, TypeReqString).ip;
         codeStream.emit(OP_REWIND_STR);
         codeStream.emit(OP_SETCURVAR_ARRAY_CREATE);
      }

      if (subType == TypeReqString)
         codeStream.emit(OP_TERMINATE_REWIND_STR);
   }
   else
   {
      codeStream.emit(OP_SETCURVAR_CREATE);
      codeStream.emitSTE(varName);
   }
   switch (subType)
   {
      case TypeReqString:
         codeStream.emit(OP_SAVEVAR_STR);
         break;
      case TypeReqUInt:
         codeStream.emit(OP_SAVEVAR_UINT);
         break;
      case TypeReqFloat:
         codeStream.emit(OP_SAVEVAR_FLT);
         break;
      case TypeReqVar:
         codeStream.emit(OP_SAVEVAR_VAR);
         break;
      case TypeReqNone:
         break;
   }
   if (type != subType)
      codeStream.emit(conversionOp(subType, type));

   return { codeStream.tell(), nullptr };
}

TypeReq AssignExprNode::getPreferredType()
{
   return expr->getPreferredType();
}

//------------------------------------------------------------

static void getAssignOpTypeOp(S32 op, TypeReq &type, U32 &operand)
{
   switch (op)
   {
      case '+':
      case opPLUSPLUS:
         type = TypeReqFloat;
         operand = OP_ADD;
         break;
      case '-':
      case opMINUSMINUS:
         type = TypeReqFloat;
         operand = OP_SUB;
         break;
      case '*':
         type = TypeReqFloat;
         operand = OP_MUL;
         break;
      case '/':
         type = TypeReqFloat;
         operand = OP_DIV;
         break;
      case '%':
         type = TypeReqUInt;
         operand = OP_MOD;
         break;
      case '&':
         type = TypeReqUInt;
         operand = OP_BITAND;
         break;
      case '^':
         type = TypeReqUInt;
         operand = OP_XOR;
         break;
      case '|':
         type = TypeReqUInt;
         operand = OP_BITOR;
         break;
      case opSHL:
         type = TypeReqUInt;
         operand = OP_SHL;
         break;
      case opSHR:
         type = TypeReqUInt;
         operand = OP_SHR;
         break;
   }
}

CompileRet AssignOpExprNode::compile(CodeStream &codeStream, U32 ip, TypeReq type)
{

   // goes like this...
   //
   // IF no array index && (op == OPPLUSPLUS or op == OPMINUSMINUS)
   //    if op == OPPLUSPLUS
   //       OP_INC
   //       varName
   //    else if op == OPMINUSMINUS
   //       OP_DEC
   //       varName
   //    else
   //       OP_INVALID
   //    endif
   // ELSE
   //    eval expr as float or int
   //    if there's an arrayIndex and we don't short circuit
   //       if arrayIndex is a var node
   //          OP_SETCURVAR_ARRAY_CREATE_VARLOOKUP
   //          varName
   //          varNodeVarName
   //       else
   //          OP_LOADIMMED_IDENT
   //          varName
   //          OP_ADVANCE_STR
   //          eval arrayIndex stringwise
   //          OP_REWIND_STR
   //          OP_SETCURVAR_ARRAY_CREATE
   //       endif
   //    else
   //       OP_SETCURVAR_CREATE
   //       varName
   //    endif
   //    OP_LOADVAR_FLT or UINT
   //    operand
   //    OP_SAVEVAR_FLT or UINT
   // ENDIF
   //
   // if subtype != type
   //    convert type
   // endif

   // conversion OP if necessary.
   getAssignOpTypeOp(op, subType, operand);

   // ++ or -- optimization support for non indexed variables.
   if ((!arrayIndex) && (op == opPLUSPLUS || op == opMINUSMINUS))
   {
      precompileIdent(varName);

      if (op == opPLUSPLUS)
      {
         codeStream.emit(OP_INC);
         codeStream.emitSTE(varName);
      }
      else if (op == opMINUSMINUS)
      {
         codeStream.emit(OP_DEC);
         codeStream.emitSTE(varName);
      }
      else
      {
         // This should NEVER happen. This is just for sanity.
         AssertISV(false, "Tried to use ++ or -- but something weird happened.");
         codeStream.emit(OP_INVALID);
      }
   }
   else
   {
      ip = expr->compile(codeStream, ip, subType).ip;

      bool shortCircuit = false;
      if (arrayIndex)
      {
         // If we have a constant, shortcircuit the array logic.

         IntNode *intNode = dynamic_cast<IntNode*>(arrayIndex);
         StrConstNode *strNode = dynamic_cast<StrConstNode*>(arrayIndex);
         if (intNode)
         {
            varName = StringTable->insert(avar("%s%d", varName, intNode->value));
            shortCircuit = true;
         }
         else if (strNode)
         {
            varName = StringTable->insert(avar("%s%s", varName, strNode->str));
            shortCircuit = true;
         }
      }

      precompileIdent(varName);

      if (!arrayIndex || shortCircuit)
      {
         codeStream.emit(OP_SETCURVAR_CREATE);
         codeStream.emitSTE(varName);
      }
      else
      {
         // Ok, lets try to optimize %var[%someothervar] as this is
         // a common case for array usage.
         StringTableEntry varNodeVarName;
         if (isSimpleVarLookup(arrayIndex, varNodeVarName))
         {
            codeStream.emit(OP_SETCURVAR_ARRAY_CREATE_VARLOOKUP);
            codeStream.emitSTE(varName);
            codeStream.emitSTE(varNodeVarName);
         }
         else
         {
            codeStream.emit(OP_LOADIMMED_IDENT);
            codeStream.emitSTE(varName);

            codeStream.emit(OP_ADVANCE_STR);
            ip = arrayIndex->compile(codeStream, ip, TypeReqString).ip;
            codeStream.emit(OP_REWIND_STR);
            codeStream.emit(OP_SETCURVAR_ARRAY_CREATE);
         }
      }
      codeStream.emit((subType == TypeReqFloat) ? OP_LOADVAR_FLT : OP_LOADVAR_UINT);
      codeStream.emit(operand);
      codeStream.emit((subType == TypeReqFloat) ? OP_SAVEVAR_FLT : OP_SAVEVAR_UINT);
   }
   if (subType != type)
      codeStream.emit(conversionOp(subType, type));

   return { codeStream.tell(), nullptr };
}

TypeReq AssignOpExprNode::getPreferredType()
{
   getAssignOpTypeOp(op, subType, operand);
   return subType;
}

//------------------------------------------------------------

U32 TTagSetStmtNode::compileStmt(CodeStream&, U32 ip)
{
   return ip;
}

//------------------------------------------------------------

CompileRet TTagDerefNode::compile(CodeStream&, U32 ip, TypeReq)
{
   return { ip, nullptr };
}

TypeReq TTagDerefNode::getPreferredType()
{
   return TypeReqNone;
}

//------------------------------------------------------------

CompileRet TTagExprNode::compile(CodeStream&, U32 ip, TypeReq)
{
   return { ip, nullptr };
}

TypeReq TTagExprNode::getPreferredType()
{
   return TypeReqNone;
}

//------------------------------------------------------------

CompileRet FuncCallExprNode::compile(CodeStream &codeStream, U32 ip, TypeReq type)
{
   // OP_PUSH_FRAME
   // arg OP_PUSH arg OP_PUSH arg OP_PUSH
   // eval all the args, then call the function.

   // OP_CALLFUNC
   // function
   // namespace
   // isDot

   precompileIdent(funcName);
   precompileIdent(nameSpace);

   codeStream.emit(OP_PUSH_FRAME);

   bool isThisCall = false;

   ExprNode *walk = args;

   // Try to optimize the this pointer call if it is a variable
   // that we are loading.
   if (callType == MethodCall)
   {
      // We cannot optimize array indices because it can have quite
      // a bit of code to figure out the array index.
      VarNode *var = dynamic_cast<VarNode*>(args);
      if (var && !var->arrayIndex)
      {
         precompileIdent(var->varName);

         // Are we a %this call?
         isThisCall = (var->varName == StringTable->insert("%this"));

         codeStream.emit(OP_PUSH_THIS);
         codeStream.emitSTE(var->varName);

         // inc args since we took care of first arg.
         walk = (ExprNode*)walk ->getNext();
      }
   }

   for (; walk; walk = (ExprNode *)walk->getNext())
   {
      TypeReq walkType = walk->getPreferredType();
      if (walkType == TypeReqNone) walkType = TypeReqString;
      ip = walk->compile(codeStream, ip, walkType).ip;
      switch (walk->getPreferredType())
      {
         case TypeReqFloat:
            codeStream.emit(OP_PUSH_FLT);
            break;
         case TypeReqUInt:
            codeStream.emit(OP_PUSH_UINT);
            break;
         default:
            codeStream.emit(OP_PUSH);
            break;
      }
   }

   if (isThisCall)
   {
      codeStream.emit(OP_CALLFUNC_THIS);
      codeStream.emitSTE(funcName);
   }
   else
   {
      if (callType == MethodCall || callType == ParentCall)
         codeStream.emit(OP_CALLFUNC);
      else
         codeStream.emit(OP_CALLFUNC_RESOLVE);

      codeStream.emitSTE(funcName);
      codeStream.emitSTE(nameSpace);
      codeStream.emit(callType);
   }

   if (type != TypeReqString)
      codeStream.emit(conversionOp(TypeReqString, type));

   return { codeStream.tell(), nullptr };
}

TypeReq FuncCallExprNode::getPreferredType()
{
   return TypeReqString;
}

//------------------------------------------------------------

CompileRet FuncPointerCallExprNode::compile(CodeStream &codeStream, U32 ip, TypeReq type)
{
   // OP_PUSH_FRAME
   // arg OP_PUSH arg OP_PUSH arg OP_PUSH
   // eval all the args, then call the function.

   // eval fn pointer
   // OP_CALLFUNC_POINTER

   codeStream.emit(OP_PUSH_FRAME);
   for (ExprNode *walk = args; walk; walk = (ExprNode *)walk->getNext())
   {
      TypeReq walkType = walk->getPreferredType();
      if (walkType == TypeReqNone) walkType = TypeReqString;
      ip = walk->compile(codeStream, ip, walkType).ip;
      switch (walk->getPreferredType())
      {
         case TypeReqFloat:
            codeStream.emit(OP_PUSH_FLT);
            break;
         case TypeReqUInt:
            codeStream.emit(OP_PUSH_UINT);
            break;
         default:
            codeStream.emit(OP_PUSH);
            break;
      }
   }

   ip = funcPointer->compile(codeStream, ip, TypeReqString).ip;
   codeStream.emit(OP_CALLFUNC_POINTER);

   if (type != TypeReqString)
      codeStream.emit(conversionOp(TypeReqString, type));

   return { codeStream.tell(), nullptr };
}

TypeReq FuncPointerCallExprNode::getPreferredType()
{
   return TypeReqString;
}

//------------------------------------------------------------

CompileRet AssertCallExprNode::compile(CodeStream &codeStream, U32 ip, TypeReq type)
{
#ifdef TORQUE_ENABLE_SCRIPTASSERTS

   messageIndex = getCurrentStringTable()->add(message, true, false);

   ip = testExpr->compile(codeStream, ip, TypeReqUInt).ip;
   codeStream.emit(OP_ASSERT);
   codeStream.emit(messageIndex);

#endif

   return { codeStream.tell(), nullptr };
}

TypeReq AssertCallExprNode::getPreferredType()
{
   return TypeReqNone;
}

//------------------------------------------------------------

CompileRet SlotAccessNode::compile(CodeStream &codeStream, U32 ip, TypeReq type)
{
   if (type == TypeReqNone)
      return { ip, nullptr };

   precompileIdent(slotName);

   // check if object is %this. If we are, we can do additional optimizations.
   if (isThisVar(objectExpr))
   {
      optimizeThisPointer(codeStream, arrayExpr, ip, slotName);
   }
   else
   {
      if (arrayExpr)
      {
         // eval array
         // OP_ADVANCE_STR
         // evaluate object expression sub (OP_SETCURFIELD)
         // OP_TERMINATE_REWIND_STR
         // OP_SETCURFIELDARRAY
         // total add of 4 + array precomp

         ip = arrayExpr->compile(codeStream, ip, TypeReqString).ip;
         codeStream.emit(OP_ADVANCE_STR);
      }
      ip = objectExpr->compile(codeStream, ip, TypeReqString).ip;
      codeStream.emit(OP_SETCUROBJECT);

      codeStream.emit(OP_SETCURFIELD);

      codeStream.emitSTE(slotName);

      if (arrayExpr)
      {
         codeStream.emit(OP_TERMINATE_REWIND_STR);
         codeStream.emit(OP_SETCURFIELD_ARRAY);
      }
   }

   switch (type)
   {
      case TypeReqUInt:
         codeStream.emit(OP_LOADFIELD_UINT);
         break;
      case TypeReqFloat:
         codeStream.emit(OP_LOADFIELD_FLT);
         break;
      case TypeReqString:
         codeStream.emit(OP_LOADFIELD_STR);
         break;
      case TypeReqNone:
         break;
   }

   return { codeStream.tell(), nullptr };
}

TypeReq SlotAccessNode::getPreferredType()
{
   return TypeReqNone;
}

//-----------------------------------------------------------------------------

CompileRet InternalSlotAccessNode::compile(CodeStream &codeStream, U32 ip, TypeReq type)
{
   if (type == TypeReqNone)
      return { ip, nullptr };

   ip = objectExpr->compile(codeStream, ip, TypeReqString).ip;
   codeStream.emit(OP_SETCUROBJECT);

   ip = slotExpr->compile(codeStream, ip, TypeReqString).ip;
   codeStream.emit(OP_SETCUROBJECT_INTERNAL);
   codeStream.emit(recurse);

   if (type != TypeReqUInt)
      codeStream.emit(conversionOp(TypeReqUInt, type));

   return { codeStream.tell(), nullptr };
}

TypeReq InternalSlotAccessNode::getPreferredType()
{
   return TypeReqUInt;
}

//-----------------------------------------------------------------------------

CompileRet SlotAssignNode::compile(CodeStream &codeStream, U32 ip, TypeReq type)
{
   // first eval the expression TypeReqString

   // if it's an array:

   // if OP_ADVANCE_STR 1
   // eval array

   // OP_ADVANCE_STR 1
   // evaluate object expr
   // OP_SETCUROBJECT 1
   // OP_SETCURFIELD 1
   // fieldName 1
   // OP_TERMINATE_REWIND_STR 1

   // OP_SETCURFIELDARRAY 1
   // OP_TERMINATE_REWIND_STR 1

   // else
   // OP_ADVANCE_STR
   // evaluate object expr
   // OP_SETCUROBJECT
   // OP_SETCURFIELD
   // fieldName
   // OP_TERMINATE_REWIND_STR

   // OP_SAVEFIELD
   // convert to return type if necessary.

   precompileIdent(slotName);

   ip = valueExpr->compile(codeStream, ip, TypeReqString).ip;

   if (isThisVar(objectExpr))
   {
      optimizeThisPointer(codeStream, arrayExpr, ip, slotName);
   }
   else
   {
      codeStream.emit(OP_ADVANCE_STR);
      if (arrayExpr)
      {
         ip = arrayExpr->compile(codeStream, ip, TypeReqString).ip;
         codeStream.emit(OP_ADVANCE_STR);
      }
      if (objectExpr)
      {
         ip = objectExpr->compile(codeStream, ip, TypeReqString).ip;
         codeStream.emit(OP_SETCUROBJECT);
      }
      else
         codeStream.emit(OP_SETCUROBJECT_NEW);
      codeStream.emit(OP_SETCURFIELD);
      codeStream.emitSTE(slotName);

      if (arrayExpr)
      {
         codeStream.emit(OP_TERMINATE_REWIND_STR);
         codeStream.emit(OP_SETCURFIELD_ARRAY);
      }

      codeStream.emit(OP_TERMINATE_REWIND_STR);
   }

   codeStream.emit(OP_SAVEFIELD_STR);

   if (typeID != -1)
   {
      codeStream.emit(OP_SETCURFIELD_TYPE);
      codeStream.emit(typeID);
   }

   if (type != TypeReqString)
      codeStream.emit(conversionOp(TypeReqString, type));

   return { codeStream.tell(), nullptr };
}

TypeReq SlotAssignNode::getPreferredType()
{
   return TypeReqString;
}

//------------------------------------------------------------

CompileRet SlotAssignOpNode::compile(CodeStream &codeStream, U32 ip, TypeReq type)
{
   // first eval the expression as its type

   // if it's an array:
   // eval array
   // OP_ADVANCE_STR
   // evaluate object expr
   // OP_SETCUROBJECT
   // OP_SETCURFIELD
   // fieldName
   // OP_TERMINATE_REWIND_STR
   // OP_SETCURFIELDARRAY

   // else
   // evaluate object expr
   // OP_SETCUROBJECT
   // OP_SETCURFIELD
   // fieldName

   // OP_LOADFIELD of appropriate type
   // operand
   // OP_SAVEFIELD of appropriate type
   // convert to return type if necessary.

   getAssignOpTypeOp(op, subType, operand);
   precompileIdent(slotName);

   ip = valueExpr->compile(codeStream, ip, subType).ip;

   if (isThisVar(objectExpr))
   {
      optimizeThisPointer(codeStream, arrayExpr, ip, slotName);
   }
   else
   {
      if (arrayExpr)
      {
         ip = arrayExpr->compile(codeStream, ip, TypeReqString).ip;
         codeStream.emit(OP_ADVANCE_STR);
      }
      ip = objectExpr->compile(codeStream, ip, TypeReqString).ip;
      codeStream.emit(OP_SETCUROBJECT);
      codeStream.emit(OP_SETCURFIELD);
      codeStream.emitSTE(slotName);

      if (arrayExpr)
      {
         codeStream.emit(OP_TERMINATE_REWIND_STR);
         codeStream.emit(OP_SETCURFIELD_ARRAY);
      }
   }
   codeStream.emit((subType == TypeReqFloat) ? OP_LOADFIELD_FLT : OP_LOADFIELD_UINT);
   codeStream.emit(operand);
   codeStream.emit((subType == TypeReqFloat) ? OP_SAVEFIELD_FLT : OP_SAVEFIELD_UINT);
   if (subType != type)
      codeStream.emit(conversionOp(subType, type));

   return { codeStream.tell(), nullptr };
}

TypeReq SlotAssignOpNode::getPreferredType()
{
   getAssignOpTypeOp(op, subType, operand);
   return subType;
}

//------------------------------------------------------------

CompileRet ObjectDeclNode::compileSubObject(CodeStream &codeStream, U32 ip, bool root)
{
   // goes

   // OP_PUSHFRAME 1
   // name expr
   // OP_PUSH 1
   // args... PUSH
   // OP_CREATE_OBJECT 1
   // parentObject 1
   // isDatablock 1
   // internalName 1
   // isSingleton 1
   // lineNumber 1
   // fail point 1

   // for each field, eval
   // OP_ADD_OBJECT (to UINT[0]) 1
   // root? 1

   // add all the sub objects.
   // OP_END_OBJECT 1
   // root? 1
   // To fix the stack issue [7/9/2007 Black]
   // OP_FINISH_OBJECT <-- fail point jumps to this opcode

   codeStream.emit(OP_PUSH_FRAME);

   ip = classNameExpr->compile(codeStream, ip, TypeReqString).ip;
   codeStream.emit(OP_PUSH);

   ip = objectNameExpr->compile(codeStream, ip, TypeReqString).ip;
   codeStream.emit(OP_PUSH);
   for (ExprNode *exprWalk = argList; exprWalk; exprWalk = (ExprNode *)exprWalk->getNext())
   {
      TypeReq walkType = exprWalk->getPreferredType();
      if (walkType == TypeReqNone) walkType = TypeReqString;
      ip = exprWalk->compile(codeStream, ip, walkType).ip;
      switch (exprWalk->getPreferredType())
      {
         case TypeReqFloat:
            codeStream.emit(OP_PUSH_FLT);
            break;
         case TypeReqUInt:
            codeStream.emit(OP_PUSH_UINT);
            break;
         default:
            codeStream.emit(OP_PUSH);
            break;
      }
   }
   codeStream.emit(OP_CREATE_OBJECT);
   codeStream.emitSTE(parentObject);

   codeStream.emit(isDatablock);
   codeStream.emit(isClassNameInternal);
   codeStream.emit(isSingleton);
   codeStream.emit(dbgLineNumber);
   const U32 failIp = codeStream.emit(0);
   for (SlotAssignNode *slotWalk = slotDecls; slotWalk; slotWalk = (SlotAssignNode *)slotWalk->getNext())
      ip = slotWalk->compile(codeStream, ip, TypeReqNone).ip;
   codeStream.emit(OP_ADD_OBJECT);
   codeStream.emit(root);
   for (ObjectDeclNode *objectWalk = subObjects; objectWalk; objectWalk = (ObjectDeclNode *)objectWalk->getNext())
      ip = objectWalk->compileSubObject(codeStream, ip, false).ip;
   codeStream.emit(OP_END_OBJECT);
   codeStream.emit(root || isDatablock);
   // Added to fix the object creation issue [7/9/2007 Black]
   failOffset = codeStream.emit(OP_FINISH_OBJECT);

   codeStream.patch(failIp, failOffset);

   return { codeStream.tell(), nullptr };
}

CompileRet ObjectDeclNode::compile(CodeStream &codeStream, U32 ip, TypeReq type)
{
   // root object decl does:

   // push 0 onto the UINT stack OP_LOADIMMED_UINT
   // precompiles the subObject(true)
   // UINT stack now has object id
   // type conv to type

   codeStream.emit(OP_LOADIMMED_UINT);
   codeStream.emit(0);
   ip = compileSubObject(codeStream, ip, true).ip;
   if (type != TypeReqUInt)
      codeStream.emit(conversionOp(TypeReqUInt, type));

   return { codeStream.tell(), nullptr };
}

TypeReq ObjectDeclNode::getPreferredType()
{
   return TypeReqUInt;
}

//------------------------------------------------------------

U32 FunctionDeclStmtNode::compileStmt(CodeStream &codeStream, U32 ip)
{
   // OP_FUNC_DECL
   // func name
   // namespace
   // package
   // hasBody?
   // func end ip
   // argc
   // ident array[argc]
   // code
   // OP_RETURN_VOID
   setCurrentStringTable(&getFunctionStringTable());
   setCurrentFloatTable(&getFunctionFloatTable());
   setCurrentTypeTable(&getFunctionTypeTable());

   argc = 0;
   for (ParamNode *walk = args; walk; walk = (ParamNode *)((StmtNode*)walk)->getNext())
   {
      precompileIdent(walk->varName);
      argc++;
   }

   CodeBlock::smInFunction = true;

   precompileIdent(fnName);
   precompileIdent(nameSpace);
   precompileIdent(package);

   CodeBlock::smInFunction = false;

   codeStream.emit(OP_FUNC_DECL);
   codeStream.emitSTE(fnName);
   codeStream.emitSTE(nameSpace);
   codeStream.emitSTE(package);

   codeStream.emit(U32(bool(stmts != NULL) ? 1 : 0) + U32(dbgLineNumber << 1));
   const U32 endIp = codeStream.emit(0);
   codeStream.emit(argc);
   for (ParamNode *walk = args; walk; walk = (ParamNode *)((StmtNode*)walk)->getNext())
   {
      codeStream.emitSTE(walk->varName);
      codeStream.emitSTE(walk->typeName);

      getCurrentTypeTable()->add(walk->varName, walk->typeName);
   }
   CodeBlock::smInFunction = true;
   ip = compileBlock(stmts, codeStream, ip);

   // Add break so breakpoint can be set at closing brace or
   // in empty function.
   addBreakLine(codeStream);

   CodeBlock::smInFunction = false;
   codeStream.emit(OP_RETURN_VOID);

   codeStream.patch(endIp, codeStream.tell());

   setCurrentStringTable(&getGlobalStringTable());
   setCurrentFloatTable(&getGlobalFloatTable());
   setCurrentTypeTable(&getGlobalTypeTable());

   return ip;
}
