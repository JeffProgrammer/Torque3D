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

#include "console/ast.h"

template< typename T >
struct Token
{
   T value;
   S32 lineNumber;
};
#include "console/cmdgram.h"

namespace Compiler
{
   String transpileBlock(StmtNode* block, S32 indentSpaces)
   {
      String str;
      for (StmtNode* walk = block; walk; walk = walk->getNext())
      {
         str += walk->tojs(indentSpaces) + "\n";
      }
      return str;
   }
}

String BreakStmtNode::tojs(S32 indentSpaces)
{
   return "";
}

String ContinueStmtNode::tojs(S32 indentSpaces)
{
   return "";
}

String ExprNode::tojs(S32 indentSpaces)
{
   return tojs(indentSpaces) + ";";
}

String ReturnStmtNode::tojs(S32 indentSpaces)
{
   return "return " + expr->tojs(0) + ";";
}

String IfStmtNode::tojs(S32 indentSpaces)
{
   const String& testExprStr = testExpr->tojs(indentSpaces);
   const String& ifStr = ifBlock->tojs(indentSpaces);

   String str = "if (" + testExprStr + ") {\n" + ifStr + "\n}";

   if (elseBlock)
   {
      const String& elseStr = elseBlock->tojs(indentSpaces);
      str += " else {\n" + elseStr + "\n}";
   }

   return str;
}

String LoopStmtNode::tojs(S32 indentSpaces)
{
   return "";
}

String IterStmtNode::tojs(S32 indentSpaces)
{
   return "";
}

String FloatBinaryExprNode::tojs(S32 indentSpaces)
{
   const String& rightExpr = right->tojs(indentSpaces);
   const String& leftExpr = left->tojs(indentSpaces);

   String operation = "";
   switch (op)
   {
   case '+':
      operation = "+";
      break;
   case '-':
      operation = "-";
      break;
   case '/':
      operation = "/";
      break;
   case '*':
      operation = "*";
      break;
   }
   return leftExpr + " " + operation + " " + rightExpr;
}

String ConditionalExprNode::tojs(S32 indentSpaces)
{
   const String& testExprStr = testExpr->tojs(indentSpaces);
   const String& trueExprStr = trueExpr->tojs(indentSpaces);
   const String& falseExprStr = falseExpr->tojs(indentSpaces);

   return "(" + testExprStr + ") ? " + trueExprStr + " : " + falseExprStr;
}

String IntBinaryExprNode::tojs(S32 indentSpaces)
{
   const String& rightExpr = right->tojs(indentSpaces);
   const String& leftExpr = left->tojs(indentSpaces);

   // Note: JS supports short circuit evaluation automatically, no need to generate complex code...

   String operation = "";
   switch (op)
   {
   case '^':
      operation = "^";
      break;
   case '%':
      operation = "%";
      break;
   case '&':
      operation = "&";
      break;
   case '|':
      operation = "|";
      break;
   case '<':
      operation = "<";
      break;
   case '>':
      operation = ">";
      break;
   case opGE:
      operation = ">=";
      break;
   case opLE:
      operation = "<=";
      break;
   case opEQ:
      operation = "==";
      break;
   case opNE:
      operation = "!=";
      break;
   case opOR:
      operation = "||";
      break;
   case opAND:
      operation = "&&";
      break;
   case opSHR:
      operation = ">>";
      break;
   case opSHL:
      operation = "<<";
      break;
   default:
      AssertISV(false, "Invalid operation");
   }

   return leftExpr + " " + operation + " " + rightExpr;
}

String StreqExprNode::tojs(S32 indentSpaces)
{
   const String& leftExpr = left->tojs(indentSpaces);
   const String& rightExpr = right->tojs(indentSpaces);

   String operation = !eq ? "!=" : "==";

   return leftExpr + " " + operation + " " + rightExpr;
}

String StrcatExprNode::tojs(S32 indentSpaces)
{
   return "";
}

String CommaCatExprNode::tojs(S32 indentSpaces)
{
   return "";
}

String IntUnaryExprNode::tojs(S32 indentSpaces)
{
   return "";
}

String FloatUnaryExprNode::tojs(S32 indentSpaces)
{
   return "";
}

String VarNode::tojs(S32 indentSpaces)
{
   const String& name = String::ToString(varName[0] == '$' ? varName : varName + 1);
   const String& arrayStr = arrayIndex ? arrayIndex->tojs(indentSpaces) : "";

   if (varName[0] == '$')
   {
      if (arrayIndex)
      {
         return "getTorqueGlobalArray(\'" + name + "\', " + arrayStr + ")";
      }

      return "getTorqueGlobal(\'" + name + "\')";
   }

   if (arrayIndex)
   {
      return name + "[" + arrayStr + "]";
   }

   return name;
}

String IntNode::tojs(S32 indentSpaces)
{
   return String::ToString(value);
}

String FloatNode::tojs(S32 indentSpaces)
{
   return String::ToString(value);
}

String StrConstNode::tojs(S32 indentSpaces)
{
   return "\'" + String::ToString(str) + "\'";
}

String ConstantNode::tojs(S32 indentSpaces)
{
   return String::ToString(value);
}

String AssignExprNode::tojs(S32 indentSpaces)
{
   const String& exprStr = expr->tojs(indentSpaces);
   const String& name = String::ToString(varName[0] == '$' ? varName : varName + 1);
   const String& arrayStr = arrayIndex ? arrayIndex->tojs(indentSpaces) : "";

   if (varName[0] == '$')
   {
      if (arrayIndex)
      {
         return "setTorqueGlobalArray(\'" + name + "\', " + arrayStr + ", " + exprStr + ");";
      }

      return "setTorqueGlobal(\'" + name  + "\', " + exprStr + ");";
   }

   if (arrayIndex)
   {
      return name + "[" + arrayStr + "] = " + exprStr + ";";
   }

   return "let " + name + " = " + exprStr + ";";
}

String AssignOpExprNode::tojs(S32 indentSpaces)
{
   return "";
}

String TTagSetStmtNode::tojs(S32 indentSpaces)
{
   return "";
}

String TTagDerefNode::tojs(S32 indentSpaces)
{
   return "";
}

String TTagExprNode::tojs(S32 indentSpaces)
{
   return "";
}

String FuncCallExprNode::tojs(S32 indentSpaces)
{
   String str = String::ToString(funcName) + "(";

   bool first = true;
   for (ExprNode* walk = args; walk; walk = static_cast<ExprNode*>(walk->getNext()))
   {
      if (!first)
      {
         str += ", ";
      }
      first = false;

      str += walk->tojs(indentSpaces);
   }

   str += ")";

   return str;
}

String AssertCallExprNode::tojs(S32 indentSpaces)
{
   return "";
}

String SlotAccessNode::tojs(S32 indentSpaces)
{
   return "";
}

String InternalSlotAccessNode::tojs(S32 indentSpaces)
{
   return "";
}

String SlotAssignNode::tojs(S32 indentSpaces)
{
   return "";
}

String SlotAssignOpNode::tojs(S32 indentSpaces)
{
   return "";
}

String ObjectDeclNode::tojs(S32 indentSpaces)
{
   return "";
}

String FunctionDeclStmtNode::tojs(S32 indentSpaces)
{
   String funcName = String::ToString(fnName);
   String str = "function " + funcName +"(";

   bool first = true;
   for (VarNode* walk = args; walk; walk = (VarNode*)((StmtNode*)walk)->getNext())
   {
      if (!first)
      {
         str += ", ";
      }
      first = false;

      str += String::ToString(walk->varName + 1);
   }

   str += ") {\n";

   str += Compiler::transpileBlock(stmts, indentSpaces);

   str += "}\n";

   // TODO: namespace and package functions...
   // (for packages, lets do monkey patching)

   return str;
}
