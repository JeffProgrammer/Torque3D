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

namespace Compiler
{
   String transpileBlock(StmtNode* block, StringBuilder& code)
   {
      String str;
      for (StmtNode* walk = block; walk; walk = walk->getNext())
      {
         str += walk->tojs(code) + "\n";
      }
      return str;
   }
}

String BreakStmtNode::tojs(StringBuilder& code)
{
   return "";
}

String ContinueStmtNode::tojs(StringBuilder& code)
{
   return "";
}

String ExprNode::tojs(StringBuilder& code)
{
   return tojs(code) + ";";
}

String ReturnStmtNode::tojs(StringBuilder& code)
{
   return "return " + expr->tojs(code) + ";";
}

String IfStmtNode::tojs(StringBuilder& code)
{
   return "";
}

String LoopStmtNode::tojs(StringBuilder& code)
{
   return "";
}

String IterStmtNode::tojs(StringBuilder& code)
{
   return "";
}

String FloatBinaryExprNode::tojs(StringBuilder& code)
{
   const String& rightExpr = right->tojs(code);
   const String& leftExpr = left->tojs(code);

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

String ConditionalExprNode::tojs(StringBuilder& code)
{
   const String& testExprStr = testExpr->tojs(code);
   const String& trueExprStr = trueExpr->tojs(code);
   const String& falseExprStr = falseExpr->tojs(code);

   return "(" + testExprStr + ") ? " + trueExprStr + " : " + falseExprStr;
}

String IntBinaryExprNode::tojs(StringBuilder& code)
{
   return "";
}

String StreqExprNode::tojs(StringBuilder& code)
{
   return "";
}

String StrcatExprNode::tojs(StringBuilder& code)
{
   return "";
}

String CommaCatExprNode::tojs(StringBuilder& code)
{
   return "";
}

String IntUnaryExprNode::tojs(StringBuilder& code)
{
   return "";
}

String FloatUnaryExprNode::tojs(StringBuilder& code)
{
   return "";
}

String VarNode::tojs(StringBuilder& code)
{
   const String& name = String::ToString(varName[0] == '$' ? varName : varName + 1);
   const String& arrayStr = arrayIndex ? arrayIndex->tojs(code) : "";

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

String IntNode::tojs(StringBuilder& code)
{
   return String::ToString(value);
}

String FloatNode::tojs(StringBuilder& code)
{
   return String::ToString(value);
}

String StrConstNode::tojs(StringBuilder& code)
{
   return "\'" + String::ToString(str) + "\'";
}

String ConstantNode::tojs(StringBuilder& code)
{
   return String::ToString(value);
}

String AssignExprNode::tojs(StringBuilder& code)
{
   const String& exprStr = expr->tojs(code);
   const String& name = String::ToString(varName[0] == '$' ? varName : varName + 1);
   const String& arrayStr = arrayIndex ? arrayIndex->tojs(code) : "";

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

String AssignOpExprNode::tojs(StringBuilder& code)
{
   return "";
}

String TTagSetStmtNode::tojs(StringBuilder& code)
{
   return "";
}

String TTagDerefNode::tojs(StringBuilder& code)
{
   return "";
}

String TTagExprNode::tojs(StringBuilder& code)
{
   return "";
}

String FuncCallExprNode::tojs(StringBuilder& code)
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

      str += walk->tojs(code);
   }

   str += ")";

   return str;
}

String AssertCallExprNode::tojs(StringBuilder& code)
{
   return "";
}

String SlotAccessNode::tojs(StringBuilder& code)
{
   return "";
}

String InternalSlotAccessNode::tojs(StringBuilder& code)
{
   return "";
}

String SlotAssignNode::tojs(StringBuilder& code)
{
   return "";
}

String SlotAssignOpNode::tojs(StringBuilder& code)
{
   return "";
}

String ObjectDeclNode::tojs(StringBuilder& code)
{
   return "";
}

String FunctionDeclStmtNode::tojs(StringBuilder& code)
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

   str += Compiler::transpileBlock(stmts, code);

   str += "}\n";

   // TODO: namespace and package functions...
   // (for packages, lets do monkey patching)

   return str;
}
