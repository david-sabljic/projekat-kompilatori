#include <iostream>
#include <cctype>
#include <cstdio>
#include <cstdlib>
#include <map>
#include <memory>
#include <string>
#include <utility>
#include <fstream>
#include <vector>

#include "llvm/Target/TargetOptions.h"
#include "llvm/TargetParser/Host.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/ExecutionEngine/Orc/LLJIT.h"
#include "llvm/ExecutionEngine/Orc/ThreadSafeModule.h"
#include "llvm/ExecutionEngine/MCJIT.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Scalar/GVN.h"
#include "llvm/IR/Value.h"


std::ifstream infile;  // Globalna promenljiva za fajl

/// Zamena za getchar() koja čita iz fajla
static int getNextChar() {
    return infile.get();
}


///--------- TOKEN ---------
///-------------------------

enum Token{
    tok_eof = -1,

    tok_using = -2,
    tok_class = -3,
    tok_static = -4,
    tok_void = -5,
    tok_int = -7,
    tok_if = -8,
    tok_else = -9,

    tok_identifier = -10,
    tok_literal = -11,
    tok_operator = -12,
    tok_symb = -13,
    tok_dot = -14,
    tok_number = -15,
};

static std::string IdentifierStr;
static std::string LiteralVal;
static std::string OperatorVal;
static std::string SymbolVal;
static int NumVal;

///--------- LEXER ---------
///-------------------------

static int gettok(){

    static int LastChar = ' ';

    while (isspace(LastChar)){
        LastChar = getNextChar();
    }

    if (isalpha(LastChar)){
        IdentifierStr = LastChar;
        while (isalnum((LastChar = getNextChar()))){
            IdentifierStr += LastChar;
        }
        if (IdentifierStr == "using"){
            return tok_using;
        }
        else if (IdentifierStr == "class"){
            return tok_class;
        }
        else if (IdentifierStr == "static"){
            return tok_static;
        }
        else if (IdentifierStr == "void"){
            return tok_void;
        }
        else if (IdentifierStr == "int"){
            return tok_int;
        }
        else if (IdentifierStr == "if"){
            return tok_if;
        }
        else if (IdentifierStr == "else"){
            return tok_else;
        }
        else if(IdentifierStr == "EOF"){
            ///TEST
            return tok_eof;
        }
        return tok_identifier;
    }

    if (isdigit(LastChar)) {
        std::string NumStr;
        do {
            NumStr += LastChar;
            LastChar = getNextChar();
        } while (isdigit(LastChar));
        NumVal = std::stoi(NumStr);
        return tok_number;
    }

    if (LastChar == '=' || LastChar == '+' || LastChar == '-' || LastChar == '>' || LastChar == '<'){
        OperatorVal = LastChar;
        LastChar = getNextChar();
        return tok_operator;
    }

    if (LastChar == ';' || LastChar == '.' || LastChar == '(' || LastChar == ')' || LastChar == '{' || LastChar == '}' ){
            SymbolVal = LastChar;
            if (LastChar =='.'){
                LastChar = getNextChar();
                return tok_dot;
            }
            LastChar = getNextChar();
            return tok_symb;
    }

     if (LastChar == '"'){
        std::string LtVal=" ";
        do {
            LastChar = getNextChar();
            if(LastChar != '"'){
              LtVal += LastChar;
            }
        } while (LastChar != '"');
        LiteralVal = LtVal;
        LastChar = getNextChar();
        return tok_literal;
    }

    if (LastChar == EOF) {
        return tok_eof;
    }

    int ThisChar = LastChar;
    LastChar = getNextChar();
    return ThisChar;
};

///--------- Abstract Syntax Tree (aka Parse Tree) ---------
///---------------------------------------------------------

class ASTNode  {
public:
  virtual ~ASTNode() = default;
  virtual llvm::Value *codegen() = 0;
};

using ASTNodePtr = std::shared_ptr<ASTNode>;

class Program : public ASTNode {
public:
    std::vector<ASTNodePtr> usings;
    std::vector<ASTNodePtr> classes;

    void addUsing(const ASTNodePtr& usingExp){
        usings.push_back(usingExp);
    }

    void addClass(const ASTNodePtr& classDecl) {
        classes.push_back(classDecl);
    }

    llvm::Value *codegen() override;
};

class Usings : public ASTNode {
public:
    std::string name;
    Usings(const std::string &name) : name(name) {}
    llvm::Value *codegen() override;
};

class ClassDeclaration : public ASTNode {
public:
    std::string name;
    std::vector<ASTNodePtr> methods;

    ClassDeclaration(const std::string &name) : name(name) {}
    ClassDeclaration(const std::string &name, const std::vector<ASTNodePtr> &methods) : name(name), methods(methods) {}

    void addMethod(ASTNodePtr method) {
        methods.push_back(std::move(method));
    }

    llvm::Value *codegen() override;
};

class MethodDeclaration : public ASTNode {
public:
    std::string type;
    std::string name;
    std::vector<ASTNodePtr> body;

    MethodDeclaration(const std::string &name) : name(name) {}
    MethodDeclaration(const std::string &name,const std::string &type) : name(name), type(type) {}

    void addStatement(ASTNodePtr stmt) {
        body.push_back(std::move(stmt));
    }

    llvm::Value *codegen() override;
};

class IfStatement : public ASTNode {
public:
    ASTNodePtr condition;
    std::vector<ASTNodePtr> body;
    ASTNodePtr Else;

    IfStatement(ASTNodePtr condition): condition(std::move(condition)){}

    void addStatement(ASTNodePtr stmt) {
        body.push_back(std::move(stmt));
    }
    void setElse(ASTNodePtr stmt) {
        Else = stmt;
    }

   llvm::Value *codegen() override;

};

class ElseStatement : public ASTNode {
public:
    std::vector<ASTNodePtr> body;

    ElseStatement(): ASTNode() {}

    void addStatement(ASTNodePtr stmt) {
        body.push_back(std::move(stmt));
    }

    llvm::Value *codegen() override;

};

class VariableDeclaration : public ASTNode {
public:
    std::string name;
    std::string type;
    ASTNodePtr initializer;

    VariableDeclaration(const std::string& type, const std::string& name, const ASTNodePtr& initializer)
        : type(type), name(name), initializer(initializer) {}

    llvm::Value *codegen() override;
};

class NumberLiteral : public ASTNode {
public:
    int val;

    NumberLiteral(int val) : val(val) {}

    int getValue() const { return val; }

   llvm::Value *codegen() override;
};

class BinaryExpression : public ASTNode {
public:
    ASTNodePtr left;
    std::string op;
    ASTNodePtr right;

    BinaryExpression(const ASTNodePtr& left, const std::string& op, const ASTNodePtr& right)
        : left(left), op(op), right(right) {}

        llvm::Value *codegen() override;

};

class MethodCall : public ASTNode{
public:
    std::string className;
    std::string methodName;
    std::vector<ASTNodePtr> arguments;

    MethodCall(const std::string &className,const std::string &methodName) : className(className), methodName(methodName) {}
    MethodCall(const std::string &methodName) : methodName(methodName) {}
    MethodCall(const std::string &methodName, const std::vector<ASTNodePtr> &arguments) : methodName(methodName), arguments(arguments) {}

    void addArgument(ASTNodePtr arg) {
        arguments.push_back(std::move(arg));
    }

    llvm::Value *codegen() override;
};

class StringLiteral : public ASTNode {
public:
    std::string Value;

    StringLiteral(std::string value) : Value(value) {}

    std::string getValue() const { return Value; }

    llvm::Value *codegen() override;
};

class VariableReference : public ASTNode {
public:
    std::string name;

    VariableReference(const std::string& name) : name(name) {}
    llvm::Value *codegen() override;

};



///--------- PARSER ---------
///--------------------------
class Parser {
int CurTok;

int getNextToken() {
        return CurTok = gettok();
    };

public:

    ASTNodePtr Parse() {
        getNextToken();
        return ParseProgram();
    };

    std::shared_ptr<Program> ParseProgram(){
         std::cerr<<"PARSIRA PROGRAM...\n";

         auto program = std::make_shared<Program>();

         while (CurTok != tok_eof) {
            if(CurTok == tok_using){
                auto usingg = ParseUsing();
                if(usingg){
                    program->addUsing(usingg);
                }
            }
            else if (CurTok == tok_class){
                auto classDecl = ParseClass();
                if (classDecl) {
                    program->addClass(classDecl);
                }
            }
            else{
                std::cerr<< "Unexpected token\n";
                return nullptr;
            }
         }
         std::cerr<<"ZAVRSI...\n";
         return program;
    }

    std::shared_ptr<Usings> ParseUsing() {
        std::cerr<<"PARSIRA USING...\n";

        getNextToken(); // Eat 'class'
        std::string name = IdentifierStr;
        auto usingg = std::make_shared<Usings>(name);
        getNextToken(); // Eat using name
        getNextToken(); // Eat ';'

        return usingg;
    }

        std::shared_ptr<ClassDeclaration> ParseClass() {
        std::cerr<<"PARSIRA KLASU...\n";
        getNextToken(); // Eat 'class'
        std::string className = IdentifierStr;
        getNextToken(); // Eat class name
        getNextToken(); // Eat '{'
        auto classDecl = std::make_shared<ClassDeclaration>(className);

        while ( SymbolVal[0] != '}' ) {
            auto methodDecl = ParseMethod();
            if (methodDecl) {
                classDecl->addMethod(methodDecl);
            } else {
                return nullptr;
            }
        }

        getNextToken(); // Eat '}'

        return classDecl;
    }

    std::shared_ptr<MethodDeclaration> ParseMethod() {
        std::cerr << "PARSIRA METODU...\n";
        getNextToken(); // Eat static
        std::string type = IdentifierStr;
        getNextToken(); // Eat type
        std::string name = IdentifierStr;
        getNextToken(); // Eat name
        getNextToken(); // Eat '('
        getNextToken(); // Eat ')'
        auto method = std::make_shared<MethodDeclaration>(name,type);
        getNextToken();// Eat '{'

        while(SymbolVal[0]!='}'){
            auto temp = ParseExpr();
            if( temp ){
                method->addStatement(temp);
            }
        }
        getNextToken();// Eat '}'

        return method;
    }

    std::shared_ptr<ASTNode> ParseExpr(){

        if( CurTok == tok_int){
            auto temp = ParseDeclaration();

            return temp;
        }
        else if( CurTok == tok_identifier ){
            auto temp = ParseMethodCall();

            return temp;
        }
        else if( CurTok == tok_if){
            auto temp = ParseIf();

            return temp;
        }
        return nullptr;
    }

    std::shared_ptr<VariableDeclaration> ParseDeclaration() {
        std::cerr << "PARSIRA DEKLARACIJU...\n";
        std::string type = IdentifierStr;
        getNextToken(); // Eat int
        std::string name = IdentifierStr;
        getNextToken(); // Eat name
        getNextToken(); // Eat '='
        auto inis = ParseInis();
        getNextToken(); // Eat ';'
        auto temp = std::make_shared<VariableDeclaration>(type,name,inis);
        return temp;
    }

    std::shared_ptr<ASTNode> ParseInis() {
        std::cerr << "PARSIRA INICIJALIZACIJU...\n";
        if(CurTok == tok_number){
            int numb = NumVal;
            auto temp = std::make_shared<NumberLiteral>(numb);
            getNextToken();// Eat numb
            return temp;
        }
        else if(CurTok == tok_int){
            std::string class_name = IdentifierStr;
            getNextToken(); // Eat name
            getNextToken(); // Eat '.'
            std::string method_name = IdentifierStr;
            getNextToken(); // Eat name
            auto temp = std::make_shared<MethodCall>(class_name,method_name);
            getNextToken(); // Eat '('
            std::string class_name2 = IdentifierStr;
            getNextToken(); // Eat name
            getNextToken(); // Eat '.'
            std::string method_name2 = IdentifierStr;
            getNextToken(); // Eat name
            auto temp2 = std::make_shared<MethodCall>(class_name2,method_name2);
            getNextToken(); // Eat '('
            getNextToken(); // Eat ')'
            getNextToken(); // Eat ')'
            temp->addArgument(temp2);
            return temp;
        }
        return nullptr;
    }

    std::shared_ptr<MethodCall> ParseMethodCall() {
        std::cerr << "PARSIRA POZIV METODE...\n";
        std::string class_name = IdentifierStr;
        getNextToken(); // Eat name
        getNextToken(); // Eat '.'
        std::string method_name = IdentifierStr;
        getNextToken(); // Eat name
        auto temp = std::make_shared<MethodCall>(class_name,method_name);
        getNextToken(); // Eat '('
        if( CurTok == tok_literal){
            std::string txt = LiteralVal;
            auto arg = std::make_shared<StringLiteral>(txt);
            temp->addArgument(arg);
        }
        else{
            auto arg = ParseBinExp();
            temp->addArgument(arg);
        }
        getNextToken(); // Eat ')'
        getNextToken(); // Eat ';'
        return temp;
    }

    std::shared_ptr<BinaryExpression> ParseBinExp() {
        auto arg1 = ParseLeft();
        getNextToken(); // Eat name
        std::string op = SymbolVal;
        getNextToken(); // Eat op
        auto arg2 =  ParseLeft();
        getNextToken(); // Eat name
        auto temp = std::make_shared<BinaryExpression>(arg1,op,arg2);
        return temp;
    }

    std::shared_ptr<ASTNode> ParseLeft() {

        if(CurTok == tok_identifier){
            std::string name = IdentifierStr;
            auto left = std::make_shared<VariableReference>(name);
            getNextToken(); // Eat variable
            return left;
        }
        else{
            int number = NumVal;
            auto left = std::make_shared<NumberLiteral>(number);
            getNextToken(); // Eat number
            return left;
        }
    }

    std::shared_ptr<IfStatement>ParseIf () {
        getNextToken(); // Eat if
        getNextToken(); // Eat '('
        auto cond = ParseBinExp();
        getNextToken(); // Eat ')'
        getNextToken(); // Eat '{'
        auto temp = std::make_shared<IfStatement>(cond);
        while(SymbolVal[0]!='}'){
            auto x = ParseExpr();
            if(x){
                temp->addStatement(x);
            }
        }
        getNextToken();// Eat '}'
        if(CurTok == tok_else){
            auto el = std::make_shared<ElseStatement>();
            getNextToken(); // Eat else
            getNextToken(); // Eat '{'
            while(SymbolVal[0]!='}'){
                auto x = ParseExpr();
                if(x){
                    el->addStatement(x);
                }
            }
            getNextToken(); // Eat '}'
            temp->setElse(el);
        }
        else{
            temp->setElse(nullptr);
        }
        return temp;
    }



};


class LogErrorV {
public:
    static llvm::Value* log(const std::string &errorMessage) {
        std::cerr << "Error: " << errorMessage << std::endl;
        return nullptr;
    }

};


///--------- CODE GENERATOR ---------
///----------------------------------


static std::unique_ptr<llvm::LLVMContext> TheContext;
static std::unique_ptr<llvm::Module> TheModule;
static std::unique_ptr<llvm::IRBuilder<>> Builder;
static std::map<std::string, llvm::Value *> NamedValues;


llvm::Value *Program::codegen() {

  for (const auto& usingDecl : usings) {
    usingDecl->codegen();
  }

  for (const auto& classDecl : classes) {
    classDecl->codegen();
  }

  return nullptr;
}

llvm::Value *Usings::codegen() {
    ///skip
    return nullptr;
}

llvm::Value *ClassDeclaration::codegen() {
  for (const auto& method : methods) {
    method->codegen();
  }
  return nullptr;
}

llvm::Value *MethodDeclaration::codegen() {
    std::vector<llvm::Type *> Doubles;

    llvm::FunctionType *FT = llvm::FunctionType::get(llvm::Type::getVoidTy(*TheContext), Doubles, false);
    llvm::Function *F = llvm::Function::Create(FT, llvm::Function::ExternalLinkage, name, TheModule.get());
    llvm::BasicBlock *BB = llvm::BasicBlock::Create(*TheContext, "entry", F);
    Builder->SetInsertPoint(BB);

    for (auto &stmt : body) {
        stmt->codegen();
    }

    Builder->CreateRetVoid();
    return F;
}

llvm::Value *IfStatement::codegen() {
    llvm::Value *CondV = condition->codegen();
    if (!CondV) return nullptr;

    CondV = Builder->CreateICmpNE(CondV, llvm::ConstantInt::get(*TheContext, llvm::APInt(1, 0)), "ifcond");

    llvm::Function *TheFunction = Builder->GetInsertBlock()->getParent();

    llvm::BasicBlock *ThenBB = llvm::BasicBlock::Create(*TheContext, "then", TheFunction);
    llvm::BasicBlock *ElseBB = llvm::BasicBlock::Create(*TheContext, "else", TheFunction);
    llvm::BasicBlock *MergeBB = llvm::BasicBlock::Create(*TheContext, "ifcont", TheFunction);


    Builder->CreateCondBr(CondV, ThenBB, ElseBB);

    Builder->SetInsertPoint(ThenBB);
    for (auto &stmt : body) {
        stmt->codegen();
    }
    Builder->CreateBr(MergeBB);

    Builder->SetInsertPoint(ElseBB);
    if (Else) {
        Else->codegen();
    }
    Builder->CreateBr(MergeBB);

    Builder->SetInsertPoint(MergeBB);

    return nullptr;
}

llvm::Value *ElseStatement::codegen(){
    for (auto &stmt : body) {
    stmt->codegen();
  }
  return nullptr;
}

llvm::Value *VariableDeclaration::codegen() {
  llvm::Value *InitVal = initializer->codegen();
  if (!InitVal) return nullptr;

  llvm::AllocaInst *Alloca = Builder->CreateAlloca(llvm::Type::getDoubleTy(*TheContext), nullptr, name.c_str());
  Builder->CreateStore(InitVal, Alloca);

  NamedValues[name] = Alloca;
  return Alloca;
}

llvm::Value *NumberLiteral::codegen() {
     return llvm::ConstantInt::get(llvm::Type::getInt32Ty(*TheContext.get()), val);
}

llvm::Value *BinaryExpression::codegen() {
  llvm::Value *L = left->codegen();
  llvm::Value *R = right->codegen();
  if (!L || !R) return nullptr;

  if (op == "+")
    return Builder->CreateFAdd(L, R, "addtmp");
  else if (op == "-")
    return Builder->CreateFSub(L, R, "subtmp");
  else if (op == "*")
    return Builder->CreateFMul(L, R, "multmp");
  else if (op == "<") {
    L = Builder->CreateFCmpULT(L, R, "cmptmp");
    return Builder->CreateUIToFP(L, llvm::Type::getDoubleTy(*TheContext), "booltmp");
  }
  return nullptr;
}

llvm::Value *MethodCall::codegen(){
    llvm::Function *calleeFunction = TheModule->getFunction(methodName);
    if (!calleeFunction) {
        std::cerr << "Unknown function referenced: " << methodName << std::endl;
        return nullptr;
    }

    if (calleeFunction->arg_size() != arguments.size()) {
        std::cerr << "Incorrect number of arguments passed to " << methodName << std::endl;
        return nullptr;
    }

    std::vector<llvm::Value*> argsV;
    for (unsigned i = 0, e = arguments.size(); i != e; ++i) {
        llvm::Value *argValue = arguments[i]->codegen();
        if (!argValue) return nullptr;
        argsV.push_back(argValue);
    }

    return Builder->CreateCall(calleeFunction, argsV, "calltmp");
}

llvm::Value *StringLiteral::codegen() {
    llvm::Constant *strConstant = llvm::ConstantDataArray::getString(*TheContext, Value, true);

    llvm::GlobalVariable *globalStr = new llvm::GlobalVariable(*TheModule,strConstant->getType(),true,llvm::GlobalValue::PrivateLinkage,strConstant,".str");
    llvm::Value *zero = llvm::ConstantInt::get(llvm::Type::getInt32Ty(*TheContext), 0);
    llvm::Value *indices[] = {zero, zero};
    return llvm::ConstantExpr::getGetElementPtr(
        llvm::cast<llvm::ArrayType>(strConstant->getType())->getElementType(),
        globalStr,
        indices
    );
}


llvm::Value *VariableReference::codegen() {
    llvm::Value *V = NamedValues[name];
    if (!V) {
        return LogErrorV::log("Unknown variable name");
    }

    auto *pointerType = llvm::dyn_cast<llvm::PointerType>(V->getType());
    if (!pointerType) {
        return LogErrorV::log("Expected pointer type");
    }
    llvm::Type *Ty = pointerType->getArrayElementType();

    return Builder->CreateLoad(Ty, V, name.c_str());
}










int main()
{
     //inicijalizacija llvm konteksta, modula i ir buildera
    TheContext = std::make_unique<llvm::LLVMContext>();
    TheModule = std::make_unique<llvm::Module>("my cool jit", *TheContext);
    Builder = std::make_unique<llvm::IRBuilder<>>(*TheContext);

    std::ifstream infile("test.txt");
    if (!infile.is_open()) {
        std::cerr << "Could not open file\n";
        return 1;
    }

    // parser
    Parser* parser = new Parser();

    auto rez = parser->Parse();
    rez->codegen();


    TheModule->print(llvm::errs(), nullptr);


    ///TODO: Potencijalno prebaciti iznad
      llvm::InitializeAllTargetInfos();
      llvm::InitializeAllTargets();
      llvm::InitializeAllTargetMCs();
      llvm::InitializeAllAsmParsers();
      llvm::InitializeAllAsmPrinters();

      auto TargetTriple = LLVMGetDefaultTargetTriple();
      TheModule->setTargetTriple(TargetTriple);

      std::string Error;
      auto Target = llvm::TargetRegistry::lookupTarget(TargetTriple, Error);

      if (!Target) {
        llvm::errs() << Error;
        return 1;
      }

      auto CPU = "generic";
      auto Features = "";

      llvm::TargetOptions opt;
      auto TheTargetMachine = Target->createTargetMachine(
          TargetTriple, CPU, Features, opt, llvm::Reloc::PIC_);

      TheModule->setDataLayout(TheTargetMachine->createDataLayout());

      auto Filename = "output.o";
      std::error_code EC;
      llvm::raw_fd_ostream dest(Filename, EC, llvm::sys::fs::OF_None);

      if (EC) {
        llvm::errs() << "Could not open file: " << EC.message();
        return 1;
      }

      llvm::legacy::PassManager pass;
      auto FileType = llvm::CodeGenFileType::ObjectFile;

      if (TheTargetMachine->addPassesToEmitFile(pass, dest, nullptr, FileType)) {
        llvm::errs() << "TheTargetMachine can't emit a file of this type";
        return 1;
      }

      pass.run(*TheModule);
      dest.flush();

      TheModule->print(llvm::errs(),nullptr);

      llvm::outs() << "Wrote " << Filename << "\n";

      infile.close();
      delete parser;

    return 0;
}
