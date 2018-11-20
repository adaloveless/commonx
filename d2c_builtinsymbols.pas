unit d2c_builtinsymbols;

//What deserves to be "built-in" and how?
//Approaches
//1. I could potentially build a delphi unit for dealing with "built-ins"... but...
//


//Ultimately, I need to translate writeln(666) into cout <<< 666
//Challenges with this:
// - 666 is a constant.integer.  How do I dispatch the usage/evaluation of the parameters?
// - writeln is a function... maybe a delphi-to-c macro.
// - therefore maybe when I do is I:
//   1. Look up the symbol
//   2. See that the symbol is built in
//   3. Dispatch the generation of code to a function that uses "emit" or something to emit C++ code.
//   4. Should the emitted parameters be post or pre eval?
//      a. To examine this... lets look at some variations/examples
            //writeln('poop');
            //writeln('hello world '+inttostr(666))
            // from the above example, it appears that the parameters should be POST eval.
//      b. some other possible solutions
//          1.  writeln(666+777);     ---->   builtin::writeln(666+777);
//          2.  writeln('hello world'+inttostr(666)); ----> builtin::writeln(STRING_CONSTANT("hello world")+system::inttostr(666))

//next thing to do:
//  add "functionCall" state
//     WriteLn(666)
//     <functionCall>WriteLn<FunctionParameters>(<functionParameter><IntegerConstant>666</IntegerConstant>)</FunctionPArameters>;</FunctionCall>
//
//     - In Function Call state set current function (push onto stack)
//      - When ( is seen, push to Function Parameters State
//      - when ; is seen, pop functionCall from stack and pop compiiler state
//
//     - In FunctionParametersState
//      - when ) is seen, pop state
//      - Evaluate Symbols as-if code block, minus voids, and plus expressions
//      - when "," is seen, emit "," or pop/push?
//

//     -

//Core Code Eval
//  common eval with a few options:
//      flag: allow voids
//      flag: allow sub-expressions
//      flag: allow inline-constants
//      flag: allow expressions

//     - evaluate symbol, void? returns void?
//        - if not allow void, error
//     - on "{" push state comment
//     - on "{$" push state directive
//     - on "//" push state linecomment
//     - on "0-9", "-", enter InlineNumericConstant
//     - on "(" enter sub-expression when expecting non-void
//     - on "'" enter InlineStringConstant

// InlineNumericConstant
//   - on whitespace, emit numeric constant
//   [ ] if "." emit float constant
//   [ ] allow "-" to be not considered whitespace

// InlineStringConstant
//  [ ] on "'" terminate string
//  [ ] allow # after string without +
//  [ ] allow "'" after # without +
//  [ ] emit C Compatable string




interface


uses
  d2c_project;

procedure DefineBuiltinSymbols(prj: TD2CProject);


implementation

procedure DefineBuiltinSymbols(prj: TD2CProject);
var
  bis: TSymbol;
begin
  bis := TFunctionDefinition.create('WriteLn', prj, nil);


end;

end.
