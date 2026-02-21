program test_compilation;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Rtti,
  Nest4D.Async in 'Source\Core\Infrastructure\Nest4D.Async.pas',
  Nest4D.Request.Data in 'Source\Core\Request\Nest4D.Request.Data.pas',
  Nest4D.Request in 'Source\Core\Request\Nest4D.Request.pas',
  Nest4D.Validation.Types in 'Source\Pipes\Core\Nest4D.Validation.Types.pas',
  Nest4D.Transform.Pipe in 'Source\Pipes\Core\Nest4D.Transform.Pipe.pas',
  Nest4D.Parse.ArrayOf.Pipe in 'Source\Pipes\Transforms\Nest4D.Parse.ArrayOf.Pipe.pas',
  Nest4D.Transform.Interfaces in 'Source\Interfaces\Nest4D.Transform.Interfaces.pas',
  Decorator.IsBase in 'Source\Pipes\Decorators\decorator.isbase.pas',
  Decorator.isstring in 'Source\Pipes\Decorators\decorator.isstring.pas',
  Validation.IsString in 'Source\Pipes\Validators\TypesChecker\Validation.IsString.pas',
  Evolution4D.ResultPair in 'Evolution4D.ResultPair.pas',
  Nest4D.Validator.Constraint in 'Source\Pipes\Core\Nest4D.Validator.Constraint.pas',
  Nest4D.Decorator.Body in 'Source\Pipes\Decorators\PayLoads\Nest4D.Decorator.Body.pas',
  Nest4D.Decorator.Param in 'Source\Pipes\Decorators\PayLoads\Nest4D.Decorator.Param.pas',
  Nest4D.Decorator.Query in 'Source\Pipes\Decorators\PayLoads\Nest4D.Decorator.Query.pas',
  Decorator.Include in 'Source\Pipes\Decorators\Decorator.Include.pas',
  Validation.Pipe in 'Source\Pipes\Validators\Validation.Pipe.pas';

begin
  try
    Writeln('Compilation test successful!');
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.