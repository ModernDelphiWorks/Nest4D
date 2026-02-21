program simple_test;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Rtti;

begin
  try
    Writeln('Teste de compilação básico executado com sucesso!');
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.