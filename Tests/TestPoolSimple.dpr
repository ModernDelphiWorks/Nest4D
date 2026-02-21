program TestPoolSimple;

{$APPTYPE CONSOLE}

// {$R *.res} - Arquivo de recursos removido para simplificar o teste

uses
  System.SysUtils,
  Test.Nest4D.Pool.Simple in 'Test.Nest4D.Pool.Simple.pas';

var
  LPoolTest: TSimplePoolTest;

begin
  try
    Writeln('=== TESTE SIMPLES DO POOL DE MÓDULOS NEST4D ===');
    Writeln('');
    
    // Criar e executar o teste
    LPoolTest := TSimplePoolTest.Create;
    try
      LPoolTest.RunAllTests;
    finally
      LPoolTest.Free;
    end;
    
    Writeln('');
    Writeln('Pressione ENTER para sair...');
    Readln;
  except
    on E: Exception do
    begin
      Writeln('ERRO: ' + E.ClassName + ': ' + E.Message);
      Writeln('Pressione ENTER para sair...');
      Readln;
    end;
  end;
end.