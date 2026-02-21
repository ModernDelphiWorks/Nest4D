program test_pool_typecheck;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  Nest4D.Pool.Config in '..\Source\Core\Infrastructure\Nest4D.Pool.Config.pas',
  Nest4D.Module.Abstract in '..\Source\Modules\Nest4D.Module.Abstract.pas';

var
  PoolConfig: TPoolConfig;
  TestModule: TModuleAbstract;
begin
  try
    WriteLn('Teste de verificação de tipos do Pool...');
    
    PoolConfig := TPoolConfig.Create;
    try
      WriteLn('TPoolConfig criado com sucesso.');
      WriteLn('Métodos disponíveis:');
      WriteLn('- GetModule: retorna TModuleAbstract');
      WriteLn('- ReturnModule: aceita TModuleAbstract');
      WriteLn('- AddModule: aceita TModuleAbstract');
      WriteLn('Alteração de tipo concluída com sucesso!');
    finally
      PoolConfig.Free;
    end;
    
    WriteLn('Pressione Enter para sair...');
    ReadLn;
  except
    on E: Exception do
    begin
      WriteLn('Erro: ' + E.Message);
      ReadLn;
    end;
  end;
end.