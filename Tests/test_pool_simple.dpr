program test_pool_simple;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Classes,
  Nest4D.Pool.Config in '..\Source\Core\Infrastructure\Nest4D.Pool.Config.pas';

type
  // Classe de teste simples para simular um módulo
  TTestModule = class
  private
    FName: String;
  public
    constructor Create(const AName: String);
    property Name: String read FName;
  end;

constructor TTestModule.Create(const AName: String);
begin
  inherited Create;
  FName := AName;
  WriteLn('TTestModule criado: ', FName);
end;

procedure TestPoolBasicFunctionality;
var
  LPool: TPoolConfig;
  LModule1, LModule2, LModule3: TTestModule;
begin
  WriteLn('=== Teste Básico do Pool de Módulos ===');
  
  LPool := TPoolConfig.Create;
  try
    // Configurar o pool
    LPool.SetMinSize(1)
           .SetMaxSize(3)
           .SetEnabled(True)
           .SetIdleTimeout(5000);
    
    WriteLn('Pool configurado: MinSize=1, MaxSize=3, Enabled=True');
    
    // Teste 1: Adicionar módulos ao pool
    WriteLn('\n--- Teste 1: Adicionando módulos ao pool ---');
    LModule1 := TTestModule.Create('Module1');
    LModule2 := TTestModule.Create('Module2');
    LModule3 := TTestModule.Create('Module3');
    
    LPool.AddModule('TTestModule', LModule1);
    LPool.AddModule('TTestModule', LModule2);
    LPool.AddModule('TTestModule', LModule3);
    
    WriteLn('Tamanho do pool: ', LPool.GetPoolSize('TTestModule'));
    WriteLn('Módulos ativos: ', LPool.GetActiveModules('TTestModule'));
    
    // Teste 2: Obter módulos do pool
    WriteLn('\n--- Teste 2: Obtendo módulos do pool ---');
    var LRetrievedModule1 := LPool.GetModule('TTestModule');
    var LRetrievedModule2 := LPool.GetModule('TTestModule');
    
    if Assigned(LRetrievedModule1) then
      WriteLn('Módulo 1 obtido do pool: ', TTestModule(LRetrievedModule1).Name)
    else
      WriteLn('Falha ao obter módulo 1 do pool');
      
    if Assigned(LRetrievedModule2) then
      WriteLn('Módulo 2 obtido do pool: ', TTestModule(LRetrievedModule2).Name)
    else
      WriteLn('Falha ao obter módulo 2 do pool');
    
    WriteLn('Módulos ativos após obtenção: ', LPool.GetActiveModules('TTestModule'));
    
    // Teste 3: Retornar módulos ao pool
    WriteLn('\n--- Teste 3: Retornando módulos ao pool ---');
    if Assigned(LRetrievedModule1) then
    begin
      LPool.ReturnModule('TTestModule', LRetrievedModule1);
      WriteLn('Módulo 1 retornado ao pool');
    end;
    
    if Assigned(LRetrievedModule2) then
    begin
      LPool.ReturnModule('TTestModule', LRetrievedModule2);
      WriteLn('Módulo 2 retornado ao pool');
    end;
    
    WriteLn('Módulos ativos após retorno: ', LPool.GetActiveModules('TTestModule'));
    
    // Teste 4: Limpar pool
    WriteLn('\n--- Teste 4: Limpando pool ---');
    LPool.ClearPool('TTestModule');
    WriteLn('Pool limpo. Tamanho: ', LPool.GetPoolSize('TTestModule'));
    
    WriteLn('\n=== Teste concluído com sucesso! ===');
    
  finally
    LPool.Free;
  end;
end;

begin
  try
    TestPoolBasicFunctionality;
  except
    on E: Exception do
    begin
      WriteLn('ERRO: ', E.Message);
      WriteLn('Classe: ', E.ClassName);
    end;
  end;
  
  WriteLn('\nPressione ENTER para sair...');
  ReadLn;
end.