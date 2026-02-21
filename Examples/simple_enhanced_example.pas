{
             Nest4D - Development Framework for Delphi

                   Copyright (c) 2023, Isaque Pinheiro
                          All rights reserved.

                    GNU Lesser General Public License
                      Versão 3, 29 de junho de 2007
}

{
  @abstract(Nest4D Framework - Simple Enhanced Monitoring Example)
  @created(01 Mai 2023)
  @author(Isaque Pinheiro <isaquesp@gmail.com>)
  @homepage(https://www.isaquepinheiro.com.br)
  @documentation(https://nest4d-en.docs-br.com)
}

program simple_enhanced_example;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.JSON,
  Horse,
  Horse.Jhonson,
  Horse.CORS,
  Nest4D.horse,
  Nest4D.exception;

// Endpoint simples que demonstra logging e métricas automáticas
procedure GetHello(ARequest: THorseRequest; AResponse: THorseResponse; ANext: TProc);
var
  LName: String;
  LResponse: TJSONObject;
begin
  // O sistema automaticamente:
  // - Cria logs da requisição
  // - Coleta métricas de performance
  // - Gera ID único para a requisição
  // - Trata exceções automaticamente
  
  LName := ARequest.Query['name'];
  if LName = '' then
    LName := 'World';
  
  // Log manual (opcional) - o sistema já faz log automático
  TNest4DEnhanced.LogInfo(Format('Greeting request for: %s', [LName]));
  
  // Métrica customizada (opcional)
  TNest4DEnhanced.IncrementCounter('greetings_total');
  
  LResponse := TJSONObject.Create;
  try
    LResponse.AddPair('message', Format('Hello, %s!', [LName]));
    LResponse.AddPair('timestamp', FormatDateTime('yyyy-mm-dd"T"hh:nn:ss"Z"', Now));
    
    AResponse.Status(200)
             .ContentType('application/json')
             .Send(LResponse.ToJSON);
  finally
    LResponse.Free;
  end;
end;

// Endpoint que demonstra tratamento de erro
procedure GetError(ARequest: THorseRequest; AResponse: THorseResponse; ANext: TProc);
begin
  // O sistema automaticamente:
  // - Captura a exceção
  // - Faz log do erro com contexto
  // - Coleta métricas de erro
  // - Formata resposta de erro padronizada
  
  raise EBadRequestException.Create('This is a test error');
end;

// Endpoint que simula operação lenta
procedure GetSlow(ARequest: THorseRequest; AResponse: THorseResponse; ANext: TProc);
begin
  // O sistema automaticamente:
  // - Mede o tempo de resposta
  // - Identifica requisições lentas
  // - Coleta métricas de performance
  
  Sleep(1500); // Simula operação lenta
  
  AResponse.Status(200)
           .ContentType('application/json')
           .Send('{"message":"Slow operation completed"}');
end;

var
  LApp: THorse;
  
begin
  try
    WriteLn('=== Nest4D Simple Enhanced Example ===');
    WriteLn('');
    
    LApp := THorse.Create;
    
    // Middlewares básicos
    LApp.Use(Jhonson());
    LApp.Use(CORS());
    
    // Configuração em uma linha - tudo automático!
    LApp.Use(Nest4D_Horse);
    
    // Suas rotas normais - o monitoramento é automático
    LApp.Get('/hello', GetHello);
    LApp.Get('/error', GetError);
    LApp.Get('/slow', GetSlow);
    
    WriteLn('Server starting on port 9000...');
    WriteLn('');
    WriteLn('Try these endpoints:');
    WriteLn('  GET  /hello?name=João     - Simple greeting');
    WriteLn('  GET  /error              - Test error handling');
    WriteLn('  GET  /slow               - Test slow request detection');
    WriteLn('');
    WriteLn('Monitoring endpoints (automatic):');
    WriteLn('  GET  /health             - Health check');
    WriteLn('  GET  /metrics            - Application metrics');
    WriteLn('');
    WriteLn('Features enabled automatically:');
    WriteLn('  ✓ Request/Response logging');
    WriteLn('  ✓ Performance metrics');
    WriteLn('  ✓ Error tracking');
    WriteLn('  ✓ Health checks');
    WriteLn('  ✓ Request tracing');
    WriteLn('');
    WriteLn('Logs are written to:');
    WriteLn('  - Console (colored)');
    WriteLn('  - logs\\Nest4D.log');
    WriteLn('  - logs\\Nest4D.json');
    WriteLn('');
    WriteLn('Press Ctrl+C to stop');
    WriteLn('');
    
    LApp.Listen(9000);
    
  except
    on E: Exception do
    begin
      WriteLn(Format('Error: %s', [E.Message]));
      WriteLn(Format('Failed to start server: %s', [E.Message]));
    end;
  end;
end.