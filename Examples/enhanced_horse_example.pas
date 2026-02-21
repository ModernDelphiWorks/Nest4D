program enhanced_horse_example;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  Horse,
  nest4d,
  Nest4D.module,
  Nest4D.controller,
  Nest4D.get,
  Nest4D.post,
  Nest4D.horse;

type
  [Controller('/api/users')]
  TUserController = class
  public
    [Get('/')]
    function GetUsers: String;
    
    [Get('/:id')]
    function GetUser(const id: String): String;
    
    [Post('/')]
    function CreateUser: String;
    
    [Get('/slow')]
    function SlowOperation: String;
    
    [Get('/error')]
    function ErrorOperation: String;
  end;

  [Module([
    TUserController
  ])]
  TAppModule = class
  end;

{ TUserController }

function TUserController.GetUsers: String;
begin
  Result := '{ "users": [{ "id": 1, "name": "John Doe" }, { "id": 2, "name": "Jane Smith" }] }';
end;

function TUserController.GetUser(const id: String): String;
begin
  Result := Format('{ "id": %s, "name": "User %s" }', [id, id]);
end;

function TUserController.CreateUser: String;
begin
  Result := '{ "id": 3, "name": "New User", "created": true }';
end;

function TUserController.SlowOperation: String;
begin
  // Simula operação lenta (2 segundos)
  Sleep(2000);
  Result := '{ "message": "Slow operation completed", "duration": "2000ms" }';
end;

function TUserController.ErrorOperation: String;
begin
  raise Exception.Create('Simulated error for testing');
end;

var
  LConfig: TNest4DHorseConfig;
begin
  try
    // Configura o sistema de monitoramento
    LConfig := TNest4DHorseConfig.Default;
    LConfig.EnableMetrics := True;
    LConfig.EnableLogging := True;
    LConfig.EnableHealthChecks := True;
    LConfig.EnableInterceptors := True;
    LConfig.LogLevel := llInfo;
    LConfig.MetricsEndpoint := '/metrics';
    LConfig.HealthEndpoint := '/health';
    LConfig.SlowRequestThresholdMs := 1000; // 1 segundo
    
    // Configura o Horse com o middleware Nest4D aprimorado
    THorse.Use(Nest4D_Horse(TAppModule, LConfig));
    
    // Os endpoints de monitoramento são configurados automaticamente pelo middleware
    
    // Adiciona rota de exemplo simples
    THorse.Get('/ping',
      procedure(Req: THorseRequest; Res: THorseResponse; Next: TNextProc)
      begin
        Res.Send('{ "message": "pong", "timestamp": "' + 
                 FormatDateTime('yyyy-mm-dd"T"hh:nn:ss.zzz"Z"', Now) + '" }');
      end);
    
    // Inicia o servidor
    THorse.Listen(9000,
      procedure(Horse: THorse)
      begin
        Writeln('Nest4D Enhanced Horse Server running on port 9000');
        Writeln('');
        Writeln('Available endpoints:');
        Writeln('  GET  /ping              - Simple ping endpoint');
        Writeln('  GET  /api/users         - List all users');
        Writeln('  GET  /api/users/:id     - Get specific user');
        Writeln('  POST /api/users         - Create new user');
        Writeln('  GET  /api/users/slow    - Slow operation (2s)');
        Writeln('  GET  /api/users/error   - Error simulation');
        Writeln('');
        Writeln('Monitoring endpoints:');
        Writeln('  GET  /health            - Health check');
        Writeln('  GET  /metrics           - Application metrics');
        Writeln('');
        Writeln('Press ENTER to stop the server...');
      end);
      
  except
    on E: Exception do
    begin
      Writeln('Error starting server: ' + E.Message);
      Readln;
    end;
  end;
end.