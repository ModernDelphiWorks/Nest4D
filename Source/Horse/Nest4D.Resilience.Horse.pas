unit Nest4D.Resilience.Horse;

interface

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  Horse,
  Horse.Callback,
  Horse.Request,
  Horse.Response,
  Nest4D.Resilience.Interfaces;

type
  // Interface para permitir injeção de dependência de resiliência
  IResilienceManager = interface
    ['{B8E5F2A1-9C3D-4E6F-8A7B-1D2E3F4A5B6C}']
    function ProtectCallback(const ACallback: THorseCallbackRequestResponse): THorseCallbackRequestResponse;
    function IsEnabled: Boolean;
    procedure SetEnabled(const AEnabled: Boolean);
  end;

  // Implementação concreta do gerenciador de resiliência
  THorseResilienceManager = class(TInterfacedObject, IResilienceManager)
  private
    FEnabled: Boolean;
    FLock: TCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;

    // IResilienceManager implementation
    function IsEnabled: Boolean;
    procedure SetEnabled(const AEnabled: Boolean);
    function ProtectCallback(const ACallback: THorseCallbackRequestResponse): THorseCallbackRequestResponse;
    
    // Método de classe para proteção direta de callbacks
    class function Protect(const ACallback: THorseCallbackRequestResponse): THorseCallbackRequestResponse;
  end;

  // Factory para criar instâncias de resiliência
  TResilienceFactory = class
  private
    class var FGlobalInstance: IResilienceManager;
    class var FLock: TCriticalSection;
  public
    class function CreateInstance(const AEnabled: Boolean = True): IResilienceManager;
    class function GetGlobalInstance: IResilienceManager;
    class procedure SetGlobalInstance(const AInstance: IResilienceManager);
    class procedure ReleaseGlobalInstance;
  end;

  // Classe para uso manual da resiliência
  TResilience = class
  private
    FResilienceManager: IResilienceManager;
  public
    constructor Create; overload;
    constructor Create(const AEnabled: Boolean); overload;
    constructor Create(AResilienceManager: IResilienceManager); overload;
    destructor Destroy; override;
    
    function Protect(const ACallback: THorseCallbackRequestResponse): THorseCallbackRequestResponse;
    function IsEnabled: Boolean;
    procedure SetEnabled(const AValue: Boolean);
  end;

implementation

uses
  System.Generics.Collections,
  Nest4D.Config.Manager;

{ THorseResilienceManager }

constructor THorseResilienceManager.Create;
begin
  inherited Create;
  FEnabled := True;
  FLock := TCriticalSection.Create;
end;

destructor THorseResilienceManager.Destroy;
begin
  FreeAndNil(FLock);
  inherited;
end;

function THorseResilienceManager.IsEnabled: Boolean;
begin
  FLock.Enter;
  try
    Result := FEnabled;
  finally
    FLock.Leave;
  end;
end;

procedure THorseResilienceManager.SetEnabled(const AEnabled: Boolean);
begin
  FLock.Enter;
  try
    FEnabled := AEnabled;
  finally
    FLock.Leave;
  end;
end;

function THorseResilienceManager.ProtectCallback(const ACallback: THorseCallbackRequestResponse): THorseCallbackRequestResponse;
begin
  // Se a resiliência estiver desabilitada, retorna o callback original
  if not IsEnabled then
  begin
    Result := ACallback;
    Exit;
  end;
  
  // Implementar a lógica de proteção do callback aqui
  // Por enquanto, retorna o callback original com proteção básica
  Result := procedure(Req: THorseRequest; Res: THorseResponse)
  begin
    try
      ACallback(Req, Res);
    except
      on E: Exception do
      begin
        // Log do erro ou tratamento específico pode ser adicionado aqui
        Res.Status(500).Send('Internal Server Error: ' + E.Message);
      end;
    end;
  end;
end;

class function THorseResilienceManager.Protect(const ACallback: THorseCallbackRequestResponse): THorseCallbackRequestResponse;
var
  LResilienceManager: IResilienceManager;
begin
  // Usa a instância global de resiliência através da factory
  LResilienceManager := TResilienceFactory.GetGlobalInstance;
  if Assigned(LResilienceManager) then
    Result := LResilienceManager.ProtectCallback(ACallback)
  else
    Result := ACallback; // Sem resiliência se não houver instância
end;

{ TResilienceFactory }

class function TResilienceFactory.CreateInstance(const AEnabled: Boolean): IResilienceManager;
begin
  Result := THorseResilienceManager.Create;
  Result.SetEnabled(AEnabled);
end;

{ TResilience }

constructor TResilience.Create;
begin
  inherited Create;
  FResilienceManager := THorseResilienceManager.Create;
end;

constructor TResilience.Create(const AEnabled: Boolean);
begin
  inherited Create;
  FResilienceManager := THorseResilienceManager.Create;
  FResilienceManager.SetEnabled(AEnabled);
end;

constructor TResilience.Create(AResilienceManager: IResilienceManager);
begin
  inherited Create;
  FResilienceManager := AResilienceManager;
end;

destructor TResilience.Destroy;
begin
  FResilienceManager := nil; // Interface será liberada automaticamente
  inherited;
end;

function TResilience.Protect(const ACallback: THorseCallbackRequestResponse): THorseCallbackRequestResponse;
begin
  if Assigned(FResilienceManager) then
    Result := FResilienceManager.ProtectCallback(ACallback)
  else
    Result := ACallback;
end;

function TResilience.IsEnabled: Boolean;
begin
  if Assigned(FResilienceManager) then
    Result := FResilienceManager.IsEnabled
  else
    Result := False;
end;

procedure TResilience.SetEnabled(const AValue: Boolean);
begin
  if Assigned(FResilienceManager) then
    FResilienceManager.SetEnabled(AValue);
end;

class function TResilienceFactory.GetGlobalInstance: IResilienceManager;
begin
  if not Assigned(FGlobalInstance) then
  begin
    if not Assigned(FLock) then
      FLock := TCriticalSection.Create;
      
    FLock.Enter;
    try
      if not Assigned(FGlobalInstance) then
        FGlobalInstance := CreateInstance(True); // Habilitado por padrão
    finally
      FLock.Leave;
    end;
  end;
  Result := FGlobalInstance;
end;

class procedure TResilienceFactory.SetGlobalInstance(const AInstance: IResilienceManager);
begin
  if not Assigned(FLock) then
    FLock := TCriticalSection.Create;
    
  FLock.Enter;
  try
    FGlobalInstance := AInstance;
  finally
    FLock.Leave;
  end;
end;

class procedure TResilienceFactory.ReleaseGlobalInstance;
begin
  if Assigned(FLock) then
  begin
    FLock.Enter;
    try
      FGlobalInstance := nil;
    finally
      FLock.Leave;
    end;
    
    FLock.Free;
    FLock := nil;
  end;
end;

initialization
  // Inicialização automática da instância global
  TResilienceFactory.GetGlobalInstance;

finalization
  TResilienceFactory.ReleaseGlobalInstance;

end.
