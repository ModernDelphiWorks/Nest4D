unit Nest4D.Health.Async;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.Variants,
  System.SyncObjs;

type
  // Status de saúde do sistema
  THealthStatus = (hsHealthy, hsUnhealthy, hsDegraded, hsUnknown);
  
  // Informações de saúde
  THealthInfo = record
    Status: THealthStatus;
    Message: String;
    Details: TDictionary<String, Variant>;
    Timestamp: TDateTime;
    
    constructor Create(AStatus: THealthStatus; const AMessage: String = '');
    procedure AddDetail(const AKey: String; const AValue: Variant);
    function StatusToString: String;
  end;
  
  // Callback para verificações de saúde assíncronas
  TAsyncHealthCheckCallback = reference to procedure(const AHealthInfo: THealthInfo);
  
  // Interface para verificadores de saúde assíncronos
  INest4DAsyncHealthChecker = interface
    ['{42FCB1FC-7B88-4890-924C-025DA06D5738}']
    procedure CheckHealthAsync(const ACallback: TAsyncHealthCheckCallback);
    function GetName: String;
    function GetTimeout: Integer;
  end;

  // Interface principal para sistema de saúde assíncrono
  INest4DAsyncHealth = interface
    ['{646D6242-C13C-40A6-9928-DE25BA266528}']
    procedure AddHealthChecker(const AChecker: INest4DAsyncHealthChecker);
    procedure RemoveHealthChecker(const AName: String);
    procedure CheckAllAsync(const ACallback: TAsyncHealthCheckCallback);
    procedure CheckByNameAsync(const AName: String; const ACallback: TAsyncHealthCheckCallback);
    function GetOverallStatus: THealthStatus;
    function GetHealthReport: TDictionary<String, THealthInfo>;
  end;
  
  // Implementação básica do verificador de saúde
  TNest4DAsyncHealthCheckerBase = class(TInterfacedObject, INest4DAsyncHealthChecker)
  private
    FName: String;
    FTimeout: Integer;
    
  protected
    procedure DoCheckHealthAsync(const ACallback: TAsyncHealthCheckCallback); virtual; abstract;
    
  public
    constructor Create(const AName: String; ATimeout: Integer = 5000);
    
    procedure CheckHealthAsync(const ACallback: TAsyncHealthCheckCallback);
    function GetName: String;
    function GetTimeout: Integer;
  end;
  
  // Implementação do sistema de saúde assíncrono
  TNest4DAsyncHealth = class(TInterfacedObject, INest4DAsyncHealth)
  private
    FHealthCheckers: TDictionary<String, INest4DAsyncHealthChecker>;
    FHealthReports: TDictionary<String, THealthInfo>;
    FLock: TCriticalSection;
    
  public
    constructor Create;
    destructor Destroy; override;
    
    procedure AddHealthChecker(const AChecker: INest4DAsyncHealthChecker);
    procedure RemoveHealthChecker(const AName: String);
    procedure CheckAllAsync(const ACallback: TAsyncHealthCheckCallback);
    procedure CheckByNameAsync(const AName: String; const ACallback: TAsyncHealthCheckCallback);
    function GetOverallStatus: THealthStatus;
    function GetHealthReport: TDictionary<String, THealthInfo>;
  end;

// Função para obter instância global
function GetAsyncHealthManager: INest4DAsyncHealth;

implementation

var
  GAsyncHealthManager: INest4DAsyncHealth;
  GAsyncHealthLock: TCriticalSection;

function GetAsyncHealthManager: INest4DAsyncHealth;
begin
  if not Assigned(GAsyncHealthManager) then
  begin
    if not Assigned(GAsyncHealthLock) then
      GAsyncHealthLock := TCriticalSection.Create;
      
    GAsyncHealthLock.Enter;
    try
      if not Assigned(GAsyncHealthManager) then
        GAsyncHealthManager := TNest4DAsyncHealth.Create;
    finally
      GAsyncHealthLock.Leave;
    end;
  end;
  Result := GAsyncHealthManager;
end;

{ THealthInfo }

constructor THealthInfo.Create(AStatus: THealthStatus; const AMessage: String);
begin
  Status := AStatus;
  Message := AMessage;
  Details := TDictionary<String, Variant>.Create;
  Timestamp := Now;
end;

procedure THealthInfo.AddDetail(const AKey: String; const AValue: Variant);
begin
  if Assigned(Details) then
    Details.AddOrSetValue(AKey, AValue);
end;

function THealthInfo.StatusToString: String;
begin
  case Status of
    hsHealthy: Result := 'Healthy';
    hsUnhealthy: Result := 'Unhealthy';
    hsDegraded: Result := 'Degraded';
    hsUnknown: Result := 'Unknown';
  else
    Result := 'Unknown';
  end;
end;

{ TNest4DAsyncHealthCheckerBase }

constructor TNest4DAsyncHealthCheckerBase.Create(const AName: String; ATimeout: Integer);
begin
  inherited Create;
  FName := AName;
  FTimeout := ATimeout;
end;

procedure TNest4DAsyncHealthCheckerBase.CheckHealthAsync(const ACallback: TAsyncHealthCheckCallback);
begin
  try
    DoCheckHealthAsync(ACallback);
  except
    on E: Exception do
    begin
      if Assigned(ACallback) then
      begin
        var HealthInfo := THealthInfo.Create(hsUnhealthy, E.Message);
        HealthInfo.AddDetail('Exception', E.ClassName);
        ACallback(HealthInfo);
      end;
    end;
  end;
end;

function TNest4DAsyncHealthCheckerBase.GetName: String;
begin
  Result := FName;
end;

function TNest4DAsyncHealthCheckerBase.GetTimeout: Integer;
begin
  Result := FTimeout;
end;

{ TNest4DAsyncHealth }

constructor TNest4DAsyncHealth.Create;
begin
  inherited Create;
  FHealthCheckers := TDictionary<String, INest4DAsyncHealthChecker>.Create;
  FHealthReports := TDictionary<String, THealthInfo>.Create;
  FLock := TCriticalSection.Create;
end;

destructor TNest4DAsyncHealth.Destroy;
begin
  FLock.Free;
  FHealthReports.Free;
  FHealthCheckers.Free;
  inherited Destroy;
end;

procedure TNest4DAsyncHealth.AddHealthChecker(const AChecker: INest4DAsyncHealthChecker);
begin
  if not Assigned(AChecker) then
    Exit;
    
  FLock.Enter;
  try
    FHealthCheckers.AddOrSetValue(AChecker.GetName, AChecker);
  finally
    FLock.Leave;
  end;
end;

procedure TNest4DAsyncHealth.RemoveHealthChecker(const AName: String);
begin
  FLock.Enter;
  try
    FHealthCheckers.Remove(AName);
    FHealthReports.Remove(AName);
  finally
    FLock.Leave;
  end;
end;

procedure TNest4DAsyncHealth.CheckAllAsync(const ACallback: TAsyncHealthCheckCallback);
var
  Checker: INest4DAsyncHealthChecker;
begin
  FLock.Enter;
  try
    for Checker in FHealthCheckers.Values do
    begin
      Checker.CheckHealthAsync(
        procedure(const AHealthInfo: THealthInfo)
        begin
          FLock.Enter;
          try
            FHealthReports.AddOrSetValue(Checker.GetName, AHealthInfo);
          finally
            FLock.Leave;
          end;
          
          if Assigned(ACallback) then
            ACallback(AHealthInfo);
        end);
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TNest4DAsyncHealth.CheckByNameAsync(const AName: String; const ACallback: TAsyncHealthCheckCallback);
var
  Checker: INest4DAsyncHealthChecker;
begin
  FLock.Enter;
  try
    if FHealthCheckers.TryGetValue(AName, Checker) then
    begin
      Checker.CheckHealthAsync(
        procedure(const AHealthInfo: THealthInfo)
        begin
          FLock.Enter;
          try
            FHealthReports.AddOrSetValue(AName, AHealthInfo);
          finally
            FLock.Leave;
          end;
          
          if Assigned(ACallback) then
            ACallback(AHealthInfo);
        end);
    end
    else if Assigned(ACallback) then
    begin
      var HealthInfo := THealthInfo.Create(hsUnknown, 'Health checker not found: ' + AName);
      ACallback(HealthInfo);
    end;
  finally
    FLock.Leave;
  end;
end;

function TNest4DAsyncHealth.GetOverallStatus: THealthStatus;
var
  HealthInfo: THealthInfo;
  HasUnhealthy: Boolean;
  HasDegraded: Boolean;
begin
  Result := hsHealthy;
  HasUnhealthy := False;
  HasDegraded := False;
  
  FLock.Enter;
  try
    for HealthInfo in FHealthReports.Values do
    begin
      case HealthInfo.Status of
        hsUnhealthy:
        begin
          HasUnhealthy := True;
          Break;
        end;
        hsDegraded:
          HasDegraded := True;
        hsUnknown:
          if not HasDegraded then
            Result := hsUnknown;
      end;
    end;
    
    if HasUnhealthy then
      Result := hsUnhealthy
    else if HasDegraded then
      Result := hsDegraded;
  finally
    FLock.Leave;
  end;
end;

function TNest4DAsyncHealth.GetHealthReport: TDictionary<String, THealthInfo>;
begin
  FLock.Enter;
  try
    Result := TDictionary<String, THealthInfo>.Create(FHealthReports);
  finally
    FLock.Leave;
  end;
end;

initialization

finalization
  if Assigned(GAsyncHealthLock) then
    GAsyncHealthLock.Free;

end.