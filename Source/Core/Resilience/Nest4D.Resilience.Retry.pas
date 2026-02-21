unit Nest4D.Resilience.Retry;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Math,
  Nest4D.Resilience.Interfaces,
  Nest4D.Request.Data;

type
  // Implementa??o da pol?tica de retry
  TRetryPolicy = class(TInterfacedObject, IRetryPolicy)
  private
    FMaxAttempts: Integer;
    FRetryStrategy: TRetryStrategy;
    FBaseDelay: Cardinal;
    FMaxDelay: Cardinal;
    FRetryCallback: TRetryCallback;
    FJitterEnabled: Boolean;
    FJitterRange: Double; // 0.0 a 1.0

    function CalculateFixedDelay: Cardinal;
    function CalculateLinearDelay(const AAttemptNumber: Integer): Cardinal;
    function CalculateExponentialDelay(const AAttemptNumber: Integer): Cardinal;
    function ApplyJitter(const ADelay: Cardinal): Cardinal;
    function IsRetriableException(const AException: Exception): Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    // IRetryPolicy implementation
    procedure SetMaxAttempts(const AMaxAttempts: Integer);
    procedure SetRetryStrategy(const AStrategy: TRetryStrategy);
    procedure SetBaseDelay(const ADelayMs: Cardinal);
    procedure SetMaxDelay(const AMaxDelayMs: Cardinal);
    procedure SetRetryCallback(const ACallback: TRetryCallback);

    function ShouldRetry(const AAttempt: TRetryAttempt): Boolean;
    function CalculateDelay(const AAttemptNumber: Integer): Cardinal;
    function Execute(const AAdapter: IRestFrameworkAdapter; const ARequestData: TRequestData): Boolean;

    function GetMaxAttempts: Integer;
    function GetRetryStrategy: TRetryStrategy;
    function GetBaseDelay: Cardinal;
    function GetMaxDelay: Cardinal;

    // Configura??es adicionais
    procedure SetJitterEnabled(const AEnabled: Boolean);
    procedure SetJitterRange(const ARange: Double);
    function IsJitterEnabled: Boolean;
    function GetJitterRange: Double;

    property JitterEnabled: Boolean read IsJitterEnabled write SetJitterEnabled;
    property JitterRange: Double read GetJitterRange write SetJitterRange;
  end;

  // Factory para criar pol?ticas de retry pr?-configuradas
  TRetryPolicyFactory = class
  public
    class function CreateDefault: IRetryPolicy;
    class function CreateAggressive: IRetryPolicy;
    class function CreateConservative: IRetryPolicy;
    class function CreateCustom(const AMaxAttempts: Integer; const AStrategy: TRetryStrategy;
      const ABaseDelay, AMaxDelay: Cardinal): IRetryPolicy;
  end;

implementation

uses
  Nest4D.Logging;

{ TRetryPolicy }

constructor TRetryPolicy.Create;
begin
  inherited Create;
  FMaxAttempts := 3;
  FRetryStrategy := rsExponential;
  FBaseDelay := 1000; // 1 segundo
  FMaxDelay := 30000; // 30 segundos
  FRetryCallback := nil;
  FJitterEnabled := True;
  FJitterRange := 0.1; // 10% de jitter
end;

destructor TRetryPolicy.Destroy;
begin
  FRetryCallback := nil;
  inherited Destroy;
end;

procedure TRetryPolicy.SetMaxAttempts(const AMaxAttempts: Integer);
begin
  if AMaxAttempts > 0 then
    FMaxAttempts := AMaxAttempts
  else
    raise EArgumentException.Create('MaxAttempts deve ser maior que zero');
end;

procedure TRetryPolicy.SetRetryStrategy(const AStrategy: TRetryStrategy);
begin
  FRetryStrategy := AStrategy;
end;

procedure TRetryPolicy.SetBaseDelay(const ADelayMs: Cardinal);
begin
  FBaseDelay := ADelayMs;
end;

procedure TRetryPolicy.SetMaxDelay(const AMaxDelayMs: Cardinal);
begin
  FMaxDelay := AMaxDelayMs;
end;

procedure TRetryPolicy.SetRetryCallback(const ACallback: TRetryCallback);
begin
  FRetryCallback := ACallback;
end;

function TRetryPolicy.GetMaxAttempts: Integer;
begin
  Result := FMaxAttempts;
end;

function TRetryPolicy.GetRetryStrategy: TRetryStrategy;
begin
  Result := FRetryStrategy;
end;

function TRetryPolicy.GetBaseDelay: Cardinal;
begin
  Result := FBaseDelay;
end;

function TRetryPolicy.GetMaxDelay: Cardinal;
begin
  Result := FMaxDelay;
end;

procedure TRetryPolicy.SetJitterEnabled(const AEnabled: Boolean);
begin
  FJitterEnabled := AEnabled;
end;

procedure TRetryPolicy.SetJitterRange(const ARange: Double);
begin
  if (ARange >= 0.0) and (ARange <= 1.0) then
    FJitterRange := ARange
  else
    raise EArgumentException.Create('JitterRange deve estar entre 0.0 e 1.0');
end;

function TRetryPolicy.IsJitterEnabled: Boolean;
begin
  Result := FJitterEnabled;
end;

function TRetryPolicy.GetJitterRange: Double;
begin
  Result := FJitterRange;
end;

function TRetryPolicy.ShouldRetry(const AAttempt: TRetryAttempt): Boolean;
begin
  Result := False;

  // Verifica se ainda h? tentativas dispon?veis
  if AAttempt.AttemptNumber >= FMaxAttempts then
    Exit;

  // Verifica se a exce??o ? retri?vel
  if not IsRetriableException(AAttempt.LastException) then
    Exit;

  // Chama callback personalizado se definido
  if Assigned(FRetryCallback) then
    Result := FRetryCallback(AAttempt)
  else
    Result := True;
end;

function TRetryPolicy.CalculateDelay(const AAttemptNumber: Integer): Cardinal;
var
  LDelay: Cardinal;
begin
  case FRetryStrategy of
    rsFixed:
      LDelay := CalculateFixedDelay;
    rsLinear:
      LDelay := CalculateLinearDelay(AAttemptNumber);
    rsExponential:
      LDelay := CalculateExponentialDelay(AAttemptNumber);
    rsCustom:
      LDelay := FBaseDelay; // Para estrat?gia customizada, usar callback
  else
    LDelay := FBaseDelay;
  end;

  // Aplica limite m?ximo
  if LDelay > FMaxDelay then
    LDelay := FMaxDelay;

  // Aplica jitter se habilitado
  if FJitterEnabled then
    LDelay := ApplyJitter(LDelay);

  Result := LDelay;
end;

function TRetryPolicy.CalculateFixedDelay: Cardinal;
begin
  Result := FBaseDelay;
end;

function TRetryPolicy.CalculateLinearDelay(const AAttemptNumber: Integer): Cardinal;
begin
  Result := FBaseDelay * AAttemptNumber;
end;

function TRetryPolicy.CalculateExponentialDelay(const AAttemptNumber: Integer): Cardinal;
var
  LMultiplier: Double;
begin
  LMultiplier := Power(2, AAttemptNumber - 1);
  Result := Round(FBaseDelay * LMultiplier);
end;

function TRetryPolicy.ApplyJitter(const ADelay: Cardinal): Cardinal;
var
  LJitterAmount: Cardinal;
  LRandomFactor: Double;
begin
  if not FJitterEnabled then
  begin
    Result := ADelay;
    Exit;
  end;

  // Gera fator aleat?rio entre -FJitterRange e +FJitterRange
  LRandomFactor := (Random - 0.5) * 2 * FJitterRange;
  LJitterAmount := Round(ADelay * LRandomFactor);

  Result := ADelay + LJitterAmount;

  // Garante que o delay n?o seja negativo
  if Integer(Result) < 0 then
    Result := ADelay div 2;
end;

function TRetryPolicy.IsRetriableException(const AException: Exception): Boolean;
begin
  Result := True; // Por padr?o, todas as exce??es s?o retri?veis

  // Exce??es que n?o devem ser retriadas
  if AException is EArgumentException then
    Result := False
  else if AException is EInvalidOperation then
    Result := False
  else if AException is EAccessViolation then
    Result := False;

  // Adicionar mais tipos de exce??o conforme necess?rio
end;

function TRetryPolicy.Execute(const AAdapter: IRestFrameworkAdapter;
  const ARequestData: TRequestData): Boolean;
var
  LAttempt: TRetryAttempt;
  LStartTime: Cardinal;
  LDelay: Cardinal;
begin
  Result := False;
  LAttempt.AttemptNumber := 0;
  LAttempt.LastException := nil;
  LAttempt.ElapsedTime := 0;

  repeat
    Inc(LAttempt.AttemptNumber);
    LStartTime := GetTickCount;

    try
      TNest4DLogger.Info(Format('Tentativa %d/%d para endpoint: %s',
        [LAttempt.AttemptNumber, FMaxAttempts, ARequestData.Endpoint]));

      Result := AAdapter.ExecuteRequest(ARequestData);

      if Result then
      begin
        TNest4DLogger.Info(Format('Sucesso na tentativa %d para endpoint: %s',
          [LAttempt.AttemptNumber, ARequestData.Endpoint]));
        Exit; // Sucesso, sair do loop
      end;

    except
      on E: Exception do
      begin
        LAttempt.LastException := E;
        LAttempt.ElapsedTime := GetTickCount - LStartTime;

        TNest4DLogger.Error(Format('Falha na tentativa %d para endpoint: %s - %s: %s',
          [LAttempt.AttemptNumber, ARequestData.Endpoint, E.ClassName, E.Message]));
      end;
    end;

    // Verifica se deve tentar novamente
    if ShouldRetry(LAttempt) then
    begin
      LDelay := CalculateDelay(LAttempt.AttemptNumber);
      LAttempt.NextRetryDelay := LDelay;

      TNest4DLogger.Info(Format('Aguardando %d ms antes da pr?xima tentativa', [LDelay]));
      Sleep(LDelay);
    end
    else
      Break;

  until LAttempt.AttemptNumber >= FMaxAttempts;

  if not Result then
  begin
    TNest4DLogger.Error(Format('Todas as %d tentativas falharam para endpoint: %s',
      [FMaxAttempts, ARequestData.Endpoint]));
  end;
end;

{ TRetryPolicyFactory }

class function TRetryPolicyFactory.CreateDefault: IRetryPolicy;
var
  LPolicy: TRetryPolicy;
begin
  LPolicy := TRetryPolicy.Create;
  LPolicy.SetMaxAttempts(3);
  LPolicy.SetRetryStrategy(rsExponential);
  LPolicy.SetBaseDelay(1000);
  LPolicy.SetMaxDelay(30000);
  LPolicy.SetJitterEnabled(True);
  Result := LPolicy;
end;

class function TRetryPolicyFactory.CreateAggressive: IRetryPolicy;
var
  LPolicy: TRetryPolicy;
begin
  LPolicy := TRetryPolicy.Create;
  LPolicy.SetMaxAttempts(5);
  LPolicy.SetRetryStrategy(rsExponential);
  LPolicy.SetBaseDelay(500);
  LPolicy.SetMaxDelay(60000);
  LPolicy.SetJitterEnabled(True);
  LPolicy.SetJitterRange(0.2);
  Result := LPolicy;
end;

class function TRetryPolicyFactory.CreateConservative: IRetryPolicy;
var
  LPolicy: TRetryPolicy;
begin
  LPolicy := TRetryPolicy.Create;
  LPolicy.SetMaxAttempts(2);
  LPolicy.SetRetryStrategy(rsLinear);
  LPolicy.SetBaseDelay(2000);
  LPolicy.SetMaxDelay(10000);
  LPolicy.SetJitterEnabled(False);
  Result := LPolicy;
end;

class function TRetryPolicyFactory.CreateCustom(const AMaxAttempts: Integer;
  const AStrategy: TRetryStrategy; const ABaseDelay, AMaxDelay: Cardinal): IRetryPolicy;
var
  LPolicy: TRetryPolicy;
begin
  LPolicy := TRetryPolicy.Create;
  LPolicy.SetMaxAttempts(AMaxAttempts);
  LPolicy.SetRetryStrategy(AStrategy);
  LPolicy.SetBaseDelay(ABaseDelay);
  LPolicy.SetMaxDelay(AMaxDelay);
  Result := LPolicy;
end;

initialization
  Randomize; // Inicializa gerador de n?meros aleat?rios para jitter

end.

