unit Nest4D.Resilience.Fallback;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  Nest4D.Resilience.Interfaces,
  Nest4D.Request.Data;

type
  // Estrutura para cache de respostas
  TCachedResponse = record
    Response: string;
    ExpirationTime: TDateTime;
    CreatedAt: TDateTime;
    HitCount: Integer;
  end;

  // Estrutura para endpoints de fallback
  TFallbackEndpoint = record
    OriginalEndpoint: string;
    FallbackEndpoint: string;
    Priority: Integer;
    IsActive: Boolean;
    LastUsed: TDateTime;
    SuccessCount: Integer;
    FailureCount: Integer;
  end;

  // Implementa??o do servi?o de fallback
  TFallbackService = class(TInterfacedObject, IFallbackService)
  private
    FFallbackEndpoints: TDictionary<string, TList<TFallbackEndpoint>>;
    FCachedResponses: TDictionary<string, TCachedResponse>;
    FDefaultResponses: TDictionary<string, string>;
    FFallbackCallback: TFallbackCallback;
    FCriticalSection: TRTLCriticalSection;
    FCleanupInterval: Cardinal;
    FLastCleanup: TDateTime;
    FMaxCacheSize: Integer;
    FDefaultCacheTTL: Cardinal;

    procedure InitializeCriticalSection;
    procedure FinalizeCriticalSection;
    procedure Lock;
    procedure Unlock;

    function GetCacheKey(const AEndpoint: string): string;
    function IsResponseExpired(const ACachedResponse: TCachedResponse): Boolean;
    procedure CleanupExpiredCacheInternal;
    procedure EnforceCacheLimit;

    function TryFallbackEndpoints(const AOriginalEndpoint: string;
      const ARequestData: TRequestData): TFallbackResult;
    function TryFallbackCache(const AEndpoint: string): TFallbackResult;
    function TryDefaultResponse(const AEndpoint: string): TFallbackResult;
    function TryCustomCallback(const AException: Exception;
      const ARequestData: TRequestData): TFallbackResult;

    procedure UpdateEndpointStats(const AEndpoint: string; const ASuccess: Boolean);
  public
    constructor Create;
    destructor Destroy; override;

    // IFallbackService implementation
    procedure AddFallbackEndpoint(const AOriginalEndpoint, AFallbackEndpoint: string);
    procedure SetCachedResponse(const AEndpoint, AResponse: string; const AExpirationTime: TDateTime);
    procedure SetDefaultResponse(const AEndpoint, AResponse: string);
    procedure SetFallbackCallback(const ACallback: TFallbackCallback);

    function ExecuteFallback(const AException: Exception; const ARequestData: TRequestData): TFallbackResult;
    function HasFallback(const AEndpoint: string): Boolean;

    procedure ClearExpiredCache;
    procedure ClearAllCache;

    // M?todos adicionais
    procedure AddFallbackEndpointWithPriority(const AOriginalEndpoint, AFallbackEndpoint: string; const APriority: Integer);
    procedure RemoveFallbackEndpoint(const AOriginalEndpoint, AFallbackEndpoint: string);
    procedure SetEndpointActive(const AOriginalEndpoint, AFallbackEndpoint: string; const AActive: Boolean);

    function GetFallbackEndpoints(const AOriginalEndpoint: string): TArray<TFallbackEndpoint>;
    function GetCacheStats: string;
    function GetFallbackStats(const AEndpoint: string): string;

    // Configura??es
    procedure SetCleanupInterval(const AIntervalMs: Cardinal);
    procedure SetMaxCacheSize(const AMaxSize: Integer);
    procedure SetDefaultCacheTTL(const ATTLMs: Cardinal);

    property CleanupInterval: Cardinal read FCleanupInterval write SetCleanupInterval;
    property MaxCacheSize: Integer read FMaxCacheSize write SetMaxCacheSize;
    property DefaultCacheTTL: Cardinal read FDefaultCacheTTL write SetDefaultCacheTTL;
  end;

  // Factory para criar servi?os de fallback pr?-configurados
  TFallbackServiceFactory = class
  public
    class function CreateDefault: IFallbackService;
    class function CreateWithCache(const AMaxCacheSize: Integer; const ADefaultTTL: Cardinal): IFallbackService;
    class function CreateMinimal: IFallbackService;
  end;

implementation

uses
  DateUtils,
  Nest4D.Logging;

{ TFallbackService }

constructor TFallbackService.Create;
begin
  inherited Create;

  FFallbackEndpoints := TDictionary<string, TList<TFallbackEndpoint>>.Create;
  FCachedResponses := TDictionary<string, TCachedResponse>.Create;
  FDefaultResponses := TDictionary<string, string>.Create;

  InitializeCriticalSection;

  FCleanupInterval := 300000; // 5 minutos
  FLastCleanup := Now;
  FMaxCacheSize := 1000;
  FDefaultCacheTTL := 3600000; // 1 hora
  FFallbackCallback := nil;
end;

destructor TFallbackService.Destroy;
var
  LEndpointList: TList<TFallbackEndpoint>;
begin
  Lock;
  try
    // Limpa listas de endpoints de fallback
    for LEndpointList in FFallbackEndpoints.Values do
      LEndpointList.Free;
    FFallbackEndpoints.Free;

    FCachedResponses.Free;
    FDefaultResponses.Free;

    FFallbackCallback := nil;
  finally
    Unlock;
    FinalizeCriticalSection;
  end;

  inherited Destroy;
end;

procedure TFallbackService.InitializeCriticalSection;
begin
  InitializeCriticalSection(FCriticalSection);
end;

procedure TFallbackService.FinalizeCriticalSection;
begin
  DeleteCriticalSection(FCriticalSection);
end;

procedure TFallbackService.Lock;
begin
  EnterCriticalSection(FCriticalSection);
end;

procedure TFallbackService.Unlock;
begin
  LeaveCriticalSection(FCriticalSection);
end;

function TFallbackService.GetCacheKey(const AEndpoint: string): string;
begin
  Result := LowerCase(Trim(AEndpoint));
end;

function TFallbackService.IsResponseExpired(const ACachedResponse: TCachedResponse): Boolean;
begin
  Result := Now > ACachedResponse.ExpirationTime;
end;

procedure TFallbackService.AddFallbackEndpoint(const AOriginalEndpoint, AFallbackEndpoint: string);
begin
  AddFallbackEndpointWithPriority(AOriginalEndpoint, AFallbackEndpoint, 1);
end;

procedure TFallbackService.AddFallbackEndpointWithPriority(const AOriginalEndpoint,
  AFallbackEndpoint: string; const APriority: Integer);
var
  LEndpointList: TList<TFallbackEndpoint>;
  LFallbackEndpoint: TFallbackEndpoint;
  LKey: string;
begin
  Lock;
  try
    LKey := GetCacheKey(AOriginalEndpoint);

    if not FFallbackEndpoints.TryGetValue(LKey, LEndpointList) then
    begin
      LEndpointList := TList<TFallbackEndpoint>.Create;
      FFallbackEndpoints.Add(LKey, LEndpointList);
    end;

    LFallbackEndpoint.OriginalEndpoint := AOriginalEndpoint;
    LFallbackEndpoint.FallbackEndpoint := AFallbackEndpoint;
    LFallbackEndpoint.Priority := APriority;
    LFallbackEndpoint.IsActive := True;
    LFallbackEndpoint.LastUsed := 0;
    LFallbackEndpoint.SuccessCount := 0;
    LFallbackEndpoint.FailureCount := 0;

    LEndpointList.Add(LFallbackEndpoint);

    TNest4DLogger.Info(Format('Fallback endpoint adicionado: %s -> %s (prioridade: %d)',
      [AOriginalEndpoint, AFallbackEndpoint, APriority]));
  finally
    Unlock;
  end;
end;

procedure TFallbackService.RemoveFallbackEndpoint(const AOriginalEndpoint, AFallbackEndpoint: string);
var
  LEndpointList: TList<TFallbackEndpoint>;
  LKey: string;
  I: Integer;
begin
  Lock;
  try
    LKey := GetCacheKey(AOriginalEndpoint);

    if FFallbackEndpoints.TryGetValue(LKey, LEndpointList) then
    begin
      for I := LEndpointList.Count - 1 downto 0 do
      begin
        if SameText(LEndpointList[I].FallbackEndpoint, AFallbackEndpoint) then
        begin
          LEndpointList.Delete(I);
          TNest4DLogger.Info(Format('Fallback endpoint removido: %s -> %s',
            [AOriginalEndpoint, AFallbackEndpoint]));
          Break;
        end;
      end;

      if LEndpointList.Count = 0 then
      begin
        LEndpointList.Free;
        FFallbackEndpoints.Remove(LKey);
      end;
    end;
  finally
    Unlock;
  end;
end;

procedure TFallbackService.SetCachedResponse(const AEndpoint, AResponse: string;
  const AExpirationTime: TDateTime);
var
  LCachedResponse: TCachedResponse;
  LKey: string;
begin
  Lock;
  try
    LKey := GetCacheKey(AEndpoint);

    LCachedResponse.Response := AResponse;
    LCachedResponse.ExpirationTime := AExpirationTime;
    LCachedResponse.CreatedAt := Now;
    LCachedResponse.HitCount := 0;

    FCachedResponses.AddOrSetValue(LKey, LCachedResponse);

    EnforceCacheLimit;

    TNest4DLogger.Info(Format('Resposta em cache definida para endpoint: %s (expira em: %s)',
      [AEndpoint, DateTimeToStr(AExpirationTime)]));
  finally
    Unlock;
  end;
end;

procedure TFallbackService.SetDefaultResponse(const AEndpoint, AResponse: string);
var
  LKey: string;
begin
  Lock;
  try
    LKey := GetCacheKey(AEndpoint);
    FDefaultResponses.AddOrSetValue(LKey, AResponse);

    TNest4DLogger.Info(Format('Resposta padr?o definida para endpoint: %s', [AEndpoint]));
  finally
    Unlock;
  end;
end;

procedure TFallbackService.SetFallbackCallback(const ACallback: TFallbackCallback);
begin
  Lock;
  try
    FFallbackCallback := ACallback;
  finally
    Unlock;
  end;
end;

function TFallbackService.ExecuteFallback(const AException: Exception;
  const ARequestData: TRequestData): TFallbackResult;
begin
  TNest4DLogger.Info(Format('Executando fallback para endpoint: %s', [ARequestData.Endpoint]));

  // Verifica se ? hora de fazer limpeza do cache
  if MilliSecondsBetween(Now, FLastCleanup) > FCleanupInterval then
    ClearExpiredCache;

  // Tenta fallback endpoints alternativos primeiro
  Result := TryFallbackEndpoints(ARequestData.Endpoint, ARequestData);
  if Result.Success then
    Exit;

  // Tenta resposta em cache
  Result := TryFallbackCache(ARequestData.Endpoint);
  if Result.Success then
    Exit;

  // Tenta resposta padr?o
  Result := TryDefaultResponse(ARequestData.Endpoint);
  if Result.Success then
    Exit;

  // Tenta callback personalizado
  Result := TryCustomCallback(AException, ARequestData);
end;

function TFallbackService.TryFallbackEndpoints(const AOriginalEndpoint: string;
  const ARequestData: TRequestData): TFallbackResult;
var
  LEndpointList: TList<TFallbackEndpoint>;
  LFallbackEndpoint: TFallbackEndpoint;
  LKey: string;
  I: Integer;
  LStartTime: Cardinal;
begin
  Result.Success := False;
  Result.Response := '';
  Result.FallbackType := ftAlternativeEndpoint;

  Lock;
  try
    LKey := GetCacheKey(AOriginalEndpoint);

    if FFallbackEndpoints.TryGetValue(LKey, LEndpointList) then
    begin
      // Ordena por prioridade (menor n?mero = maior prioridade)
      LEndpointList.Sort(TComparer<TFallbackEndpoint>.Construct(
        function(const Left, Right: TFallbackEndpoint): Integer
        begin
          Result := Left.Priority - Right.Priority;
        end));

      for I := 0 to LEndpointList.Count - 1 do
      begin
        LFallbackEndpoint := LEndpointList[I];

        if not LFallbackEndpoint.IsActive then
          Continue;

        LStartTime := GetTickCount;

        try
          // Aqui seria feita a chamada para o endpoint de fallback
          // Por enquanto, simula sucesso para demonstra??o
          TNest4DLogger.Info(Format('Tentando fallback endpoint: %s', [LFallbackEndpoint.FallbackEndpoint]));

          // Simula execu??o (em implementa??o real, usaria o adapter)
          Result.Success := True;
          Result.Response := Format('{"fallback": true, "endpoint": "%s", "timestamp": "%s"}',
            [LFallbackEndpoint.FallbackEndpoint, DateTimeToStr(Now)]);
          Result.ExecutionTime := GetTickCount - LStartTime;

          // Atualiza estat?sticas
          UpdateEndpointStats(LFallbackEndpoint.FallbackEndpoint, True);

          TNest4DLogger.Info(Format('Fallback endpoint bem-sucedido: %s', [LFallbackEndpoint.FallbackEndpoint]));
          Exit;

        except
          on E: Exception do
          begin
            UpdateEndpointStats(LFallbackEndpoint.FallbackEndpoint, False);
            TNest4DLogger.Error(Format('Fallback endpoint falhou: %s - %s',
              [LFallbackEndpoint.FallbackEndpoint, E.Message]));
          end;
        end;
      end;
    end;
  finally
    Unlock;
  end;
end;

function TFallbackService.TryFallbackCache(const AEndpoint: string): TFallbackResult;
var
  LCachedResponse: TCachedResponse;
  LKey: string;
begin
  Result.Success := False;
  Result.Response := '';
  Result.FallbackType := ftCachedResponse;
  Result.ExecutionTime := 0;

  Lock;
  try
    LKey := GetCacheKey(AEndpoint);

    if FCachedResponses.TryGetValue(LKey, LCachedResponse) then
    begin
      if not IsResponseExpired(LCachedResponse) then
      begin
        Result.Success := True;
        Result.Response := LCachedResponse.Response;

        // Incrementa contador de hits
        Inc(LCachedResponse.HitCount);
        FCachedResponses.AddOrSetValue(LKey, LCachedResponse);

        TNest4DLogger.Info(Format('Resposta em cache utilizada para endpoint: %s', [AEndpoint]));
      end
      else
      begin
        // Remove resposta expirada
        FCachedResponses.Remove(LKey);
        TNest4DLogger.Info(Format('Resposta em cache expirada removida para endpoint: %s', [AEndpoint]));
      end;
    end;
  finally
    Unlock;
  end;
end;

function TFallbackService.TryDefaultResponse(const AEndpoint: string): TFallbackResult;
var
  LDefaultResponse: string;
  LKey: string;
begin
  Result.Success := False;
  Result.Response := '';
  Result.FallbackType := ftDefaultResponse;
  Result.ExecutionTime := 0;

  Lock;
  try
    LKey := GetCacheKey(AEndpoint);

    if FDefaultResponses.TryGetValue(LKey, LDefaultResponse) then
    begin
      Result.Success := True;
      Result.Response := LDefaultResponse;

      TNest4DLogger.Info(Format('Resposta padr?o utilizada para endpoint: %s', [AEndpoint]));
    end;
  finally
    Unlock;
  end;
end;

function TFallbackService.TryCustomCallback(const AException: Exception;
  const ARequestData: TRequestData): TFallbackResult;
begin
  Result.Success := False;
  Result.Response := '';
  Result.FallbackType := ftCustomHandler;
  Result.ExecutionTime := 0;

  Lock;
  try
    if Assigned(FFallbackCallback) then
    begin
      try
        Result := FFallbackCallback(AException, ARequestData);

        if Result.Success then
          TNest4DLogger.Info(Format('Callback personalizado bem-sucedido para endpoint: %s', [ARequestData.Endpoint]))
        else
          TNest4DLogger.Info(Format('Callback personalizado falhou para endpoint: %s', [ARequestData.Endpoint]));

      except
        on E: Exception do
        begin
          TNest4DLogger.Error(Format('Erro no callback personalizado: %s', [E.Message]));
          Result.Success := False;
        end;
      end;
    end;
  finally
    Unlock;
  end;
end;

procedure TFallbackService.UpdateEndpointStats(const AEndpoint: string; const ASuccess: Boolean);
var
  LEndpointList: TList<TFallbackEndpoint>;
  LFallbackEndpoint: TFallbackEndpoint;
  I, J: Integer;
begin
  // Atualiza estat?sticas do endpoint em todas as listas
  for LEndpointList in FFallbackEndpoints.Values do
  begin
    for I := 0 to LEndpointList.Count - 1 do
    begin
      LFallbackEndpoint := LEndpointList[I];

      if SameText(LFallbackEndpoint.FallbackEndpoint, AEndpoint) then
      begin
        LFallbackEndpoint.LastUsed := Now;

        if ASuccess then
          Inc(LFallbackEndpoint.SuccessCount)
        else
          Inc(LFallbackEndpoint.FailureCount);

        LEndpointList[I] := LFallbackEndpoint;
      end;
    end;
  end;
end;

function TFallbackService.HasFallback(const AEndpoint: string): Boolean;
var
  LKey: string;
begin
  Lock;
  try
    LKey := GetCacheKey(AEndpoint);

    Result := FFallbackEndpoints.ContainsKey(LKey) or
              FCachedResponses.ContainsKey(LKey) or
              FDefaultResponses.ContainsKey(LKey) or
              Assigned(FFallbackCallback);
  finally
    Unlock;
  end;
end;

procedure TFallbackService.ClearExpiredCache;
var
  LKeysToRemove: TArray<string>;
  LKey: string;
  LCachedResponse: TCachedResponse;
begin
  Lock;
  try
    CleanupExpiredCacheInternal;
    FLastCleanup := Now;
  finally
    Unlock;
  end;
end;

procedure TFallbackService.CleanupExpiredCacheInternal;
var
  LKeysToRemove: TList<string>;
  LKey: string;
  LCachedResponse: TCachedResponse;
begin
  LKeysToRemove := TList<string>.Create;
  try
    for LKey in FCachedResponses.Keys do
    begin
      if FCachedResponses.TryGetValue(LKey, LCachedResponse) then
      begin
        if IsResponseExpired(LCachedResponse) then
          LKeysToRemove.Add(LKey);
      end;
    end;

    for LKey in LKeysToRemove do
    begin
      FCachedResponses.Remove(LKey);
      TNest4DLogger.Info(Format('Resposta em cache expirada removida: %s', [LKey]));
    end;

    if LKeysToRemove.Count > 0 then
      TNest4DLogger.Info(Format('Limpeza de cache conclu?da: %d entradas removidas', [LKeysToRemove.Count]));

  finally
    LKeysToRemove.Free;
  end;
end;

procedure TFallbackService.EnforceCacheLimit;
var
  LCacheEntries: TList<TPair<string, TCachedResponse>>;
  LPair: TPair<string, TCachedResponse>;
  I: Integer;
begin
  if FCachedResponses.Count <= FMaxCacheSize then
    Exit;

  LCacheEntries := TList<TPair<string, TCachedResponse>>.Create;
  try
    for LPair in FCachedResponses do
      LCacheEntries.Add(LPair);

    // Ordena por data de cria??o (mais antigas primeiro)
    LCacheEntries.Sort(TComparer<TPair<string, TCachedResponse>>.Construct(
      function(const Left, Right: TPair<string, TCachedResponse>): Integer
      begin
        Result := CompareDateTime(Left.Value.CreatedAt, Right.Value.CreatedAt);
      end));

    // Remove entradas mais antigas at? atingir o limite
    I := 0;
    while (FCachedResponses.Count > FMaxCacheSize) and (I < LCacheEntries.Count) do
    begin
      FCachedResponses.Remove(LCacheEntries[I].Key);
      Inc(I);
    end;

    if I > 0 then
      TNest4DLogger.Info(Format('Limite de cache aplicado: %d entradas antigas removidas', [I]));

  finally
    LCacheEntries.Free;
  end;
end;

procedure TFallbackService.ClearAllCache;
begin
  Lock;
  try
    FCachedResponses.Clear;
    TNest4DLogger.Info('Todo o cache de fallback foi limpo');
  finally
    Unlock;
  end;
end;

procedure TFallbackService.SetEndpointActive(const AOriginalEndpoint, AFallbackEndpoint: string;
  const AActive: Boolean);
var
  LEndpointList: TList<TFallbackEndpoint>;
  LFallbackEndpoint: TFallbackEndpoint;
  LKey: string;
  I: Integer;
begin
  Lock;
  try
    LKey := GetCacheKey(AOriginalEndpoint);

    if FFallbackEndpoints.TryGetValue(LKey, LEndpointList) then
    begin
      for I := 0 to LEndpointList.Count - 1 do
      begin
        LFallbackEndpoint := LEndpointList[I];

        if SameText(LFallbackEndpoint.FallbackEndpoint, AFallbackEndpoint) then
        begin
          LFallbackEndpoint.IsActive := AActive;
          LEndpointList[I] := LFallbackEndpoint;

          TNest4DLogger.Info(Format('Fallback endpoint %s: %s -> %s',
            [IfThen(AActive, 'ativado', 'desativado'), AOriginalEndpoint, AFallbackEndpoint]));
          Break;
        end;
      end;
    end;
  finally
    Unlock;
  end;
end;

function TFallbackService.GetFallbackEndpoints(const AOriginalEndpoint: string): TArray<TFallbackEndpoint>;
var
  LEndpointList: TList<TFallbackEndpoint>;
  LKey: string;
begin
  Lock;
  try
    LKey := GetCacheKey(AOriginalEndpoint);

    if FFallbackEndpoints.TryGetValue(LKey, LEndpointList) then
      Result := LEndpointList.ToArray
    else
      SetLength(Result, 0);
  finally
    Unlock;
  end;
end;

function TFallbackService.GetCacheStats: string;
var
  LTotalEntries, LExpiredEntries: Integer;
  LCachedResponse: TCachedResponse;
begin
  Lock;
  try
    LTotalEntries := FCachedResponses.Count;
    LExpiredEntries := 0;

    for LCachedResponse in FCachedResponses.Values do
    begin
      if IsResponseExpired(LCachedResponse) then
        Inc(LExpiredEntries);
    end;

    Result := Format('Cache Stats - Total: %d, Expiradas: %d, Ativas: %d, Limite: %d',
      [LTotalEntries, LExpiredEntries, LTotalEntries - LExpiredEntries, FMaxCacheSize]);
  finally
    Unlock;
  end;
end;

function TFallbackService.GetFallbackStats(const AEndpoint: string): string;
var
  LEndpoints: TArray<TFallbackEndpoint>;
  LEndpoint: TFallbackEndpoint;
  LStats: TStringBuilder;
begin
  LStats := TStringBuilder.Create;
  try
    LEndpoints := GetFallbackEndpoints(AEndpoint);

    LStats.AppendFormat('Fallback Stats para %s:%s', [AEndpoint, sLineBreak]);

    for LEndpoint in LEndpoints do
    begin
      LStats.AppendFormat('  %s (Prioridade: %d, Ativo: %s, Sucessos: %d, Falhas: %d)%s',
        [LEndpoint.FallbackEndpoint, LEndpoint.Priority,
         IfThen(LEndpoint.IsActive, 'Sim', 'N?o'),
         LEndpoint.SuccessCount, LEndpoint.FailureCount, sLineBreak]);
    end;

    if Length(LEndpoints) = 0 then
      LStats.Append('  Nenhum endpoint de fallback configurado');

    Result := LStats.ToString;
  finally
    LStats.Free;
  end;
end;

procedure TFallbackService.SetCleanupInterval(const AIntervalMs: Cardinal);
begin
  FCleanupInterval := AIntervalMs;
end;

procedure TFallbackService.SetMaxCacheSize(const AMaxSize: Integer);
begin
  if AMaxSize > 0 then
  begin
    FMaxCacheSize := AMaxSize;
    Lock;
    try
      EnforceCacheLimit;
    finally
      Unlock;
    end;
  end;
end;

procedure TFallbackService.SetDefaultCacheTTL(const ATTLMs: Cardinal);
begin
  FDefaultCacheTTL := ATTLMs;
end;

{ TFallbackServiceFactory }

class function TFallbackServiceFactory.CreateDefault: IFallbackService;
var
  LService: TFallbackService;
begin
  LService := TFallbackService.Create;
  LService.SetMaxCacheSize(1000);
  LService.SetDefaultCacheTTL(3600000); // 1 hora
  LService.SetCleanupInterval(300000);  // 5 minutos
  Result := LService;
end;

class function TFallbackServiceFactory.CreateWithCache(const AMaxCacheSize: Integer;
  const ADefaultTTL: Cardinal): IFallbackService;
var
  LService: TFallbackService;
begin
  LService := TFallbackService.Create;
  LService.SetMaxCacheSize(AMaxCacheSize);
  LService.SetDefaultCacheTTL(ADefaultTTL);
  LService.SetCleanupInterval(ADefaultTTL div 12); // Cleanup a cada 1/12 do TTL
  Result := LService;
end;

class function TFallbackServiceFactory.CreateMinimal: IFallbackService;
var
  LService: TFallbackService;
begin
  LService := TFallbackService.Create;
  LService.SetMaxCacheSize(100);
  LService.SetDefaultCacheTTL(600000);  // 10 minutos
  LService.SetCleanupInterval(120000);  // 2 minutos
  Result := LService;
end;

end..Length -gt 0) {  param($match) $prefix = $match.Groups[1].Value; $base = $match.Groups[2].Value; $suffix = $match.Groups[3].Value; $end = $match.Groups[4].Value; $fullUnit = $base + $suffix; $parts = $fullUnit.Split('.'); $camelParts = @(); foreach ($part in $parts) { if ($part.Length -gt 0) { $camelParts += $part.Substring(0,1).ToUpper() + $part.Substring(1).ToLower() } }; $camelUnit = $camelParts -join '.'; return $prefix + $camelUnit + $end

{$ifdef fpc}
  {$mode delphi}
{$endif}

interface

uses
  SysUtils,
  Classes,
  Generics.Collections,
  Nest4D.resilience.interfaces,
  Nest4D.request.data;

type
  // Estrutura para cache de respostas
  TCachedResponse = record
    Response: string;
    ExpirationTime: TDateTime;
    CreatedAt: TDateTime;
    HitCount: Integer;
  end;

  // Estrutura para endpoints de fallback
  TFallbackEndpoint = record
    OriginalEndpoint: string;
    FallbackEndpoint: string;
    Priority: Integer;
    IsActive: Boolean;
    LastUsed: TDateTime;
    SuccessCount: Integer;
    FailureCount: Integer;
  end;

  // Implementa??o do servi?o de fallback
  TFallbackService = class(TInterfacedObject, IFallbackService)
  private
    FFallbackEndpoints: TDictionary<string, TList<TFallbackEndpoint>>;
    FCachedResponses: TDictionary<string, TCachedResponse>;
    FDefaultResponses: TDictionary<string, string>;
    FFallbackCallback: TFallbackCallback;
    FCriticalSection: TRTLCriticalSection;
    FCleanupInterval: Cardinal;
    FLastCleanup: TDateTime;
    FMaxCacheSize: Integer;
    FDefaultCacheTTL: Cardinal;

    procedure InitializeCriticalSection;
    procedure FinalizeCriticalSection;
    procedure Lock;
    procedure Unlock;

    function GetCacheKey(const AEndpoint: string): string;
    function IsResponseExpired(const ACachedResponse: TCachedResponse): Boolean;
    procedure CleanupExpiredCacheInternal;
    procedure EnforceCacheLimit;

    function TryFallbackEndpoints(const AOriginalEndpoint: string;
      const ARequestData: TRequestData): TFallbackResult;
    function TryFallbackCache(const AEndpoint: string): TFallbackResult;
    function TryDefaultResponse(const AEndpoint: string): TFallbackResult;
    function TryCustomCallback(const AException: Exception;
      const ARequestData: TRequestData): TFallbackResult;

    procedure UpdateEndpointStats(const AEndpoint: string; const ASuccess: Boolean);
  public
    constructor Create;
    destructor Destroy; override;

    // IFallbackService implementation
    procedure AddFallbackEndpoint(const AOriginalEndpoint, AFallbackEndpoint: string);
    procedure SetCachedResponse(const AEndpoint, AResponse: string; const AExpirationTime: TDateTime);
    procedure SetDefaultResponse(const AEndpoint, AResponse: string);
    procedure SetFallbackCallback(const ACallback: TFallbackCallback);

    function ExecuteFallback(const AException: Exception; const ARequestData: TRequestData): TFallbackResult;
    function HasFallback(const AEndpoint: string): Boolean;

    procedure ClearExpiredCache;
    procedure ClearAllCache;

    // M?todos adicionais
    procedure AddFallbackEndpointWithPriority(const AOriginalEndpoint, AFallbackEndpoint: string; const APriority: Integer);
    procedure RemoveFallbackEndpoint(const AOriginalEndpoint, AFallbackEndpoint: string);
    procedure SetEndpointActive(const AOriginalEndpoint, AFallbackEndpoint: string; const AActive: Boolean);

    function GetFallbackEndpoints(const AOriginalEndpoint: string): TArray<TFallbackEndpoint>;
    function GetCacheStats: string;
    function GetFallbackStats(const AEndpoint: string): string;

    // Configura??es
    procedure SetCleanupInterval(const AIntervalMs: Cardinal);
    procedure SetMaxCacheSize(const AMaxSize: Integer);
    procedure SetDefaultCacheTTL(const ATTLMs: Cardinal);

    property CleanupInterval: Cardinal read FCleanupInterval write SetCleanupInterval;
    property MaxCacheSize: Integer read FMaxCacheSize write SetMaxCacheSize;
    property DefaultCacheTTL: Cardinal read FDefaultCacheTTL write SetDefaultCacheTTL;
  end;

  // Factory para criar servi?os de fallback pr?-configurados
  TFallbackServiceFactory = class
  public
    class function CreateDefault: IFallbackService;
    class function CreateWithCache(const AMaxCacheSize: Integer; const ADefaultTTL: Cardinal): IFallbackService;
    class function CreateMinimal: IFallbackService;
  end;

implementation

uses
  DateUtils,
  Nest4D.logging;

{ TFallbackService }

constructor TFallbackService.Create;
begin
  inherited Create;

  FFallbackEndpoints := TDictionary<string, TList<TFallbackEndpoint>>.Create;
  FCachedResponses := TDictionary<string, TCachedResponse>.Create;
  FDefaultResponses := TDictionary<string, string>.Create;

  InitializeCriticalSection;

  FCleanupInterval := 300000; // 5 minutos
  FLastCleanup := Now;
  FMaxCacheSize := 1000;
  FDefaultCacheTTL := 3600000; // 1 hora
  FFallbackCallback := nil;
end;

destructor TFallbackService.Destroy;
var
  LEndpointList: TList<TFallbackEndpoint>;
begin
  Lock;
  try
    // Limpa listas de endpoints de fallback
    for LEndpointList in FFallbackEndpoints.Values do
      LEndpointList.Free;
    FFallbackEndpoints.Free;

    FCachedResponses.Free;
    FDefaultResponses.Free;

    FFallbackCallback := nil;
  finally
    Unlock;
    FinalizeCriticalSection;
  end;

  inherited Destroy;
end;

procedure TFallbackService.InitializeCriticalSection;
begin
  InitializeCriticalSection(FCriticalSection);
end;

procedure TFallbackService.FinalizeCriticalSection;
begin
  DeleteCriticalSection(FCriticalSection);
end;

procedure TFallbackService.Lock;
begin
  EnterCriticalSection(FCriticalSection);
end;

procedure TFallbackService.Unlock;
begin
  LeaveCriticalSection(FCriticalSection);
end;

function TFallbackService.GetCacheKey(const AEndpoint: string): string;
begin
  Result := LowerCase(Trim(AEndpoint));
end;

function TFallbackService.IsResponseExpired(const ACachedResponse: TCachedResponse): Boolean;
begin
  Result := Now > ACachedResponse.ExpirationTime;
end;

procedure TFallbackService.AddFallbackEndpoint(const AOriginalEndpoint, AFallbackEndpoint: string);
begin
  AddFallbackEndpointWithPriority(AOriginalEndpoint, AFallbackEndpoint, 1);
end;

procedure TFallbackService.AddFallbackEndpointWithPriority(const AOriginalEndpoint,
  AFallbackEndpoint: string; const APriority: Integer);
var
  LEndpointList: TList<TFallbackEndpoint>;
  LFallbackEndpoint: TFallbackEndpoint;
  LKey: string;
begin
  Lock;
  try
    LKey := GetCacheKey(AOriginalEndpoint);

    if not FFallbackEndpoints.TryGetValue(LKey, LEndpointList) then
    begin
      LEndpointList := TList<TFallbackEndpoint>.Create;
      FFallbackEndpoints.Add(LKey, LEndpointList);
    end;

    LFallbackEndpoint.OriginalEndpoint := AOriginalEndpoint;
    LFallbackEndpoint.FallbackEndpoint := AFallbackEndpoint;
    LFallbackEndpoint.Priority := APriority;
    LFallbackEndpoint.IsActive := True;
    LFallbackEndpoint.LastUsed := 0;
    LFallbackEndpoint.SuccessCount := 0;
    LFallbackEndpoint.FailureCount := 0;

    LEndpointList.Add(LFallbackEndpoint);

    TNest4DLogger.Info(Format('Fallback endpoint adicionado: %s -> %s (prioridade: %d)',
      [AOriginalEndpoint, AFallbackEndpoint, APriority]));
  finally
    Unlock;
  end;
end;

procedure TFallbackService.RemoveFallbackEndpoint(const AOriginalEndpoint, AFallbackEndpoint: string);
var
  LEndpointList: TList<TFallbackEndpoint>;
  LKey: string;
  I: Integer;
begin
  Lock;
  try
    LKey := GetCacheKey(AOriginalEndpoint);

    if FFallbackEndpoints.TryGetValue(LKey, LEndpointList) then
    begin
      for I := LEndpointList.Count - 1 downto 0 do
      begin
        if SameText(LEndpointList[I].FallbackEndpoint, AFallbackEndpoint) then
        begin
          LEndpointList.Delete(I);
          TNest4DLogger.Info(Format('Fallback endpoint removido: %s -> %s',
            [AOriginalEndpoint, AFallbackEndpoint]));
          Break;
        end;
      end;

      if LEndpointList.Count = 0 then
      begin
        LEndpointList.Free;
        FFallbackEndpoints.Remove(LKey);
      end;
    end;
  finally
    Unlock;
  end;
end;

procedure TFallbackService.SetCachedResponse(const AEndpoint, AResponse: string;
  const AExpirationTime: TDateTime);
var
  LCachedResponse: TCachedResponse;
  LKey: string;
begin
  Lock;
  try
    LKey := GetCacheKey(AEndpoint);

    LCachedResponse.Response := AResponse;
    LCachedResponse.ExpirationTime := AExpirationTime;
    LCachedResponse.CreatedAt := Now;
    LCachedResponse.HitCount := 0;

    FCachedResponses.AddOrSetValue(LKey, LCachedResponse);

    EnforceCacheLimit;

    TNest4DLogger.Info(Format('Resposta em cache definida para endpoint: %s (expira em: %s)',
      [AEndpoint, DateTimeToStr(AExpirationTime)]));
  finally
    Unlock;
  end;
end;

procedure TFallbackService.SetDefaultResponse(const AEndpoint, AResponse: string);
var
  LKey: string;
begin
  Lock;
  try
    LKey := GetCacheKey(AEndpoint);
    FDefaultResponses.AddOrSetValue(LKey, AResponse);

    TNest4DLogger.Info(Format('Resposta padr?o definida para endpoint: %s', [AEndpoint]));
  finally
    Unlock;
  end;
end;

procedure TFallbackService.SetFallbackCallback(const ACallback: TFallbackCallback);
begin
  Lock;
  try
    FFallbackCallback := ACallback;
  finally
    Unlock;
  end;
end;

function TFallbackService.ExecuteFallback(const AException: Exception;
  const ARequestData: TRequestData): TFallbackResult;
begin
  TNest4DLogger.Info(Format('Executando fallback para endpoint: %s', [ARequestData.Endpoint]));

  // Verifica se ? hora de fazer limpeza do cache
  if MilliSecondsBetween(Now, FLastCleanup) > FCleanupInterval then
    ClearExpiredCache;

  // Tenta fallback endpoints alternativos primeiro
  Result := TryFallbackEndpoints(ARequestData.Endpoint, ARequestData);
  if Result.Success then
    Exit;

  // Tenta resposta em cache
  Result := TryFallbackCache(ARequestData.Endpoint);
  if Result.Success then
    Exit;

  // Tenta resposta padr?o
  Result := TryDefaultResponse(ARequestData.Endpoint);
  if Result.Success then
    Exit;

  // Tenta callback personalizado
  Result := TryCustomCallback(AException, ARequestData);
end;

function TFallbackService.TryFallbackEndpoints(const AOriginalEndpoint: string;
  const ARequestData: TRequestData): TFallbackResult;
var
  LEndpointList: TList<TFallbackEndpoint>;
  LFallbackEndpoint: TFallbackEndpoint;
  LKey: string;
  I: Integer;
  LStartTime: Cardinal;
begin
  Result.Success := False;
  Result.Response := '';
  Result.FallbackType := ftAlternativeEndpoint;

  Lock;
  try
    LKey := GetCacheKey(AOriginalEndpoint);

    if FFallbackEndpoints.TryGetValue(LKey, LEndpointList) then
    begin
      // Ordena por prioridade (menor n?mero = maior prioridade)
      LEndpointList.Sort(TComparer<TFallbackEndpoint>.Construct(
        function(const Left, Right: TFallbackEndpoint): Integer
        begin
          Result := Left.Priority - Right.Priority;
        end));

      for I := 0 to LEndpointList.Count - 1 do
      begin
        LFallbackEndpoint := LEndpointList[I];

        if not LFallbackEndpoint.IsActive then
          Continue;

        LStartTime := GetTickCount;

        try
          // Aqui seria feita a chamada para o endpoint de fallback
          // Por enquanto, simula sucesso para demonstra??o
          TNest4DLogger.Info(Format('Tentando fallback endpoint: %s', [LFallbackEndpoint.FallbackEndpoint]));

          // Simula execu??o (em implementa??o real, usaria o adapter)
          Result.Success := True;
          Result.Response := Format('{"fallback": true, "endpoint": "%s", "timestamp": "%s"}',
            [LFallbackEndpoint.FallbackEndpoint, DateTimeToStr(Now)]);
          Result.ExecutionTime := GetTickCount - LStartTime;

          // Atualiza estat?sticas
          UpdateEndpointStats(LFallbackEndpoint.FallbackEndpoint, True);

          TNest4DLogger.Info(Format('Fallback endpoint bem-sucedido: %s', [LFallbackEndpoint.FallbackEndpoint]));
          Exit;

        except
          on E: Exception do
          begin
            UpdateEndpointStats(LFallbackEndpoint.FallbackEndpoint, False);
            TNest4DLogger.Error(Format('Fallback endpoint falhou: %s - %s',
              [LFallbackEndpoint.FallbackEndpoint, E.Message]));
          end;
        end;
      end;
    end;
  finally
    Unlock;
  end;
end;

function TFallbackService.TryFallbackCache(const AEndpoint: string): TFallbackResult;
var
  LCachedResponse: TCachedResponse;
  LKey: string;
begin
  Result.Success := False;
  Result.Response := '';
  Result.FallbackType := ftCachedResponse;
  Result.ExecutionTime := 0;

  Lock;
  try
    LKey := GetCacheKey(AEndpoint);

    if FCachedResponses.TryGetValue(LKey, LCachedResponse) then
    begin
      if not IsResponseExpired(LCachedResponse) then
      begin
        Result.Success := True;
        Result.Response := LCachedResponse.Response;

        // Incrementa contador de hits
        Inc(LCachedResponse.HitCount);
        FCachedResponses.AddOrSetValue(LKey, LCachedResponse);

        TNest4DLogger.Info(Format('Resposta em cache utilizada para endpoint: %s', [AEndpoint]));
      end
      else
      begin
        // Remove resposta expirada
        FCachedResponses.Remove(LKey);
        TNest4DLogger.Info(Format('Resposta em cache expirada removida para endpoint: %s', [AEndpoint]));
      end;
    end;
  finally
    Unlock;
  end;
end;

function TFallbackService.TryDefaultResponse(const AEndpoint: string): TFallbackResult;
var
  LDefaultResponse: string;
  LKey: string;
begin
  Result.Success := False;
  Result.Response := '';
  Result.FallbackType := ftDefaultResponse;
  Result.ExecutionTime := 0;

  Lock;
  try
    LKey := GetCacheKey(AEndpoint);

    if FDefaultResponses.TryGetValue(LKey, LDefaultResponse) then
    begin
      Result.Success := True;
      Result.Response := LDefaultResponse;

      TNest4DLogger.Info(Format('Resposta padr?o utilizada para endpoint: %s', [AEndpoint]));
    end;
  finally
    Unlock;
  end;
end;

function TFallbackService.TryCustomCallback(const AException: Exception;
  const ARequestData: TRequestData): TFallbackResult;
begin
  Result.Success := False;
  Result.Response := '';
  Result.FallbackType := ftCustomHandler;
  Result.ExecutionTime := 0;

  Lock;
  try
    if Assigned(FFallbackCallback) then
    begin
      try
        Result := FFallbackCallback(AException, ARequestData);

        if Result.Success then
          TNest4DLogger.Info(Format('Callback personalizado bem-sucedido para endpoint: %s', [ARequestData.Endpoint]))
        else
          TNest4DLogger.Info(Format('Callback personalizado falhou para endpoint: %s', [ARequestData.Endpoint]));

      except
        on E: Exception do
        begin
          TNest4DLogger.Error(Format('Erro no callback personalizado: %s', [E.Message]));
          Result.Success := False;
        end;
      end;
    end;
  finally
    Unlock;
  end;
end;

procedure TFallbackService.UpdateEndpointStats(const AEndpoint: string; const ASuccess: Boolean);
var
  LEndpointList: TList<TFallbackEndpoint>;
  LFallbackEndpoint: TFallbackEndpoint;
  I, J: Integer;
begin
  // Atualiza estat?sticas do endpoint em todas as listas
  for LEndpointList in FFallbackEndpoints.Values do
  begin
    for I := 0 to LEndpointList.Count - 1 do
    begin
      LFallbackEndpoint := LEndpointList[I];

      if SameText(LFallbackEndpoint.FallbackEndpoint, AEndpoint) then
      begin
        LFallbackEndpoint.LastUsed := Now;

        if ASuccess then
          Inc(LFallbackEndpoint.SuccessCount)
        else
          Inc(LFallbackEndpoint.FailureCount);

        LEndpointList[I] := LFallbackEndpoint;
      end;
    end;
  end;
end;

function TFallbackService.HasFallback(const AEndpoint: string): Boolean;
var
  LKey: string;
begin
  Lock;
  try
    LKey := GetCacheKey(AEndpoint);

    Result := FFallbackEndpoints.ContainsKey(LKey) or
              FCachedResponses.ContainsKey(LKey) or
              FDefaultResponses.ContainsKey(LKey) or
              Assigned(FFallbackCallback);
  finally
    Unlock;
  end;
end;

procedure TFallbackService.ClearExpiredCache;
var
  LKeysToRemove: TArray<string>;
  LKey: string;
  LCachedResponse: TCachedResponse;
begin
  Lock;
  try
    CleanupExpiredCacheInternal;
    FLastCleanup := Now;
  finally
    Unlock;
  end;
end;

procedure TFallbackService.CleanupExpiredCacheInternal;
var
  LKeysToRemove: TList<string>;
  LKey: string;
  LCachedResponse: TCachedResponse;
begin
  LKeysToRemove := TList<string>.Create;
  try
    for LKey in FCachedResponses.Keys do
    begin
      if FCachedResponses.TryGetValue(LKey, LCachedResponse) then
      begin
        if IsResponseExpired(LCachedResponse) then
          LKeysToRemove.Add(LKey);
      end;
    end;

    for LKey in LKeysToRemove do
    begin
      FCachedResponses.Remove(LKey);
      TNest4DLogger.Info(Format('Resposta em cache expirada removida: %s', [LKey]));
    end;

    if LKeysToRemove.Count > 0 then
      TNest4DLogger.Info(Format('Limpeza de cache conclu?da: %d entradas removidas', [LKeysToRemove.Count]));

  finally
    LKeysToRemove.Free;
  end;
end;

procedure TFallbackService.EnforceCacheLimit;
var
  LCacheEntries: TList<TPair<string, TCachedResponse>>;
  LPair: TPair<string, TCachedResponse>;
  I: Integer;
begin
  if FCachedResponses.Count <= FMaxCacheSize then
    Exit;

  LCacheEntries := TList<TPair<string, TCachedResponse>>.Create;
  try
    for LPair in FCachedResponses do
      LCacheEntries.Add(LPair);

    // Ordena por data de cria??o (mais antigas primeiro)
    LCacheEntries.Sort(TComparer<TPair<string, TCachedResponse>>.Construct(
      function(const Left, Right: TPair<string, TCachedResponse>): Integer
      begin
        Result := CompareDateTime(Left.Value.CreatedAt, Right.Value.CreatedAt);
      end));

    // Remove entradas mais antigas at? atingir o limite
    I := 0;
    while (FCachedResponses.Count > FMaxCacheSize) and (I < LCacheEntries.Count) do
    begin
      FCachedResponses.Remove(LCacheEntries[I].Key);
      Inc(I);
    end;

    if I > 0 then
      TNest4DLogger.Info(Format('Limite de cache aplicado: %d entradas antigas removidas', [I]));

  finally
    LCacheEntries.Free;
  end;
end;

procedure TFallbackService.ClearAllCache;
begin
  Lock;
  try
    FCachedResponses.Clear;
    TNest4DLogger.Info('Todo o cache de fallback foi limpo');
  finally
    Unlock;
  end;
end;

procedure TFallbackService.SetEndpointActive(const AOriginalEndpoint, AFallbackEndpoint: string;
  const AActive: Boolean);
var
  LEndpointList: TList<TFallbackEndpoint>;
  LFallbackEndpoint: TFallbackEndpoint;
  LKey: string;
  I: Integer;
begin
  Lock;
  try
    LKey := GetCacheKey(AOriginalEndpoint);

    if FFallbackEndpoints.TryGetValue(LKey, LEndpointList) then
    begin
      for I := 0 to LEndpointList.Count - 1 do
      begin
        LFallbackEndpoint := LEndpointList[I];

        if SameText(LFallbackEndpoint.FallbackEndpoint, AFallbackEndpoint) then
        begin
          LFallbackEndpoint.IsActive := AActive;
          LEndpointList[I] := LFallbackEndpoint;

          TNest4DLogger.Info(Format('Fallback endpoint %s: %s -> %s',
            [IfThen(AActive, 'ativado', 'desativado'), AOriginalEndpoint, AFallbackEndpoint]));
          Break;
        end;
      end;
    end;
  finally
    Unlock;
  end;
end;

function TFallbackService.GetFallbackEndpoints(const AOriginalEndpoint: string): TArray<TFallbackEndpoint>;
var
  LEndpointList: TList<TFallbackEndpoint>;
  LKey: string;
begin
  Lock;
  try
    LKey := GetCacheKey(AOriginalEndpoint);

    if FFallbackEndpoints.TryGetValue(LKey, LEndpointList) then
      Result := LEndpointList.ToArray
    else
      SetLength(Result, 0);
  finally
    Unlock;
  end;
end;

function TFallbackService.GetCacheStats: string;
var
  LTotalEntries, LExpiredEntries: Integer;
  LCachedResponse: TCachedResponse;
begin
  Lock;
  try
    LTotalEntries := FCachedResponses.Count;
    LExpiredEntries := 0;

    for LCachedResponse in FCachedResponses.Values do
    begin
      if IsResponseExpired(LCachedResponse) then
        Inc(LExpiredEntries);
    end;

    Result := Format('Cache Stats - Total: %d, Expiradas: %d, Ativas: %d, Limite: %d',
      [LTotalEntries, LExpiredEntries, LTotalEntries - LExpiredEntries, FMaxCacheSize]);
  finally
    Unlock;
  end;
end;

function TFallbackService.GetFallbackStats(const AEndpoint: string): string;
var
  LEndpoints: TArray<TFallbackEndpoint>;
  LEndpoint: TFallbackEndpoint;
  LStats: TStringBuilder;
begin
  LStats := TStringBuilder.Create;
  try
    LEndpoints := GetFallbackEndpoints(AEndpoint);

    LStats.AppendFormat('Fallback Stats para %s:%s', [AEndpoint, sLineBreak]);

    for LEndpoint in LEndpoints do
    begin
      LStats.AppendFormat('  %s (Prioridade: %d, Ativo: %s, Sucessos: %d, Falhas: %d)%s',
        [LEndpoint.FallbackEndpoint, LEndpoint.Priority,
         IfThen(LEndpoint.IsActive, 'Sim', 'N?o'),
         LEndpoint.SuccessCount, LEndpoint.FailureCount, sLineBreak]);
    end;

    if Length(LEndpoints) = 0 then
      LStats.Append('  Nenhum endpoint de fallback configurado');

    Result := LStats.ToString;
  finally
    LStats.Free;
  end;
end;

procedure TFallbackService.SetCleanupInterval(const AIntervalMs: Cardinal);
begin
  FCleanupInterval := AIntervalMs;
end;

procedure TFallbackService.SetMaxCacheSize(const AMaxSize: Integer);
begin
  if AMaxSize > 0 then
  begin
    FMaxCacheSize := AMaxSize;
    Lock;
    try
      EnforceCacheLimit;
    finally
      Unlock;
    end;
  end;
end;

procedure TFallbackService.SetDefaultCacheTTL(const ATTLMs: Cardinal);
begin
  FDefaultCacheTTL := ATTLMs;
end;

{ TFallbackServiceFactory }

class function TFallbackServiceFactory.CreateDefault: IFallbackService;
var
  LService: TFallbackService;
begin
  LService := TFallbackService.Create;
  LService.SetMaxCacheSize(1000);
  LService.SetDefaultCacheTTL(3600000); // 1 hora
  LService.SetCleanupInterval(300000);  // 5 minutos
  Result := LService;
end;

class function TFallbackServiceFactory.CreateWithCache(const AMaxCacheSize: Integer;
  const ADefaultTTL: Cardinal): IFallbackService;
var
  LService: TFallbackService;
begin
  LService := TFallbackService.Create;
  LService.SetMaxCacheSize(AMaxCacheSize);
  LService.SetDefaultCacheTTL(ADefaultTTL);
  LService.SetCleanupInterval(ADefaultTTL div 12); // Cleanup a cada 1/12 do TTL
  Result := LService;
end;

class function TFallbackServiceFactory.CreateMinimal: IFallbackService;
var
  LService: TFallbackService;
begin
  LService := TFallbackService.Create;
  LService.SetMaxCacheSize(100);
  LService.SetDefaultCacheTTL(600000);  // 10 minutos
  LService.SetCleanupInterval(120000);  // 2 minutos
  Result := LService;
end;

end..Substring(0,1).ToUpper() +  param($match) $prefix = $match.Groups[1].Value; $base = $match.Groups[2].Value; $suffix = $match.Groups[3].Value; $end = $match.Groups[4].Value; $fullUnit = $base + $suffix; $parts = $fullUnit.Split('.'); $camelParts = @(); foreach ($part in $parts) { if ($part.Length -gt 0) { $camelParts += $part.Substring(0,1).ToUpper() + $part.Substring(1).ToLower() } }; $camelUnit = $camelParts -join '.'; return $prefix + $camelUnit + $end

{$ifdef fpc}
  {$mode delphi}
{$endif}

interface

uses
  SysUtils,
  Classes,
  Generics.Collections,
  Nest4D.resilience.interfaces,
  Nest4D.request.data;

type
  // Estrutura para cache de respostas
  TCachedResponse = record
    Response: string;
    ExpirationTime: TDateTime;
    CreatedAt: TDateTime;
    HitCount: Integer;
  end;

  // Estrutura para endpoints de fallback
  TFallbackEndpoint = record
    OriginalEndpoint: string;
    FallbackEndpoint: string;
    Priority: Integer;
    IsActive: Boolean;
    LastUsed: TDateTime;
    SuccessCount: Integer;
    FailureCount: Integer;
  end;

  // Implementa??o do servi?o de fallback
  TFallbackService = class(TInterfacedObject, IFallbackService)
  private
    FFallbackEndpoints: TDictionary<string, TList<TFallbackEndpoint>>;
    FCachedResponses: TDictionary<string, TCachedResponse>;
    FDefaultResponses: TDictionary<string, string>;
    FFallbackCallback: TFallbackCallback;
    FCriticalSection: TRTLCriticalSection;
    FCleanupInterval: Cardinal;
    FLastCleanup: TDateTime;
    FMaxCacheSize: Integer;
    FDefaultCacheTTL: Cardinal;

    procedure InitializeCriticalSection;
    procedure FinalizeCriticalSection;
    procedure Lock;
    procedure Unlock;

    function GetCacheKey(const AEndpoint: string): string;
    function IsResponseExpired(const ACachedResponse: TCachedResponse): Boolean;
    procedure CleanupExpiredCacheInternal;
    procedure EnforceCacheLimit;

    function TryFallbackEndpoints(const AOriginalEndpoint: string;
      const ARequestData: TRequestData): TFallbackResult;
    function TryFallbackCache(const AEndpoint: string): TFallbackResult;
    function TryDefaultResponse(const AEndpoint: string): TFallbackResult;
    function TryCustomCallback(const AException: Exception;
      const ARequestData: TRequestData): TFallbackResult;

    procedure UpdateEndpointStats(const AEndpoint: string; const ASuccess: Boolean);
  public
    constructor Create;
    destructor Destroy; override;

    // IFallbackService implementation
    procedure AddFallbackEndpoint(const AOriginalEndpoint, AFallbackEndpoint: string);
    procedure SetCachedResponse(const AEndpoint, AResponse: string; const AExpirationTime: TDateTime);
    procedure SetDefaultResponse(const AEndpoint, AResponse: string);
    procedure SetFallbackCallback(const ACallback: TFallbackCallback);

    function ExecuteFallback(const AException: Exception; const ARequestData: TRequestData): TFallbackResult;
    function HasFallback(const AEndpoint: string): Boolean;

    procedure ClearExpiredCache;
    procedure ClearAllCache;

    // M?todos adicionais
    procedure AddFallbackEndpointWithPriority(const AOriginalEndpoint, AFallbackEndpoint: string; const APriority: Integer);
    procedure RemoveFallbackEndpoint(const AOriginalEndpoint, AFallbackEndpoint: string);
    procedure SetEndpointActive(const AOriginalEndpoint, AFallbackEndpoint: string; const AActive: Boolean);

    function GetFallbackEndpoints(const AOriginalEndpoint: string): TArray<TFallbackEndpoint>;
    function GetCacheStats: string;
    function GetFallbackStats(const AEndpoint: string): string;

    // Configura??es
    procedure SetCleanupInterval(const AIntervalMs: Cardinal);
    procedure SetMaxCacheSize(const AMaxSize: Integer);
    procedure SetDefaultCacheTTL(const ATTLMs: Cardinal);

    property CleanupInterval: Cardinal read FCleanupInterval write SetCleanupInterval;
    property MaxCacheSize: Integer read FMaxCacheSize write SetMaxCacheSize;
    property DefaultCacheTTL: Cardinal read FDefaultCacheTTL write SetDefaultCacheTTL;
  end;

  // Factory para criar servi?os de fallback pr?-configurados
  TFallbackServiceFactory = class
  public
    class function CreateDefault: IFallbackService;
    class function CreateWithCache(const AMaxCacheSize: Integer; const ADefaultTTL: Cardinal): IFallbackService;
    class function CreateMinimal: IFallbackService;
  end;

implementation

uses
  DateUtils,
  Nest4D.logging;

{ TFallbackService }

constructor TFallbackService.Create;
begin
  inherited Create;

  FFallbackEndpoints := TDictionary<string, TList<TFallbackEndpoint>>.Create;
  FCachedResponses := TDictionary<string, TCachedResponse>.Create;
  FDefaultResponses := TDictionary<string, string>.Create;

  InitializeCriticalSection;

  FCleanupInterval := 300000; // 5 minutos
  FLastCleanup := Now;
  FMaxCacheSize := 1000;
  FDefaultCacheTTL := 3600000; // 1 hora
  FFallbackCallback := nil;
end;

destructor TFallbackService.Destroy;
var
  LEndpointList: TList<TFallbackEndpoint>;
begin
  Lock;
  try
    // Limpa listas de endpoints de fallback
    for LEndpointList in FFallbackEndpoints.Values do
      LEndpointList.Free;
    FFallbackEndpoints.Free;

    FCachedResponses.Free;
    FDefaultResponses.Free;

    FFallbackCallback := nil;
  finally
    Unlock;
    FinalizeCriticalSection;
  end;

  inherited Destroy;
end;

procedure TFallbackService.InitializeCriticalSection;
begin
  InitializeCriticalSection(FCriticalSection);
end;

procedure TFallbackService.FinalizeCriticalSection;
begin
  DeleteCriticalSection(FCriticalSection);
end;

procedure TFallbackService.Lock;
begin
  EnterCriticalSection(FCriticalSection);
end;

procedure TFallbackService.Unlock;
begin
  LeaveCriticalSection(FCriticalSection);
end;

function TFallbackService.GetCacheKey(const AEndpoint: string): string;
begin
  Result := LowerCase(Trim(AEndpoint));
end;

function TFallbackService.IsResponseExpired(const ACachedResponse: TCachedResponse): Boolean;
begin
  Result := Now > ACachedResponse.ExpirationTime;
end;

procedure TFallbackService.AddFallbackEndpoint(const AOriginalEndpoint, AFallbackEndpoint: string);
begin
  AddFallbackEndpointWithPriority(AOriginalEndpoint, AFallbackEndpoint, 1);
end;

procedure TFallbackService.AddFallbackEndpointWithPriority(const AOriginalEndpoint,
  AFallbackEndpoint: string; const APriority: Integer);
var
  LEndpointList: TList<TFallbackEndpoint>;
  LFallbackEndpoint: TFallbackEndpoint;
  LKey: string;
begin
  Lock;
  try
    LKey := GetCacheKey(AOriginalEndpoint);

    if not FFallbackEndpoints.TryGetValue(LKey, LEndpointList) then
    begin
      LEndpointList := TList<TFallbackEndpoint>.Create;
      FFallbackEndpoints.Add(LKey, LEndpointList);
    end;

    LFallbackEndpoint.OriginalEndpoint := AOriginalEndpoint;
    LFallbackEndpoint.FallbackEndpoint := AFallbackEndpoint;
    LFallbackEndpoint.Priority := APriority;
    LFallbackEndpoint.IsActive := True;
    LFallbackEndpoint.LastUsed := 0;
    LFallbackEndpoint.SuccessCount := 0;
    LFallbackEndpoint.FailureCount := 0;

    LEndpointList.Add(LFallbackEndpoint);

    TNest4DLogger.Info(Format('Fallback endpoint adicionado: %s -> %s (prioridade: %d)',
      [AOriginalEndpoint, AFallbackEndpoint, APriority]));
  finally
    Unlock;
  end;
end;

procedure TFallbackService.RemoveFallbackEndpoint(const AOriginalEndpoint, AFallbackEndpoint: string);
var
  LEndpointList: TList<TFallbackEndpoint>;
  LKey: string;
  I: Integer;
begin
  Lock;
  try
    LKey := GetCacheKey(AOriginalEndpoint);

    if FFallbackEndpoints.TryGetValue(LKey, LEndpointList) then
    begin
      for I := LEndpointList.Count - 1 downto 0 do
      begin
        if SameText(LEndpointList[I].FallbackEndpoint, AFallbackEndpoint) then
        begin
          LEndpointList.Delete(I);
          TNest4DLogger.Info(Format('Fallback endpoint removido: %s -> %s',
            [AOriginalEndpoint, AFallbackEndpoint]));
          Break;
        end;
      end;

      if LEndpointList.Count = 0 then
      begin
        LEndpointList.Free;
        FFallbackEndpoints.Remove(LKey);
      end;
    end;
  finally
    Unlock;
  end;
end;

procedure TFallbackService.SetCachedResponse(const AEndpoint, AResponse: string;
  const AExpirationTime: TDateTime);
var
  LCachedResponse: TCachedResponse;
  LKey: string;
begin
  Lock;
  try
    LKey := GetCacheKey(AEndpoint);

    LCachedResponse.Response := AResponse;
    LCachedResponse.ExpirationTime := AExpirationTime;
    LCachedResponse.CreatedAt := Now;
    LCachedResponse.HitCount := 0;

    FCachedResponses.AddOrSetValue(LKey, LCachedResponse);

    EnforceCacheLimit;

    TNest4DLogger.Info(Format('Resposta em cache definida para endpoint: %s (expira em: %s)',
      [AEndpoint, DateTimeToStr(AExpirationTime)]));
  finally
    Unlock;
  end;
end;

procedure TFallbackService.SetDefaultResponse(const AEndpoint, AResponse: string);
var
  LKey: string;
begin
  Lock;
  try
    LKey := GetCacheKey(AEndpoint);
    FDefaultResponses.AddOrSetValue(LKey, AResponse);

    TNest4DLogger.Info(Format('Resposta padr?o definida para endpoint: %s', [AEndpoint]));
  finally
    Unlock;
  end;
end;

procedure TFallbackService.SetFallbackCallback(const ACallback: TFallbackCallback);
begin
  Lock;
  try
    FFallbackCallback := ACallback;
  finally
    Unlock;
  end;
end;

function TFallbackService.ExecuteFallback(const AException: Exception;
  const ARequestData: TRequestData): TFallbackResult;
begin
  TNest4DLogger.Info(Format('Executando fallback para endpoint: %s', [ARequestData.Endpoint]));

  // Verifica se ? hora de fazer limpeza do cache
  if MilliSecondsBetween(Now, FLastCleanup) > FCleanupInterval then
    ClearExpiredCache;

  // Tenta fallback endpoints alternativos primeiro
  Result := TryFallbackEndpoints(ARequestData.Endpoint, ARequestData);
  if Result.Success then
    Exit;

  // Tenta resposta em cache
  Result := TryFallbackCache(ARequestData.Endpoint);
  if Result.Success then
    Exit;

  // Tenta resposta padr?o
  Result := TryDefaultResponse(ARequestData.Endpoint);
  if Result.Success then
    Exit;

  // Tenta callback personalizado
  Result := TryCustomCallback(AException, ARequestData);
end;

function TFallbackService.TryFallbackEndpoints(const AOriginalEndpoint: string;
  const ARequestData: TRequestData): TFallbackResult;
var
  LEndpointList: TList<TFallbackEndpoint>;
  LFallbackEndpoint: TFallbackEndpoint;
  LKey: string;
  I: Integer;
  LStartTime: Cardinal;
begin
  Result.Success := False;
  Result.Response := '';
  Result.FallbackType := ftAlternativeEndpoint;

  Lock;
  try
    LKey := GetCacheKey(AOriginalEndpoint);

    if FFallbackEndpoints.TryGetValue(LKey, LEndpointList) then
    begin
      // Ordena por prioridade (menor n?mero = maior prioridade)
      LEndpointList.Sort(TComparer<TFallbackEndpoint>.Construct(
        function(const Left, Right: TFallbackEndpoint): Integer
        begin
          Result := Left.Priority - Right.Priority;
        end));

      for I := 0 to LEndpointList.Count - 1 do
      begin
        LFallbackEndpoint := LEndpointList[I];

        if not LFallbackEndpoint.IsActive then
          Continue;

        LStartTime := GetTickCount;

        try
          // Aqui seria feita a chamada para o endpoint de fallback
          // Por enquanto, simula sucesso para demonstra??o
          TNest4DLogger.Info(Format('Tentando fallback endpoint: %s', [LFallbackEndpoint.FallbackEndpoint]));

          // Simula execu??o (em implementa??o real, usaria o adapter)
          Result.Success := True;
          Result.Response := Format('{"fallback": true, "endpoint": "%s", "timestamp": "%s"}',
            [LFallbackEndpoint.FallbackEndpoint, DateTimeToStr(Now)]);
          Result.ExecutionTime := GetTickCount - LStartTime;

          // Atualiza estat?sticas
          UpdateEndpointStats(LFallbackEndpoint.FallbackEndpoint, True);

          TNest4DLogger.Info(Format('Fallback endpoint bem-sucedido: %s', [LFallbackEndpoint.FallbackEndpoint]));
          Exit;

        except
          on E: Exception do
          begin
            UpdateEndpointStats(LFallbackEndpoint.FallbackEndpoint, False);
            TNest4DLogger.Error(Format('Fallback endpoint falhou: %s - %s',
              [LFallbackEndpoint.FallbackEndpoint, E.Message]));
          end;
        end;
      end;
    end;
  finally
    Unlock;
  end;
end;

function TFallbackService.TryFallbackCache(const AEndpoint: string): TFallbackResult;
var
  LCachedResponse: TCachedResponse;
  LKey: string;
begin
  Result.Success := False;
  Result.Response := '';
  Result.FallbackType := ftCachedResponse;
  Result.ExecutionTime := 0;

  Lock;
  try
    LKey := GetCacheKey(AEndpoint);

    if FCachedResponses.TryGetValue(LKey, LCachedResponse) then
    begin
      if not IsResponseExpired(LCachedResponse) then
      begin
        Result.Success := True;
        Result.Response := LCachedResponse.Response;

        // Incrementa contador de hits
        Inc(LCachedResponse.HitCount);
        FCachedResponses.AddOrSetValue(LKey, LCachedResponse);

        TNest4DLogger.Info(Format('Resposta em cache utilizada para endpoint: %s', [AEndpoint]));
      end
      else
      begin
        // Remove resposta expirada
        FCachedResponses.Remove(LKey);
        TNest4DLogger.Info(Format('Resposta em cache expirada removida para endpoint: %s', [AEndpoint]));
      end;
    end;
  finally
    Unlock;
  end;
end;

function TFallbackService.TryDefaultResponse(const AEndpoint: string): TFallbackResult;
var
  LDefaultResponse: string;
  LKey: string;
begin
  Result.Success := False;
  Result.Response := '';
  Result.FallbackType := ftDefaultResponse;
  Result.ExecutionTime := 0;

  Lock;
  try
    LKey := GetCacheKey(AEndpoint);

    if FDefaultResponses.TryGetValue(LKey, LDefaultResponse) then
    begin
      Result.Success := True;
      Result.Response := LDefaultResponse;

      TNest4DLogger.Info(Format('Resposta padr?o utilizada para endpoint: %s', [AEndpoint]));
    end;
  finally
    Unlock;
  end;
end;

function TFallbackService.TryCustomCallback(const AException: Exception;
  const ARequestData: TRequestData): TFallbackResult;
begin
  Result.Success := False;
  Result.Response := '';
  Result.FallbackType := ftCustomHandler;
  Result.ExecutionTime := 0;

  Lock;
  try
    if Assigned(FFallbackCallback) then
    begin
      try
        Result := FFallbackCallback(AException, ARequestData);

        if Result.Success then
          TNest4DLogger.Info(Format('Callback personalizado bem-sucedido para endpoint: %s', [ARequestData.Endpoint]))
        else
          TNest4DLogger.Info(Format('Callback personalizado falhou para endpoint: %s', [ARequestData.Endpoint]));

      except
        on E: Exception do
        begin
          TNest4DLogger.Error(Format('Erro no callback personalizado: %s', [E.Message]));
          Result.Success := False;
        end;
      end;
    end;
  finally
    Unlock;
  end;
end;

procedure TFallbackService.UpdateEndpointStats(const AEndpoint: string; const ASuccess: Boolean);
var
  LEndpointList: TList<TFallbackEndpoint>;
  LFallbackEndpoint: TFallbackEndpoint;
  I, J: Integer;
begin
  // Atualiza estat?sticas do endpoint em todas as listas
  for LEndpointList in FFallbackEndpoints.Values do
  begin
    for I := 0 to LEndpointList.Count - 1 do
    begin
      LFallbackEndpoint := LEndpointList[I];

      if SameText(LFallbackEndpoint.FallbackEndpoint, AEndpoint) then
      begin
        LFallbackEndpoint.LastUsed := Now;

        if ASuccess then
          Inc(LFallbackEndpoint.SuccessCount)
        else
          Inc(LFallbackEndpoint.FailureCount);

        LEndpointList[I] := LFallbackEndpoint;
      end;
    end;
  end;
end;

function TFallbackService.HasFallback(const AEndpoint: string): Boolean;
var
  LKey: string;
begin
  Lock;
  try
    LKey := GetCacheKey(AEndpoint);

    Result := FFallbackEndpoints.ContainsKey(LKey) or
              FCachedResponses.ContainsKey(LKey) or
              FDefaultResponses.ContainsKey(LKey) or
              Assigned(FFallbackCallback);
  finally
    Unlock;
  end;
end;

procedure TFallbackService.ClearExpiredCache;
var
  LKeysToRemove: TArray<string>;
  LKey: string;
  LCachedResponse: TCachedResponse;
begin
  Lock;
  try
    CleanupExpiredCacheInternal;
    FLastCleanup := Now;
  finally
    Unlock;
  end;
end;

procedure TFallbackService.CleanupExpiredCacheInternal;
var
  LKeysToRemove: TList<string>;
  LKey: string;
  LCachedResponse: TCachedResponse;
begin
  LKeysToRemove := TList<string>.Create;
  try
    for LKey in FCachedResponses.Keys do
    begin
      if FCachedResponses.TryGetValue(LKey, LCachedResponse) then
      begin
        if IsResponseExpired(LCachedResponse) then
          LKeysToRemove.Add(LKey);
      end;
    end;

    for LKey in LKeysToRemove do
    begin
      FCachedResponses.Remove(LKey);
      TNest4DLogger.Info(Format('Resposta em cache expirada removida: %s', [LKey]));
    end;

    if LKeysToRemove.Count > 0 then
      TNest4DLogger.Info(Format('Limpeza de cache conclu?da: %d entradas removidas', [LKeysToRemove.Count]));

  finally
    LKeysToRemove.Free;
  end;
end;

procedure TFallbackService.EnforceCacheLimit;
var
  LCacheEntries: TList<TPair<string, TCachedResponse>>;
  LPair: TPair<string, TCachedResponse>;
  I: Integer;
begin
  if FCachedResponses.Count <= FMaxCacheSize then
    Exit;

  LCacheEntries := TList<TPair<string, TCachedResponse>>.Create;
  try
    for LPair in FCachedResponses do
      LCacheEntries.Add(LPair);

    // Ordena por data de cria??o (mais antigas primeiro)
    LCacheEntries.Sort(TComparer<TPair<string, TCachedResponse>>.Construct(
      function(const Left, Right: TPair<string, TCachedResponse>): Integer
      begin
        Result := CompareDateTime(Left.Value.CreatedAt, Right.Value.CreatedAt);
      end));

    // Remove entradas mais antigas at? atingir o limite
    I := 0;
    while (FCachedResponses.Count > FMaxCacheSize) and (I < LCacheEntries.Count) do
    begin
      FCachedResponses.Remove(LCacheEntries[I].Key);
      Inc(I);
    end;

    if I > 0 then
      TNest4DLogger.Info(Format('Limite de cache aplicado: %d entradas antigas removidas', [I]));

  finally
    LCacheEntries.Free;
  end;
end;

procedure TFallbackService.ClearAllCache;
begin
  Lock;
  try
    FCachedResponses.Clear;
    TNest4DLogger.Info('Todo o cache de fallback foi limpo');
  finally
    Unlock;
  end;
end;

procedure TFallbackService.SetEndpointActive(const AOriginalEndpoint, AFallbackEndpoint: string;
  const AActive: Boolean);
var
  LEndpointList: TList<TFallbackEndpoint>;
  LFallbackEndpoint: TFallbackEndpoint;
  LKey: string;
  I: Integer;
begin
  Lock;
  try
    LKey := GetCacheKey(AOriginalEndpoint);

    if FFallbackEndpoints.TryGetValue(LKey, LEndpointList) then
    begin
      for I := 0 to LEndpointList.Count - 1 do
      begin
        LFallbackEndpoint := LEndpointList[I];

        if SameText(LFallbackEndpoint.FallbackEndpoint, AFallbackEndpoint) then
        begin
          LFallbackEndpoint.IsActive := AActive;
          LEndpointList[I] := LFallbackEndpoint;

          TNest4DLogger.Info(Format('Fallback endpoint %s: %s -> %s',
            [IfThen(AActive, 'ativado', 'desativado'), AOriginalEndpoint, AFallbackEndpoint]));
          Break;
        end;
      end;
    end;
  finally
    Unlock;
  end;
end;

function TFallbackService.GetFallbackEndpoints(const AOriginalEndpoint: string): TArray<TFallbackEndpoint>;
var
  LEndpointList: TList<TFallbackEndpoint>;
  LKey: string;
begin
  Lock;
  try
    LKey := GetCacheKey(AOriginalEndpoint);

    if FFallbackEndpoints.TryGetValue(LKey, LEndpointList) then
      Result := LEndpointList.ToArray
    else
      SetLength(Result, 0);
  finally
    Unlock;
  end;
end;

function TFallbackService.GetCacheStats: string;
var
  LTotalEntries, LExpiredEntries: Integer;
  LCachedResponse: TCachedResponse;
begin
  Lock;
  try
    LTotalEntries := FCachedResponses.Count;
    LExpiredEntries := 0;

    for LCachedResponse in FCachedResponses.Values do
    begin
      if IsResponseExpired(LCachedResponse) then
        Inc(LExpiredEntries);
    end;

    Result := Format('Cache Stats - Total: %d, Expiradas: %d, Ativas: %d, Limite: %d',
      [LTotalEntries, LExpiredEntries, LTotalEntries - LExpiredEntries, FMaxCacheSize]);
  finally
    Unlock;
  end;
end;

function TFallbackService.GetFallbackStats(const AEndpoint: string): string;
var
  LEndpoints: TArray<TFallbackEndpoint>;
  LEndpoint: TFallbackEndpoint;
  LStats: TStringBuilder;
begin
  LStats := TStringBuilder.Create;
  try
    LEndpoints := GetFallbackEndpoints(AEndpoint);

    LStats.AppendFormat('Fallback Stats para %s:%s', [AEndpoint, sLineBreak]);

    for LEndpoint in LEndpoints do
    begin
      LStats.AppendFormat('  %s (Prioridade: %d, Ativo: %s, Sucessos: %d, Falhas: %d)%s',
        [LEndpoint.FallbackEndpoint, LEndpoint.Priority,
         IfThen(LEndpoint.IsActive, 'Sim', 'N?o'),
         LEndpoint.SuccessCount, LEndpoint.FailureCount, sLineBreak]);
    end;

    if Length(LEndpoints) = 0 then
      LStats.Append('  Nenhum endpoint de fallback configurado');

    Result := LStats.ToString;
  finally
    LStats.Free;
  end;
end;

procedure TFallbackService.SetCleanupInterval(const AIntervalMs: Cardinal);
begin
  FCleanupInterval := AIntervalMs;
end;

procedure TFallbackService.SetMaxCacheSize(const AMaxSize: Integer);
begin
  if AMaxSize > 0 then
  begin
    FMaxCacheSize := AMaxSize;
    Lock;
    try
      EnforceCacheLimit;
    finally
      Unlock;
    end;
  end;
end;

procedure TFallbackService.SetDefaultCacheTTL(const ATTLMs: Cardinal);
begin
  FDefaultCacheTTL := ATTLMs;
end;

{ TFallbackServiceFactory }

class function TFallbackServiceFactory.CreateDefault: IFallbackService;
var
  LService: TFallbackService;
begin
  LService := TFallbackService.Create;
  LService.SetMaxCacheSize(1000);
  LService.SetDefaultCacheTTL(3600000); // 1 hora
  LService.SetCleanupInterval(300000);  // 5 minutos
  Result := LService;
end;

class function TFallbackServiceFactory.CreateWithCache(const AMaxCacheSize: Integer;
  const ADefaultTTL: Cardinal): IFallbackService;
var
  LService: TFallbackService;
begin
  LService := TFallbackService.Create;
  LService.SetMaxCacheSize(AMaxCacheSize);
  LService.SetDefaultCacheTTL(ADefaultTTL);
  LService.SetCleanupInterval(ADefaultTTL div 12); // Cleanup a cada 1/12 do TTL
  Result := LService;
end;

class function TFallbackServiceFactory.CreateMinimal: IFallbackService;
var
  LService: TFallbackService;
begin
  LService := TFallbackService.Create;
  LService.SetMaxCacheSize(100);
  LService.SetDefaultCacheTTL(600000);  // 10 minutos
  LService.SetCleanupInterval(120000);  // 2 minutos
  Result := LService;
end;

end..Substring(1).ToLower() } }; $camelUnit = $camelParts -join '.'; return $prefix + $camelUnit + $suffix

{$ifdef fpc}
  {$mode delphi}
{$endif}

interface

uses
  SysUtils,
  Classes,
  Generics.Collections,
  Nest4D.resilience.interfaces,
  Nest4D.request.data;

type
  // Estrutura para cache de respostas
  TCachedResponse = record
    Response: string;
    ExpirationTime: TDateTime;
    CreatedAt: TDateTime;
    HitCount: Integer;
  end;

  // Estrutura para endpoints de fallback
  TFallbackEndpoint = record
    OriginalEndpoint: string;
    FallbackEndpoint: string;
    Priority: Integer;
    IsActive: Boolean;
    LastUsed: TDateTime;
    SuccessCount: Integer;
    FailureCount: Integer;
  end;

  // Implementa??o do servi?o de fallback
  TFallbackService = class(TInterfacedObject, IFallbackService)
  private
    FFallbackEndpoints: TDictionary<string, TList<TFallbackEndpoint>>;
    FCachedResponses: TDictionary<string, TCachedResponse>;
    FDefaultResponses: TDictionary<string, string>;
    FFallbackCallback: TFallbackCallback;
    FCriticalSection: TRTLCriticalSection;
    FCleanupInterval: Cardinal;
    FLastCleanup: TDateTime;
    FMaxCacheSize: Integer;
    FDefaultCacheTTL: Cardinal;

    procedure InitializeCriticalSection;
    procedure FinalizeCriticalSection;
    procedure Lock;
    procedure Unlock;

    function GetCacheKey(const AEndpoint: string): string;
    function IsResponseExpired(const ACachedResponse: TCachedResponse): Boolean;
    procedure CleanupExpiredCacheInternal;
    procedure EnforceCacheLimit;

    function TryFallbackEndpoints(const AOriginalEndpoint: string;
      const ARequestData: TRequestData): TFallbackResult;
    function TryFallbackCache(const AEndpoint: string): TFallbackResult;
    function TryDefaultResponse(const AEndpoint: string): TFallbackResult;
    function TryCustomCallback(const AException: Exception;
      const ARequestData: TRequestData): TFallbackResult;

    procedure UpdateEndpointStats(const AEndpoint: string; const ASuccess: Boolean);
  public
    constructor Create;
    destructor Destroy; override;

    // IFallbackService implementation
    procedure AddFallbackEndpoint(const AOriginalEndpoint, AFallbackEndpoint: string);
    procedure SetCachedResponse(const AEndpoint, AResponse: string; const AExpirationTime: TDateTime);
    procedure SetDefaultResponse(const AEndpoint, AResponse: string);
    procedure SetFallbackCallback(const ACallback: TFallbackCallback);

    function ExecuteFallback(const AException: Exception; const ARequestData: TRequestData): TFallbackResult;
    function HasFallback(const AEndpoint: string): Boolean;

    procedure ClearExpiredCache;
    procedure ClearAllCache;

    // M?todos adicionais
    procedure AddFallbackEndpointWithPriority(const AOriginalEndpoint, AFallbackEndpoint: string; const APriority: Integer);
    procedure RemoveFallbackEndpoint(const AOriginalEndpoint, AFallbackEndpoint: string);
    procedure SetEndpointActive(const AOriginalEndpoint, AFallbackEndpoint: string; const AActive: Boolean);

    function GetFallbackEndpoints(const AOriginalEndpoint: string): TArray<TFallbackEndpoint>;
    function GetCacheStats: string;
    function GetFallbackStats(const AEndpoint: string): string;

    // Configura??es
    procedure SetCleanupInterval(const AIntervalMs: Cardinal);
    procedure SetMaxCacheSize(const AMaxSize: Integer);
    procedure SetDefaultCacheTTL(const ATTLMs: Cardinal);

    property CleanupInterval: Cardinal read FCleanupInterval write SetCleanupInterval;
    property MaxCacheSize: Integer read FMaxCacheSize write SetMaxCacheSize;
    property DefaultCacheTTL: Cardinal read FDefaultCacheTTL write SetDefaultCacheTTL;
  end;

  // Factory para criar servi?os de fallback pr?-configurados
  TFallbackServiceFactory = class
  public
    class function CreateDefault: IFallbackService;
    class function CreateWithCache(const AMaxCacheSize: Integer; const ADefaultTTL: Cardinal): IFallbackService;
    class function CreateMinimal: IFallbackService;
  end;

implementation

uses
  DateUtils,
  Nest4D.logging;

{ TFallbackService }

constructor TFallbackService.Create;
begin
  inherited Create;

  FFallbackEndpoints := TDictionary<string, TList<TFallbackEndpoint>>.Create;
  FCachedResponses := TDictionary<string, TCachedResponse>.Create;
  FDefaultResponses := TDictionary<string, string>.Create;

  InitializeCriticalSection;

  FCleanupInterval := 300000; // 5 minutos
  FLastCleanup := Now;
  FMaxCacheSize := 1000;
  FDefaultCacheTTL := 3600000; // 1 hora
  FFallbackCallback := nil;
end;

destructor TFallbackService.Destroy;
var
  LEndpointList: TList<TFallbackEndpoint>;
begin
  Lock;
  try
    // Limpa listas de endpoints de fallback
    for LEndpointList in FFallbackEndpoints.Values do
      LEndpointList.Free;
    FFallbackEndpoints.Free;

    FCachedResponses.Free;
    FDefaultResponses.Free;

    FFallbackCallback := nil;
  finally
    Unlock;
    FinalizeCriticalSection;
  end;

  inherited Destroy;
end;

procedure TFallbackService.InitializeCriticalSection;
begin
  InitializeCriticalSection(FCriticalSection);
end;

procedure TFallbackService.FinalizeCriticalSection;
begin
  DeleteCriticalSection(FCriticalSection);
end;

procedure TFallbackService.Lock;
begin
  EnterCriticalSection(FCriticalSection);
end;

procedure TFallbackService.Unlock;
begin
  LeaveCriticalSection(FCriticalSection);
end;

function TFallbackService.GetCacheKey(const AEndpoint: string): string;
begin
  Result := LowerCase(Trim(AEndpoint));
end;

function TFallbackService.IsResponseExpired(const ACachedResponse: TCachedResponse): Boolean;
begin
  Result := Now > ACachedResponse.ExpirationTime;
end;

procedure TFallbackService.AddFallbackEndpoint(const AOriginalEndpoint, AFallbackEndpoint: string);
begin
  AddFallbackEndpointWithPriority(AOriginalEndpoint, AFallbackEndpoint, 1);
end;

procedure TFallbackService.AddFallbackEndpointWithPriority(const AOriginalEndpoint,
  AFallbackEndpoint: string; const APriority: Integer);
var
  LEndpointList: TList<TFallbackEndpoint>;
  LFallbackEndpoint: TFallbackEndpoint;
  LKey: string;
begin
  Lock;
  try
    LKey := GetCacheKey(AOriginalEndpoint);

    if not FFallbackEndpoints.TryGetValue(LKey, LEndpointList) then
    begin
      LEndpointList := TList<TFallbackEndpoint>.Create;
      FFallbackEndpoints.Add(LKey, LEndpointList);
    end;

    LFallbackEndpoint.OriginalEndpoint := AOriginalEndpoint;
    LFallbackEndpoint.FallbackEndpoint := AFallbackEndpoint;
    LFallbackEndpoint.Priority := APriority;
    LFallbackEndpoint.IsActive := True;
    LFallbackEndpoint.LastUsed := 0;
    LFallbackEndpoint.SuccessCount := 0;
    LFallbackEndpoint.FailureCount := 0;

    LEndpointList.Add(LFallbackEndpoint);

    TNest4DLogger.Info(Format('Fallback endpoint adicionado: %s -> %s (prioridade: %d)',
      [AOriginalEndpoint, AFallbackEndpoint, APriority]));
  finally
    Unlock;
  end;
end;

procedure TFallbackService.RemoveFallbackEndpoint(const AOriginalEndpoint, AFallbackEndpoint: string);
var
  LEndpointList: TList<TFallbackEndpoint>;
  LKey: string;
  I: Integer;
begin
  Lock;
  try
    LKey := GetCacheKey(AOriginalEndpoint);

    if FFallbackEndpoints.TryGetValue(LKey, LEndpointList) then
    begin
      for I := LEndpointList.Count - 1 downto 0 do
      begin
        if SameText(LEndpointList[I].FallbackEndpoint, AFallbackEndpoint) then
        begin
          LEndpointList.Delete(I);
          TNest4DLogger.Info(Format('Fallback endpoint removido: %s -> %s',
            [AOriginalEndpoint, AFallbackEndpoint]));
          Break;
        end;
      end;

      if LEndpointList.Count = 0 then
      begin
        LEndpointList.Free;
        FFallbackEndpoints.Remove(LKey);
      end;
    end;
  finally
    Unlock;
  end;
end;

procedure TFallbackService.SetCachedResponse(const AEndpoint, AResponse: string;
  const AExpirationTime: TDateTime);
var
  LCachedResponse: TCachedResponse;
  LKey: string;
begin
  Lock;
  try
    LKey := GetCacheKey(AEndpoint);

    LCachedResponse.Response := AResponse;
    LCachedResponse.ExpirationTime := AExpirationTime;
    LCachedResponse.CreatedAt := Now;
    LCachedResponse.HitCount := 0;

    FCachedResponses.AddOrSetValue(LKey, LCachedResponse);

    EnforceCacheLimit;

    TNest4DLogger.Info(Format('Resposta em cache definida para endpoint: %s (expira em: %s)',
      [AEndpoint, DateTimeToStr(AExpirationTime)]));
  finally
    Unlock;
  end;
end;

procedure TFallbackService.SetDefaultResponse(const AEndpoint, AResponse: string);
var
  LKey: string;
begin
  Lock;
  try
    LKey := GetCacheKey(AEndpoint);
    FDefaultResponses.AddOrSetValue(LKey, AResponse);

    TNest4DLogger.Info(Format('Resposta padr?o definida para endpoint: %s', [AEndpoint]));
  finally
    Unlock;
  end;
end;

procedure TFallbackService.SetFallbackCallback(const ACallback: TFallbackCallback);
begin
  Lock;
  try
    FFallbackCallback := ACallback;
  finally
    Unlock;
  end;
end;

function TFallbackService.ExecuteFallback(const AException: Exception;
  const ARequestData: TRequestData): TFallbackResult;
begin
  TNest4DLogger.Info(Format('Executando fallback para endpoint: %s', [ARequestData.Endpoint]));

  // Verifica se ? hora de fazer limpeza do cache
  if MilliSecondsBetween(Now, FLastCleanup) > FCleanupInterval then
    ClearExpiredCache;

  // Tenta fallback endpoints alternativos primeiro
  Result := TryFallbackEndpoints(ARequestData.Endpoint, ARequestData);
  if Result.Success then
    Exit;

  // Tenta resposta em cache
  Result := TryFallbackCache(ARequestData.Endpoint);
  if Result.Success then
    Exit;

  // Tenta resposta padr?o
  Result := TryDefaultResponse(ARequestData.Endpoint);
  if Result.Success then
    Exit;

  // Tenta callback personalizado
  Result := TryCustomCallback(AException, ARequestData);
end;

function TFallbackService.TryFallbackEndpoints(const AOriginalEndpoint: string;
  const ARequestData: TRequestData): TFallbackResult;
var
  LEndpointList: TList<TFallbackEndpoint>;
  LFallbackEndpoint: TFallbackEndpoint;
  LKey: string;
  I: Integer;
  LStartTime: Cardinal;
begin
  Result.Success := False;
  Result.Response := '';
  Result.FallbackType := ftAlternativeEndpoint;

  Lock;
  try
    LKey := GetCacheKey(AOriginalEndpoint);

    if FFallbackEndpoints.TryGetValue(LKey, LEndpointList) then
    begin
      // Ordena por prioridade (menor n?mero = maior prioridade)
      LEndpointList.Sort(TComparer<TFallbackEndpoint>.Construct(
        function(const Left, Right: TFallbackEndpoint): Integer
        begin
          Result := Left.Priority - Right.Priority;
        end));

      for I := 0 to LEndpointList.Count - 1 do
      begin
        LFallbackEndpoint := LEndpointList[I];

        if not LFallbackEndpoint.IsActive then
          Continue;

        LStartTime := GetTickCount;

        try
          // Aqui seria feita a chamada para o endpoint de fallback
          // Por enquanto, simula sucesso para demonstra??o
          TNest4DLogger.Info(Format('Tentando fallback endpoint: %s', [LFallbackEndpoint.FallbackEndpoint]));

          // Simula execu??o (em implementa??o real, usaria o adapter)
          Result.Success := True;
          Result.Response := Format('{"fallback": true, "endpoint": "%s", "timestamp": "%s"}',
            [LFallbackEndpoint.FallbackEndpoint, DateTimeToStr(Now)]);
          Result.ExecutionTime := GetTickCount - LStartTime;

          // Atualiza estat?sticas
          UpdateEndpointStats(LFallbackEndpoint.FallbackEndpoint, True);

          TNest4DLogger.Info(Format('Fallback endpoint bem-sucedido: %s', [LFallbackEndpoint.FallbackEndpoint]));
          Exit;

        except
          on E: Exception do
          begin
            UpdateEndpointStats(LFallbackEndpoint.FallbackEndpoint, False);
            TNest4DLogger.Error(Format('Fallback endpoint falhou: %s - %s',
              [LFallbackEndpoint.FallbackEndpoint, E.Message]));
          end;
        end;
      end;
    end;
  finally
    Unlock;
  end;
end;

function TFallbackService.TryFallbackCache(const AEndpoint: string): TFallbackResult;
var
  LCachedResponse: TCachedResponse;
  LKey: string;
begin
  Result.Success := False;
  Result.Response := '';
  Result.FallbackType := ftCachedResponse;
  Result.ExecutionTime := 0;

  Lock;
  try
    LKey := GetCacheKey(AEndpoint);

    if FCachedResponses.TryGetValue(LKey, LCachedResponse) then
    begin
      if not IsResponseExpired(LCachedResponse) then
      begin
        Result.Success := True;
        Result.Response := LCachedResponse.Response;

        // Incrementa contador de hits
        Inc(LCachedResponse.HitCount);
        FCachedResponses.AddOrSetValue(LKey, LCachedResponse);

        TNest4DLogger.Info(Format('Resposta em cache utilizada para endpoint: %s', [AEndpoint]));
      end
      else
      begin
        // Remove resposta expirada
        FCachedResponses.Remove(LKey);
        TNest4DLogger.Info(Format('Resposta em cache expirada removida para endpoint: %s', [AEndpoint]));
      end;
    end;
  finally
    Unlock;
  end;
end;

function TFallbackService.TryDefaultResponse(const AEndpoint: string): TFallbackResult;
var
  LDefaultResponse: string;
  LKey: string;
begin
  Result.Success := False;
  Result.Response := '';
  Result.FallbackType := ftDefaultResponse;
  Result.ExecutionTime := 0;

  Lock;
  try
    LKey := GetCacheKey(AEndpoint);

    if FDefaultResponses.TryGetValue(LKey, LDefaultResponse) then
    begin
      Result.Success := True;
      Result.Response := LDefaultResponse;

      TNest4DLogger.Info(Format('Resposta padr?o utilizada para endpoint: %s', [AEndpoint]));
    end;
  finally
    Unlock;
  end;
end;

function TFallbackService.TryCustomCallback(const AException: Exception;
  const ARequestData: TRequestData): TFallbackResult;
begin
  Result.Success := False;
  Result.Response := '';
  Result.FallbackType := ftCustomHandler;
  Result.ExecutionTime := 0;

  Lock;
  try
    if Assigned(FFallbackCallback) then
    begin
      try
        Result := FFallbackCallback(AException, ARequestData);

        if Result.Success then
          TNest4DLogger.Info(Format('Callback personalizado bem-sucedido para endpoint: %s', [ARequestData.Endpoint]))
        else
          TNest4DLogger.Info(Format('Callback personalizado falhou para endpoint: %s', [ARequestData.Endpoint]));

      except
        on E: Exception do
        begin
          TNest4DLogger.Error(Format('Erro no callback personalizado: %s', [E.Message]));
          Result.Success := False;
        end;
      end;
    end;
  finally
    Unlock;
  end;
end;

procedure TFallbackService.UpdateEndpointStats(const AEndpoint: string; const ASuccess: Boolean);
var
  LEndpointList: TList<TFallbackEndpoint>;
  LFallbackEndpoint: TFallbackEndpoint;
  I, J: Integer;
begin
  // Atualiza estat?sticas do endpoint em todas as listas
  for LEndpointList in FFallbackEndpoints.Values do
  begin
    for I := 0 to LEndpointList.Count - 1 do
    begin
      LFallbackEndpoint := LEndpointList[I];

      if SameText(LFallbackEndpoint.FallbackEndpoint, AEndpoint) then
      begin
        LFallbackEndpoint.LastUsed := Now;

        if ASuccess then
          Inc(LFallbackEndpoint.SuccessCount)
        else
          Inc(LFallbackEndpoint.FailureCount);

        LEndpointList[I] := LFallbackEndpoint;
      end;
    end;
  end;
end;

function TFallbackService.HasFallback(const AEndpoint: string): Boolean;
var
  LKey: string;
begin
  Lock;
  try
    LKey := GetCacheKey(AEndpoint);

    Result := FFallbackEndpoints.ContainsKey(LKey) or
              FCachedResponses.ContainsKey(LKey) or
              FDefaultResponses.ContainsKey(LKey) or
              Assigned(FFallbackCallback);
  finally
    Unlock;
  end;
end;

procedure TFallbackService.ClearExpiredCache;
var
  LKeysToRemove: TArray<string>;
  LKey: string;
  LCachedResponse: TCachedResponse;
begin
  Lock;
  try
    CleanupExpiredCacheInternal;
    FLastCleanup := Now;
  finally
    Unlock;
  end;
end;

procedure TFallbackService.CleanupExpiredCacheInternal;
var
  LKeysToRemove: TList<string>;
  LKey: string;
  LCachedResponse: TCachedResponse;
begin
  LKeysToRemove := TList<string>.Create;
  try
    for LKey in FCachedResponses.Keys do
    begin
      if FCachedResponses.TryGetValue(LKey, LCachedResponse) then
      begin
        if IsResponseExpired(LCachedResponse) then
          LKeysToRemove.Add(LKey);
      end;
    end;

    for LKey in LKeysToRemove do
    begin
      FCachedResponses.Remove(LKey);
      TNest4DLogger.Info(Format('Resposta em cache expirada removida: %s', [LKey]));
    end;

    if LKeysToRemove.Count > 0 then
      TNest4DLogger.Info(Format('Limpeza de cache conclu?da: %d entradas removidas', [LKeysToRemove.Count]));

  finally
    LKeysToRemove.Free;
  end;
end;

procedure TFallbackService.EnforceCacheLimit;
var
  LCacheEntries: TList<TPair<string, TCachedResponse>>;
  LPair: TPair<string, TCachedResponse>;
  I: Integer;
begin
  if FCachedResponses.Count <= FMaxCacheSize then
    Exit;

  LCacheEntries := TList<TPair<string, TCachedResponse>>.Create;
  try
    for LPair in FCachedResponses do
      LCacheEntries.Add(LPair);

    // Ordena por data de cria??o (mais antigas primeiro)
    LCacheEntries.Sort(TComparer<TPair<string, TCachedResponse>>.Construct(
      function(const Left, Right: TPair<string, TCachedResponse>): Integer
      begin
        Result := CompareDateTime(Left.Value.CreatedAt, Right.Value.CreatedAt);
      end));

    // Remove entradas mais antigas at? atingir o limite
    I := 0;
    while (FCachedResponses.Count > FMaxCacheSize) and (I < LCacheEntries.Count) do
    begin
      FCachedResponses.Remove(LCacheEntries[I].Key);
      Inc(I);
    end;

    if I > 0 then
      TNest4DLogger.Info(Format('Limite de cache aplicado: %d entradas antigas removidas', [I]));

  finally
    LCacheEntries.Free;
  end;
end;

procedure TFallbackService.ClearAllCache;
begin
  Lock;
  try
    FCachedResponses.Clear;
    TNest4DLogger.Info('Todo o cache de fallback foi limpo');
  finally
    Unlock;
  end;
end;

procedure TFallbackService.SetEndpointActive(const AOriginalEndpoint, AFallbackEndpoint: string;
  const AActive: Boolean);
var
  LEndpointList: TList<TFallbackEndpoint>;
  LFallbackEndpoint: TFallbackEndpoint;
  LKey: string;
  I: Integer;
begin
  Lock;
  try
    LKey := GetCacheKey(AOriginalEndpoint);

    if FFallbackEndpoints.TryGetValue(LKey, LEndpointList) then
    begin
      for I := 0 to LEndpointList.Count - 1 do
      begin
        LFallbackEndpoint := LEndpointList[I];

        if SameText(LFallbackEndpoint.FallbackEndpoint, AFallbackEndpoint) then
        begin
          LFallbackEndpoint.IsActive := AActive;
          LEndpointList[I] := LFallbackEndpoint;

          TNest4DLogger.Info(Format('Fallback endpoint %s: %s -> %s',
            [IfThen(AActive, 'ativado', 'desativado'), AOriginalEndpoint, AFallbackEndpoint]));
          Break;
        end;
      end;
    end;
  finally
    Unlock;
  end;
end;

function TFallbackService.GetFallbackEndpoints(const AOriginalEndpoint: string): TArray<TFallbackEndpoint>;
var
  LEndpointList: TList<TFallbackEndpoint>;
  LKey: string;
begin
  Lock;
  try
    LKey := GetCacheKey(AOriginalEndpoint);

    if FFallbackEndpoints.TryGetValue(LKey, LEndpointList) then
      Result := LEndpointList.ToArray
    else
      SetLength(Result, 0);
  finally
    Unlock;
  end;
end;

function TFallbackService.GetCacheStats: string;
var
  LTotalEntries, LExpiredEntries: Integer;
  LCachedResponse: TCachedResponse;
begin
  Lock;
  try
    LTotalEntries := FCachedResponses.Count;
    LExpiredEntries := 0;

    for LCachedResponse in FCachedResponses.Values do
    begin
      if IsResponseExpired(LCachedResponse) then
        Inc(LExpiredEntries);
    end;

    Result := Format('Cache Stats - Total: %d, Expiradas: %d, Ativas: %d, Limite: %d',
      [LTotalEntries, LExpiredEntries, LTotalEntries - LExpiredEntries, FMaxCacheSize]);
  finally
    Unlock;
  end;
end;

function TFallbackService.GetFallbackStats(const AEndpoint: string): string;
var
  LEndpoints: TArray<TFallbackEndpoint>;
  LEndpoint: TFallbackEndpoint;
  LStats: TStringBuilder;
begin
  LStats := TStringBuilder.Create;
  try
    LEndpoints := GetFallbackEndpoints(AEndpoint);

    LStats.AppendFormat('Fallback Stats para %s:%s', [AEndpoint, sLineBreak]);

    for LEndpoint in LEndpoints do
    begin
      LStats.AppendFormat('  %s (Prioridade: %d, Ativo: %s, Sucessos: %d, Falhas: %d)%s',
        [LEndpoint.FallbackEndpoint, LEndpoint.Priority,
         IfThen(LEndpoint.IsActive, 'Sim', 'N?o'),
         LEndpoint.SuccessCount, LEndpoint.FailureCount, sLineBreak]);
    end;

    if Length(LEndpoints) = 0 then
      LStats.Append('  Nenhum endpoint de fallback configurado');

    Result := LStats.ToString;
  finally
    LStats.Free;
  end;
end;

procedure TFallbackService.SetCleanupInterval(const AIntervalMs: Cardinal);
begin
  FCleanupInterval := AIntervalMs;
end;

procedure TFallbackService.SetMaxCacheSize(const AMaxSize: Integer);
begin
  if AMaxSize > 0 then
  begin
    FMaxCacheSize := AMaxSize;
    Lock;
    try
      EnforceCacheLimit;
    finally
      Unlock;
    end;
  end;
end;

procedure TFallbackService.SetDefaultCacheTTL(const ATTLMs: Cardinal);
begin
  FDefaultCacheTTL := ATTLMs;
end;

{ TFallbackServiceFactory }

class function TFallbackServiceFactory.CreateDefault: IFallbackService;
var
  LService: TFallbackService;
begin
  LService := TFallbackService.Create;
  LService.SetMaxCacheSize(1000);
  LService.SetDefaultCacheTTL(3600000); // 1 hora
  LService.SetCleanupInterval(300000);  // 5 minutos
  Result := LService;
end;

class function TFallbackServiceFactory.CreateWithCache(const AMaxCacheSize: Integer;
  const ADefaultTTL: Cardinal): IFallbackService;
var
  LService: TFallbackService;
begin
  LService := TFallbackService.Create;
  LService.SetMaxCacheSize(AMaxCacheSize);
  LService.SetDefaultCacheTTL(ADefaultTTL);
  LService.SetCleanupInterval(ADefaultTTL div 12); // Cleanup a cada 1/12 do TTL
  Result := LService;
end;

class function TFallbackServiceFactory.CreateMinimal: IFallbackService;
var
  LService: TFallbackService;
begin
  LService := TFallbackService.Create;
  LService.SetMaxCacheSize(100);
  LService.SetDefaultCacheTTL(600000);  // 10 minutos
  LService.SetCleanupInterval(120000);  // 2 minutos
  Result := LService;
end;

end.

