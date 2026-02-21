unit Nest4D.Validation.async.integration;

{$mode delphi}

interface

uses
  Classes,
  SysUtils,
  Generics.Collections,
  // Nest4D.Async,
  // Nest4D.Validation.async.pipeline,
  Nest4D.Validation.parse.json,
  Validation.pipe;
  // NOTA: Funcionalidades assíncronas foram removidas por serem implementações incompletas

type
  // Integration helper for async validation with existing Nest4D components
  TNest4DAsyncValidationIntegration = class
  private
    class var FInstance: TNest4DAsyncValidationIntegration;
    class var FAsyncExecutor: TNest4DAsyncExecutor;
    class var FJsonParser: TNest4DStreamingJsonParser;
    class var FValidationPipe: TNest4DAdvancedAsyncValidationPipe;
    
    constructor Create;
  public
    class function GetInstance: TNest4DAsyncValidationIntegration;
    class procedure ReleaseInstance;
    
    destructor Destroy; override;
    
    // Factory methods for creating async validation components
    class function CreateAsyncJsonParser: TNest4DAsyncJsonParser;
    class function CreateAsyncValidationPipe: TNest4DAsyncValidationPipe;
    class function CreateAdvancedValidationPipe: TNest4DAdvancedAsyncValidationPipe;
    class function CreateStreamingJsonParser: TNest4DStreamingJsonParser;
    
    // Integration with existing Validation.pipe.pas
    class function WrapSyncValidationPipe(ASyncPipe: TValidationPipe): TNest4DAsyncValidationPipe;
    
    // Utility methods for common async validation scenarios
    class function ValidateJsonAsync(const AJsonString: string): TNest4DPromise<Boolean>;
    class function ValidateJsonFileAsync(const AFileName: string): TNest4DPromise<Boolean>;
    class function ValidateJsonStreamAsync(AStream: TStream): TNest4DPromise<Boolean>;
    
    // Batch validation methods
    class function ValidateJsonBatchAsync(const AJsonStrings: TArray<string>): TNest4DPromise<TArray<Boolean>>;
    
    // Performance optimization methods
    class procedure ConfigureAsyncValidation(const AMaxParallelTasks: Integer = 4;
      const AEnableCache: Boolean = True; const AValidationTimeoutMs: Integer = 30000);
    
    // Metrics and monitoring
    class function GetValidationMetrics: string;
    class function GetCacheStatistics: string;
    
    // Compatibility layer for existing sync code
    class function ValidateJsonSync(const AJsonString: string): Boolean;
    class function ParseJsonSync(const AJsonString: string): TJsonDataMap;
  end;

  // Middleware integration for Horse framework
  TNest4DAsyncValidationMiddleware = class
  public
    class procedure RegisterAsyncValidation;
    class procedure ValidateRequestAsync(const AJsonPayload: string; 
      ACallback: TProc<Boolean, TArray<string>>);
  end;

  // Extension methods for existing validation pipe
  TValidationPipeAsyncExtensions = class helper for TValidationPipe
  public
    function ToAsync: TNest4DAsyncValidationPipe;
    function ValidateAsync(const AData: TJsonDataMap): TNest4DPromise<Boolean>;
    function TransformAsync(const AData: TJsonDataMap): TNest4DPromise<TJsonDataMap>;
  end;

implementation

uses
  Math,
  StrUtils;

{ TNest4DAsyncValidationIntegration }

constructor TNest4DAsyncValidationIntegration.Create;
begin
  inherited Create;
  
  // Initialize shared async executor
  FAsyncExecutor := TNest4DAsyncExecutor.GetInstance;
  
  // Create optimized components
  FJsonParser := TNest4DStreamingJsonParser.Create(FAsyncExecutor);
  FValidationPipe := TNest4DAdvancedAsyncValidationPipe.Create(FAsyncExecutor);
  
  // Configure for optimal performance
  FValidationPipe.SetMaxParallelTasks(4);
  FValidationPipe.SetCacheEnabled(True);
  FValidationPipe.SetValidationTimeout(30000);
end;

destructor TNest4DAsyncValidationIntegration.Destroy;
begin
  FJsonParser.Free;
  FValidationPipe.Free;
  inherited;
end;

class function TNest4DAsyncValidationIntegration.GetInstance: TNest4DAsyncValidationIntegration;
begin
  if not Assigned(FInstance) then
    FInstance := TNest4DAsyncValidationIntegration.Create;
  Result := FInstance;
end;

class procedure TNest4DAsyncValidationIntegration.ReleaseInstance;
begin
  if Assigned(FInstance) then
  begin
    FInstance.Free;
    FInstance := nil;
  end;
end;

class function TNest4DAsyncValidationIntegration.CreateAsyncJsonParser: TNest4DAsyncJsonParser;
begin
  Result := TNest4DAsyncJsonParser.Create(FAsyncExecutor);
end;

class function TNest4DAsyncValidationIntegration.CreateAsyncValidationPipe: TNest4DAsyncValidationPipe;
begin
  Result := TNest4DAsyncValidationPipe.Create(FAsyncExecutor);
end;

class function TNest4DAsyncValidationIntegration.CreateAdvancedValidationPipe: TNest4DAdvancedAsyncValidationPipe;
begin
  Result := TNest4DAdvancedAsyncValidationPipe.Create(FAsyncExecutor);
end;

class function TNest4DAsyncValidationIntegration.CreateStreamingJsonParser: TNest4DStreamingJsonParser;
begin
  Result := TNest4DStreamingJsonParser.Create(FAsyncExecutor);
end;

class function TNest4DAsyncValidationIntegration.WrapSyncValidationPipe(ASyncPipe: TValidationPipe): TNest4DAsyncValidationPipe;
begin
  Result := TNest4DAsyncValidationPipe.Create(FAsyncExecutor);
  
  // Copy configuration from sync pipe to async pipe
  // This would require access to internal properties of TValidationPipe
  // Implementation depends on the actual structure of TValidationPipe
end;

class function TNest4DAsyncValidationIntegration.ValidateJsonAsync(const AJsonString: string): TNest4DPromise<Boolean>;
var
  LParser: TNest4DAsyncJsonParser;
  LPipe: TNest4DAsyncValidationPipe;
begin
  LParser := GetInstance.FJsonParser;
  LPipe := GetInstance.FValidationPipe;
  
  Result := LParser.ParseAsync(AJsonString)
    .Then(
      function(const AData: TJsonDataMap): TNest4DPromise<Boolean>
      begin
        Result := LPipe.ValidateAsync(AData, nil);
      end
    );
end;

class function TNest4DAsyncValidationIntegration.ValidateJsonFileAsync(const AFileName: string): TNest4DPromise<Boolean>;
var
  LParser: TNest4DStreamingJsonParser;
  LPipe: TNest4DAsyncValidationPipe;
begin
  LParser := GetInstance.FJsonParser;
  LPipe := GetInstance.FValidationPipe;
  
  Result := LParser.ParseJsonFileAsync(AFileName)
    .Then(
      function(const AData: TJsonDataMap): TNest4DPromise<Boolean>
      begin
        Result := LPipe.ValidateAsync(AData, nil);
      end
    );
end;

class function TNest4DAsyncValidationIntegration.ValidateJsonStreamAsync(AStream: TStream): TNest4DPromise<Boolean>;
var
  LParser: TNest4DStreamingJsonParser;
  LPipe: TNest4DAsyncValidationPipe;
begin
  LParser := GetInstance.FJsonParser;
  LPipe := GetInstance.FValidationPipe;
  
  Result := LParser.ParseLargeJsonAsync(AStream)
    .Then(
      function(const AData: TJsonDataMap): TNest4DPromise<Boolean>
      begin
        Result := LPipe.ValidateAsync(AData, nil);
      end
    );
end;

class function TNest4DAsyncValidationIntegration.ValidateJsonBatchAsync(const AJsonStrings: TArray<string>): TNest4DPromise<TArray<Boolean>>;
begin
  Result := FAsyncExecutor.ExecuteAsync<TArray<Boolean>>(
    function: TArray<Boolean>
    var
      LResults: TArray<Boolean>;
      LPromises: TArray<TNest4DPromise<Boolean>>;
      I: Integer;
    begin
      SetLength(LResults, Length(AJsonStrings));
      SetLength(LPromises, Length(AJsonStrings));
      
      // Create validation promises for each JSON string
      for I := 0 to High(AJsonStrings) do
      begin
        LPromises[I] := ValidateJsonAsync(AJsonStrings[I]);
      end;
      
      // Wait for all validations to complete
      for I := 0 to High(LPromises) do
      begin
        LResults[I] := LPromises[I].GetValue;
      end;
      
      Result := LResults;
    end
  );
end;

class procedure TNest4DAsyncValidationIntegration.ConfigureAsyncValidation(const AMaxParallelTasks: Integer;
  const AEnableCache: Boolean; const AValidationTimeoutMs: Integer);
begin
  if Assigned(FValidationPipe) then
  begin
    FValidationPipe.SetMaxParallelTasks(AMaxParallelTasks);
    FValidationPipe.SetCacheEnabled(AEnableCache);
    FValidationPipe.SetValidationTimeout(AValidationTimeoutMs);
  end;
end;

class function TNest4DAsyncValidationIntegration.GetValidationMetrics: string;
begin
  if Assigned(FValidationPipe) then
    Result := FValidationPipe.GetValidationMetrics
  else
    Result := 'Async validation not initialized';
end;

class function TNest4DAsyncValidationIntegration.GetCacheStatistics: string;
begin
  if Assigned(FValidationPipe) then
    Result := FValidationPipe.GetCacheStatistics
  else
    Result := 'Async validation not initialized';
end;

class function TNest4DAsyncValidationIntegration.ValidateJsonSync(const AJsonString: string): Boolean;
var
  LPromise: TNest4DPromise<Boolean>;
begin
  LPromise := ValidateJsonAsync(AJsonString);
  
  // Wait for async operation to complete (blocking)
  while LPromise.State = psPending do
    Sleep(1);
    
  if LPromise.State = psResolved then
    Result := LPromise.Result
  else
  begin
    Result := False;
    if Assigned(LPromise.Error) then
      raise LPromise.Error;
  end;
end;

class function TNest4DAsyncValidationIntegration.ParseJsonSync(const AJsonString: string): TJsonDataMap;
var
  LParser: TNest4DAsyncJsonParser;
  LPromise: TNest4DPromise<TJsonDataMap>;
begin
  LParser := GetInstance.FJsonParser;
  LPromise := LParser.ParseAsync(AJsonString);
  
  // Wait for async operation to complete (blocking)
  while LPromise.State = psPending do
    Sleep(1);
    
  if LPromise.State = psResolved then
    Result := LPromise.Result
  else
  begin
    Result := nil;
    if Assigned(LPromise.Error) then
      raise LPromise.Error;
  end;
end;

{ TNest4DAsyncValidationMiddleware }

class procedure TNest4DAsyncValidationMiddleware.RegisterAsyncValidation;
begin
  // This would integrate with Horse middleware system
  // Implementation depends on Horse framework integration
  // Example:
  // Horse.Use(AsyncValidationMiddleware);
end;

class procedure TNest4DAsyncValidationMiddleware.ValidateRequestAsync(const AJsonPayload: string;
  ACallback: TProc<Boolean, TArray<string>>);
var
  LValidationPromise: TNest4DPromise<Boolean>;
begin
  LValidationPromise := TNest4DAsyncValidationIntegration.ValidateJsonAsync(AJsonPayload);
  
  LValidationPromise
    .Then(
      procedure(const AResult: Boolean)
      begin
        if Assigned(ACallback) then
          ACallback(AResult, []);
      end
    )
    .Catch(
      procedure(const AError: Exception)
      begin
        if Assigned(ACallback) then
          ACallback(False, [AError.Message]);
      end
    );
end;

{ TValidationPipeAsyncExtensions }

function TValidationPipeAsyncExtensions.ToAsync: TNest4DAsyncValidationPipe;
begin
  Result := TNest4DAsyncValidationIntegration.WrapSyncValidationPipe(Self);
end;

function TValidationPipeAsyncExtensions.ValidateAsync(const AData: TJsonDataMap): TNest4DPromise<Boolean>;
var
  LAsyncPipe: TNest4DAsyncValidationPipe;
begin
  LAsyncPipe := ToAsync;
  try
    Result := LAsyncPipe.ValidateAsync(AData, nil);
  finally
    // Note: In a real implementation, you might want to cache the async pipe
    // LAsyncPipe.Free;
  end;
end;

function TValidationPipeAsyncExtensions.TransformAsync(const AData: TJsonDataMap): TNest4DPromise<TJsonDataMap>;
var
  LAsyncPipe: TNest4DAsyncValidationPipe;
begin
  LAsyncPipe := ToAsync;
  try
    Result := LAsyncPipe.TransformAsync(AData);
  finally
    // Note: In a real implementation, you might want to cache the async pipe
    // LAsyncPipe.Free;
  end;
end;

initialization

finalization
  TNest4DAsyncValidationIntegration.ReleaseInstance;

end.