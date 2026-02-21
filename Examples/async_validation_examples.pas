unit async_validation_examples;

{$mode delphi}

interface

uses
  Classes,
  SysUtils,
  Generics.Collections,
  // Nest4D.Async,
  // Nest4D.Validation.async.pipeline,
  // Nest4D.Validation.async.integration,
  Nest4D.Validation.parse.json,
  Validation.pipe;
  // NOTA: Funcionalidades assíncronas foram removidas por serem implementações incompletas

type
  // Examples demonstrating backward compatibility and async usage
  TNest4DValidationExamples = class
  public
    // Example 1: Traditional sync validation (existing code continues to work)
    class function ValidateUserDataSync(const AJsonString: string): Boolean;
    
    // Example 2: New async validation with same result
    class function ValidateUserDataAsync(const AJsonString: string): TNest4DPromise<Boolean>;
    
    // Example 3: Hybrid approach - async with sync fallback
    class function ValidateUserDataHybrid(const AJsonString: string; AUseAsync: Boolean = True): Boolean;
    
    // Example 4: Batch validation for multiple JSON strings
    class function ValidateBatchSync(const AJsonStrings: TArray<string>): TArray<Boolean>;
    class function ValidateBatchAsync(const AJsonStrings: TArray<string>): TNest4DPromise<TArray<Boolean>>;
    
    // Example 5: Large file validation with streaming
    class function ValidateLargeFileSync(const AFileName: string): Boolean;
    class function ValidateLargeFileAsync(const AFileName: string): TNest4DPromise<Boolean>;
    
    // Example 6: Migration helper - convert existing sync code to async
    class function MigrateToAsync(ASyncValidationFunc: TFunc<string, Boolean>): TFunc<string, TNest4DPromise<Boolean>>;
    
    // Example 7: Performance comparison
    class procedure ComparePerformance(const AJsonStrings: TArray<string>);
    
    // Example 8: Error handling in both sync and async scenarios
    class function ValidateWithErrorHandlingSync(const AJsonString: string): Boolean;
    class function ValidateWithErrorHandlingAsync(const AJsonString: string): TNest4DPromise<Boolean>;
  end;

  // Compatibility wrapper that maintains existing TValidationPipe interface
  TNest4DCompatibilityValidationPipe = class(TValidationPipe)
  private
    FAsyncPipe: TNest4DAsyncValidationPipe;
    FUseAsyncByDefault: Boolean;
  public
    constructor Create; override;
    destructor Destroy; override;
    
    // Override existing methods to optionally use async internally
    function Transform(const AData: TJsonDataMap): TJsonDataMap; override;
    function Validate(const AData: TJsonDataMap): Boolean; override;
    
    // New async methods that extend the interface
    function TransformAsync(const AData: TJsonDataMap): TNest4DPromise<TJsonDataMap>;
    function ValidateAsync(const AData: TJsonDataMap): TNest4DPromise<Boolean>;
    
    // Configuration
    procedure SetAsyncByDefault(AValue: Boolean);
    procedure ConfigureAsync(const AMaxParallelTasks: Integer = 4; const AEnableCache: Boolean = True);
  end;

implementation

uses
  DateUtils,
  Math;

{ TNest4DValidationExamples }

class function TNest4DValidationExamples.ValidateUserDataSync(const AJsonString: string): Boolean;
var
  LParser: TJsonDataMap;
  LPipe: TValidationPipe;
begin
  Result := False;
  LParser := nil;
  LPipe := nil;
  
  try
    // Traditional approach - existing code
    LParser := TJsonDataMap.Create;
    LParser.ParseFromString(AJsonString);
    
    LPipe := TValidationPipe.Create;
    Result := LPipe.Validate(LParser);
    
  except
    on E: Exception do
    begin
      // Log error
      Result := False;
    end;
  end;
  
  LParser.Free;
  LPipe.Free;
end;

class function TNest4DValidationExamples.ValidateUserDataAsync(const AJsonString: string): TNest4DPromise<Boolean>;
begin
  // New async approach - same result, better performance
  Result := TNest4DAsyncValidationIntegration.ValidateJsonAsync(AJsonString);
end;

class function TNest4DValidationExamples.ValidateUserDataHybrid(const AJsonString: string; AUseAsync: Boolean): Boolean;
var
  LAsyncResult: TNest4DPromise<Boolean>;
begin
  if AUseAsync then
  begin
    // Use async validation but wait for result (non-blocking internally)
    LAsyncResult := ValidateUserDataAsync(AJsonString);
    
    // Wait for completion
    while LAsyncResult.State = psPending do
      Sleep(1);
      
    if LAsyncResult.State = psResolved then
      Result := LAsyncResult.Result
    else
      Result := False;
  end
  else
  begin
    // Fallback to sync validation
    Result := ValidateUserDataSync(AJsonString);
  end;
end;

class function TNest4DValidationExamples.ValidateBatchSync(const AJsonStrings: TArray<string>): TArray<Boolean>;
var
  I: Integer;
begin
  SetLength(Result, Length(AJsonStrings));
  
  // Sequential validation - slower for large batches
  for I := 0 to High(AJsonStrings) do
  begin
    Result[I] := ValidateUserDataSync(AJsonStrings[I]);
  end;
end;

class function TNest4DValidationExamples.ValidateBatchAsync(const AJsonStrings: TArray<string>): TNest4DPromise<TArray<Boolean>>;
begin
  // Parallel validation - much faster for large batches
  Result := TNest4DAsyncValidationIntegration.ValidateJsonBatchAsync(AJsonStrings);
end;

class function TNest4DValidationExamples.ValidateLargeFileSync(const AFileName: string): Boolean;
var
  LFileStream: TFileStream;
  LJsonString: string;
  LStringStream: TStringStream;
begin
  Result := False;
  
  if not FileExists(AFileName) then
    Exit;
    
  LFileStream := nil;
  LStringStream := nil;
  
  try
    LFileStream := TFileStream.Create(AFileName, fmOpenRead);
    LStringStream := TStringStream.Create;
    
    // Load entire file into memory - not optimal for large files
    LStringStream.CopyFrom(LFileStream, 0);
    LJsonString := LStringStream.DataString;
    
    Result := ValidateUserDataSync(LJsonString);
    
  except
    on E: Exception do
      Result := False;
  end;
  
  LFileStream.Free;
  LStringStream.Free;
end;

class function TNest4DValidationExamples.ValidateLargeFileAsync(const AFileName: string): TNest4DPromise<Boolean>;
begin
  // Streaming validation - optimal for large files
  Result := TNest4DAsyncValidationIntegration.ValidateJsonFileAsync(AFileName);
end;

class function TNest4DValidationExamples.MigrateToAsync(ASyncValidationFunc: TFunc<string, Boolean>): TFunc<string, TNest4DPromise<Boolean>>;
begin
  Result := function(const AJsonString: string): TNest4DPromise<Boolean>
    begin
      Result := TNest4DAsyncExecutor.GetInstance.ExecuteAsync<Boolean>(
        function: Boolean
        begin
          Result := ASyncValidationFunc(AJsonString);
        end
      );
    end;
end;

class procedure TNest4DValidationExamples.ComparePerformance(const AJsonStrings: TArray<string>);
var
  LStartTime, LEndTime: TDateTime;
  LSyncResults: TArray<Boolean>;
  LAsyncResults: TNest4DPromise<TArray<Boolean>>;
  LSyncTime, LAsyncTime: Double;
begin
  WriteLn('Performance Comparison for ', Length(AJsonStrings), ' JSON validations:');
  
  // Test sync performance
  LStartTime := Now;
  LSyncResults := ValidateBatchSync(AJsonStrings);
  LEndTime := Now;
  LSyncTime := MilliSecondsBetween(LEndTime, LStartTime);
  
  WriteLn('Sync validation time: ', LSyncTime:0:2, ' ms');
  
  // Test async performance
  LStartTime := Now;
  LAsyncResults := ValidateBatchAsync(AJsonStrings);
  
  // Wait for async completion
  while LAsyncResults.State = psPending do
    Sleep(1);
    
  LEndTime := Now;
  LAsyncTime := MilliSecondsBetween(LEndTime, LStartTime);
  
  WriteLn('Async validation time: ', LAsyncTime:0:2, ' ms');
  WriteLn('Performance improvement: ', ((LSyncTime - LAsyncTime) / LSyncTime * 100):0:1, '%');
end;

class function TNest4DValidationExamples.ValidateWithErrorHandlingSync(const AJsonString: string): Boolean;
begin
  try
    Result := ValidateUserDataSync(AJsonString);
  except
    on E: EJsonParseError do
    begin
      WriteLn('JSON Parse Error: ', E.Message);
      Result := False;
    end;
    on E: EValidationError do
    begin
      WriteLn('Validation Error: ', E.Message);
      Result := False;
    end;
    on E: Exception do
    begin
      WriteLn('Unexpected Error: ', E.Message);
      Result := False;
    end;
  end;
end;

class function TNest4DValidationExamples.ValidateWithErrorHandlingAsync(const AJsonString: string): TNest4DPromise<Boolean>;
begin
  Result := ValidateUserDataAsync(AJsonString)
    .Catch(
      function(const AError: Exception): Boolean
      begin
        if AError is EJsonParseError then
          WriteLn('Async JSON Parse Error: ', AError.Message)
        else if AError is EValidationError then
          WriteLn('Async Validation Error: ', AError.Message)
        else
          WriteLn('Async Unexpected Error: ', AError.Message);
          
        Result := False;
      end
    );
end;

{ TNest4DCompatibilityValidationPipe }

constructor TNest4DCompatibilityValidationPipe.Create;
begin
  inherited Create;
  FAsyncPipe := TNest4DAsyncValidationIntegration.CreateAsyncValidationPipe;
  FUseAsyncByDefault := False; // Maintain backward compatibility
end;

destructor TNest4DCompatibilityValidationPipe.Destroy;
begin
  FAsyncPipe.Free;
  inherited;
end;

function TNest4DCompatibilityValidationPipe.Transform(const AData: TJsonDataMap): TJsonDataMap;
var
  LAsyncResult: TNest4DPromise<TJsonDataMap>;
begin
  if FUseAsyncByDefault then
  begin
    // Use async internally but return sync result
    LAsyncResult := FAsyncPipe.TransformAsync(AData);
    
    // Wait for completion
    while LAsyncResult.State = psPending do
      Sleep(1);
      
    if LAsyncResult.State = psResolved then
      Result := LAsyncResult.Result
    else
    begin
      Result := nil;
      if Assigned(LAsyncResult.Error) then
        raise LAsyncResult.Error;
    end;
  end
  else
  begin
    // Use original sync implementation
    Result := inherited Transform(AData);
  end;
end;

function TNest4DCompatibilityValidationPipe.Validate(const AData: TJsonDataMap): Boolean;
var
  LAsyncResult: TNest4DPromise<Boolean>;
begin
  if FUseAsyncByDefault then
  begin
    // Use async internally but return sync result
    LAsyncResult := FAsyncPipe.ValidateAsync(AData, nil);
    
    // Wait for completion
    while LAsyncResult.State = psPending do
      Sleep(1);
      
    if LAsyncResult.State = psResolved then
      Result := LAsyncResult.Result
    else
    begin
      Result := False;
      if Assigned(LAsyncResult.Error) then
        raise LAsyncResult.Error;
    end;
  end
  else
  begin
    // Use original sync implementation
    Result := inherited Validate(AData);
  end;
end;

function TNest4DCompatibilityValidationPipe.TransformAsync(const AData: TJsonDataMap): TNest4DPromise<TJsonDataMap>;
begin
  Result := FAsyncPipe.TransformAsync(AData);
end;

function TNest4DCompatibilityValidationPipe.ValidateAsync(const AData: TJsonDataMap): TNest4DPromise<Boolean>;
begin
  Result := FAsyncPipe.ValidateAsync(AData, nil);
end;

procedure TNest4DCompatibilityValidationPipe.SetAsyncByDefault(AValue: Boolean);
begin
  FUseAsyncByDefault := AValue;
end;

procedure TNest4DCompatibilityValidationPipe.ConfigureAsync(const AMaxParallelTasks: Integer; const AEnableCache: Boolean);
begin
  if FAsyncPipe is TNest4DAdvancedAsyncValidationPipe then
  begin
    TNest4DAdvancedAsyncValidationPipe(FAsyncPipe).SetMaxParallelTasks(AMaxParallelTasks);
    TNest4DAdvancedAsyncValidationPipe(FAsyncPipe).SetCacheEnabled(AEnableCache);
  end;
end;

end.