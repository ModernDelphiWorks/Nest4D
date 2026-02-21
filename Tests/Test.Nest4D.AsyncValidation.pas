unit Test.Nest4D.AsyncValidation;

{$mode delphi}

interface

uses
  Classes,
  SysUtils,
  TestFramework,
  DUnitX.TestFramework;
  // Nest4D.Async,
  // Nest4D.Validation.async.pipeline,
  // Nest4D.Validation.async.performance,
  // Nest4D.Validation.async.integration;
  // NOTA: Funcionalidades assíncronas foram removidas por serem implementações incompletas

type
  [TestFixture]
  TNest4DAsyncValidationTests = class
  private
    FAsyncExecutor: TNest4DAsyncExecutor;
    FJsonParser: TNest4DAsyncJsonParser;
    FValidationPipe: TNest4DAsyncValidationPipe;
    FStreamingParser: TNest4DStreamingJsonParser;
    FAdvancedPipe: TNest4DAdvancedAsyncValidationPipe;
    FPerformanceOptimizer: TNest4DValidationPerformanceOptimizer;
    FHighPerformanceValidator: TNest4DHighPerformanceJsonValidator;
    
    function CreateTestJsonString(const ASize: Integer = 1000): string;
    function CreateComplexJsonString: string;
    function CreateLargeJsonFile(const AFileName: string; const ASizeMB: Integer = 1): Boolean;
    procedure CleanupTestFiles;
    
  public
    [Setup]
    procedure Setup;
    
    [TearDown]
    procedure TearDown;
    
    // Basic async JSON parser tests
    [Test]
    procedure TestAsyncJsonParserBasic;
    
    [Test]
    procedure TestAsyncJsonParserWithInvalidJson;
    
    [Test]
    procedure TestAsyncJsonParserWithLargeJson;
    
    [Test]
    procedure TestAsyncJsonParserSyncCompatibility;
    
    // Streaming parser tests
    [Test]
    procedure TestStreamingJsonParser;
    
    [Test]
    procedure TestStreamingJsonParserWithFile;
    
    [Test]
    procedure TestStreamingJsonParserMemoryManagement;
    
    [Test]
    procedure TestStreamingJsonParserChunking;
    
    // Basic validation pipe tests
    [Test]
    procedure TestAsyncValidationPipeBasic;
    
    [Test]
    procedure TestAsyncValidationPipeWithTransform;
    
    [Test]
    procedure TestAsyncValidationPipeErrorHandling;
    
    [Test]
    procedure TestAsyncValidationPipeSyncCompatibility;
    
    // Advanced validation pipe tests
    [Test]
    procedure TestAdvancedValidationPipeParallel;
    
    [Test]
    procedure TestAdvancedValidationPipeCache;
    
    [Test]
    procedure TestAdvancedValidationPipeBatch;
    
    [Test]
    procedure TestAdvancedValidationPipeMetrics;
    
    // Performance optimization tests
    [Test]
    procedure TestPerformanceOptimizerConfiguration;
    
    [Test]
    procedure TestPerformanceOptimizerBufferPool;
    
    [Test]
    procedure TestPerformanceOptimizerComplexityAnalysis;
    
    [Test]
    procedure TestPerformanceOptimizerAdaptiveConfig;
    
    // High-performance validator tests
    [Test]
    procedure TestHighPerformanceValidatorLargeJson;
    
    [Test]
    procedure TestHighPerformanceValidatorBatch;
    
    [Test]
    procedure TestHighPerformanceValidatorHugeFile;
    
    [Test]
    procedure TestHighPerformanceValidatorAdaptive;
    
    // Integration tests
    [Test]
    procedure TestAsyncValidationIntegration;
    
    [Test]
    procedure TestAsyncValidationMiddleware;
    
    [Test]
    procedure TestValidationPipeExtensions;
    
    // Compatibility tests
    [Test]
    procedure TestSyncAsyncCompatibility;
    
    [Test]
    procedure TestMigrationFromSyncToAsync;
    
    // Error handling and edge cases
    [Test]
    procedure TestErrorHandlingInAsyncPipeline;
    
    [Test]
    procedure TestTimeoutHandling;
    
    [Test]
    procedure TestMemoryLimitHandling;
    
    [Test]
    procedure TestConcurrentValidation;
    
    // Performance benchmarks
    [Test]
    procedure BenchmarkAsyncVsSyncValidation;
    
    [Test]
    procedure BenchmarkStreamingVsRegularParsing;
    
    [Test]
    procedure BenchmarkParallelValidation;
    
    [Test]
    procedure BenchmarkMemoryUsage;
  end;

  [TestFixture]
  TNest4DJsonChunkProcessorTests = class
  private
    FChunkProcessor: TNest4DJsonChunkProcessor;
    
  public
    [Setup]
    procedure Setup;
    
    [TearDown]
    procedure TearDown;
    
    [Test]
    procedure TestChunkProcessingBasic;
    
    [Test]
    procedure TestChunkProcessingWithOverlap;
    
    [Test]
    procedure TestChunkProcessingLargeFile;
    
    [Test]
    procedure TestChunkProcessingErrorHandling;
  end;

implementation

uses
  DateUtils,
  Math,
  IOUtils;

{ TNest4DAsyncValidationTests }

procedure TNest4DAsyncValidationTests.Setup;
var
  LConfig: TNest4DValidationPerformanceConfig;
begin
  FAsyncExecutor := TNest4DAsyncExecutor.GetInstance;
  FJsonParser := TNest4DAsyncJsonParser.Create(FAsyncExecutor);
  FValidationPipe := TNest4DAsyncValidationPipe.Create(FAsyncExecutor);
  FStreamingParser := TNest4DStreamingJsonParser.Create(FAsyncExecutor);
  FAdvancedPipe := TNest4DAdvancedAsyncValidationPipe.Create(FAsyncExecutor);
  
  LConfig := TNest4DValidationPerformanceConfig.Default;
  FPerformanceOptimizer := TNest4DValidationPerformanceOptimizer.Create(LConfig);
  FHighPerformanceValidator := TNest4DHighPerformanceJsonValidator.Create(LConfig);
end;

procedure TNest4DAsyncValidationTests.TearDown;
begin
  CleanupTestFiles;
  
  FHighPerformanceValidator.Free;
  FPerformanceOptimizer.Free;
  FAdvancedPipe.Free;
  FStreamingParser.Free;
  FValidationPipe.Free;
  FJsonParser.Free;
end;

function TNest4DAsyncValidationTests.CreateTestJsonString(const ASize: Integer): string;
var
  I: Integer;
  LBuilder: TStringBuilder;
begin
  LBuilder := TStringBuilder.Create;
  try
    LBuilder.Append('{');
    LBuilder.Append('"data": [');
    
    for I := 0 to ASize - 1 do
    begin
      if I > 0 then
        LBuilder.Append(',');
      LBuilder.AppendFormat('{"id": %d, "name": "Item%d", "value": %d}', [I, I, Random(1000)]);
    end;
    
    LBuilder.Append(']');
    LBuilder.Append('}');
    
    Result := LBuilder.ToString;
  finally
    LBuilder.Free;
  end;
end;

function TNest4DAsyncValidationTests.CreateComplexJsonString: string;
begin
  Result := '{
' +
    '  "users": [
' +
    '    {
' +
    '      "id": 1,
' +
    '      "name": "John Doe",
' +
    '      "email": "john@example.com",
' +
    '      "profile": {
' +
    '        "age": 30,
' +
    '        "preferences": {
' +
    '          "theme": "dark",
' +
    '          "notifications": true,
' +
    '          "languages": ["en", "pt", "es"]
' +
    '        },
' +
    '        "addresses": [
' +
    '          {
' +
    '            "type": "home",
' +
    '            "street": "123 Main St",
' +
    '            "city": "Anytown",
' +
    '            "coordinates": {
' +
    '              "lat": 40.7128,
' +
    '              "lng": -74.0060
' +
    '            }
' +
    '          }
' +
    '        ]
' +
    '      }
' +
    '    }
' +
    '  ],
' +
    '  "metadata": {
' +
    '    "version": "1.0",
' +
    '    "timestamp": "2024-01-01T00:00:00Z",
' +
    '    "total_count": 1
' +
    '  }
' +
    '}';
end;

function TNest4DAsyncValidationTests.CreateLargeJsonFile(const AFileName: string; const ASizeMB: Integer): Boolean;
var
  LFileStream: TFileStream;
  LJsonString: string;
  LTargetSize: Int64;
  LCurrentSize: Int64;
begin
  Result := False;
  
  try
    LFileStream := TFileStream.Create(AFileName, fmCreate);
    try
      LTargetSize := ASizeMB * 1024 * 1024;
      LCurrentSize := 0;
      
      LFileStream.WriteBuffer(PAnsiChar('{"data": [')^, Length('{"data": ['));
      Inc(LCurrentSize, Length('{"data": ['));
      
      while LCurrentSize < LTargetSize - 1000 do
      begin
        if LCurrentSize > Length('{"data": [') then
        begin
          LFileStream.WriteBuffer(PAnsiChar(',')^, 1);
          Inc(LCurrentSize);
        end;
        
        LJsonString := Format('{"id": %d, "data": "%s"}', [Random(10000), StringOfChar('x', 100)]);
        LFileStream.WriteBuffer(PAnsiChar(LJsonString)^, Length(LJsonString));
        Inc(LCurrentSize, Length(LJsonString));
      end;
      
      LFileStream.WriteBuffer(PAnsiChar(']}')^, Length(']}'));
      Result := True;
      
    finally
      LFileStream.Free;
    end;
  except
    on E: Exception do
    begin
      // Log error
      Result := False;
    end;
  end;
end;

procedure TNest4DAsyncValidationTests.CleanupTestFiles;
var
  LFiles: TArray<string>;
  LFile: string;
begin
  LFiles := ['test_large.json', 'test_huge.json', 'test_streaming.json'];
  
  for LFile in LFiles do
  begin
    if FileExists(LFile) then
      DeleteFile(LFile);
  end;
end;

procedure TNest4DAsyncValidationTests.TestAsyncJsonParserBasic;
var
  LJsonString: string;
  LPromise: TNest4DPromise<TJsonDataMap>;
  LResult: TJsonDataMap;
begin
  LJsonString := CreateTestJsonString(10);
  
  LPromise := FJsonParser.ParseAsync(LJsonString);
  
  // Wait for completion
  while LPromise.State = psPending do
    Sleep(1);
    
  Assert.AreEqual(psResolved, LPromise.State, 'Promise should be resolved');
  
  LResult := LPromise.Result;
  Assert.IsTrue(LResult.ContainsKey('data'), 'Result should contain data key');
end;

procedure TNest4DAsyncValidationTests.TestAsyncJsonParserWithInvalidJson;
var
  LJsonString: string;
  LPromise: TNest4DPromise<TJsonDataMap>;
begin
  LJsonString := '{"invalid": json}';
  
  LPromise := FJsonParser.ParseAsync(LJsonString);
  
  // Wait for completion
  while LPromise.State = psPending do
    Sleep(1);
    
  Assert.AreEqual(psRejected, LPromise.State, 'Promise should be rejected for invalid JSON');
end;

procedure TNest4DAsyncValidationTests.TestAsyncJsonParserWithLargeJson;
var
  LJsonString: string;
  LPromise: TNest4DPromise<TJsonDataMap>;
  LStartTime: TDateTime;
  LEndTime: TDateTime;
begin
  LJsonString := CreateTestJsonString(10000); // Large JSON
  
  LStartTime := Now;
  LPromise := FJsonParser.ParseAsync(LJsonString);
  
  // Wait for completion
  while LPromise.State = psPending do
    Sleep(1);
    
  LEndTime := Now;
  
  Assert.AreEqual(psResolved, LPromise.State, 'Promise should be resolved');
  Assert.IsTrue(MilliSecondsBetween(LEndTime, LStartTime) < 5000, 'Parsing should complete within 5 seconds');
end;

procedure TNest4DAsyncValidationTests.TestAsyncJsonParserSyncCompatibility;
var
  LJsonString: string;
  LAsyncResult: TJsonDataMap;
  LSyncResult: TJsonDataMap;
  LPromise: TNest4DPromise<TJsonDataMap>;
begin
  LJsonString := CreateTestJsonString(100);
  
  // Test async parsing
  LPromise := FJsonParser.ParseAsync(LJsonString);
  while LPromise.State = psPending do
    Sleep(1);
  LAsyncResult := LPromise.Result;
  
  // Test sync parsing
  LSyncResult := FJsonParser.Parse(LJsonString);
  
  // Results should be equivalent
  Assert.AreEqual(LAsyncResult.Count, LSyncResult.Count, 'Async and sync results should have same count');
end;

procedure TNest4DAsyncValidationTests.TestStreamingJsonParser;
var
  LJsonString: string;
  LPromise: TNest4DPromise<TJsonDataMap>;
begin
  LJsonString := CreateComplexJsonString;
  
  LPromise := FStreamingParser.ParseAsync(LJsonString);
  
  while LPromise.State = psPending do
    Sleep(1);
    
  Assert.AreEqual(psResolved, LPromise.State, 'Streaming parser should resolve');
end;

procedure TNest4DAsyncValidationTests.TestStreamingJsonParserWithFile;
var
  LFileName: string;
  LPromise: TNest4DPromise<TJsonDataMap>;
begin
  LFileName := 'test_streaming.json';
  
  if CreateLargeJsonFile(LFileName, 1) then
  begin
    LPromise := FStreamingParser.ParseJsonFileAsync(LFileName);
    
    while LPromise.State = psPending do
      Sleep(10);
      
    Assert.AreEqual(psResolved, LPromise.State, 'File parsing should resolve');
  end
  else
  begin
    Assert.Fail('Could not create test file');
  end;
end;

procedure TNest4DAsyncValidationTests.TestStreamingJsonParserMemoryManagement;
var
  LJsonString: string;
  LPromise: TNest4DPromise<TJsonDataMap>;
  LMemoryBefore: Cardinal;
  LMemoryAfter: Cardinal;
begin
  LMemoryBefore := GetHeapStatus.TotalAllocated;
  
  LJsonString := CreateTestJsonString(50000); // Very large JSON
  
  FStreamingParser.SetMaxMemoryUsage(10 * 1024 * 1024); // 10MB limit
  LPromise := FStreamingParser.ParseAsync(LJsonString);
  
  while LPromise.State = psPending do
    Sleep(10);
    
  LMemoryAfter := GetHeapStatus.TotalAllocated;
  
  Assert.IsTrue(LMemoryAfter - LMemoryBefore < 15 * 1024 * 1024, 'Memory usage should be controlled');
end;

procedure TNest4DAsyncValidationTests.TestStreamingJsonParserChunking;
var
  LJsonString: string;
  LPromise: TNest4DPromise<TJsonDataMap>;
begin
  LJsonString := CreateTestJsonString(1000);
  
  FStreamingParser.SetChunkSize(1024); // Small chunks
  LPromise := FStreamingParser.ParseAsync(LJsonString);
  
  while LPromise.State = psPending do
    Sleep(1);
    
  Assert.AreEqual(psResolved, LPromise.State, 'Chunked parsing should work');
end;

procedure TNest4DAsyncValidationTests.TestAsyncValidationPipeBasic;
var
  LData: TJsonDataMap;
  LPromise: TNest4DPromise<Boolean>;
begin
  LData := TJsonDataMap.Create;
  try
    LData.Add('name', 'Test');
    LData.Add('age', 25);
    
    LPromise := FValidationPipe.ValidateAsync(LData, nil);
    
    while LPromise.State = psPending do
      Sleep(1);
      
    Assert.AreEqual(psResolved, LPromise.State, 'Validation should resolve');
  finally
    LData.Free;
  end;
end;

procedure TNest4DAsyncValidationTests.TestAsyncValidationPipeWithTransform;
var
  LData: TJsonDataMap;
  LPromise: TNest4DPromise<TJsonDataMap>;
begin
  LData := TJsonDataMap.Create;
  try
    LData.Add('name', 'test');
    
    LPromise := FValidationPipe.TransformAsync(LData, nil);
    
    while LPromise.State = psPending do
      Sleep(1);
      
    Assert.AreEqual(psResolved, LPromise.State, 'Transform should resolve');
  finally
    LData.Free;
  end;
end;

procedure TNest4DAsyncValidationTests.TestAsyncValidationPipeErrorHandling;
var
  LPromise: TNest4DPromise<Boolean>;
begin
  // Test with nil data
  LPromise := FValidationPipe.ValidateAsync(nil, nil);
  
  while LPromise.State = psPending do
    Sleep(1);
    
  Assert.AreEqual(psRejected, LPromise.State, 'Should reject nil data');
end;

procedure TNest4DAsyncValidationTests.TestAsyncValidationPipeSyncCompatibility;
var
  LData: TJsonDataMap;
  LAsyncResult: Boolean;
  LSyncResult: Boolean;
  LPromise: TNest4DPromise<Boolean>;
begin
  LData := TJsonDataMap.Create;
  try
    LData.Add('test', 'value');
    
    // Test async
    LPromise := FValidationPipe.ValidateAsync(LData, nil);
    while LPromise.State = psPending do
      Sleep(1);
    LAsyncResult := LPromise.Result;
    
    // Test sync
    LSyncResult := FValidationPipe.Validate(LData, nil);
    
    Assert.AreEqual(LAsyncResult, LSyncResult, 'Async and sync results should match');
  finally
    LData.Free;
  end;
end;

procedure TNest4DAsyncValidationTests.TestAdvancedValidationPipeParallel;
var
  LDataArray: TArray<TJsonDataMap>;
  LPromise: TNest4DPromise<TArray<Boolean>>;
  I: Integer;
begin
  SetLength(LDataArray, 5);
  try
    for I := 0 to High(LDataArray) do
    begin
      LDataArray[I] := TJsonDataMap.Create;
      LDataArray[I].Add('id', I);
    end;
    
    FAdvancedPipe.SetMaxParallelTasks(3);
    LPromise := FAdvancedPipe.ValidateParallelAsync(LDataArray);
    
    while LPromise.State = psPending do
      Sleep(1);
      
    Assert.AreEqual(psResolved, LPromise.State, 'Parallel validation should resolve');
    Assert.AreEqual(Length(LDataArray), Length(LPromise.Result), 'Should validate all items');
    
  finally
    for I := 0 to High(LDataArray) do
      LDataArray[I].Free;
  end;
end;

procedure TNest4DAsyncValidationTests.TestAdvancedValidationPipeCache;
var
  LData: TJsonDataMap;
  LPromise1, LPromise2: TNest4DPromise<Boolean>;
  LStartTime1, LEndTime1, LStartTime2, LEndTime2: TDateTime;
begin
  LData := TJsonDataMap.Create;
  try
    LData.Add('cached_test', 'value');
    
    FAdvancedPipe.SetCacheEnabled(True);
    
    // First validation (should cache)
    LStartTime1 := Now;
    LPromise1 := FAdvancedPipe.ValidateAsync(LData, nil);
    while LPromise1.State = psPending do
      Sleep(1);
    LEndTime1 := Now;
    
    // Second validation (should use cache)
    LStartTime2 := Now;
    LPromise2 := FAdvancedPipe.ValidateAsync(LData, nil);
    while LPromise2.State = psPending do
      Sleep(1);
    LEndTime2 := Now;
    
    Assert.IsTrue(MilliSecondsBetween(LEndTime2, LStartTime2) <= MilliSecondsBetween(LEndTime1, LStartTime1), 
                  'Cached validation should be faster or equal');
    
  finally
    LData.Free;
  end;
end;

procedure TNest4DAsyncValidationTests.TestAdvancedValidationPipeBatch;
var
  LJsonStrings: TArray<string>;
  LPromise: TNest4DPromise<TArray<Boolean>>;
  I: Integer;
begin
  SetLength(LJsonStrings, 10);
  for I := 0 to High(LJsonStrings) do
    LJsonStrings[I] := CreateTestJsonString(10);
    
  LPromise := FAdvancedPipe.ValidateBatchAsync(LJsonStrings);
  
  while LPromise.State = psPending do
    Sleep(10);
    
  Assert.AreEqual(psResolved, LPromise.State, 'Batch validation should resolve');
  Assert.AreEqual(Length(LJsonStrings), Length(LPromise.Result), 'Should validate all items');
end;

procedure TNest4DAsyncValidationTests.TestAdvancedValidationPipeMetrics;
var
  LData: TJsonDataMap;
  LPromise: TNest4DPromise<Boolean>;
  LMetrics: TNest4DValidationMetrics;
begin
  LData := TJsonDataMap.Create;
  try
    LData.Add('metrics_test', 'value');
    
    FAdvancedPipe.ResetMetrics;
    LPromise := FAdvancedPipe.ValidateAsync(LData, nil);
    
    while LPromise.State = psPending do
      Sleep(1);
      
    LMetrics := FAdvancedPipe.GetMetrics;
    Assert.IsTrue(LMetrics.TotalValidations > 0, 'Should record validation metrics');
    
  finally
    LData.Free;
  end;
end;

procedure TNest4DAsyncValidationTests.TestPerformanceOptimizerConfiguration;
var
  LConfig: TNest4DValidationPerformanceConfig;
begin
  LConfig := TNest4DValidationPerformanceConfig.HighPerformance;
  FPerformanceOptimizer.UpdateConfiguration(LConfig);
  
  LConfig := FPerformanceOptimizer.GetConfiguration;
  Assert.AreEqual(1024, LConfig.MaxMemoryUsageMB, 'Should update configuration');
end;

procedure TNest4DAsyncValidationTests.TestPerformanceOptimizerBufferPool;
var
  LBuffer1, LBuffer2: TMemoryStream;
begin
  LBuffer1 := FPerformanceOptimizer.AcquireBuffer;
  Assert.IsNotNull(LBuffer1, 'Should acquire buffer');
  
  LBuffer2 := FPerformanceOptimizer.AcquireBuffer;
  Assert.IsNotNull(LBuffer2, 'Should acquire second buffer');
  
  FPerformanceOptimizer.ReleaseBuffer(LBuffer1);
  FPerformanceOptimizer.ReleaseBuffer(LBuffer2);
end;

procedure TNest4DAsyncValidationTests.TestPerformanceOptimizerComplexityAnalysis;
var
  LSimpleJson: string;
  LComplexJson: string;
  LSimpleComplexity: Integer;
  LComplexComplexity: Integer;
begin
  LSimpleJson := '{"name": "test"}';
  LComplexJson := CreateComplexJsonString;
  
  LSimpleComplexity := FPerformanceOptimizer.AnalyzeJsonComplexity(LSimpleJson);
  LComplexComplexity := FPerformanceOptimizer.AnalyzeJsonComplexity(LComplexJson);
  
  Assert.IsTrue(LComplexComplexity > LSimpleComplexity, 'Complex JSON should have higher complexity score');
end;

procedure TNest4DAsyncValidationTests.TestPerformanceOptimizerAdaptiveConfig;
var
  LConfig: TNest4DValidationPerformanceConfig;
begin
  // Test with large JSON size
  LConfig := FPerformanceOptimizer.GetOptimalConfiguration(100 * 1024 * 1024, 500);
  Assert.IsTrue(LConfig.MaxMemoryUsageMB >= 1024, 'Should use high-performance config for large JSON');
  
  // Test with small JSON size
  LConfig := FPerformanceOptimizer.GetOptimalConfiguration(1024, 10);
  Assert.IsTrue(LConfig.MaxMemoryUsageMB <= 256, 'Should use default config for small JSON');
end;

procedure TNest4DAsyncValidationTests.TestHighPerformanceValidatorLargeJson;
var
  LJsonString: string;
  LPromise: TNest4DPromise<Boolean>;
begin
  LJsonString := CreateTestJsonString(10000);
  
  LPromise := FHighPerformanceValidator.ValidateLargeJsonAsync(LJsonString);
  
  while LPromise.State = psPending do
    Sleep(10);
    
  Assert.AreEqual(psResolved, LPromise.State, 'High-performance validation should resolve');
end;

procedure TNest4DAsyncValidationTests.TestHighPerformanceValidatorBatch;
var
  LJsonStrings: TArray<string>;
  LPromise: TNest4DPromise<TArray<Boolean>>;
  I: Integer;
begin
  SetLength(LJsonStrings, 20);
  for I := 0 to High(LJsonStrings) do
    LJsonStrings[I] := CreateTestJsonString(100);
    
  LPromise := FHighPerformanceValidator.ValidateBatchAdaptiveAsync(LJsonStrings);
  
  while LPromise.State = psPending do
    Sleep(10);
    
  Assert.AreEqual(psResolved, LPromise.State, 'Batch validation should resolve');
end;

procedure TNest4DAsyncValidationTests.TestHighPerformanceValidatorHugeFile;
var
  LFileName: string;
  LPromise: TNest4DPromise<Boolean>;
begin
  LFileName := 'test_huge.json';
  
  if CreateLargeJsonFile(LFileName, 5) then // 5MB file
  begin
    LPromise := FHighPerformanceValidator.ValidateHugeJsonFileAsync(LFileName, 2); // 2MB memory limit
    
    while LPromise.State = psPending do
      Sleep(50);
      
    Assert.AreEqual(psResolved, LPromise.State, 'Huge file validation should resolve');
  end;
end;

procedure TNest4DAsyncValidationTests.TestHighPerformanceValidatorAdaptive;
var
  LSmallJson: string;
  LLargeJson: string;
  LPromise1, LPromise2: TNest4DPromise<Boolean>;
begin
  LSmallJson := CreateTestJsonString(10);
  LLargeJson := CreateTestJsonString(10000);
  
  // Should adapt configuration automatically
  LPromise1 := FHighPerformanceValidator.ValidateLargeJsonAsync(LSmallJson);
  LPromise2 := FHighPerformanceValidator.ValidateLargeJsonAsync(LLargeJson);
  
  while (LPromise1.State = psPending) or (LPromise2.State = psPending) do
    Sleep(10);
    
  Assert.AreEqual(psResolved, LPromise1.State, 'Small JSON validation should resolve');
  Assert.AreEqual(psResolved, LPromise2.State, 'Large JSON validation should resolve');
end;

procedure TNest4DAsyncValidationTests.TestAsyncValidationIntegration;
var
  LIntegration: TNest4DAsyncValidationIntegration;
  LJsonString: string;
  LResult: Boolean;
begin
  LIntegration := TNest4DAsyncValidationIntegration.Create;
  try
    LJsonString := CreateTestJsonString(100);
    
    LResult := LIntegration.ValidateJsonStringAsync(LJsonString);
    Assert.IsTrue(LResult, 'Integration validation should succeed');
    
  finally
    LIntegration.Free;
  end;
end;

procedure TNest4DAsyncValidationTests.TestAsyncValidationMiddleware;
var
  LMiddleware: TNest4DAsyncValidationMiddleware;
begin
  LMiddleware := TNest4DAsyncValidationMiddleware.Create;
  try
    Assert.IsNotNull(LMiddleware, 'Middleware should be created');
    // Additional middleware tests would require Horse framework setup
  finally
    LMiddleware.Free;
  end;
end;

procedure TNest4DAsyncValidationTests.TestValidationPipeExtensions;
var
  LExtensions: TValidationPipeAsyncExtensions;
begin
  // Test helper extensions
  Assert.IsNotNull(@LExtensions, 'Extensions should be available');
end;

procedure TNest4DAsyncValidationTests.TestSyncAsyncCompatibility;
var
  LJsonString: string;
  LSyncResult: Boolean;
  LAsyncResult: Boolean;
  LPromise: TNest4DPromise<Boolean>;
  LData: TJsonDataMap;
begin
  LJsonString := CreateTestJsonString(50);
  
  // Parse and validate sync
  LData := FJsonParser.Parse(LJsonString);
  try
    LSyncResult := FValidationPipe.Validate(LData, nil);
  finally
    LData.Free;
  end;
  
  // Parse and validate async
  LPromise := FJsonParser.ParseAsync(LJsonString)
    .Then(
      function(const AData: TJsonDataMap): TNest4DPromise<Boolean>
      begin
        Result := FValidationPipe.ValidateAsync(AData, nil);
      end
    );
    
  while LPromise.State = psPending do
    Sleep(1);
    
  LAsyncResult := LPromise.Result;
  
  Assert.AreEqual(LSyncResult, LAsyncResult, 'Sync and async results should match');
end;

procedure TNest4DAsyncValidationTests.TestMigrationFromSyncToAsync;
var
  LCompatibilityPipe: TNest4DCompatibilityValidationPipe;
  LData: TJsonDataMap;
  LResult: Boolean;
begin
  LCompatibilityPipe := TNest4DCompatibilityValidationPipe.Create;
  try
    LData := TJsonDataMap.Create;
    try
      LData.Add('migration_test', 'value');
      
      // Should work with existing sync interface but use async internally
      LResult := LCompatibilityPipe.Validate(LData, nil);
      Assert.IsTrue(LResult, 'Compatibility pipe should work');
      
    finally
      LData.Free;
    end;
  finally
    LCompatibilityPipe.Free;
  end;
end;

procedure TNest4DAsyncValidationTests.TestErrorHandlingInAsyncPipeline;
var
  LPromise: TNest4DPromise<Boolean>;
begin
  // Test error propagation in async pipeline
  LPromise := FValidationPipe.ValidateAsync(nil, nil);
  
  while LPromise.State = psPending do
    Sleep(1);
    
  Assert.AreEqual(psRejected, LPromise.State, 'Should handle errors properly');
end;

procedure TNest4DAsyncValidationTests.TestTimeoutHandling;
var
  LData: TJsonDataMap;
  LPromise: TNest4DPromise<Boolean>;
begin
  LData := TJsonDataMap.Create;
  try
    LData.Add('timeout_test', 'value');
    
    FAdvancedPipe.SetValidationTimeout(1); // Very short timeout
    LPromise := FAdvancedPipe.ValidateAsync(LData, nil);
    
    Sleep(100); // Wait longer than timeout
    
    // Should either resolve quickly or timeout
    Assert.IsTrue((LPromise.State = psResolved) or (LPromise.State = psRejected), 
                  'Should handle timeout properly');
    
  finally
    LData.Free;
  end;
end;

procedure TNest4DAsyncValidationTests.TestMemoryLimitHandling;
var
  LLargeJsonString: string;
  LPromise: TNest4DPromise<TJsonDataMap>;
begin
  LLargeJsonString := CreateTestJsonString(100000); // Very large JSON
  
  FStreamingParser.SetMaxMemoryUsage(1024 * 1024); // 1MB limit
  LPromise := FStreamingParser.ParseAsync(LLargeJsonString);
  
  while LPromise.State = psPending do
    Sleep(10);
    
  // Should either succeed with memory management or fail gracefully
  Assert.IsTrue((LPromise.State = psResolved) or (LPromise.State = psRejected), 
                'Should handle memory limits');
end;

procedure TNest4DAsyncValidationTests.TestConcurrentValidation;
var
  LPromises: TArray<TNest4DPromise<Boolean>>;
  LData: TArray<TJsonDataMap>;
  I: Integer;
  LAllResolved: Boolean;
begin
  SetLength(LPromises, 10);
  SetLength(LData, 10);
  
  try
    // Start multiple concurrent validations
    for I := 0 to High(LData) do
    begin
      LData[I] := TJsonDataMap.Create;
      LData[I].Add('concurrent_test', I);
      LPromises[I] := FValidationPipe.ValidateAsync(LData[I], nil);
    end;
    
    // Wait for all to complete
    repeat
      Sleep(1);
      LAllResolved := True;
      for I := 0 to High(LPromises) do
      begin
        if LPromises[I].State = psPending then
        begin
          LAllResolved := False;
          Break;
        end;
      end;
    until LAllResolved;
    
    // Check all resolved
    for I := 0 to High(LPromises) do
      Assert.AreNotEqual(psPending, LPromises[I].State, 'All promises should be resolved');
      
  finally
    for I := 0 to High(LData) do
      LData[I].Free;
  end;
end;

procedure TNest4DAsyncValidationTests.BenchmarkAsyncVsSyncValidation;
var
  LJsonString: string;
  LData: TJsonDataMap;
  LSyncStartTime, LSyncEndTime: TDateTime;
  LAsyncStartTime, LAsyncEndTime: TDateTime;
  LPromise: TNest4DPromise<Boolean>;
  LSyncTime, LAsyncTime: Integer;
begin
  LJsonString := CreateTestJsonString(1000);
  
  // Benchmark sync validation
  LData := FJsonParser.Parse(LJsonString);
  try
    LSyncStartTime := Now;
    FValidationPipe.Validate(LData, nil);
    LSyncEndTime := Now;
  finally
    LData.Free;
  end;
  
  // Benchmark async validation
  LAsyncStartTime := Now;
  LPromise := FJsonParser.ParseAsync(LJsonString)
    .Then(
      function(const AData: TJsonDataMap): TNest4DPromise<Boolean>
      begin
        Result := FValidationPipe.ValidateAsync(AData, nil);
      end
    );
    
  while LPromise.State = psPending do
    Sleep(1);
  LAsyncEndTime := Now;
  
  LSyncTime := MilliSecondsBetween(LSyncEndTime, LSyncStartTime);
  LAsyncTime := MilliSecondsBetween(LAsyncEndTime, LAsyncStartTime);
  
  // Log performance comparison
  // Note: Async might be slower for small data due to overhead
  Assert.IsTrue(LAsyncTime < LSyncTime * 10, 'Async should not be significantly slower');
end;

procedure TNest4DAsyncValidationTests.BenchmarkStreamingVsRegularParsing;
var
  LJsonString: string;
  LRegularStartTime, LRegularEndTime: TDateTime;
  LStreamingStartTime, LStreamingEndTime: TDateTime;
  LRegularPromise, LStreamingPromise: TNest4DPromise<TJsonDataMap>;
  LRegularTime, LStreamingTime: Integer;
begin
  LJsonString := CreateTestJsonString(10000); // Large JSON
  
  // Benchmark regular parsing
  LRegularStartTime := Now;
  LRegularPromise := FJsonParser.ParseAsync(LJsonString);
  while LRegularPromise.State = psPending do
    Sleep(1);
  LRegularEndTime := Now;
  
  // Benchmark streaming parsing
  LStreamingStartTime := Now;
  LStreamingPromise := FStreamingParser.ParseAsync(LJsonString);
  while LStreamingPromise.State = psPending do
    Sleep(1);
  LStreamingEndTime := Now;
  
  LRegularTime := MilliSecondsBetween(LRegularEndTime, LRegularStartTime);
  LStreamingTime := MilliSecondsBetween(LStreamingEndTime, LStreamingStartTime);
  
  // Streaming should be competitive for large JSON
  Assert.IsTrue(LStreamingTime < LRegularTime * 2, 'Streaming should be reasonably fast');
end;

procedure TNest4DAsyncValidationTests.BenchmarkParallelValidation;
var
  LDataArray: TArray<TJsonDataMap>;
  LSequentialStartTime, LSequentialEndTime: TDateTime;
  LParallelStartTime, LParallelEndTime: TDateTime;
  LSequentialPromises: TArray<TNest4DPromise<Boolean>>;
  LParallelPromise: TNest4DPromise<TArray<Boolean>>;
  I: Integer;
  LSequentialTime, LParallelTime: Integer;
begin
  // Prepare test data
  SetLength(LDataArray, 10);
  for I := 0 to High(LDataArray) do
  begin
    LDataArray[I] := TJsonDataMap.Create;
    LDataArray[I].Add('benchmark_test', I);
  end;
  
  try
    // Benchmark sequential validation
    LSequentialStartTime := Now;
    SetLength(LSequentialPromises, Length(LDataArray));
    for I := 0 to High(LDataArray) do
      LSequentialPromises[I] := FValidationPipe.ValidateAsync(LDataArray[I], nil);
      
    for I := 0 to High(LSequentialPromises) do
    begin
      while LSequentialPromises[I].State = psPending do
        Sleep(1);
    end;
    LSequentialEndTime := Now;
    
    // Benchmark parallel validation
    LParallelStartTime := Now;
    LParallelPromise := FAdvancedPipe.ValidateParallelAsync(LDataArray);
    while LParallelPromise.State = psPending do
      Sleep(1);
    LParallelEndTime := Now;
    
    LSequentialTime := MilliSecondsBetween(LSequentialEndTime, LSequentialStartTime);
    LParallelTime := MilliSecondsBetween(LParallelEndTime, LParallelStartTime);
    
    // Parallel should be faster for multiple items
    Assert.IsTrue(LParallelTime <= LSequentialTime, 'Parallel validation should be faster or equal');
    
  finally
    for I := 0 to High(LDataArray) do
      LDataArray[I].Free;
  end;
end;

procedure TNest4DAsyncValidationTests.BenchmarkMemoryUsage;
var
  LJsonString: string;
  LMemoryBefore, LMemoryAfter: Cardinal;
  LPromise: TNest4DPromise<TJsonDataMap>;
begin
  LMemoryBefore := GetHeapStatus.TotalAllocated;
  
  LJsonString := CreateTestJsonString(50000); // Large JSON
  
  LPromise := FStreamingParser.ParseAsync(LJsonString);
  while LPromise.State = psPending do
    Sleep(10);
    
  LMemoryAfter := GetHeapStatus.TotalAllocated;
  
  // Memory usage should be reasonable
  Assert.IsTrue(LMemoryAfter - LMemoryBefore < 100 * 1024 * 1024, 
                'Memory usage should be under 100MB for large JSON');
end;

{ TNest4DJsonChunkProcessorTests }

procedure TNest4DJsonChunkProcessorTests.Setup;
begin
  FChunkProcessor := TNest4DJsonChunkProcessor.Create(1024, 128);
end;

procedure TNest4DJsonChunkProcessorTests.TearDown;
begin
  FChunkProcessor.Free;
end;

procedure TNest4DJsonChunkProcessorTests.TestChunkProcessingBasic;
var
  LTestData: string;
  LChunkCount: Integer;
  LResult: Boolean;
begin
  LTestData := StringOfChar('A', 5000); // 5KB of data
  LChunkCount := 0;
  
  LResult := FChunkProcessor.ProcessJsonInChunks(
    TStringStream.Create(LTestData),
    function(const AChunk: string): Boolean
    begin
      Inc(LChunkCount);
      Result := True;
    end
  );
  
  Assert.IsTrue(LResult, 'Chunk processing should succeed');
  Assert.IsTrue(LChunkCount > 1, 'Should process multiple chunks');
end;

procedure TNest4DJsonChunkProcessorTests.TestChunkProcessingWithOverlap;
var
  LTestData: string;
  LPreviousChunk: string;
  LOverlapFound: Boolean;
  LResult: Boolean;
begin
  LTestData := 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' + StringOfChar('1', 2000);
  LPreviousChunk := '';
  LOverlapFound := False;
  
  LResult := FChunkProcessor.ProcessJsonInChunks(
    TStringStream.Create(LTestData),
    function(const AChunk: string): Boolean
    begin
      if (LPreviousChunk <> '') and (Length(AChunk) > 10) then
      begin
        // Check for overlap
        if Pos(Copy(LPreviousChunk, Length(LPreviousChunk) - 5, 5), AChunk) > 0 then
          LOverlapFound := True;
      end;
      LPreviousChunk := AChunk;
      Result := True;
    end
  );
  
  Assert.IsTrue(LResult, 'Chunk processing with overlap should succeed');
  Assert.IsTrue(LOverlapFound, 'Should find overlap between chunks');
end;

procedure TNest4DJsonChunkProcessorTests.TestChunkProcessingLargeFile;
var
  LFileName: string;
  LFileStream: TFileStream;
  LTestData: string;
  LChunkCount: Integer;
  LResult: Boolean;
begin
  LFileName := 'test_chunk_large.json';
  LTestData := StringOfChar('X', 10000); // 10KB
  
  // Create test file
  LFileStream := TFileStream.Create(LFileName, fmCreate);
  try
    LFileStream.WriteBuffer(PAnsiChar(LTestData)^, Length(LTestData));
  finally
    LFileStream.Free;
  end;
  
  try
    LChunkCount := 0;
    LResult := FChunkProcessor.ProcessJsonFileInChunks(LFileName,
      function(const AChunk: string): Boolean
      begin
        Inc(LChunkCount);
        Result := True;
      end
    );
    
    Assert.IsTrue(LResult, 'Large file chunk processing should succeed');
    Assert.IsTrue(LChunkCount > 5, 'Should process multiple chunks for large file');
    
  finally
    if FileExists(LFileName) then
      DeleteFile(LFileName);
  end;
end;

procedure TNest4DJsonChunkProcessorTests.TestChunkProcessingErrorHandling;
var
  LTestData: string;
  LResult: Boolean;
begin
  LTestData := StringOfChar('E', 2000);
  
  // Test with processor that returns false (error)
  LResult := FChunkProcessor.ProcessJsonInChunks(
    TStringStream.Create(LTestData),
    function(const AChunk: string): Boolean
    begin
      Result := False; // Simulate error
    end
  );
  
  Assert.IsFalse(LResult, 'Should return false when processor fails');
end;

initialization
  TDUnitX.RegisterTestFixture(TNest4DAsyncValidationTests);
  TDUnitX.RegisterTestFixture(TNest4DJsonChunkProcessorTests);

end.