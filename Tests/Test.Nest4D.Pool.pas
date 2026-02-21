unit Test.Nest4D.Pool;

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  Nest4D.handler.pool,
  Nest4D.Pool.Config;

[TestFixture]
TTestNest4DPool = class
private
  FPool: IHandlerPool;
  FConfig: TPoolConfig;
public
  [Setup]
  procedure Setup;
  [TearDown]
  procedure TearDown;
  
  [Test]
  procedure TestPoolCreation;
  
  [Test]
  procedure TestPoolConfiguration;
  
  [Test]
  procedure TestHandlerAcquisition;
  
  [Test]
  procedure TestHandlerRelease;
  
  [Test]
  procedure TestPoolCapacity;
  
  [Test]
  procedure TestPoolStatistics;
end;

implementation

procedure TTestNest4DPool.Setup;
begin
  FConfig := TPoolConfig.Create;
  FConfig.MinSize := 2;
  FConfig.MaxSize := 10;
  FConfig.GrowthFactor := 2;
  FConfig.IdleTimeout := 30000; // 30 seconds
  
  FPool := CreateHandlerPool(FConfig);
end;

procedure TTestNest4DPool.TearDown;
begin
  FPool := nil;
  FConfig.Free;
end;

procedure TTestNest4DPool.TestPoolCreation;
begin
  Assert.IsNotNull(FPool, 'Handler pool should be created successfully');
end;

procedure TTestNest4DPool.TestPoolConfiguration;
var
  LStats: TPoolStatistics;
begin
  Assert.WillNotRaise(
    procedure
    begin
      LStats := FPool.GetStatistics;
    end,
    'Getting pool statistics should not raise exception'
  );
  
  // Pool should start with minimum size
  Assert.AreEqual(FConfig.MinSize, LStats.TotalHandlers, 
    'Pool should start with minimum number of handlers');
end;

procedure TTestNest4DPool.TestHandlerAcquisition;
var
  LHandler: IRequestHandler;
begin
  Assert.WillNotRaise(
    procedure
    begin
      LHandler := FPool.AcquireHandler;
    end,
    'Acquiring handler should not raise exception'
  );
  
  Assert.IsNotNull(LHandler, 'Acquired handler should not be nil');
end;

procedure TTestNest4DPool.TestHandlerRelease;
var
  LHandler: IRequestHandler;
  LStatsBefore, LStatsAfter: TPoolStatistics;
begin
  // Get initial statistics
  LStatsBefore := FPool.GetStatistics;
  
  // Acquire a handler
  LHandler := FPool.AcquireHandler;
  
  Assert.WillNotRaise(
    procedure
    begin
      FPool.ReleaseHandler(LHandler);
    end,
    'Releasing handler should not raise exception'
  );
  
  // Get statistics after release
  LStatsAfter := FPool.GetStatistics;
  
  // Available handlers should be back to original count
  Assert.AreEqual(LStatsBefore.AvailableHandlers, LStatsAfter.AvailableHandlers,
    'Available handlers count should be restored after release');
end;

procedure TTestNest4DPool.TestPoolCapacity;
var
  LHandlers: array of IRequestHandler;
  I: Integer;
  LStats: TPoolStatistics;
begin
  SetLength(LHandlers, FConfig.MaxSize + 2);
  
  // Try to acquire more handlers than max capacity
  for I := 0 to High(LHandlers) do
  begin
    LHandlers[I] := FPool.AcquireHandler;
    Assert.IsNotNull(LHandlers[I], Format('Handler %d should be acquired', [I]));
  end;
  
  LStats := FPool.GetStatistics;
  
  // Pool should not exceed maximum size
  Assert.IsTrue(LStats.TotalHandlers <= FConfig.MaxSize,
    'Pool should not exceed maximum configured size');
  
  // Release all handlers
  for I := 0 to High(LHandlers) do
  begin
    if Assigned(LHandlers[I]) then
      FPool.ReleaseHandler(LHandlers[I]);
  end;
end;

procedure TTestNest4DPool.TestPoolStatistics;
var
  LStats: TPoolStatistics;
  LHandler: IRequestHandler;
begin
  // Get initial statistics
  LStats := FPool.GetStatistics;
  Assert.AreEqual(FConfig.MinSize, LStats.TotalHandlers, 'Initial total handlers');
  Assert.AreEqual(FConfig.MinSize, LStats.AvailableHandlers, 'Initial available handlers');
  Assert.AreEqual(0, LStats.BusyHandlers, 'Initial busy handlers should be 0');
  
  // Acquire a handler
  LHandler := FPool.AcquireHandler;
  LStats := FPool.GetStatistics;
  
  Assert.AreEqual(1, LStats.BusyHandlers, 'Should have 1 busy handler');
  Assert.AreEqual(FConfig.MinSize - 1, LStats.AvailableHandlers, 
    'Available handlers should decrease by 1');
  
  // Release the handler
  FPool.ReleaseHandler(LHandler);
  LStats := FPool.GetStatistics;
  
  Assert.AreEqual(0, LStats.BusyHandlers, 'Busy handlers should be back to 0');
  Assert.AreEqual(FConfig.MinSize, LStats.AvailableHandlers, 
    'Available handlers should be back to initial count');
end;

initialization
  TDUnitX.RegisterTestFixture(TTestNest4DPool);

end.