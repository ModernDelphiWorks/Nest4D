unit Test.Nest4D.Logging;

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Classes,
  Nest4D.logging;

[TestFixture]
TTestNest4DLogging = class
private
  FLogger: IStructuredLogger;
public
  [Setup]
  procedure Setup;
  [TearDown]
  procedure TearDown;
  
  [Test]
  procedure TestLoggerCreation;
  
  [Test]
  procedure TestLogInfo;
  
  [Test]
  procedure TestLogError;
  
  [Test]
  procedure TestLogWithContext;
  
  [Test]
  procedure TestLogLevels;
end;

implementation

procedure TTestNest4DLogging.Setup;
begin
  FLogger := GetLogger;
end;

procedure TTestNest4DLogging.TearDown;
begin
  FLogger := nil;
end;

procedure TTestNest4DLogging.TestLoggerCreation;
begin
  Assert.IsNotNull(FLogger, 'Logger should be created successfully');
end;

procedure TTestNest4DLogging.TestLogInfo;
begin
  Assert.WillNotRaise(
    procedure
    begin
      FLogger.Info('Test info message');
    end,
    'Info logging should not raise exception'
  );
end;

procedure TTestNest4DLogging.TestLogError;
begin
  Assert.WillNotRaise(
    procedure
    begin
      FLogger.Error('Test error message');
    end,
    'Error logging should not raise exception'
  );
end;

procedure TTestNest4DLogging.TestLogWithContext;
var
  LContext: TLogContext;
begin
  LContext := TLogContext.Create;
  try
    LContext.Add('RequestId', 'test-123');
    LContext.Add('UserId', '456');
    
    Assert.WillNotRaise(
      procedure
      begin
        FLogger.InfoWithContext('Test message with context', LContext);
      end,
      'Logging with context should not raise exception'
    );
  finally
    LContext.Free;
  end;
end;

procedure TTestNest4DLogging.TestLogLevels;
begin
  Assert.WillNotRaise(
    procedure
    begin
      FLogger.Debug('Debug message');
      FLogger.Info('Info message');
      FLogger.Warn('Warning message');
      FLogger.Error('Error message');
    end,
    'All log levels should work without exception'
  );
end;

initialization
  TDUnitX.RegisterTestFixture(TTestNest4DLogging);

end.