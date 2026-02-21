unit Test.Nest4D.Health;

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.JSON,
  Nest4D.health;

[TestFixture]
TTestNest4DHealth = class
private
  FHealthService: IHealthService;
public
  [Setup]
  procedure Setup;
  [TearDown]
  procedure TearDown;
  
  [Test]
  procedure TestHealthServiceCreation;
  
  [Test]
  procedure TestHealthCheckRegistration;
  
  [Test]
  procedure TestHealthStatus;
  
  [Test]
  procedure TestHealthReport;
  
  [Test]
  procedure TestMultipleHealthChecks;
end;

implementation

procedure TTestNest4DHealth.Setup;
begin
  FHealthService := GetHealthService;
end;

procedure TTestNest4DHealth.TearDown;
begin
  FHealthService := nil;
end;

procedure TTestNest4DHealth.TestHealthServiceCreation;
begin
  Assert.IsNotNull(FHealthService, 'Health service should be created successfully');
end;

procedure TTestNest4DHealth.TestHealthCheckRegistration;
begin
  Assert.WillNotRaise(
    procedure
    begin
      FHealthService.RegisterCheck('database', 
        function: THealthStatus
        begin
          Result := hsHealthy;
        end
      );
    end,
    'Health check registration should not raise exception'
  );
end;

procedure TTestNest4DHealth.TestHealthStatus;
var
  LStatus: THealthStatus;
begin
  // Register a simple health check
  FHealthService.RegisterCheck('test_service', 
    function: THealthStatus
    begin
      Result := hsHealthy;
    end
  );
  
  Assert.WillNotRaise(
    procedure
    begin
      LStatus := FHealthService.GetOverallStatus;
    end,
    'Getting health status should not raise exception'
  );
  
  Assert.AreEqual(Ord(hsHealthy), Ord(LStatus), 'Health status should be healthy');
end;

procedure TTestNest4DHealth.TestHealthReport;
var
  LReport: string;
begin
  // Register a health check
  FHealthService.RegisterCheck('api_service', 
    function: THealthStatus
    begin
      Result := hsHealthy;
    end
  );
  
  Assert.WillNotRaise(
    procedure
    begin
      LReport := FHealthService.GetHealthReport;
    end,
    'Getting health report should not raise exception'
  );
  
  Assert.IsNotEmpty(LReport, 'Health report should not be empty');
  Assert.IsTrue(LReport.Contains('"status"'), 'Health report should contain status field');
end;

procedure TTestNest4DHealth.TestMultipleHealthChecks;
var
  LStatus: THealthStatus;
begin
  // Register multiple health checks
  FHealthService.RegisterCheck('database', 
    function: THealthStatus
    begin
      Result := hsHealthy;
    end
  );
  
  FHealthService.RegisterCheck('cache', 
    function: THealthStatus
    begin
      Result := hsHealthy;
    end
  );
  
  FHealthService.RegisterCheck('external_api', 
    function: THealthStatus
    begin
      Result := hsDegraded; // Simulate degraded service
    end
  );
  
  LStatus := FHealthService.GetOverallStatus;
  
  // With one degraded service, overall status should be degraded
  Assert.AreEqual(Ord(hsDegraded), Ord(LStatus), 
    'Overall status should be degraded when one service is degraded');
end;

initialization
  TDUnitX.RegisterTestFixture(TTestNest4DHealth);

end.