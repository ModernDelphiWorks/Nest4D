unit Test.Nest4D.Metrics;

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  Nest4D.metrics;

[TestFixture]
TTestNest4DMetrics = class
private
  FMetrics: IMetricsCollector;
public
  [Setup]
  procedure Setup;
  [TearDown]
  procedure TearDown;
  
  [Test]
  procedure TestMetricsCreation;
  
  [Test]
  procedure TestCounterIncrement;
  
  [Test]
  procedure TestHistogramRecord;
  
  [Test]
  procedure TestGaugeSet;
  
  [Test]
  procedure TestMetricsExport;
end;

implementation

procedure TTestNest4DMetrics.Setup;
begin
  FMetrics := GetMetricsCollector;
end;

procedure TTestNest4DMetrics.TearDown;
begin
  FMetrics := nil;
end;

procedure TTestNest4DMetrics.TestMetricsCreation;
begin
  Assert.IsNotNull(FMetrics, 'Metrics collector should be created successfully');
end;

procedure TTestNest4DMetrics.TestCounterIncrement;
begin
  Assert.WillNotRaise(
    procedure
    begin
      FMetrics.IncrementCounter('test_counter');
      FMetrics.IncrementCounter('test_counter', 5);
    end,
    'Counter increment should not raise exception'
  );
end;

procedure TTestNest4DMetrics.TestHistogramRecord;
begin
  Assert.WillNotRaise(
    procedure
    begin
      FMetrics.RecordHistogram('test_histogram', 100.5);
      FMetrics.RecordHistogram('test_histogram', 250.0);
    end,
    'Histogram recording should not raise exception'
  );
end;

procedure TTestNest4DMetrics.TestGaugeSet;
begin
  Assert.WillNotRaise(
    procedure
    begin
      FMetrics.SetGauge('test_gauge', 42.0);
      FMetrics.SetGauge('test_gauge', 84.5);
    end,
    'Gauge setting should not raise exception'
  );
end;

procedure TTestNest4DMetrics.TestMetricsExport;
var
  LExportedMetrics: string;
begin
  // Record some test metrics
  FMetrics.IncrementCounter('http_requests_total');
  FMetrics.RecordHistogram('http_request_duration_ms', 150.0);
  FMetrics.SetGauge('active_connections', 10.0);
  
  Assert.WillNotRaise(
    procedure
    begin
      LExportedMetrics := FMetrics.ExportMetrics;
    end,
    'Metrics export should not raise exception'
  );
  
  Assert.IsNotEmpty(LExportedMetrics, 'Exported metrics should not be empty');
end;

initialization
  TDUnitX.RegisterTestFixture(TTestNest4DMetrics);

end.