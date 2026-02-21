program nest4d_core_tests;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  DUnitX.Loggers.Console,
  DUnitX.Loggers.Xml.NUnit,
  DUnitX.TestFramework,
  // Nest4D Core Units
  nest4d.logging in '..\Source\Core\nest4d.logging.pas',
  nest4d.metrics in '..\Source\Core\nest4d.metrics.pas',
  nest4d.health in '..\Source\Core\nest4d.health.pas',
  nest4d.cache in '..\Source\Core\nest4d.cache.pas',
  nest4d.handler.pool in '..\Source\Core\nest4d.handler.pool.pas',
  Nest4D.Pool.Config in '..\Source\Core\Infrastructure\Nest4D.Pool.Config.pas',
  // Resilience System Units
  nest4d.resilience.interfaces in '..\Source\Core\nest4d.resilience.interfaces.pas',
  nest4d.resilience.retry in '..\Source\Core\nest4d.resilience.retry.pas',
  nest4d.resilience.fallback in '..\Source\Core\nest4d.resilience.fallback.pas',
  nest4d.resilience.circuitbreaker in '..\Source\Core\nest4d.resilience.circuitbreaker.pas',
  nest4d.resilience.interceptor in '..\Source\Core\nest4d.resilience.interceptor.pas',
  nest4d.resilience.horse in '..\Source\Core\nest4d.resilience.horse.pas',
  // JsonFlow - Implementação própria do Nest4D
  nest4d.validation.parse.json in '..\Source\Pipes\Core\nest4d.validation.parse.json.pas',
  // Async Validation System
  // Test Units
  Test.Nest4D.Logging in 'Test.Nest4D.Logging.pas',
  Test.Nest4D.Metrics in 'Test.Nest4D.Metrics.pas',
  Test.Nest4D.Health in 'Test.Nest4D.Health.pas',
  Test.Nest4D.Cache in 'Test.Nest4D.Cache.pas',
  Test.Nest4D.Pool in 'Test.Nest4D.Pool.pas',
  Test.Nest4D.Resilience in 'Test.Nest4D.Resilience.pas',
  Test.Nest4D.Resilience.Integration in 'Test.Nest4D.Resilience.Integration.pas',
  Test.Nest4D.AsyncValidation in 'Test.Nest4D.AsyncValidation.pas',
  Test.Nest4D.AsyncValidation.Integration in 'Test.Nest4D.AsyncValidation.Integration.pas';

var
  runner: ITestRunner;
  results: IRunResults;
  logger: ITestLogger;
  nunitLogger : ITestLogger;
begin
  try
    //Check command line options, will exit if invalid
    TDUnitX.CheckCommandLine;
    //Create the test runner
    runner := TDUnitX.CreateRunner;
    //Tell the runner to use RTTI to find Fixtures
    runner.UseRTTI := True;
    //tell the runner how we will log things
    //Log to the console window
    logger := TDUnitXConsoleLogger.Create(true);
    runner.AddLogger(logger);
    //Generate an NUnit compatible XML File
    nunitLogger := TDUnitXXMLNUnitFileLogger.Create(TDUnitX.Options.XMLOutputFile);
    runner.AddLogger(nunitLogger);
    runner.FailsOnNoAsserts := False; //When true, Assertions must be made during tests;

    //Run tests
    results := runner.Execute;
    if not results.AllPassed then
      System.ExitCode := EXIT_ERRORS;

    {$IFNDEF CI}
    //We don't want this happening when running under CI.
    if TDUnitX.Options.ExitBehavior = TDUnitXExitBehavior.Pause then
    begin
      System.Write('Done.. press <Enter> key to quit.');
      System.Readln;
    end;
    {$ENDIF}
  except
    on E: Exception do
    begin
      System.Writeln(E.ClassName, ': ', E.Message);
      System.ExitCode := EXIT_ERRORS;
    end;
  end;
end.