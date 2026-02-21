program test_config;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  Nest4D.Exception in 'Source\Core\Nest4D.Exception.pas',
  Nest4D.Config in 'Source\Core\Configuration\Nest4D.Config.pas',
  Nest4D.Config.Defaults in 'Source\Core\Configuration\Nest4D.Config.Defaults.pas',
  Nest4D.Config.Loader in 'Source\Core\Configuration\Nest4D.Config.Loader.pas',
  Nest4D.Config.Environment in 'Source\Core\Configuration\Nest4D.Config.Environment.pas',
  Nest4D.Config.Manager in 'Source\Core\Configuration\Nest4D.Config.Manager.pas',
  Nest4D.Config.Validator in 'Source\Core\Configuration\Nest4D.Config.Validator.pas',
  Nest4D.Logging in 'Source\Core\Monitoring\Nest4D.Logging.pas';

var
  Config: TNest4DConfig;
begin
  try
    Config := TNest4DConfigDefaults.Development;
    try
      Writeln('Configuration test successful!');
      Writeln('Logging enabled: ', Config.Logging.Enabled);
      Writeln('Pool min size: ', Config.HandlerPool.MinSize);
    finally
      Config.Free;
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.