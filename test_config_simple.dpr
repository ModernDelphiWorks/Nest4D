program test_config_simple;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  Nest4D.Config in 'Source\Core\Configuration\Nest4D.Config.pas',
  Nest4D.Config.Defaults in 'Source\Core\Configuration\Nest4D.Config.Defaults.pas',
  Nest4D.Config.Loader in 'Source\Core\Configuration\Nest4D.Config.Loader.pas',
  Nest4D.Config.Environment in 'Source\Core\Configuration\Nest4D.Config.Environment.pas',
  Nest4D.Config.Manager in 'Source\Core\Configuration\Nest4D.Config.Manager.pas',
  Nest4D.Config.Validator in 'Source\Core\Configuration\Nest4D.Config.Validator.pas';

var
  Config: TNest4DConfig;
begin
  try
    // Teste simples de criação de configuração
    Config := TNest4DConfigDefaults.CreateDevelopmentConfig;
    try
      Writeln('Configuração criada com sucesso!');
      Writeln('Porta: ', Config.Framework.Port);
    finally
      Config.Free;
    end;
  except
    on E: Exception do
      Writeln('Erro: ', E.Message);
  end;
  
  Writeln('Pressione Enter para sair...');
  Readln;
end.