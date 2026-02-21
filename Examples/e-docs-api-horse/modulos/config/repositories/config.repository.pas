unit config.repository;

interface

uses
  SysUtils,
  IniFiles,
  nfebr.config,
  core.types,
  config.infra,
  config.interfaces;

type
  TConfigRepository = class
  private
    FInfra: TConfigInfra;
    function _ConfigFilePath: String;
  public
    constructor Create(const AInfra: TConfigInfra);
    destructor Destroy; override;
    function Find: String;
    function Insert(const AJson: String): String;
    function Update(const AJson: String): String;
    function Delete: String;
  end;

implementation

{ TConfigRepository }

constructor TConfigRepository.Create(const AInfra: TConfigInfra);
begin
  FInfra := AInfra;
end;

destructor TConfigRepository.Destroy;
begin
  FInfra.Free;
  inherited;
end;

function TConfigRepository.Delete: String;
var
  LFile: String;
begin
  LFile := _ConfigFilePath;
  try
    if DirectoryExists('.\data') then
      if FileExists(LFile) then
        DeleteFile(LFile);
    Result := 'Configurações deletada com sucesso!';
  except
    raise Exception.Create('Houve um erro ao deletar as configurações!');
  end;
end;

function TConfigRepository.Find: String;
var
  LFile: String;
  LConfig: TNFeConfig;
begin
  LFile := _ConfigFilePath;
  LConfig := TNFeConfig.Create;
  LConfig.LoadConfig(LFile);
  try
    try
      Result := FInfra.ToJson(LConfig);
    except
      raise Exception.Create('Houve um erro ao fazer o parser as configuarações!');
    end;
  finally
    LConfig.Free;
  end;
end;

function TConfigRepository.Insert(const AJson: String): String;
var
  LFile: String;
  LConfig: TNFeConfig;
begin
  LConfig := FInfra.FromJson<TNFeConfig>(Ajson);
  LFile := _ConfigFilePath;
  try
    LConfig.SaveConfig(LFile);
    Result := 'Configurações salva com sucesso!';
  except
    raise Exception.Create('Houve um erro na gravar as configurações!');
  end;
end;

function TConfigRepository.Update(const AJson: String): String;
var
  LFile: String;
  LConfig: TNFeConfig;
begin
  LConfig := FInfra.FromJson<TNFeConfig>(Ajson);
  LFile := _ConfigFilePath;
  try
    LConfig.SaveConfig(LFile);
    Result := 'Configurações alterada com sucesso!';
  except
    raise Exception.Create('Houve um erro ao gravar as alterações das configurações!');
  end;
end;

function TConfigRepository._ConfigFilePath: String;
begin
  if not DirectoryExists('.\data') then
    CreateDir('.\data');
  Result := '.\data\config.ini';
end;

end.

