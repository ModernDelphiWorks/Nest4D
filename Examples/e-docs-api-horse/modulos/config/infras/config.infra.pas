unit config.infra;

interface

uses
  SysUtils,
  Generics.Collections,
  Evolution4D.Std,
  nfebr.lib.json,
  nfebr.lib.nfe,
  nfebr.lib.include,
  core.types,
  config.interfaces,
  nfebr.config;

type
  TConfigInfra = class
  private
    FNFeLib: INFeLib;
    FJsonLib: TJsonLib;
  public
    constructor Create(const ANFeLib: INFeLib; const AJsonLib: TJsonLib);
    destructor Destroy; override;
    // Json
    function FromJson<T: class, constructor>(const AJson: String): T;
    function ToJson<T: class, constructor>(const AObject: T): String;
  end;

implementation

uses
  Evolution4D.Threading;

{ TConfigInfra }

constructor TConfigInfra.Create(const ANFeLib: INFeLib;
  const AJsonLib: TJsonLib);
begin
  FNFeLib := ANFeLib;
  FJsonLib := AJsonLib;
end;

destructor TConfigInfra.Destroy;
begin
  FNFeLib := nil;
  FJsonLib := nil;
  inherited;
end;

function TConfigInfra.FromJson<T>(const AJson: String): T;
var
  LFuture: TFuture;
begin
  LFuture := Async(function: TValue
                   begin
                     Result := FJsonLib.FromJson<T>(Ajson);
                   end).Await();
  if LFuture.IsOk then
    Result := LFuture.Ok<T>
  else
    raise Exception.Create(LFuture.Err);
end;

function TConfigInfra.ToJson<T>(const AObject: T): String;
var
  LFuture: TFuture;
begin
  LFuture := Async(function: TValue
                   begin
                     Result := FJsonLib.ToJson<T>(AObject);
                   end).Await();
  if LFuture.IsOk then
    Result := LFuture.Ok<String>
  else
    raise Exception.Create(LFuture.Err);
end;

end.

