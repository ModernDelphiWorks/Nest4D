unit pdf.infra;

interface

uses
  SysUtils,
  Evolution4D.Std,
  core.types,
  nfebr.lib.json,
  nfebr.lib.nfe,
  pdf.interfaces,
  nfebr.lib.include;

type
  TPDFInfra = class
  private
    FNFeLib: INFeLib;
    FJsonLib: TJsonLib;
    FNFeConfig: TNFeConfig;
    procedure _SetFNFeConfig(const Value: TNFeConfig);
  public
    constructor Create(const ANFeLib: INFeLib; const AJsonLib: TJsonLib);
    destructor Destroy; override;
    function NFePDF(const AChave: String): String;
    function NFeCancelamentoPDF(const AChave: String): String;
    function NFeInutilizacaoPDF(const AProtocolo: String): String;
    function NFeCartaCorrecaoPDF(const AChave: String): String;
    //
    property NFeConfig: TNFeConfig read FNFeConfig write _SetFNFeConfig;
  end;

implementation

uses
  Evolution4D.Threading,
  nfebr.lib.utils;

{ TPDFInfra }

constructor TPDFInfra.Create(const ANFeLib: INFeLib; const AJsonLib: TJsonLib);
begin
  FNFeLib := ANFeLib;
  FJsonLib := AJsonLib;
end;

destructor TPDFInfra.Destroy;
begin
  FNFeLib := nil;
  FJsonLib := nil;
  inherited;
end;

function TPDFInfra.NFeCancelamentoPDF(const AChave: String): String;
begin
//  FNFeLib.Cancela()
  Result := 'NFeCancelamentoPDF';
end;

function TPDFInfra.NFeCartaCorrecaoPDF(const AChave: String): String;
begin
//  FNFeLib.Cancela()
  Result := 'NFeCartaCorrecaoPDF';
end;

function TPDFInfra.NFeInutilizacaoPDF(const AProtocolo: String): String;
begin
//  FNFeLib.Cancela()
  Result := 'NFeInutilizacaoPDF';
end;

function TPDFInfra.NFePDF(const AChave: String): String;
var
  LFuture: TFuture;
  LFileName: String;
begin
  LFileName := AChave + '-nfe.pdf';
  LFuture := Async(function: TValue
                   begin
                     Result := TUtils.FindFile(FNFeConfig.DFePath.FilePDF, LFileName);
                   end).Await();
  if LFuture.IsOk then
    Result := LFuture.Ok<String>
  else
    raise Exception.Create(LFuture.Err);
end;

procedure TPDFInfra._SetFNFeConfig(const Value: TNFeConfig);
begin
  FNFeConfig := Value;
end;

end.
