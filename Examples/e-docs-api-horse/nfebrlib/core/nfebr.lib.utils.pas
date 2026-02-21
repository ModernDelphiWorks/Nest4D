unit nfebr.lib.utils;

interface

uses
  Types,
  SysUtils,
  DateUtils,
  IOUtils,
  nfebr.lib.enum,
  nfebr.lib.include;

type
  TUtils = class
  public
    class function GerarNumeroSequencial: Int64;
    class function MD5Simple(const AData: TDate;
      const ANr1: integer; const ANr2: integer;
      const Akey: String = 'MD5Simple'): String;
    class function GenerateUniqueNumber(const AData: TDate;
      const ANumeroNota: integer;
      const ASerieNota: integer;
      const ACodModelo: integer;
      const AUFCode: integer): integer; static;
    class function FindFile(const ADirStart, AFileName: String): String; static;
  end;

implementation

class function TUtils.GerarNumeroSequencial: Int64;
var
  LSequencia: Int64;
begin
  LSequencia := Trunc((Now - EncodeDate(2022, 1, 1)) * 86400);
  Result := StrToInt(Format('%010d', [LSequencia]));
end;

class function TUtils.MD5Simple(const AData: TDate; const ANr1: integer;
  const ANr2: integer; const Akey: String): String;
var
  LData: String;
  LCode: String;
  LHash: String;
  LFor: integer;
begin
  // Converter a data de emissão para uma String sem horas, minutos e segundos
  LData := FormatDateTime('YYYYMMDD', AData);
  // Concatenar as informações e o caractere
  LCode := LData + IntToStr(ANr1) + IntToStr(ANr2) + Akey;
  // Calcular o hash MD5 simples dos dados
  LHash := '';
  for LFor := 1 to Length(LCode) do
    LHash := LHash + IntToHex(Ord(LCode[LFor]), 2);
  Result := LHash;
end;

class function TUtils.GenerateUniqueNumber(const AData: TDate;
  const ANumeroNota: integer;
  const ASerieNota: integer;
  const ACodModelo: integer;
  const AUFCode: integer): integer;
var
  LData: String;
  LCode: String;
  LHash: Integer;
  LChar: Char;
begin
  // Converter a data de emissão para uma String sem horas, minutos e segundos
  LData := FormatDateTime('YYYYMMDD', AData);
  // Concatenar as informações e a chave
  LCode := LData +
           IntToStr(ANumeroNota) +
           IntToStr(ASerieNota) +
           IntToStr(ACodModelo) +
           IntToStr(AUFCode) +
           'DFe';
  // Calcular um hash simples dos dados
  LHash := 0;
  for LChar in LCode do
    LHash := LHash + Ord(LChar);
  // Garantir que o número seja positivo e com no máximo 8 dígitos
  LHash := Abs(LHash) mod 100000000;
   // Considerar apenas os 8 primeiros dígitos
  Result := LHash mod 10000000;
end;

class function TUtils.FindFile(const ADirStart: String;
  const AFileName: String): String;
var
  LDirs: TStringDynArray;
  LDir: String;
  LPathFile: String;
begin
  Result := '';
  LPathFile := TPath.Combine(ADirStart, AFileName);
  if TFile.Exists(LPathFile) then
  begin
    Result := LPathFile;
    exit;
  end;
  LDirs := TDirectory.GetDirectories(ADirStart, '*', TSearchOption.soAllDirectories);
  for LDir in LDirs do
  begin
    LPathFile := TPath.Combine(LDir, AFileName);
    if TFile.Exists(LPathFile) then
    begin
      Result := LPathFile;
      break;
    end;
  end;
end;

end.
