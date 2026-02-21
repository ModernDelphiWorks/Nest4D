unit core.exception;

interface

uses
  SysUtils;

type
  ERequestException = class(Exception)
  private
    FStatus: integer;
    procedure SetStatus(const Value: integer);
  public
    constructor Create(const Msg: String = '';
      const ACode: integer = 0); reintroduce;
    property Status: integer read FStatus write SetStatus;
    constructor CreateFmt(const Msg: String; const Args: array of const;
      const ACode: integer = 0); reintroduce;
  end;

  EReqTransmiteException = class(ERequestException)
  public
    constructor Create(const Msg: String);
  end;
  EReqCancultaException = class(ERequestException);

  EReqTransmiteLoteException = class(ERequestException);
  EReqConsultaLoteException = class(ERequestException);

  EReqCancelaException = class(ERequestException);
  EReqCancelaConsultaException = class(ERequestException);

  EReqInutilizaException = class(ERequestException);
  EReqInutilizaConsultaException = class(ERequestException);

  EReqCCeException = class(ERequestException);
  EReqCCeConsultaException = class(ERequestException);

  EReqServicoStatuException = class(ERequestException);

  EPDFNotExistException = class(ERequestException)
  public
    constructor Create(const Msg: String); reintroduce;
  end;

  EXMLNotExistException = class(ERequestException)
  public
    constructor Create(const Msg: String); reintroduce;
  end;

implementation

{ ERequestException }

constructor ERequestException.Create(const Msg: String; const ACode: integer);
begin
  inherited Create(Msg);
  FStatus := ACode;
end;

constructor ERequestException.CreateFmt(const Msg: String;
  const Args: array of const; const ACode: integer);
begin
  inherited CreateFmt(Msg, Args);
  FStatus := ACode;
end;

procedure ERequestException.SetStatus(const Value: integer);
begin
  FStatus := Value;
end;

{ EReqTransmiteException }

constructor EReqTransmiteException.Create(const Msg: String);
begin
  // Pegar a mensagen texto e tratar ela no formato que desejar devolver a requisição
end;

{ EPDFNotExistException }

constructor EPDFNotExistException.Create(const Msg: String);
var
  LMsg: String;
begin
  LMsg := '{' + sLineBreak +
          '"PDF": "' + Msg + '"' + sLineBreak +
          '}';
  inherited Create(LMsg, 404);
end;

{ EXMLException }

constructor EXMLNotExistException.Create(const Msg: String);
var
  LMsg: String;
begin
  LMsg := '{' + sLineBreak +
          '"XML": "' + Msg + '"' + sLineBreak +
          '}';
  inherited Create(LMsg, 404);
end;

end.
