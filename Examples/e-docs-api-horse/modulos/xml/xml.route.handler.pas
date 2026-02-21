unit xml.route.handler;

interface

uses
  Classes,
  core.include,
  core.types,
  core.exception,
  core.constantes,
  nest4d,
  Nest4D.route.handler.horse;

type
  TXMLRouteHandler = class(TRouteHandlerHorse)
  protected
    procedure RegisterRoutes; override;
  public
    constructor Create; override;
    procedure NFeTransmitidaXML(Req: THorseRequest; Res: THorseResponse);
    procedure NFeCancelamentoXML(Req: THorseRequest; Res: THorseResponse);
    procedure NFeInutilizacaoXML(Req: THorseRequest; Res: THorseResponse);
    procedure NFeCartaCorrecaoXML(Req: THorseRequest; Res: THorseResponse);
  end;

implementation

uses
  app.route,
  app.module,
  Nest4D.horse,
  xml.controller;

{ TXMLRouteHandler }

procedure TXMLRouteHandler.RegisterRoutes;
begin
  inherited;
  RouteGet(Rota.NFeTransmitidaXML, NFeTransmitidaXML)
    .RouteGet(Rota.NFeCancelamentoXML, NFeCancelamentoXML)
    .RouteGet(Rota.NFeInutilizacaoXML, NFeInutilizacaoXML)
    .RouteGet(Rota.NFeCartaCorrecaoXML, NFeCartaCorrecaoXML);
end;

constructor TXMLRouteHandler.Create;
begin
  inherited;

end;

procedure TXMLRouteHandler.NFeCancelamentoXML(Req: THorseRequest; Res: THorseResponse);
var
  LChave: String;
  LResult: TXMLResponse;
begin
  LChave := Req.Params['chave'];
  LResult := GetNest4D.Get<TXMLController>.NFeCancelamentoXML(LChave);
  LResult.When(
    procedure (XML: TFileStream)
    begin
      Res.Send(XML).ContentType(CONTENTTYPE_XML).Status(200);
    end,
    procedure (Error: ERequestException)
    begin
      try
        raise ERequestException.Create(Error.Message, Error.Status);
      finally
        Error.Free;
      end;
    end);
end;

procedure TXMLRouteHandler.NFeCartaCorrecaoXML(Req: THorseRequest; Res: THorseResponse);
var
  LChave: String;
  LResult: TXMLResponse;
begin
  LChave := Req.Params['chave'];
  LResult := GetNest4D.Get<TXMLController>.NFeCartaCorrecaoXML(LChave);
  LResult.When(
    procedure (XML: TFileStream)
    begin
      Res.Send(XML).ContentType(CONTENTTYPE_XML).Status(200);
    end,
    procedure (Error: ERequestException)
    begin
      try
        raise ERequestException.Create(Error.Message, Error.Status);
      finally
        Error.Free;
      end;
    end);
end;

procedure TXMLRouteHandler.NFeInutilizacaoXML(Req: THorseRequest; Res: THorseResponse);
var
  LProtocolo: String;
  LResult: TXMLResponse;
begin
  LProtocolo := Req.Params['protocolo'];
  LResult := GetNest4D.Get<TXMLController>.NFeInutilizacaoXML(LProtocolo);
  LResult.When(
    procedure (XML: TFileStream)
    begin
      Res.Send(XML).ContentType(CONTENTTYPE_XML).Status(200);
    end,
    procedure (Error: ERequestException)
    begin
      try
        raise ERequestException.Create(Error.Message, Error.Status);
      finally
        Error.Free;
      end;
    end);
end;

procedure TXMLRouteHandler.NFeTransmitidaXML(Req: THorseRequest; Res: THorseResponse);
var
  LChave: String;
  LResult: TXMLResponse;
begin
  LChave := Req.Params.Items['chave'];
  LResult := GetNest4D.Get<TXMLController>.NFeXML(LChave);
  LResult.When(
    procedure (XML: TFileStream)
    begin
      Res.Send(XML).ContentType(CONTENTTYPE_XML).Status(200);
    end,
    procedure (Error: ERequestException)
    begin
      try
        raise ERequestException.Create(Error.Message, Error.Status);
      finally
        Error.Free;
      end;
    end);
end;

end.

