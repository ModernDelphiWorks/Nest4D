unit pdf.route.handler;

interface

uses
  Classes,
  core.include,
  core.exception,
  core.types,
  core.constantes,
  nest4d,
  Nest4D.route.handler.horse;

type
  TPDFRouteHandler = class(TRouteHandlerHorse)
  protected
    procedure RegisterRoutes; override;
  public
    constructor Create; override;
    procedure NFeTransmitidaPDF(Req: THorseRequest; Res: THorseResponse);
    procedure NFeCancelamentoPDF(Req: THorseRequest; Res: THorseResponse);
    procedure NFeInutilizacaoPDF(Req: THorseRequest; Res: THorseResponse);
    procedure NFeCartaCorrecaoPDF(Req: THorseRequest; Res: THorseResponse);
  end;

implementation

uses
  app.route,
  app.module,
  Nest4D.horse,
  pdf.controller;

{ TPDFRouteHandler }

procedure TPDFRouteHandler.RegisterRoutes;
begin
  inherited;
  RouteGet(Rota.NFeTransmitidaPDF, NFetransmitidaPDF)
    .RouteGet(Rota.NFeCancelamentoPDF, NFeCancelamentoPDF)
    .RouteGet(Rota.NFeInutilizacaoPDF, NFeInutilizacaoPDF)
    .RouteGet(Rota.NFeCartaCorrecaoPDF, NFeCartaCorrecaoPDF);
end;

constructor TPDFRouteHandler.Create;
begin
  inherited;

end;

procedure TPDFRouteHandler.NFeCancelamentoPDF(Req: THorseRequest; Res: THorseResponse);
var
  LChave: String;
  LResult: TPDFResponse;
begin
  LChave := Req.Params['chave'];
  LResult := GetNest4D.Get<TPDFController>.NFeCancelamentoPDF(LChave);
  LResult.When(
    procedure (PDF: TFileStream)
    begin
      Res.Send(PDF).ContentType(CONTENTTYPE_PDF).Status(200);
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

procedure TPDFRouteHandler.NFeCartaCorrecaoPDF(Req: THorseRequest; Res: THorseResponse);
var
  LChave: String;
  LResult: TPDFResponse;
begin
  LChave := Req.Params['chave'];
  LResult := GetNest4D.Get<TPDFController>.NFeCartaCorrecaoPDF(LChave);
  LResult.When(
    procedure (PDF: TFileStream)
    begin
      Res.Send(PDF).ContentType(CONTENTTYPE_PDF).Status(200);
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

procedure TPDFRouteHandler.NFeInutilizacaoPDF(Req: THorseRequest; Res: THorseResponse);
var
  LProtocolo: String;
  LResult: TPDFResponse;
begin
  LProtocolo := Req.Params['protocolo'];
  LResult := GetNest4D.Get<TPDFController>.NFeInutilizacaoPDF(LProtocolo);
  LResult.When(
    procedure (PDF: TFileStream)
    begin
      Res.Send(PDF).ContentType(CONTENTTYPE_PDF).Status(200);
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

procedure TPDFRouteHandler.NFeTransmitidaPDF(Req: THorseRequest; Res: THorseResponse);
var
  LChave: String;
  LResult: TPDFResponse;
begin
  LChave := Req.Params['chave'];
  LResult := GetNest4D.Get<TPDFController>.NFePDF(LChave);
  LResult.When(
    procedure (PDF: TFileStream)
    begin
      Res.Send(PDF).ContentType(CONTENTTYPE_PDF).Status(200);
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
