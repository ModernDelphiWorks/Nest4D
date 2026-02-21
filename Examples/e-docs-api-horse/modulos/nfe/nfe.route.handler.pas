unit nfe.route.handler;

interface

uses
  SysUtils,
  Generics.Collections,
  core.include,
  core.exception,
  core.constantes,
  nfe.controller,
  nfebr.model,
  nfebr.lib.include,
  nest4d,
  Nest4D.route.handler.horse,
  decorator.include,
  Nest4D.Validation.include,
  Nest4D.parse.jsonbr.pipe,
  app.route,
  person;

type
  TNFeRouteHandler = class(TRouteHandlerHorse)
  protected
    procedure RegisterRoutes; override;
  public
    constructor Create; override;
    procedure NFeTransmitir(Req: THorseRequest; Res: THorseResponse);
    procedure NFeTransmitirLote(Req: THorseRequest; Res: THorseResponse);
    procedure NFeConsultar(Req: THorseRequest; Res: THorseResponse);
    procedure NFeCancelar(Req: THorseRequest; Res: THorseResponse);
    procedure NFeInutilizar(Req: THorseRequest; Res: THorseResponse);
    procedure NFeCorrigir(Req: THorseRequest; Res: THorseResponse);

    [Body(TPerson, TParseJsonPipe)]
//    [Param('id', TIsNotEmpty), Param('id', TParseIntegerPipe)]
//    [Query('name', TIsString), Query('name', TIsNotEmpty)]
    procedure NFeServicoStatus(Req: THorseRequest; Res: THorseResponse);
  end;

implementation

{ TNFeRouteHandler }

procedure TNFeRouteHandler.RegisterRoutes;
begin
  RouteGet(Rota.NFeTransmitir, NFeTransmitir)
    .RouteGet(Rota.NFeTransmitirLote, NFeTransmitirLote)
    .RouteGet(Rota.NFeConsultar, NFeConsultar)
    .RouteGet(Rota.NFeCancelar, NFeCancelar)
    .RouteGet(Rota.NFeInutilizar, NFeInutilizar)
    .RouteGet(Rota.NFeCorrigir, NFeCorrigir)
    .RoutePost(Rota.NFeServicoStatus, NFeServicoStatus);
end;

procedure TNFeRouteHandler.NFeTransmitir(Req: THorseRequest; Res: THorseResponse);
var
  LResult: TNFeResponse;
begin
  LResult := GetNest4D.Get<TNFeController>.NFeTransmitir(Req.Body);
  LResult.When(
    procedure (Json: String)
    begin
      Res.Send(Json).ContentType(CONTENTTYPE_JSON).Status(200);
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

constructor TNFeRouteHandler.Create;
begin
  inherited;

end;

procedure TNFeRouteHandler.NFeCancelar(Req: THorseRequest; Res: THorseResponse);
var
  LResult: TNFeResponse;
begin
  LResult := GetNest4D.Get<TNFeController>.NFeCancelar(Req.Body);
  LResult.When(
    procedure (Msg: String)
    begin
      Res.Send(Msg).ContentType(CONTENTTYPE_JSON).Status(200);
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

procedure TNFeRouteHandler.NFeCorrigir(Req: THorseRequest; Res: THorseResponse);
var
  LResult: TNFeResponse;
begin
  LResult := GetNest4D.Get<TNFeController>.NFeCorrigir(Req.Body);
  LResult.When(
    procedure (Msg: String)
    begin
      Res.Send(Msg).ContentType(CONTENTTYPE_JSON).Status(200);
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

procedure TNFeRouteHandler.NFeConsultar(Req: THorseRequest; Res: THorseResponse);
var
  LResult: TNFeResponse;
begin
  LResult := GetNest4D.Get<TNFeController>.NFeConsultar(Req.Body);
  LResult.When(
    procedure (Msg: String)
    begin
      Res.Send(Msg).ContentType(CONTENTTYPE_JSON).Status(200);
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

procedure TNFeRouteHandler.NFeInutilizar(Req: THorseRequest; Res: THorseResponse);
var
  LResult: TNFeResponse;
begin
  LResult := GetNest4D.Get<TNFeController>.NFeInutilizar(Req.Body);
  LResult.When(
    procedure (Msg: String)
    begin
      Res.Send(Msg).ContentType(CONTENTTYPE_JSON).Status(200);
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

procedure TNFeRouteHandler.NFeTransmitirLote(Req: THorseRequest; Res: THorseResponse);
var
  LResult: TNFeResponse;
begin
  LResult := GetNest4D.Get<TNFeController>.NFeTransmitirLote(Req.Body);
  LResult.When(
    procedure (Msg: String)
    begin
      Res.Send(Msg).ContentType(CONTENTTYPE_JSON).Status(200);
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

procedure TNFeRouteHandler.NFeServicoStatus(Req: THorseRequest;
  Res: THorseResponse);
var
  LResult: TNFeResponse;
begin
  LResult := GetNest4D.Get<TNFeController>.NFeServicoStatus;
  LResult.When(
    procedure (Msg: String)
    begin
      Res.Send(Msg).ContentType(CONTENTTYPE_JSON).Status(200);
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

initialization
  GetNest4D.RegisterRouteHandler(TNFeRouteHandler);

end.
