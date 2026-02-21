{
             Nest4D - Development Framework for Delphi


                   Copyright (c) 2023, Isaque Pinheiro
                          All rights reserved.

                    GNU Lesser General Public License
                      Vers?o 3, 29 de junho de 2007

       Copyright (C) 2007 Free Software Foundation, Inc. <http://fsf.org/>
       A todos ? permitido copiar e distribuir c?pias deste documento de
       licen?a, mas mud?-lo n?o ? permitido.

       Esta vers?o da GNU Lesser General Public License incorpora
       os termos e condi??es da vers?o 3 da GNU General Public License
       Licen?a, complementado pelas permiss?es adicionais listadas no
       arquivo LICENSE na pasta principal.
}

{
  @abstract(Nest4D Framework for Delphi)
  @created(01 Mai 2023)
  @author(Isaque Pinheiro <isaquesp@gmail.com>)
  @homepage(https://www.isaquepinheiro.com.br)
  @documentation(https://Nest4D-en.docs-br.com)
}

unit Nest4D.Horse;

interface

uses
  System.TypInfo,
  System.SysUtils,
  System.StrUtils,
  System.Classes,
  System.NetEncoding,
  Evolution4D.ResultPair,
  Horse,
  Nest4D,
  Nest4D.Module,
  Nest4D.Exception,
  Nest4D.Request,
  Nest4D.Route.Parse,
  Nest4D.Validation.Interfaces;

function _ResolverRouteRequest(const Req: THorseRequest): IRouteRequest;
function Nest4D_Horse(const AppModule: TModule): THorseCallback; overload;
function Nest4D_Horse(const ACharset: String): THorseCallback; overload;
procedure Middleware(Req: THorseRequest; Res: THorseResponse; Next: TNextProc);

implementation

uses
  Web.HTTPApp;

const
  C_AUTHORIZATION = 'Authorization';

function Nest4D_Horse(const AppModule: TModule): THorseCallback;
begin
  GetNest4D.Start(AppModule);
  Result := Nest4D_Horse('UTF-8');
end;

function Nest4D_Horse(const ACharset: String): THorseCallback;
begin
  Result := Middleware;
end;

procedure Middleware(Req: THorseRequest; Res: THorseResponse; Next: TProc);
const
  C_CONTENT_TYPE = 'application/json; charset=UTF-8';
var
  LResult: TReturnPair;
  LRequest: IRouteRequest;
begin
  // Treatment to ignore Swagger documentation routes in this middleware.
  if (Pos(LowerCase('swagger'), LowerCase(Req.RawWebRequest.PathInfo)) > 0) or
     (Pos(LowerCase('favicon.ico'), LowerCase(Req.RawWebRequest.PathInfo)) > 0) then
    Exit;

  // Route initialization and bindings.
  if (Req.MethodType in [mtGet, mtPost, mtPut, mtPatch, mtDelete]) then
  begin
    LRequest := _ResolverRouteRequest(Req);
    LResult := GetNest4D.LoadRouteModule(Req.RawWebRequest.PathInfo, LRequest);
    LResult.When(
      procedure (Route: TRouteAbstract)
      begin
        // If the route is found, it has come this far,
        // but we don't need to do anything with it, the module handles everything.
      end,
      procedure (Error: Exception)
      begin
        if Error is ENest4DException then
        begin
          Res.Send(Error.Message).Status(ENest4DException(Error)
                                 .Status).ContentType(C_CONTENT_TYPE);
          Error.Free;
          raise EHorseCallbackInterrupted.Create;
        end
        else
        begin
          Res.Send(Error.Message).Status(500).ContentType(C_CONTENT_TYPE);
          Error.Free;
          raise EHorseCallbackInterrupted.Create;
        end;
      end);
  end;
  try
    try
      Next;
    except
      on E: EHorseCallbackInterrupted do
        raise;
      on E: EHorseException do
        Res.Send(Format('{ ' + sLineBreak +
                        '   "statusCode": %s,' + sLineBreak +
                        '   "message": "%s"' + sLineBreak +
                        '}', [IntToStr(E.Code), E.Message]))
           .Status(E.Status)
           .ContentType(C_CONTENT_TYPE);
      on E: Exception do
        Res.Send(Format('{ ' + sLineBreak +
                        '   "statusCode": "%s", ' + sLineBreak +
                        '   "scope": "%s", ' + sLineBreak +
                        '   "message": "%s"' + sLineBreak +
                        '}', ['400', E.UnitScope, E.Message]))
           .Status(THTTPStatus.BadRequest)
           .ContentType(C_CONTENT_TYPE);
    end;
  finally
    GetNest4D.DisposeRouteModule(Req.RawWebRequest.PathInfo);
  end;
end;

function _ResolverRouteRequest(const Req: THorseRequest): IRouteRequest;
begin
  try
    Result := TRouteRequest.Create(Req.Headers.Content,
                                   Req.Params.Content,
                                   Req.Query.Content,
                                   Req.Body,
                                   Req.RawWebRequest.Host,
                                   Req.RawWebRequest.ContentType,
                                   Req.RawWebRequest.Method,
                                   Req.RawWebRequest.PathInfo,
                                   Req.RawWebRequest.ServerPort,
                                   Req.RawWebRequest.Authorization);
  except
    Exit;
  end;
end;

end.







