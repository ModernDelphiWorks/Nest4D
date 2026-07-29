{
  ------------------------------------------------------------------------------
  Nidus
  Modular and scalable application framework for Delphi, inspired by the architectural patterns of NestJS.

  SPDX-License-Identifier: MIT
  Copyright (c) 2025-2026 Isaque Pinheiro

  Licensed under the MIT License.
  See the LICENSE file in the project root for full license information.
  ------------------------------------------------------------------------------
}

unit Nidus.Driver.Horse;

interface

uses
  TypInfo,
  SysUtils,
  StrUtils,
  Classes,
  NetEncoding,
  Web.HTTPApp,
  ModernSyntax.ResultPair,
  Horse,
  Horse.Commons,  // must stay after Web.HTTPApp: Horse 4.x declares its own TMethodType
  Nidus,
  Nidus.Module,
  Nidus.Exception,
  Nidus.Request,
  Nidus.Route.Parse,
  Nidus.Validation.Interfaces;

function _ResolverRouteRequest(const Req: THorseRequest): IRouteRequest;
function Nidus_Horse(const AAppModule: TModule): THorseCallback; overload;
function Nidus_Horse(const ACharset: String): THorseCallback; overload;
procedure Middleware(Req: THorseRequest; Res: THorseResponse; Next: TNextProc);

implementation

const
  C_AUTHORIZATION = 'Authorization';

function Nidus_Horse(const AAppModule: TModule): THorseCallback;
begin
  GetNidus.Start(AAppModule);
  Result := Nidus_Horse('UTF-8');
end;

function Nidus_Horse(const ACharset: String): THorseCallback;
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
    LResult := GetNidus.LoadRouteModule(Req.RawWebRequest.PathInfo, LRequest);
    LResult.When(
      procedure (Route: TRouteAbstract)
      begin
        // If the route is found, it has come this far,
        // but we don't need to do anything with it, the module handles everything.
      end,
      procedure (Error: Exception)
      begin
        if Error is ENidusException then
        begin
          Res.Send(Error.Message).Status(ENidusException(Error)
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
    GetNidus.DisposeRouteModule(Req.RawWebRequest.PathInfo);
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




