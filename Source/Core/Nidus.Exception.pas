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

unit Nidus.Exception;

interface

uses
  Windows,
  Classes,
  StrUtils,
  SysUtils;

type
  ENidusException = class;
  EBadRequestException = class;
  EUnauthorizedException = class;
  ERouteNotFoundException = class;
  EBindException = class;
  EBindNotFoundException = class;
  EModuleStartedException = class;
  EModuleStartedInitException = class;

  ENidusException = class abstract (Exception)
  public
    Status: integer;
    constructor Create(const Msg: String); overload; virtual;
    constructor CreateFmt(const Msg: String; const Args: array of const); virtual;
  end;

  EBadRequestException = class(ENidusException)
  public
    const cMSG_DEFAULT = 'Internal server error';
    const cMSG_DEFAULT_ARGS = 'Internal server error (%s)';
    constructor Create(const Msg: String); overload; override;
    constructor CreateFmt(const Msg: String; const Args: array of const); override;
  end;

  ERouteNotFoundException = class(ENidusException)
  public
    const cMSG_DEFAULT = 'Nidus route not found';
    const cMSG_DEFAULT_ARGS = 'Nidus route (%s) not found';
    constructor Create(const Msg: String); overload; override;
    constructor CreateFmt(const Msg: String; const Args: array of const); override;
  end;

  EBindException = class(ENidusException)
  public
    const cMSG_DEFAULT = 'Class error occurred';
    const cMSG_DEFAULT_ARGS = 'Class [%s] error occurred';
    constructor Create(const Msg: String); overload; override;
    constructor CreateFmt(const Msg: String; const Args: array of const); override;
  end;

  EUnauthorizedException = class(ENidusException)
  public
    const cMSG_DEFAULT = 'Access to route unauthorized';
    const cMSG_DEFAULT_ARGS = 'Access to route (%s) unauthorized';
    constructor Create(const Msg: String); overload; override;
    constructor CreateFmt(const Msg: String; const Args: array of const); override;
  end;

  EBindNotFoundException = class(ENidusException)
  public
    const cMSG_DEFAULT = 'Class not found';
    const cMSG_DEFAULT_ARGS = 'Class [%s] not found';
    constructor Create(const Msg: String); overload; override;
    constructor CreateFmt(const Msg: String; const Args: array of const); override;
  end;

  EModuleStartedException = class(ENidusException)
  public
    const cMSG_DEFAULT = 'Module is already started';
    const cMSG_DEFAULT_ARGS = 'Module [%s] is already started';
    constructor Create(const Msg: String); overload; override;
    constructor CreateFmt(const Msg: String; const Args: array of const); override;
  end;

  EModuleStartedInitException = class(ENidusException)
  public
    const cMSG_DEFAULT = 'Execute "Nest.Init(TAppModule.Create)" this is just an example';
    const cMSG_DEFAULT_ARGS = 'Execute "Nest.Init(%s)" this is just an example';
    constructor Create(const Msg: String); overload; override;
    constructor CreateFmt(const Msg: String; const Args: array of const); override;
  end;

  ERPCProviderNotSetException = class(Exception)
  public
    constructor Create;
  end;

  EAppInjector = class(Exception)
  public
    constructor Create;
  end;

{$IFDEF DEBUG}
procedure DebugPrint(const AMessage: String);
{$ENDIF}

function FormatListenerMessage(const AMessage: String): String;

implementation

{$IFDEF DEBUG}
procedure DebugPrint(const AMessage: String);
begin
  TThread.Queue(nil,
          procedure
          begin
            OutputDebugString(PWideChar('[Nidus] - ' + FormatDateTime('mm/dd/yyyy, hh:mm:ss AM/PM', Now) + ' LOG ' + AMessage));
          end);
end;
{$ENDIF}

function FormatListenerMessage(const AMessage: String): String;
begin
  Result := '[Nidus] - ' + FormatDateTime('mm/dd/yyyy, hh:mm:ss AM/PM', Now) + ' LOG ' + AMessage;
end;

{ ERouteNotFound }

constructor ERouteNotFoundException.Create(const Msg: String);
var
  LMsg: String;
begin
  LMsg := Format('{"statusCode": "404", "message": %s, "error": "Not Found"}', [cMSG_DEFAULT]);
  inherited Create(ifThen(Msg = '', LMsg, Msg));
  Status := 404;
end;

constructor ERouteNotFoundException.CreateFmt(const Msg: String;
  const Args: array of const);
var
  LMsg: String;
begin
  LMsg := ifThen(Msg = '', cMSG_DEFAULT_ARGS, Msg);
  Create(Format(LMsg, Args));
end;

{ EBindError }

constructor EBindException.Create(const Msg: String);
var
  LMsg: String;
begin
  LMsg := Format('{"statusCode": "500", "message": %s, "error": "Internal Server Error"}', [cMSG_DEFAULT]);
  inherited Create(ifThen(Msg = '', LMsg, Msg));
  Status := 500;
end;

constructor EBindException.CreateFmt(const Msg: String; const Args: array of const);
var
  LMsg: String;
begin
  LMsg := ifThen(Msg = '', cMSG_DEFAULT_ARGS, Msg);
  Create(Format(LMsg, Args));
end;

{ ERouteGuardianAuthorized }

constructor EUnauthorizedException.Create(const Msg: String);
var
  LMsg: String;
begin
  LMsg := Format('{"statusCode": "401", "message": %s, "error": "Unauthorized"}', [cMSG_DEFAULT]);
  inherited Create(ifThen(Msg = '', LMsg, Msg));
  Status := 401;
end;

constructor EUnauthorizedException.CreateFmt(const Msg: String;
  const Args: array of const);
var
  LMsg: String;
begin
  LMsg := ifThen(Msg = '', cMSG_DEFAULT_ARGS, Msg);
  Create(Format(LMsg, Args));
end;

{ EBindNotFound }

constructor EBindNotFoundException.Create(const Msg: String);
var
  LMsg: String;
begin
  LMsg := Format('{"statusCode": "404", "message": %s, "error": "Not Found"}', [cMSG_DEFAULT]);
  inherited Create(ifThen(Msg = '', LMsg, Msg));
  Status := 404;
end;

constructor EBindNotFoundException.CreateFmt(const Msg: String;
  const Args: array of const);
var
  LMsg: String;
begin
  LMsg := ifThen(Msg = '', cMSG_DEFAULT_ARGS, Msg);
  Create(Format(LMsg, Args));
end;

{ EModuleStartedException }

constructor EModuleStartedException.Create(const Msg: String);
var
  LMsg: String;
begin
  LMsg := Format('{"statusCode": "500", "message": %s, "error": "Internal Server Error"}', [cMSG_DEFAULT]);
  inherited Create(ifThen(Msg = '', LMsg, Msg));
  Status := 500;
end;

constructor EModuleStartedException.CreateFmt(const Msg: String;
  const Args: array of const);
var
  LMsg: String;
begin
  LMsg := ifThen(Msg = '', cMSG_DEFAULT_ARGS, Msg);
  Create(Format(LMsg, Args));
end;

{ EModularError }

constructor ENidusException.Create(const Msg: String);
begin
  inherited Create(Msg);
  Status := 0;
end;

constructor ENidusException.CreateFmt(const Msg: String;
  const Args: array of const);
begin
  inherited CreateFmt(Msg, Args);
  Status := 0;
end;

{ EModuleStartedInit }

constructor EModuleStartedInitException.Create(const Msg: String);
var
  LMsg: String;
begin
  LMsg := Format('{"statusCode": "500", "message": %s, "error": "Internal Server Error"}', [cMSG_DEFAULT]);
  inherited Create(ifThen(Msg = '', LMsg, Msg));
  Status := 500;
end;

constructor EModuleStartedInitException.CreateFmt(const Msg: String;
  const Args: array of const);
var
  LMsg: String;
begin
  LMsg := ifThen(Msg = '', cMSG_DEFAULT_ARGS, Msg);
  Create(Format(LMsg, Args));
end;

{ EAppInjector }

constructor EAppInjector.Create;
begin
  inherited Create('The AppInjector pointer is not assigned.')
end;

{ EBadRequestException }

constructor EBadRequestException.Create(const Msg: String);
var
  LMsg: String;
begin
  LMsg := Format('{"statusCode": "400", "message": %s, "error": "Bad Request"}', [cMSG_DEFAULT]);
  inherited Create(ifThen(Msg = '', LMsg, Msg));
  Status := 400;
end;

constructor EBadRequestException.CreateFmt(const Msg: String;
  const Args: array of const);
var
  LMsg: String;
begin
  LMsg := ifThen(Msg = '', cMSG_DEFAULT_ARGS, Msg);
  Create(Format(LMsg, Args));
end;

{ ERPCProviderNotSetException }

constructor ERPCProviderNotSetException.Create;
begin
  inherited Create('Provider not set. Call the method SetProvider before using UseRPC()');
end;

end.






