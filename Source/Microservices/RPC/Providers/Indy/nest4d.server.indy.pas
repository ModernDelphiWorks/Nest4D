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

unit Nest4D.Server.Indy;

interface

uses
  System.SysUtils,
  System.Classes,
  IdContext,
  IdTCPConnection,
  IdCustomTCPServer,
  Nest4D.Rpc.Server;

type
  TIdCustomTCPServerHacker = class(IdCustomTCPServer.TIdCustomTCPServer);

  TRPCProviderServerIndy = class(TRPCProviderServer)
  private
    FTCPServer: TIdCustomTCPServer;
    procedure _OnExecute(AContext: TIdContext);
  public
    constructor Create(const AHost: String; const APort: integer = 8080); override;
    destructor Destroy; override;
    procedure Start; override;
    procedure Stop; override;
  end;

implementation

{ TTCPRPCProviderIndy }

constructor TRPCProviderServerIndy.Create(const AHost: String; const APort: integer);
begin
  inherited Create(AHost, APort);
  FTCPServer := TIdCustomTCPServer.Create(nil);
  FTCPServer.Bindings.Add.IP := FHost;
  FTCPServer.Bindings.Add.Port := FPort;
  TIdCustomTCPServerHacker(FTCPServer).FOnExecute := _OnExecute;
end;

destructor TRPCProviderServerIndy.Destroy;
begin
  FTCPServer.Free;
  inherited;
end;

procedure TRPCProviderServerIndy._OnExecute(AContext: TIdContext);
var
  LResponseData: String;
begin
  LResponseData := ExecuteRPC(AContext.Connection.IOHandler.ReadLn);
  AContext.Connection.IOHandler.WriteLn(LResponseData);
  AContext.Connection.Disconnect;
end;

procedure TRPCProviderServerIndy.Start;
begin
  FTCPServer.Active := True;
end;

procedure TRPCProviderServerIndy.Stop;
begin
  FTCPServer.Active := False;
end;

end.









