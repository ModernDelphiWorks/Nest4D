{
             Nest4D - Development Framework for Delphi

                   Copyright (c) 2023, Isaque Pinheiro
                          All rights reserved.

                    GNU Lesser General Public License
                      Vers�o 3, 29 de junho de 2007

       Copyright (C) 2007 Free Software Foundation, Inc. <http://fsf.org/>
       A todos � permitido copiar e distribuir c�pias deste documento de
       licen�a, mas mud�-lo n�o � permitido.

       Esta vers�o da GNU Lesser General Public License incorpora
       os termos e condi��es da vers�o 3 da GNU General Public License
       Licen�a, complementado pelas permiss�es adicionais listadas no
       arquivo LICENSE na pasta principal.
}

{
  @abstract(Nest4D Framework for Delphi)
  @created(01 Mai 2023)
  @author(Isaque Pinheiro <isaquesp@gmail.com>)
  @homepage(https://www.isaquepinheiro.com.br)
  @documentation(https://nest4d-en.docs-br.com)
}

unit nest4d.rpc.server;

interface

uses
  nest4d.rpc.routehandle,
  nest4d.rpc.interfaces,
  nest4d.rpc.resource;

type
  TRPCProviderServer = class(TInterfacedObject, IRPCProviderServer)
  private
    FRPCRouteHandle: IRPCRouteHandle;
  protected
    FHost: String;
    FPort: integer;
  public
    constructor Create(const AHost: String; const APort: integer = 8080); virtual;
    destructor Destroy; override;
    procedure Start; virtual;
    procedure Stop; virtual;
    procedure PublishRPC(const ARPCName: String; const ARPCClass: TRPCResourceClass);
    function ExecuteRPC(const ARequest: String): String;
  end;

implementation

{ TTCPRPCProvider }

constructor TRPCProviderServer.Create(const AHost: String; const APort: integer);
begin
  FHost := AHost;
  FPort := APort;
  FRPCRouteHandle := TRPCRouteHandle.Create;
end;

destructor TRPCProviderServer.Destroy;
begin
  inherited;
end;

function TRPCProviderServer.ExecuteRPC(const ARequest: String): String;
begin
  Result := FRPCRouteHandle.ExecuteRPC(ARequest);
end;

procedure TRPCProviderServer.PublishRPC(const ARPCName: String;
  const ARPCClass: TRPCResourceClass);
begin
  FRPCRouteHandle.PublishRPC(ARPCName, ARPCClass);
end;

procedure TRPCProviderServer.Start;
begin

end;

procedure TRPCProviderServer.Stop;
begin

end;

end.





