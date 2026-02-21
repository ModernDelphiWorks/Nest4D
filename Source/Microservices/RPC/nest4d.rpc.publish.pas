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

unit Nest4D.Rpc.Publish;

interface

uses
  System.Generics.Collections,
  Nest4D.Rpc.Resource;

type
  TRPCPublish = class
  private
    FRegisteredRPCs: TDictionary<String, TRPCResourceClass>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure PublishRPC(const ARPCName: String; const ARPCClass: TRPCResourceClass);
    procedure UnPublishRPC(const ARPCName: String);
    function RPCs: TDictionary<String, TRPCResourceClass>;
  end;

implementation

{ TRegisterRPC }

constructor TRPCPublish.Create;
begin
  FRegisteredRPCs := TDictionary<String, TRPCResourceClass>.Create;
end;

destructor TRPCPublish.Destroy;
begin
  FRegisteredRPCs.Free;
  inherited;
end;

procedure TRPCPublish.PublishRPC(const ARPCName: String;
  const ARPCClass: TRPCResourceClass);
begin
  FRegisteredRPCs.AddOrSetValue(ARPCName, ARPCClass);
end;

function TRPCPublish.RPCs: TDictionary<String, TRPCResourceClass>;
begin
  Result := FRegisteredRPCs;
end;

procedure TRPCPublish.UnPublishRPC(const ARPCName: String);
begin
  FRegisteredRPCs.Remove(ARPCName);
end;

end.







