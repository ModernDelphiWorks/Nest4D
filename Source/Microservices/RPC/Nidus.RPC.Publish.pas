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

unit Nidus.RPC.Publish;

interface

uses
  Generics.Collections,
  Nidus.RPC.Resource;

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






