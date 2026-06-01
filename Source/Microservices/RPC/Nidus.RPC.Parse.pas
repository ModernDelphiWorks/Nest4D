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

unit Nidus.RPC.Parse;

interface

uses
  Rtti,
  JSON,
  SysUtils,
  Generics.Collections;

type
  TRPCParse = class
  public
    procedure RPCParseRequest(const ARequest: String;
      var ARPCID: String; var ARPCName: String; var ARPCParams: TArray<TValue>);
  end;

implementation

uses
  Nidus.RPC.Exception;

procedure TRPCParse.RPCParseRequest(const ARequest: String;
  var ARPCID: String; var ARPCName: String; var ARPCParams: TArray<TValue>);
var
  LJSONValue: TJSONValue;
  LJSONObject: TJSONObject;
  LJSONParam: TJSONValue;
  LParams: TJSONArray;
  LParamValue: TValue;
  LFor: integer;
begin
  SetLength(ARPCParams, 0);
  LJSONValue := TJSONObject.ParseJSONValue(ARequest);
  try
    if not Assigned(LJSONValue) then
      raise ERPCJSONException.Create('JSON-RPC request is not valid. Missing "params" field.')
    else if not (LJSONValue is TJSONObject) then
      raise ERPCJSONException.Create('JSON-RPC request is not valid. "params" field is not an object.');

    LJSONObject := TJSONObject(LJSONValue);
    try
      ARPCID := LJSONObject.GetValue('id').Value;
      ARPCName := LJSONObject.GetValue('method').Value;
      LParams := TJSONArray(LJSONObject.GetValue('params'));
      if (not Assigned(LParams)) and (not (LParams is TJSONArray)) then
        raise ERPCJSONParamsException.Create('JSON-RPC request is not valid. "params" field is not an array.');

      SetLength(ARPCParams, LParams.Count);
      for LFor := 0 to LParams.Count - 1 do
      begin
        LJSONParam := LParams.Items[LFor];
        if LJSONParam.TryGetValue<TValue>(LParamValue) then
          ARPCParams[LFor] := LParamValue
        else
          ARPCParams[LFor] := TValue.Empty;
      end;
    finally
      LJSONObject.Free;
    end;
  finally
    LJSONValue.Free;
  end;
end;

end.





