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

unit Nidus.Parse.Json.Pipe;

interface

uses
  Rtti,
  SysUtils,
  Generics.Collections,
  Nidus.Validation.Parse.Json,
  Nidus.Transform.Pipe,
  Nidus.Transform.Interfaces;


type
  TParseJsonPipe = class(TTransformPipe)
  private
    FJsonMap: TJsonMapped;
  public
    constructor Create;
    destructor Destroy; override;
    function Transform(const Value: TValue;
      const Metadata: ITransformArguments): TResultTransform; override;
  end;

implementation

{ TParseJsonPipe }

constructor TParseJsonPipe.Create;
begin
  FJsonMap := TJsonMapped.Create([doOwnsValues]);
end;

destructor TParseJsonPipe.Destroy;
begin
  FJsonMap.Free;
  inherited;
end;

function TParseJsonPipe.Transform(const Value: TValue;
  const Metadata: ITransformArguments): TResultTransform;
var
  LKey: String;
begin
  try
    TJsonMap.Map(Value.AsString, Metadata.ObjectType,
      procedure (const AClassType: TClass; const AFieldName: String; const AValue: TValue)
      begin
        LKey := AClassType.ClassName + '->' + AFieldName;
        if not FJsonMap.ContainsKey(LKey) then
          FJsonMap.Add(LKey, TList<TValue>.Create);
        FJsonMap[LKey].Add(AValue);
      end);
    Result.Success(FJsonMap);
  except
    on E: Exception do
      Result.Failure(E.Message);
  end;
end;

end.





