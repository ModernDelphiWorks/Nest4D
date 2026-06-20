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

unit Nidus.Parse.Jsonbr.Pipe;

// Optional transform pipe — requires the third-party JsonBr library (jsonbr / jsonbr.builders).
// Define NIDUS_JSONBR (and have JsonBr installed) to compile it in.
{$IFDEF NIDUS_JSONBR}

interface

uses
  Rtti,
  SysUtils,
  Generics.Collections,
  Nidus.Transform.Pipe,
  Nidus.Transform.Interfaces;


type
  TParseJsonBrPipe = class(TTransformPipe)
  public
    function Transform(const Value: TValue;
      const Metadata: ITransformArguments): TResultTransform; override;
  end;

implementation

uses
  jsonbr,
  jsonbr.builders;

{ TParseJsonBrPipe }

function TParseJsonBrPipe.Transform(const Value: TValue;
  const Metadata: ITransformArguments): TResultTransform;
var
  LObject: TObject;
  LObjects: TObjectList<TObject>;
  LIsArray: Boolean;
begin
  LIsArray := Value.AsString[1] = '[';
  try
    if LIsArray then
    begin
      LObjects := TObjectList<TObject>.Create;
      LObjects := TJsonBr.JsonToObjectList(Value.AsString, Metadata.ObjectType);
      Result.Success(LObjects);
    end
    else
    begin
      LObject := Metadata.ObjectType.Create;
      TJsonBr.JsonToObject(Value.AsString, LObject);
      Result.Success(LObject);
    end;
  except
    on E: Exception do
      Result.Failure(E.Message);
  end;
end;

{$ELSE}

interface

implementation

{$ENDIF}

end.





