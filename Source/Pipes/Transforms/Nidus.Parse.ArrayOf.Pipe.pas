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

unit Nidus.Parse.ArrayOf.Pipe;

interface

uses
  Rtti,
  Types,
  SysUtils,
  StrUtils,
  Classes,
  Generics.Collections,
  Nidus.Transform.Pipe,
  Nidus.Transform.Interfaces;


type
  TParseArrayPipe = class(TTransformPipe)
  public
    function Transform(const Value: TValue;
      const Metadata: ITransformArguments): TResultTransform; override;
  end;

implementation

function TParseArrayPipe.Transform(const Value: TValue;
  const Metadata: ITransformArguments): TResultTransform;
var
  LValue: TStringDynArray;
  LMessage: String;
begin
  LValue := SplitString(Value.AsString, Metadata.Values[0].AsString);
  if Length(LValue) > 1 then
    Result.Success(TValue.From<TStringDynArray>(LValue))
  else
  begin
    LMessage := ifThen(Metadata.Message = '',
                       Format('[%s] %s-> [%s] Validation failed (array String is expected)', [Metadata.TagName,
                                                                                              Self.ClassName,
                                                                                              Metadata.FieldName]), Metadata.Message);
    Result.Failure(LMessage);
  end;
end;

end.





