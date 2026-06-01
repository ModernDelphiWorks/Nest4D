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

unit Nidus.Parse.Float.Pipe;

interface

uses
  Rtti,
  SysUtils,
  StrUtils,
  Generics.Collections,
  Nidus.Transform.Pipe,
  Nidus.Transform.Interfaces;


type
  TParseFloatPipe = class(TTransformPipe)
  public
    function Transform(const Value: TValue;
      const Metadata: ITransformArguments): TResultTransform; override;
  end;

implementation

function TParseFloatPipe.Transform(const Value: TValue;
  const Metadata: ITransformArguments): TResultTransform;
var
  LValue: double;
  LMessage: String;
begin
  if TryStrToFloat(Value.ToString, LValue) then
    Result.Success(LValue)
  else
  begin
    LMessage := ifThen(Metadata.Message = '',
                       Format('[%s] %s-> [%s] Validation failed (float String is expected)', [Metadata.TagName,
                                                                                              Self.ClassName,
                                                                                              Metadata.FieldName]), Metadata.Message);
    Result.Failure(LMessage);
  end;
end;

end.





