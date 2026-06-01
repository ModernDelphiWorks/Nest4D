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

unit Nidus.Parse.UUID.Pipe;

interface

uses
  Rtti,
  SysUtils,
  StrUtils,
  Generics.Collections,
  Generics.Defaults,
  Nidus.Transform.Pipe,
  Nidus.Transform.Interfaces;


type
  TParseUUIDPipe = class(TTransformPipe)
  public
    function Transform(const Value: TValue;
      const Metadata: ITransformArguments): TResultTransform; override;
  end;

implementation

uses
  MOdernSyntax.RegExpression;

function TParseUUIDPipe.Transform(const Value: TValue;
  const Metadata: ITransformArguments): TResultTransform;
var
  LMessage: String;
begin
  if TModernRegEx.IsMatchUUID(Value.ToString)  then
    Result.Success(Value)
  else
  begin
    LMessage := ifThen(Metadata.Message = '',
                       Format('[%s] %s-> [%s] Validation failed (uuid String is expected)', [Metadata.TagName,
                                                                                             Self.ClassName,
                                                                                             Metadata.FieldName]), Metadata.Message);
    Result.Failure(LMessage);
  end;
end;

end.





