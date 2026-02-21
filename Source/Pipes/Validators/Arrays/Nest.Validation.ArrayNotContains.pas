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
  @documentation(https://nest4d-en.docs-br.com)
}

unit Nest.Validation.ArrayNotContains;

interface

uses
  Rtti,
  SysUtils,
  StrUtils,
  Nest.validator.constraint,
  Nest.validation.interfaces;

type
  TArrayNotContains = class(TValidatorConstraint)
  private
    function _ArrayToString(const AValues: TArray<TValue>): String;
    function _ArrayNotContainsAllElements(const ASource, AValues: TArray<TValue>): Boolean;
  public
    function Validate(const Value: TValue;
      const Args: IValidationArguments): TResultValidation; override;
  end;

implementation

{ TArrayContains }

function TArrayNotContains.Validate(const Value: TValue;
  const Args: IValidationArguments): TResultValidation;
var
  LMessage: String;
begin
  Result.Success(False);
  if Value.Kind in [tkArray, tkDynArray] then
  begin
    if _ArrayNotContainsAllElements(Value.AsType<TArray<TValue>>, Args.Values) then
      Result.Success(True);
  end;
  if not Result.ValueSuccess then
  begin
    LMessage := IfThen(Args.Message = '',
                       Format('[%s] %s->%s [%s] must contain a %s values',
                       [Args.TagName,
                        Args.TypeName,
                        Args.Values[Length(Args.Values) -1].ToString,
                        Args.FieldName,
                        _ArrayToString(Args.Values)]), Args.Message);
    Result.Failure(LMessage);
  end;
end;

function TArrayNotContains._ArrayToString(const AValues: TArray<TValue>): String;
var
  LItem: TValue;
begin
  Result := '';
  for LItem in AValues do
    Result := Result + LItem.ToString + ', ';
end;

function TArrayNotContains._ArrayNotContainsAllElements(const ASource, AValues: TArray<TValue>): Boolean;
var
  LFor, LFind: integer;
  LFound: Boolean;
begin
  Result := True;
  for LFor := Low(AValues) to High(AValues) do
  begin
    LFound := False;
    for LFind := Low(ASource) to High(ASource) do
    begin
      if AValues[LFor].ToString <> ASource[LFind].ToString then
      begin
        LFound := True;
        break;
      end;
    end;
    if not LFound then
    begin
      Result := False;
      exit;
    end;
  end;
end;

end.





