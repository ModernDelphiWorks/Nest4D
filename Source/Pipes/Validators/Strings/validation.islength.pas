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

unit validation.islength;

interface

uses
  Rtti,
  SysUtils,
  StrUtils,
  nest4d.validator.constraint,
  nest4d.validation.interfaces;

type
  TIsLength = class(TValidatorConstraint)
  public
    function Validate(const Value: TValue;
      const Args: IValidationArguments): TResultValidation; override;
  end;

implementation

{ TIsMax }

function TIsLength.Validate(const Value: TValue;
  const Args: IValidationArguments): TResultValidation;
var
  LMessage: String;
  LLength: integer;
begin
  Result.Success(False);
  if Value.Kind in [tkString, tkLString, tkWString, tkUString] then
  begin
    LLength := Length(Value.AsString);
    // Args.Values[0]=Min; Args.Values[1]=Max
    if (Args.Values[0].Kind in [tkInt64, tkInteger, tkFloat]) and
       (Args.Values[1].Kind in [tkInt64, tkInteger, tkFloat]) then
    begin
      if (LLength >= Args.Values[0].AsExtended) and
         (LLength <= Args.Values[1].AsExtended) then
        Result.Success(True);
    end;
  end;
  if not Result.ValueSuccess then
  begin
    LMessage := IfThen(Args.Message = '',
                       Format('[%s] %s->%s [%s] must be longer than or equal to %s and shorter than or equal to %s characters',
                       [Args.TagName,
                        Args.TypeName,
                        Args.Values[Length(Args.Values) -1].ToString,
                        Args.FieldName,
                        Args.Values[0].ToString,
                        Args.Values[1].ToString]), Args.Message);
    Result.Failure(LMessage);
  end;
end;

end.





