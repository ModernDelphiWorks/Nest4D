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
  @abstract(Nest4D Framework)
  @created(01 Mai 2023)
  @author(Isaque Pinheiro <isaquesp@gmail.com>)
  @author(Site : https://www.isaquepinheiro.com.br)
}

unit nest4d.route.service;

interface

uses
  Rtti,
  Classes,
  SysUtils,
  nest4d.route.provider,
  nest4d.route.param,
  nest4d.route.abstract,
  nest4d.exception,
  System.Evolution.ResultPair;

type
  TRouteService = class
  private
    FProvider: TRouteProvider;
  public
    destructor Destroy; override;
    procedure IncludeProvider(const AProvider: TRouteProvider);
    function GetRoute(const AArgs: TRouteParam): TResultPair<TRouteAbstract, Exception>;
  end;

implementation

{ TRouteService }

destructor TRouteService.Destroy;
begin
  FProvider.Free;
  inherited;
end;

function TRouteService.GetRoute(const AArgs: TRouteParam): TResultPair<TRouteAbstract, Exception>;
begin
  try
    Result := FProvider.GetRoute(AArgs);
    if Result.ValueSuccess = nil then
      Result.Failure(ERouteNotFoundException.CreateFmt('', [AArgs.Path]));
  except
    on E: EUnauthorizedException do
      Result.Failure(EUnauthorizedException.Create(''));
    on E: Exception do
      Result.Failure(EBadRequestException.Create(E.Message));
  end;
end;

procedure TRouteService.IncludeProvider(const AProvider: TRouteProvider);
begin
  FProvider := AProvider;
end;

end.






