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

unit nest4d.bind.provider;

interface

uses
  SysUtils,
  System.Evolution.ResultPair,
  nest4d.tracker;

type
  TBindProvider = class
  private
    FTracker: TTracker;
  public
    constructor Create(const ATracker: TTracker);
    destructor Destroy; override;
    function GetBind<T: class, constructor>(const ATag: String): TResultPair<T, Exception>;
    function GetBindInterface<I: IInterface>(const ATag: String): TResultPair<I, Exception>;
  end;

implementation

constructor TBindProvider.Create(const ATracker: TTracker);
begin
  FTracker := ATracker;
end;

destructor TBindProvider.Destroy;
begin
  FTracker := nil;
  inherited;
end;

function TBindProvider.GetBindInterface<I>(const ATag: String): TResultPair<I, Exception>;
begin
  Result.Success(FTracker.GetBindInterface<I>(ATag));
end;

function TBindProvider.GetBind<T>(const ATag: String): TResultPair<T, Exception>;
begin
  Result.Success(FTracker.GetBind<T>(ATag));
end;

end.







