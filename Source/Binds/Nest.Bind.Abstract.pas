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

unit Nest.Bind.Abstract;

interface

uses
  SysUtils,
  Injector,
  Injector.Events;

type
  TBindAbstract<T: class, constructor> = class
  protected
    FOnCreate: TProc<T>;
    FOnDestroy: TProc<T>;
    FOnParams: TConstructorCallback;
    FAddInstance: TObject;
  public
    destructor Destroy; override;
    procedure IncludeInjector(const AInjector4d: TInjector4D); virtual; abstract;
  end;

implementation

destructor TBindAbstract<T>.Destroy;
begin
  FOnCreate := nil;
  FOnDestroy := nil;
  FOnParams := nil;
  FAddInstance := nil;
  inherited;
end;

end.




