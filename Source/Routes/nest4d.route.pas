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

unit nest4d.route;

interface

uses
  nest4d.route.abstract;

type
  TRoute = class(TRouteAbstract)
  end;

  TRouteModule = class(TRoute)
  public
    class function AddModule(const APath: String;
      const AModule: TClass;
      const AMiddlewares: TMiddlewares): TRouteAbstract; override;
  end;

  TRouteChild = class(TRoute)
  public
    class function AddModule(const APath: String;
      const AModule: TClass;
      const AMiddlewares: TMiddlewares): TRouteAbstract; override;
  end;

implementation

{ TRouteModule }

class function TRouteModule.AddModule(const APath: String;
  const AModule: TClass;
  const AMiddlewares: TMiddlewares): TRouteAbstract;
begin
  inherited;
  Result := TRouteModule.Create(APath,
                                AModule.ClassName,
                                AModule,
                                AMiddlewares);
end;

{ TRouteChild }

class function TRouteChild.AddModule(const APath: String;
  const AModule: TClass;
  const AMiddlewares: TMiddlewares): TRouteAbstract;
begin
  inherited;
  Result := TRouteChild.Create(APath, '', AModule, AMiddlewares);
end;

end.





