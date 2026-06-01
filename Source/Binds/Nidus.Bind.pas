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

unit Nidus.Bind;

interface

uses
  Classes,
  SysUtils,
  Generics.Collections,
  Nidus.Bind.Abstract,
  Inject,
  Inject.Events;

type
  TBind<T: class, constructor> = class(TBindAbstract<T>)
  public
    constructor Create(const AOnCreate: TProc<T>;
      const AOnDestroy: TProc<T>;
      const AOnConstructorParams: TConstructorCallback); overload;
    class function Singleton(const AOnCreate: TProc<T> = nil;
      const AOnDestroy: TProc<T> = nil;
      const AOnConstructorParams: TConstructorCallback = nil): TBind<TObject>;
    class function SingletonLazy(const AOnCreate: TProc<T> = nil;
      const AOnDestroy: TProc<T> = nil;
      const AOnConstructorParams: TConstructorCallback = nil): TBind<TObject>;
    class function Factory(const AOnCreate: TProc<T> = nil;
      const AOnDestroy: TProc<T> = nil;
      const AOnConstructorParams: TConstructorCallback = nil): TBind<TObject>;
    class function SingletonInterface<I: IInterface>(const AOnCreate: TProc<T> = nil;
      const AOnDestroy: TProc<T> = nil;
      const AOnConstructorParams: TConstructorCallback = nil): TBind<TObject>;
    class function AddInstance(const AInstance: TObject): TBind<TObject>;
  end;

  TSingletonBind<T: class, constructor> = class(TBind<T>)
  public
    procedure IncludeInject(const ANidusInject: TInject); override;
  end;

  TSingletonLazyBind<T: class, constructor> = class(TBind<T>)
  public
    procedure IncludeInject(const ANidusInject: TInject); override;
  end;

  TFactoryBind<T: class, constructor> = class(TBind<T>)
  public
    procedure IncludeInject(const ANidusInject: TInject); override;
  end;

  TSingletonInterfaceBind<I: IInterface; T: class, constructor> = class(TBind<T>)
  public
    procedure IncludeInject(const ANidusInject: TInject); override;
  end;

  TAddInstanceBind<T: class, constructor> = class(TBind<T>)
  public
    constructor Create(const AInstance: TObject); overload;
    procedure IncludeInject(const ANidusInject: TInject); override;
  end;

implementation

{ TBind<T> }

constructor TBind<T>.Create(const AOnCreate: TProc<T>; const AOnDestroy: TProc<T>;
  const AOnConstructorParams: TConstructorCallback);
begin
  FOnCreate := AOnCreate;
  FOnDestroy := AOnDestroy;
  FOnParams := AOnConstructorParams;
end;

class function TBind<T>.Factory(const AOnCreate: TProc<T>; const AOnDestroy: TProc<T>;
  const AOnConstructorParams: TConstructorCallback): TBind<TObject>;
begin
  Result := TBind<TObject>(TFactoryBind<T>.Create(AOnCreate,
                                                  AOnDestroy,
                                                  AOnConstructorParams));
end;

class function TBind<T>.Singleton(const AOnCreate: TProc<T>; const AOnDestroy: TProc<T>;
  const AOnConstructorParams: TConstructorCallback): TBind<TObject>;
begin
  Result := TBind<TObject>(TSingletonBind<T>.Create(AOnCreate,
                                                    AOnDestroy,
                                                    AOnConstructorParams));
end;

class function TBind<T>.SingletonInterface<I>(const AOnCreate: TProc<T>; const AOnDestroy: TProc<T>;
  const AOnConstructorParams: TConstructorCallback): TBind<TObject>;
begin
  Result := TBind<TObject>(TSingletonInterfaceBind<I, T>.Create(AOnCreate,
                                                                AOnDestroy,
                                                                AOnConstructorParams));
end;

class function TBind<T>.SingletonLazy(const AOnCreate: TProc<T>; const AOnDestroy: TProc<T>;
  const AOnConstructorParams: TConstructorCallback): TBind<TObject>;
begin
  Result := TBind<TObject>(TSingletonLazyBind<T>.Create(AOnCreate,
                                                        AOnDestroy,
                                                        AOnConstructorParams));
end;

class function TBind<T>.AddInstance(const AInstance: TObject): TBind<TObject>;
begin
  Result := TBind<TObject>(TAddInstanceBind<T>.Create(AInstance));
end;

{ TSingletonBind }

procedure TSingletonBind<T>.IncludeInject(const ANidusInject: TInject);
begin
  ANidusInject.Singleton<T>(FOnCreate, FOnDestroy, FOnParams);
end;

{ TSingletonLazyBind<T> }

procedure TSingletonLazyBind<T>.IncludeInject(const ANidusInject: TInject);
begin
  ANidusInject.SingletonLazy<T>(FOnCreate, FOnDestroy, FOnParams);
end;

{ TFactoryBind<T> }

procedure TFactoryBind<T>.IncludeInject(const ANidusInject: TInject);
begin
  ANidusInject.Factory<T>(FOnCreate, FOnDestroy, FOnParams);
end;

{ TSingletonInterfaceBind<T> }

procedure TSingletonInterfaceBind<I, T>.IncludeInject(const ANidusInject: TInject);
begin
  ANidusInject.SingletonInterface<I, T>('', FOnCreate, FOnDestroy, FOnParams);
end;

{ TAddInstanceBind<T> }

constructor TAddInstanceBind<T>.Create(const AInstance: TObject);
begin
  FAddInstance := Ainstance;
end;

procedure TAddInstanceBind<T>.IncludeInject(const ANidusInject: TInject);
begin
  ANidusInject.AddInstance<T>(FAddInstance);
end;

end.


