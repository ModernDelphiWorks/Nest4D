unit nfe.acbr.tag.interfaces;

{$include ..\..\..\nfebrlib.inc}

interface

uses
  ACBrNFe,
  {$IFDEF FASTREPORT}
  ACBrNFeDANFEFR,
  {$ENDIF}
  ACBrNFeDANFeESCPOS,
  ACBrMail,
  ACBrPosPrinter,
  nfebr.lib.enum;

type
  INFeCommandConfiguracoes = interface
    ['{825A2486-BBF6-44A5-A164-D05BB9BD0B39}']
    procedure Execute(const AMod: TDFeModelo);
  end;

  INFeTagDet = interface
    ['{CA782853-8FEF-4A37-A9B8-6A0FBF86B5DC}']
    procedure Execute(const AID: Integer);
  end;

  INFeTagProd = interface
    ['{E613A21D-6261-44C2-BDF5-5BF027FC2B47}']
//    procedure Execute(const ADataSet: IDBResultSet);
  end;

  INFeACBr = interface
    ['{6ADC318F-5758-4A32-8406-C4D0AE46446F}']
    function DFe: TACBrNFe;
    {$IFDEF FASTREPORT}
    function DanfeFR: TACBrNFeDANFEFR;
    {$ENDIF}
    function DanfeESCPOS: TACBrNFeDANFeESCPOS;
    function EMail: TACBrMail;
    function PosPrinter: TACBrPosPrinter;
  end;

implementation

end.
