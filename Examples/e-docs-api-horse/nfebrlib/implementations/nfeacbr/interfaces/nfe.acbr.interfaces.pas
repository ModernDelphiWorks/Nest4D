unit nfe.acbr.interfaces;

interface

uses
  Generics.Collections,
  ACBrNFe.Classes,
  nfebr.lib.enum,
  nfebr.lib.utils,
  nfebr.lib.include,
  nfebr.model,
  nfebr.tag.det.model;

type
  INFeExecute = interface
    ['{FEF23ECE-1396-478E-BB01-C6769216CBB2}']
    procedure Execute(const AIndex: integer; const ANFeModel: TNFeModel);
  end;

  INFeFactory = interface(INFeExecute)
    ['{BB75F7C1-DE2D-4816-9B63-CA3D6B6181CA}']
  end;

  INFeImposto = interface
    ['{D02A6402-8E95-4497-8B56-6F0AF25EA81D}']
    procedure Execute(const ADetItem: TDetCollectionItem;
      const ANFeModel: TNFeModel; const ADetModel: TDetModel);
  end;

  INFeConfiguracao = interface
    ['{115CB3D1-63C0-460E-86E1-AFB8477774B7}']
    function Execute: INFeConfiguracao;
    function EnvioSincrona: Boolean;
  end;

  INFeInutilizaCommand = interface
    ['{240F79E1-2C05-4316-BC7A-020EAA71043A}']
    function Execute(const AReq: TNFeRequestInutiliza): TNFeResponseInutiliza;
  end;

  INFeCartaCorrecaoCommand = interface
    ['{86894B1B-5F8D-453A-A96F-7F69982095D7}']
    function Execute(const AReq: TNFeRequestCCe): TNFeResponseCCe;
  end;

  INFeCancelaCommand = interface
    ['{4A3F0609-DDE5-4D0A-8FCE-47A509129DE5}']
    function Execute(const AReq: TNFeRequestCancela): TNFeResponseCancela;
  end;

  INFeConsultaCommand = interface
    ['{6D30C39D-5A5F-4767-90FD-53293F1DFCB3}']
    function Execute(const AReq: TNFeRequestConsulta): TNFeResponseConsulta;
  end;

  INFeCCeConsultaCommand = interface
    ['{47C64B31-5E69-4646-A5D8-B6EB96AC9886}']
    function Execute(const AReq: TNFeRequestConsulta): TNFeResponseCCeConsulta;
  end;

  INFeCancelaConsultaCommand = interface
    ['{F9323B2D-E84D-44CD-A20B-76A614AAEDB1}']
    function Execute(const AReq: TNFeRequestConsulta): TNFeResponseCancelaConsulta;
  end;

  INFeTransmiteCommand = interface
    ['{2D0F1428-88F6-44FC-AF7A-FE93D8D592F9}']
    function Execute(const AReq: TNFeRequestTransmite;
      const AIsSincrona: boolean): TNFeResponseTransmite;
  end;

  INFeTransmiteLoteCommand = interface
    ['{043B6F7E-7D50-4E3B-89AE-6B6F61A79705}']
    function Execute(const AReq: TObjectList<TNFeRequestTransmite>;
      const AIsSincrona: boolean): TNFeResponseTransmiteLote;
  end;

  INFeServicoStatusCommand = interface
    ['{5380A31A-B668-4889-A173-CBBFA0AE261C}']
    function Execute: TNFeResponseServicoStatus;
  end;

implementation

end.
