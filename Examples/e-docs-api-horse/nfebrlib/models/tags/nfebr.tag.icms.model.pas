unit nfebr.tag.icms.model;

interface

uses
  nfebr.tag.icms00.model,
  nfebr.tag.icms02.model,
  nfebr.tag.icms10.model,
  nfebr.tag.icms15.model,
  nfebr.tag.icms20.model,
  nfebr.tag.icms30.model,
  nfebr.tag.icms40.model,
  nfebr.tag.icms51.model,
  nfebr.tag.icms53.model,
  nfebr.tag.icms60.model,
  nfebr.tag.icms61.model,
  nfebr.tag.icms70.model,
  nfebr.tag.icms90.model,
  nfebr.tag.icmspart.model,
  nfebr.tag.icmssn101.model,
  nfebr.tag.icmssn102.model,
  nfebr.tag.icmssn201.model,
  nfebr.tag.icmssn202.model,
  nfebr.tag.icmssn500.model,
  nfebr.tag.icmssn900.model,
  nfebr.tag.icmsst.model;

type
  TICMSModel = class
  private
    FCST: String;
    FCSOSN: String;
    FOrig: integer;
    FICMS00: TICMS00Model;
    FICMS02: TICMS02Model;
    FICMS10: TICMS10Model;
    FICMS15: TICMS15Model;
    FICMS20: TICMS20Model;
    FICMS30: TICMS30Model;
    FICMS40: TICMS40Model;
    FICMS51: TICMS51Model;
    FICMS53: TICMS53Model;
    FICMS60: TICMS60Model;
    FICMS61: TICMS61Model;
    FICMS70: TICMS70Model;
    FICMS90: TICMS90Model;
    FICMSPart: TICMSPartModel;
    FICMSSN101: TICMSSN101Model;
    FICMSSN102: TICMSSN102Model;
    FICMSSN201: TICMSSN201Model;
    FICMSSN202: TICMSSN202Model;
    FICMSSN500: TICMSSN500Model;
    FICMSSN900: TICMSSN900Model;
    FICMSST: TICMSSTModel;
  public
    constructor Create;
    destructor Destroy; override;
    property CST: String read FCST write FCST;
    property Orig: integer read FOrig write FOrig;
    property CSOSN: String read FCSOSN write FCSOSN;
    property ICMS00: TICMS00Model read FICMS00;
    property ICMS02: TICMS02Model read FICMS02;
    property ICMS10: TICMS10Model read FICMS10;
    property ICMS15: TICMS15Model read FICMS15;
    property ICMS20: TICMS20Model read FICMS20;
    property ICMS30: TICMS30Model read FICMS30;
    property ICMS40: TICMS40Model read FICMS40;
    property ICMS51: TICMS51Model read FICMS51;
    property ICMS53: TICMS53Model read FICMS53;
    property ICMS60: TICMS60Model read FICMS60;
    property ICMS61: TICMS61Model read FICMS61;
    property ICMS70: TICMS70Model read FICMS70;
    property ICMS90: TICMS90Model read FICMS90;
    property ICMSPart: TICMSPartModel read FICMSPart;
    property ICMSSN101: TICMSSN101Model read FICMSSN101;
    property ICMSSN102: TICMSSN102Model read FICMSSN102;
    property ICMSSN201: TICMSSN201Model read FICMSSN201;
    property ICMSSN202: TICMSSN202Model read FICMSSN202;
    property ICMSSN500: TICMSSN500Model read FICMSSN500;
    property ICMSSN900: TICMSSN900Model read FICMSSN900;
    property ICMSST: TICMSSTModel read FICMSST;
  end;

implementation

{ TICMSModel }

constructor TICMSModel.Create;
begin
  FICMS00 := TICMS00Model.Create;
  FICMS02 := TICMS02Model.Create;
  FICMS10 := TICMS10Model.Create;
  FICMS15 := TICMS15Model.Create;
  FICMS20 := TICMS20Model.Create;
  FICMS30 := TICMS30Model.Create;
  FICMS40 := TICMS40Model.Create;
  FICMS51 := TICMS51Model.Create;
  FICMS53 := TICMS53Model.Create;
  FICMS60 := TICMS60Model.Create;
  FICMS61 := TICMS61Model.Create;
  FICMS70 := TICMS70Model.Create;
  FICMS90 := TICMS90Model.Create;
  FICMSPart := TICMSPartModel.Create;
  FICMSSN101 := TICMSSN101Model.Create;
  FICMSSN102 := TICMSSN102Model.Create;
  FICMSSN201 := TICMSSN201Model.Create;
  FICMSSN202 := TICMSSN202Model.Create;
  FICMSSN500 := TICMSSN500Model.Create;
  FICMSSN900 := TICMSSN900Model.Create;
  FICMSST := TICMSSTModel.Create;
end;

destructor TICMSModel.Destroy;
begin
  FICMS00.Free;
  FICMS02.Free;
  FICMS10.Free;
  FICMS15.Free;
  FICMS20.Free;
  FICMS30.Free;
  FICMS40.Free;
  FICMS51.Free;
  FICMS53.Free;
  FICMS60.Free;
  FICMS61.Free;
  FICMS70.Free;
  FICMS90.Free;
  FICMSPart.Free;
  FICMSSN101.Free;
  FICMSSN102.Free;
  FICMSSN201.Free;
  FICMSSN202.Free;
  FICMSSN500.Free;
  FICMSSN900.Free;
  FICMSST.Free;
  inherited;
end;

end.

