unit nfebr.tag.total.model;

interface

uses
  nfebr.tag.icmstot.model,
  nfebr.tag.issqntot.model,
  nfebr.tag.rettrib.model;

type
  TTotalModel = class
  private
    FICMSTot: TICMSTotModel;
    FISSQNtot: TISSQNtotModel;
    FRetTrib: TRetTribModel;
  public
    property ICMSTot: TICMSTotModel read FICMSTot;
    property ISSQNtot: TISSQNtotModel read FISSQNtot;
    property RetTrib: TRetTribModel read FRetTrib;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

constructor TTotalModel.Create;
begin
  FICMSTot := TICMSTotModel.Create;
  FISSQNtot := TISSQNtotModel.Create;
  FRetTrib := TRetTribModel.Create;
end;

destructor TTotalModel.Destroy;
begin
  FICMSTot.Free;
  FISSQNtot.Free;
  FRetTrib.Free;
  inherited;
end;

end.
