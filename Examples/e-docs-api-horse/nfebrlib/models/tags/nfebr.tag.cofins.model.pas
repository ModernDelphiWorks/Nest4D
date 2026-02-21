unit nfebr.tag.cofins.model;

interface

uses
  nfebr.tag.cofinsaliq.model,
  nfebr.tag.cofinsoutr.model,
  nfebr.tag.cofinsqtde.model;

type
  TCOFINSModel = class
  private
    FCST: String;
    FCOFINSAliq: TCOFINSAliqModel;
    FCOFINSOutr: TCOFINSOutrModel;
    FCOFINSQtde: TCOFINSQtdeModel;
  public
    constructor Create;
    destructor Destroy; override;
    property CST: String read FCST write FCST;
    property COFINSAliq: TCOFINSAliqModel read FCOFINSAliq;
    property COFINSOutr: TCOFINSOutrModel read FCOFINSOutr;
    property COFINSQtde: TCOFINSQtdeModel read FCOFINSQtde;
  end;

implementation

{ TCOFINSModel }

constructor TCOFINSModel.Create;
begin
  FCOFINSAliq := TCOFINSAliqModel.Create;
  FCOFINSOutr := TCOFINSOutrModel.Create;
  FCOFINSQtde := TCOFINSQtdeModel.Create;
end;

destructor TCOFINSModel.Destroy;
begin
  FCOFINSAliq.Free;
  FCOFINSOutr.Free;
  FCOFINSQtde.Free;
  inherited;
end;

end.

