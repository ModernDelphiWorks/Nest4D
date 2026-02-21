unit nfebr.tag.detpag.model;

interface

uses
  nfebr.tag.card.model;

type
  TDetPagModel = class
  private
    FCard: TCardModel;
    FIndPag: integer;
    FTPag: String;
    FVPag: Currency;
    FXPag: String;
  public
    constructor Create;
    destructor Destroy; override;
    property Card: TCardModel read FCard;
    property IndPag: integer read FIndPag write FIndPag;
    property TPag: String read FTPag write FTPag;
    property VPag: Currency read FVPag write FVPag;
    property XPag: String read FXPag write FXPag;
  end;

implementation

{ TDetPagModel }

constructor TDetPagModel.Create;
begin
  FCard := TCardModel.Create;
end;

destructor TDetPagModel.Destroy;
begin
  FCard.Free;
  inherited;
end;

end.

