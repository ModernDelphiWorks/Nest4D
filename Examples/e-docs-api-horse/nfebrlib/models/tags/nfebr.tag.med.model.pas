unit nfebr.tag.med.model;

interface

type
  TMedModel = class
  private
    FCProdANVISA: String;
    FVPMC: Integer;
    FXMotivoIsencao: String;
  public
    property CProdANVISA: String read FCProdANVISA write FCProdANVISA;
    property VPMC: Integer read FVPMC write FVPMC;
    property XMotivoIsencao: String read FXMotivoIsencao write FXMotivoIsencao;
  end;

implementation

end.

