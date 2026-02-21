unit nfebr.tag.compra.model;

interface

type
  TCompraModel = class
  private
    FXCont: String;
    FXNEmp: String;
    FXPed: String;
  public
    property XCont: String read FXCont write FXCont;
    property XNEmp: String read FXNEmp write FXNEmp;
    property XPed: String read FXPed write FXPed;
  end;


implementation

end.

