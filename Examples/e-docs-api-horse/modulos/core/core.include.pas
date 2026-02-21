unit core.include;

interface

uses
  Rtti,
  Horse,
  Horse.Callback,
  core.types,
  nfebr.model,
  nest4d;

type
  TValue = Rtti.TValue;
  TNFeModel = nfebr.model.TNFeModel;
  TNFeResponse = core.types.TNFeResponse;
  TXMLResponse = core.types.TXMLResponse;
  TPDFResponse = core.types.TPDFResponse;
  TConfigResponse = core.types.TConfigResponse;
  THorseRequest = Horse.THorseRequest;
  THorseResponse = Horse.THorseResponse;
  THorse = Horse.THorse;

implementation

end.
