unit core.types;

interface

uses
  Classes,
  Core.Exception,
  Evolution4D.ResultPair;

type
   TNFeResponse = TResultPair<String, ERequestException>;
   TXMLResponse = TResultPair<TFileStream, ERequestException>;
   TPDFResponse = TResultPair<TFileStream, ERequestException>;
   TConfigResponse = TResultPair<String, ERequestException>;

implementation

end.

