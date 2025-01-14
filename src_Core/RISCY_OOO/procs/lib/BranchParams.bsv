typedef 256 GlobalHistoryLength;
typedef 18 MaxSpecSize;

String dirGoldStandard  = "/home/katy/C++/Toooba/";

`ifdef OFF_GOLD_STANDARD
String regInitFilenameBimodalPred = "Build/regfileMemInit_8192.mem";
String regInitFilenameBimodalHyst = "Build/regfileMemInit_2048.mem";
String regInitTaggedTableFilename = "Build/regfileMemInit_512.mem";
`else
String regInitFilenameBimodalPred = dirGoldStandard + "Build/regfileMemInit_8192.mem";
String regInitFilenameBimodalHyst = dirGoldStandard + "Build/regfileMemInit_2048.mem";
String regInitTaggedTableFilename = dirGoldStandard + "tableInit/regfileMemInit_512.mem";
`endif
