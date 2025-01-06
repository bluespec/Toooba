typedef 256 GlobalHistoryLength;
typedef 16 MaxSpecSize;

String dirGoldStandard  = "branch/gold_standard/Predictors/TageTest/Bluespec/";

`ifdef OFF_GOLD_STANDARD
String regInitFilenameBimodalPred = "Build/regfileMemInit_8192.mem";
String regInitFilenameBimodalHyst = "Build/regfileMemInit_2048.mem";
String regInitTaggedTableFilename = "Build/regfileMemInit_512.mem";
`else
String regInitFilenameBimodalPred = dirGoldStandard + "Build/regfileMemInit_8192.mem";
String regInitFilenameBimodalHyst = dirGoldStandard + "Build/regfileMemInit_2048.mem";
String regInitTaggedTableFilename = dirGoldStandard + "Build/regfileMemInit_512.mem";
`endif
