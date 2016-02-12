(*========= INITIALIZATION CHECKS ================*)
(*
	Initialization Checks:
		VersionCheck: 	at least $VersionNumber 10.3;
		FileCheck: 		all source files present in directory;
		PathCheck:		set localPath to directory containing optimizer;
		LicenseCheck:	license_configs.xml in localPath/license;
		
	Failures:
		Any failures of the above checks will terminate the kernel.

	Scope:
		Initialization checks and vars located in Global context
*)
(* Version Check for compatibility *)
VersionCheck[]:= Block[
	{v=$VersionNumber,Version},
	Version::outdated="CSS Optimizer requires Mathematica 10.3 or newer.
	You are running `1`.";
	If[ v <10.3,
		Message[Version::outdated,v]; Exit[]
	](* endIf *);
](* endVersionCheck *);

(* Confirm all file are present for loading *)
FileCheck[]:= Block[{Files,license},
	(* check for all Mathematica package files *)
	Files::NotPresent="Not all required packages are present.";
	If[ Not @ ContainsAll[
		FileNames[],
	 	{"Constraints.m", "DecisionVariables.m", "Goals.m", "Model.m"}
		](* endContainsAll *),
		Message[Files::NotPresent]; Exit[]
	](* endIf *);
](* endFileCheck *);	

(* Confirm presence of license file *)
LicenseCheck[]:=Block[{},
	(* check for presence of license in proper folder *)
	License::NotFound="license_configs.xml not found in `1`";
	license=FileNames["license_configs.xml",{localPath<>"/license"}];
	If[ Length @ license < 1,
		Message[License::NotFound,localPath<>"/license"]; Exit[]
	]; 
](* endLicenseCheck *);

(* Confirm valid path has ben set *)	
PathCheck[]:=Block[{head=Head@localPath},
	localPath::missing="localPath must be set to the directory containing the optimization engine.
	ex: localPath= ~/dir/../css-optimization-engine-version";
	localPath::def="localPath set to `1`";
	localPath::assign="localPath must have head _String";
	Switch[head,
		Symbol, Message[localPath::missing];Exit[],
		String,Message[localPath::def,localPath],
		_,Message[localPath::assign];Exit[]
		](* endSwitch *)
](* endPathCheck *);

(*----- Evaluate all initialization checks ----------*)
Setup::usage="
Setup[localPath_String] sets the path to the parent directory of optimizer and then runs all check functions.
Setup[] runs all check functions.";

Setup[path_String]:=(
 localPath=path;
 Setup[]
);

Setup[]:=(
	VersionCheck[];
	PathCheck[];
	FileCheck[];
);


(*========================== CSSOPTIMIZER PACKAGE ====================================*)
BeginPackage[
	"CSSOptimizer`",
	{"DecisionVariables`","Goals`","Constraints`","Model`"}
	];




EndPackage[];