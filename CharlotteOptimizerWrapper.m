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
(*
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
*)
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
	(*FileCheck[];*)
);


(*========================== CSSOPTIMIZER PACKAGE ====================================*)
Needs["JLink`"];
InstallJava[];

(* rules to be used in string expansions *)
pqn="kmw.platform.optimizer";

rules = Dispatch@{
    "CONT" -> pqn <> ".variables.ContinuousDecisionVariable",
    "INTEGER" -> pqn <> ".variables.IntegerDecisionVariable",
    "INTERVAL" -> pqn <> ".variables.IntervalDecisionVariable",
    "CATEGORY" -> pqn <> ".variables.CategoricalDecisionVariable",
    "PERMUTE" -> pqn <> ".variables.PermutationDecisionVariable"
    };


(* Structure testing for Decision Variables *)
CONTQ[type_String, name_String, lower_?NumericQ, upper_?NumericQ] /; (type === "CONT" && lower < upper) := True;
CONTQ[__] := False

INTEGERQ[type_String, name_String, lower_?IntegerQ, upper_?IntegerQ] /; (type === "INTEGER" && lower < upper) := True;
INTEGERQ[__] := False

INTERVALQ[type_String, name_String, lower_?NumericQ, upper_?NumericQ, step_?NumericQ] /; (type === "INTERVAL" && lower < upper) := True
INTERVALQ[__] := False

CATEGORYQ[type_String, name_String, list_JavaObject] /; (type === "CATEGORY") := True;
CATEGORYQ[__] := False

PERMUTEQ[type_String, set_String] /; (type === "PERMUTE") := True
PERMUTEQ[__] := False

(* Decision Variable  *)
DecisionVariables::usage = 
  "DecisionVariables[{type,name,parameter..}..] accepts tuples of the \
given form. The tuples are converted into decision variables based on \
their type. The created decision variables are passed then appended \
to an ArrayList. The output is x_JavaObject where \
IsInstance[x]===ArrayList. ";

DecisionVariables[dv__List] := Block[
   {DesVar, JN},
   (* messages *)
   
   DesVar::badType = "{Type,__}; Type not recognized";
   DesVar::fail = "Failure to load decision variables";
   
   (* instantiate new ArrayList as variable container *)
   (* NOTE: variables IS A GLOBAL VAR *)
   variables = JavaNew["java.util.ArrayList"];
   
   (* instantiates new java objects *)
   JN = (variables@add[JavaNew[# /. rules, ##2]]) & @@ # &;
   
   (* confirm structure of all elements and load variables *)
   Which[
      CONTQ @@ #, JN@#,
      INTEGERQ @@ #, JN@#,
      INTERVALQ @@ #, JN@#,
      CATEGORYQ @@ #, JN@#,
      PERMUTEQ @@ #, JN@#,
      _, Message[DesVar::badType]
      ](* endSwitch *)& /@ {dv};
   
   (* confirm variables_JavaObject created and loaded with all dv__ *)
   Which[(InstanceOf[variables, "java.util.ArrayList"] && 
      variables@size[] == Length[{dv}]),Row[{"variables =",variables}],
    _, Message[DesVar::fail]
   ]
](* endDecisionVarialbes *);

(*==================== GOALS ==============================*)
Goals::usage="Goals[{name,direction}..] sets the goals available to the optimizer. 
At most two goals can be implemented.";

Goals[goal__List] /; (Length@{goal} < 3) := Block[{VALIDQ},
   (*------- Confirm valid optimization direction --------*)
   VALIDQ[name_String, direction_String] /; (
      direction === "MINIMIZE" ||
       direction === "MAXIMIZE"
      ) := True;
   VALIDQ[__] := False;
   
   (*------- Error message --------------*)
   Goal::fail = "Failed to load goal(s).";
   
   (*-------- Instantiate ArrayList and add Subscript[goal, i] *)
   (* 
   NOTE: goals IS A GLOBAL VAR *)
   goals = JavaNew["java.util.ArrayList"];
   
   Switch[True,
      VALIDQ @@ #, (goals@add[JavaNew[pqn <> ".GoalImpl", ##]]) & @@ #,
      _, Message[Goal::fail]
      ] & /@ {goal};
      
   Row[{"goals =",goals}]
   ];





















