Require Import CoqOfOCaml.CoqOfOCaml.
Require Import CoqOfOCaml.Settings.

Inductive Things : Set :=
| Coq 
| OrgModeNotes.

Inductive ProofThings : Set :=
| CoqProofs2.

Inductive NoteThings : Set :=
| OrgModeNotes2 : string -> NoteThings
.

(*level chunking.splitting.sorting*)
Inductive DerivedNoteThings : Set :=                                  
  | OrgModeNotesPrompt : string -> DerivedNoteThings
  | OrgModeNotesModel : string -> DerivedNoteThings                                                             
  | OrgModeNotesWordList : list string -> DerivedNoteThings
  | OrgModeNotesSentenceList : list string -> DerivedNoteThings
.

Inductive Chunker : Set :=
| Chunk: NoteThings -> DerivedNoteThings -> Chunker.

Inductive Grammar : Set :=
| GBNFGrammar : string -> Grammar
| MenhirGrammar : string -> Grammar.

Inductive Translation_grammar : Set :=
| TranslateGrammar: Grammar -> Grammar -> Translation_grammar.
  
Inductive LLM : Set :=
| Generate : (*model*)string -> (*prompt*)string -> LLM
| GenerateList : (*model*)string -> (*prompt list*)DerivedNoteThings -> LLM.

Inductive Translation : Set :=
  | Translate : DerivedNoteThings * ProofThings -> Translation.


Inductive BootstrapSteps : Set :=
| Notes:  string -> BootstrapSteps
.

