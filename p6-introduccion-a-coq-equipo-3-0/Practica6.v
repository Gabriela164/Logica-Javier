
Module Props .

Notation "x :: l" := (cons x l)
                     (at level 60, right associativity).
Notation "[ ]" := nil.
Notation "[ x ; .. ; y ]" := (cons x .. (cons y nil) ..).

Fixpoint map {A B : Type} (F : A -> B) (l : list A) : list B :=
  match l with
  | [] => []
  | x :: l' => F x :: map F l'
  end.

(* Dada una lista de tipo A y un elemento del mismo tipo, construye una formula proposicional
   que nos permita saber si el elemento esta contenido en la lista.
   Hint: Usa una formula de or anidados *)
Fixpoint In {A : Type} (x : A) (l : list A) : Prop
:= match l with
   | [] => False
   | y :: l' => y = x \/ In x l'
   end.


Example In_example_1 : In 4 [1; 2; 3; 4; 5].
Proof.
   simpl. (*Separa los elementos 1 :: 2 :: 3 :: ... :: []*)
   right. right. right. left. (* Se mueve por la lista*)
   reflexivity. (*Ve si los elementos son iguales a 4*)
Qed.

Lemma In_map :
forall (A B : Type) (f : A -> B) (l : list A) (x : A),
In x l -> In (f x) (map f l).
Proof.
  intros. (* Definiciones de variables *)
  simpl in H. (* Simplificar la hipótesis en el caso base *)
  induction l as [| h t HI]. (* Caso base: lista vacía o no *)
  - intros. (* Subcaso lista vacía *)
    simpl in H.
    destruct H. (* No hay elementos en una lista vacía, contradicción *)
  - intros. (* Subcaso lista no vacía *)
    simpl in H. (* Simplificar la hipótesis en el caso inductivo *)
    destruct H as [Hl | Hr]. (* Dos subcasos: x está en el primer elemento o en la cola de la lista *)
    + rewrite Hl. (* Sustituir f x por f h en el objetivo *)
      simpl. (* Simplificar el objetivo *)
      left. (* Demostrar que f h = f h *)
      reflexivity.
    + simpl. (* Simplificar el objetivo *)
      right. (* Demostrar que f x está en la lista mapeada de la cola de la lista *)
      apply HI. (* Aplicar la hipótesis de inducción *)
      apply Hr. (* Aplicar la hipótesis de la cola de la lista *)
Qed.

(* Las Funciones que devuelven proposiciones pueden verse como propiedades de sus argumentos
   Por ejemplo: Si P tiene tipo nat -> Prop , entonces P n afirma que la propiedad P es valida para n.
   Implementa All que afirma que una propiedad P se cumple para todos los elementos de una lista l *)
Fixpoint All {T : Type} (P : T -> Prop) (l : list T) : Prop 
:= match l with
   | [] => True
   | x :: l' => P x /\ All P l'
   end.

Lemma All_In :
forall T (P : T -> Prop) (l : list T),
(forall x, In x l -> P x) <-> All P l.
Proof.
  intros. (* Definiciones de variables *)
  split. (* Dos pasos: Se cumple P para cada elemento entonces P es verdadero, P es verdadero entonces se cumple para cada elemento*)
  - induction l as [ | h t HI]. (* Vacio o no vacio*)
   + simpl. intros. reflexivity. (* La lista es vacía, por lo que se cumple*)
   + simpl. intros. (* Segundo caso*)
      split.
      * apply H. left. reflexivity. (* Aplicamos la hipótesis H al primer elemento *)
      * apply HI. intros h' H'. apply H. right. apply H'. (* Aplicamos la hipótesis de inducción a la cola de la lista *)
  - induction l as [| h t HI].
    + simpl. intros. destruct H0.
    + simpl. intros.
      destruct H.
      destruct H0.
      * subst. apply H. (* Si el elemento está en la cabeza de la lista, aplicamos directamente la hipótesis *)
      * apply HI.
          apply H1.
          apply H0.
Qed.

End Props .

Module Streams.

Variable L : Type.

Inductive TStream :=
   TScons : L -> TStream -> TStream.

(* Destructor que obtiene la cabeza del tipo TStream *)
Definition TShead (s:TStream) : L 
:= match s with
   | TScons h t => h
   end.


(* Destructor que obtiene la cola del tipo TStream *)
Definition TStail (s:TStream) : TStream
:= match s with
   | TScons h t => t
   end.

(* Dado un natural n y un TStream regresa el n-esimo elemento de s *)
Fixpoint TSnth (n: nat) (s:TStream) : L 
:= match n with
   | 0 => TShead s
   | S n' => TSnth n' (TStail s)
   end.

(* Aplica n veces TSnth_tail a s *)
Fixpoint TSnth_tail (n: nat) (s:TStream) : TStream 
:= match n with
   | 0 => s
   | S n' => TSnth_tail n' (TStail s)
   end.

(* Concatena los primeros n elementos del TStream s1 con s2 *)
Fixpoint TSnth_conc (n:nat) (s1 s2:TStream) : TStream 
:= match n with
   | 0 =>  s2
   | S n' => TScons (TShead s1) (TSnth_conc n' (TStail s1) s2)
   end.


(*1er lema*)
Lemma one_step_nth_tail :
  forall (n : nat) (s : TStream),
    TStail (TSnth_tail n s) = TSnth_tail (S n) s.
Proof.

intro n.
induction n.
intros.
- simpl. reflexivity. (*Caso base*)
- intro my_s.    (*Caso recursivo*)
  simpl.
  rewrite IHn.
  simpl.
  reflexivity.
Qed.
 

(*2do lema*)
Lemma multi_step_nth_tail : 
  forall  (s:TStream) (n:nat) ,
 ( TSnth_tail (S n) s) = (TSnth_tail n (TStail s)).
Proof.
  intros.
  induction n as [| n' HI].
  - simpl.
    reflexivity. (*Caso base*)
  - simpl.
    reflexivity. (*Caso recursivo*)
Qed.


(*3er lema*)
Lemma multi_step_nth_conc : 
  forall (n:nat) (s1 s2:TStream) ,
  (TSnth_conc (S n) s1 s2) = (TSnth_conc n s1 (TScons (TSnth n s1) s2)).
Proof.
  intro n.
  induction n.
  intros.
   - simpl. reflexivity. (*Caso base*)
   - intro my_s1. (*Caso recursivo*)
     intro my_s2.
     simpl.
     rewrite <- IHn. 
     simpl. 
     reflexivity.
Qed.
 


(*4to lema*)
Lemma cons_head_tail :
  forall (s:TStream) ,
  TScons (TShead s) (TStail s) = s.
Proof.
  intros s.
  destruct s as [h t]. (* Descomponer s en cabeza y cola*)
  simpl.
  reflexivity.
Qed.


(*5to lema*)
Lemma nth_tail_with_nth_conc : 
  forall (n:nat)(s1 s2:TStream) ,
  (TSnth_tail n (TSnth_conc n s1 s2)) = s2.
Proof.
  intro n.
  induction n.
  intros.
  - simpl. reflexivity. (*Caso base*)
  - intro my_s1. (*Caso recursivo*)
    intro my_s2.
    simpl. 
    rewrite IHn.
    reflexivity.
Qed.
 

(*6to lema*)
Lemma nth_conc_with_nth_tail :
  forall (n:nat)(s:TStream),
  (TSnth_conc n s (TSnth_tail n s)) = s.
Proof.
  intros n.
  induction n.
  intros.
  - simpl. reflexivity. (*Caso base*)
  - intro my_s.         (*Caso recursivo*)
    simpl. 
    rewrite IHn.
    rewrite cons_head_tail.
    reflexivity.
Qed.

End Streams.